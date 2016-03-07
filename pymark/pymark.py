#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re

#==============================================================================
# Globals
CODE_INDENT = 4

C_SPACE = ' '
C_TAB = '\t'
C_GREATERTHAN = '>'

#BLOCKTAGNAME = '(?:article|header|aside|hgroup|iframe|blockquote|hr|body|li|map|button|object|canvas|ol|caption|output|col|p|colgroup|pre|dd|progress|div|section|dl|table|td|dt|tbody|embed|textarea|fieldset|tfoot|figcaption|th|figure|thead|footer|footer|tr|form|ul|h1|h2|h3|h4|h5|h6|video|script|style)'
#UNQUOTEDVALUE = "[^\"'=<>`\\x00-\\x20]+"
#SINGLEQUOTEDVALUE = "'[^']*'"
#DOUBLEQUOTEDVALUE = '"[^"]*"'
#ATTRIBUTEVALUESPEC = "(?:" + "\\s*=" + "\\s*" + ATTRIBUTEVALUE + ")"
#OPENBLOCKTAG = "<" + BLOCKTAGNAME + ATTRIBUTE + "*" + "\\s*/?>"
#CLOSEBLOCKTAG = "</" + BLOCKTAGNAME + "\\s*[>]"

#==============================================================================
# Helpers

def is_space_or_tab(character):
    return character == C_SPACE or character == C_TAB

class Line(object):
    """A line"""
    re_tab_space = re.compile(r'([ \t]*)')
    def __init__(self, line, line_num, offset=0, column=0):
        super(Line, self).__init__()

        # offset and column are different only when tabs are counted as 4 spaces
        # offset is counted in characters
        # column is counted when treat tab as 4 spaces

        self.line     = line
        self.line_num = line_num
        self.offset   = offset
        self.column   = column

        # if a tab exists, then it counts as 4 spaces normally.
        # then if we advance cursor less than 4 column,
        # then this tab is called partial_consumed_tab
        self.partial_consumed_tab = False

    def find_next_non_space(self):
        """Find next character that is not tab or space, save indentation status"""

        indent = self.re_tab_space.match(self.line[self.offset:])
        indent = indent.group(0) if indent else ''

        self.blank = len(indent) >= len(self.line[self.offset:])

        self.next_non_space = self.offset + len(indent)
        self.indent = len(indent.replace('\t', '    '))
        self.next_non_space_col = self.column + self.indent
        self.indented = self.indent >= CODE_INDENT

    def advance_next_non_space(self):
        """Actually move the cursor of current line to next none space character"""
        self.offset = self.next_non_space
        self.column = self.next_non_space_col

    def advance_offset(self, count, tab_as_space=False):
        """Advance the cursor by count position, if tab_as_space is true, count a
        tab as 4 spaces"""

        line = self.line
        for c in line[self.offset:]:
            if c == '\t' and tab_as_space:
                # one special rule here is that a tab with preceding spaces can not be
                # treated as full 4 spaces, but treated as to the next tabstop
                # (_ as space, > as tab)
                # __> abc  => _____abc (5 space + abc)
                tab_spaces = 4 - (self.column % 4)

                self.partial_consumed_tab = tab_space > count

                column_to_advance = count if tab_space > count else tab_space
                self.column += column_to_advance
                self.offset += 0 if self.partial_consumed_tab else 1

                count -= tab_spaces
            else:
                self.partial_consumed_tab = False
                self.offset += 1;
                self.column += 1
                count -= 1

            if count <= 0:
                break

        # update indent information
        self.find_next_non_space()

    @property
    def clean_line(self):
        """return the line the strip contents before current offset, properly handle partially
        consumed tabs"""
        ret = ""
        if self.partial_consumed_tab:
            self.offset += 1
            tab_spaces = 4 - (self.column % 4)
            ret += ' ' * tab_as_space
        return ret + self.line[self.offset:]

    @property
    def after_strip(self):
        """return the line that ignore the indents"""
        return self.line[self.next_non_space:]

    def peek(self, offset=0):
        """peek the character `offset` after on the current line"""
        try:
            return self.line[self.offset + offset]
        except:
            return None

    def get_char(self, offset=0):
        """get the character on the current line at `offset`"""
        try:
            return self.line[offset]
        except:
            return None

#==============================================================================
# Parser

class Parser(object):
    """parse state"""
    def __init__(self):
        super(Parser, self).__init__()
        self.line_num               = 0
        self.doc                    = BlockFactory.make_block('document', 0, 0)
        self.last_matched_container = None
        self.tip                    = self.doc # inner most block

    def close(self, block):
        block.end_line = self.line.line_num
        block.end_col = len(self.line.line)
        block.is_open = False
        block.close(self)
        self.tip = block.parent

    def close_unmatched(self):
        while self.oldtip != self.last_matched_container:
            parent = self.oldtip.parent
            self.close(self.oldtip)
            self.oldtip = parent

    def add_child(self, block):
        while not self.tip.can_contain(block):
            self.close(self.tip)

        self.tip.append_child(block)
        self.tip = block.tail_child

    def unlink_tail(self):
        ret = self.tip
        parent = self.tip.parent
        parent.unlink_last()
        self.tip = parent.tail_child
        return ret

    def parse_line(self, line):
        """Analyze a line of text and update the AST accordingly"""

        self.line_num += 1
        self.line = Line(line, self.line_num)

        self.oldtip = self.tip
        container = self.doc
        ret = None

        # go through the containers and check if the container can contain this line.
        last_child = container.last_child
        self.line.find_next_non_space()
        while last_child and last_child.is_open:
            container = last_child
            last_child = container.last_child

            self.line.find_next_non_space()

            ret = container.can_strip(self)
            if ret == Block.YES:
                pass
            elif ret == Block.NO:
                container = container.parent
                break
            elif ret == Block.CONSUMED:
                # the line is already handled, return
                return
            else:
                raise Exception('can_strip returns unknown value')

        # last_matched_container is used for closing unmatched blocks
        # while container is used for parsing, which may be changed during parse_rest()
        self.last_matched_container = container
        self.container = container

        # Now the line is striped, parse it as a normal unindented line

        if self.tip.name == 'paragraph':
            return self.tip.consume(self)
        elif ret == Block.YES:
            # the inner most block(leaf block) can consume this line
            container.consume(self)
        else:
            # treat the line as a new line
            self.close_unmatched()
            block = self.parse_rest()
            if block is not None:
                self.add_child(block)

    def parse_rest(self):
        """parse rest of the line, that means it will not check indents of containing blocks"""
        pass

        # use parse to get the block.
        # use block's

        block = BlockFactory.matched_block(self)
        if block is None:
            return

        if block.type == 'leaf':
            return block
        elif block.type == 'container':
            # the line header is already consumed
            self.container = block
            child = self.parse_rest()
            if child is not None:
                block.append_tail(child)
            return block

    def parse_inlines(self):
        """Parse the generated AST for inline elements"""
        pass

    def parse(self, input):
        """parse the input string and return the AST"""
        for line in input.split('\n'):
            self.parse_line(line)

        while self.tip:
            self.close(self.tip)

        self.parse_inlines()
        return self.doc



#==============================================================================
# Node & Block

class Node(object):
    """A tree node"""
    def __init__(self, parent=None, start_line=1, start_col=0, end_line=0, end_col=0):
        self.parent     = parent
        self.children   = []
        self.is_open    = True
        self.start_line = start_line
        self.start_col  = start_col
        self.end_line   = end_line
        self.end_col    = end_col

    @property
    def first_child(self):
        """accessing first child, return None if no child exists"""
        try:
            return self.children[0]
        except:
            return None

    @property
    def last_child(self):
        """Helper function for accessing last child, return None if not child exists"""
        try:
            return self.children[-1]
        except:
            return None

    @property
    def tail_child(self):
        container = self
        while container.last_child:
            container = container.last_child
        return container

    @property
    def sibling(self):
        """Get the next sibling of a node"""
        try:
            return self.parent.children[self._index + 1]
        except:
            return None

    def append_child(self, node):
        """append a child"""
        node._index = len(self.children)
        node.parent = self
        self.children.append(node)

    def append_tail(self, node):
        container = self
        while container.last_child:
            container = container.last_child
        container.append_child(node)

    def unlink_last(self):
        return self.children.pop()

#==============================================================================
# Various blocks

class Block(Node):
    """block"""
    YES      = 0
    NO       = 1
    CONSUMED = 2

    name = 'block'

    def __init__(self, *args, **kws):
        super(Block, self).__init__(*args, **kws)
        self.last_line_blank = False
        self.lines = []

    def can_strip(self, parser):
        """Check if the current line can be part of current block, block should be
        a containing block

        :returns: YES      if can parse the line
                  NO       if cannot parse the line
                  CONSUMED if the line ends the current block

        """
        return Block.YES

    def close(self, parser):
        """Action to be made when a block is closed/finalized"""
        pass

    def can_contain(self, block):
        """Check if current block can contain other block, for example, a blockquote can contain
        other blocks, but `list` block can contain only `list-item` block.

        :block: the block to be contained
        :returns: true if `block` can be contained else false

        """
        return False

    def consume(self, parser):
        """Consume current line"""
        pass

    def _get_content(self):
        return '|'.join(self.lines)

    def _repr(self, level=0):
        ret = []
        ret.append('    '*level + '%s[%d, %d, %d, %d] [%s]' % (self.name,self.start_line, self.start_col, self.end_line, self.end_col, self._get_content()))
        for child in self.children:
            ret.append(child._repr(level+1))
        return '\n'.join(ret)

    def __repr__(self):
        return self._repr()


class BlockParser(object):
    """Parse a line for block"""

    precedence = 10 # bigger number means LESS precedence

    @staticmethod
    def parse(parser):
        """parse line given parser and the current container, return a block if can be parsed,
        otherwise return None"""
        pass

class InlineNode(Node):
    """Nodes for inline"""
    def __init__(self, name, literal = ""):
        super(InlineNode, self).__init__()
        self.name = name
        self._literal = literal

#------------------------------------------------------------------------------
class Document(Block):
    """root block of a document"""

    name = 'document'
    type = 'root'
    def __init__(self, *args, **kws):
        super(Document, self).__init__(*args, **kws)

    def can_contain(self, block):
        # a document can contain `list` but not `item` directly
        # that means an `item` should be wrapped by `list`
        return block.name != 'list-item'

#------------------------------------------------------------------------------

class Paragraph(Block):
    """A paragraph"""

    name = 'paragraph'
    type = 'leaf'

    def __init__(self, *args, **kws):
        super(Paragraph, self).__init__(*args, **kws)

    def can_strip(self, parser):
        return Block.NO if parser.line.blank else Block.YES

    def close(self, parser):
        # try parsing the beginning as link reference definitions
        # TODO: after finish the inline parser
        pass

    def consume(self, parser):
        block = parser.parse_rest()
        if block and block.name == 'paragraph':
            for line in block.lines:
                self.lines.append(line)
        else:
            parser.close_unmatched()
            parser.add_child(block)

class ParagraphParser(BlockParser):
    precedence = -1

    @staticmethod
    def parse(parser):
        line = parser.line
        if line.blank:
            return None

        paragraph = BlockFactory.make_block('paragraph', line.line_num, line.next_non_space)
        paragraph.lines.append(line.clean_line)
        return paragraph

#------------------------------------------------------------------------------

class Blank(Block):
    """A blank line"""

    name = 'blank'
    type = 'leaf'

    def __init__(self, *args, **kws):
        super(Blank, self).__init__(*args, **kws)
        self.blank_lines = 1

    def can_strip(self, parser):
        return Block.YES if parser.line.blank else Block.NO

    def _get_content(self):
        return 'Blank: %d blank lines' % self.blank_lines

    def consume(self, parser):
        self.blank_lines += 1

class BlankParser(BlockParser):
    precedence = -2

    @staticmethod
    def parse(parser):
        line = parser.line
        if not line.blank:
            return None

        blank = BlockFactory.make_block('blank', line.line_num, line.next_non_space)
        return blank

#------------------------------------------------------------------------------

class Heading(Block):
    """Heading"""
    name = 'heading'
    type = 'leaf'

    def __init__(self, *args, **kws):
        super(Heading, self).__init__(*args, **kws)
        self.level = 0
        self.lines = []

    def _get_content(self):
        return str(self.level) + ': ' + '|'.join(self.lines)

    def can_strip(self, parser):
        return Block.NO

class SetextHeadingParser(BlockParser):
    re_setext_heading_line = re.compile(r'^(?:=+|-+) *$')

    @staticmethod
    def parse(parser):
        line = parser.line
        container = parser.container
        if line.indented or container.name != 'paragraph':
            return None

        match = SetextHeadingParser.re_setext_heading_line.match(line.after_strip)
        if match is None:
            return None

        # this time, container should == parser.tip

        heading = BlockFactory.make_block('heading', line.line_num, line.next_non_space)
        heading.level = 1 if match.group(0)[0] == '=' else 2
        paragraph = parser.unlink_tail()
        heading.lines = paragraph.lines
        return heading

class AtxHeadingParser(BlockParser):
    re_atx_heading_line = re.compile(r'^#{1,6}(?: +|$)')
    re_trail_hash = re.compile(r' +# *$')

    @staticmethod
    def parse(parser):
        line = parser.line
        container = parser.container
        if line.indented:
            return None

        match = AtxHeadingParser.re_atx_heading_line.match(line.after_strip)
        if match is None:
            return None

        # this time, container should == parser.tip

        line.advance_next_non_space()
        line.advance_offset(len(match.group(0)))

        heading = BlockFactory.make_block('heading', line.line_num, line.next_non_space)
        heading.level = len(match.group(0).strip())

        # remove trailing ###s:
        heading.lines.append(AtxHeadingParser.re_trail_hash.sub('', line.after_strip))

        return heading

#------------------------------------------------------------------------------

class CodeBlock(Block):
    """Code Block"""
    name = 'code-block'
    type = 'leaf'

    def __init__(self, *args, **kws):
        super(CodeBlock, self).__init__(*args, **kws)
        self.lines = []
        self._is_fence = False
        self._fence_length = 0
        self._fence_char = None
        self._fence_offset = 0
        self._fence_option = ''

    def _get_content(self):
        return str(self._fence_char) + ': ' + self._fence_option + '>' + '|'.join(self.lines)

    def can_strip(self, parser):
        line = parser.line
        if self._is_fence:
            # fenced code block
            match = (line.indent <= 3 and line.get_char(line.next_non_space) == self._fence_char
                    and CodeBlock.re_closing_fence.match(line.after_strip))
            if match and len(match.group(0)) >= self._fence_length:
                parser.close(self)
                return Block.CONSUMED
            else:
                # skip optional spaces of fence offset
                for i in range(self._fence_offset):
                    if not is_space_or_tab(line.peek()):
                        break
                    line.advance_offset(1, True)
        else:
            # indented code block
            if line.indent >= CODE_INDENT:
                line.advance_offset(CODE_INDENT, True)
            elif line.blank:
                line.advance_next_non_space()
            else:
                return Block.NO
        return Block.YES

    def consume(self, parser):
        self.lines.append(parser.line.clean_line)

class CodeBlockParser(BlockParser):
    re_open_fence = re.compile(r'`{3,}(?!.*`)|^~{3,}(?!.*~)')
    re_closing_fence = re.compile(r'^(?:`{3,}|~{3,})(?= *$)')

    @staticmethod
    def parse(parser):
        line = parser.line
        if line.indented:
            # try indented code block
            if parser.tip.name == 'paragraph' or line.blank:
                # counted as a lazy line
                return None

            line.advance_offset(CODE_INDENT, True)
            codeblock = BlockFactory.make_block('code-block', line.line_num, line.next_non_space)
            codeblock.lines.append(line.clean_line)
            return codeblock
        else:
            # try fenced code block
            match = CodeBlockParser.re_open_fence.match(line.after_strip)
            if not match:
                return None
            codeblock = BlockParser.make_block('code-block', line.line_num, line.next_non_space)
            codeblock._is_fence = True
            codeblock._fence_length = len(match.group(0))
            codeblock._fence_char = match.group(0)[0]
            codeblock._fence_offset = line.indent
            line.advance_next_non_space()
            line.advance_offset(codeblock._fence_length)
            codeblock._fence_option = line.clean_line
            return codeblock

#------------------------------------------------------------------------------

class ThematicBreak(Block):
    """Thematic Break"""
    name = 'thematic-break'
    type = 'leaf'

    def __init__(self, *args, **kws):
        super(ThematicBreak, self).__init__(*args, **kws)

    def can_strip(self, parser):
        return Block.NO

class ThematicBreakParser(BlockParser):
    re_thematic_break = re.compile(r'^(?:(?:\* *){3,}|(?:_ *){3,}|(?:- *){3,}) *$')

    @staticmethod
    def parse(parser):
        line = parser.line
        if line.indented:
            return None
        match = ThematicBreakParser.re_thematic_break.match(line.after_strip)
        if match is None:
            return match
        thematicbreak = BlockFactory.make_block('thematic-break', line.line_num, line.next_non_space)
        return thematicbreak

#------------------------------------------------------------------------------

class BlockQuote(Block):
    """Block Quote"""
    name = 'block-quote'
    type = 'container'

    def __init__(self, *args, **kws):
        super(BlockQuote, self).__init__(*args, **kws)

    def can_contain(self, block):
        return block.name != 'list-item'

    def can_strip(self, parser):
        line = parser.line
        if line.indented or line.get_char(line.next_non_space) != C_GREATERTHAN:
            return Block.NO

        line.advance_next_non_space()
        line.advance_offset(1)
        if is_space_or_tab(line.peek()):
            line.advance_offset(1, True)
        return Block.YES

class BlockQuoteParser(BlockParser):
    @staticmethod
    def parse(parser):
        line = parser.line
        if line.indented or line.get_char(line.next_non_space) != C_GREATERTHAN:
            return None

        line.advance_next_non_space()
        line.advance_offset(1)

        # optional following space
        if is_space_or_tab(line.peek()):
            line.advance_offset(1, True)

        blockquote = BlockFactory.make_block('block-quote', line.line_num, line.next_non_space)
        return blockquote

#------------------------------------------------------------------------------

class List(Block):
    """A container list block"""

    name = 'list'
    type = 'container'
    def __init__(self, *args, **kws):
        super(List, self).__init__(*args, **kws)
        self.tight = True

    def can_contain(self, block):
        # can contain only 'list-item'
        return block.name == 'list-item'

    def close(self, parser):
        # set the tight status of a list
        for item in self.children:
            if not item.is_tight():
                self.tight = False
                break

    def _get_content(self):
        return 'is_tight: ' + str(self.tight)

#------------------------------------------------------------------------------
class Meta(object):
    """meta info for list marks"""
    def __init__(self):
        self.type          = None
        self.tight         = True
        self.bullet_char   = None
        self.start         = None
        self.delimiter     = None
        self.padding       = None
        self.marker_offset = None

    def __eq__(self, other):
        return (self.type == other.type and
                self.delimiter == other.delimiter and
                self.bullet_char == other.bullet_char)
    def __ne__(self, other):
        return not self == other

    def __str__(self):
        if self.type == 'bullet':
            return self.bullet_char
        elif self.type == 'ordered':
            return str(self.start) + self.delimiter
        else:
            return ''
    def __repr__(self):
        return str(self)

class ListItem(Block):
    """A single list item"""

    name = 'list-item'
    type = 'container'

    def __init__(self, *args, **kw):
        super(ListItem, self).__init__(*args, **kw)

    def can_strip(self, parser):
        line = parser.line
        if line.blank:
            if not self.first_child:
                # blank line after empty list item
                return Block.NO
            else:
                line.advance_next_non_space()

        elif line.indent >= self.meta.marker_offset+ self.meta.padding:
            # | |<--- offset
            #    1.  abc
            #  ->|   |<- padding
            line.advance_offset(self.meta.marker_offset + self.meta.padding)

        else:
            return Block.NO

        return Block.YES

    def can_contain(self, block):
        return block.type != 'list-item'

    def consume(self, parser):
        block = parser.parse_rest()
        if block is not None:
            parser.add_child(block)

    def _get_content(self):
        return str(self.meta)

    def is_tight(self):
        # First case, ends with blank line and has sibling
        if self.sibling and ListItem._ends_with_blank_line(self):
            return False

        # recurse into children of list item, to see if there are spaces between them
        # 1. aaa  <- subitem
        #
        #    bbb  <- subitem
        # 2. ccc
        for item in self.children:
            if ListItem._ends_with_blank_line(item) and item.sibling:
                return False
        return True

    @staticmethod
    def _ends_with_blank_line(block):
        """check if a container block ends with blank line"""
        if block.type == 'container':
            tail = block.tail_child
            ret = tail and tail.name == 'blank'
            if tail and tail.parent.name == 'list-item':
                ret = tail.name == 'blank' and (len(block.children) > 1 or tail.blank_lines > 1)
            return ret
        elif block.name == 'blank':
            return True
        return False


class ListParser(BlockParser):
    re_bullet_list_marker = re.compile(r'^([*+-])')
    re_ordered_list_marker = re.compile(r'^(\d{1,9})([.)])')

    @staticmethod
    def parse(parser):
        if parser.line.indented and parser.tip.name != 'list':
            return None

        meta = ListParser.parse_list_marker(parser)
        if not meta:
            return None

        ret = None
        # add outer list if needed
        container = parser.container
        if container.name != 'list' or container.meta != meta:
            list_block = BlockFactory.make_block('list', parser.line.line_num, parser.line.next_non_space)
            list_block.meta = meta
            ret = list_block

        # add the list item
        list_item = BlockFactory.make_block('list-item', parser.line.line_num, parser.line.next_non_space)
        list_item.meta = meta

        if ret is not None:
            ret.append_child(list_item)
        else:
            ret = list_item

        return ret

    @staticmethod
    def parse_list_marker(parser):
        line = parser.line
        meta = Meta()
        meta.marker_offset = line.indent

        m = ListParser.re_bullet_list_marker.match(line.after_strip)
        if m is not None:
            meta.type = 'bullet'
            meta.bullet_char = m.group(1)
            match = m

        m = ListParser.re_ordered_list_marker.match(line.after_strip)
        if m is not None:
            meta.type = 'ordered'
            meta.start = int(m.group(1))
            meta.delimiter = m.group(2)
            match = m

        if meta.type is None:
            # not matched
            return

        # make sure that we have at least one space after marker or is blank
        next_char = line.get_char(line.next_non_space + len(match.group(0)))
        if not (next_char is None or next_char == C_TAB or next_char == C_SPACE):
            return None

        # got a match, advance cursor over current list marker
        line.advance_next_non_space()
        line.advance_offset(len(match.group(0)), True)

        # calculate padding

        spaces_start_col = line.column
        spaces_start_offset = line.offset

        while line.column - spaces_start_col < 5 and is_space_or_tab(line.peek()):
            line.advance_offset(1, True)

        blank_item = line.peek() == None
        spaces_after_marker = line.column - spaces_start_col
        if spaces_after_marker >= 5 or spaces_after_marker < 1 or blank_item:
            meta.padding = len(match.group(0)) + 1
            # restore the padding movement
            line.column = spaces_start_col
            line.offset = spaces_start_offset
            if is_space_or_tab(line.peek()):
                line.advance_offset(1, True)
        else:
            meta.padding = len(match.group(0)) + spaces_after_marker
        return meta

#------------------------------------------------------------------------------
# HTML Block

try:
    from HTMLParser import HTMLParser
except:
    from html.parser import HTMLParser

class HTMLContentParser(HTMLParser):
    def __init__(self):
        HTMLParser.__init__(self)
        self.first_tag = None
        self.tag_num = 1 # once initialized, it is an open-tag
    def handle_starttag(self, tag, attrs):
        # increase first and then initialize first_tag might seems to be wired
        # but we need this, becuase an open tag might be in several lines
        # and before consuming later lines, starttag is not triggered
        # and once parser is initialized, tag_num is set to 1 already
        if tag == self.first_tag:
            self.tag_num += 1

        if self.first_tag is None:
            self.first_tag = tag

    def handle_endtag(self, tag):
        if tag == self.first_tag:
            self.tag_num -= 1
    def handle_comment(self, data):
        self._set_done()
    def handle_decl(self, data):
        self._set_done()
    def handle_pi(self, data):
        self._set_done()
    def unknown_decl(self, data):
        self._set_done()
    def _set_done(self):
        if self.first_tag is None:
            self.tag_num = -9999 # negtive number will be OK
    @property
    def done(self):
        return self.tag_num < 0

class HTMLBlock(Block):
    """Block level HTML"""

    name = 'html-block'
    type = 'leaf'

    def __init__(self, *args, **kws):
        super(HTMLBlock, self).__init__(*args, **kws)

    def can_strip(self, parser):
        if self.html_parser.done:
            return Block.NO

        # consume another line
        line = parser.line
        self.html_parser.feed(line.after_strip)
        self.lines.append(line.after_strip)
        return Block.CONSUMED

    def _get_content(self):
        return '|'.join(self.lines)


class HTMLBlockParser(BlockParser):
    precedence = 1

    re_html_block_start = re.compile(r'^<(!--|\?|![A-Z]|!\[CDATA\[|[a-zA-Z])')

    @staticmethod
    def parse(parser):
        line = parser.line
        if line.indent > 0:
            # we require HTML block to have no indent at all(0 space)
            return None

        match = HTMLBlockParser.re_html_block_start.match(line.after_strip)
        if match is None:
            return None

        # now we have an HTML start tag
        html_parser = HTMLContentParser()
        html_parser.feed(line.after_strip)

        html_block = BlockFactory.make_block('html-block', parser.line.line_num, parser.line.next_non_space)
        html_block.html_parser = html_parser
        html_block.lines.append(line.after_strip)

        return html_block

#==============================================================================

class BlockFactory(object):
    """docstring for BlockFactory"""

    blocks = {b.name: b for b in Block.__subclasses__()}
    block_parsers = sorted(BlockParser.__subclasses__(), key=lambda b: -b.precedence)

    @staticmethod
    def make_block(tagtype, start_line, start_col):
        try:
            block = BlockFactory.blocks[tagtype]
            return block(start_line = start_line, start_col = start_col)
        except:
            return None

    @staticmethod
    def matched_block(parser):
        """iterate through all block trying to parse current line.

        :returns: the block that consume the line, if no block matches return None.

        """
        for b in BlockFactory.block_parsers:
            ret = b.parse(parser)
            if ret is not None:
                return ret
        return None

#==============================================================================
# Inline Parser

TAGNAME = '[A-Za-z][A-Za-z0-9]*'
ATTRIBUTENAME = '[a-zA-Z_:][a-zA-Z0-9:._-]*'
ATTRIBUTEVALUE = "(?:" + UNQUOTEDVALUE + "|" + SINGLEQUOTEDVALUE + "|" + DOUBLEQUOTEDVALUE + ")"
ATTRIBUTE = "(?:" + "\\s+" + ATTRIBUTENAME + ATTRIBUTEVALUESPEC + "?)"
OPENTAG = "<" + TAGNAME + ATTRIBUTE + "*" + "\\s*/?>"

CLOSETAG = "</" + TAGNAME + "\\s*[>]"
HTMLCOMMENT = "<!--([^-]+|[-][^-]+)*-->"
PROCESSINGINSTRUCTION = "[<][?].*?[?][>]"
DECLARATION = "<![A-Z]+" + "\\s+[^>]*>"
CDATA = "<!\\[CDATA\\[([^\\]]+|\\][^\\]]|\\]\\][^>])*\\]\\]>"

HTMLTAG = "(?:" + OPENTAG + "|" + CLOSETAG + "|" + HTMLCOMMENT + "|" + PROCESSINGINSTRUCTION + "|" + DECLARATION + "|" + CDATA + ")"

ESCAPABLE = '[!"#$%&\'()*+,./:;<=>?@[\\\\\\]^_`{|}~-]'
ESCAPED_CHAR = '\\\\' + ESCAPABLE

REG_CHAR = '[^\\\\()\\x00-\\x20]'



class InlineParser(object):
    """Inline Parser"""
    compiled_re_type = type(re.compile('compiled'))

    re_escapable = re.compile(r'[!"#$%&\'()*+,./:;<=>?@[\\\\\\]^_`{|}~-]')
    re_email_autolink = re.compile(r"^<([a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*)>", re.IGNORECASE)
    re_autolink = re.compile(r"^<[A-Za-z][A-Za-z0-9.+-]{1,31}:[^<>\x00-\x20]*>")
    re_html_tag = re.compile('^' + HTMLTAG, re.IGNORECASE)

    re_whitespacechar = re.compile('^\s')
    re_whitespace = re.compile(r'\s+')
    re_punctuation = re.compile(r"""^[\u2000-\u206F\u2E00-\u2E7F\\'!"#\$%&\(\)\*\+,\-\.\/:;<=>\?@\[\]\^_`\{\|\}~]""")

    re_link_title = re.compile('^(?:"(' + ESCAPED_CHAR + '|[^"\\x00])*"' + '|' + '\'(' + ESCAPED_CHAR + '|[^\'\\x00])*\'' + '|' + '\\((' + ESCAPED_CHAR + '|[^)\\x00])*\\))')
    re_link_destination_braces = re.compile('^(?:[<](?:[^<>\\n\\\\\\x00]' + '|' + ESCAPED_CHAR + '|' + '\\\\)*[>])')
    re_link_destination = re.compile('^(?:' + REG_CHAR + '+|' + ESCAPED_CHAR + '|' + IN_PARENS_NOSP + ')*')
    re_link_label = re.compile('^\\[(?:[^\\\\\\[\\]]|' + ESCAPED_CHAR + '|\\\\){0,1000}\\]')

    def __init__(self):
        self.subject          = ""  # the string literal
        self.label_nest_level = 0
        self.pos              = 0
        self.refmap           = {}
        self.delimiters       = None

    def match(self, regex, reCompileFlag=0):
        """If re matches at current position in the subject, advance the position
        in subject and return the match; otherwise return None"""
        match = None

        if isinstance(regex, InlineParser.compiled_re_type):
            match = regex.search(self.subject[self.pos:])
        else:
            match = re.search(self.subject[self.pos:], flags = reCompileFlag)

        if not match:
            return None

        self.pos += match.end(0)
        return match.group()

    def peek(self):
        """Peek the next character, if no more characters left in the string
        literal, return None"""
        try:
            return self.subject[self.pos]
        except IndexError:
            return None

    def spnl(self):
        """Parse zero or more sapce characters, including at most one newline"""
        self.match(r"^ *(?:\n *)?")
        return True

    # The following methods corresond to the inline types
    # If one element is parsed, add it to array: `block`
    # and return the number of characters parsed.

    def parse_backticks(self, block):
        """Parse backticks, i.e. code spans"""
        start_pos = self.pos
        ticks = self.match(r'^`+')
        if not ticks:
            return 0
        after_open_ticks = self.pos

        match = self.match(self.match(r"`+", re.MULTILINE))
        while match is not None:
            if match == ticks:
                literal = self.subject[after_open_ticks:(self.pos-len(ticks))]
                literal = re.sub(r"[ \n]+", ' ', literal)
                code_span = InlineNode('code')
                code_span._literal = literal.strip()
                block.append_child(code_span)
                return self.pos - start_pos
            match = self.match(self.match(r"`+", re.MULTILINE))

        # code span not closed, interpret it as string
        block.append_child(InlineNode('text', ticks))
        self.pos = after_open_ticks
        return self.pos - start_pos

    def parse_escape(self, block):
        """Parse backslash-escaped special character, adding the escaped character into
        `block`"""
        subj = self.subject
        pos = self.pos
        if subj[pos] == '\\':
            node = None
            advance_pos = 0

            if (len(subj) > pos + 1) and (subj[pos+1] == '\n'):
                node = InlineNode('hard-break')
                advance_pos = 2
            elif InlineParser.match(subj[pos+1:pos+2]):
                node = InlineNode('text', subj[pos+1:pos+2])
                advance_pos = 2
            else:
                node = InlineNode('text', '\\')
                advance_pos = 1

            block.append_child(node)
            self.pos += advance_pos
            return advance_pos

        else:
            return 0

    def parse_autolink(self, block):
        """ Parse an autolink (URL or email)"""
        m_email = self.match(InlineParser.re_email_autolink)
        m_link  = self.match(InlineParser.re_autolink)

        if m_email:
            dest = m_email[1:-1]
            node = InlineNode('link')
            node._destination = 'mailto:' + dest
            node._title = ''
            node.append_child(InlineNode('text', dest))
            block.append_child(node)
            return len(m_email)
        elif m_link:
            dest = m_link[1:-1]
            node = InlineNode('link')
            node._destination = dest
            node._title = ''
            node.append_child(InlineNode('text', dest))
            block.append_child(node)
            return len(m_link)
        else:
            return 0

    def parse_html_tag(self, block):
        """parse a raw HTML tag"""
        match = self.match(InlineParser.re_html_tag)
        if match:
            block.append_child(InlineNode('html-inline', match))
            return len(match)
        else:
            return 0

    def _scanDelims(self, char):
        """Scan a sequence of characters == char, and return information about
        the number of delimiters and whether they are positioned such that they
        can open and/or close emphasis or strong emphasis. A utility function for
        strong/emph parsing"""
        num_delims         = 0
        first_close_delims = 0
        char_before        = char_after = None
        start_pos          = self.pos

        char_before = '\n' if self.pos == 0 else self.subject[self.pos - 1]

        if char == C_SINGLEQUOTE or char == C_DOUBLEQUOTE:
            num_delims += 1
            self.pos   += 1
        else:
            while self.peek() == char:
                num_delims += 1
                self.pos += 1

        if num_delims == 0:
            return None

        char_after = self.peek()
        char_after = char_after if char_after else '\n'

        after_is_whitespace = InlineParser.re_whitespacechar.match(char_after)
        after_is_punctuation = InlineParser.re_punctuation.match(char_after)
        before_is_whitespace = InlineParser.re_whitespacechar.match(char_before)
        before_is_punctuation = InlineParser.re_punctuation.match(char_before)

        left_flanking = (not after_is_whitespace) and not (after_is_punctuation and
                not before_is_whitespace and not before_is_punctuation)
        right_flanking = (not before_is_whitespace) and not (before_is_punctuation and
                not after_is_whitespace and not after_is_punctuation)

        can_open = left_flanking
        can_close = right_flanking

        if char == C_UNDERSCORE:
            can_open = left_flanking and (not right_flanking or before_is_punctuation)
            can_close = right_flanking and (not left_flanking or after_is_punctuation)
        elif char == C_SINGLEQUOTE or char == C_DOUBLEQUOTE:
            can_open = left_flanking and not right_flanking
            can_close = right_flanking

        self.pos = start_pos
        return {"num_delims": num_delims, "can_open": can_open, "can_close": can_close }

    def parse_delim(self, block, char):
        """Handle a delimiter marker for emphasis or a quote"""
        ret = self._scanDelims(char)
        if not ret:
            return None

        num_delims = ret['num_delims']
        start_pos = self.pos
        contents = '';

        self.pos += num_delims

        if char == "'":
            contents = "\u2019"
        elif char == '"':
            contents = '\u201c'
        else:
            contents = self.subject[start_pos, self.pos]

        node = InlineNode('text', contents)
        block.append_child(node)
        self.delimiters = {
            "char": char,
            "num_delims": num_delims,
            "node": node,
            "prev": self.delimiters,
            "next": None,
            "can_open": can_open,
            "can_close": can_close,
            "active": True}

        if self.delimiters["prev"] is not None:
            self.delimiters["prev"]["next"] = self.delimiters

        return True

    def _remove_delim(self, delim):
        if delim["prev"] is not None:
            delim["prev"]["next"] = delim['next']

        if delim['next'] is None:
            self.delimiters = delim['prev']
        else:
            delim['next']['prev'] = delim['prev']

    def _remove_delim_between(self, bottom, top):
        if bottom['next'] != top:
            bottom['next'] = top
            top['prev'] = bottom

    def process_emphasis(self, stack_bottom):
        # TODO
        pass

    def parse_link_title(self):
        """Parse link title, return the String or None if not matched"""
        title = self.match(InlineParser.re_link_title)
        return unescape(title[1:len(title)-1]) if title is not None else None

    def parse_link_destination(self):
        """Parse link destination, returning the string or None if not matched"""
        res = self.match(InlineParser.re_link_destination_braces)
        if res is not None:
            return unescape(res[1:len(res)-1])
        else:
            res2 = self.match(InlineParser.re_link_destination)
            return unescape(res2) if res2 is not None else None

    def parse_link_label(self):
        """parse a link label, return the number of characters parsed"""
        label = self.match(InlineParser.re_link_label)
        return 0 if (label is None or len(label) > 1001) else len(label)

    def parse_open_bracket(self, block):
        """Add open bracket to delimiter stack and add a text node to block's children"""
        start_pos = self.pos
        self.pos += 1

        node = InlineNode('text', '[')
        block.append_child(node)
        self.delimiters = {
            "char": '[',
            "num_delims": 1,
            "node": node,
            "prev": self.delimiters,
            "next": None,
            "can_open": True,
            "can_close": False,
            "index": start_pos,
            "active": True}

        if self.delimiters["prev"] is not None:
            self.delimiters["prev"]["next"] = self.delimiters

        return True

    def parse_bang(self, block):
        """If the next character is '[', then it is an start of image link"""
        start_pos = self.pos
        if self.peek() == '[':
            self.pos += 1

            node = InlineNode('text', '![')
            block.append_child(node)
            self.delimiters = {
                "char": '!',
                "num_delims": 1,
                "node": node,
                "prev": self.delimiters,
                "next": None,
                "can_open": True,
                "can_close": False,
                "index": start_pos+1,
                "active": True}

            if self.delimiters["prev"] is not None:
                self.delimiters["prev"]["next"] = self.delimiters
        else:
            block.append_child(InlineNode('text', '!'))
        return True

    def parse_close_bracket(self, block):
        """Try to match close bracket against an opening in the delimiter
        stack. Add either a link or image, or a plain [ character to the
        block's children, If there is a matching delimiter
        remove it from the delimiter stack"""
        self.pos += 1
        start_pos = self.pos

        # look through the stack of delimiters for '[' or '!['
        opener = self.delimiters
        while opener is not None:
            if opener['char'] == '[' or opener['char'] == '!':
                break;
            opener = opener['prev']

        # not matched, just return a literal
        if opener is None:
            block.append_child(InlineNode('text', ']'))
            return True

        if not opener['active']:
            block.append_child(InlineNode('text', ']'))
            self._remove_delim(opener)
            return True

        is_image = opener['char'] == '!'

        if self.peek() == '(':
            # inline link []() or image: ![]()
            self.pos += 1

            self.spnl()
            dest = self.parse_link_destination()

            if dest and self.spnl() and InlineParser.re_whitespacechar.match(self.subject[self.pos-1]):
                # parse and save title
                title = self.parse_link_title()
                self.spnl()

                if self.peek() == ')':
                    self.pos += 1
                    matched = True
        else:
            # check if there is a label: [text][ref-label]
            save_pos = self.pos
            before_label = self.pos

            length = self.parse_link_label()
            if length == 0 or length == 2:
                # empty or missing second label
                ref_label = self.subject[opener['index'], start_pos]
            else:
                ref_label = self.subject[before_label, before_label + length]

            if length == 0:
                # [] shortcut reference link
                self.pos = save_pos

            # lookup raw label in refmap
            try:
                link = self.refmap[ref_label]
                dest = link.destination
                title = link.title
                matched = True
            except:
                pass

        if matched:
            node = InlineNode('image' if is_image else 'link')
            node._destination = dest
            node._title = title or '';

            tmp = opener['node'].sibling
            while tmp:
                next = tmp.sibling

    def parse_inline(self, block):
        """Parse the next inline element in subject, advancing subject position
        On success, add the result to block, and return True
        On failure, return False"""
        ret = False
        char = self.peek()

        if char is None:
            return False

        if char == '\n':
            ret = self.parse_newline(block)
        elif char == '\\':
            ret = self.parse_escape(block)
        elif char == '`':
            ret = self.parse_backticks(block)
        elif char == '*' or char == '_':
            ret = self.parse_delim(block, char)
        elif char == "'" or char == '"':
            # don't support this for now
            ret = False
        elif char == '[':
            ret = self.parse_open_bracket(block)
        elif char == '!':
            ret = self.parse_bang(block)
        elif char == ']':
            ret = self.parse_close_bracket(block)
        elif char == '<':
            ret = self.parse_autolink(block) or self.parse_html_tag(block)
        elif char == '&':
            ret = self.parse_entity(block)
        else:
            ret = self.parse_string(block)

        if not ret:
            self.pos += 1
            block.append_child(InlineNode('text', char))

        return True

#==============================================================================
x = Parser()
string = """
1.
2. abc
> x
1. <div
     >aaa
2.   >aaa

1. a

       x
   c

   > ---
1. > 1. a
bbb
   >
   > 1. b
   >c
> aaa
c
> bbb
## ATX Level 2 Heading
1. a
   # a
   b
a


   b
   2. b

      3. c
4. d

First heading
====
Second Heading
---
aaaaaaa"""
x.parse(string)
print(x.doc)

