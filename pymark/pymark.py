#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re

#==============================================================================
# Globals
CODE_INDENT = 4

C_SPACE = ' '
C_TAB = '\t'
C_GREATERTHAN = '>'

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
        self.tip.unlink()
        self.tip = parent.tail_child if parent.tail_child else parent
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
        self.parent      = parent
        self.first_child = None
        self.last_child  = None
        self.prv         = None
        self.nxt         = None

        self.start_line  = start_line
        self.start_col   = start_col
        self.end_line    = end_line
        self.end_col     = end_col

    @property
    def tail_child(self):
        container = self
        while container.last_child:
            container = container.last_child
        return container

    @property
    def sibling(self):
        """Get the next sibling of a node"""
        return self.nxt

    def append_child(self, node):
        node.unlink()
        node.parent = self
        if self.last_child:
            self.last_child.nxt = node
            node.prv = self.last_child
            self.last_child = node
        else:
            self.first_child = node
            self.last_child = node

    def prepend_child(self, node):
        node.unlink()
        node.parent = self
        if self.first_child:
            self.first_child.prv = node
            node.nxt = self.first_child
            self.first_child = node
        else:
            self.first_child = node
            self.last_child = node

    def unlink(self):
        if self.prv:
            self.prv.nxt = self.nxt
        elif self.parent:
            self.parent.first_child = self.nxt

        if self.nxt:
            self.nxt.prv = self.prv
        elif self.parent:
            self.parent.last_child = self.prv

        self.prv   = None
        self.nxt   = None
        self.parent = None

    def append_tail(self, node):
        container = self
        while container.last_child:
            container = container.last_child
        container.append_child(node)

    def insert_after(self, sibling):
        sibling.unlink()
        sibling.nxt = self.nxt
        if sibling.nxt:
            sibling.nxt.prv = sibling

        sibling.prv = self
        self.nxt = sibling
        sibling.parent = self.parent
        if sibling.nxt is None:
            sibling.parent.last_child = sibling

    def insert_before(self, sibling):
        sibling.unlink()
        sibling.prv = self.prv
        if sibling.prv:
            sibling.prv.nxt = sibling
        sibling.nxt = self
        self.prv = sibling
        sibling.parent = self.parent
        if sibling.prv is None:
            sibling.parent.first_child = sibling

    def __iter__(self):
        self.cursor = self.first_child
        return self

    def next(self):
        """Iterate through all its children"""
        if self.cursor:
            ret = self.cursor
            self.cursor = self.cursor.nxt
            return ret
        else:
            raise StopIteration

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
        self.is_open     = True

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
        for child in self:
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
        for item in self:
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
        if self.nxt and ListItem._ends_with_blank_line(self):
            return False

        # recurse into children of list item, to see if there are spaces between them
        # 1. aaa  <- subitem
        #
        #    bbb  <- subitem
        # 2. ccc
        for item in self:
            if ListItem._ends_with_blank_line(item) and item.nxt:
                return False
        return True

    @staticmethod
    def _ends_with_blank_line(block):
        """check if a container block ends with blank line"""
        if block.type == 'container':
            tail = block.tail_child
            ret = tail and tail.name == 'blank'
            if tail and tail.parent.name == 'list-item':
                ret = tail.name == 'blank' and ((block.first_child != block.last_child) or tail.blank_lines > 1)
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

class Content(object):
    """Utility Class for string content"""
    compiled_re_type = type(re.compile('compiled'))

    def __init__(self, string):
        self.string = string
        self.pos = 0

    def peek(self, offset=0):
        """peek the character `offset` after on the current string"""
        try:
            return self.string[self.offset + offset]
        except:
            return None

    def get_char(self, offset=0):
        """get the character on the current string at `offset`"""
        try:
            return self.string[offset]
        except:
            return None

    @property
    def rest(self):
        """Get the rest string content"""
        return self.string[self.offset:]

    def is_end(self):
        return self.pos >= len(string)

    def advance(self, num):
        self.pos += num

    def match(self, regex, reCompileFlag=0):
        """If re matches at current position in the subject, advance the position
        in string content and return the match; otherwise return None"""
        match = None

        if isinstance(regex, Content.compiled_re_type):
            match = regex.search(self.string[self.pos:])
        else:
            match = re.search(self.string[self.pos:], flags = reCompileFlag)

        if not match:
            return None

        self.pos += match.end(0)
        return match.group()


class InlineRule(object):
    """Base Class for rules that parses inline elements"""
    def parse(self, parser, content, side_effect=True):
        """Try to parse the content, return None if cannot parse current situation
        Else return an InlineNode.

        The parse method should modify the content's position accordingly

        If `side_effect` is False, do not do anything but modify the content's cursor
        position."""
        return None


class InlineParser(object):
    """Inline Parser"""
    def __init__(self):
        pass

    def parse_content(self, string):
        """The main entry for inline parser, return a InlineNode whose childrens are the parsed
        result"""
        content = Content(string)
        parser.node = InlineNode('parent')

        while not content.is_end():
            for rule in self.rules:
                node = rule.parse(self, content)
                if node is not None:
                    ret.append_child()
                    break

        # in the end, process
        for rule in reversed(self.rules):
            if hasattr(rule, 'post_process'):
                rule.post_process(self)

        return parser.node

    def skip_token(self, content):
        """parse the content with all rules, but do not do all the side effects.
        Move the cursor forward"""

        # TODO: later add cache to enhance performance

        while not content.is_end():
            for rule in self.rules:
                node = rule.parse(self, content, side_effect=False)
                if node is not None:
                    break



#------------------------------------------------------------------------------
# Common regex

ESCAPABLE = '[!"#$%&\'()*+,./:;<=>?@[\\\\\\]^_`{|}~-]'

re_whitespace_char = re.compile(r'\s')
re_whitespace = re.compile(r'\s+')

#------------------------------------------------------------------------------
# Helper functions
def normalize_uri(uri):
    # TODO: implement this
    return uri

#------------------------------------------------------------------------------
# Rule: Escape characters

re_escapable = re.compile('^' + ESCAPABLE)

class RuleEscape(InlineRule):
    """Parse Escaped characters, return either the escaped character, a hard
    line break, or a literal backslash."""

    def parse(self, parser, content, side_effect=True):
        if content.peek() != '\\':
            return None

        content.advance(1)

        next_char = content.peek()
        if next_char == '\n':
            content.advance(1)
            return InlineNode('hard-break')
        elif next_char is not None and content.match(re_escapable):
            content.advance(1)
            return InlineNode('text', next_char)
        else:
            return InlineNode('text', '\\')

#------------------------------------------------------------------------------
# Rule: Code Span

re_ticks = re.compile(r'`+')
re_ticks_here = re.compile(r'^`+')

class RuleCodeSpan(InlineRule):
    """Parse backticks as code span"""

    def parse(self, parser, content, side_effect=True):
        ticks = content.match(re_ticks_here)
        if ticks is None:
            return None

        after_open_ticks = content.pos

        # find the closing ticks
        matched = content.match(re_ticks)
        while matched is not None:
            if matched == ticks:
                # found matched
                node = InlineNode('code')
                literal = content.string[after_open_ticks:self.pos - len(ticks)]
                literal = re_whitespace.sub(' ', literal.strip())
                node._literal = literal
                return node
            matched = content.match(re_ticks)

        # not matched, resume position
        self.pos = after_open_ticks
        return InlineNode('text', ticks)

#------------------------------------------------------------------------------
# Rule: Auto Link

re_email_autolink = re.compile(
    r"^<([a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+@[a-zA-Z0-9]"
    r"(?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?"
    r"(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*)>")
re_autolink = re.compile(
    r'^<[A-Za-z][A-Za-z0-9.+-]{1,31}:[^<>\x00-\x20]*>',
    re.IGNORECASE)

class RuleAutolink(InlineRule):
    """parse auto link"""

    def parse(self, parser, content, side_effect=True):
        m_email = content.match(re_email_autolink)
        match = None
        if m_email:
            # match email
            match = m_email
        else:
            m_link = content.match(re_autolink)
            if m_link:
                match = m_link
            else:
                return None

        dest = match[1:-1]
        node = InlineNode('link')
        node._destination = normalize_uri('mailto:' + dest)
        node._title = ''
        node.append_child(InlineNode('text', dest))
        return node

#------------------------------------------------------------------------------
# Rule: HTML TAG

TAGNAME = '[A-Za-z][A-Za-z0-9-]*'
ATTRIBUTENAME = '[a-zA-Z_:][a-zA-Z0-9:._-]*'
UNQUOTEDVALUE = "[^\"'=<>`\\x00-\\x20]+"
SINGLEQUOTEDVALUE = "'[^']*'"
DOUBLEQUOTEDVALUE = '"[^"]*"'
ATTRIBUTEVALUE = "(?:" + UNQUOTEDVALUE + "|" + SINGLEQUOTEDVALUE + \
    "|" + DOUBLEQUOTEDVALUE + ")"
ATTRIBUTEVALUESPEC = "(?:" + "\\s*=" + "\\s*" + ATTRIBUTEVALUE + ")"
ATTRIBUTE = "(?:" + "\\s+" + ATTRIBUTENAME + ATTRIBUTEVALUESPEC + "?)"
OPENTAG = "<" + TAGNAME + ATTRIBUTE + "*" + "\\s*/?>"
CLOSETAG = "</" + TAGNAME + "\\s*[>]"
HTMLCOMMENT = '<!---->|<!--(?:-?[^>-])(?:-?[^-])*-->'
PROCESSINGINSTRUCTION = "[<][?].*?[?][>]"
DECLARATION = "<![A-Z]+" + "\\s+[^>]*>"
CDATA = '<!\\[CDATA\\[[\\s\\S]*?\\]\\]>'
HTMLTAG = "(?:" + OPENTAG + "|" + CLOSETAG + "|" + HTMLCOMMENT + "|" + \
    PROCESSINGINSTRUCTION + "|" + DECLARATION + "|" + CDATA + ")"
re_html_tag = re.compile('^' + HTMLTAG, re.IGNORECASE)


class RuleHTMLTag(InlineRule):
    """parse inline HTML tag"""

    def parse(self, parser, content, side_effect=True):
        match = content.match(re_html_tag)
        if match is None:
            return None
        else:
            node = InlineNode('html-inline')
            node._literal = match
            return node

#------------------------------------------------------------------------------
# Rule: Delimiters, i.e. *em* **strong**

re_punctuation = re.compile(
    r'^[\u2000-\u206F\u2E00-\u2E7F\\' + "'" + '!"#\$%&\(\)'
    r'\*\+,\-\.\/:;<=>\?@\[\]\^_`\{\|\}~]')

class RuleDelimiter(InlineRule):
    """parse delimiter"""

    @staticmethod
    def _scanDelims(content, char):
        """Scan a sequence of characters == char, and return information about
        the number of delimiters and whether they are positioned such that they
        can open and/or close emphasis or strong emphasis. A utility function for
        strong/emph parsing"""
        num_delims         = 0
        first_close_delims = 0
        char_before        = char_after = None
        start_pos          = content.pos

        char_before = '\n' if content.pos == 0 else content.get_char(content.pos - 1)

        if char == C_SINGLEQUOTE or char == C_DOUBLEQUOTE:
            num_delims += 1
            content.pos += 1
        else:
            while content.peek() == char:
                num_delims += 1
                content.pos += 1

        if num_delims == 0:
            return None

        char_after = content.peek()
        char_after = char_after if char_after else '\n'

        after_is_whitespace   = re_whitespacechar.match(char_after)
        after_is_punctuation  = re_punctuation.match(char_after)
        before_is_whitespace  = re_whitespacechar.match(char_before)
        before_is_punctuation = re_punctuation.match(char_before)

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

        content.pos = start_pos
        return {"num_delims": num_delims, "can_open": can_open, "can_close": can_close }

    @staticmethod
    def _remove_delimiter(parser, delim):
        if delim.get('prev') is not None:
            delim['prev']['next'] = delim.get('next')
        if delim.get('next') is None:
            # top of stack
            parser.delimiters = delim.get('previous')

    @staticmethod
    def _remove_delimiter_between(bottom, top):
        nxt = bottom.get('next')
        if bottom.get('next') != top:
            bottom['next'] = top
            top['prev'] = bottom

        # free all the delimiters between, so that they can be GC-ed
        while nxt is not None and nxt != top:
            tmp = nxt['next']
            nxt['prev'] = None
            nxt['next'] = None
            nxt = tmp

    def parse(self, parser, content, side_effect=True):
        char = content.peek()
        if char != '*' and char != '_':
            return None

        res = RuleDelimiter._scanDelims(content, char)
        if res is None:
            return res

        num_delims = res.get('num_delims')
        start_pos = content.pos
        content.advance(num_delims)
        contents = content.string[start_pos:content.pos]
        node = InlineNode('text', contents)

        if not side_effect:
            return node

        parser.delimiters = {
            'char': char,
            'num_delims': num_delims,
            'node': node,
            'prev': getattr(parser, 'delimiters'),
            'next': None,
            'can_open': res.get('can_open'),
            'can_close': res.get('can_close'),
            'active': True,
         }

        if parser.delimiters['prev'] is not None:
            parser.delimiters['prev']['next'] = parser.delimiters

        return node

    def post_process(self, parser, stack_bottom=None):
        """correctly close the delimiters"""

        opener_bottom = {
                '_': stack_bottom,
                '*': stack_bottom,
                }

        use_delims = 0

        # find first closer, i.e find the stack bottom
        closer = parser.delimiters
        while closer is not None and closer.get('prev') != stack_bottom:
            closer = closer.get('prev')

        # move forward, looking for closers, and handling each
        while closer is not None:
            closer_char = closer.get('char')
            if not (closer.get('can_close') and (closer_char == '_' or closer_char == '*')):
                closer = closer.get('next')
                continue

            # found emphasis closer, now look back for first matching
            # opener
            opener = closer.get('prev')
            opener_found = False
            while (opener is not None and opener != stack_bottom and opener != opener_bottom[closer_char]):
                if opener.get('char') == closer_char and opener.get('can_open'):
                    opener_found = True
                    break
                opener = opener.get('prev')
            old_closer = closer

            if closer_char == '*' or closer_char == '_':
                if not opener_found:
                    closer = closer.get('next')
                    # set lower bound for future searches for openers
                    openers_bottom[closer_char] = old_closer['prev']
                    if not old_closer['can_open']:
                        RuleDelimiter._remove_delimiter(old_closer)
                    continue

                # calculate actual number of delimiters used by closer
                if closer['num_delims'] < 3 or opener['num_delims'] < 3:
                    if closer['num_delims'] < opener['num_delims']:
                        use_delims = closer['num_delims']
                    else:
                        use_delims = opener['num_delims']
                else:
                    if closer['num_delims'] % 2 == 0:
                        use_delims = 2
                    else:
                        use_delims = 1

                opener_inl = opener.get('node')
                closer_inl = closer.get('node')

                # remove used delimiters from stack elements and inlines
                opener['num_delims'] -= use_delims
                closer['num_delims'] -= use_delims
                opener_inl.literal = opener_inl._literal[:len(opener_inl._literal)-use_delims]
                closer_inl.literal = closer_inl._literal[:len(closer_inl._literal)-use_delims]

                # build contents for new emph element
                if use_delims == 1:
                    emph = InlineNode('emph')
                else:
                    emph = InlineNode('Strong')

                # move the nodes to emph element
                tmp = opener_inl.nxt
                while tmp and tmp != closer_inl:
                    nxt = tmp.nxt
                    tmp.unlink()
                    emph.append_child(tmp)
                    tmp = nxt

                opener_inl.insert_after(emph)

                # remove delimiters between opener and closer from stack
                RuleDelimiter._remove_delimiter_between(opener, closer)

                # remove opener and closer if they have 0 delimiters now
                if opener['num_delims'] == 0:
                    opener_inl.unlink()
                    RuleDelimiter._remove_delimiter(opener)

                if closer['num_delims'] == 0:
                    closer_inl.unlink()

                    # closer will be used for next iterator
                    tempstack = closer['next']
                    RuleDelimiter._remove_delimiter(closer)
                    closer = tempstack
                continue
            else:
                # can handle other delimiters here, for example quote
                # or strike through
                # TODO: think about how to add rules dynamically
                pass

        # Remove all delmiters
        while parser.delimiters is not None and parser.delimiters != stack_bottom:
            # should remove them one by one, otherwise they will not be GC-ed
            RuleDelimiter._remove_delimiter(parser, parser.delimiters)

#------------------------------------------------------------------------------
# Rule: Link

def parse_link_label(parser, content):
    """parse link's label in format: [label] (link), return the position of the end
    of label, None if cannot be satisfied"""
    start_pos = content.pos

    if content.peek() != '[':
        return None
    content.advance(1)
    level = 1
    ret = None

    while not content.is_end():
        if content.peek() == ']':
            content.advance(1)
            level -= 1
            if level == 0:
                break

        parser.skip_token(content)
        if content.peek() == '[':
            level += 1

    if level == 0:
        ret = content.string[start_pos+1:content.pos-1]
    else:
        # restore old state
        content.pos = start_pos
    return ret


re_link_title = re.compile(
    '^(?:"(' + ESCAPED_CHAR + '|[^"\\x00])*"' +
    '|' +
    '\'(' + ESCAPED_CHAR + '|[^\'\\x00])*\'' +
    '|' +
    '\\((' + ESCAPED_CHAR + '|[^)\\x00])*\\))')

def parse_link_title(parser, content):
    """link's format: [label](link_destination "title")"""
    title = content.match(re_link_title)
    return unescape_string(title[1:-1]) if title else None

re_link_destination_braces = re.compile(
    '^(?:[<](?:[^ <>\\t\\n\\\\\\x00]' + '|' + ESCAPED_CHAR + '|' +
    '\\\\)*[>])')
re_link_destination = re.compile(
    '^(?:' + REG_CHAR + '+|' + ESCAPED_CHAR + '|\\\\|' +
    IN_PARENS_NOSP + ')*')

def parse_link_destination(parser, content):
    """link's format: [label](link_destination "title")"""
    description = content.match(re_link_destination_braces)
    if description is None:
        description = content.match(re_link_destination)
    else:
        # chop off <..>
        description = description[1:-1]

    return normalize_uri(description) if description else None





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

