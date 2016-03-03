#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re

#==============================================================================
# Globals
CODE_INDENT = 4

C_SPACE = ' '
C_TAB = '\t'

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
        self.indented = self.indent > CODE_INDENT

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


#==============================================================================
# Various blocks

class Block(Node):
    """block"""
    YES      = 0
    NO       = 1
    CONSUMED = 2

    name = 'block'
    precendence = 0 # the less the later

    def __init__(self, *args, **kws):
        super(Block, self).__init__(*args, **kws)
        self.last_line_blank = False

    @staticmethod
    def make_block(tagtype, start_line, start_col):
        for block in Block.__subclasses__():
            if block.name == tagtype:
                return block(start_line = start_line, start_col = start_col)
        return None

    @staticmethod
    def matched_block(parser):
        """iterate through all block trying to parse current line.

        :returns: the block that consume the line, if no block matches return None.

        """
        blocks = [b for b in Block.__subclasses__() if hasattr(b, 'try_parsing')]

        for b in sorted(blocks, key=lambda b: -b.precendence):
            ret = b.try_parsing(parser)
            if ret is not None:
                return ret
        return None

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

    def ends_with_blank_line(self):
        """Check if current block ends with blank line, descending if needed for lists

        :returns: True if it ends with blank line, else False

        """
        if self.last_line_blank:
            return True
        elif self.name == 'list' or self.name == 'list-item':
            last_child = self.last_child
            return last_child.last_line_blank if last_child else False
        else:
            return False

    def consume(self, parser):
        """Consume current line"""
        parser.parse_rest()

    def _get_content(self):
        return ''

    def _repr(self, level=0):
        ret = []
        ret.append('    '*level + '%s[%d, %d, %d, %d] [%s]' % (self.name,self.start_line, self.start_col, self.end_line, self.end_col, self._get_content()))
        for child in self.children:
            ret.append(child._repr(level+1))
        return '\n'.join(ret)

    def __repr__(self):
        return self._repr()

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


class Paragraph(Block):
    """A paragraph"""

    name = 'paragraph'
    type = 'leaf'
    precendence = -1
    def __init__(self, *args, **kws):
        super(Paragraph, self).__init__(*args, **kws)
        self.lines = []

    def can_strip(self, parser):
        return Block.NO if parser.line.blank else Block.YES

    def close(self, parser):
        pass

    def append_line(self, line):
        self.lines.append(line)

    def _get_content(self):
        return '|'.join(self.lines)

    @staticmethod
    def try_parsing(parser):
        line = parser.line

        if line.blank:
            return None

        paragraph = Block.make_block('paragraph', line.line_num, line.next_non_space)
        paragraph.append_line(line.clean_line)
        return paragraph

    def consume(self, parser):
        block = parser.parse_rest()
        if block and block.name == 'paragraph':
            for line in block.lines:
                self.append_line(line)
        else:
            parser.tip = parser.oldtip
            parser.add_child(block)

class Blank(Block):
    """A paragraph"""

    name = 'blank'
    type = 'leaf'
    precendence = -2
    def __init__(self, *args, **kws):
        super(Blank, self).__init__(*args, **kws)

    def can_strip(self, parser):
        return Block.YES if parser.line.blank else Block.NO

    def _get_content(self):
        return 'Blank'

    @staticmethod
    def try_parsing(parser):
        line = parser.line

        if not line.blank:
            return None

        blank = Block.make_block('blank', line.line_num, line.next_non_space)
        return blank

    def consume(self, parser):
        pass

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
        self.tight = True

        for item in self.children:
            if item.ends_with_blank_line() and item.sibling:
                self.tight = False
                break

            # recurse into children of list item, to see if there are spaces between them
            # 1. aaa  <- subitem
            #
            #    bbb  <- subitem
            # 2. ccc
            for subitem in item.children:
                if subitem.ends_with_blank_line() and subitem.sibling and item.sibling:
                    self.tight = False
                    break



class ListItem(Block):
    """A single list item"""

    name = 'list-item'
    type = 'container'
    re_bullet_list_marker = re.compile(r'^([*+-])')
    re_ordered_list_marker = re.compile(r'^(\d{1,9})([.)])')

    def __init__(self, *args, **kw):
        super(ListItem, self).__init__(*args, **kw)

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

    @staticmethod
    def try_parsing(parser):
        if parser.line.indented and parser.tip.name != 'list':
            return None

        meta = ListItem.parse_list_marker(parser)
        if not meta:
            return None

        ret = None
        # add outer list if needed
        if parser.tip.name != 'list' or parser.tip.meta != meta:
            list_block = Block.make_block('list', parser.line.line_num, parser.line.next_non_space)
            list_block.meta = meta
            ret = list_block

        # add the list item
        list_item = Block.make_block('list-item', parser.line.line_num, parser.line.next_non_space)
        list_item.meta = meta

        if ret is not None:
            ret.append_child(list_item)
        else:
            ret = list_item

        return ret

    @staticmethod
    def parse_list_marker(parser):
        line = parser.line
        meta = ListItem.Meta()
        meta.marker_offset = line.indent

        m = ListItem.re_bullet_list_marker.match(line.after_strip)
        if m is not None:
            meta.type = 'bullet'
            meta.bullet_char = m.group(1)
            match = m

        m = ListItem.re_ordered_list_marker.match(line.after_strip)
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

    def _get_content(self):
        return str(self.meta)

#==============================================================================
# Parser

class Parser(object):
    """parse state"""
    def __init__(self):
        super(Parser, self).__init__()
        self.line_num               = 0
        self.doc                    = Block.make_block('document', 0, 0)
        self.last_matched_container = None
        self.tip                    = self.doc # inner most block

    def close(self, block):
        block.end_line = self.line.line_num
        block.end_col = len(self.line.line)
        block.is_open = False
        block.close(self)
        self.tip = block.parent

    def close_unmatched(self):
        if not self.all_closed:
            while self.oldtip != self.last_matched_container:
                parent = self.oldtip.parent
                self.close(self.oldtip)
                self.oldtip = parent
            self.all_closed = True

    def add_child(self, block):
        while not self.tip.can_contain(block):
            self.close(self.tip)

        self.tip.append_child(block)
        self.tip = block.tail_child

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

        # close blocks that are not matched
        self.all_closed = container == self.oldtip
        self.last_matched_container = container

        # Now the line is striped, parse it as a normal unindented line

        if self.tip.name == 'paragraph' or self.tip.name == 'fence':
            self.oldtip = container
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

        # use try_parsing to get the block.
        # use block's

        block = Block.matched_block(self)
        if block is None:
            return

        if block.type == 'leaf':
            return block
        elif block.type == 'container':
            # the line header is already consumed
            child = self.parse_rest()
            if child is not None:
                block.append_tail(child)
            return block

x = Parser()
x.parse_line('1. a')
x.parse_line('a')
x.parse_line('')
x.parse_line('')
x.parse_line('   b')
x.parse_line('   2. b')
x.parse_line('       ')
x.parse_line('      3. c')
x.parse_line('4. d')
print(x.doc)
