#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re

#==============================================================================
# Globals
CODE_INDENT = 4

#==============================================================================
# Helpers

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

        indent = re_tab_space.match(self.line[self.offset:])
        indent = indent if indent else ''

        self.blank = len(indent) >= len(self.line[self.offset:])

        self.next_non_space = self.offset + len(indent)
        self.next_non_space_col = len(indent.replace('\t', '    '))
        self.indent = self.next_non_space_col - self.column
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

    def peek(self, offset=None):
        """peek the character on the current line at `offset`"""
        try:
            return self.line[self.offset if not offset else offset]
        except:
            return None

#==============================================================================
# Node & Block

class Node(object):
    """A tree node"""
    def __init__(self, parent=None, start_line=1, start_col=0, end_line=0, end_col=0):
        self.parent     = parent
        self.children   = []
        self.is_open    = False
        self.lines      = []
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
    def sibling(self):
        """Get the next sibling of a node"""
        try:
            return self.parent.children[self._index + 1]
        except:
            return None

    def append_child(self, node):
        """append a child"""
        node._index = len(self.children)
        self.children.append(node)

#==============================================================================
# Various blocks

class Block(Node):
    """block"""
    YES      = 0
    NO       = 1
    CONSUMED = 2

    name = 'block'

    block_starts = [ListItem]

    def __init__(self, *args, **kws):
        super(Block, self).__init__(*args, **kws)
        self.last_line_blank = False

    @staticmethod
    def make_block(tagtype, start_line, start_col):
        for block in block_starts:
            if block.name == tagtype:
                return block(start_line = start_line, start_col = start_col)
        return None

    @staticmethod
    def matched_block(parser):
        """iterate through all block trying to parse current line.

        :returns: the block that consume the line, if no block matches return None.

        """
        for block in Block.__subclasses__():
            ret = block.try_parsing(parser)
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
        return YES

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
            return last_child.ends_with_blank_line if last_child else False
        else:
            return False


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

class List(Block):
    """A container list block"""

    name = 'list'
    type = 'container'
    def __init__(self, *args, **kws):
        super(List, self).__init__(*args, **kws)
        self.tight = True

    def can_contain(self, block):
        # can contain only 'list-item'
        return block == 'list-item'

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
    def __init__(self, *args, **kw):
        super(ListItem, self).__init__(*args, **kw)

    def can_strip(self, parser):
        line = parser.line
        if line.blank:
            if not self.first_child:
                # blank line after empty list item
                return NO
            else:
                line.advance_next_non_space()

        elif line.indent > self.meta.offset + self.meta.padding:
            # |<->|     offset
            #    1.  abc
            #    ->||<- padding
            line.advance_offset(self.meta.offset + self.meta.padding)

        elif parser.tip.name == 'paragraph' or parser.tip.name == 'fence':
            return YES
        else:
            return NO

        return YES

    def can_contain(self, block):
        return block.type != 'list-item'


#==============================================================================
# Parser

class Parser(object):
    """parse state"""
    def __init__(self, arg):
        super(Parser, self).__init__()
        self.line_num               = 0
        self.doc                    = Block.make_block('document', 0, 0)
        self.last_matched_container = None
        self.tip                    = self.doc # inner most block

    def close(self, block):
        block.end_line = block.line.line_num
        block.end_col = len(block.line.line)
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
        self.tip = block

    def parse_line(self, line):
        """Analyze a line of text and update the AST accordingly"""

        self.line_num += 1
        self.line = Line(line, self.line_num)

        self.oldtip = self.tip
        container = self.doc

        # go through the containers and check if the container can contain this line.
        last_child = container.last_child
        while last_child and last_child.is_open:
            container = last_child

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
        self.close_unmatched()

        # Now the line is striped, parse it as a normal unindented line

        if ret == Block.YES:
            # the inner most block(leaf block) can consume this line
            container.consume(parser)
        else:
            # treat the line as a new line
            self.parse_rest()

    def parse_rest(self):
        """parse rest of the line, that means it will not check indents of containing blocks"""
        pass

        # use try_parsing to get the block.
        # use block's

        block = Block.matched_block(self)
        self.add_child(block)

        if block.type == 'leaf':
            return
        elif block.type == 'container':
            # the line header is already consumed
            parse_rest()
