##!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re
#==============================================================================
# Globals
CODE_INDENT = 4

C_TAB = '\t'
C_SPACE = ' '
C_GREATERTHAN = '>'

# Regular Expressions
re_closing_code_fence = re.compile(r'^(`{3,}|~{3,})(?= *$)')

#==============================================================================
# Helper Functions
def match_at(pattern, string, start_offset):
    """search for regex pattern, starts at offset

    :pattern: a regex string
    :returns: offset if found else None

    """
    try:
        return re.search(pattern, string, start_offset).start()
    except:
        return None

def is_space_or_tab(character):
    return character == C_SPACE or character == C_TAB

#==============================================================================
# Helper Class & Object

class Line(object):
    """Parser for line, storing several states about line"""
    def __init__(self, line, offset=0, column=0):
        super(Line, self).__init__()
        self.line = line

        self.offset = offset # offset is based on characters
        self.column = column # column is based on spaces(tab as 4 space)


        # if a tab exists, then it counts as 4 spaces normally.
        # then if we advance cursor less than 4 column,
        # then this tab is called partial_consumed_tab
        self.partial_consumed_tab = False

    def find_next_none_space(self):
        """Find the next none-space character, save some states accordingly"""
        next_non_space = match_at(r'[^ \t]', self.line, self.offset)

        self.blank = next_non_space is None or self.line[next_non_space] in ('\n', '\r')
        self.next_non_space = next_non_space

        spaces = self.line if next_non_space is None else self.line[self.offset, next_non_space]
        self.next_non_space_column = len(spaces.replace('\t', ' '))
        self.indent = self.next_non_space_column - self.column
        self.indented = self.indent >= CODE_INDENT

    def advance_next_non_space(self):
        """Advance current column to next non space character"""
        self.offset = self.next_non_space
        self.column = self.next_non_space_column

    def advance_offset(self, count, tab_as_space=False):
        """advance current cursor `offset` columns

        :count: the offset to advance
        :tab_as_space: if it is true, then tab is treated as 4 spaces

        """
        line = self.line
        for c in line[self.offset:]:
            if c == '\t':

                # one special rule here is that a tab with preceding spaces can not be
                # treated as full 4 spaces(_ as space, > as tab)
                # __> abc  => _____abc (5 space + abc)
                tab_spaces = 4 - (self.column % 4)

                self.partial_consumed_tab = tab_as_space and tab_as_space > count
                column_to_advance = count if tab_as_space > count else tab_as_space
                self.column += column_to_advance
                self.offset += 0 if self.partial_consumed_tab else 1
                count -= tab_spaces if tab_as_space else 1
            else:
                self.partial_consumed_tab = False
                self.offset += 1;
                self.column += 1
                count -= 1

            if count <= 0:
                break

    def get_line(self):
        """return the line the strip contents before current offset, properly handle partially
        consumed tabs

        :returns: a string that contains processed line

        """
        ret = ""
        if self.partial_consumed_tab:
            self.offset += 1
            tab_spaces = 4 - (self.column % 4)
            ret += ' ' * tab_as_space
        return ret + self.line[self.offset:]

    def peek(self, column=None):
        """peek the character on the current line at `column`

        :column: the target position
        :returns: None if not exist, else the character

        """
        try:
            return self.line[self.offset if not column else column]
        except:
            return None

class Parser(object):
    """Parser object that saves parse states"""
    def __init__(self, subject = None, pos = 0):
        super(Parser, self).__init__()
        self.doc = Block.make_block('document', 0, 0)
        self.subject = subject
        self.pos = pos
        self.tip = self.doc
        self.refmap = {}
        self.line_num = 0

    def incorporate_line(self, line):
        """Analyze a line of text and update the document appropriately"""
        all_matched = True
        self.line_num += 1

        self.current_line = Line(line, 0, 0)

        container = self.doc
        self.oldtip = self.tip

        # we want to find the correct container that is valid to process
        # current line
        while len(container.children) > 0:
            last_child = container.last_child
            if not last_child.is_open:
                break
            container = last_child

            self.current_line.find_next_none_space()

            can_be_sibling = container.can_be_sibling(self)
            if can_be_sibling == 0:
                # can be sibling
                pass
            elif can_be_sibling == 1:
                # cannot be sibling
                all_matched = False
            elif can_be_sibling == 2:
                # the line is process
                return
            else:
                raise Exception('can_be_sibling returned illegal value')

            if not all_matched:
                container = container.parent
                break

        # now we have the correct container, we need to actually process this
        # line according to current context





#==============================================================================
# Node & block

class Node(object):
    """A tree node"""
    def __init__(self, parent=None):
        self.parent = parent
        self.children = []
        self.is_open = False

    @property
    def first_child(self):
        """accessing first child

        :returns: first child if exists, else return None

        """
        try:
            return self.children[0]
        except:
            return None

    @property
    def last_child(self):
        """Helper function for accessing last child

        :returns: last child if exists, else return None

        """
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


# Block
class Block(Node):
    """A block element"""

    # whether we can directly add a line to the end of the block or not
    acceptsLines = False
    type = 'block'

    @staticmethod
    def make_block(tagtype, line_num, col_num):
        for block in Block.__subclasses__():
            if block.type == tagtype:
                return block(line_num = line_num, col_num = col_num)
        return None

    def __init__(self, description="", label="", line_num=0, col_num=0, title=""):
        super(Block, self).__init__()

        self.description = description
        self.label       = label
        self.line_num    = line_num
        self.col_num     = col_num
        self.title       = title

    def can_be_sibling(self, parser):
        """Check if a line can be sibling of current element

        :parser: parser object that containsof class Line: The input line of text to be checked
        :returns: 0 for yes,
                  1 for no
                  2 for 'OK, we take this line, you guys go on with the next line', for code_block

        """
        return 0

    def can_contain(self, block):
        """Check if current block can contain other block, for example, a blockquote can contain
        other blocks, but `list` block can contain only `list-item` block.

        :block: the block to be contained
        :returns: true if `block` can be contained else false

        """
        return False

    def finalize(self, parser):
        """Action to be made when a block structure is finalized"""
        pass

    def ends_with_blank_line(self):
        """Check if current block ends with blank line, descending if needed for lists

        :returns: True if it ends with blank line, else False

        """
        if self.last_line_blank:
            return True
        elif self.type == 'list' or self.type == 'list-item':
            last_child = self.last_child
            return last_child.ends_with_blank_line if last_child else False
        else:
            return False


class Document(Block):
    """Root block of a document"""
    type = 'document'
    def __init__(self, *args, **kw):
        super(Document, self).__init__(*args, **kw)

    def can_contain(self, block):
        # a document can contain `list` but not `item` directly
        # that means an `item` should be wrapped by `list`
        return block.type != 'list-item'

class List(Block):
    """A list block that wraps list items"""
    type = 'list'
    def __init__(self, *args, **kw):
        super(List, self).__init__(*args, **kw)

    def can_contain(self, block):
        # a `list` can contain only `list-items`
        return block.type == 'list-item'

    def finalize(self, parser):
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

class BlockQuote(Block):
    """Block quote"""
    type = 'blockquote'
    def __init__(self, *args, **kw):
        super(BlockQuote, self).__init__(*args, **kw)

    def can_be_sibling(self, parser):
        line = parser.current_line
        if not line.indented and line.peek(line.next_non_space) == C_GREATERTHAN:
            line.advance_next_non_space()
            line.advance_offset(1) # consume '>'

            optional_space = line.peek()
            if is_space_or_tab(optional_space):
                line.advance_offset(1, True)

            return 0 # can be sibling

        else:
            return 1 # can not be sibling

    def can_contain(self, block):
        return block.type != 'list-item'

class ListItem(Block):
    """A single list item"""

    type = 'list-item'
    def __init__(self, *args, **kw):
        super(ListItem, self).__init__(*args, **kw)

    def can_be_sibling(self, parser):
        line = parser.current_line
        if line.blank:
            if not self.first_child:
                # blank line after empty list item
                return 1 # cannot be sibling
            else:
                line.advance_next_non_space()

        elif line.indent > self.meta.offset + self.meta.padding:
            # |<->|     offset
            #    1.  abc
            #    ->||<- padding
            line.advance_offset(self.meta.offset + self.meta.padding)

        else:
            return 1

        return 0 # can be sibling by default

    def can_contain(self, block):
        return block.type != 'list-item'


class Heading(Block):
    """Heading"""

    type = 'heading'
    def __init__(self, *args, **kw):
        super(Heading, self).__init__(*args, **kw)

    def can_be_sibling(self, parser):
        # headings can never contain more than one line
        return 1 # can not


class ThematicBreak(Block):
    """Thematic Break"""

    type = 'thematic-break'
    def __init__(self, *args, **kw):
        super(ThematicBreak, self).__init__(*args, **kw)

    def can_be_sibling(self, parser):
        # thematic break can never contain more than one line
        return 1 # can not

class CodeBlock(Block):
    """code block"""
    type = "code-block"
    acceptsLines = True
    def __init__(self, *args, **kw):
        super(CodeBlock, self).__init__(*args, **kw)

    def can_be_sibling(self, parser):
        line = parser.current_line
        if self.is_fenced:
            # fenced
            match = re_closing_code_fence.match(line[line.next_non_space:])
            fence = match.group(1) if match else None
            if fence and fence.startswith(self.fence_char) and len(fence) > self.fence_length:
                # closing fence
                self.finalize(line)
                return 2 # line processed, go on
            else:
                # skip optional spaces of fence
                count = self.fence_offset
                while count > 0 and is_space_or_tab(line.peek()):
                    line.advance_offset(1, true)
                    count -= 1
        else:
            # indented
            if line.indent >= CODE_INDENT:
                line.advance_offset(CODE_INDENT, true)
            elif line.blank:
                line.advance_next_non_space()
            else:
                return 1 # not allowed

        return 0 # allowed

    def finalize(self, parser):
        if self.is_fenced:
            # TODO: first line becomes info string
            pass
        else:
            # TODO: strip ending spaces
            pass

class Paragraph(Block):
    """paragraph"""
    type = 'paragraph'
    acceptsLines = True
    def __init__(self, *arg, **kw):
        super(Paragraph, self).__init__(*arg, **kw)

    def can_be_sibling(self, parser):
        line = parser.current_line
        return 1 if line.blank else 0

    def finalize(self, line):
        # TODO: later
        pass


