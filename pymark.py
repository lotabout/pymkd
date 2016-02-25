##!/usr/bin/env python3
# -*- coding: utf-8 -*-

class Node(object):
    """A tree node"""
    def __init__(self, parent=None):
        self.parent = parent
        self.children = []

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

        :line: The input line of text to be checked
        :returns: 0 for yes, 1 for no
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
    def __init__(self, *args, **kw):
        super(BlockQuote, self).__init__(*args, **kw)

    def can_be_sibling(self, parser):
        line = parser.current_line

