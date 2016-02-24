##!/usr/bin/env python3
# -*- coding: utf-8 -*-

from inliner import inline

class Node():
    """A tree node"""
    def __init__(self, parent):
        self.parent = parent
        self.children = []

    def get_first_child(self):
        """Helper function for accessing first child

        :returns: first child if exists, else return None

        """
        try:
            return self.children[0]
        except:
            return None

    def get_last_child(self):
        """Helper function for accessing last child

        :returns: last child if exists, else return None

        """
        try:
            return self.children[-1]
        except:
            return None


# Block
class Block(Node):
    """A block element"""

    # whether we can directly add a line to the end of the block or not
    acceptsLines = False
    type = 'block'

    def __init__(self, parent=None):
        super(Block, self).__init__(parent)

    def can_be_sibling(self, line):
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

    @staticmethod
    def ends_with_blank_line(self):
        """Check if current block ends with blank line, descending if needed for lists

        :returns: True if it ends with blank line, else False

        """
        block = self


class Document(Block):
    """Root block of a document"""
    type = 'document'
    def __init__(self, parent=None):
        super(Document, self).__init__(parent)

    def can_contain(self, block):
        # a document can contain `list` but not `item` directly
        # that means an `item` should be wrapped by `list`
        return block.type != 'list-item'

class List(Block):
    """A list block that wraps list items"""
    type = 'list'
    def __init__(self, parent=None):
        super(List, self).__init__(parent)

    def can_contain(self, block):
        # a `list` can contain only `list-items`
        return block.type == 'list-item'

    def finalize(self, parser):
        for item in self.children:
            self.ends_with_blank_line()

