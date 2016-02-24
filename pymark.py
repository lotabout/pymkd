##!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Block
class Block():
    """A block element"""

    # whether we can directly add a line to the end of the block or not
    acceptsLines = False

    def __init__(self, parent=None):
        self.children = []
        self.parent = parent

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

    def finalize(self):
        """Action to be made when a block structure is finalized"""
        pass
        
