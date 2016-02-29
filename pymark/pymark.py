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
# Parser

class Parser(object):
    """parse state"""
    def __init__(self, arg):
        super(Parser, self).__init__()
        self.line_num = 0

    def parse_line(self, line):
        """Analyze a line of text and update the AST accordingly"""

        self.line_num += 1
        self.line = Line(line, self.line_num)
