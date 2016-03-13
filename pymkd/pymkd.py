#!/usr/bin/env python3
# -*- coding: utf-8 -*-
from __future__ import absolute_import, unicode_literals

import re
import sys

try:
    from urllib.parse import quote
except ImportError:
    from urllib import quote

try:
    from urllib.parse import quote
except ImportError:
    from urllib import quote

if sys.version_info >= (3, 0):
    if sys.version_info >= (3, 4):
        import html
        HTMLunescape = html.unescape
    else:
        from .entitytrans import _unescape
        HTMLunescape = _unescape
else:
    from pymkd import entitytrans
    HTMLunescape = entitytrans._unescape

#==============================================================================
# Globals
CODE_INDENT = 4

C_SPACE = ' '
C_TAB = '\t'
C_GREATERTHAN = '>'
C_SINGLEQUOTE = "'"
C_DOUBLEQUOTE = '"'
C_UNDERSCORE = '_'

#==============================================================================
# Helpers
def is_space_or_tab(character):
    return character == C_SPACE or character == C_TAB

#==============================================================================
# Line Object

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

                self.partial_consumed_tab = tab_spaces > count

                column_to_advance = count if tab_spaces > count else tab_spaces
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
            ret += ' ' * tab_spaces
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
#------------------------------------------------------------------------------
# Content for inline parser

re_spnl = re.compile(r'^ *(?:\n *)?')

class Content(object):
    """Utility Class for string content"""
    compiled_re_type = type(re.compile('compiled'))

    def __init__(self, string):
        self.string = string
        self.pos = 0

    def peek(self, offset=0):
        """peek the character `offset` after on the current string"""
        try:
            return self.string[self.pos + offset]
        except IndexError:
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
        return self.string[self.pos:]

    def is_end(self):
        return self.pos >= len(self.string)

    def advance(self, num=1):
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

    def skip_spaces(self):
        """Parse zero or more space characters, including at most one newline"""
        self.match(re_spnl)


#==============================================================================
# Node

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

    def __next__(self):
        """Iterate through all its children"""
        if self.cursor:
            ret = self.cursor
            self.cursor = self.cursor.nxt
            return ret
        else:
            raise StopIteration

    # python 2 need this
    next = __next__

    def _repr(self, level=0):
        ret = []
        ret.append('    '*level + '%s[%d, %d, %d, %d] [%s]' % (self.name,self.start_line, self.start_col, self.end_line, self.end_col, self._get_content()))
        for child in self:
            ret.append(child._repr(level+1))
        return '\n'.join(ret)

    def __repr__(self):
        return self._repr()



#==============================================================================
# Inline Parser


class InlineRule(object):
    """Base Class for rules that parses inline elements"""
    def parse(self, parser, content, side_effect=True):
        """Try to parse the content, return None if cannot parse current situation
        Else return an InlineNode.

        The parse method should modify the content's position accordingly

        If `side_effect` is False, do not do anything but modify the content's cursor
        position."""
        return None


#------------------------------------------------------------------------------
# Common regex

ESCAPABLE = '[!"#$%&\'()*+,./:;<=>?@[\\\\\\]^_`{|}~-]'
ESCAPED_CHAR = '\\\\' + ESCAPABLE
REG_CHAR = '[^\\\\()\\x00-\\x20]'
IN_PARENS_NOSP = '\\((' + REG_CHAR + '|' + ESCAPED_CHAR + '|\\\\)*\\)'

ENTITY = '&(?:#x[a-f0-9]{1,8}|#[0-9]{1,8}|[a-z][a-z0-9]{1,31});'
re_entity_or_escaped_char = re.compile( '\\\\' + ESCAPABLE + '|' + ENTITY, re.IGNORECASE)

re_whitespace_char = re.compile(r'\s')
re_whitespace = re.compile(r'\s+')
re_backslash_or_amp = re.compile(r'[\\&]')

#------------------------------------------------------------------------------
# Helper functions

# taken from CommonMark.py
# https://github.com/rtfd/CommonMark-py/blob/master/CommonMark/common.py
def normalize_uri(uri):
    try:
        return quote(uri, safe=str('/@:+?=&()%#*,'))
    except KeyError:
        # Python 2 throws a KeyError sometimes
        try:
            return quote(uri.encode('utf-8'), safe=str('/@:+?=&()%#*,'))
        except UnicodeDecodeError:
            # Python 2 also throws a UnicodeDecodeError, complaining about
            # the width of the "safe" string. Removing this parameter
            # solves the issue, but yields overly aggressive quoting, but we
            # can correct those errors manually.
            s = quote(uri.encode('utf-8'))
            s = re.sub(r'%40', '@', s)
            s = re.sub(r'%3A', ':', s)
            s = re.sub(r'%2B', '+', s)
            s = re.sub(r'%3F', '?', s)
            s = re.sub(r'%3D', '=', s)
            s = re.sub(r'%26', '&', s)
            s = re.sub(r'%28', '(', s)
            s = re.sub(r'%29', ')', s)
            s = re.sub(r'%25', '%', s)
            s = re.sub(r'%23', '#', s)
            s = re.sub(r'%2A', '*', s)
            s = re.sub(r'%2C', ',', s)
            return s

def normalize_reference(s):
    """Normalize reference label.
    Collapse internal whitespace to single space, remove
    leading/trailing whitespace, case fold.
    """
    return re.sub(r'\s+', ' ', s.strip()).upper()

def unescape_char(s):
    return s[1] if s[0] == '\\' else HTMLunescape(s)

def unescape_string(s):
    """Replace entities and backslash escapes with literal characters."""
    if re.search(re_backslash_or_amp, s):
        return re.sub(re_entity_or_escaped_char, lambda m: unescape_char(m.group()), s)
    else:
        return s

#------------------------------------------------------------------------------
# Rule: Escape characters

re_escapable = re.compile('^' + ESCAPABLE)

class RuleEscape(InlineRule):
    """Parse Escaped characters, return either the escaped character, a hard
    line break, or a literal backslash."""

    def parse(self, parser, content, side_effect=True):
        if content.peek() != '\\':
            return None

        content.advance()

        next_char = content.peek()
        if next_char == '\n':
            content.advance()
            return InlineNode('HardBreak')
        elif next_char is not None and content.match(re_escapable):
            return InlineNode('Text', next_char)
        else:
            return InlineNode('Text', '\\')

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
                node = InlineNode('CodeSpan')
                literal = content.string[after_open_ticks:content.pos - len(ticks)]
                literal = re_whitespace.sub(' ', literal.strip())
                node._literal = literal
                return node
            matched = content.match(re_ticks)

        # not matched, resume position
        content.pos = after_open_ticks
        return InlineNode('Text', ticks)

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
            label = match[1:-1]
            href = 'mailto:' + label
        else:
            m_link = content.match(re_autolink)
            if m_link:
                match = m_link
                label = href = match[1:-1]
            else:
                return None

        node = InlineNode('Link')
        node._href = normalize_uri(href)
        node._title = ''
        node.append_child(InlineNode('Text', label))
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
            node = InlineNode('HTMLInline')
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

        # python 2 doesn't recognize '\xa0' as whitespace
        after_is_whitespace   = re_whitespace_char.match(char_after) or char_after == u'\xa0'
        after_is_punctuation  = re_punctuation.match(char_after)
        before_is_whitespace  = re_whitespace_char.match(char_before) or char_before == u'\xa0'
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
            parser.delimiters = delim.get('prev')
        else:
            delim['next']['prev'] = delim.get('prev')

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
        node = InlineNode('Text', contents)

        if not side_effect:
            return node

        parser.delimiters = {
            'char': char,
            'num_delims': num_delims,
            'node': node,
            'prev': getattr(parser, 'delimiters', None),
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

        # set default delimiters if not exists
        parser.delimiters = getattr(parser, 'delimiters', None)

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
                    opener_bottom[closer_char] = old_closer['prev']
                    if not old_closer['can_open']:
                        RuleDelimiter._remove_delimiter(parser, old_closer)
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
                opener_inl._literal = opener_inl._literal[:len(opener_inl._literal)-use_delims]
                closer_inl._literal = closer_inl._literal[:len(closer_inl._literal)-use_delims]

                # build contents for new emph element
                if use_delims == 1:
                    emph = InlineNode('Emph')
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
                    RuleDelimiter._remove_delimiter(parser, opener)

                if closer['num_delims'] == 0:
                    closer_inl.unlink()

                    # closer will be used for next iterator
                    tempstack = closer['next']
                    RuleDelimiter._remove_delimiter(parser, closer)
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
    content.advance()
    level = 1
    ret = None

    while not content.is_end():
        if content.peek() == ']':
            content.advance()
            level -= 1
            if level == 0:
                break

        old_pos = content.pos
        parser.skip_token(content)
        if content.peek() == '[':
            content.advance()
            level += 1

        if content.peek() != ']' and content.pos == old_pos:
            # met special character, skip
            content.advance()

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

    return normalize_uri(unescape_string(description)) if description is not None else None

class RuleLink(InlineRule):
    """Rule for parsing link"""

    def parse(self, parser, content, side_effect=True):
        start_pos = content.pos

        label = parse_link_label(parser, content)
        if label is None:
            content.pos = start_pos
            return None

        matched = False
        title = ''

        #content.skip_spaces()

        if content.peek() == '(':
            # inline link
            content.advance()

            # [link](  <href> "title)
            #        ^^ skip these spaces
            content.skip_spaces()

            start = content.pos
            dest = parse_link_destination(parser, content)
            if dest is not None:
                # match title
                content.skip_spaces()

                # make sure there's a space before the title
                if re.match(re_whitespace_char, content.get_char(content.pos-1)):
                    title = parse_link_title(parser, content)
                content.skip_spaces()
                if content.peek() == ')':
                    content.advance()
                    matched = True
        elif content.peek() == '[':
            # reference link
            ref_label = parse_link_label(parser, content)
            ref_label = label if not ref_label else ref_label
            ref_label = normalize_reference(ref_label) if ref_label is not None  else None

            # if side effect if False, do not lookup the refmap, and consider it as a token
            if not side_effect:
                matched = True
                dest = ''
                title = ''
            elif ref_label is not None and ref_label in parser.refmap:
                matched = True
                dest = parser.refmap[ref_label]['dest']
                title = parser.refmap[ref_label]['title']
        else:
            # reference link [ref label]
            ref_label = normalize_reference(label)

            # if side effect if False, do not lookup the refmap, and consider it as a token
            if not side_effect:
                matched = True
                dest = ''
                title = ''
            elif ref_label is not None and ref_label in parser.refmap:
                matched = True
                dest = parser.refmap[ref_label]['dest']
                title = parser.refmap[ref_label]['title']

        if not matched:
            content.pos = start_pos
            return None

        node = InlineNode('Link')
        node._href = dest
        node._title = title
        if label:
            label_parser = InlineParser(parser.refmap)
            for child in label_parser.parse_content(label):
                node.append_child(child)

        return node

#------------------------------------------------------------------------------
# Rule: Image, depends on link

class RuleImage(InlineRule):
    """Rule for parsing image"""
    def parse(self, parser, content, side_effect=True):
        if content.peek() != '!':
            return None

        start_pos = content.pos
        content.advance()

        rule = RuleLink()
        node = rule.parse(parser, content, side_effect)
        if node is None:
            content.pos = start_pos
            return None

        node.name = 'Image'
        return node

#------------------------------------------------------------------------------
# Rule: Newline, for hard break and softbreak

re_final_space = re.compile(r' *$')
re_initial_space = re.compile(r'^ *')

class RuleNewline(InlineRule):
    """Parse newline, generate hardbreak or softbreak"""
    def parse(self, parser, content, side_effect=True):
        if content.peek() != '\n':
            return None
        content.advance()

        try:
            last_child = parser.node.last_child
        except AttributeError:
            last_child = None
        if last_child and last_child.name == 'Text' and last_child._literal[-1] == ' ':
            hardbreak = len(last_child._literal) >= 2 and last_child._literal[-2] == ' '
            last_child._literal = re_final_space.sub('', last_child._literal)
            if hardbreak:
                node = InlineNode('HardBreak')
            else:
                node = InlineNode('SoftBreak')
        else:
            node = InlineNode('SoftBreak')

        # remove leading spaces in next line
        content.match(re_initial_space)
        return node

#------------------------------------------------------------------------------
# Rule: Entity.
re_entity_here = re.compile('^' + ENTITY, re.IGNORECASE)

class RuleEntity(InlineRule):
    """Entity"""
    def parse(self, parser, content, side_effect=True):
        match = content.match(re_entity_here)
        return InlineNode('Text', HTMLunescape(match)) if match else None

#------------------------------------------------------------------------------
# Rule: Plain Text
re_main = re.compile(r'^[^\n`\[\]\\!<&*_\'"]+')

class RuleText(InlineRule):
    """Plain Text"""
    def parse(self, parser, content, side_effect=True):
        match = content.match(re_main)
        if not match:
            return None

        node = InlineNode('Text', match)
        return node

#------------------------------------------------------------------------------
# Rule: Reference Link Definition
re_space_at_end_of_line = re.compile(r'^ *(?:\n|$)')

# This is actually a leaf block, so we don't inherit Inline Rule
class RuleReferenceLink(object):
    """Reference Link Definition"""
    def parse(self, inline_parser, content, side_effect=True):
        start_pos = content.pos

        # match label:
        label = parse_link_label(inline_parser, content)
        # label should not contain only whitespaces
        if label is None or label.strip() == '':
            content.pos = start_pos
            return None

        # match colon []: link 'title'
        if content.peek() != ':':
            content.pos = start_pos
            return None
        else:
            content.advance()

        # link url
        content.skip_spaces()

        dest = parse_link_destination(inline_parser, content)

        if dest is None or len(dest) == 0:
            content.pos = start_pos
            return None

        before_title = content.pos
        content.skip_spaces()
        title = parse_link_title(inline_parser, content)
        if title is None:
            title = ''
            content.pos = before_title

        # check if we are at the end of the line
        at_line_end = True

        if content.match(re_space_at_end_of_line) is None:
            # still have other contents
            if title == '':
                at_line_end = False
            else:
                # the potential title we found is not at the end of the line
                # but it could still be a legal link reference if we discard
                # the title. i.e. we've matched a title that might belong to
                # later contents
                title = ''
                content.pos = before_title
                at_line_end = content.match(re_space_at_end_of_line) is not None

        if not at_line_end:
            content.pos = start_pos
            return None

        node = InlineNode('Ref')
        node._href = dest
        node._title = title
        node._label = normalize_reference(label)
        return node

#------------------------------------------------------------------------------
# Actual parser that utilize all rules

class InlineParser(object):
    """Inline Parser"""
    def __init__(self, refmap={}):
        self.refmap = refmap
        self.rules = [b() for b in InlineRule.__subclasses__()]

    def parse_content(self, string):
        """The main entry for inline parser, return a InlineNode whose childrens are the parsed
        result"""
        content = Content(string)
        self.node = InlineNode('Parent')

        while not content.is_end():
            matched = False
            for rule in self.rules:
                node = rule.parse(self, content)
                if node is not None:
                    matched = True
                    break
            if not matched:
                # treat the next character as text
                node = InlineNode('Text', content.peek())
                content.advance()
            self.node.append_child(node)

        # in the end, process
        for rule in reversed(self.rules):
            if hasattr(rule, 'post_process'):
                rule.post_process(self)

        return self.node

    def skip_token(self, content):
        """parse the content with all rules, but do not do all the side effects.
        Move the cursor forward"""

        # TODO: later add cache to enhance performance

        for rule in self.rules:
            node = rule.parse(self, content, side_effect=False)
            if node is not None:
                break


#==============================================================================
# Parser

class Parser(object):
    """parse state"""
    def __init__(self):
        super(Parser, self).__init__()
        self.line_num               = 0
        self.doc                    = BlockFactory.make_block('Document', 0, 0)
        self.last_matched_container = None
        self.tip                    = self.doc # inner most block
        self.refmap                 = {}
        self.inline_parser          = InlineParser(self.refmap)

    def close(self, block):
        if block.is_open:
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
        self.tip = parent.last_child if parent.last_child else parent
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

        if self.tip.name == 'Paragraph':
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
            self.container = block.tail_child if block.tail_child else block
            child = self.parse_rest()
            if child is not None:
                block.append_tail(child)
            return block

    def parse_inlines(self, root):
        """Parse the generated AST for inline elements"""

        # handle current node
        inlines = None
        if root.name == 'Paragraph':
            inlines = self.inline_parser.parse_content('\n'.join(root.lines).strip())
        elif root.name == 'Heading':
            inlines = self.inline_parser.parse_content('\n'.join(root.lines).strip())

        if inlines:
            root.lines = []
            for inline in inlines:
                root.append_child(inline)

        if root.type == 'container' or root.type == 'root':
            for c in root:
                self.parse_inlines(c)

    def parse_end(self):
        while self.tip:
            self.close(self.tip)

        self.parse_inlines(self.doc)
        return self.doc

    def parse(self, input):
        """parse the input string and return the AST"""

        # ignore last blank line created by final newline
        if len(input) > 0 and input[-1] == '\n':
            input = input[:-1]

        for line in input.split('\n'):
            self.parse_line(line)
        return self.parse_end()

#==============================================================================
# Various blocks

class Block(Node):
    """block"""
    YES      = 0
    NO       = 1
    CONSUMED = 2

    name = 'Block'

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
        other blocks, but `list` block can contain only `ListItem` block.

        :block: the block to be contained
        :returns: true if `block` can be contained else false

        """
        return False

    def consume(self, parser):
        """Consume current line"""
        pass

    def _get_content(self):
        return '|'.join(self.lines)


class BlockParser(object):
    """Parse a line for block"""

    precedence = 100 # bigger number means LESS precedence

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

    def _get_content(self):
        if self.name == 'Link':
            ret = 'ref: (%s) ' % self._href
            ret += 'title: (%s)' % self._title
        elif self.name == 'Image':
            ret = 'ref: (%s) ' % self._href
            ret += 'title: (%s)' % self._title
        elif self.name == 'Ref':
            ret = 'label: (%s) ' % self._label
            ret += 'ref: (%s) ' % self._href
            ret += 'title: (%s)' % self._title
        else:
            ret = self._literal
        return ret

#------------------------------------------------------------------------------
class Document(Block):
    """root block of a document"""

    name = 'Document'
    type = 'root'
    def __init__(self, *args, **kws):
        super(Document, self).__init__(*args, **kws)

    def can_contain(self, block):
        # a document can contain `list` but not `item` directly
        # that means an `item` should be wrapped by `list`
        return block.name != 'ListItem'

#------------------------------------------------------------------------------

class Paragraph(Block):
    """A paragraph"""

    name = 'Paragraph'
    type = 'leaf'

    def __init__(self, *args, **kws):
        super(Paragraph, self).__init__(*args, **kws)
        self.is_empty = False

    def can_strip(self, parser):
        return Block.NO if parser.line.blank else Block.YES

    def close(self, parser):
        # try parsing the beginning as link reference definitions
        content = Content('\n'.join(self.lines))
        has_ref_defs = False
        content.skip_spaces()
        rule_reference = RuleReferenceLink()
        while content.peek() == '[':
            node = rule_reference.parse(parser.inline_parser, content)
            if node is None:
                break

            # save the label
            if node._label not in parser.refmap:
                parser.refmap[node._label] = {'dest': node._href, 'title': node._title}
            content.skip_spaces()
            has_ref_defs = True
            self.append_child(node)

        if has_ref_defs and content.rest.strip() == '':
            self.is_empty = True
        else:
            self.lines = content.rest.split('\n')

    def consume(self, parser):
        block = parser.parse_rest()
        if block and block.name == 'Paragraph':
            for line in block.lines:
                self.lines.append(line.lstrip())
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

        paragraph = BlockFactory.make_block('Paragraph', line.line_num, line.next_non_space)
        paragraph.lines.append(line.clean_line)
        return paragraph

#------------------------------------------------------------------------------

class Blank(Block):
    """A blank line"""

    name = 'Blank'
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

        blank = BlockFactory.make_block('Blank', line.line_num, line.next_non_space)
        return blank

#------------------------------------------------------------------------------

class Heading(Block):
    """Heading"""
    name = 'Heading'
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
    precedence = 40
    re_setext_heading_line = re.compile(r'^(?:=+|-+) *$')

    @staticmethod
    def parse(parser):
        line = parser.line
        container = parser.container
        if line.indented or container.name != 'Paragraph':
            return None

        match = SetextHeadingParser.re_setext_heading_line.match(line.after_strip)
        if match is None:
            return None

        # this time, container should == parser.tip

        heading = BlockFactory.make_block('Heading', line.line_num, line.next_non_space)
        heading.level = 1 if match.group(0)[0] == '=' else 2
        paragraph = parser.unlink_tail()
        heading.lines = map(lambda x: x.strip(), paragraph.lines)
        return heading

class AtxHeadingParser(BlockParser):
    precedence = 50
    re_atx_heading_line = re.compile(r'^#{1,6}(?: +|$)')
    re_trail_hash_1 = re.compile(r'^ *#+ *$')
    re_trail_hash_2 = re.compile(r' +#+ *$')

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

        heading = BlockFactory.make_block('Heading', line.line_num, line.next_non_space)
        heading.level = len(match.group(0).strip())

        # remove trailing ###s:
        headline = line.after_strip.strip()
        heading.lines.append(AtxHeadingParser.re_trail_hash_2.sub('',
            AtxHeadingParser.re_trail_hash_1.sub('', headline)))

        return heading

#------------------------------------------------------------------------------

class CodeBlock(Block):
    """Code Block"""
    name = 'CodeBlock'
    type = 'leaf'

    def __init__(self, *args, **kws):
        super(CodeBlock, self).__init__(*args, **kws)
        self.lines = []
        self._literal = ''
        self._is_fence = False
        self._fence_length = 0
        self._fence_char = None
        self._fence_offset = 0
        self._fence_option = ''

    def _get_content(self):
        return str(self._fence_char) + ': ' + self._fence_option + '>' + repr(self._literal)

    def close(self, parser):
        if self._is_fence:
            self._fence_option = unescape_string(self._fence_option.strip())
            self._literal = ''.join(self.lines)
            self.lines = []
        else:
            # remove trailing blank lines
            line = ''.join(self.lines)
            self.lines = []
            self._literal = re.sub(r'(\n *)+$', '\n', line)

    def can_strip(self, parser):
        line = parser.line
        if self._is_fence:
            # fenced code block
            match = (line.indent <= 3 and line.get_char(line.next_non_space) == self._fence_char
                    and CodeBlockParser.re_closing_fence.match(line.after_strip))
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
        # we will need the newline character in renderer
        self.lines.append(parser.line.clean_line + '\n')

class CodeBlockParser(BlockParser):
    precedence = 10

    re_open_fence = re.compile(r'`{3,}(?!.*`)|^~{3,}(?!.*~)')
    re_closing_fence = re.compile(r'^(?:`{3,}|~{3,})(?= *$)')

    @staticmethod
    def parse(parser):
        line = parser.line
        if line.indented:
            # try indented code block
            if parser.tip.name == 'Paragraph' or line.blank:
                # counted as a lazy line
                return None

            line.advance_offset(CODE_INDENT, True)
            codeblock = BlockFactory.make_block('CodeBlock', line.line_num, line.next_non_space)
            codeblock.lines.append(line.clean_line + '\n')
            return codeblock
        else:
            # try fenced code block
            match = CodeBlockParser.re_open_fence.match(line.after_strip)
            if not match:
                return None
            codeblock = BlockFactory.make_block('CodeBlock', line.line_num, line.next_non_space)
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
    name = 'ThematicBreak'
    type = 'leaf'

    def __init__(self, *args, **kws):
        super(ThematicBreak, self).__init__(*args, **kws)

    def can_strip(self, parser):
        return Block.NO

class ThematicBreakParser(BlockParser):
    precedence = 30
    re_thematic_break = re.compile(r'^(?:(?:\* *){3,}|(?:_ *){3,}|(?:- *){3,}) *$')

    @staticmethod
    def parse(parser):
        line = parser.line
        if line.indented:
            return None
        match = ThematicBreakParser.re_thematic_break.match(line.after_strip)
        if match is None:
            return match
        thematicbreak = BlockFactory.make_block('ThematicBreak', line.line_num, line.next_non_space)
        return thematicbreak

#------------------------------------------------------------------------------

class BlockQuote(Block):
    """Block Quote"""
    name = 'BlockQuote'
    type = 'container'

    def __init__(self, *args, **kws):
        super(BlockQuote, self).__init__(*args, **kws)

    def can_contain(self, block):
        return block.name != 'ListItem'

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
    precedence = 60
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

        blockquote = BlockFactory.make_block('BlockQuote', line.line_num, line.next_non_space)
        return blockquote

#------------------------------------------------------------------------------

class List(Block):
    """A container list block"""

    name = 'List'
    type = 'container'
    def __init__(self, *args, **kws):
        super(List, self).__init__(*args, **kws)
        self.tight = True

    def can_contain(self, block):
        # can contain only 'ListItem'
        return block.name == 'ListItem'

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

    name = 'ListItem'
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
            line.advance_offset(self.meta.marker_offset + self.meta.padding, True)

        else:
            return Block.NO

        return Block.YES

    def can_contain(self, block):
        return block.type != 'ListItem'

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
            if ListItem._ends_with_blank_line(item) and item.prv and item.nxt:
                return False
        return True

    @staticmethod
    def _ends_with_blank_line(block):
        """check if a container block ends with blank line"""
        ret = False
        while block:
            if block.name == 'Blank':
                ret = True
            elif block.name == 'List' or block.name == 'ListItem':
                last = block.last_child
                ret = last and last.name == 'Blank'
                if last and last.parent.name == 'ListItem':
                    ret = last.name == 'Blank' and ((block.first_child != block.last_child) or last.blank_lines > 1)
                block = last

            if block.name != 'List' and block.name != 'ListItem':
                break
        return ret

class ListParser(BlockParser):
    precedence = 20
    re_bullet_list_marker = re.compile(r'^([*+-])')
    re_ordered_list_marker = re.compile(r'^(\d{1,9})([.)])')

    @staticmethod
    def parse(parser):
        if parser.line.indented and parser.container.name != 'List':
            return None

        meta = ListParser.parse_list_marker(parser)
        if not meta:
            return None

        ret = None
        # add outer list if needed
        container = parser.container
        if container.name != 'List' or container.meta != meta:
            list_block = BlockFactory.make_block('List', parser.line.line_num, parser.line.next_non_space)
            list_block.meta = meta
            ret = list_block

        # add the list item
        list_item = BlockFactory.make_block('ListItem', parser.line.line_num, parser.line.next_non_space)
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

all_block_tag = set([ "address", "article", "aside", "base", "basefont", "blockquote", "body",
    "caption", "center", "col", "colgroup", "dd", "details", "dialog", "dir", "div", "dl", "dt",
    "fieldset", "figcaption", "figure", "footer", "form", "frame", "frameset", "h1", "head",
    "header", "hr", "html", "iframe", "legend", "li", "link", "main", "menu", "menuitem", "meta",
    "nav", "noframes", "ol", "optgroup", "option", "p", "param", "section", "source", "summary",
    "table", "tbody", "td", "tfoot", "th", "thead", "title", "tr", "track", "ul"])

try:
    from HTMLParser import HTMLParser
except:
    from html.parser import HTMLParser

class HTMLContentParser(HTMLParser):
    def __init__(self):

        if sys.version_info >= (3, 4):
            HTMLParser.__init__(self, convert_charrefs=True)
        else:
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
        return self.tag_num <= 0

class HTMLBlock(Block):
    """Block level HTML"""

    name = 'HTMLBlock'
    type = 'leaf'

    def __init__(self, *args, **kws):
        super(HTMLBlock, self).__init__(*args, **kws)

    def can_strip(self, parser):
        if self.html_parser.done:
            return Block.NO

        # consume another line
        line = parser.line
        self.html_parser.feed(line.clean_line)
        self.lines.append(line.clean_line)
        return Block.CONSUMED

    def _get_content(self):
        return '|'.join(self.lines)


class HTMLBlockParser(BlockParser):
    precedence = 10

    re_html_block_start = re.compile(r'^<(?:!--|\?|![A-Z]|!\[CDATA\[|([a-zA-Z](?:[^ \n>])*))')

    @staticmethod
    def parse(parser):
        line = parser.line
        if line.indent > 0:
            # we require HTML block to have no indent at all(0 space)
            return None

        # try auto link, if it matches, return. Normally an autolink will not be an HTML tag anyway
        match_email_autolink = re_email_autolink.match(line.after_strip)
        match_autolink = re_autolink.match(line.after_strip)
        if match_email_autolink or match_autolink:
            return None

        match = HTMLBlockParser.re_html_block_start.match(line.after_strip)
        if match is None:
            return None

        # normal tag should not break a pragraph
        tagname = match.group(1)
        if parser.tip.name == 'Paragraph' and tagname != '' and tagname not in all_block_tag:
            return None

        # now we have an HTML start tag
        html_parser = HTMLContentParser()
        html_parser.feed(line.after_strip)

        html_block = BlockFactory.make_block('HTMLBlock', parser.line.line_num, parser.line.next_non_space)
        html_block.html_parser = html_parser
        html_block.lines.append(line.clean_line)

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
# HTML Renderer

XMLSPECIAL = '[&<>"]'
re_xml_special = re.compile(XMLSPECIAL)
re_xml_special_or_entity = re.compile(ENTITY + '|' + XMLSPECIAL, re.IGNORECASE)

UNSAFE_MAP = {
    '&': '&amp;',
    '<': '&lt;',
    '>': '&gt;',
    '"': '&quot;',
}


def replace_unsafe_char(char):
    return UNSAFE_MAP.get(char, char)

def escape_xml(string, preserve_entities=False):
    if string is None:
        return ''
    if re.search(re_xml_special, string):
        if preserve_entities:
            return re.sub(
                re_xml_special_or_entity,
                lambda m: replace_unsafe_char(m.group()),
                string)
        else:
            return re.sub(
                re_xml_special,
                lambda m: replace_unsafe_char(m.group()),
                string)
    else:
        return string


def noop(node, info):
    return ''

class HTMLRenderer(object):
    """Render AST to HTML"""

    block_separator = '\n'

    def render(self, root):
        self._ret = []
        self._last_out = '\n'
        self._info = {'img_level': 0}
        self._render(root)
        return ''.join(self._ret)

    def _out(self, content):
        if self._info.get('img_level', 0) > 0:
            content = re_html_tag.sub('', content)
        self._ret.append(content)
        self._last_out = content

    def _cr(self):
        if self._last_out != HTMLRenderer.block_separator:
            self._out(HTMLRenderer.block_separator)

    def _render(self, node, info={}):
        """dispatch function for a node"""
        method = getattr(self, 'render'+node.name, noop)
        return method(node, info)

    def _render_child(self, root, info={}):
        for child in root:
            self._render(child, info)

    def _tag(self, tagname, attrs=[], selfclosing=False):
        result = '<'+tagname
        for attr in attrs:
            result += " " + attr[0] + '="' + attr[1] + '"'

        if selfclosing:
            result += ' /'
        result += '>'
        return result

#----------------------------------------------------------------------
# Block Level nodes

    # Document
    def renderDocument(self, node, info):
        return self._render_child(node, {})

    # Paragraph
    def renderParagraph(self, node, info):
        if node.is_empty:
            return

        try:
            gp = node.parent.parent
        except:
            gp = None

        if gp is not None and gp.name == 'List' and gp.tight:
            self._render_child(node)
        else:
            self._cr()
            self._out(self._tag('p'))
            self._render_child(node)
            self._out(self._tag('/p'))
            self._cr()

    # Heading
    def renderHeading(self, node, info):
        tagname = 'h'+str(node.level)
        self._cr()
        self._out(self._tag(tagname))
        self._render_child(node)
        self._out(self._tag('/'+tagname))
        self._cr()

    # CodeBlock
    def renderCodeBlock(self, node, info):
        attrs = []
        if node._is_fence and node._fence_option:
            option = re.split('\s+', node._fence_option)
            if len(option) > 0 and len(option[0]) > 0:
                attrs.append(('class', 'language-'+ option[0]))

        self._cr()
        self._out(self._tag('pre') + self._tag('code', attrs))
        self._out(escape_xml(node._literal))
        self._out(self._tag('/code') + self._tag('/pre'))
        self._cr()

    # ThematicBreak
    def renderThematicBreak(self, node, info):
        self._cr()
        self._out(self._tag('hr', selfclosing=True))
        self._cr()

    # BlockQuote
    def renderBlockQuote(self, node, info):
        self._cr()
        self._out(self._tag('blockquote'))
        self._cr()
        self._render_child(node)
        self._cr()
        self._out(self._tag('/blockquote'))
        self._cr()

    # List
    def renderList(self, node, info):
        attrs = []
        if node.meta.type == 'ordered':
            tag = 'ol'
            if node.meta.start != 1:
                attrs.append(('start', str(node.meta.start)))
        else:
            tag = 'ul'

        self._cr()
        self._out(self._tag(tag, attrs))
        self._cr()
        self._render_child(node, info)
        self._cr()
        self._out(self._tag('/'+tag))
        self._cr()

    # ListItem
    def renderListItem(self, node, info):
        self._out(self._tag('li'))
        self._render_child(node, info)
        self._out(self._tag('/li'))
        self._cr()

    # HTMLBlock
    def renderHTMLBlock(self, node, info):
        self._cr()
        self._out('\n'.join(node.lines))
        self._cr()
#----------------------------------------------------------------------
# Inline Level nodes

    def renderText(self, node, info):
        self._out(escape_xml(node._literal))

    def renderCodeSpan(self, node, info):
        self._out(self._tag('code'))
        self._out(escape_xml(node._literal))
        self._out(self._tag('/code'))

    def renderSoftBreak(self, node, info):
        self._out('\n')

    def renderHardBreak(self, node, info):
        self._out(self._tag('br', [], True))
        self._cr()

    def renderLink(self, node, info):
        attrs = [('href', escape_xml(node._href, True))]
        if node._title:
            attrs.append(('title', escape_xml(node._title, True)))

        self._out(self._tag('a', attrs))
        self._render_child(node)
        self._out(self._tag('/a'))

    def renderImage(self, node, info):
        self._info['img_level'] += 1
        if self._info['img_level'] <= 1:
            self._out('<img src="{0}" alt="'.format(escape_xml(node._href, True)))

        self._render_child(node)

        if self._info['img_level'] <= 1:
            self._out('"')
            if node._title:
                self._out(' title="{0}"'.format(escape_xml(node._title, True)))
            self._out(' />')

        self._info['img_level'] -= 1


    def renderHTMLInline(self, node, info):
        self._out(node._literal)

    def renderEmph(self, node, info):
        self._out(self._tag('em'))
        self._render_child(node)
        self._out(self._tag('/em'))

    def renderStrong(self, node, info):
        self._out(self._tag('strong'))
        self._render_child(node)
        self._out(self._tag('/strong'))

#==============================================================================

def main():
    parser = Parser()
    renderer = HTMLRenderer()

    for line in sys.stdin:
        parser.parse_line(line)
    doc = parser.parse_end()
    print(renderer.render(doc))

if __name__ == '__main__':
    main()
