#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import json
import os
import unittest
import pymark

class TestPymark(unittest.TestCase):
    def check_markdown(self, info):
        parser = pymark.Parser()
        renderer = pymark.HTMLRenderer()
        output = renderer.render(parser.parse(info['markdown']))
        expected = info['html']
        self.assertEqual(expected, output, str(info['example'])+': '+info['section'] + '\n'
                + 'input   : [' + repr(info['markdown']) + ']\n'
                + 'expected: [' + repr(expected) + ']\n'
                + 'output  : [' + repr(output)   + ']')

def _add_test(test_case):
    def test_method(self):
        self.check_markdown(test_case)
    setattr(TestPymark, 'test_'+str(test_case['example']), test_method)
    test_method.__name__ = 'test_' + str(test_case['example'])

with open(os.path.join(os.path.dirname(__file__), 'tests.json')) as fp:
    data = json.load(fp)
for test in data[:360]:
    _add_test(test)


if __name__ == '__main__':
    unittest.main()
