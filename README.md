# pymkd

Markdown parser in pure python. [pymkd](https://github.com/lotabout/pymkd)
trys to be compatible with [CommonMark
Spec](http://spec.commonmark.org/0.24/). Current Spec version is 0.24. However
it did has some differences with commonMark.

pymkd passes tests 2.7.10 and python 3.4.3. Other versions of python should
works too(but not tested).

## Installation

```
$ pip install pymkd
```

## Usage

This is written in python 2

```python
>>> import pymkd
>>> parser = pymkd.Parser()                # New parser instance
>>> renderer = pymkd.HTMLRenderer()        # New renderer
>>> doc = parser.parse('Hello *world*!')   # Parse string, got syntax tree
>>> html = renderer.render(doc)            # render the tree, got HTML string
>>> print html
<p>Hello <em>world</em>!</p>

>>> print doc                              # You can directly print the tree
Document[0, 0, 1, 14] []
    Paragraph[1, 0, 1, 14] []
        Text[1, 0, 0, 0] [Hello ]
        Emph[1, 0, 0, 0] []
            Text[1, 0, 0, 0] [world]
        Text[1, 0, 0, 0] [!]
```

## Difference with CommonMark

There are some rules that I don't like CommonMark, So in my implementation, I
choose my own way. You may consider this as a new *flavor* of Markdown.

### Blank Lines do not break out lists

```
Input:       CommonMark

1. Foo       <ol>       
             <li>Foo</li>
             </ol>
   bar       <p>bar</p>
