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

Normally you should not care about this. You will when your content don't
generate as you expected.

There are some rules that I don't like CommonMark, So in my implementation, I
choose my own way. You may consider this as a new *flavor* of Markdown.

### Blank Lines do not break out lists

CommonMark specify that two blank lines should break out the list, which will
make `bar` in the following example a new paragraph. I don't think that is
natural enough. So pymkd don't have this restriction.

```
Input:       CommonMark         pymkd

1. Foo       <ol>               <ol>
             <li>Foo</li>       <li>
             </ol>              <p>Foo</p>
   bar       <p>bar</p>         <p>bar</p>
                                </li>
                                </ol>
```

So in pymkd, you just type enough indentation, and the content will belongs to
the list. No other restrictions(maybe if I don't miss something).

### HTML Blocks

I don't quite understand how CommonMark's rule come into place about HTML
Blocks. I think it is hard to remember and user can get it wrong easily. For
example, a blank lines will end an HTML block:

```
all as HTML Block     *foo* is interpreted as markdown

<del>                  <del>
*foo*
</del>                 *foo*

                       </del>
```

Will result in two different result.

So pymkd had only 3 rules about HTML Block, all easy to understand:

1. HTML block should start at the begining of current line.
2. If there are texts before current line, then the HTML tag should be
   blockwise: type 6 of [CommonMark's HTML Block](http://spec.commonmark.org/0.24/#html-blocks)
3. Once it starts, you should close it. Either with ending tag `</tagname>` or
   self-closing style `<tag .... />`

I won't go into further detail, normally it will do what you expected.

### Nested Links

CommonMark don't allow nested Links, we allow so even if you don't actually
need it. Normally you should not do this, the only exception might be use an
image as the label of a link:

```
[![img](img_url)](url_for_link)
```

## What's wrong with the name!

Well, I don't like the name `pymkd`. `mkd` is the shortcut for `markdown` and
`py` for python. So it is "markdown in python".

Actually I use `pymark` at first and found it was used before trying to submit
it to [pypi](http://pypi.python.org/). And then I thought about `pymarkdown`
and it is taken. So what about `markdownpy`? well that doesn't look good.

It is like you want to name your baby and found that all the good names are
occupied. At least I got a name that don't look bad(hard to type actually).

## License

MIT License

# Acknowledgements

1. Thanks for CommonMark for a good specification
2. Also for [commonmark.js](https://github.com/jgm/commonmark.js) for the
   reference implementation.
3. And [CommonMark-py](https://github.com/rtfd/CommonMark-py), pymkd took quite a lot
   of code from it.

And of course John Gruber for [The
Markdown](https://daringfireball.net/projects/markdown/). Markdown really make
life easier.
