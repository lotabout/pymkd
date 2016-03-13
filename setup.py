#!/usr/bin/env python
# -*- coding: utf-8 -*-

from setuptools import setup

setup(
        name = 'pymkd',
        version = "0.2.33",
        description = "Markdown Parser in python, compatible with CommonMark",
        license = "MIT",
        url = "https://github.com/lotabout/pymkd",
        download_url = "https://github.com/lotabout/pymkd/archive/v0.2.33.tar.gz",
        author = "Jinzhou Zhang",
        author_email = "lotabout@gmail.com",
        packages = ["pymkd"],
        scripts = ["bin/pymkd"],
        keywords = ["markup", "markdown", "commonmark"],
        test_suite="test.test_pymkd",
        install_requires = ['future']
        )
