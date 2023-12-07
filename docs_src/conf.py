# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
import os
import sys
import sphinx_rtd_theme
# sys.path.insert(0, os.path.abspath('.'))
sys.path.insert(0, os.path.abspath("./_ext"))


# -- Project information -----------------------------------------------------

project = 'Brew processor'
copyright = '2022, Andras Tantos'
author = 'Andras Tantos'


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.

extensions = [
    'exec',
    'myst_parser',
    'sphinxcontrib.wavedrom',
    'sphinxcontrib.svgbob',
    'sphinx_rtd_theme',
    'sphinx.ext.todo',
]

offline_skin_js_path = r"default.js"
offline_wavedrom_js_path = r"wavedrom.min.js"

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store', 'isa_detail_*.rst', 'isa_classes.rst']

todo_include_todos = True

# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
#html_theme = 'alabaster'
html_theme = "sphinx_rtd_theme"

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']
# Disable copying source over to build
html_copy_source = False

source_suffix = {
    '.rst': 'restructuredtext',
    '.md': 'markdown',
}

# - Config for wavedrom ------------------------------------------------------
#wavedrom_html_jsinline = False
# We need either this, or: npm i wavedrom-cli -g
#render_using_wavedrompy = True

from docutils import nodes

