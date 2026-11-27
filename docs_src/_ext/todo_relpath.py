"""Show todo entries' source files relative to the documentation sources.

sphinx.ext.todo's todolist says where each entry comes from ("The original
entry is located in FILE, line N") with the file's absolute path, which puts
the build machine's directory layout into the generated HTML.  This rewrites
the path relative to the source directory (e.g. ``abi.rst``) as soon as a
document is read, before the todo domain stores the entries.  The link to
the entry itself is relative already.
"""

import os

from sphinx.ext.todo import todo_node


def relative_todo_sources(app, doctree):
    for node in doctree.findall(todo_node):
        if node.source and os.path.isabs(node.source):
            node.source = os.path.relpath(node.source, app.srcdir)


def setup(app):
    app.setup_extension('sphinx.ext.todo')
    app.connect('doctree-read', relative_todo_sources, priority=100)
    return {'parallel_read_safe': True, 'parallel_write_safe': True}
