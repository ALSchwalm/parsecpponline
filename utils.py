from clang.cindex import Cursor, CursorKind


def get_root_cursors(source, filename=None):
    """ A generator yielding each 'top level' cursor
    """
    root_cursor = source if isinstance(source, Cursor) else source.cursor

    for cursor in root_cursor.get_children():
        if filename is not None and filename != str(cursor.location.file):
            continue
        yield cursor


def format_cursor(cursor, children):
    return {
        "spelling": cursor.spelling,
        "label": str(cursor.kind),
        "children": children,
        "location": {
            "start": [cursor.extent.start.line,
                      cursor.extent.start.column],
            "end": [cursor.extent.end.line,
                    cursor.extent.end.column]
        }
    }


def get_child_cursor_preorder(root):
    def inner(root):
        children = []
        for child in root.get_children():
            if child.kind == CursorKind.UNEXPOSED_EXPR:
                children.extend(inner(child))
                continue
            children.append(format_cursor(child,
                                          inner(child)))
        return children
    return format_cursor(root, inner(root))
