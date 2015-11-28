from clang.cindex import Cursor


def get_root_cursors(source, filename=None):
    """ A generator yielding each 'top level' cursor
    """
    root_cursor = source if isinstance(source, Cursor) else source.cursor

    for cursor in root_cursor.get_children():
        if filename is not None and filename != str(cursor.location.file):
            continue
        yield cursor


def get_child_cursor_preorder(root):
    #TODO: filter unexposed
    def inner(root):
        children = []
        for child in root.get_children():
            sub = {
                "spelling": child.spelling,
                "kind": str(child.kind),
                "children": inner(child)
            }
            children.append(sub)
        return children
    return {
        "spelling": root.spelling,
        "kind": str(root.kind),
        "children": inner(root)
    }
