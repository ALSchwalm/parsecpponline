from flask import Flask, render_template, request, jsonify
from clang.cindex import TranslationUnit, Index
from utils import get_root_cursors, get_child_cursor_preorder

app = Flask(__name__)


@app.route("/parse", methods=["POST"])
def parse():
    name = "unsaved.cpp"
    source = request.form["source"]

    index = Index.create(excludeDecls=True)
    tu = index.parse(name, ["-std=c++11", "-I/usr/lib/clang/3.7.0/include"],
                     options=TranslationUnit.PARSE_PRECOMPILED_PREAMBLE,
                     unsaved_files=[(name, source)])

    errors = []
    for d in tu.diagnostics:
        errors.append({
            "severity": d.severity,
            "spelling": d.spelling,
            "location": d.location.file.name
        })

    root_cursors = get_root_cursors(tu, name)
    ast = [get_child_cursor_preorder(c) for c in root_cursors]

    return jsonify(ast=ast,
                   errors=errors)


@app.route("/")
def index():
    return render_template('index.html')

if __name__ == "__main__":
    app.run(debug=True)
