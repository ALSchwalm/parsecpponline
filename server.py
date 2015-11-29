from flask import Flask, render_template, request, jsonify
from clang.cindex import TranslationUnit, Index
from utils import get_root_cursors, get_child_cursor_preorder
import json

app = Flask(__name__)


def parse_source(source):
    name = "unsaved.cpp"

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

    return json.dumps({
        "ast": ast,
        "errors": errors
    })

@app.route("/parse", methods=["GET"])
def parse_get():
    source = request.args.get('source', '')
    return render_template("index.html", source=source,
                           parsed=parse_source(source),
                           default_page=False)

@app.route("/")
def index():
    return render_template('index.html', source="", parsed="\"\"",
                           default_page=True)

if __name__ == "__main__":
    app.run(debug=True)
