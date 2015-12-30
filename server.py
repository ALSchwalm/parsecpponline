from flask import Flask, render_template, request
from cppparser import CPPParser
import json

app = Flask(__name__)


def parse_source(source):
    c = CPPParser(source.encode("UTF-8"))

    return json.dumps(c.ast), json.dumps(c.errors)


@app.route("/parse", methods=["GET"])
def parse_get():
    source = request.args.get('source', '')
    ast, errors = parse_source(source)
    return render_template("index.html", source=source,
                           ast=ast,
                           errors=errors,
                           default_page=False)


@app.route("/")
def index():
    return render_template('index.html',
                           source=open("static/examples/helloworld.cpp").read(),
                           ast="\"\"",
                           errors="\"\"",
                           default_page=True)

if __name__ == "__main__":
    app.run(debug=True)
