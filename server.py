from flask import Flask, render_template, request, jsonify
from cppparser import CPPParser
import json

app = Flask(__name__)


def parse_source(source):
    c = CPPParser(source.encode("UTF-8"))

    return json.dumps({
        "ast": c.ast,
        "errors": []
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
