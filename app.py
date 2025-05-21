from flask import Flask, render_template, request, jsonify
import subprocess
import tempfile
import os

app = Flask(__name__)

@app.route('/')
def index():
    return render_template('index.html', output=None)

@app.route('/compile', methods=['POST'])
def compile_code():
    code = request.json.get('code')
    action = request.json.get('action')  # 'tokenize', 'parse', etc.

    # Save code to a temporary file
    with tempfile.NamedTemporaryFile(delete=False, suffix='.txt', mode='w') as f:
        f.write(code)
        filename = f.name

    # Call your C++ compiler executable with the filename
    # Make sure 'com' is the compiled executable from com.cpp in the same directory
    try:
        result = subprocess.run(['./minicompiler', filename], capture_output=True, text=True, timeout=10)
        output = result.stdout
    except Exception as e:
        output = f"Error running compiler: {e}"

    os.unlink(filename)  # Clean up

    # Filter output based on action (optional, can be improved)
    if action == "tokenize":
        output = extract_section(output, "Lexical Analysis (Tokenization)")
    elif action == "parse":
        output = extract_section(output, "Syntax Analysis (Parsing)")
    elif action == "semantic":
        output = extract_section(output, "Semantic Analysis")
    elif action == "intermediate":
        output = extract_section(output, "Intermediate Code Generation")
    elif action == "assembly":
        output = extract_section(output, "Assembly Code Generation")

    return jsonify({'output': output})

def extract_section(text, section_title):
    """Extracts a section from the compiler output based on the title."""
    lines = text.splitlines()
    start = None
    for i, line in enumerate(lines):
        if section_title in line:
            start = i
            break
    if start is None:
        return "Section not found."
    # Grab lines until the next section or end
    end = len(lines)
    for j in range(start + 1, len(lines)):
        if "===" in lines[j] and j != start:
            end = j
            break
    return "\n".join(lines[start:end])

if __name__ == '__main__':
    app.run(debug=True)