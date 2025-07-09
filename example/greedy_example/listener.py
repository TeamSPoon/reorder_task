
from flask import Flask, request, jsonify
import os
import pexpect

app = Flask(__name__)

@app.route('/')
def home():
    return "🧠 Python Listener is running on /run", 200

@app.route('/run', methods=['POST'])
def run_command():
    data = request.get_json()
    command = data.get('command')
    if not command:
        return jsonify({"error": "Missing 'command' field"}), 400

    try:
        log_path = "/tmp/chatgpt_shell.log"
        os.makedirs(os.path.dirname(log_path), exist_ok=True)

        child = pexpect.spawn("/bin/bash", ["-c", command], encoding='utf-8', timeout=60)
        output_lines = []

        with open(log_path, "a") as f:
            f.write(f"\n🟢 Command: {command}\n")
            while True:
                try:
                    line = child.readline().strip()
                    if not line:
                        break
                    f.write(line + "\n")
                    output_lines.append(line)
                except pexpect.exceptions.EOF:
                    break
                except Exception as e:
                    f.write(f"⚠️ Exception: {str(e)}\n")
                    break
            child.close()

        return jsonify({
            "stdout": "\n".join(output_lines),
            "exitstatus": child.exitstatus,
            "logfile": log_path
        })
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/info')
def info_query():
    query_type = request.args.get("type", "")
    try:
        if query_type == "cwd":
            return jsonify({"result": os.getcwd()})
        elif query_type == "python-version":
            import sys
            return jsonify({"result": sys.version})
        elif query_type == "files":
            return jsonify({"result": "\n".join(os.listdir(os.getcwd()))})
        elif query_type.startswith("shell:"):
            cmd = query_type[len("shell:"):]
            result = pexpect.run(cmd, encoding='utf-8', timeout=30)
            return jsonify({"result": result.strip()})
        else:
            return jsonify({"error": "Unknown info type"}), 400
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/info-log')
def get_log():
    try:
        with open("/tmp/chatgpt_shell.log") as f:
            return jsonify({"result": f.read()})
    except Exception as e:
        return jsonify({"error": str(e)}), 500

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--port', type=int, default=15005)
    parser.add_argument('--host', type=str, default='0.0.0.0')
    args = parser.parse_args()
    app.run(host=args.host, port=args.port)
