#!/bin/sh
set -e

VERBOSE=0
CMD_ARGS=""

# === Script Path Detection ===
if [ -n "$BASH_SOURCE" ]; then
    SCRIPT="$BASH_SOURCE"
elif [ -n "$ZSH_EVAL_CONTEXT" ]; then
    SCRIPT="${(%):-%N}"
else
    SCRIPT="$0"
fi

while [ -L "$SCRIPT" ]; do
    SCRIPT="$(readlink "$SCRIPT")"
done

SCRIPT_DIR="$(cd "$(dirname "$SCRIPT")" && pwd)"
VENV_DIR="$SCRIPT_DIR/.venv"

[ ! -d "$VENV_DIR" ] && VERBOSE=1

# === Help Menu ===
show_help() {
    cat <<EOF
Usage: $0 [OPTIONS] [--] [COMMAND [ARGS...]]

Initializes and activates a Python virtual environment for this project.
Detects your shell, installs requirements, and links the Prolog pack.

Options:
  -v, --verbose      Enable verbose output
  -h, --help         Show this help message
  --                 Stop parsing options; run following COMMAND in venv

Examples:
  $0
      Launch interactive (venv) shell using your preferred shell

  $0 python script.py
      Run Python script inside venv

  $0 -v
      Verbose setup, then interactive shell

  source $0
      Activate venv in current shell session

  source $0 python myscript.py
      Activate venv and run script, stay in same shell

  $0 -- bash -c 'echo Hello'
      Run a bash command inside the venv shell
EOF
    exit 0
}

# === Argument Parsing ===
while [ $# -gt 0 ]; do
    case "$1" in
        -v|--verbose)
            VERBOSE=1
            shift
            ;;
        -h|--help)
            show_help
            ;;
        --)
            shift
            CMD_ARGS="$@"
            break
            ;;
        *)
            CMD_ARGS="$@"
            break
            ;;
    esac
done

log() {
    [ "$VERBOSE" -eq 1 ] && printf "%s\n" "$*"
}

# === .env Auto-Loading ===
if [ -f "$SCRIPT_DIR/.env" ]; then
    log "📄 Loading .env file..."
    set -a
    . "$SCRIPT_DIR/.env"
    set +a
fi

# === Shell Detection ===
PREFERRED_SHELL="${SHELL:-/bin/bash}"
case "$(basename "$PREFERRED_SHELL")" in
    bash|zsh|fish)
        USE_SHELL="$PREFERRED_SHELL"
        ;;
    *)
        log "⚠️  Unknown or unsupported shell: $PREFERRED_SHELL. Falling back to bash."
        USE_SHELL="/bin/bash"
        ;;
esac

# === SWI-Prolog Pack Link ===
PACK_NAME="$(basename "$SCRIPT_DIR")"
PACK_DEST="$HOME/.local/share/swi-prolog/pack/$PACK_NAME"
PY_DIR="$SCRIPT_DIR/python"

log "🔍 Ensuring pack directory exists..."
mkdir -p "$HOME/.local/share/swi-prolog/pack"

if [ -L "$PACK_DEST" ] || [ -d "$PACK_DEST" ]; then
    log "✅ Symlink or directory already exists: $PACK_DEST"
else
    log "🔗 Creating symlink: $PACK_DEST → $SCRIPT_DIR"
    ln -s "$SCRIPT_DIR" "$PACK_DEST"
fi

# === Virtualenv Setup ===
if [ -n "$VIRTUAL_ENV" ]; then
    log "🧠 Already inside virtualenv at: $VIRTUAL_ENV"
    VENV_ACTIVE=1
    VENV_DIR="$VIRTUAL_ENV"
else
    VENV_ACTIVE=0
    if [ ! -d "$VENV_DIR" ]; then
        log "📦 Creating virtual environment at .venv/"
        python3 -m venv "$VENV_DIR"
    else
        log "✅ Virtual environment already exists: .venv/"
    fi
fi

VENV_PY="$VENV_DIR/bin/python"
VENV_PIP="$VENV_DIR/bin/pip"

# === Install Python Requirements ===
if [ -d "$PY_DIR" ]; then
    (
        log "🔁 (cd $PY_DIR)"
        cd "$PY_DIR"

        if [ -f requirements.txt ]; then
            log "📦 Installing requirements.txt..."
            "$VENV_PIP" install -r requirements.txt > /dev/null 2>&1 || [ "$VERBOSE" -eq 1 ]
        else
            log "⚠️  No requirements.txt found."
        fi

        if [ -f setup.py ] || [ -f pyproject.toml ]; then
            log "📦 Installing local Python package in editable mode with dev extras..."
            "$VENV_PIP" install -e .[dev] > /dev/null 2>&1 || [ "$VERBOSE" -eq 1 ]
        else
            log "⚠️  No setup.py or pyproject.toml found in ./python/"
        fi
    )
else
    log "⚠️  No ./python directory found. Skipping Python install."
fi

# === Detect If Sourced ===
is_sourced() {
    [ -n "$ZSH_EVAL_CONTEXT" ] && case $ZSH_EVAL_CONTEXT in *:file) return 0 ;; esac
    [ -n "$BASH_VERSION" ] && [ "${BASH_SOURCE[0]}" != "$0" ] 2>/dev/null
}

# === Final Execution ===
if is_sourced; then
    log "📎 Script is sourced. Staying in current shell and activating venv."
    # shellcheck disable=SC1090
    . "$VENV_DIR/bin/activate"
    if [ -n "$CMD_ARGS" ]; then
        log "🚀 Running command: $CMD_ARGS"
        eval "$CMD_ARGS"
    fi
else
    if [ -n "$CMD_ARGS" ]; then
        log "🚀 Running in virtualenv: $CMD_ARGS"
        case "$(basename "$USE_SHELL")" in
            fish)
                exec "$USE_SHELL" -c "source \"$VENV_DIR/bin/activate.fish\"; exec $CMD_ARGS"
                ;;
            zsh|bash)
                exec "$USE_SHELL" -c "source \"$VENV_DIR/bin/activate\" && exec $CMD_ARGS"
                ;;
        esac
    else
        log "🟢 Launching interactive shell with virtualenv activated..."
        case "$(basename "$USE_SHELL")" in
            fish)
                exec "$USE_SHELL" -i -c "source \"$VENV_DIR/bin/activate.fish\"; exec $USE_SHELL"
                ;;
            zsh|bash)
                exec "$USE_SHELL" --rcfile "$VENV_DIR/bin/activate"
                ;;
        esac
    fi
fi

