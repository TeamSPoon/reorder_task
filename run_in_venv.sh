#!/bin/sh
set -e

# Default to quiet, unless no .venv exists yet
VERBOSE=0
CMD_ARGS=""

SCRIPT="$0"
if [ "$SCRIPT" = "bash" ] || [ "$SCRIPT" = "sh" ]; then
    SCRIPT="${BASH_SOURCE:-${ZSH_SCRIPT:-$0}}"
fi

while [ -L "$SCRIPT" ]; do
    SCRIPT="$(readlink "$SCRIPT")"
done
SCRIPT_DIR="$(cd "$(dirname "$SCRIPT")" && pwd)"
VENV_DIR="$SCRIPT_DIR/.venv"

[ ! -d "$VENV_DIR" ] && VERBOSE=1

# Print help
show_help() {
    cat <<EOF
Usage: $0 [OPTIONS] [--] [COMMAND [ARGS...]]

Sets up and enters a Python virtual environment for this project.
Can be sourced to activate venv in current shell, or run to spawn subshell.

Options:
  -v, --verbose      Enable verbose output
  -h, --help         Show this help message
  --                 Stop parsing options; run following COMMAND in venv

Examples:
  $0
      Launch interactive (venv) bash shell

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

# Parse arguments
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

log "🐍 Checking Python virtual environment..."
if [ ! -d "$VENV_DIR" ]; then
    log "📦 Creating virtual environment at .venv/"
    python3 -m venv "$VENV_DIR"
else
    log "✅ Virtual environment already exists: .venv/"
fi

VENV_PY="$VENV_DIR/bin/python"
VENV_PIP="$VENV_DIR/bin/pip"

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

# Detect if script is being sourced
is_sourced() {
    [ -n "$ZSH_EVAL_CONTEXT" ] && case $ZSH_EVAL_CONTEXT in *:file) return 0 ;; esac
    [ "${BASH_SOURCE[0]}" != "$0" ] 2>/dev/null
}

# Act based on execution mode
if is_sourced; then
    log "📎 Script is sourced. Staying in current shell and activating venv."
    # shellcheck disable=SC1090
    . "$VENV_DIR/bin/activate"
    if [ -n "$CMD_ARGS" ]; then
        log "🚀 Running command: $CMD_ARGS"
        eval "$CMD_ARGS"
    fi
else
    if command -v bash >/dev/null 2>&1; then
        if [ -n "$CMD_ARGS" ]; then
            log "🚀 Running in virtualenv: $CMD_ARGS"
            exec "$VENV_DIR/bin/bash" -c "source \"$VENV_DIR/bin/activate\" && exec $CMD_ARGS"
        else
            log "🟢 Launching interactive Bash shell with virtualenv activated..."
            exec "$VENV_DIR/bin/bash" --rcfile "$VENV_DIR/bin/activate"
        fi
    else
        echo "❌ Error: bash not found in PATH." >&2
        exit 1
    fi
fi

