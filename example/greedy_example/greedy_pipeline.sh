#!/bin/bash
# 🧠 Full GreedyChess MeTTa-to-Prolog Pipeline Runner
# Must be run from: swi-prolog/packs/reorder_task/example/greedy_example

set -e

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <GreedyChess.metta>"
  exit 1
fi

METTA_FILE="$1"
BASENAME=$(basename "$METTA_FILE" .metta)
OUTPUT_DIR="greedy_output"
PL_FILE="$OUTPUT_DIR/$BASENAME.pl"
TRACE_JSON="$OUTPUT_DIR/trace_log.json"
DAG_DOT="$OUTPUT_DIR/dag.dot"
STAGE_DOT="$OUTPUT_DIR/stages.dot"
DAG_PNG="$OUTPUT_DIR/dag.png"
STAGE_PNG="$OUTPUT_DIR/stages.png"
TRACE_HTML="../../prolog/trace_viewer.html"

mkdir -p "$OUTPUT_DIR"

echo "📥 Converting $METTA_FILE to Prolog: $PL_FILE"
python3 ../../python/metta_to_prolog.py "$METTA_FILE" "$PL_FILE"

echo "📦 Loading and executing pipeline..."
swipl -q -g "
  consult('../../prolog/reorder_repl.pl'),
  consult('$PL_FILE'),
  retractall(trace_event(_)),
  deep_inline:deep_inline_body(main_loop, [], Inlined),
  annotate_goals_with_llm([Inlined], Annotated),
  stage_builder:build_execution_stages(Annotated, Stages),
  trace_executor:run_traced_stages_with_logging(Stages),
  trace_executor:dump_trace_log('$TRACE_JSON'),
  dag_export_shell:export_goal_dag_png(Annotated, '$DAG_PNG'),
  stage_export_shell:export_stages_png(Annotated, '$STAGE_PNG')
" -t halt

echo "✅ Done! Output files:"
echo " - $TRACE_JSON"
echo " - $DAG_PNG"
echo " - $STAGE_PNG"
echo "📂 Output folder: $OUTPUT_DIR"
echo "🌐 Open $TRACE_HTML to view trace"
