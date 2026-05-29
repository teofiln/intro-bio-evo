#!/bin/bash

set -euo pipefail

echo "Rendering Quarto site..."
render_output="$(quarto render . --no-execute 2>&1)"

if [[ -n "$render_output" ]]; then
  echo "$render_output"
else
  echo "No files required rendering (site already up to date)."
fi

echo ""
echo "Rendering worksheets to PDF and DOCX..."
for qmd in worksheets/worksheet_*.qmd; do
  echo "  $qmd → pdf"
  quarto render "$qmd" --to pdf --no-execute
  echo "  $qmd → docx"
  quarto render "$qmd" --to docx --no-execute
done
echo "Worksheets done."

echo ""
echo "Done! Now you can:"
echo "  git add ."
echo "  git commit -m 'Update content'"
echo "  git push"
echo ""
echo "GitHub Pages will automatically deploy the pre-rendered files."