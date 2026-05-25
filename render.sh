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
echo "Done! Now you can:"
echo "  git add ."
echo "  git commit -m 'Update content'"
echo "  git push"
echo ""
echo "GitHub Pages will automatically deploy the pre-rendered files."