#!/bin/bash

if [ $# -lt 1 ]; then
  echo "Usage: $0 <main_ref> [<fallback_ref>]"
  exit 1
fi

MAIN_REF=$1
FALLBACK_REF=${2:-origin/develop}

git diff-index --quiet HEAD
if [ $? -ne 0 ]; then
  echo "Refusing to update refs on dirty tree"
  exit 1
fi

echo "Updating submodules to ${MAIN_REF} (fallback: ${FALLBACK_REF})"
for SUBMODULE in `git submodule | cut -d' ' -f3`; do
  echo "Updating ${SUBMODULE}"
  git -C $SUBMODULE fetch
  git -C $SUBMODULE checkout $MAIN_REF || git -C $SUBMODULE checkout $FALLBACK_REF
  git add $SUBMODULE
done

git commit -em "Update refs to $MAIN_REF."

