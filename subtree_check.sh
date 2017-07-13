#!/bin/bash

result=0

subtree_check() {
  subtreeA=$1
  subtreeB=$2

  diff_count=$(diff -qr "$subtreeA" "$subtreeB" | grep -v .git | wc -l)

  if [ $diff_count -gt 0 ]; then
    echo "=================================================================="
    echo "ERROR: Subtrees $subtreeA and $subtreeB differ..."
    echo "=================================================================="

    diff -Naur $subtreeA $subtreeB
    result=1
  fi

  echo ""
}

subtree_check op_worker/helpers oneclient/helpers

exit $result
