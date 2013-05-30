#!/bin/bash

if [ -z "$1" ]; then
  echo "Usage: $0 testfile1 [testfile2 ...]"
  exit 0
fi

set_default () {
  eval "
if [ -z \$$1 ]; then
  $1=$2
fi
"
}

# http://www.linuxjournal.com/content/use-bash-trap-statement-cleanup-temporary-files
function on_exit()
{
    for i in "${on_exit_items[@]}"
    do
        eval $i
    done
}

function add_on_exit()
{
    local n=${#on_exit_items[*]}
    on_exit_items[$n]="$*"
    if [[ $n -eq 0 ]]; then
        trap on_exit EXIT
    fi
}

set_default EL_GET_LIB_DIR "$(dirname "$(cd "$(dirname "$0")"; pwd)")"
set_default TEMPDIR "$(mktemp -d -t el-get-test)"
set_default TEST_HOME "$TEMPDIR/home"
set_default EMACS "$(which emacs)"
set_default TEST_DIR "$(dirname $0)"

run_test () {
  for x in "$1" "$TEST_DIR/$1" "$TEST_DIR/$1.el" "$TEST_DIR/el-get-issue-$1.el"; do
    if [ -f "$x" ]; then
      testfile="$x"
    fi
  done
  if [ -z "$testfile" ]; then
    echo "*** ERROR $1: Could not find test file ***"
  else
    echo "*** Running el-get test $testfile interactively ***"
    if [ -n "$DO_NOT_CLEAN" ]; then
      echo "Not deleting $TEST_HOME after test completion";
    else
      add_on_exit "rm -rf $TEMPDIR"
      rm -rf "$TEMPDIR"
    fi
    mkdir -p "$TEST_HOME"/.emacs.d/el-get/
    HOME="$TEST_HOME" "$EMACS" -Q -L "$EL_GET_LIB_DIR" \
      -l "$EL_GET_LIB_DIR/el-get.el" -l "$EL_GET_LIB_DIR/test/test-setup.el" \
      "$testfile" -l "$testfile"
  fi
}

for t in "$@"; do
  run_test "$t"
done
