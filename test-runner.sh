#!/usr/bin/env bash

# Fire up a fresh Emacs instance and run config tests

# Tests should be in tests/ relative to this script
BASE=$(dirname "$(readlink -f "$0")")

emacs --script "$BASE/tests/run.el"
