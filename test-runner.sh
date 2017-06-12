#!/usr/bin/env bash

# Fire up a fresh Emacs instance and run config tests

# Tests should be in tests/ relative to this script
BASE=$(dirname "$(readlink -f "$0")")

# Use this var to alter behaviour when being tested. Use sparingly, to ensure
# the tests are accurate, but sometimes it's needed (e.g. to prevent overriding
# the system-wide Emacs server)
export EMACS_UNDER_TEST=1

emacs --script "$BASE/tests/run.el"
