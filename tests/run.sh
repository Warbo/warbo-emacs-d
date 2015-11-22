#!/usr/bin/env bash
D=$(dirname "$0")
pushd "$D"
emacs --script run.el
