#!/usr/bin/env sh

set -e
nix build
rm "result"