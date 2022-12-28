#!/usr/bin/env bash

set -eux -o pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

if [[ ! -d venv ]]; then
    python3 -m venv venv
fi

venv/bin/pip install Jinja2 PyYAML
venv/bin/python generate.py
