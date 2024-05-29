#!/bin/bash
set -e

# This script builds the ESA SNAP Python API and runs the tests
# In theory this shouldn't be required but it seems that config, at least on
# my machine, is not working correctly. This script is a workaround for that.

# Set the paths to:
# ESA SNAP Python API
ESA_SNAPPY="$HOME/.snap/snap-python/esa_snappy"
# ESA SNAP binaries
ESA_SNAP_BIN="$HOME/esa-snap/bin"
# ESA SNAP virtual environment
VENV_LOC="$HOME/virtualenvs/snap"

# Create the virtual environment
if [ ! -d "$VENV_LOC" ]; then
  python -m venv "$VENV_LOC"
fi

# Activate the virtual environment
source "$VENV_LOC/bin/activate"

# Copy the jpy wheel to the binary folder
cp "$ESA_SNAPPY/lib/jpy-0.13.0-cp310-cp310-linux_x86_64.whl" \
  "$ESA_SNAP_BIN/jpy-0.13.0-cp310-cp310-linux_x86_64.whl"

# Copy the python module to the virtual environment
cp -r "$ESA_SNAPPY" "$VENV_LOC"/lib/python*/site-packages

# Run snappy-conf to set up the python module
"$ESA_SNAP_BIN/snappy-conf" "$VENV_LOC/bin/python" "$VENV_LOC"/lib/python*/site-packages

# Install missing dependencies and pytest
pip install numpy
pip install pytest

# Run tests
cd "$VENV_LOC"/lib/python*/site-packages/esa_snappy/tests
pytest
