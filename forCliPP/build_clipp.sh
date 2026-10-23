#!/usr/bin/env bash
# Build CliPP's compiled kernel for THIS machine.
#
# CliPP 1.4.0 dlopens its kernel from build/*/CliPP*<platform>*.so, matching on
# Python's sys.platform. A kernel built on macOS is invisible to Linux and vice
# versa, so this must run on whatever machine serves the app -- including the
# deployment server, every time the app is deployed to a new host.
#
# Usage:  cd forCliPP && ./build_clipp.sh [python]
set -euo pipefail

cd "$(dirname "$0")/CliPP"
PY="${1:-python3}"

echo "CliPP kernel build"
echo "  directory : $(pwd)"
echo "  python    : $PY ($($PY --version 2>&1))"
echo "  platform  : $($PY -c 'import sys; print(sys.platform)')"

# numpy and pandas are imported by CliPP's own scripts, not by the build, but a
# build that succeeds on a host without them only defers the failure to runtime.
# These two are the whole list: nothing in 1.4.0 imports scipy, despite what
# upstream's Dockerfile installs.
$PY - <<'EOF'
import sys
import importlib.util
missing = [m for m in ("numpy", "pandas") if importlib.util.find_spec(m) is None]
if missing:
    sys.stderr.write("ERROR: python is missing required packages: %s\n" % ", ".join(missing))
    sys.stderr.write("       install them for THIS interpreter, e.g. pip install %s\n" % " ".join(missing))
    sys.exit(1)
EOF

# Eigen is vendored in-tree (eigen-3.4.0/), header-only, ~2,300 files. It has a
# habit of arriving incomplete: Eigen ships its own .gitignore containing the
# line "core", and because git sets core.ignoreCase=true on macOS and Windows
# filesystems, a plain "git add ." silently drops Eigen/Core and all 258 files
# under Eigen/src/Core. The compile then fails deep in a header with a message
# that says nothing about copying. Check for it up front instead. (That
# .gitignore has been deleted from this tree, so a copy made from here is safe;
# this guards against a tree copied from somewhere else.)
EIGEN_DIR="eigen-3.4.0"
missing=""
for h in "$EIGEN_DIR/Eigen/Core" "$EIGEN_DIR/Eigen/Dense" "$EIGEN_DIR/Eigen/src/Core/Matrix.h"; do
  [ -f "$h" ] || missing="$missing\n    $h"
done
n_eigen=$(find "$EIGEN_DIR" -type f 2>/dev/null | wc -l | tr -d ' ')
if [ -n "$missing" ] || [ "$n_eigen" -lt 2000 ]; then
  echo "ERROR: the vendored Eigen headers are incomplete ($n_eigen files, expected ~2,300)." >&2
  [ -n "$missing" ] && printf "       missing:$missing\n" >&2
  echo "       CliPP cannot build without them. This usually means the copy that" >&2
  echo "       produced this tree dropped files -- most often 'git add .' honouring" >&2
  echo "       Eigen's own .gitignore ('core' matches Eigen/Core case-insensitively)." >&2
  echo "       Re-copy forCliPP/CliPP/$EIGEN_DIR with rsync/tar, or restore it from" >&2
  echo "       https://gitlab.com/libeigen/eigen/-/releases/3.4.0" >&2
  exit 1
fi
echo "  eigen     : $n_eigen files, headers present"

rm -rf build
$PY setup.py build

PLATFORM=$($PY -c 'import sys; print(sys.platform)')
SO=$(find build -name "CliPP*${PLATFORM}*.so" | head -1 || true)
if [ -z "$SO" ]; then
  echo "BUILD FAILED: no build/*/CliPP*${PLATFORM}*.so was produced." >&2
  echo "CliPP will not run until this succeeds." >&2
  exit 1
fi

echo
echo "OK: $SO"
echo "This is the file CliPP loads at runtime. Restart the Shiny app to pick it up."
