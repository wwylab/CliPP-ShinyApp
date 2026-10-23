# forCliPP — the CliPP engine behind CliPP-on-Web

This directory holds **one** thing: the CliPP source tree the Shiny app shells
out to. There is deliberately no second copy, no platform variant and no
previous version here — if you find another CliPP directory alongside this one,
something has gone wrong.

    forCliPP/
      CliPP/            <- CliPP 1.4.0 (upstream tag 1.4.0, commit f80bc2f)
      build_clipp.sh    <- compiles the kernel for the current machine
      README.md         <- this file

`R/file_handlers.R` runs `python3 forCliPP/CliPP/run_clipp_main.py` with no
extra flags. 1.4.0 has no `--filtering` option: Filter 3 is commented out in its
`postprocess.R`, so its only mode is what CliPP 1.3.3 called "manuscript mode"
(`--filtering 2`), which is what every shipped result was produced with.

## The kernel must be compiled on the machine that serves the app

CliPP 1.4.0 does not import a Python extension module. It `dlopen`s its kernel
by globbing

    forCliPP/CliPP/build/*/CliPP*<sys.platform>*.so

so a `.so` built on macOS (`CliPP.cpython-39-darwin.so`) is **invisible** on a
Linux host, which looks for `*linux*.so`. Copying the repo to a server is not
enough. On every deployment host, run:

    cd forCliPP
    ./build_clipp.sh            # or: ./build_clipp.sh /path/to/python3

The script builds into `CliPP/build/`, verifies that a `.so` matching this
platform was produced, and fails loudly if not. Restart the app afterwards.

Requirements on the host: a C++ compiler (`g++` on Linux, built with `-fopenmp`;
`clang++` on macOS), Python ≥ 3.5.1 with **numpy and pandas** — that is the
entire list, nothing in 1.4.0 imports scipy despite upstream's Dockerfile
installing it — and an R with base `parallel`, which `preprocess.R` uses. Eigen
3.4.0 is committed inside `CliPP/eigen-3.4.0`, so there is nothing to fetch.

The interpreter matters: `build_clipp.sh` checks the one it is given (default
`python3`) and refuses to build against a Python that cannot import numpy and
pandas, since that failure would otherwise only surface on the first upload.
Pass an explicit path if the default `python3` is not the one that will run the
app — on this laptop, for instance, `/usr/bin/python3` has numpy while the
Homebrew `python3` does not.

## Eigen ships with this app — and why it used to go missing

Eigen 3.4.0 is vendored in full at `CliPP/eigen-3.4.0` (2,310 files, ~26 MB,
header-only). There is no submodule and nothing to fetch: `setup.py` compiles
with `include_dirs=['eigen-3.4.0']`, relative to `CliPP/`, which is why
`build_clipp.sh` builds from inside that directory.

If Eigen has gone missing on a previous deploy, this is almost certainly why.
Eigen ships **its own `.gitignore`**, and line 11 of it is `core` — intended to
ignore core dumps. Git sets `core.ignoreCase = true` on macOS and Windows
filesystems, so that pattern also matches `Eigen/Core` (the umbrella header) and
everything under `Eigen/src/Core/`. Putting this tree into a fresh repository
and running `git add .` therefore drops **259 files, including `Eigen/Core`**,
with no warning; the next build dies inside a header, complaining about
something that looks unrelated to copying. Verified here: `git add -A` staged
2,052 of 2,311 eigen files before the fix, and all 2,310 after.

Two things now prevent it:

1. That vendored `.gitignore` has been **deleted from this tree**, so any copy
   made from here — git, rsync, tar, zip — carries Eigen intact.
2. `build_clipp.sh` refuses to build if `Eigen/Core`, `Eigen/Dense` or
   `Eigen/src/Core/Matrix.h` is absent, or if the directory holds fewer than
   2,000 files, and says explicitly that the copy dropped files rather than
   letting the compiler fail obscurely.

If Eigen ever does need restoring, it is the plain 3.4.0 release tarball from
<https://gitlab.com/libeigen/eigen/-/releases/3.4.0>, unpacked as
`CliPP/eigen-3.4.0/`.

Note for whoever automates the deploy: prefer `rsync -a` or `tar` over
`git add`-based packaging for this directory. The app as a whole is ~3.1 GB and
~146k files, nearly all of it `dataSource/`, which is well past the default
limits of `rsconnect`-style bundling.

If the kernel is missing, the app says so instead of failing obscurely:
`global.R` prints a loud warning at startup, and an upload returns
"CliPP's compiled kernel is missing for this platform" rather than the useless
"CliPP did not generate the expected output files".

## Upload size limit

`R/file_handlers.R` rejects samples above `MAX_SNVS_WEB` (30,000 SNVs) before
CliPP is invoked, and points the user at the command-line version. CliPP itself
has no such limit; it is a wall-clock guard for the shared server, since the
pairwise kernel grows with the square of the mutation count.

## Version history of this directory

- **1.4.0** (current). Installed 2026-09-16.
- **1.3.3 + a local `--filtering 2` patch** (previous). Moved out of the app to
  `provenance/CliPP_1.3.3_backup/` so only one engine is reachable here. It is
  also recoverable from upstream at tag `1.3.3` (commit c944623).
- **`CliPP-rhel7/`** (deleted 2026-09-16). A stub — Dockerfile, LICENSE,
  README, `run_clipp_main.py`, `setup.py`, with no `src/` and no build — that no
  R code referenced. Its Dockerfile was a build recipe for a Linux host, which
  is what `build_clipp.sh` now does explicitly. Nothing was lost; see
  `Dockerfile` inside `CliPP/` for the upstream container recipe.
