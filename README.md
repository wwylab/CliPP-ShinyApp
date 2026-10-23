<p align="center">

  <h1 align="center">
   CliPP-ShinyApp is the web version of CliPP: <a href="https://bioinformatics.mdanderson.org/apps/CliPP">https://bioinformatics.mdanderson.org/apps/CliPP 
  </h1>

  <p align="center">
   <a href="https://github.com/wwylab/CliPP-ShinyApp" target="_blank">
     <img alt="Status"
          src="https://img.shields.io/badge/status-active-success.svg" />
   </a>
   <a href="https://github.com/wwylab/CliPP-ShinyApp/issues" target="_blank">
     <img alt="Github Issues"
          src="https://img.shields.io/github/issues/wwylab/CliPP-ShinyApp" />
   </a>
   <a href="https://github.com/wwylab/CliPP-ShinyApp/pulls" target="_blank">
     <img alt="Pull Requests"
          src="https://img.shields.io/github/issues-pr/wwylab/CliPP-ShinyApp" />
   </a>
   <a href="https://github.com/wwylab/CliPP-ShinyApp/blob/master/LICENSE" target="_blank">
     <img alt="License: GPLv3"
          src="https://img.shields.io/badge/License-GPLv3-blue.svg" />
   </a>
   <br />
  </p>

  </p>
  <p align="center">
   <a href="https://github.com/wwylab/CliPP-ShinyApp/issues/new?assignees=&labels=&template=feature_request.md&title=Descriptive%20Title&labels=enhancement">Request Feature</a>
    | 
   <a href="https://github.com/wwylab/CliPP-ShinyApp/issues/new?assignees=&labels=&template=bug_report.md&title=Descriptive%20Title&labels=bug">Report Bug</a>
   <br />
    ⭐ Consider starring the repo! ⭐
   <br />
  </p>
</p>

---
## Quick Start

### Requirements

| | |
|---|---|
| R | 4.5.1, with `shiny` (1.12.1), `bslib`, `shinyjs`, `shinyWidgets`, `plotly`, `ggplot2`, `ggpubr`, `dplyr`, `data.table` |
| Python | 3.9, with `numpy` and `pandas` |
| Build | a C++ compiler with OpenMP, to build CliPP's kernel |

The app shells out to the CliPP command-line tool as a subprocess; it is a front
end to the released tool, not a reimplementation, so a browser run and a
command-line run execute the same code.

### 1. Clone this repository

```
git clone https://github.com/wwylab/CliPP-ShinyApp.git
cd CliPP-ShinyApp
```

### 2. Install CliPP into forCliPP/

CliPP is a separate project and is not vendored here.

```
git clone https://github.com/wwylab/CliPP.git forCliPP/CliPP
cd forCliPP && ./build_clipp.sh
```

`build_clipp.sh` compiles CliPP's C++ kernel **for the machine it runs on**.
CliPP loads that kernel by matching on `sys.platform`, so a kernel built on
macOS is invisible to Linux and vice versa: it has to be built on whatever
machine serves the app, including the deployment host, every time the app moves
to a new host. `forCliPP/README.md` covers the failure modes, including the one
where Eigen's own `.gitignore` silently truncates a copied tree.

At startup the app logs the kernel it found, or warns loudly if there is none
for the current platform. Check that line after deploying.

### 3. Add the data (optional)

`dataSource/` is not tracked here -- the precomputed TCGA, PCAWG and CliPPSim4k
results are far too large for git. The app starts without them; the Downloads
page and the cohort dropdowns will simply be empty. To populate them, place the
published archives and their unpacked per-sample directories under
`dataSource/`.

### 4. Run

```
R -e "shiny::runApp('.', port = 3838)"
```

## Repository layout

```
global.R, ui.R, server.R     the app
R/                           data processing, file handling, plotting, silhouette
assets/css, assets/js        styles and the citation export helpers
assets/img/tutorial          walkthrough screenshots used by the Tutorial tab
forCliPP/build_clipp.sh      builds CliPP's kernel on this machine
forCliPP/README.md           CliPP setup and deployment notes
dataSource/                  (untracked) downloadable data
```

## Publication
Manuscript under preparation.

## Contact
* mdmontierth@mdanderson.org
* wwang7@mdanderson.org

---
## COPYRIGHT 
@2025 The University of Texas MD Anderson Cancer Center
