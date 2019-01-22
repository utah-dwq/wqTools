## Utah DWQ's wqTools R-package

### Description
wqTools is a collection of R tools developed and designed for use by Utah Division of Water Quality staff.
This includes functions for reading data from EPA's water quality portal, EPA ECHO, assigning beneficial uses & assessment units, and generating study maps.

### Install
This package is in active development and can be installed via GitHub with `devtools::install_github("utah-dwq/wqTools")`.

### Manual
See wqTools-manual.pdf in GitHub for more information.

### Vignettes
Detailed examples of package usage are available as vignettes. To install the wqTools package including vignettes use `devtools::install_github("utah-dwq/wqTools", build_opts = c("--no-resave-data", "--no-manual"))`.
Note that this install will take considerably longer than a standard install.
Then use `library(wqTools)` and `browseVignettes("wqTools")` to see vignettes. If installing from base R console, you may need to install pandoc (https://pandoc.org/installing.html) to install wqTools with vignettes. If installing from RStudio, a separate pandoc installation is not necessary.
