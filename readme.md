## Utah DWQ's wqTools R-package

### Description
wqTools is a collection of R tools developed and designed for use by Utah Division of Water Quality staff.
This includes functions for reading data from EPA's water quality portal, EPA ECHO, assigning beneficial uses & assessment units, and generating study maps.

### Install
This package is in active development and can be installed via GitHub with devtools::install_github("utah-dwq/wqTools").

### Manual
See wqTools-manual.pdf in GitHub for more information.

### Vignettes
Use `devtools::build_vignettes(paste0(.libPaths(),"/wqTools"))` then `browseVignettes("wqTools")` to see detailed examples of package usage. You may need to install pandoc (https://pandoc.org/installing.html) to build vignettes.
