## Install instructions

An R version of 4.5.0 is needed to run the code and it can be downloaded from https://cran.r-project.org/

The following R packages, and their dependencies, are needed to run the reproducible code:  

```r
packages <- c("tidyverse", "patchwork", "MASS", "ggh4x", 
              "broom", "knitr", "rpart", "infotheo", "patchwork", 
              "readr", "kableExtra", "scales", "qqplotr")

```

They can be installed using the following code:

```r
install.packages(packages, repos = "https://cran.r-project.org")
```

The source code for the package `adtoolbox` is included in the repository. You can also install from GitHub using the following code:

```r
# install.packages("remotes")
remotes::install_github("huizezhang-sherry/adtoolbox")
```
