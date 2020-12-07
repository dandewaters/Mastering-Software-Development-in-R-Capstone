# msdirCapstone

## Introduction

This is my submission for the capstone project for the Mastering Software Development in R Specialization provided by Johns Hopkins University on Coursera. For this project, we were tasked to create a package with functions to read, clean, and plot NOAA's earthquake dataset.

## Installation
 
To install, make sure you have the [devtools package](https://cran.r-project.org/package=devtools) installed and loaded. Then run the following commands:

```{r installation, eval=FALSE}
library(devtools)
install_github("dandewaters/msdirCapstone")
```

## Vignettes

Read the intro vignette by running install_github with vignettes = TRUE and running the following commands:

```{r vignettes, eval=FALSE}
library(devtools)
install_github("dandewaters/msdirCapstone", build_vignette=TRUE)
vignette("introduction", package="msdirCapstone")
```

## References

National Geophysical Data Center / World Data Service (NGDC/WDS): NCEI/WDS Global Significant Earthquake Database. NOAA National Centers for Environmental Information. doi:10.7289/V5TD9V7K
