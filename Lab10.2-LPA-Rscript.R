# ---
# title: "Lab 10.2 - Latent Profile Analysis "
# author: "Adam Garber"
# subtitle: 'Structural Equation Modeling - Instructor: Karen Nylund-Gibson'
# date: "`June 4, 2020')`"
# output: 
# ---
 
# `University of California, Santa Barbara`

# ______________________________________________

# Lab preparation

# ______________________________________________

## Creating a version-controlled R-Project with Github

# Download repository here: https://github.com/garberadamc/SEM-Lab10
# 
# On the Github repository webpage:
#   
# a. `fork` your own `branch` of the lab repository 
# b. copy the repository web URL address from the `clone or download` menu
# 
# Within R-Studio:
#   
# c. click "NEW PROJECT" 
# d. choose option `Version Control`
# e. choose option `Git`
# f. paste the repository web URL path copied from the `clone or download` menu on Github page
# g. choose location of the R-Project 

# ______________________________________________

## Data source:

# 1. The first example closely follows the vignette used to demonstrate the `tidyLPA` package (Rosenberg, 2019): [$\color{blue}{\text{See detailed documentation of this model here}}$](https://data-edu.github.io/tidyLPA/articles/Introduction_to_tidyLPA.html)
# 
# This model utilizes the `PISA` data collected in the U.S. in 2015. To learn more about this data [$\color{blue}{\text{see here}}$](http://www.oecd.org/pisa/data/). 
# 
# To access the 2015 US `PISA` data in R use the following code:
# `devtools::install_github("jrosen48/pisaUSA15")`
# `library(pisaUSA15)`
# `open_codebook()`
# 
# 2. The second examples utilizes 4 test score measures from the public-use dataset, *The Longitudinal Survey of American Youth* (**LSAY**):  [$\color{blue}{\text{See documentation here}}$](https://www.lsay.org/)

# ______________________________________________

# Load packages
library(naniar)
library(tidyverse)
library(haven)
library(glue)
library(MplusAutomation)
library(rhdf5)
library(here)
library(janitor)
library(gt)
library(tidyLPA)

# Load data
pisa <- pisaUSA15
 
# ___________________________________

# Latent Profile Analysis 

# ___________________________________


 

# from tutorial (Rosenberg, 2019).

# - `model 1` Class-invariant / Diagonal: Equal variances, and covariances fixed to 0
# - `model 2` Class-varying / Diagonal: Free variances and covariances fixed to 0
# - `model 3` Class-invariant / Non-Diagonal: Equal variances and equal covariances
# - `model 4` Free variances, and equal covariances
# - `model 5` Equal variances, and free covariances 
# - `model 6` Class Varying / Non-Diagonal: Free variances and free covariances

# ___________________________________

# Example 1: PISA dataset from the `tidyLPA` package

# ___________________________________

# Enumerate using `estimate_profiles()`:
  
# - Estimate models with classes $K = 1:3$
# - Model has 4 continuous indicators
# - Default variance-covariance sprecifications (model 1)
# - Add line `scale() %>%` to center indicator means

lpa_models <- pisa[1:500,] %>%
  select(broad_interest, enjoyment, instrumental_mot, self_efficacy) %>%
  estimate_profiles(1:3,
                    package = "MplusAutomation",
                    ANALYSIS = "starts = 100, 20;",
                    variances = c("equal", "varying"),
                    covariances = c("zero", "varying"))

get_fit(lpa_models)
 

# ___________________________________

# Plot 3-class model

# **Note:** single imputation is used in this example as `plot_profiles()` requires complete cases

pisa[1:200,] %>%
  select(broad_interest, enjoyment, instrumental_mot, self_efficacy) %>%
  single_imputation() %>%
  estimate_profiles(3, package = "MplusAutomation") %>% 
  plot_profiles(sd=FALSE)
 

# ___________________________________

# Plot densities for classes `k = 1:4`

pisa[1:500, c("broad_interest","enjoyment")] %>%
  single_imputation() %>%
  estimate_profiles(1:4, package = "MplusAutomation") %>%
  plot_density()

 

# ___________________________________

## Example 2: Math, Science, Physics, and Biology measures (LSAY).

# ___________________________________


# Read in data 
lsay_data <- read_spss(here("data", "lsay_lab10.2_lpa.csv"))

# ___________________________________

# Run a quick enumeration 

lpa_k14  <- lapply(1:4, function(k) {
  lpa_enum  <- mplusObject(
    
    TITLE = glue("Class {k}"), 
    
    VARIABLE = glue(
      "usevar = mth_scor-bio_scor;
     classes = c({k}); "),
    
    ANALYSIS = 
      "estimator = mlr; 
    type = mixture;
    starts = 200 50; 
    processors = 10;",
    
    OUTPUT = "sampstat residual tech11 tech14;",
    
    PLOT = 
      "type = plot3; 
     series = mth_scor-bio_scor(*);",
    
    usevariables = colnames(lsay_data),
    rdata = lsay_data)
  
  lpa_enum_fit <- mplusModeler(lpa_enum, 
                               dataout=glue(here("enum_lpa", "c_lpa_lsay_Lab10.dat")),
                               modelout=glue(here("enum_lpa", "c{k}_lpa_lsay_Lab10.inp")) ,
                               check=TRUE, run = TRUE, hashfilename = FALSE)
})

 
# ___________________________________

# Plot 3-class profile
lsay_data[1:500,5:8] %>%
  single_imputation() %>%
  estimate_profiles(1:4, package = "MplusAutomation") %>% 
  plot_profiles(sd=FALSE)
 

# *Figure.* Here we see ordered solutions.

# ___________________________________

# Compare model fit.

all_output <- readModels(here("enum_lpa"), quiet = TRUE)

enum_extract <- LatexSummaryTable(all_output,                                             
                                  keepCols=c("Title","Parameters", "LL", "BIC",             
                                             "aBIC", "BLRT_PValue", "T11_VLMR_PValue"),     
                                  sortBy = "Title")                                         

gt(enum_extract)

#  ______________________________________________

# [Lab Materials - Return to Home Page](https://garberadamc.github.io/project-site/)

#  ______________________________________________
# 
# # References
# 
# Hallquist, M. N., & Wiley, J. F. (2018). MplusAutomation: An R Package for Facilitating Large-Scale Latent Variable Analyses in Mplus. Structural equation modeling: a multidisciplinary journal, 25(4), 621-638.
# 
# Miller, J. D., Hoffer, T., Suchner, R., Brown, K., & Nelson, C. (1992). LSAY codebook. Northern Illinois University.
# 
# Muthén, B. O., Muthén, L. K., & Asparouhov, T. (2017). Regression and mediation analysis using Mplus. Los Angeles, CA: Muthén & Muthén.
# 
# Muthén, L.K. and Muthén, B.O. (1998-2017).  Mplus User’s Guide.  Eighth Edition. Los Angeles, CA: Muthén & Muthén
# 
# Rosenberg, J. M., van Lissa, C. J., Beymer, P. N., Anderson, D. J., Schell, M. J. & Schmidt, J. A. (2019). tidyLPA: Easily carry out Latent Profile Analysis (LPA) using open-source or commercial software [R package]. https://data-edu.github.io/tidyLPA/
#   
# R Core Team (2017). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL http://www.R-project.org/
#   
# Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686
# 
# ---------------------------------------------------






