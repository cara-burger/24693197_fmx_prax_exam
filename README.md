# Financial Econometrics Practical Exam 2026

Student number: 24693197

This README contains and describes all code used in the analysis.
Functions are stored in each question’s `code/` folder (for example
`Questions/Question1/code/`) and are sourced in the relevant section
below. The write-ups (pdf, html or slides) are found in the `Questions/`
folders.

# Setup

The project was created with `fmxdat::make_project()`. The question
folders were created with Texevier’s folder functions. The chunk below
is not evaluated when knitting (`eval = F`), as re-running it would try
to create the folders again.

``` r
# Project root:
fmxdat::make_project()

# Question folders (PDF Texevier):
Texevier::create_template(directory = "Questions", template_name = "Question1",
                          build_project = TRUE, open_project = FALSE)

# HTML Texevier (research report for Question 2):
Texevier::create_template_html(directory = "Questions", template_name = "Question2",
                               build_project = TRUE, open_project = FALSE)

Texevier::create_template(directory = "Questions", template_name = "Question3",
                          build_project = TRUE, open_project = FALSE)

Texevier::create_template(directory = "Questions", template_name = "Question4",
                          build_project = TRUE, open_project = FALSE)
```

The exam data (`Prac26_Data.zip`) was unzipped into `data/` in the root
and the files used per question were copied into that question’s `data/`
folder. All `data` folders are excluded from Git in `.gitignore`.

``` r
# Hide package messages and warnings in the knitted README; fix default figure size:
knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.width = 8, fig.height = 5)

rm(list = ls()) # Clean your environment:
gc() # garbage collection
```

    ##           used (Mb) gc trigger (Mb) limit (Mb) max used (Mb)
    ## Ncells  565815 30.3    1258771 67.3         NA   715685 38.3
    ## Vcells 1081456  8.3    8388608 64.0      24576  2010723 15.4

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.1     ✔ readr     2.2.0
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
    ## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
    ## ✔ purrr     1.2.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
pacman::p_load("xts", "tbl2xts", "PerformanceAnalytics", "lubridate", "glue", "RcppRoll", "gt", "rmsfuns")
if (!require("fmxdat")) pacman::p_install_gh("Nicktz/fmxdat")
```

    ## Loading required package: fmxdat

``` r
# Shared functions used across questions:
list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```

# Question 1: Exploring Risk and Uncertainty

*Write-up:* `Questions/Question1/Question1.pdf`

``` r
# Functions for this question:
list.files('Questions/Question1/code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```

## Data

## Part A: Has local volatility increased, and does it mirror global volatility?

## Part B: SafeCore volatility, risk sources and factor exposures

## Part C: Dispersion, correlations and alpha opportunity

# Question 2: Mid, Small and Large Cap Indices

``` r
# Functions for this question:
list.files('Questions/Question2/code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```

# Question 3: Portfolio Return Comparison

``` r
# Functions for this question:
list.files('Questions/Question3/code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```

# Question 4: Portfolio Construction

``` r
# Functions for this question:
list.files('Questions/Question4/code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```
