#' ---
#' title:  "Example: How to Create Tables of Baseline Characteristics (Descriptive Statistics)"
#' author: "Certara PMxO R Tools Workstream"
#' date:   "`r format(Sys.Date(), '%d-%b-%Y')`"
#' output: html_document
#' ---

#+ echo=F
knitr::opts_chunk$set(echo=F, warning=FALSE, message=FALSE)
#+

library(table1c, quietly=TRUE)

# Read in the data from its 'spec'
dat <- read_from_spec("data_spec.yaml")

# Filter the data, one row per ID
dat <- one_row_per_id(dat, "id")

# Set up function for abbreviation footnotes
abbrev_footnote <- make_abbrev_footnote("abbrevs.yaml")

#' # Summary of Baseline Characteristics in the PK Population

#' ## Summary of Baseline Characteristics in the PK Population -- Demographic

table1(~ sex + race + ethnic + age + agecat + wt + ht + bmi + bsa + fdarenal | study, data=dat,
  footnote=abbrev_footnote("BMI", "BSA", "FDA", "SD", "Min", "Max", "N"))

#' ## Summary of Baseline Characteristics in the PK Population -- Laboratory Tests

table1(~ alb + alp + alt + ast + bili + crcl | study, data=dat,
  footnote=abbrev_footnote("ALP", "ALT", "AST", "CrCL", "SD", "Min", "Max", "N"))

#' ## Summary of Study Level Characteristics in the PK Population

table1(~ hv + form + fasted | study, data=dat,
  footnote=abbrev_footnote("N"))

#' # R session information

sessionInfo()

