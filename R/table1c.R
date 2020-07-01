
#' Generate an HTML table of descriptive statistics
#'
#' The \code{table1c} package is a light wrapper around the package
#' \href{https://CRAN.R-project.org/package=table1}{\code{table1}}, with some
#' customizations for the convenience of Certara IDD. The main interface (i.e.
#' this function), is retained. For details, see the function in the parent
#' package: \code{\link[table1]{table1}}.
#'
#' @param x An object.
#' @param ... Additional aguments, see \code{\link[table1]{table1}}.
#' @import table1 
#' @export
table1 <- function(x, ...) {
  render.continuous.default <- function(x, ...) {
    with(stats.apply.rounding(stats.default(x, ...), ...), c("",
        "Mean (SD)"    = sprintf("%s (%s)", MEAN, SD),
        "Median (CV%)" = sprintf("%s (%s)", MEDIAN, CV),
        "[Min, Max]"   = sprintf("[%s, %s]", MIN, MAX)))
  }
  if ("render.continuous" %in% names(list(...))) {
    y <- table1::table1(x, ...)
  } else {
    y <- table1::table1(x, ..., render.continuous=render.continuous.default)
  }
  class(y) <- c("table1c", class(y))
  y
}

#' @export
print.table1c <- function(x, ...) {
  if (interactive()) {
    x <- htmltools::HTML(x)
    default.style <- htmltools::htmlDependency("table1c", "1.0",
      src=system.file(package="table1c", "table1c_defaults_1.0"),
      stylesheet="table1c_defaults.css")
    x <- htmltools::div(class="Rtable1", default.style, x)
    x <- htmltools::browsable(x)
    print(x, ...) # Calls htmltools:::print.html(x, ...)
  } else {
    cat(x)
  }
  invisible(x)
}

#' @importFrom knitr knit_print
#' @export
knit_print.table1c <- function(x, ...) {
  knit_to_html <-
    !is.null(knitr::opts_knit$get("rmarkdown.pandoc.to")) &&
    grepl("^html", knitr::opts_knit$get("rmarkdown.pandoc.to"))

  if (knit_to_html) {
    x <- htmltools::HTML(x)
    default.style <- htmltools::htmlDependency("table1c", "1.0",
      src=system.file(package="table1c", "table1c_defaults_1.0"),
      stylesheet="table1c_defaults.css")
    x <- htmltools::div(class="Rtable1", default.style, x)
    knitr::knit_print(x, ...)
  } else {
    knitr::knit_print(as.character(x), ...)
  }
}


#' Make footnotes of abbreviations
#'
#' This higher order function can be used to generate functions to produce
#' strings that are suitable for use as footnotes in tables (or figures) that
#' contain abbreviations. By convention, it is required that all abbreviations
#' appearing in a table or figure be spelled out in full in a footnote.
#'
#' @param abbrevs Either a named \code{list} of abbreviations, where the list
#' names are the abbreviations and the list items are the strings being
#' abbreviated, or the name of a YAML file that contains such a list.
#' @param sort Should the abbreviations be automatically sorted?
#' @param sep The seperator to use between abbreviations.
#' @param end Punctuation to appear at the end of the footnote.
#' @return A function that takes any number of arguments, each argument being
#' an abbreviation to include. This function, when called, will generate the
#' footnote string.
#'
#' @details If an abbreviation is not found in the list, the function will
#' generate a warning, and produce a strange HTML code that renders as a red on
#' yellow "XXX" in the browser (to draw attention to the issue).
#'
#' @examples
#' abbrevs <- list(
#'   ALP  = "alkaline phosphatase",
#'   ALT  = "alanine aminotransferase",
#'   AST  = "aspartate aminotransferase",
#'   BMI  = "body mass index",
#'   BSA  = "body surface area")
#'
#' abbrev_footnote <- make_abbrev_footnote(abbrevs)
#' abbrev_footnote("BMI", "ALP", "ALT")
#'
#' \dontrun{
#' abbrev_footnote("BMI", "XXX", "YYY")  # Generates a warning
#' }
#' @export
make_abbrev_footnote <- function(abbrevs, sort=TRUE, sep="; ", end=".") {
  if (is.character(abbrevs)) {
    abbrevs <- yaml::read_yaml(abbrevs)
  }
  function(...) {
    x <- as.character(unlist(list(...)))
    if (isTRUE(sort)) {
      x <- sort(x)
    }
    y <- unlist(abbrevs)[x]
    if (any(is.na(y))) {
      warning(sprintf("Abbreviation not found for: %s", paste(x[is.na(y)], collapse=", ")))
    }
    y[is.na(y)] <- "<span style='color: red; background-color: yellow;'>XXX</span>"
    paste0(paste0(x, "=", y, collapse=sep), end)
  }
}


#' Read a dataset from a data specification file
#'
#' This convenience function can be used to read a dataset and augment it with
#' meta-information from a specification file. The specification file is a YAML
#' file with certain format. Using the function allows a cleaner separation of
#' script logic from data and meta-data, resulting in more generic and
#' re-usable scripts (the data and meta-data can be centralized and segregated
#' from the scripting logic).
#'
#' @param spec Either the name of a YAML file that contains the data
#' specification, or an equivalent object (i.e. as returned by
#' \code{\link[yaml]{read_yaml}}).
#' @param read.fun A function that reads in data. It should accept a file name
#' as its first argument and return some sort of
#' \code{\link[base]{data.frame}}.
#' @param file The name of the file containing the data. if \code{NULL}, then
#' the file name will be taken from the \code{dataset} item of the data
#' specification.
#' @param ... Additional arguments that get passed to \code{read.fun}.
#' @return A \code{\link[base]{data.frame}}, as returned by \code{read.fun},
#' that has been augmented with meta-information from the data specification
#' (i.e. labels, factor levels).
#' @importFrom utils read.csv
#' @export
read_from_spec <- function(spec, read.fun=read.csv, file=NULL, ...) {
  if (is.character(spec)) {
    path <- dirname(spec)
    spec <- yaml::read_yaml(spec)
  } else {
    path <- getwd()
  }
  if (is.null(file)) {
    file <- file.path(path, spec$dataset)
  }
  table1::t1read(data=file, metadata=spec, read.fun=read.fun, ...)
}

#' Extract one row per ID from a \code{data.frame}
#'
#' A convenience function to filter a \code{data.frame} retaining one row for
#' each unique value of an ID column. Only columns that are invariant (and
#' hence unambiguouse) within each ID level are retained.
#'
#' @param data A \code{\link[base]{data.frame}}.
#' @param idcol The name or numeric index of the ID column.
#' @return A \code{\link[base]{data.frame}} in which each ID appears in a
#' single row.  Only columns that are invariant (and hence unambiguous)
#' within each ID level are retained.
#' @examples
#' library(nlme)
#' one_row_per_id(Phenobarb, "Subject")
#' @export
one_row_per_id <- function(data, idcol) {
  subsetp <- function(x, ..., droplevels=TRUE) {
    y <- eval(subset(x, ...))
    if (droplevels) {
      y <- droplevels(y)
    }
    if (is.data.frame(x)) {
      for (i in seq_along(x)) {
        a <- attributes(x[[i]])
        if (droplevels && is.factor(y[[i]])) {
          a$levels <- attributes(y[[i]])$levels
        }
        attributes(y[[i]]) <- a
      }
    }
    y
  }
  id <- data[[idcol]]
  if (is.null(id)) stop("ID column not found")
  keep <- sapply(data, function(x) {
    all(tapply(x, id, function(y) length(unique(y)) == 1))
  })
  .idx <- !duplicated(id)
  subsetp(data, .idx)[, keep]
}


# vim: ts=2 sw=2 et
