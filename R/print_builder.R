#' Print builder
#'
#' A series of helper functions for printing model parameters
#'
#' @param model a model object
#' @param latex Output is LaTeX or markdown
#' @keywords Report model
#' @export
#' @examples
#' library(stringr)

# Generic builder
print_builder <- function(model, latex = TRUE) {
  out <- paste0(
    parameterize(model[, "estimate"], latex = latex),
    std_error(model[, "std.error"], latex = latex),
    confidence(model[, "conf.low"], model[, "conf.high"], latex = latex),
    statistic(model[, "statistic"], latex = latex),
    print_pval(model[, "p.value"], latex = latex)) %>%
      parenthesize()

  if (latex == FALSE) {
    out <- gsub("-", "&minus;", out)
  }

  return(out)
}

# Separate elements in builder
sep <- function(x, latex = TRUE) {
  if (latex == TRUE) {
    sep <- "; "
  } else {
    sep <- ";&nbsp;"
  }
  return(sep)
}

# Put parenthesis around something
parenthesize <- function(x) {
  l_side <- "("
  r_side <- ")"
  out <- paste0(l_side, x, r_side)
  return(out)
}

# Put brackets around something
bracketize <- function(x) {
  l_side <- "["
  r_side <- "]"
  out <- paste0(l_side, x, r_side)
  return(out)
}

# Get math equal sign
equalize <- function(x, latex = TRUE) {
  if (latex == TRUE) {
    equal <- "="
  } else {
    equal <- "&nbsp;=&nbsp;"
  }
  return(equal)
}

# Get into math mode
mathesize <- function(x) {
  sym_open <- " $"
  sym_close <- "$ "
  out <- paste0(sym_open, x, sym_close)
  return(out)
}

# Build parameter estimate
parameterize <- function(x, latex = TRUE) {
  if (latex == TRUE) {
    beta <- paste0("$\\beta ", equalize(latex = TRUE), " ", x, "$", sep())
  } else {
    beta <- paste0("&beta;", equalize(latex = FALSE), x, sep(latex = latex))
  }
  return(beta)
}

# Build standard error
std_error <- function(x, latex = TRUE) {
  if (latex == TRUE) {
    error <- paste0("SE", mathesize(equalize()), "$", x, "$", sep())
  } else {
    error <- paste0("SE", equalize(latex = latex), x, sep(latex = latex))
  }
  return(error)
}

# Build confidence interval
confidence <- function(lb, ub, latex = TRUE) {
  # Build it
  if (latex == TRUE) {
    interval <- paste0("$", lb, "$, $", ub, "$")
    ci <- paste0("CI ", mathesize(equalize()), bracketize(interval), sep())
    } else {
    interval <- paste0(lb, ", ", ub)
    ci <- paste0("CI", equalize(latex = latex), bracketize(interval),
                 sep(latex = latex))
  }
  return(ci)
}

# Build statistic
statistic <- function(x, latex = TRUE) {
  if (latex == TRUE) {
    stat <- paste0("\\emph{t}", mathesize(equalize(latex = TRUE)), "$",
                   x, "$", sep())
  } else {
    stat <- paste0("*t*", equalize(latex = FALSE), x, sep(latex = latex))
  }
  return(stat)
}
