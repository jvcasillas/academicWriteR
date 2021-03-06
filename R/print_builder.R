#' Print builder
#'
#' A series of helper functions for printing model parameters
#'
#' @param model a model object
#' @param latex Output is LaTeX or markdown
#' @keywords Report model
#' @import dplyr
#' @importFrom rlang .data
#' @export

print_builder <- function(model, latex = TRUE) {
  UseMethod(generic = "print_builder", object = model)
}

#' @export
# Generic builder
print_builder.default <- function(model, latex = TRUE) {
  out <- paste(sep = "; ",
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

#' @export
# Builder for bayesian models
print_builder.brmsfit <- function(model, latex = TRUE) {
  out <- paste(sep = "; ",
    parameterize(model[, "Median"], latex = latex),
    confidence(model[, "CI_low"], model[, "CI_high"], latex = latex),
    roper(model[, "ROPE_Percentage"], latex = latex),
    mper(model[, "pd"], latex = latex)) %>%
    parenthesize()

  if (latex == FALSE) {
    out <- gsub("-", "&minus;", out)
  }

  return(out)
}






# Put parenthesis around something
parenthesize <- function(x) {
  out <- paste0("(", x, ")")
  return(out)
}

# Put brackets around something
bracketize <- function(x) {
  out <- paste0("[", x, "]")
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
  out <- paste0(" $", x, "$ ")
  return(out)
}

# Build parameter estimate
parameterize <- function(x, latex = TRUE) {
  if (latex == TRUE) {
    beta <- paste0("$\\beta ", equalize(latex = TRUE), " ", x, "$")
  } else {
    beta <- paste0("&beta;", equalize(latex = FALSE), x)
  }
  return(beta)
}

# Build standard error
std_error <- function(x, latex = TRUE) {
  if (latex == TRUE) {
    error <- paste0("SE", mathesize(equalize()), "$", x, "$")
  } else {
    error <- paste0("SE", equalize(latex = latex), x)
  }
  return(error)
}

# Build confidence interval
confidence <- function(lb, ub, latex = TRUE) {
  # Build it
  if (latex == TRUE) {
    interval <- paste0("$", lb, "$, $", ub, "$")
    ci <- paste0("CI ", mathesize(equalize()), bracketize(interval))
    } else {
    interval <- paste0(lb, ", ", ub)
    ci <- paste0("CI", equalize(latex = latex), bracketize(interval))
  }
  return(ci)
}

# Build statistic
statistic <- function(x, latex = TRUE) {
  if (latex == TRUE) {
    stat <- paste0("\\emph{t}", mathesize(equalize(latex = TRUE)), "$",
                   x, "$")
  } else {
    stat <- paste0("*t*", equalize(latex = FALSE), x)
  }
  return(stat)
}

# Build rope
roper <- function(x, latex = TRUE) {
  if (latex == TRUE) {
    stat <- paste0("ROPE", mathesize(equalize(latex = TRUE)), "$",
                   x, "$")
  } else {
    stat <- paste0("ROPE", equalize(latex = FALSE), x)
  }
  return(stat)
}

# Build MPE
mper <- function(x, latex = TRUE) {
  if (latex == TRUE) {
    stat <- paste0("MPE", mathesize(equalize(latex = TRUE)), "$",
                   x, "$")
  } else {
    stat <- paste0("MPE", equalize(latex = FALSE), x)
  }
  return(stat)
}
