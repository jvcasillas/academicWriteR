#' Print model parameters
#'
#' This function takes a model object and prints a row. This is used for
#' reporting results.
#'
#' @param model A model object
#' @param parameter A model parameter
#' @param latex Determine if output is LaTeX or markdown
#' @keywords Report model
#' @imort dplyr
#' @export
#' @examples

print_model_param <- function(model, parameter, latex = TRUE) {
  UseMethod("print_model_param")
}

#' @export
#' @examples
#' mod1 <- lm(mpg ~ wt, data = mtcars)
#' print_model_param(mod1, "wt")
#' print_model_param(mod1, "wt", latex = FALSE)

print_model_param.default <- function(model, parameter, latex = TRUE){

  use_latex <- determine_latex()

  # Tidy model to facilitate printing
  mod <- suppressWarnings(broom::tidy(model, conf.int = TRUE)) %>%
    mutate_each(., give_n_digits, -term, -p.value)

  # Filter row with parameter of interest
  mod_out <- mod[mod$term == parameter, ]

  line <- print_builder(mod_out, latex = use_latex)

  return(line)
}


#' @export
#' @examples
#' library(lme4)
#' library(dplyr)
#' mod1a <- lmer(Reaction ~ 1 + Days + (1|Subject), data = sleepstudy)
#' print_model_param(mod1a, "Days")

print_model_param.lmerMod <- function(model, parameter, latex = TRUE) {

  use_latex <- determine_latex()

  # Tidy model to facilitate printing
  mod <- suppressWarnings(broom::tidy(model, conf.int = TRUE)) %>%
    mutate_each(., give_n_digits, -term)

  # Filter row with parameter of interest
  mod_out <- mod[mod$term == parameter, ]

  if(!("p.value" %in% colnames(mod_out))) {
    mod_out$p.value <- normal_approximation(as.numeric(mod_out$statistic))
  }

  line <- print_builder(mod_out, latex = use_latex)

  return(line)
}

#' @export
#' @examples
#'

print_model_param.brmsfit <- function(model, parameter, latex = TRUE) {

  use_latex <- determine_latex()

  # Tidy model to facilitate printing
  mod <- suppressWarnings(broom::tidy(model, conf.int = TRUE)) %>%
    mutate_each(., give_n_digits, -term)

  # Filter row with parameter of interest
  mod_out <- mod[mod$term == parameter, ]

  line <- print_builder.brmsfit(mod_out, latex = use_latex)

  return(line)
}


# Helper for determining how to set `latex` argument
determine_latex <- function() {
  doc_type <- knitr::opts_knit$get('rmarkdown.pandoc.to')
  if (is.null(doc_type)) {
    use_latex <- latex
  } else {
    use_latex <- ifelse(doc_type == "latex", yes = TRUE, no = FALSE)
  }
}
