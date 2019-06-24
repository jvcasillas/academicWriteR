#' Print model parameters
#'
#' This function takes a model object and prints a row. This is used for
#' reporting results.
#'
#' @param model A model object
#' @param parameter A model parameter
#' @param latex Determine if output is LaTeX or markdown
#' @keywords Report model
#' @export
#' @examples

print_model_param <- function(model, parameter, latex = TRUE) {
  UseMethod("print_model_param")
}

#' @export
#' @examples
#' library(dplyr)
#' mod1 <- lm(mpg ~ wt, data = mtcars)
#' print_model_param(mod1, "wt")
#' print_model_param(mod1, "wt", latex = FALSE)

print_model_param.default <- function(model, parameter, latex = TRUE){

  # Tidy model to facilitate printing
  mod <- suppressWarnings(broom::tidy(model, conf.int = TRUE)) %>%
    mutate_if(., is.numeric, round, digits = 2)

  # Filter row with parameter of interest
  mod_out <- mod[mod$term == parameter, ]

  line <- print_builder(mod_out, latex = latex)

  return(line)
}


#' @export
#' @examples
#' library(lme4)
#' library(dplyr)
#' mod1a <- lmer(Reaction ~ 1 + Days + (1|Subject), data = sleepstudy)
#' print_model_param(mod1a, "Days")

print_model_param.lmerMod <- function(model, parameter, latex = TRUE) {

  # Tidy model to facilitate printing
  mod <- suppressWarnings(broom::tidy(model, conf.int = TRUE)) %>%
    mutate_if(., is.numeric, round, digits = 2)

  # Filter row with parameter of interest
  mod_out <- mod[mod$term == parameter, ]

  if(!("p.value" %in% colnames(mod_out))) {
    mod_out$p.value <- normal_approximation(mod_out$statistic)
  }

  line <- print_builder(mod_out, latex = latex)

  return(line)
}

#' @export
#' @examples
#'

print_model_param.brmsfit <- function(model, parameter, latex = TRUE) {

  # Tidy model to facilitate printing
  mod <- suppressWarnings(broom::tidy(model, conf.int = TRUE)) %>%
    mutate_if(., is.numeric, round, digits = 2)

  # Filter row with parameter of interest
  mod_out <- mod[mod$term == parameter, ]

  line <- print_builder.brmsfit(mod_out, latex = latex)

  return(line)
}

