#' Helper function for reporting individual model parameters
#'
#' This function takes a model object, a predictor, and a parameter and prints
#' the numeric output.
#'
#' @param model A model object
#' @param predictor (character) The effect (fixed/random) you want to report
#' @param parameter (character) The parameter you want to report ('estimate',
#' 'std.error', 'statistic')
#' @keywords Report parameter
#' @export
#' @examples
#' library(lme4)
#' mod1 <- lm(mpg ~ wt, data = mtcars)
#' mod2 <- lmer(Reaction ~ 1 + Days + (1|Subject), data = sleepstudy,
#'              REML = FALSE)
#' print_model_params(model = mod1, predictor = "wt", parameter = "estimate")
#' print_model_params(model = mod2, predictor = "Days", parameter = "estimate")



print_model_params <- function(model, predictor, parameter) {

  tidy_model <- suppressWarnings(broom.mixed::tidy(model))

  # Extract wanted value from model output
  val_in <- round(tidy_model[tidy_model$term == predictor, parameter], 2)

  paste(
    capture.output(
      cat(
        paste0(val_in,
               "\n")
        )
      )
    )

}
