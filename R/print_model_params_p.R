#' Helper function for reporting model parameters in parenthesis
#'
#' This function takes a model object  and a predictor, and prints the output
#' in parenthesis. Includes confidence intervals, stat type and p-values.
#'
#' @param model A model object
#' @param predictor (character) The effect (fixed/random) you want to report
#' @param latex (Logical) If true, output is appropriate for LaTeX. Otherwise
#' formatting is for markdown.
#' @keywords Report model parameter
#' @export
#' @examples
#' library(lme4)
#' library(lmerTest)
#' library(broom.mixed)
#' mod1 <- lm(mpg ~ wt, data = mtcars)
#' mod2 <- lmer(Reaction ~ 1 + Days + (1|Subject), data = sleepstudy,
#'              REML = FALSE)
#' print_model_params_p(model = mod1, predictor = "wt", latex = FALSE)
#' print_model_params_p(model = mod2, predictor = "Days")


# Helper function for reporting model parameters in parenthesis
print_model_params_p <- function(model, predictor, latex = TRUE) {

  # Get tidy model object
  tidy_model <- suppressWarnings(broom::tidy(model, conf.int = TRUE))

  # Extract values from model output
  ciLo <- round(tidy_model[tidy_model$term == predictor, 'conf.low'], 2)
  ciHi <- round(tidy_model[tidy_model$term == predictor, 'conf.high'], 2)
  stat <- round(tidy_model[tidy_model$term == predictor, 'statistic'], 2)

  # Determine test type and get pvalue
  if ("t value" %in% colnames(summary(model)$coef)) {
    pval <- as.data.frame(summary(model)$coef)[predictor, ]$`Pr(>|t|)`
    stat_type <- "t"
  } else {
    pval <- as.data.frame(summary(model)$coef)[predictor, ]$`Pr(>|z|)`
    stat_type <- "z"
  }

  # Determine output format
  if (latex == TRUE) {
    stat_start <- "; \\emph{"
    stat_end   <- "} = "
  } else {
    stat_start <- "; *"
    stat_end   <- "* = "
  }

  paste(
    capture.output(
      cat(
        paste0("(CI = [",
                ciLo,
               ", ",
                ciHi,
               "]",
                stat_start,
                stat_type,
                stat_end,
                stat,
               "; ",
                print_pval(pval, latex = latex),
               ")",
               "\n")
        )
      )
    )

}