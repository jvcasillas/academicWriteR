#' Calculate p-values using the normal approximation
#'
#' This function calculates a p-value from a t-value
#'
#' @param statistic The estimated statistic for a model parameter
#' @keywords Report model
#' @export
#' @examples
#' library(lme4)
#' mod3a <- lmer(Reaction ~ 1 + Days + (1|Subject), data = sleepstudy)
#' normal_approximation(2.02)

normal_approximation <- function(statistic) {
  2 * (1 - stats::pnorm(abs(statistic)))
}

