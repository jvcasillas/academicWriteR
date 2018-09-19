#' Helper function for reporting nested model comparisons
#'
#' This function takes a nested model comparison ANOVA object and prints the
#' result in LaTeX or markdown. This currenlty only works for linear mixed
#' effects model comparisons.
#'
#' @param table An ANOVA table
#' @param model The model you want to report
#' @param latex (Logical) If true, output is appropriate for LaTeX. Otherwise
#' formatting is for markdown.
#' @keywords Report nested model comparisons
#' @import utils
#' @export
#' @examples
#' library(lme4)
#' mod1 <- lmer(Reaction ~ 1 + (1|Subject), data = sleepstudy, REML = FALSE)
#' mod2 <- lmer(Reaction ~ 1 + Days + (1|Subject), data = sleepstudy,
#'              REML = FALSE)
#' aov_table <- anova(mod1, mod2, test = "Chisq")
#' print_nmc_p(table = aov_table, model = 'mod2')
#' print_nmc_p(table = aov_table, model = 'mod2', latex = FALSE)

print_nmc_p <- function(table, model, latex = TRUE) {

  tidy_table <- suppressWarnings(broom::tidy(table))

  # Collect appropriate values from model table
  stat   <- tidy_table[tidy_table$term == model, 'statistic']
  df     <- tidy_table[tidy_table$term == model, 'Chi.Df']
  praw   <- tidy_table[tidy_table$term == model, 'p.value']

  if (latex == TRUE) {
    method <- "$\\chi^2$"
    pval <- print_pval(praw, latex = TRUE)
  } else {
    method <- "\U03C7^2"
    pval <- print_pval(praw, latex = FALSE)
  }

  # Print bare output to console so it can be used with latex
  paste(
    capture.output(
      cat(
        paste0("(",
               method,
               "(",
               df,
               ") = ",
               round(stat, 2),
               "; ",
               pval,
               ")",
               "\n")
        )
      )
    )
}

