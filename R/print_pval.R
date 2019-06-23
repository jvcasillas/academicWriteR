#' A helper function to round p-values and paste less than or greater than
#' symbol
#'
#' This function takes a numeric value and rounds it, then it formats it in
#' order to be reported in an academic report written in LaTeX or markdown.
#' It is most useful for R output that is more than three decimal places long.
#'
#' @param x (numeric) A p-value to round
#' @param latex (Logical) If true, output is appropriate for LaTeX. Otherwise
#' formatting is for markdown.
#' @keywords Round p-values
#' @export
#' @examples
#' print_pval(0.03765)
#' print_pval(0.0000001, latex = FALSE)


print_pval <- function(x, latex = TRUE) {

  # Round pvalue
  val_out <- round_pval(x)
  val_num <- as.numeric(val_out)

  # For LaTeX and Markdown
  if (latex == TRUE) {

    # If/then to determine LaTeX output w/ symbol
    result <- ifelse(val_num <= 0.001,
                     yes = paste0("\\emph{p} < ", val_out),
                     no = paste0("\\emph{p} = ", val_out))

  } else {

    # If/then to determine markdown output w/ symbol
    result <- ifelse(val_num <= 0.001,
                     yes = paste0("*p* < ", val_out),
                     no = paste0("*p* = ", val_out))

  }

  return(result)

}
