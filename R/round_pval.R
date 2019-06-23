#' A rounding function for pvalues
#'
#' This function takes a numeric value and rounds it. It is most useful for
#' R output that is more than three decimal places long.
#'
#' @param x (numeric) A p-value to round
#' @keywords Round p-values
#' @export
#' @examples
#' round_pval(0.03765)
#' round_pval(0.0000001)


# Must delete trailing 0
# must print exact value unless it is
# less than .001


round_pval <- function(x) {

  # Import pvalue to 3 decimal places (returns char)
  val_in <- sprintf("%.3f", x)

  # If values is less than 0.001, then set to .001,
  # Otherwise print as is w/o leading 0
  val_out <- ifelse(
    val_in < 0.001,
    yes = ".001",
    no = substr(as.character(as.numeric(val_in)), start = 2, stop = 5))

  return(val_out)
}
