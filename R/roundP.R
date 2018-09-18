#' A rounding function for pvalues
#'
#' This function takes a numeric value and rounds it. It is most useful for
#' R output that is more than three decimal places long.
#'
#' @param x A p-value to round
#' @keywords Round p-values
#' @export
#' @examples
#' roundP(0.03765)
#' roundP(0.0000001)

roundP <- function(x) {

  # Import pvalue to 3 decimal places (returns char)
  valIn <- sprintf("%.3f", x)

  # Get last char
  lastChar <- substr(valIn, start = 5, stop = 5)

  # If/then chain to determine rounded pvalue
  valOut <- ifelse(lastChar != "0", yes = as.numeric(valIn) + 0.001, no =
                   ifelse(as.numeric(valIn) == 0, yes = 0.001,
                          no = as.numeric(valIn) + 0.01))

  return(valOut)
}
