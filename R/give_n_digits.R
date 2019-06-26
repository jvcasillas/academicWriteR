#' Give N digits
#'
#' A helper function that always prints a specified number of decimals
#'
#' @param x a numeric vector
#' @param n the number of decimal places to print
#' @keywords printing helpers
#' @export
#' @examples
#' give_n_digits(0.0293526, n = 2)

give_n_digits <- function(x, n = 2) {
  formatC(x, digits = n, format = "f")
}