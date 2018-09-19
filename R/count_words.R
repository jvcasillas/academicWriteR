#' A function for counting words in an Rmarkdown document.
#'
#' This function takes a path to a .Rmd file and returns a word count. For best
#' results each sentence should start on a new line. Inline code chunks should
#' also start on a new line.
#'
#' @param file A path to a .Rmd file
#' @keywords Word count
#' @import dplyr
#' @importFrom rlang .data
#' @export
#' @examples
#' # word_count("test.Rmd")

# Get word count
count_words <- function(file) {
  wc <- readr::read_lines(file) %>%
    tibble::as_data_frame(.) %>%
    remove_front_matter(.) %>%
    remove_code_chunks(.) %>%
    remove_inline_code(.) %>%
    tidytext::unnest_tokens(., output = .data$words, input = .data$value) %>%
    nrow()
  return(wc)
}

# Helper function for removing unwanted lines from count
# Checks is value is odd.
is_odd <- function(x, val) {
  x %% 2 == 1
}

# Helper function to remove front matter (lines starting with "---" and
# anything between)
remove_front_matter <- function(x) {
  mutate(x, is_code = cumsum(grepl("^---", .data$value))) %>%
    group_by(., .data$is_code) %>%
    mutate(., start_end = lag(.data$is_code, 1)) %>%
    ungroup(.) %>%
    filter(., !is_odd(.data$is_code), !is.na(.data$start_end)) %>%
    select(-.data$is_code, -.data$start_end)
}

# Helper function for removing knitr code chunks (lines starting with "```"
# and anything in between)
remove_code_chunks <- function(x) {
  mutate(x, is_code = cumsum(grepl("^```", .data$value))) %>%
    group_by(., .data$is_code) %>%
    mutate(., start_end = lag(.data$is_code, 1)) %>%
    ungroup(.) %>%
    mutate(., is_odd = is_odd(.data$is_code)) %>%
    filter(., .data$is_odd != TRUE, !is.na(.data$start_end)) %>%
    select(-.data$is_code, -.data$start_end, -.data$is_odd)
}

# Helper function to remove inline code (lines starting with "`r")
remove_inline_code <- function(x) {
  filter(x, !grepl("`r", .data$value))
}

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
