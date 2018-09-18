#' An function to generate unordered lists from the directory structure
#'
#' This function will create an unordered list of all the files in a directory
#' and print the output. It should be used in conjunction with RMarkdown and
#' knitr. Specifically, you want to call it within an R chunk that has the
#' results set to 'asis'.
#' @param path The path to the directory of which you want to print the
#' structure
#' @param remove A regular expression to subset the output
#' @param nested (logical) If false, creates unordered list of current
#' directory only. Otherwise, subdirectories within current directory
#' (1 level) are also included in the list.
#' @keywords unordered list
#' @export
#' @examples
#' # In any directory
#' print_dir_ul()
#'
#' # To specify the path
#' print_dir_ul(path = "./")
#'
#' # All files that are not .txt
#' print_dir_ul(remove = ".txt")



print_dir_ul <- function(path = ".", remove = NULL, nested = FALSE) {

  if (nested == FALSE) {
  files <- list.files(path = path)
  cat(paste("-", files, '\n'))
  } else {

    # Get vector of everything in specified directory
    # If no path is specified it will default to the current
    # working directory.
    directories   <- list.files(path = path, full.names = T)

    if (is.null(remove)) {
      # Reset name
      dirs <- directories
    } else {
      # Remove files, keep dirs
      dirs <- directories[!grepl(remove, directories)]
    }

    # Get vector of everything w/o paths
    names_no_path <- list.files(path = path, full.names = F)

    if (is.null(remove)) {
      # Reset name
      just_names <- names_no_path
    } else {
      # Remove files, keep dirs
      just_names <- names_no_path[!grepl(remove, names_no_path)]
    }

    # Initialize list
    files <- list()

    # Loop through dirs and get contents, store in 'files'
    for (directory in 1:length(dirs)) {
      files[[directory]] <- list.files(path = dirs[directory])
    }

    # Name the columns of 'files'
    names(files) <- just_names

    # Print unordered list in markdown
    for (name in names(files)) {
      cat("-", name, '\n', paste('   -', files[[name]], '\n'))
    }
  }
}
