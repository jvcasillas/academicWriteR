#' A new project function
#'
#' This function will create a new project directory for you
#' with subdirectories, and .Rmd and .R templates.
#'
#' @param name A name for your project
#' @param type A valid .Rmd output (html, pdf, word)
#' @keywords projects
#' @export
#' @examples
#' # Create a new project with html report
#' # create_project(name = 'my_project', type = 'pdf')

create_project <- function(name = 'my_project', type = 'pdf') {

    # Get path to working directory
    current_path <- getwd()

    # Get list of files in wd
    dir_files <- list.files('.')

    # If project folder already exists STOP
    # Otherwise create directories
    if (name %in% dir_files == TRUE) {

        # The directory you want to create already exists in wd
        stop("\nThis directory already exists. Choose another name or delete the poser directory.")
    } else {

        # Store project directory name
        dir_name <- paste('/', name, sep = '', collapse = '')

        # Store path to project directory
        home_dir <- paste(current_path, dir_name, sep = '', collape = '')

        # Create project directory
        dir.create(path = home_dir)

        # Print to console so you know it worked
        cat(paste(name, 'created.\n', sep = " "))

        # Set paths for secondary directories and store in vector
        scripts_dir    <- paste(home_dir, 'scripts', sep = '/', collapse = '')
        manuscript_dir <- paste(home_dir, 'manuscript', sep = '/', collapse = '')
        data_dir       <- paste(home_dir, 'data', sep = '/', collapse = '')
        slides_dir     <- paste(home_dir, 'slides', sep = '/', collapse = '')
        docs_dir       <- paste(home_dir, 'docs', sep = '/', collapse = '')
        all_dirs       <- c(scripts_dir, manuscript_dir, data_dir, slides_dir, docs_dir)

        # Create secondary directories
        for (i in all_dirs) {
            dir.create(path = i)
        }

        # Print secondary structure success
        cat("Secondary structure added.\n")

        # Store .Rmd file path for manuscript
        manuscript <- paste(manuscript_dir, '/', name, '.Rmd', sep = '', collapse = '')

        # Create .Rmd file and set basic template
        file.create(manuscript)
        writeLines(c("---",
                     paste("title: '", name, "'", sep = ""),
                     "subtitle: \'\'",
                     "author: \'\'",
                     "date: \'Last update: `r Sys.time()`\'",
                     paste("output: ", type, "_document", sep = ''),
                     "---",
                     "",
                     "```{r echo=FALSE}",
                     "library(knitr)",
                     paste("read_chunk('../scripts/", name, ".R')", sep = ''),
                     "```",
                     "",
                     "```{r, 'load', echo=FALSE}", "```"), manuscript)

        # Store .R script path
        script <- paste(scripts_dir, '/', name, '.R', sep = '', collapse = '')

        # Create .R script and set basic template
        file.create(script)
        writeLines(c("## @knitr load",
                     "",
                     "# Load library",
                     "library(lingStuff)",
                     "",
                     "biVarPlot(cars, dist, speed)"), script)

        # Store .Rmd file path for slides
        slides <- paste(slides_dir, '/', name, '.Rmd', sep = '', collapse = '')

        # Create .Rmd file for slides and set basic template
        file.create(slides)
        writeLines(c("---",
                     paste("title: '", name, "'", sep = ""),
                     "subtitle: \'\'",
                     "author: \'\'",
                     "date: \'Rutgers University </br> `r Sys.Date()`\'",
                     "output: ",
                     "  xaringan::moon_reader:",
                     "    lib_dir: libs",
                     "    css: [\"hygge\", \"rutgers\", \"rutgers-fonts\"]",
                     "    nature:",
                     "      beforeInit: [\"https://www.jvcasillas.com/ru_xaringan/js/ru_xaringan.js\"]",
                     "      highlightStyle: github",
                     "      highlightLines: true",
                     "      countIncrementalSlides: false",
                     "      ratio: \"16:9\"",
                     "---",
                     "",
                     "# Slide",
                     "",
                     "```{r echo=FALSE}",
                     "library(knitr)",
                     paste("read_chunk('../scripts/", name, ".R')", sep = ''),
                     "```",
                     "",
                     "```{r, 'load', echo=FALSE, fig.retina=2}", "```"), slides)

    }
    cat('Finished. :) \n')
}
