#' Model to table
#'
#' This function takes a model object and generates a table in pdf or word
#' format.
#'
#' @param model A model object
#' @param param_names For renaming the default parameter names
#' @param font_size The font size for the table
#' @param left_align Columns that should be left aligned
#' @param width The width in inches of the columns
#' @param col The number of a specific column for width adjustments
#' @param ci Range of the HDCI for brms models
#' @param rope Range of the ROPE interval for brms models
#' @keywords Report model
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import lme4
#' @importFrom rlang .data
#' @importFrom methods is
#' @export
#' @examples
#' mod <- lm(mpg ~ drat, data = mtcars)
#' mod_to_table(mod)

mod_to_table <- function(
  model, param_names = NULL, font_size = 11, left_align = 1, width = NULL,
  col = NULL, ci = 0.95, rope = c(-0.1, 0.1)) {
  UseMethod(generic = "mod_to_table", object = model)
}


#' @export
# Default for lm/glm
mod_to_table.default <- function(
  model, param_names = NULL, font_size = 11, left_align = 1, width = NULL,
  col = NULL, ci = 0.95, rope = c(-0.1, 0.1)) {

  doc_type <- set_doc_type()

  # Get tidy model
  mod <- suppressWarnings(broom::tidy(model, conf.int = TRUE))

  # Create vector of number of columns for alignment
  n_cols <- 1:length(colnames(mod))

  # Table adjustments
  add_p(mod)
  mod$term <- parameter_adj(mod = mod, param_names = param_names)
  set_col_widths <- column_widths(width = width, col = col)
  rename_cols <- set_col_names(model = model, doc_type = doc_type)

  if (doc_type == "docx") {

  standard_flex <- set_flex(model)

  mod %>%
    mutate(across(-c("term", "p.value"), give_n_digits)) %>%
    format_p %>%
    rename_cols %>%
    standard_flex %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::align(align = "left", j = left_align, part = "all") %>%
    flextable::align(align = "right", j = n_cols[-left_align], part = "all") %>%
    set_col_widths
  } else {
    mod %>%
      mutate(across(-c("term", "p.value"), give_n_digits)) %>%
      format_p %>%
      rename_cols %>%
      knitr::kable(format = "latex", booktabs = T, escape = F,
           align = c("@{}l", rep("r", length(n_cols) -2), "r@{}")
           ) %>%
      kableExtra::kable_styling(font_size = font_size,
                                latex_options = "hold_position")
  }

}


#' @export
# Default for lmer
mod_to_table.lmerMod <- function(
  model, param_names = NULL, font_size = 11, left_align = 1, width = NULL,
  col = NULL, ci = 0.95, rope = c(-0.1, 0.1)) {

  doc_type <- set_doc_type()

  # Get tidy model
  mod <- suppressWarnings(broom::tidy(model, conf.int = TRUE)) %>%
    filter(.data$group == "fixed") %>%
    select(-.data$group)

  # Create vector of number of columns for alignment
  n_cols <- 1:length(colnames(mod))

  # Table adjustments
  add_p(mod)
  mod$term <- parameter_adj(mod = mod, param_names = param_names)
  set_col_widths <- column_widths(width = width, col = col)
  rename_cols <- set_col_names(model = model, doc_type = doc_type)

  if (doc_type == "docx") {

  standard_flex <- set_flex(model)

  # Print table
  mod %>%
  mutate(across(-c("term", "p.value"), give_n_digits)) %>%
    format_p %>%
    rename_cols %>%
    standard_flex %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::align(align = "left", j = left_align, part = "all") %>%
    flextable::align(align = "right", j = n_cols[-left_align], part = "all") %>%
    set_col_widths
  } else {
    mod %>%
      mutate(across(-c("term", "p.value"), give_n_digits)) %>%
      format_p %>%
      rename_cols %>%
      knitr::kable(format = "latex", booktabs = T, escape = F,
           #caption = 'Posterior parameter estimates for voiced stops.',
           align = c("@{}l", rep("r", length(n_cols) -2), "r@{}")
           ) %>%
      kableExtra::kable_styling(font_size = font_size,
                                latex_options = "hold_position")

  }
}


#' @export
# Default for lmer if lmerTest is loaded
mod_to_table.lmerModLmerTest <- function(
  model, param_names = NULL, font_size = 11, left_align = 1, width = NULL,
  col = NULL, ci = 0.95, rope = c(-0.1, 0.1)) {

  doc_type <- set_doc_type()

  # Get tidy model
  mod <- suppressWarnings(broom::tidy(model, conf.int = TRUE)) %>%
    filter(.data$effect == "fixed") %>%
    select(-.data$effect, -.data$group)

  # Create vector of number of columns for alignment
  n_cols <- 1:length(colnames(mod))

  # Table adjustments
  mod$term <- parameter_adj(mod = mod, param_names = param_names)
  set_col_widths <- column_widths(width = width, col = col)
  rename_cols <- set_col_names(model = model, doc_type = doc_type)

  if (doc_type == "docx") {

  standard_flex <- set_flex(model)

  mod %>%
  mutate(across(-c("term", "p.value"), give_n_digits)) %>%
    format_p %>%
    rename_cols %>%
    standard_flex %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::align(align = "left", j = left_align, part = "all") %>%
    flextable::align(align = "right", j = n_cols[-left_align], part = "all") %>%
    set_col_widths
  } else {
    mod %>%
      mutate(across(-c("term", "p.value"), give_n_digits)) %>%
      format_p %>%
      rename_cols %>%
      knitr::kable(format = "latex", booktabs = T, escape = F,
           #caption = 'Posterior parameter estimates for voiced stops.',
           align = c("@{}l", rep("r", length(n_cols) -2), "r@{}")
           ) %>%
      kableExtra::kable_styling(font_size = font_size,
                                latex_options = "hold_position")

  }
}


#' @export
# Default for glmer
mod_to_table.glmerMod <- function(
  model, param_names = NULL, font_size = 11, left_align = 1, width = NULL,
  col = NULL, ci = 0.95, rope = c(-0.1, 0.1)) {

  doc_type <- set_doc_type()

  # Get tidy model
  mod <- suppressWarnings(broom::tidy(model, conf.int = TRUE)) %>%
    filter(.data$effect == "fixed") %>%
    select(-.data$effect, -.data$group)

  # Create vector of number of columns for alignment
  n_cols <- 1:length(colnames(mod))

  # Table adjustments
  mod$term <- parameter_adj(mod = mod, param_names = param_names)
  set_col_widths <- column_widths(width = width, col = col)
  rename_cols <- set_col_names(model = model, doc_type = doc_type)

  if (doc_type == "docx") {

  standard_flex <- set_flex(model)

  # Print table
  mod %>%
  mutate(across(-c("term", "p.value"), give_n_digits)) %>%
    format_p %>%
    rename_cols %>%
    standard_flex %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::align(align = "left", j = left_align, part = "all") %>%
    flextable::align(align = "right", j = n_cols[-left_align], part = "all") %>%
    set_col_widths
  } else {
    mod %>%
      mutate(across(-c("term", "p.value"), give_n_digits)) %>%
      format_p %>%
      rename_cols %>%
      knitr::kable(format = "latex", booktabs = T, escape = F,
           #caption = 'Posterior parameter estimates for voiced stops.',
           align = c("@{}l", rep("r", length(n_cols) -2), "r@{}")
           ) %>%
      kableExtra::kable_styling(font_size = font_size,
                                latex_options = "hold_position")

  }
}


#' @export
# Default for brms
mod_to_table.brmsfit <- function(
  model, param_names = NULL, font_size = 11, left_align = 1, width = NULL,
  col = NULL, ci = 0.95, rope = c(-0.1, 0.1)) {

  doc_type <- set_doc_type()

  # Get tidy model
  mod <- bayestestR::describe_posterior(model, ci = ci, rope_ci = ci,
                                        rope_range = rope) %>%
    as_tibble() %>%
    mutate(across(.data$Parameter, give_n_digits)) %>%
    mutate(HDI = paste0("[", give_n_digits(.data$CI_low, 2), ", ",
           give_n_digits(.data$CI_high, 2), "]")) %>%
    select(term = .data$Parameter, estimate = .data$Median, .data$HDI,
           ROPE = .data$ROPE_Percentage, MPE = .data$pd)

  # Create vector of number of columns for alignment
  n_cols <- 1:length(colnames(mod))

  # Table adjustments
  mod$term <- parameter_adj(mod = mod, param_names = param_names)
  set_col_widths <- column_widths(width = width, col = col)
  rename_cols <- set_col_names(model = model, doc_type = doc_type)

  if (doc_type == "docx") {

    standard_flex <- set_flex(model)

    # Print table
    mod %>%
    mutate(across(-.data$term, give_n_digits)) %>%
      rename_cols %>%
      standard_flex %>%
      flextable::fontsize(size = font_size, part = "all") %>%
      flextable::align(align = "left", j = left_align, part = "all") %>%
      flextable::align(align = "right", j = n_cols[-left_align], part = "all") %>%
      set_col_widths
  } else {
    mod %>%
      mutate(across(-.data$term, give_n_digits)) %>%
      rename_cols %>%
      knitr::kable(format = "latex", booktabs = T, escape = T,
           #caption = 'Posterior parameter estimates for voiced stops.',
           align = c("@{}l", rep("r", length(n_cols) -2), "r@{}")
           ) %>%
      kableExtra::kable_styling(font_size = font_size,
                                latex_options = "hold_position")

  }
}






# Helpers ---------------------------------------------------------------------
#
# These are helper functions used in the mod_to_table function(s)
# The helpers are not exported to academicWriteR


# Add standard column names
#
# This function standardizes the column names that come out of broom::tidy or
# bayestestR::describe_posterior
#
set_col_names <- function(model, doc_type = doc_type) {
  if (doc_type == "docx") {
    if (class(model)[1] %in% "lmerModLmerTest") {
      rename_cols <- . %>%
        select(Parameter = .data$term, Estimate = .data$estimate,
               SE = .data$std.error, `CI low` = .data$conf.low,
               `CI high` = .data$conf.high, t = statistic, .data$df,
               p = .data$p.value)
    } else if (is(model, "brmsfit")) {
      rename_cols <- . %>%
        select(Parameter = .data$term, Estimate = .data$estimate, .data$HDI,
               .data$ROPE, .data$MPE)
    } else {
      rename_cols <- . %>%
        select(Parameter = .data$term, Estimate = .data$estimate,
               SE = .data$std.error, `CI low` = .data$conf.low,
               `CI high` = .data$conf.high, t = statistic, p = .data$p.value)
    }
    return(rename_cols)
  } else {
    if (class(model)[1] %in% "lmerModLmerTest") {
      rename_cols <- . %>%
        select(Parameter = .data$term, Estimate = .data$estimate,
               SE = .data$std.error, `CI low` = .data$conf.low,
               `CI high` = .data$conf.high, `\\emph{t}` = statistic, .data$df,
               `\\emph{p}` = .data$p.value)
    } else if (is(model, "brmsfit")) {
      rename_cols <- . %>%
        select(Parameter = .data$term, Estimate = .data$estimate, .data$HDI,
               .data$ROPE, .data$MPE)
    } else {
      rename_cols <- . %>%
        select(Parameter = .data$term, Estimate = .data$estimate,
               SE = .data$std.error, `CI low` = .data$conf.low,
               `CI high` = .data$conf.high, `\\emph{t}` = statistic,
               `\\emph{p}` = .data$p.value)
    }
  }
  return(rename_cols)
}


# Adjust parameter names
#
# Customize parameter names if necessary
#
parameter_adj <- function(mod, param_names = NULL) {

  if (!is.null(param_names)) {
    mod$term <- forcats::fct_recode(mod$term, !!!param_names)
  return(mod$term)
  } else {
  return(mod$term)
  }

}


# Standard flextable format
#
# Table template for when knitting to word
#
set_flex <- function(model) {
  if (is(model, "brmsfit")) {
    standard_flex <- . %>%
    flextable::flextable(.) %>%
    flextable::font(fontname = "Times", part = "all") %>%
    flextable::border_remove() %>%
    flextable::border(part = "header",
              border.top = officer::fp_border(width = 0.95),
              border.bottom = officer::fp_border(width = 0.75)) %>%
    flextable::hline_bottom(part = "body",
                            border = officer::fp_border(width = 0.95)) %>%
    flextable::padding(padding = 0, part = "all")
  } else {
    standard_flex <- . %>%
    flextable::flextable(.) %>%
    flextable::font(fontname = "Times", part = "all") %>%
    flextable::border_remove() %>%
    flextable::border(part = "header",
              border.top = officer::fp_border(width = 0.95),
              border.bottom = officer::fp_border(width = 0.75)) %>%
    flextable::hline_bottom(part = "body",
                            border = officer::fp_border(width = 0.95)) %>%
    flextable::padding(padding = 0, part = "all") %>%
    flextable::italic(italic = TRUE, part = "header", j = c("t", "p"))
  }
}


# Add pvalue if missing
#
# Approximate a pvalue for lmer objects when lmerTest is not loaded
#
add_p <- function(mod) {
  if(!("p.value" %in% colnames(mod))) {
    mod$p.value <- normal_approximation(as.numeric(mod$statistic))
  }
}


# Format the pvalues and remove fluff
#
format_p <- . %>%
  mutate(p.value = print_pval(p.value, latex = F)) %>%
  mutate(p.value = gsub("*p* = ", "", fixed = T, paste(.$p.value))) %>%
  mutate(p.value = gsub("*p* ", "", fixed = T, paste(.$p.value)))


# Determine columns widths
#
# Custom column widths for tables in word
#
column_widths <- function(width = NULL, col = NULL) {
  # Set col widths (default to 1)
  if (is.null(width)) {
    set_col_widths <- . %>% flextable::width(width = 1)
    } else if (is.numeric(width & is.null(col))) {
    set_col_widths <- . %>% flextable::width(width = width)
    } else {
    set_col_widths <- . %>% flextable::width(j = col, width = width)
  }
}

# Get document type/set default
#
# Use knitr::opts_knit$get to determine what the document is being knit as
# and set accordingly so LaTeX or word versions are selected automatically
#
set_doc_type <- function() {
  this_doc <- knitr::opts_knit$get('rmarkdown.pandoc.to')
  if (is.null(this_doc)) {
    this_doc <- "latex"
  }
  return(this_doc)
}

utils::globalVariables(".")
