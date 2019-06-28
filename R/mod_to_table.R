#' Model to table
#'
#' This function takes a model object generates a table
#'
#' @param model A model object
#' @param param_names For renaming the default parameter names
#' @param font_size The font size for the table
#' @param left_align Columns that should be left aligned
#' @param width The width in inches of the columns
#' @param col The number of a specific column for width adjustments
#' @keywords Report model
#' @export
#' @examples
#' library(dplyr)
#' mod1 <- lm(mpg ~ drat, data = mtcars)
#' mod_to_table(mod1)

mod_to_table <- function(
  model, param_names = NULL, font_size = 11, left_align = 1, width = NULL,
  col = NULL) {
  UseMethod("mod_to_table")
}


# Adjust parameter names
parameter_adj <- function(mod, param_names = NULL) {

  if (!is.null(param_names)) {
    mod$term <- fct_recode(mod$term, !!!param_names)
  return(mod$term)
  } else {
  return(mod$term)
  }

}



#' @export
# Default for lm/glm
mod_to_table.default <- function(
  model, param_names = NULL, font_size = 11, left_align = 1, width = NULL,
  col = NULL) {

  # Get tidy model
  mod <- suppressWarnings(broom::tidy(model, conf.int = TRUE))

  # Create vector of number of columns for alignment
  n_cols <- 1:length(colnames(mod))

  if(!("p.value" %in% colnames(mod))) {
    mod$p.value <- normal_approximation(as.numeric(mod$statistic))
  }

  # Adjust terms
  mod$term <- parameter_adj(mod = mod, param_names = param_names)

  # Set col widths (default to 1)
  if (is.null(width)) {
    set_col_widths <- . %>% flextable::width(., width = 1)
    } else if (is.numeric(width & is.null(col))) {
    set_col_widths <- . %>% flextable::width(., width = width)
    } else {
    set_col_widths <- . %>% flextable::width(., j = col, width = width)
  }

  # Add standard column names
  rename_cols <- . %>%
    select(
      Parameter = term,
      Estimate = estimate,
      SE = std.error,
      `CI low` = conf.low,
      `CI high` = conf.high,
      t = statistic,
      p = p.value)

  # Format the pvalues and remove fluff
  format_p <- . %>%
    mutate(p.value = print_pval(p.value, latex = F)) %>%
    mutate(p.value = gsub("*p* = ", "", fixed = T, paste(.$p.value))) %>%
    mutate(p.value = gsub("*p* ", "", fixed = T, paste(.$p.value)))

  # Strandard flextable with unchanging params
  standard_flex <- . %>%
    flextable::flextable() %>%
    flextable::font(., fontname = "Times", part = "all") %>%
    flextable::border_remove(.) %>%
    flextable::border(., part = "header",
              border.top = officer::fp_border(width = 0.95),
              border.bottom = officer::fp_border(width = 0.75)) %>%
    flextable::hline_bottom(., part = "body",
                            border = officer::fp_border(width = 0.95)) %>%
    flextable::padding(., padding = 0, part = "all") %>%
    flextable::italic(., italic = TRUE, part = "header", j = c("t", "p"))

  mod %>%
    mutate_each(give_n_digits, -term, -p.value) %>%
    format_p %>%
    rename_cols %>%
    standard_flex %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::align(align = "left", j = left_align, part = "all") %>%
    flextable::align(align = "right", j = n_cols[-left_align], part = "all") %>%
    set_col_widths

}

















#' @export
#' @examples
#' library(lme4)
#' library(dplyr)
#' mod1a <- lmer(Reaction ~ 1 + Days + (1|Subject), data = sleepstudy)
#' mod_to_table(mod1a)

mod_to_table.lmerMod <- function(
  model, param_names = NULL, font_size = 11, left_align = 1, width = NULL,
  col = NULL) {

  # Get tidy model
  mod <- suppressWarnings(broom::tidy(model, conf.int = TRUE)) %>%
    filter(group == "fixed") %>%
    select(-group)

  # Create vector of number of columns for alignment
  n_cols <- 1:length(colnames(mod))

  if(!("p.value" %in% colnames(mod))) {
    mod$p.value <- normal_approximation(as.numeric(mod$statistic))
  }

  # Adjust terms
  mod$term <- parameter_adj(mod = mod, param_names = param_names)

  # Set col widths (default to 1)
  if (is.null(width)) {
    set_col_widths <- . %>% flextable::width(., width = 1)
    } else if (is.numeric(width & is.null(col))) {
    set_col_widths <- . %>% flextable::width(., width = width)
    } else {
    set_col_widths <- . %>% flextable::width(., j = col, width = width)
  }

  # Add standard column names
  rename_cols <- . %>%
    select(
      Parameter = term,
      Estimate = estimate,
      SE = std.error,
      `CI low` = conf.low,
      `CI high` = conf.high,
      t = statistic,
      p = p.value)

  # Format the pvalues and remove fluff
  format_p <- . %>%
    mutate(p.value = print_pval(p.value, latex = F)) %>%
    mutate(p.value = gsub("*p* = ", "", fixed = T, paste(.$p.value))) %>%
    mutate(p.value = gsub("*p* ", "", fixed = T, paste(.$p.value)))

  # Strandard flextable with unchanging params
  standard_flex <- . %>%
    flextable::flextable() %>%
    flextable::font(., fontname = "Times", part = "all") %>%
    flextable::border_remove(.) %>%
    flextable::border(., part = "header",
              border.top = officer::fp_border(width = 0.95),
              border.bottom = officer::fp_border(width = 0.75)) %>%
    flextable::hline_bottom(., part = "body",
                            border = officer::fp_border(width = 0.95)) %>%
    flextable::padding(., padding = 0, part = "all") %>%
    flextable::italic(., italic = TRUE, part = "header", j = c("t", "p"))

  mod %>%
  mutate_each(give_n_digits, -term, -p.value) %>%
    format_p %>%
    rename_cols %>%
    standard_flex %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::align(align = "left", j = left_align, part = "all") %>%
    flextable::align(align = "right", j = n_cols[-left_align], part = "all") %>%
    set_col_widths

}






#' @export
#' @examples

mod_to_table.lmerModLmerTest <- function(
  model, param_names = NULL, font_size = 11, left_align = 1, width = NULL,
  col = NULL) {

  # Get tidy model
  mod <- suppressWarnings(broom::tidy(model, conf.int = TRUE)) %>%
    filter(effect == "fixed") %>%
    select(-effect, -group)

  # Create vector of number of columns for alignment
  n_cols <- 1:length(colnames(mod))

  # Adjust terms
  mod$term <- parameter_adj(mod = mod, param_names = param_names)

  # Set col widths (default to 1)
  if (is.null(width)) {
    set_col_widths <- . %>% flextable::width(., width = 1)
    } else if (is.numeric(width & is.null(col))) {
    set_col_widths <- . %>% flextable::width(., width = width)
    } else {
    set_col_widths <- . %>% flextable::width(., j = col, width = width)
  }

  # Add standard column names
  rename_cols <- . %>%
    select(
      Parameter = term,
      Estimate = estimate,
      SE = std.error,
      `CI low` = conf.low,
      `CI high` = conf.high,
      t = statistic,
      df,
      p = p.value)

  # Format the pvalues and remove fluff
  format_p <- . %>%
    mutate(p.value = print_pval(p.value, latex = F)) %>%
    mutate(p.value = gsub("*p* = ", "", fixed = T, paste(.$p.value))) %>%
    mutate(p.value = gsub("*p* ", "", fixed = T, paste(.$p.value)))

  # Strandard flextable with unchanging params
  standard_flex <- . %>%
    flextable::flextable() %>%
    flextable::font(., fontname = "Times", part = "all") %>%
    flextable::border_remove(.) %>%
    flextable::border(., part = "header",
              border.top = officer::fp_border(width = 0.95),
              border.bottom = officer::fp_border(width = 0.75)) %>%
    flextable::hline_bottom(., part = "body",
                            border = officer::fp_border(width = 0.95)) %>%
    flextable::padding(., padding = 0, part = "all") %>%
    flextable::italic(., italic = TRUE, part = "header", j = c("t", "p"))

  mod %>%
  mutate_each(give_n_digits, -term, -p.value) %>%
    format_p %>%
    rename_cols %>%
    standard_flex %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::align(align = "left", j = left_align, part = "all") %>%
    flextable::align(align = "right", j = n_cols[-left_align], part = "all") %>%
    set_col_widths

}







#' @export
#' @examples
#' library(lme4)
#' library(dplyr)
#' mod4 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
#'               data = cbpp, family = binomial)
#' mod_to_table(mod4)

mod_to_table.glmerMod <- function(
  model, param_names = NULL, font_size = 11, left_align = 1, width = NULL,
  col = NULL) {

  # Get tidy model
  mod <- suppressWarnings(broom::tidy(model, conf.int = TRUE)) %>%
    filter(effect == "fixed") %>%
    select(-effect, -group)

  # Create vector of number of columns for alignment
  n_cols <- 1:length(colnames(mod))

  # Adjust terms
  mod$term <- parameter_adj(mod = mod, param_names = param_names)

  # Set col widths (default to 1)
  if (is.null(width)) {
    set_col_widths <- . %>% flextable::width(., width = 1)
    } else if (is.numeric(width & is.null(col))) {
    set_col_widths <- . %>% flextable::width(., width = width)
    } else {
    set_col_widths <- . %>% flextable::width(., j = col, width = width)
  }

  # Add standard column names
  rename_cols <- . %>%
    select(
      Parameter = term,
      Estimate = estimate,
      SE = std.error,
      `CI low` = conf.low,
      `CI high` = conf.high,
      t = statistic,
      p = p.value)

  # Format the pvalues and remove fluff
  format_p <- . %>%
    mutate(p.value = print_pval(p.value, latex = F)) %>%
    mutate(p.value = gsub("*p* = ", "", fixed = T, paste(.$p.value))) %>%
    mutate(p.value = gsub("*p* ", "", fixed = T, paste(.$p.value)))

  # Strandard flextable with unchanging params
  standard_flex <- . %>%
    flextable::flextable() %>%
    flextable::font(., fontname = "Times", part = "all") %>%
    flextable::border_remove(.) %>%
    flextable::border(., part = "header",
              border.top = officer::fp_border(width = 0.95),
              border.bottom = officer::fp_border(width = 0.75)) %>%
    flextable::hline_bottom(., part = "body",
                            border = officer::fp_border(width = 0.95)) %>%
    flextable::padding(., padding = 0, part = "all") %>%
    flextable::italic(., italic = TRUE, part = "header", j = c("t", "p"))

  mod %>%
  mutate_each(give_n_digits, -term, -p.value) %>%
    format_p %>%
    rename_cols %>%
    standard_flex %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::align(align = "left", j = left_align, part = "all") %>%
    flextable::align(align = "right", j = n_cols[-left_align], part = "all") %>%
    set_col_widths

}












#' @export
#' @examples

mod_to_table.brmsfit <- function(
  model, param_names = NULL, font_size = 11, left_align = 1, width = NULL,
  col = NULL) {

  # Get tidy model
  mod <- suppressWarnings(broom::tidy(model, conf.int = TRUE)) %>%
    filter(effect == "fixed") %>%
    select(-effect, -component, -group)

  # Create vector of number of columns for alignment
  n_cols <- 1:length(colnames(mod))

  # Adjust terms
  mod$term <- parameter_adj(mod = mod, param_names = param_names)

  # Set col widths (default to 1)
  if (is.null(width)) {
    set_col_widths <- . %>% flextable::width(., width = 1)
    } else if (is.numeric(width & is.null(col))) {
    set_col_widths <- . %>% flextable::width(., width = width)
    } else {
    set_col_widths <- . %>% flextable::width(., j = col, width = width)
  }

  # Add standard column names
  rename_cols <- . %>%
    select(
      Parameter = term,
      Estimate = estimate,
      SE = std.error,
      `2.5%` = conf.low,
      `97.5%` = conf.high)

  # Strandard flextable with unchanging params
  standard_flex <- . %>%
    flextable::flextable() %>%
    flextable::font(., fontname = "Times", part = "all") %>%
    flextable::border_remove(.) %>%
    flextable::border(., part = "header",
              border.top = officer::fp_border(width = 0.95),
              border.bottom = officer::fp_border(width = 0.75)) %>%
    flextable::hline_bottom(., part = "body",
                            border = officer::fp_border(width = 0.95)) %>%
    flextable::padding(., padding = 0, part = "all")

  mod %>%
  mutate_each(give_n_digits, -term) %>%
    rename_cols %>%
    standard_flex %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::align(align = "left", j = left_align, part = "all") %>%
    flextable::align(align = "right", j = n_cols[-left_align], part = "all") %>%
    set_col_widths

}



