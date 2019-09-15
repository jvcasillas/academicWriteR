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
#' @import dplyr
#' @export
#' @examples
#' mod <- lm(mpg ~ drat, data = mtcars)
#' mod_to_table(mod)

mod_to_table <- function(
  model, param_names = NULL, font_size = 11, left_align = 1, width = NULL,
  col = NULL) {
  UseMethod("mod_to_table")
}


#' @export
# Default for lm/glm
mod_to_table.default <- function(
  model, param_names = NULL, font_size = 11, left_align = 1, width = NULL,
  col = NULL) {

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
    mutate_each(give_n_digits, -term, -p.value) %>%
    format_p %>%
    rename_cols %>%
    standard_flex %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::align(align = "left", j = left_align, part = "all") %>%
    flextable::align(align = "right", j = n_cols[-left_align], part = "all") %>%
    set_col_widths
  } else {
    mod %>%
      mutate_each(give_n_digits, -term, -p.value) %>%
      format_p %>%
      rename_cols %>%
      knitr::kable(., format = "latex", booktabs = T, escape = F,
           #caption = 'Posterior parameter estimates for voiced stops.',
           align = c("@{}l", rep("r", length(n_cols) -2), "r@{}")
           ) %>%
      kableExtra::kable_styling(font_size = font_size,
                                latex_options = c("hold_position"))
  }

}


#' @export
#' @examples
#' mod1a <- lmer(Reaction ~ 1 + Days + (1|Subject), data = sleepstudy)
#' mod_to_table(mod1a)

mod_to_table.lmerMod <- function(
  model, param_names = NULL, font_size = 11, left_align = 1, width = NULL,
  col = NULL) {

  doc_type <- set_doc_type()

  # Get tidy model
  mod <- suppressWarnings(broom::tidy(model, conf.int = TRUE)) %>%
    filter(group == "fixed") %>%
    select(-group)

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
  mutate_each(give_n_digits, -term, -p.value) %>%
    format_p %>%
    rename_cols %>%
    standard_flex %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::align(align = "left", j = left_align, part = "all") %>%
    flextable::align(align = "right", j = n_cols[-left_align], part = "all") %>%
    set_col_widths
  } else {
    mod %>%
      mutate_each(give_n_digits, -term, -p.value) %>%
      format_p %>%
      rename_cols %>%
      knitr::kable(., format = "latex", booktabs = T, escape = F,
           #caption = 'Posterior parameter estimates for voiced stops.',
           align = c("@{}l", rep("r", length(n_cols) -2), "r@{}")
           ) %>%
      kableExtra::kable_styling(font_size = font_size,
                                latex_options = c("hold_position"))

  }
}


#' @export
#' @examples

mod_to_table.lmerModLmerTest <- function(
  model, param_names = NULL, font_size = 11, left_align = 1, width = NULL,
  col = NULL) {

  doc_type <- set_doc_type()

  # Get tidy model
  mod <- suppressWarnings(broom::tidy(model, conf.int = TRUE)) %>%
    filter(effect == "fixed") %>%
    select(-effect, -group)

  # Create vector of number of columns for alignment
  n_cols <- 1:length(colnames(mod))

  # Table adjustments
  mod$term <- parameter_adj(mod = mod, param_names = param_names)
  set_col_widths <- column_widths(width = width, col = col)
  rename_cols <- set_col_names(model = model, doc_type = doc_type)

  if (doc_type == "docx") {

  standard_flex <- set_flex(model)

  mod %>%
  mutate_each(give_n_digits, -term, -p.value) %>%
    format_p %>%
    rename_cols %>%
    standard_flex %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::align(align = "left", j = left_align, part = "all") %>%
    flextable::align(align = "right", j = n_cols[-left_align], part = "all") %>%
    set_col_widths
  } else {
    mod %>%
      mutate_each(give_n_digits, -term, -p.value) %>%
      format_p %>%
      rename_cols %>%
      knitr::kable(., format = "latex", booktabs = T, escape = F,
           #caption = 'Posterior parameter estimates for voiced stops.',
           align = c("@{}l", rep("r", length(n_cols) -2), "r@{}")
           ) %>%
      kableExtra::kable_styling(font_size = font_size,
                                latex_options = c("hold_position"))

  }
}


#' @export
#' @examples
#' mod4 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
#'               data = cbpp, family = binomial)
#' mod_to_table(mod4)

mod_to_table.glmerMod <- function(
  model, param_names = NULL, font_size = 11, left_align = 1, width = NULL,
  col = NULL) {

  doc_type <- set_doc_type()

  # Get tidy model
  mod <- suppressWarnings(broom::tidy(model, conf.int = TRUE)) %>%
    filter(effect == "fixed") %>%
    select(-effect, -group)

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
  mutate_each(give_n_digits, -term, -p.value) %>%
    format_p %>%
    rename_cols %>%
    standard_flex %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::align(align = "left", j = left_align, part = "all") %>%
    flextable::align(align = "right", j = n_cols[-left_align], part = "all") %>%
    set_col_widths
  } else {
    mod %>%
      mutate_each(give_n_digits, -term, -p.value) %>%
      format_p %>%
      rename_cols %>%
      knitr::kable(., format = "latex", booktabs = T, escape = F,
           #caption = 'Posterior parameter estimates for voiced stops.',
           align = c("@{}l", rep("r", length(n_cols) -2), "r@{}")
           ) %>%
      kableExtra::kable_styling(font_size = font_size,
                                latex_options = c("hold_position"))

  }
}


#' @export
#' @examples

mod_to_table.brmsfit <- function(
  model, param_names = NULL, font_size = 11, left_align = 1, width = NULL,
  col = NULL) {

  doc_type <- set_doc_type()

  # Get tidy model
  mod <- suppressWarnings(broom::tidy(model, conf.int = TRUE)) %>%
    filter(effect == "fixed") %>%
    select(-effect, -component, -group)

  # calc prob that given beta is > 0
  p_beta <- posterior_samples(model) %>%
    select(starts_with("b_")) %>%
    rename(`(Intercept)` = b_Intercept) %>%
    tidyr::gather(term, estimate) %>%
    group_by(term) %>%
    summarize(MPE = bayestestR::p_direction(estimate) %>% round(., 3)) %>%
    mutate(term = gsub("b_", "", fixed = T, paste(.$term)))

  mod <- left_join(mod, p_beta, by = "term")

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
    mutate_each(give_n_digits, -term) %>%
      rename_cols %>%
      standard_flex %>%
      flextable::fontsize(size = font_size, part = "all") %>%
      flextable::align(align = "left", j = left_align, part = "all") %>%
      flextable::align(align = "right", j = n_cols[-left_align], part = "all") %>%
      set_col_widths
  } else {
    mod %>%
      mutate_each(give_n_digits, -term) %>%
      rename_cols %>%
      knitr::kable(., format = "latex", booktabs = T, escape = T,
           #caption = 'Posterior parameter estimates for voiced stops.',
           align = c("@{}l", rep("r", length(n_cols) -2), "r@{}")
           ) %>%
      kableExtra::kable_styling(font_size = font_size,
                                latex_options = c("hold_position"))

  }
}






# Helpers ---------------------------------------------------------------------
# Add standard column names
set_col_names <- function(model, doc_type = doc_type) {
  if (doc_type == "docx") {
    if (class(model)[1] %in% c("lmerModLmerTest")) {
      rename_cols <- . %>%
        select(Parameter = term, Estimate = estimate, SE = std.error,
               `CI low` = conf.low, `CI high` = conf.high, t = statistic, df,
               p = p.value)
    } else if (class(model)[1] == "brmsfit") {
      rename_cols <- . %>%
        select(Parameter = term, Estimate = estimate, SE = std.error,
               `2.5%` = conf.low, `97.5%` = conf.high, MPE)
    } else {
      rename_cols <- . %>%
        select(Parameter = term, Estimate = estimate, SE = std.error,
               `CI low` = conf.low, `CI high` = conf.high, t = statistic,
               p = p.value)
    }
  } else {
    if (class(model)[1] %in% c("lmerModLmerTest")) {
      rename_cols <- . %>%
        select(Parameter = term, Estimate = estimate, SE = std.error,
               `CI low` = conf.low, `CI high` = conf.high,
               `\\emph{t}` = statistic, df, `\\emph{p}` = p.value)
    } else if (class(model)[1] == "brmsfit") {
      rename_cols <- . %>%
        select(Parameter = term, Estimate = estimate, SE = std.error,
               `2.5%` = conf.low, `97.5%` = conf.high, MPE)
    } else {
      rename_cols <- . %>%
        select(Parameter = term, Estimate = estimate, SE = std.error,
               `CI low` = conf.low, `CI high` = conf.high,
               `\\emph{t}` = statistic, `\\emph{p}` = p.value)
    }
  }
}


# Adjust parameter names
parameter_adj <- function(mod, param_names = NULL) {

  if (!is.null(param_names)) {
    mod$term <- forcats::fct_recode(mod$term, !!!param_names)
  return(mod$term)
  } else {
  return(mod$term)
  }

}


# Standard flextable format
set_flex <- function(model) {
  if (class(model) == "brmsfit") {
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
  } else {
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
  }
}


# Add pvalue if missing
add_p <- function(mod) {
  if(!("p.value" %in% colnames(mod))) {
    mod$p.value <- normal_approximation(as.numeric(mod$statistic))
  }
}


# Format the pvalues and remove fluff
format_p <- . %>%
  mutate(p.value = print_pval(p.value, latex = F)) %>%
  mutate(p.value = gsub("*p* = ", "", fixed = T, paste(.$p.value))) %>%
  mutate(p.value = gsub("*p* ", "", fixed = T, paste(.$p.value)))


# Determine columns widths
column_widths <- function(width = NULL, col = NULL) {
  # Set col widths (default to 1)
  if (is.null(width)) {
    set_col_widths <- . %>% flextable::width(., width = 1)
    } else if (is.numeric(width & is.null(col))) {
    set_col_widths <- . %>% flextable::width(., width = width)
    } else {
    set_col_widths <- . %>% flextable::width(., j = col, width = width)
  }
}

# Get document type/set default
set_doc_type <- function() {
  this_doc <- knitr::opts_knit$get('rmarkdown.pandoc.to')
  if (is.null(this_doc)) {
    this_doc <- "latex"
  }
  return(this_doc)
}


# add ROPE?


utils::globalVariables(
  c("effect", "component", "group", "p.value", "estimate", "std.error", "df",
    "conf.low", "conf.high", "doc_type", "posterior_samples", "b_Intercept",
    "MPE"))
