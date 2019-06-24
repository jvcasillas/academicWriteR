
## Overview <img src='https://raw.githubusercontent.com/jvcasillas/hex_stickers/master/stickers/academicWriteR.png' align='right' width='275px' style="padding-left:5px;"/>

This is a package of helper functions that are useful for writing
academic reports/manuscripts. The majority of the functions were written
for reporting statistical analyses, but there are also a few functions
that were designed for project management and document preparation using
`papaja`.

## Functions

``` r
academicWriteR::print_dir_ul(path = "./R")
```

  - count\_words.R
  - create\_project.R
  - normal\_approximation.R
  - print\_builder.R
  - print\_dir\_ul.R
  - print\_model\_param.R
  - print\_model\_params\_p.R
  - print\_model\_params.R
  - print\_nmc\_p.R
  - print\_pval.R
  - round\_pval.R

## Examples

### Rounding

`academicWriteR` includes a rounding function, `round_pval`,
specifically for p-values. It is precise up to the thousandths column.
Anything smaller than 0.001 is rounded to 0.001. `round_pval` is used in
the printing functions.

``` r
library(academicWriteR)

round_pval(0.03765) 
round_pval(0.0000001)
round_pval(0.07)
```

    ## [1] ".038"
    ## [1] ".001"
    ## [1] ".07"

### Printing functions

The printing functions were made to facilitate reporting statistical
models in RMarkdown using `papaja`. `papaja` comes with it’s own
printing functions but I found that I am constantly tinkering with them
to get things to work the way I prefer, thus I decided to make my own.

The first is a wrapper around the `round_pval` function: `print_pval`.

``` r
print_pval(0.03765, latex = FALSE)
```

*p* = .038.

P-values are printed exactly unless they are less than 0.001. Trailing
0’s are deleted.

``` r
print_pval(0.0765, latex = FALSE)
```

*p* = .076.

This function is included in other printing functions. Set `latex =
TRUE` is the output is LaTeX.

#### `lm()` example

The printing functions are used for reporting the results of statistical
analyses. There are designed to be used in in-line `knitr` chunks (i.e.,
`` `r 1 + 1` ``) in an RMarkdown document. Instead of going in to more
detail here, I will just work through and example. First, I will fit a
linear model with the `lm` function.

``` r
lm_mod1 <- lm(mpg ~ wt, data = mtcars)
summary(lm_mod1)$coef
```

    ##              Estimate Std. Error   t value     Pr(>|t|)
    ## (Intercept) 37.285126   1.877627 19.857575 8.241799e-19
    ## wt          -5.344472   0.559101 -9.559044 1.293959e-10

In a typical write-up of the results we might say something like:

**Miles per gallon varied as a function of `weight` (wt). Specifically,
a 1-unit increase in weight was associated with a decrease in `mpg` of
-5.34 +/- 0.56 standard errors (CI = \[-6.49, -4.2\]; *t* = -9.56; *p*
\< .001).**

The aforementioned sentence was written in RMarkdown like this:

``` r
Miles per gallon varied as a function of `weight` (wt). Specifically, a 
1-unit increase in weight was associated with a decrease in `mpg` of 
`r print_model_params(model = lm_mod1, predictor = 'wt', parameter = 'estimate')` 
+/- `r print_model_params(model = lm_mod1, predictor = 'wt', parameter = 'std.error')`
standard errors `r print_model_params_p(model = lm_mod1, predictor = "wt", latex = FALSE)`.
```

Here I use two print functions: `print_model_params` and
`print_model_params_p`. The `_p` variant referes to parenthesis. In
other words it prints the parameter information that I typically put
between parenthesis. Notice each in-line chunk is given it’s own line
(more on this below).

#### `lmer()` example

Here is another example using `lme4` objects.

``` r
library(lme4)
library(lmerTest)
library(broom.mixed)
lmer_mod0 <- lmer(Reaction ~ 1 + (1|Subject), data = sleepstudy, REML = F)
lmer_mod1 <- lmer(Reaction ~ 1 + Days + (1|Subject), data = sleepstudy, REML = F)
model_comparison <- anova(lmer_mod0, lmer_mod1, test = 'Chisq')
summary(lmer_mod1)$coef
```

    ##              Estimate Std. Error        df  t value    Pr(>|t|)
    ## (Intercept) 251.40510  9.5061887  24.49051 26.44647 1.58857e-19
    ## Days         10.46729  0.8017354 162.00001 13.05579 4.46271e-27

And a possible write-up:

**There was a main effect of days (χ<sup>2</sup>(1) = 116.46; *p* \<
.001). Specifically, a change of one day was associated with an increase
in reaction time of 10.47 ms +/- 0.8 standard errors (CI = \[8.9,
12.04\]; *t* = 13.06; *p* \< .001).**

The above paragraph was written as follows:

``` r
**There was a main effect of days 
`r print_nmc_p(table = model_comparison, model = 'lmer_mod1', latex = FALSE)`. 
Specifically, a change of one day was associated with an increase in reaction 
time of 
`r print_model_params(model = lmer_mod1, predictor = 'Days', parameter = 'estimate')` 
ms +/- 
`r print_model_params(model = lmer_mod1, predictor = 'Days', parameter = 'std.error')` 
standard errors 
`r print_model_params_p(model = lmer_mod1, predictor = "Days", latex = FALSE)`.**
```

If you don’t interpret the effect directly in the prose you can print
all the parameters at once using `print_model_param(model, parameter)`.

``` r
print_model_param(lm_mod1, "(Intercept)", latex = F)
print_model_param(lmer_mod1, "(Intercept)", latex = F)
```

Ex.  
The intercept differed from 0 (β = 37.29; SE = 1.88; CI = \[33.45,
41.12\]; *t* = 19.86; *p* \< .001).

Ex.  
The intercept differed from 0 (β = 251.41; SE = 9.51; CI = \[232.77,
270.04\]; *t* = 26.45; *p* \< .001).

#### Models fitted in `brms`

You can now (v. 0.3) print `brms` model objects using
`print_model_param`.

``` r
library(brms)
brms_mod <- brm(mpg ~ wt, data = mtcars)
```

Miles per gallon varied as a function of weight (β = −5.35; SE = 0.59;
CI = \[−6.51, −4.18\]).

…which was written as:

``` r
Miles per gallon varied as a function of weight 
`r print_model_param(brms_mod, "wt", latex = FALSE)`.
```

#### Directory structures

The `print_dir_ul` function will print the files of a given directory as
an unordered list. This function was originally written to improve
project management. I typically use it in README files to print the
files in the current directory.

``` r
print_dir_ul(path = ".")
```

    ## - academicWriteR.Rproj 
    ##  - DESCRIPTION 
    ##  - docs 
    ##  - LICENSE 
    ##  - man 
    ##  - NAMESPACE 
    ##  - R 
    ##  - README_cache 
    ##  - README.md 
    ##  - README.Rmd

If you set `results='asis'` in the knitr chunk you will get an unordered
list in markdown/HTML.

    ```{r, 'print_dir_ul-ex', results='asis'}
    print_dir_ul(path = ".")
    ```

  - academicWriteR.Rproj
  - DESCRIPTION
  - docs
  - LICENSE
  - man
  - NAMESPACE
  - R
  - README\_cache
  - README.md
  - README.Rmd

If you set `nested = TRUE` you can print the directory of your choice
and the files nested within any subdirectories. Additionally, the
`remove` argument can be used to exclude files/folders using regex.

    ```{r, 'print_dir_ul-ex2'}
    print_dir_ul(path = ".", nested = TRUE, remove = ".md")
    ```

    ## - academicWriteR.Rproj 
    ##     -  
    ## - DESCRIPTION 
    ##     -  
    ## - docs 
    ##     - authors.html 
    ##     - docsearch.css 
    ##     - docsearch.js 
    ##     - index.html 
    ##     - LICENSE-text.html 
    ##     - link.svg 
    ##     - pkgdown.css 
    ##     - pkgdown.js 
    ##     - pkgdown.yml 
    ##     - reference 
    ## - LICENSE 
    ##     -  
    ## - man 
    ##     - count_words.Rd 
    ##     - create_project.Rd 
    ##     - normal_approximation.Rd 
    ##     - print_builder.Rd 
    ##     - print_dir_ul.Rd 
    ##     - print_model_param.Rd 
    ##     - print_model_params_p.Rd 
    ##     - print_model_params.Rd 
    ##     - print_nmc_p.Rd 
    ##     - print_pval.Rd 
    ##     - round_pval.Rd 
    ## - NAMESPACE 
    ##     -  
    ## - R 
    ##     - count_words.R 
    ##     - create_project.R 
    ##     - normal_approximation.R 
    ##     - print_builder.R 
    ##     - print_dir_ul.R 
    ##     - print_model_param.R 
    ##     - print_model_params_p.R 
    ##     - print_model_params.R 
    ##     - print_nmc_p.R 
    ##     - print_pval.R 
    ##     - round_pval.R 
    ## - README_cache 
    ##     - gfm

## Word count

If your output is a word document, you will probably find this function
to be of little value. However, if your manuscript is knit as a PDF, you
have probably discovered that there isn’t a straightforward way to get a
word count, especially in a dynamic document.

The `count_words` function will give an approximate count of words in an
.Rmd document. It is not perfect—it is still a work in progress—, but it
is pretty good if you are careful in how you write. The function uses
regex to remove knitr code chunks, the standard variety as well as
in-line code. For this reason it is best to keep in-line code chunks on
their own line (to avoid not counting words that are also on the same
line). Also, when using comments in RMarkdown (i.e., `<!-- comment -->`)
one should keep each arrow on its own line. For example,

    <!--
    This is a comment
    -->

If you write your comments like this:

    <!-- this is a comment -->

no words will be counted after this point in the document (I am working
on fixing this).

``` r
This document contains 
`r count_words("README.Rmd")`
words. 
```

This document contains 665 words.
