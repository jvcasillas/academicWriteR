Overview
--------

Examples
--------

    library(academicWriteR)
    library(lme4)

    ## Loading required package: Matrix

    library(lmerTest)

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

    library(broom.mixed)
    mod1 <- lm(mpg ~ wt, data = mtcars)
    mod2 <- lmer(Reaction ~ 1 + Days + (1|Subject), data = sleepstudy,
                 REML = FALSE)

Rounding functions
------------------

0.039 0.001

Printing functions
------------------

*p* &lt; 0.039 *p* &lt; 0.001

There was an effect of `wt` (CI low = -6.49; CI high = -4.2; *t* =
-9.56; *p* &lt; 0.001).

There was an effect of `Days` (CI low = 8.9; CI high = 12.04; *t* =
13.06; *p* &lt; 0.001)

Word count
----------

The `count_words` function will give an approximate count of words in
.Rmd document.

Functions
---------

    print_dir_ul(path = "./R")

-   count\_words.R
-   print\_dir\_ul.R
-   print\_model\_params\_p.R
-   print\_model\_params.R
-   print\_nmc\_p.R
-   print\_pval.R
-   round\_pval.R

This document contains 38 words.
