
<img src='https://raw.githubusercontent.com/jvcasillas/hex_stickers/master/stickers/academicWriteR.png' align='right' width='275px'/>

## Overview

## Examples

``` r
library(academicWriteR)
library(lme4)
```

    ## Loading required package: Matrix

``` r
library(lmerTest)
```

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
library(broom.mixed)
mod1 <- lm(mpg ~ wt, data = mtcars)
mod2 <- lmer(Reaction ~ 1 + Days + (1|Subject), data = sleepstudy,
             REML = FALSE)
```

## Rounding functions

0.039 0.001

## Printing functions

*p* \< 0.039 *p* \< 0.001

There was an effect of `wt` (CI low = -6.49; CI high = -4.2; *t* =
-9.56; *p* \< 0.001).

There was an effect of `Days` (CI low = 8.9; CI high = 12.04; *t* =
13.06; *p* \< 0.001)

## Word count

The `count_words` function will give an approximate count of words in
.Rmd document.

## Functions

``` r
print_dir_ul(path = "./R")
```

  - count\_words.R
  - print\_dir\_ul.R
  - print\_model\_params\_p.R
  - print\_model\_params.R
  - print\_nmc\_p.R
  - print\_pval.R
  - round\_pval.R

This document contains 53 words.

<!--
this is a comment
-->

testing

<!--
another comment
-->

end
