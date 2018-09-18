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
