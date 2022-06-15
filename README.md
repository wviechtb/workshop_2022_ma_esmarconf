# Meta-Analysis with R Workshop

### Wolfgang Viechtbauer

#### February 21 and May 6, 2022

## Overview

This repo contains the materials for the "Meta-Analysis with R" workshop given on February 21 (with a follow-up session on May 6), 2022, as part of the [2022 Evidence Synthesis & Meta-Analysis in R Conference](https://www.eshackathon.org/events/2022-01-ESMARConf2022.html). Further details about the workshop can be found [here](https://www.wvbauer.com/doku.php/workshop_ma_esmarconf).

## Structure

The lecture slides are provided [here](workshop_meta-analysis.pdf). We loosely followed the slides, but the focus in this workshop was on the application of the methods using R. Start with [code_r_bcg.r](code_r_bcg.r), which illustrates some of the basic functionality of the `metafor` package (in particular, the use of the `escalc()` function for computing various effect size or outcome measures and the `rma()` function for fitting equal/random-effects model to these values and for conducting meta-regression analyses). This is also the same example as used in [Viechtbauer (2010)](https://www.jstatsoft.org/article/view/v036i03). The [exercises_completed.r](exercises_completed.r) script provides further examples of this.

Next, we looked at the issue of publication bias and methods related to that. These are illustrated in [code_r_magnesium.r](code_r_magnesium.r). The exercise script again contains further examples. Then we got into multilevel/multivariate models, some of which are illustrated in [code_r_ml_mv.r](code_r_ml_mv.r). More examples thereof were again part of the exercises.

The [code_r_network_ma.r](code_r_network_ma.r) is relevant for those interested in network meta-analysis. Finally, the [code_r_other.r](code_r_other.r) script covers various topics, including improved methods for testing / constructing confidence intervals, specialized methods for meta-analyzing 2x2 table data, model diagnostics, and Bayesian models.
