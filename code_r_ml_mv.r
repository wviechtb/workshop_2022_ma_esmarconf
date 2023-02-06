############################################################################

# materials from the "Meta-Analysis with R" workshop given by Wolfgang
# Viechtbauer on February 21 (with a follow-up session on May 6), 2022, as
# part of the 2022 "Evidence Synthesis & Meta-Analysis in R Conference"
#
# workshop details: https://www.wvbauer.com/doku.php/workshop_2022_ma_esmarconf
# author details:   https://www.wvbauer.com/

############################################################################

# load metafor package
library(metafor)

############################################################################

# meta-analysis on the effects of modified school calendars on student
# achievement
#
# source: Konstantopoulos, S. (2011). Fixed effects and variance
# components estimation in three-level meta-analysis. Research Synthesis
# Methods, 2(1), 61–76. https://doi.org/10.1002/jrsm.35
#
# original meta-analysis: Cooper, H., Valentine, J. C., Charlton, K., &
# Melson, A. (2003). The effects of modified school calendars on student
# achievement and on school and community attitudes. Review of Educational
# Research, 73(1), 1–52. https://doi.org/10.3102/00346543073001001

# for a description of the dataset, see:
help(dat.konstantopoulos2011)

# copy data into 'dat'
dat <- dat.konstantopoulos2011

# show data
dat

# fit standard random-effects model
res <- rma(yi, vi, data = dat)
res

# fit standard random-effects model with rma.mv()
res <- rma.mv(yi, vi, random = ~ 1 | study, data = dat)
res

# fit multilevel random-effects model
res <- rma.mv(yi, vi, random = ~ 1 | district/school, data = dat)
res

# variance components
round(res$sigma2, digits=4)

# within cluster correlation of true outcomes
round(res$sigma2[1] / sum(res$sigma2), digits=4)

# total heterogeneity
round(sum(res$sigma2), digits=4)

# fit multivariate random-effects model
res <- rma.mv(yi, vi, random = ~ school | district, data = dat)
res

############################################################################

# meta-analysis comparing surgical and non-surgical treatments for medium-
# severity periodontal disease
#
# source: Berkey, C. S., Hoaglin, D. C., Antczak-Bouckoms, A., Mosteller, F.,
# & Colditz, G. A. (1998). Meta-analysis of multiple outcomes by regression
# with random effects. Statistics in Medicine, 17(22), 2537–2550.
# https://doi.org/10.1002/(sici)1097-0258(19981130)17:22<2537::aid-sim953>3.0.co;2-c

# for a description of the dataset, see:
help(dat.berkey1998)

# copy data into 'dat'
dat <- dat.berkey1998

# show data
dat

# construct var-cov matrix of the sampling errors
V <- vcalc(vi=1, cluster=author, rvars=c(v1i, v2i), data=dat)
V

# fit multivariate random-effects model
res <- rma.mv(yi, V, mods = ~ outcome - 1, data = dat,
              random = ~ outcome | trial, struct = "UN")
res

# contrast for differences in outcomes
anova(res, L=c(1,-1))

############################################################################

# what if you cannot construct the V matrix?

# meta-analysis on the association between recidivism and mental health in
# delinquent juveniles
#
# source: Assink, M., & Wibbelink, C. J. M. (2016). Fitting three-level
# meta-analytic models in R: A step-by-step tutorial. The Quantitative Methods
# for Psychology, 12(3), 154–174. https://doi.org/10.20982/tqmp.12.3.p154
#
# original meta-analysis: Assink, M., van der Put, C. E., Hoeve, M., de Vries,
# S. L. A., Stams, G. J. J. M., & Oort, F. J. (2015). Risk factors for
# persistent delinquent behavior among juveniles: A meta-analytic review.
# Clinical Psychology Review, 42, 47–61. https://doi.org/10.1016/j.cpr.2015.08.002
#
# note: the standardized mean differences are already computed for the studies
# in this dataset, so there is no need to use the escalc() function; positive
# values indicate a higher prevalence of recidivism in the group of juveniles
# with a mental health disorder

# for a description of the dataset, see:
help(dat.assink2016)

# copy data into 'dat'
dat <- dat.assink2016

# examine first 10 rows of dataset
head(dat, 10)

# standard random-effects model that completely ignores the dependencies
rma(yi, vi, data = dat)

# 1) fit multilevel random-effects model ignoring covariances in the V matrix

res <- rma.mv(yi, vi, random = ~ 1 | study/esid, data = dat)
res

# 2) use cluster-robust inference methods

robust(res, cluster=study)

# even better: use the clubSandwich package

robust(res, cluster=study, clubSandwich=TRUE)

# 3) approximate the V matrix, fit model, and do sensitivity analyses

V <- vcalc(vi, cluster=study, obs=esid, data=dat, rho=0.5)
head(dat, 10)
round(V[1:10,1:10], 4)

res <- rma.mv(yi, V, random = ~ 1 | study/esid, data = dat)
res

V <- vcalc(vi, cluster=study, obs=esid, data=dat, rho=0.7)

res <- rma.mv(yi, V, random = ~ 1 | study/esid, data = dat)
res

V <- vcalc(vi, cluster=study, obs=esid, data=dat, rho=0.3)

res <- rma.mv(yi, V, random = ~ 1 | study/esid, data = dat)
res

# 4) combine approaches 2 and 3

V <- vcalc(vi, cluster=study, obs=esid, data=dat, rho=0.5)
res <- rma.mv(yi, V, random = ~ 1 | study/esid, data = dat)
robust(res, cluster=study, clubSandwich=TRUE)

# test if type of delinquent behavior is a potential moderator

res <- rma.mv(yi, V, mods = ~ deltype, random = ~ 1 | study/esid, data = dat)
res
robust(res, cluster=study, clubSandwich=TRUE)

############################################################################
