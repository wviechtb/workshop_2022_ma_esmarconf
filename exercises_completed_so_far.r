############################################################################

# load metafor package
library(metafor)

############################################################################

# meta-analysis examining the effectiveness of school-based writing-to-learn
# interventions on academic achievement
#
# source: Bangert-Drowns, R. L., Hurley, M. M., & Wilkinson, B. (2004). The
# effects of school-based writing-to-learn interventions on academic
# achievement: A meta-analysis. Review of Educational Research, 74(1), 29-58.
# https://doi.org/10.3102/00346543074001029 https://www.jstor.org/stable/3516060
#
# one of the included studies: Ganguli, A. B. (1989). Integrating writing in
# developmental mathematics. College Teaching, 37(4), 140-142.
# https://www.jstor.org/stable/27558364
#
# note: the standardized mean differences are already computed for the studies
# in this dataset, so there is no need to use the escalc() function; positive
# values indicate a higher mean level of academic achievement in the group
# receiving a writing-to-learn intervention compared to the control group

# for a description of the dataset, see:
help(dat.bangertdrowns2004)

# copy data to 'dat'
dat <- dat.bangertdrowns2004
dat

# note: NA = not available (i.e., missing values)

# illustrate the calculation of the SMD for study 14: Ganguli (1989)
escalc(measure="SMD", m1i=342, sd1i=68, n1i=27,
                      m2i=303, sd2i=75, n2i=23)

# fit a random-effects model (use either the DL or REML estimator)
res <- rma(yi, vi, data=dat)
res

# obtain the 95% prediction interval
predict(res, digits=2)

# do the results suggest that writing-to-learn interventions have on average a
# positive effect on academic achievement? if so, how consistent is the effect
# across studies?

# obtain a 95% CIs for tau^2 and I^2
confint(res)

# fit an equal-effects model (as in Bangert-Drowns et al., 2004)
res <- rma(yi, vi, data=dat, method="EE", digits=2)
res

# find the results provided by this model in Bangert-Drowns et al. (2004); do
# you think these are the results that should have been reported in the paper?
# why or why not?

# like Figure 1 in the paper, but without a normal distribution superimposed
# on the histogram, because we do NOT assume that the observed outcomes follow
# a normal distribution (the sampling distributions are assumed to be normal,
# but this does not imply that the observed outcomes are normally distributed)
hist(dat$yi, breaks=seq(-1.5, 2, by=0.25), xlab="Standardized Mean Difference",
     main="Histogram of the SMD Values")
abline(v=0, lwd=3)

# fit mixed-effects meta-regression models with the following moderators (one
# at a time): grade (treated categorically!), length (continuously), wic,
# feedback, info, pers, imag, and meta

# for grade, compute the estimated average SMD for each level
res <- rma(yi, vi, mods = ~ factor(grade), data=dat)
res
predict(res, newmods=c(0,0,0), digits=2)
predict(res, newmods=c(1,0,0), digits=2)
predict(res, newmods=c(0,1,0), digits=2)
predict(res, newmods=c(0,0,1), digits=2)

# for length, compute the estimated average SMD for length equal to 1 and 24
res <- rma(yi, vi, mods = ~ length, data=dat)
res
predict(res, newmods=1,  digits=2)
predict(res, newmods=24, digits=2)

# scatterplot of the SMDs against length with the regression line added
regplot(res, xlim=c(0,25), xlab="Length", las=1, digits=1, bty="l")

# other moderators
rma(yi, vi, mods = ~ wic, data=dat)
rma(yi, vi, mods = ~ feedback, data=dat)
rma(yi, vi, mods = ~ info, data=dat)
rma(yi, vi, mods = ~ pers, data=dat)
rma(yi, vi, mods = ~ imag, data=dat)
rma(yi, vi, mods = ~ meta, data=dat)

# for meta, compute the estimated average SMD for meta=0 and meta=1
res <- rma(yi, vi, mods = ~ meta, data=dat)
res
predict(res, newmods=0, digits=2)
predict(res, newmods=1, digits=2)

# fit a model with multiple moderators and compute some predicted average SMDs
# for various combinations of the moderator values; do you see differences in
# the relevance of particular moderator variables in the model compared to the
# models where each moderator was examined individually?
res <- rma(yi, vi, mods = ~ factor(grade) + length + meta, data=dat)
res

# predicted average SMDs for grade = 2, length = 15, and meta=0/1
predict(res, newmods=c(1,0,0, 15, 0), digits=2)
predict(res, newmods=c(1,0,0, 15, 1), digits=2)

# test the grade factor as a whole (use btt to specify the coefficient numbers
# that you want to include in the omnibus test; here, we want to test the 2nd,
# 3rd, and 4th coefficient)
anova(res, btt=2:4)

# can also specify a string that identifies all coefficients to be tested
anova(res, btt="grade")

############################################################################

# meta-analysis on the relationship between class attendance and class
# performance / grade point average in college students
#
# source: personal communication
#
# original meta-analysis: Crede, M., Roch, S. G., & Kieszczynka, U. M. (2010).
# Class attendance in college: A meta-analytic review of the relationship of
# class attendance with grades and student characteristics. Review of
# Educational Research, 80(2), 272-295. https://doi.org/10.3102/0034654310362998
# https://www.jstor.org/stable/40658464
#
# one of the included studies: Culler, R. E., & Holahan, C. J. (1980). Test
# anxiety and academic performance: The effects of study-related behaviors.
# Journal of Educational Psychology, 72(1), 16-20.
# https://doi.org/10.1037/0022-0663.72.1.16 (note: study 12 in the dataset)
#
# note: the data used in the meta-analysis by Crede et al. (2010) are slightly
# different than the data included in this dataset (but just slightly)

# for a description of the dataset, see:
help(dat.crede2010)

# copy data to 'dat'
dat <- dat.crede2010
dat

# we will focus on the relationship between class attendance and performance
# (i.e., the grade) within the class (i.e., when the criterion is 'grade')

# calculate r-to-z transformed correlations and corresponding sampling variances
# (note: using 'subset' to select those rows where criterion is 'grade')
dat <- escalc(measure="ZCOR", ri=ri, ni=ni, data=dat, subset=criterion=="grade")
dat

# fit a random-effects model
res <- rma(yi, vi, data=dat)
res

# compute the estimated average true correlation, 95% CI, and 95% PI
# note: use the back-transformation transf=transf.ztor
predict(res, transf=transf.ztor, digits=2)

# is there on average a correlation between class attendance and performance?
# if so, how strong is that correlation? how consistent is the correlation
# across studies? what was the estimated average correlation reported in Crede
# et al. (2004)?

# fit a mixed-effects meta-regression model with class as moderator and
# compute the predicted average correlation for non-science and science
# classes
res <- rma(yi, vi, mods = ~ class, data=dat)
res
predict(res, newmods=0, transf=transf.ztor, digits=2)
predict(res, newmods=1, transf=transf.ztor, digits=2)

# fit a mixed-effects meta-regression model with source as moderator
res <- rma(yi, vi, mods = ~ source, data=dat)
res

# compute the predicted average correlations for the three source types; for
# which type is the estimated average correlation the highest / the lowest?
predict(res, newmods=c(0,0), transf=transf.ztor, digits=2)
predict(res, newmods=c(1,0), transf=transf.ztor, digits=2)
predict(res, newmods=c(0,1), transf=transf.ztor, digits=2)

# fit a mixed-effects meta-regression model with the year of publication as
# moderator; does it appear as if the strength of the correlation has changed
# over time? if so, is it getting stronger or weaker?
res <- rma(yi, vi, mods = ~ year, data=dat)
res

# compute the predicted average correlations for 1973, 1999, and 2009
predict(res, newmods=1973, transf=transf.ztor, digits=2)
predict(res, newmods=1999, transf=transf.ztor, digits=2)
predict(res, newmods=2009, transf=transf.ztor, digits=2)

# scatterplot of r-to-z transformed correlations against year of publication
regplot(res, xlab="Year of Publication", las=1, digits=1, bty="l", psize="seinv")

# in Crede et al. (2004), the relationship between the year of publication and
# the correlations was tested with a simple correlation test (but this ignores
# differences in the size of the studies, so not a recommended approach)
cor.test(dat$ri, dat$year)

############################################################################
