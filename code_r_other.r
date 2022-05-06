############################################################################

# load metafor package
library(metafor)

############################################################################

### alternative methods for making inferences

# standard random-effects model
dat <- dat.bangertdrowns2004
res <- rma(yi, vi, method="DL", data=dat)
res

# the standard Wald-type z-test tends to have an inflated Type I error rate

# try to do an exact permutation test
permutest(res, exact=TRUE)

# approximate permutation test
sav <- permutest(res, iter=10000, seed=1234)
sav

# plot the permutation distribution of the test statistic
plot(sav, lwd=c(2,6,6,4), ylim=c(0,.4), col.ref="red")
legend("topleft", inset=.02, lty="solid", lwd=3, col=c("blue","red"),
       c("Kernel Density Estimate of\nthe Permutation Distribution",
       "Standard Normal Density"), cex=.8)

# Knapp and Hartung method (uses an adjusted standard error and an approximate
# t-distribution for conducting the test)
res <- rma(yi, vi, method="DL", data=dat, test="knha")
res

# can also use these methods in the context of meta-regression models
rma(yi, vi, mods = ~ length, method="DL", data=dat)
rma(yi, vi, mods = ~ length, method="DL", data=dat, test="knha")

############################################################################

### specialized methods for meta-analyzing 2x2 table data

# copy dataset to 'dat'
dat <- dat.nielweise2007
dat

# calculate log odds ratios and corresponding sampling variances
dat <- escalc(measure="OR", ai=ai, n1i=n1i, ci=ci, n2i=n2i, data=dat)
dat

# standard equal-effects model
res <- rma(yi, vi, method="EE", digits=2, data=dat)
res
predict(res, transf=exp)

# Mantel-Haenszel method
rma.mh(measure="OR", ai=ai, n1i=n1i, ci=ci, n2i=n2i, digits=2, data=dat)

# Peto's method
rma.peto(ai=ai, n1i=n1i, ci=ci, n2i=n2i, digits=2, data=dat)

# can also use generalized linear models (i.e., logistic regression) to
# analyze such data; a variety of different models types have been suggested

# unconditional model with fixed study effects
res <- rma.glmm(measure="OR", ai=ai, n1i=n1i, ci=ci, n2i=n2i,
                method="EE", model="UM.FS", digits=2, data=dat)
res
predict(res, transf=exp)

# unconditional model with random study effects
res <- rma.glmm(measure="OR", ai=ai, n1i=n1i, ci=ci, n2i=n2i,
                method="EE", model="UM.RS", digits=2, data=dat)
res
predict(res, transf=exp)

# conditional model with an exact likelihood
res <- rma.glmm(measure="OR", ai=ai, n1i=n1i, ci=ci, n2i=n2i,
                method="EE", model="CM.EL", digits=2, data=dat)
res
predict(res, transf=exp)

# conditional model with an approximate likelihood
res <- rma.glmm(measure="OR", ai=ai, n1i=n1i, ci=ci, n2i=n2i,
                method="EE", model="CM.AL", digits=2, data=dat)
res
predict(res, transf=exp)

# standard random-effects model
res <- rma(yi, vi, digits=2, data=dat)
res
predict(res, transf=exp)

# can again use various logistic regression models for the analysis

res <- rma.glmm(measure="OR", ai=ai, n1i=n1i, ci=ci, n2i=n2i,
                model="UM.FS", digits=2, data=dat)
res
predict(res, transf=exp)

res <- rma.glmm(measure="OR", ai=ai, n1i=n1i, ci=ci, n2i=n2i,
                model="UM.RS", digits=2, data=dat)
res
predict(res, transf=exp)

res <- rma.glmm(measure="OR", ai=ai, n1i=n1i, ci=ci, n2i=n2i,
                model="CM.EL", digits=2, data=dat)
res
predict(res, transf=exp)

res <- rma.glmm(measure="OR", ai=ai, n1i=n1i, ci=ci, n2i=n2i,
                model="CM.AL", digits=2, data=dat)
res
predict(res, transf=exp)

############################################################################

### model diagnostics (outliers, influential cases, assumptions)

# copy dataset to 'dat'
dat <- dat.viechtbauer2021
dat

# calculate log odds ratios and corresponding sampling variances
dat <- escalc(measure="OR", ai=xTi, n1i=nTi, ci=xCi, n2i=nCi,
              add=1/2, to="all", data=dat)
dat

# fit models

res.EE <- rma(yi, vi, data=dat, method="EE")
res.EE

res.RE <- rma(yi, vi, data=dat, method="DL")
res.RE

res.ME <- rma(yi, vi, mods = ~ dose, data=dat, method="DL")
res.ME

# forest plot
forest(dat$yi, dat$vi, psize=1, efac=0, xlim=c(-4,6), ylim=c(-3,23), header=TRUE)
addpoly(res.EE, row=-1.5, mlab="EE Model")
addpoly(res.RE, row=-2.5, mlab="RE Model")
abline(h=0)

# bubble plot
regplot(res.ME, xlim=c(0,250), predlim=c(0,250), digits=1, bty="l", las=1,
        xlab="Dosage (mg per day)", label=TRUE, labsize=0.8, offset=c(1,0.7))

# standardized deleted residuals (studentized residuals)
rstudent(res.EE)
rstudent(res.RE)
rstudent(res.ME)

# plot of the standardized (deleted) residuals for the RE model
plot(NA, NA, xlim=c(1,20), ylim=c(-4,4), xlab="Study", ylab="Standardized (Deleted) Residual",
     xaxt="n", main="Random-Effects Model", las=1)
axis(side=1, at=1:20)
abline(h=c(-1.96,1.96), lty="dotted")
abline(h=0)
points(1:20, rstandard(res.RE)$z, type="o", pch=19, col="gray70")
points(1:20, rstudent(res.RE)$z,  type="o", pch=19)
legend("top", pch=19, col=c("gray70","black"), lty="solid", bty="n",
       legend=c("Standardized Residuals","Standardized Deleted Residuals"))

# plot of the standardized (deleted) residuals for the ME model
plot(NA, NA, xlim=c(1,20), ylim=c(-4,4), xlab="Study", ylab="Standardized (Deleted) Residual",
     xaxt="n", main="Mixed-Effects Model", las=1)
axis(side=1, at=1:20)
abline(h=c(-1.96,1.96), lty="dotted")
abline(h=0)
points(1:20, rstandard(res.ME)$z, type="o", pch=19, col="gray70")
points(1:20, rstudent(res.ME)$z,  type="o", pch=19)
legend("top", pch=19, col=c("gray70","black"), lty="solid", bty="n",
       legend=c("Standardized Residuals","Standardized Deleted Residuals"))

# number of standardized deleted residuals larger than +-1.96 in each model
sum(abs(rstudent(res.EE)$z) >= qnorm(.975))
sum(abs(rstudent(res.RE)$z) >= qnorm(.975))
sum(abs(rstudent(res.ME)$z) >= qnorm(.975))

# Baujat plots
baujat(res.EE, main="Equal-Effects Model")
baujat(res.RE, main="Random-Effects Model")
baujat(res.ME, main="Mixed-Effects Model")

# GOSH plot for the EE model
res.GOSH.EE <- gosh(res.EE, subset=10000, seed=1234)
plot(res.GOSH.EE, out=6, breaks=50)

# GOSH plot for the ME model
res.GOSH.ME <- gosh(res.ME, subset=10000, seed=1234)
plot(res.GOSH.ME, het="tau2", out=6, breaks=50)

# checking normality of the standardized deleted residuals
qqnorm(res.ME, type="rstudent", label="out")

# checking normality of the random effects
ranef(res.ME)
hist(ranef(res.ME)$pred)
sav <- qqnorm(ranef(res.ME)$pred, cex=1, pch=19,
              xlim=c(-2.2,2.2), ylim=c(-0.6,0.6), las=1)
abline(a=0, b=sd(ranef(res.ME)$pred))
text(sav$x[6], sav$y[6], "6", pos=4, offset=0.4)

# outlier / influential case diagnostics
inf <- influence(res.ME)
inf

# plot the values
plot(inf)

# fit mixed-effects model without studies 3 and/or 6
rma(yi, vi, mods = ~ dose, data=dat, method="DL", subset=-3)
rma(yi, vi, mods = ~ dose, data=dat, method="DL", subset=-6)
rma(yi, vi, mods = ~ dose, data=dat, method="DL", subset=-c(3,6))

############################################################################

### variance inflation factors

# copy data to 'dat'
dat <- dat.bangertdrowns2004

# fit meta-regression model
res <- rma(yi, vi, mods = ~ factor(grade) + length + meta, data=dat)
res

# VIFs for the individual coefficients
vif(res)
vif(res, table=TRUE)

# generalized variance inflation factor for the grade factor
vif(res, btt=2:4)
vif(res, btt="grade")

############################################################################

### the reporter() function

# copy BCG vaccine data into 'dat'
dat <- dat.bcg

# calculate log risk ratios and corresponding sampling variances (using
# argument 'slab' to add the study labels to the escalc object)
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat,
              slab=paste(author, ", ", year, sep=""))

# fit random-effects model
res <- rma(yi, vi, data=dat)

# generate report
reporter(res)

############################################################################

### Bayesian models

# standard random-effects model (using K&H method)

dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
res1 <- rma(yi, vi, data=dat, test="knha")
res1
predict(res1)
confint(res1)

# Bayesian model using brms

#install.packages("brms")
library(brms)
dat$sei <- sqrt(dat$vi)
res2 <- brm(yi | se(sei) ~ 1 + (1 | trial), data=dat, family=gaussian, chains=1, iter=10000)
print(res2, digits=4)

# Bayesian model using bayesmeta

#install.packages("bayesmeta")
library(bayesmeta)
res3 <- bayesmeta(dat)
res3

# Bayesian model using metaBMA

#install.packages("metaBMA")
library(metaBMA)
res4 <- meta_random(yi, SE = sqrt(vi), data = dat)
res4

############################################################################
