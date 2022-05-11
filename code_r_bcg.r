############################################################################

# materials from the "Meta-Analysis with R" workshop given by Wolfgang
# Viechtbauer on February 21 (with a follow-up session on May 6), 2022, as
# part of the 2022 "Evidence Synthesis & Meta-Analysis in R Conference"
#
# workshop details: https://www.wvbauer.com/doku.php/workshop_ma_esmarconf
# author details:   https://www.wvbauer.com/

############################################################################

# meta-analysis examining the effectiveness of the BCG vaccine for preventing
# tuberculosis infections
#
# source: Colditz, G. A., Brewer, T. F., Berkey, C. S., Wilson, M. E.,
# Burdick, E., Fineberg, H. V., & Mosteller, F. (1994). Efficacy of BCG
# vaccine in the prevention of tuberculosis: Meta-analysis of the published
# literature. Journal of the American Medical Association, 271(9), 698-702.
# https://doi.org/10.1001/jama.1994.03510330076038
#
# one of the included studies: Hart, P. D. & Sutherland, I. (1977). BCG and
# vole bacillus vaccines in the prevention of tuberculosis in adolescence and
# early adult life. British Medical Journal, 2, 293-295.
# https://doi.org/10.1136/bmj.2.6082.293

############################################################################

# load metafor package
library(metafor)

# make sure that this script (code_r_bcg.r) and the BCG dataset (data_bcg.txt)
# are both placed in the same directory/folder on your computer; the first
# thing we need to do now is to set the 'working directory' to this location

# in RStudio: Menu 'Session', Set Working Directory, To Source File Location

# this sets the working directory to the location of the currently opened R
# script (i.e., code_r_bcg.r) and hence the following command will be able to
# find the dataset in the same directory

# read in data from data_bcg.txt
dat <- read.table("data_bcg.txt", header=TRUE)

# examine data
dat

# spreadsheet-like view
View(dat)

# or use the built-in dataset (called 'dat.bcg')
# copy BCG vaccine data to 'dat'
dat <- dat.bcg
dat

# tpos, tneg, cpos, cneg are the variables corresponding to the 2x2 tables
#
#          | TB+   TB-
# ---------+-----------
# treated  | tpos  tneg
# control  | cpos  cneg

# compute log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg,
                            ci=cpos, di=cneg, data=dat)
dat

# yi = the log risk ratios
# vi = the corresponding sampling variances

# check the calculation of the log risk rations for Hart & Sutherland (1977)
 62 / ( 62 + 13536) # risk in the treated (vaccinated) group
248 / (248 + 12619) # risk in the control (non-vaccinated) group
(62 / (62 + 13536)) / (248 / (248 + 12619)) # risk ratio
log((62 / (62 + 13536)) / (248 / (248 + 12619))) # log risk ratio

# so, a risk ratio of 1 leads to a log risk ratio of 0, which indicates that
# the risk in the treated (vaccinated) group is the same as the risk in the
# control (non-vaccinated group); if the risk is lower in the treated group,
# then the risk ratio will be below 1 and the log risk ratio below 0

# forest plot (a very basic one)
forest(dat$yi, dat$vi)
with(dat, forest(yi, vi))

# a slightly nicer one
with(dat, forest(yi, vi, slab=paste(author, year, sep=", "), header=TRUE))

# fit equal-effects model
res <- rma(yi, vi, method="EE", data=dat)
res

# double-check the computation of the estimate
with(dat, sum(1/vi * yi) / sum(1/vi))

# or round results to 2 digits
print(res, digits=2)

# back-transform results to the risk ratio scale
predict(res, transf=exp, digits=2)

# forest plot with results from the model
forest(res)

# fit random-effects model (using the DerSimonian-Laird estimator for tau^2)
res <- rma(yi, vi, method="DL", data=dat)
res

# fit random-effects model (the default is to use REML estimation)
rma(yi, vi, data=dat)

# or round results to 2 digits
print(res, digits=2)

# estimated average risk ratio (and 95% CI/PI)
predict(res, transf=exp, digits=2)

# forest plot with results from the model
forest(res)

# a nicer forest plot
res <- rma(yi, vi, data=dat, slab=paste(author, year, sep=", "))
forest(res, xlim=c(-16,6), header="Author(s) and Year", addpred=TRUE,
       ilab=cbind(tpos, tneg, cpos, cneg),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=.90)
par(cex=.90, font=2)
text(c(-9.5,-8,-6,-4.5), 15, c("TB+", "TB-", "TB+", "TB-"))
text(c(-8.75,-5.25),     16, c("Vaccinated", "Control"))

# a nicer forest plot (with back-transformation)
res <- rma(yi, vi, data=dat, slab=paste(author, year, sep=", "))
forest(res, xlim=c(-16,6), atransf=exp, at=log(c(.05,.25,1,4)),
       header="Author(s) and Year", addpred=TRUE,
       ilab=cbind(tpos, tneg, cpos, cneg),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=.90)
par(cex=.90, font=2)
text(c(-9.5,-8,-6,-4.5), 15, c("TB+", "TB-", "TB+", "TB-"))
text(c(-8.75,-5.25),     16, c("Vaccinated", "Control"))

# computation of I^2
res <- rma(yi, vi, data=dat)
res
k <- res$k
wi <- 1/res$vi
# think of 'vt' as a way to quantify the 'typical' sampling variance
vt <- (k-1) * sum(wi) / (sum(wi)^2 - sum(wi^2))
round(100 * res$tau2 / (res$tau2 + vt), digits=2)

# confidence interval for tau^2 and I^2
confint(res)

############################################################################

# fit mixed-effects meta-regression model
res <- rma(yi, vi, mods = ~ ablat, method="DL", data=dat)
res

# predicted average log risk ratio at 44 degrees
predict(res, newmods=44)

# predicted average risk ratio at 44 degrees
predict(res, newmods=44, digits=2, transf=exp)

# draw bubble plot
regplot(res, xlab="Absolute Latitude")
regplot(res, xlab="Absolute Latitude", xlim=c(0,60), predlim=c(0,60), transf=exp)
regplot(res, xlab="Absolute Latitude", xlim=c(0,60), predlim=c(0,60), transf=exp,
        refline=1, legend=TRUE, las=1, bty="l")

# computation of the (pseudo) R^2 value
res0 <- rma(yi, vi, method="DL", data=dat)
res1 <- rma(yi, vi, mods = ~ ablat, method="DL", data=dat)
round(100 * (res0$tau2 - res1$tau2) / res0$tau2, digits=2)

# mixed-effects meta-regression model with 2 moderators
res <- rma(yi, vi, mods = ~ ablat + year, method="DL", data=dat)
res

# predicted average risk ratios at 10, 30, and 50 degrees in 1970
predict(res, newmods=c(10, 1970), digits=2, transf=exp)
predict(res, newmods=c(30, 1970), digits=2, transf=exp)
predict(res, newmods=c(50, 1970), digits=2, transf=exp)

# plot the marginal relationships
regplot(res, mod="ablat", xlab="Absolute Latitude")
regplot(res, mod="year", xlab="Publication Year")

# create dummy variable (1 for random, 0 otherwise)
dat$random <- ifelse(dat$alloc=="random", 1, 0)

# subgrouping based on the 'random' dummy variable
res0 <- rma(yi, vi, method="DL", subset=c(random==0), data=dat)
res0
res1 <- rma(yi, vi, method="DL", subset=c(random==1), data=dat)
res1

# test whether the two coefficients differ from each other
rma(c(coef(res0), coef(res1)), c(vcov(res0), vcov(res1)), mods = c(0,1), method="FE")

# mixed-effects meta-regression model with dummy variable
res <- rma(yi, vi, mods = ~ random, method="DL", data=dat)
res

# predicted average log risk ratio for random=0
predict(res, newmods=0)

# predicted average log risk ratio for random=1
predict(res, newmods=1)

# predicted average risk ratio for random=0
predict(res, newmods=0, digits=2, transf=exp)

# predicted average risk ratio for random=1
predict(res, newmods=1, digits=2, transf=exp)

# these two approaches are not identical, since subgrouping allows for a
# different tau^2 value within subgroups, while the meta-regression approach
# assumes a single tau^2 value within subgroups

# mixed-effects meta-regression model with a categorical moderator
res <- rma(yi, vi, mods = ~ alloc, method="DL", data=dat)
res

# predicted average risk ratio for 'alternate'
predict(res, newmods=c(0,0), digits=2, transf=exp)

# predicted average risk ratio for 'random'
predict(res, newmods=c(1,0), digits=2, transf=exp)

# predicted average risk ratio for 'systematic'
predict(res, newmods=c(0,1), digits=2, transf=exp)

# for more details on this, see:
# https://www.metafor-project.org/doku.php/tips:comp_two_independent_estimates

############################################################################

# another example of a bubble plot
res <- rma(yi, vi, mods = ~ ablat, method="DL", data=dat)
regplot(res, xlim=c(10,60), predlim=c(10,60), xlab="Absolute Latitude",
        atransf=exp, at=log(seq(0.2,1.6,by=0.2)), digits=1, las=1, bty="l",
        refline=0, label=c(7,12,13), labsize=0.9)

############################################################################

# note: we will not focus on publication bias in the context of this dataset
# (instead, see code_r_magnesium.r), but for completeness sake some of the
# methods are also applied here to the BCG vaccine dataset

# random-effects model
res <- rma(yi, vi, method="DL", data=dat)
res

# funnel plot
funnel(res)

# regression test
regtest(res)

# trim and fill method
taf <- trimfill(res)
taf
funnel(taf)

# fit selection model (assuming selection in favor of significant negative effects)
sel <- selmodel(res, type="logistic", alternative="less")
sel
plot(sel)

# PET/PEESE
regtest(res)
regtest(res, predictor="vi")

# contour-enhanced funnel plot
funnel(res, xlim=c(-2.5,2.5), ylim=c(0,0.8), refline=0, level=c(90, 95, 99),
       shade=c("white", "gray55", "gray75"), legend=TRUE)

############################################################################
