############################################################################

# materials from the "Meta-Analysis with R" workshop given by Wolfgang
# Viechtbauer on February 21 (with a follow-up session on May 6), 2022, as
# part of the 2022 "Evidence Synthesis & Meta-Analysis in R Conference"
#
# workshop details: https://www.wvbauer.com/doku.php/workshop_ma_esmarconf2022
# author details:   https://www.wvbauer.com/

############################################################################

# load metafor package
library(metafor)

############################################################################

# first consider a meta-analysis of single group studies

# meta-analysis on the effectiveness of hyperdynamic therapy for treating
# cerebral vasospasm
#
# source: Zhou, X.-H., Brizendine, E. J., & Pritz, M. B. (1999). Methods for
# combining rates from several studies. Statistics in Medicine, 18(5), 557-566.
# https://doi.org/10.1002/(SICI)1097-0258(19990315)18:5<557::AID-SIM53>3.0.CO;2-F
#
# original meta-analysis: Pritz M. B., Zhou, X.-H., & Brizendine, E. J.
# (1996). Hyperdynamic therapy for cerebral vasospasm: A meta-analysis of 14
# studies. Journal of Neurovascular Disease, 1, 6-8.

# for a description of the dataset, see:
help(dat.pritz1997)

# copy data into 'dat'
dat <- dat.pritz1997

# show data
dat

# xi = number of patients that improved with hyperdynamic therapy
# ni = total number of patients treated

# compute proportions and corresponding sampling variances
dat <- escalc(measure="PR", xi=xi, ni=ni, data=dat)
dat

# fit random-effects model with the raw proportions
res <- rma(yi, vi, data=dat)
res

# compute log odds and corresponding sampling variances
dat <- escalc(measure="PLO", xi=xi, ni=ni, data=dat)
dat

# fit random-effects model with the log odds
res <- rma(yi, vi, data=dat)
res

# back-transform results to raw proportions
predict(res, transf=transf.ilogit)

############################################################################

# meta-analysis on the effectiveness of counseling for smoking cessation
#
# source: Hasselblad, V. (1998). Meta-analysis of multitreatment studies.
# Medical Decision Making, 18(1), 37-43. https://doi.org/10.1177/0272989X9801800110

# copy data into 'dat'
dat <- dat.hasselblad1998

# remove author and year variables
dat$authors <- NULL
dat$year    <- NULL

# calculate log odds for each study arm
dat <- escalc(measure="PLO", xi=xi, ni=ni, data=dat)

# show data
dat

# xi = number of individuals abstinent from smoking between months 6 and 12
# ni = total number of individuals in group

# convert trt variable to factor with desired ordering of levels
dat$trt <- factor(dat$trt, levels=c("no_contact", "self_help", "ind_counseling", "grp_counseling"))

# add a space before each level
levels(dat$trt) <- paste0(" ", levels(dat$trt))

# network meta-analysis using an arm-based multilevel model
res <- rma.mv(yi, vi, mods = ~ trt, random = ~ 1 | study/id, data = dat)
res

# all pairwise odds ratios of interventions versus no contact
predict(res, newmods=c(1, 0, 0), intercept=FALSE, transf=exp, digits=2)
predict(res, newmods=c(0, 1, 0), intercept=FALSE, transf=exp, digits=2)
predict(res, newmods=c(0, 0, 1), intercept=FALSE, transf=exp, digits=2)

# can do this more compactly in a single line of code
predict(res, newmods=diag(3), intercept=FALSE, transf=exp, digits=2)

# all pairwise odds ratios comparing interventions
predict(res, newmods=c(-1, 1, 0), intercept=FALSE, transf=exp, digits=2)
predict(res, newmods=c(-1, 0, 1), intercept=FALSE, transf=exp, digits=2)
predict(res, newmods=c( 0,-1, 1), intercept=FALSE, transf=exp, digits=2)

# can do this more compactly in a single line of code
predict(res, newmods=rbind(c(-1, 1, 0),
                           c(-1, 0, 1),
                           c( 0,-1, 1)), intercept=FALSE, transf=exp, digits=2)

# forest plot of results
forest(c(0,coef(res)[2:4]), sei=c(0,res$se[2:4]), psize=1, xlim=c(-3,4), digits=c(2,1), efac=2,
       slab=c("No Contact", "Self-Help", "Individual Counseling", "Group Counseling"),
       atransf=exp, at=log(c(.5, 1, 2, 4, 8)), xlab="Odds Ratio for Intervention vs. No Contact",
       header=c("Intervention", "OR [95% CI]"))

############################################################################
