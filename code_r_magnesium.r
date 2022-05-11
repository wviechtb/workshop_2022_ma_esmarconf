############################################################################

# materials from the "Meta-Analysis with R" workshop given by Wolfgang
# Viechtbauer on February 21 (with a follow-up session on May 6), 2022, as
# part of the 2022 "Evidence Synthesis & Meta-Analysis in R Conference"
#
# workshop details: https://www.wvbauer.com/doku.php/workshop_ma_esmarconf
# author details:   https://www.wvbauer.com/

############################################################################

# meta-analysis on the effectiveness of intravenous magnesium treatment in acute
# myocardial infarction for reducing the risk of mortality and arrhythmias

# Sources:
#
# Teo, K. K., Yusuf, S., Collins, R., Held, P. H., & Peto, R. (1991). Effects
# of intravenous magnesium in suspected acute myocardial infarction: Overview
# of randomised trials. British Medical Journal, 303(6816), 1499-1503.
# https://doi.org/10.1136/bmj.303.6816.1499
#
# Horner, S. M. (1992). Efficacy of intravenous magnesium in acute myocardial
# infarction in reducing arrhythmias and mortality: Meta-analysis of magnesium
# in acute myocardial infarction. Circulation, 86(3), 774-779.
# https://doi.org/10.1161/01.cir.86.3.774
#
# Sterne, J. A. C., & Egger, M. (2001). Funnel plots for detecting bias in
# meta-analysis: Guidelines on choice of axis. Journal of Clinical
# Epidemiology, 54(10), 1046-1055. See Table 1 (p. 1047).
# https://doi.org/10.1016/S0895-4356(01)00377-8
#
# one of the included studies: Ceremuiynski, L., Jurgiel, R., Kulakowski, P. &
# Gebalska, J. (1989). Threatening arrhythmias in acute myocardial infarction
# are prevented by intravenous magnesium sulfate. American Heart Journal,
# 118(6), 1333-1334. https://doi.org/10.1016/0002-8703(89)90027-6

############################################################################

# load metafor package
library(metafor)

############################################################################

# for a description of the dataset, see:
help(dat.egger2001)

# copy magnesium treatment dataset to 'dat'
dat <- dat.egger2001
dat

# remove studies 8 and 16
dat <- dat[-c(8,16),]

# compute the log risk ratios (using +1/2 adjustment for all studies)
dat <- escalc(measure="RR", ai=ai, n1i=n1i,
                            ci=ci, n2i=n2i, data=dat, add=1/2, to="all")
dat

# fit random-effects model
res <- rma(yi, vi, data=dat)
res

# estimated average risk ratio (with 95% CI)
predict(res, transf=exp, digits=2)

# these results suggest that magnesium treatment leads on average to a 50%
# reduction in mortality risk; however, this contradicts the results from the
# ISIS-4 study (which included more participants than all previous studies
# combined!), which found no reduction in risk in the magnesium group

# funnel plot
funnel(res, ylim=c(0,1.2))

# failsafe-N
fsn(yi, vi, data=dat)

# regression test for funnel plot asymmetry
# meta-regression model with sqrt(vi) as predictor
dat$sei  <- sqrt(dat$vi)
res <- rma(yi, vi, mods = ~ sei, data=dat)
res

# or use the regtest() function after fitting RE model
res <- rma(yi, vi, data=dat)
regtest(res)

# trim and fill method
res <- rma(yi, vi, data=dat)
taf <- trimfill(res)
taf

# funnel plot with filled-in studies
funnel(taf)

# fit selection model (based on an equal-effects model and assuming selection
# in favor of significant negative effects)
res <- rma(yi, vi, method="EE", data=dat)
sel <- selmodel(res, type="logistic", alternative="less")
sel
plot(sel)

# PET/PEESE methods
rma(yi, vi, mods = ~ sei, data=dat)
rma(yi, vi, mods = ~ vi,  data=dat)

# or use regtest()
res <- rma(yi, vi, data=dat)
regtest(res)
regtest(res, predictor="vi")

# show funnel plot with PET/PEESE results
sav <- funnel(res, ylim=c(0,1.2))
reg <- regtest(res)
se <- seq(0, 1.3, length=100)
lines(coef(reg$fit)[1] + coef(reg$fit)[2]*se, se, lwd=3, col="blue")
reg <- regtest(res, predictor="vi")
lines(coef(reg$fit)[1] + coef(reg$fit)[2]*se^2, se, lwd=3, col="red")
legend("topright", inset=.02, legend=c("PET","PEESE"), col=c("blue","red"), lwd=3, bg="white")
points(sav$x, sav$y, pch=19)

# test of excess significance
tes(yi, vi, data=dat)

############################################################################

# funnel plot variations

# compute the total sample sizes of the studies
dat$ntotali <- dat$n1i + dat$n2i

# supply this information to rma() via 'ni' argument
res <- rma(yi, vi, ni=ntotali, method="EE", data=dat)

# four examples of other funnel plot types
par(mfrow=c(2,2))
funnel(res, xlim=c(-3.5,3), ylim=c(0,1.2))
funnel(res, xlim=c(-3.5,3), ylim=c(0,1.2), refline=0)
funnel(res, xlim=c(-3.5,3), ylim=c(0,.15), yaxis="sqrtninv")
funnel(res, xlim=c(-3.5,3), ylim=c(0,1.2), refline=0,
       level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), legend=TRUE)

############################################################################
