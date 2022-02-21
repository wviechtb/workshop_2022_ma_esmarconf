############################################################################

# note: for this workshop, a current version of R should be installed (the
# most recent version is R 4.1.2); you will see the version installed when you
# start up R / RStudio; if it is older (especially older than version 4.0.0),
# then you should upgrade R (i.e., go to https://cran.r-project.org/ and
# download/install the current version)

# standard way to install the metafor package from CRAN
install.packages("metafor")

# for this workshop, please install the 'development' versions of the metadat
# and metafor packages (for this, need to install the remotes package first)
install.packages("remotes")
remotes::install_github("wviechtb/metadat")
remotes::install_github("wviechtb/metafor")

# note: if you run into problems with installing the development versions,
# then for the most part you should be fine using the current CRAN version of
# metafor throughout this workshop; in general, I cannot troubleshoot package
# installation issues, since this is very difficult to do remotely; the above
# is guaranteed to work under a standard setup, but problems can arise for
# example when your files / R library are stored on a network drive; googling
# for the error messages may give you clues how to resolve or work around the
# problem

# load the metafor package
library(metafor)

# if you do NOT get an error message here, then you should be fine

# please also install the numDeriv and clubSandwich packages
install.packages("numDeriv")
install.packages("clubSandwich")

############################################################################
