############################################################################

# note: for this workshop, a current version of R should be installed (the
# most recent version is R 4.2.0); you will see the version installed when you
# start up R / RStudio; if it is older (especially older than version 4.0.0),
# then you should upgrade R (i.e., go to https://cran.r-project.org/ and
# download/install the current version of R)

# install the metadat and metafor packages
install.packages("metadat")
install.packages("metafor")

# also install the numDeriv and clubSandwich packages
install.packages("numDeriv")
install.packages("clubSandwich")

# load the metafor package (to check that this works; it should tell you that
# version 3.4-0 of the metafor package was loaded)
library(metafor)

############################################################################
