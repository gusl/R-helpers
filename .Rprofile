#################
##  .Rprofile  ##  Place this file on your home directory.
#################

rm(list=ls())

R_HELPERS_PATH <- "~/projects/R-helpers"

helperFiles <- list.files(pattern=".R$", R_HELPERS_PATH, full.names=TRUE)

is.error <- function(e) class(e)=="try-error"

cat("Loading helper files:\n")
for (file in helperFiles) {
  cat(" *", file,"\n")
  e <- try(source(file))
  if (is.error(e)) cat("\nContinuing to load helper files:\n")
}

cat("\n        ###############################")
cat("\n        ### Welcome to the R shell! ###")
cat("\n        ############################### \n\n")
