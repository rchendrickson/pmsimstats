# Scratch Script space for doing the development work on the package.

#devtools::install_github("r-lib/devtools")

# # Get tools on board
# library("devtools")
# library("roxygen2")
# library("testthat")
# library("knitr")
#
# # Stuff we have to figure out how to handle in terms of namespace...
# library(data.table)
# library(lme4)
# library(lmerTest)
# library(corpcor)
# library(ggplot2)
# library(MASS)
# library(svMisc)
# library(tictoc)
# library(ggpubr)
# library(gridExtra)

# Check, because SO many problems with installation...
#library(devtools)
#has_devel()

# Build an R.buildignore file:
#use_build_ignore("scratch_WorkingScript")

# Start the vingette:
#use_vignette("Produce_Publication_Results")
#use_vignette("Produce_Publication_Results_2_Look_at_data")

#
# # Create the data files for the plotting results side:
# mdir<-"C:\\Users\\afane\\Documents\\GitHub\\pmsimstats"
#
# basesavenames_basicpowersets<-c("2020_02_17_core1",
#                                 "2020_02_17_core2",
#                                 "2020_02_17_core3",
#                                 "2020_02_17_core4",
#                                 "2020_02_17_core5",
#                                 "2020_02_18_core1",
#                                 "2020_02_18_core2",
#                                 "2020_02_18_core3",
#                                 "2020_02_18_core4",
#                                 "2020_02_18_core5")
# basesavenames_respparamsetsRates<-c("2020_02_19_respRates1",
#                                     "2020_02_19_respRates2",
#                                     "2020_02_19_respRates3",
#                                     "2020_02_19_respRates4",
#                                     "2020_02_19_respRates5",
#                                     "2020_02_19_respRates6",
#                                     "2020_02_19_respRates7",
#                                     "2020_02_19_respRates8",
#                                     "2020_02_19_respRates9",
#                                     "2020_02_19_respRates10")
# basesavenames_respparamsetsMaxes<-c("2020_02_18_respMaxes1",
#                                     "2020_02_18_respMaxes2",
#                                     "2020_02_18_respMaxes3",
#                                     "2020_02_18_respMaxes4",
#                                     "2020_02_18_respMaxes5")

# Our core parameters
# setwd(wdir)
# simresults<-reknitsimresults(basesavenames_basicpowersets)
# setwd(mdir)
# save(results_core, file="data/results_core.rda")
# setwd(wdir)
# simresults<-reknitsimresults(basesavenames_respparamsetsRates)
# setwd(mdir)
# save(results_rates, file="data/results_rates.rda")
# setwd(wdir)
# simresults<-reknitsimresults(basesavenames_respparamsetsMaxes)
# setwd(mdir)
# save(results_maxes, file="data/results_maxes.rda")
# setwd(wdir)
# simresults<-readRDS("LME_compbaseparam_2020_02_08v2_500rep.rds") # 1.5Gigs!
# setwd(mdir)
# # TOOOOO big!!! Cut down to what we acutally need, minimally...
# simresults$rawdata<-simresults$rawdata$precensor # cuts down to 950 MB
# dim(simresults$results)
# length(simresults$rawdata)
# sr<-simresults$results[censorparamset==0]
# dim(sr) # now indexed the same
# ikeep<-sr[respparamset==1][blparamset==1][N==75][c.bm==.6][c.cf1t==.2]$irep
#
#setwd(mdir)
#save(results_trajectories, file="data/results_trajectories.rda")
