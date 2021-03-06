# Scratch Script space for doing the development work on the package.

#devtools::install_github("r-lib/devtools")
#install_github("rchendrickson/pmsimstats")

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
#use_vignette("Produce_Publication_Results_3_apply_to_actual_clinical_trial_data")

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

# See if can apply our lme_analysis to the clinical trial results??? What happens if include
# the placebo group vs not...??

# rddir<-"C:\\Users\\afane\\Dropbox\\Misc\\DataSave\\Data\\CAPSitems"
# hddir<-getwd()
# setwd(rddir)
# dt.s<-as.data.table(readRDS("FreshPullY13AllTimepoints_scoresWithCovar.rds"))
# # s for scores
# dt.d<-readRDS("FreshPullY13AllTimepoints_diffs.rds")
# # d for differences - gives deltaCAPS for each time interval ("timeptDiff")
# # timepts are: weeks 0, 7, 11, 15
# dto<-readRDS("FreshPullY13AllTimepoints_SummaryCalcs.rds")
# # this version goes item by item and gives the mean rating at baseline and at end of trial for eachgroup
# # Data (produced by createDataSetForY13data_06.R)
#
# # Get BP and total CAPS into useable format
# dt.s[,CAPSToti:=sum(B1,B2,B3,B4,B5,C1,C2,C3,C4,C5,C6,C7,D1,D2,D3,D4,D5),by=c("SubjID","timept")]
# dt.s[,BP:=as.numeric(strsplit(ScrBpStan,"/")[[1]][1]),by=c("SubjID","timept")]
#
# # Make match dat file: names are...
# # bm, BL, ptID, (timept names), path, replicate
# rdat<-dt.s[,.(bm=BP,ptID=SubjID,StudyDrug,replicate=1,timept,path=1,CAPSToti)]
# rdat[StudyDrug=="placebo"]$path<-2
# rdatw<-dcast(rdat,bm+ptID+StudyDrug+replicate+path~timept,value.var="CAPSToti")
# setnames(rdatw,c("1","2","3","4"),c("BL","T1","T2","T3"))
# dattest<-rdatw[,.(bm,BL,ptID=1:.N,T1,T2,T3,path)]
#
# The bm of 76 turns out to be an error where the SBP is missing:
# dattest<-dattest[bm>80]
#
# datasavedir<-"C://Users//afane//Documents//GitHub//pmsimstats//data//"
# setwd(datasavedir)
# CTdata<-dattest
# save(CTdata,file="CTdata.rda")
#
# simresults<-generateSimulatedResults(
#   trialdesigns=list(trialdesigns$OL,trialdesigns$PGRCT),
#   respparamsets=origrespparamsets[1],
#   blparamsets=blparamsets[1],
#   censorparams=censorparams,
#   modelparams=coremodelparams[1,],
#   simparam=list(Nreps=1,
#                 progressiveSave=FALSE,
#                 basesavename="TEST",
#                 nRep2save=5,
#                 saveunit2start=1,
#                 savedir=getwd()),
#   analysisparams=analysisparams,
#   rawdataout=TRUE)
#
