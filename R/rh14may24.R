# Testing pmsimstats script
#
#

# I. Setup

library(pmsimstats)
library(data.table)
library(lme4)
library(lmerTest)
library(corpcor)
library(ggplot2)
library(MASS)
library(svMisc)
library(tictoc)
library(ggpubr)
library(gridExtra)

library(devtools)
#install_github("rchendrickson/pmsimstats")

# II. Generate trial designs

tdNof11<-buildtrialdesign(
  name_longform="primary N-of-1 design",
  name_shortform="N-of-1",
  timepoints=c(4,8,9,10,11,12,16,20),
  timeptname=c('OL1','OL2','BD1','BD2','BD3','BD4','COd','COp'),
  expectancies=c(1,1,.5,.5,.5,.5,.5,.5),
  ondrug=list(
    # Assumes on drug entire previous interval and this measurement point
    pathA=c(1,1,1,1,0,0,1,0),
    pathB=c(1,1,1,1,0,0,0,1),
    pathC=c(1,1,1,0,0,0,1,0),
    pathD=c(1,1,1,0,0,0,0,1)
  )
)


tdOL<-buildtrialdesign(
  name_longform="open label",
  name_shortform="OL",
  timepoints=cumulative(rep(2.5,8)),
  timeptname=paste("OL",1:8,sep=""),
  expectancies=rep(1,8),
  ondrug=list(
    pathA=rep(1,8)
  )
)

tdOLBDC<-buildtrialdesign(
  name_longform="open label+blinded discontinuation",
  name_shortform="OL+BDC",
  timepoints=c(4,8,12,16,17,18,19,20),
  timeptname=c('OL1','OL2','OL3','OL4','BD1','BD2','BD3','BD4'),
  expectancies=c(1,1,1,1,.5,.5,.5,.5),
  ondrug=list(
    pathA=c(1,1,1,1,1,1,0,0),
    pathB=c(1,1,1,1,1,0,0,0)
  )
)

tdCO<-buildtrialdesign(
  name_longform="traditional crossover",
  name_shortform="CO",
  timepoints=cumulative(rep(2.5,8)),
  timeptname=c(paste("COa",1:4,sep=""),paste("COb",1:4,sep="")),
  expectancies=rep(.5,8),
  ondrug=list(
    # Assumes on drug entire previous interval and this measurement point
    pathA=c(1,1,1,1,0,0,0,0),
    pathB=c(0,0,0,0,1,1,1,1)
  )
)

tdPGRCT<-buildtrialdesign(
  name_longform="Parallel group RCT",
  name_shortform="PG-RCT",
  timepoints=cumulative(rep(2.5,8)),
  timeptname=paste("V",1:8,sep=""),
  expectancies=rep(.5,8),
  ondrug=list(
    pathA=rep(1,8),
    pathB=rep(0,8)
  )
)

trialdesigns<-list(PG=tdPGRCT,OL=tdOL,OLBDC=tdOLBDC,CO=tdCO,Nof1=tdNof11)

# III. Setup parameter spaces

data(extracted_bp)
TRblparam<-list(
  name="TR",
  verbaldesc="Extracted from data with blank slate assumptions",
  param=extracted_bp
)
altblparam1<-list(
  name="LowerSDs",
  verbaldesc="Lower SD for the BL",
  param=data.table(
    cat=c("BL","bm"),
    m=extracted_bp$m,
    sd=c(10,15.36)
  )
)

blparamsets<-list(TRblparam,altblparam1)

# IV. Setup response parameters

data(extracted_rp)

TRrespparam<-list(
  name="TR",
  verbaldesc="Extracted from data with blank slate assumptions",
  param=extracted_rp
)

# Create some alternative response parameters:
altrespparam1<-list(
  name="FastPbBrSlowTv",
  verbaldesc="Faster Pb & Br, slower tv, all else same",
  param=data.table(
    cat=c("tv","pb","br"),
    max=extracted_rp$max,
    disp=extracted_rp$disp,
    rate=c(.5,.2,.2),
    sd=extracted_rp$sd
  )
)

altrespparam2<-list(
  name="LargeTVsmallPb",
  verbaldesc="Large TV smaller Pb, all else same",
  param=data.table(
    cat=c("tv","pb","br"),
    max=c(9,4,11),
    disp=extracted_rp$disp,
    rate=extracted_rp$rate,
    sd=extracted_rp$sd
  )
)

origrespparamsets<-list(TRrespparam,altrespparam1,altrespparam2)

# Other model params

censorparams<-data.table(
  names=c("balanced","more of flat","more of biased","high dropout"),
  beta0=c(.05,.05 ,.05,.15),
  beta1=c(.5,.2,.8 ,.5),
  eb1=  c(2,  2  ,2  ,2 )
)
coremodelparams<-expand.grid(
  N=c(35,70),
  c.bm=c(0,.3,.6),
  carryover_t1half=c(0,1,2),
  c.tv=.8,c.pb=.8,c.br=.8,
  c.cf1t=.2,c.cfct=.1
)
abbrevmodelparams<-expand.grid(
  N=c(70),
  c.bm=c(0,.3),
  carryover_t1half=c(0),
  c.tv=.8,c.pb=.8,c.br=.8,
  c.cf1t=.2,c.cfct=.1
)
xabbrevmodelparams<-expand.grid(
  N=c(35),
  c.bm=c(.6),
  carryover_t1half=c(0),
  c.tv=.8,c.pb=.8,c.br=.8,
  c.cf1t=.2,c.cfct=.1
)
trajmp<-expand.grid(
  N=c(35),
  c.bm=c(.6),
  carryover_t1half=c(0,.5),
  c.tv=.8,c.pb=.8,c.br=.8,
  c.cf1t=.2,c.cfct=.1
)
coremodelparams2<-expand.grid(
  N=c(35,70),
  c.bm=c(.6),
  carryover_t1half=c(0,1,2),
  c.tv=.8,c.pb=.8,c.br=.8,
  c.cf1t=.2,c.cfct=.1
)

# V. Parameters on the analysis side
analysisparamsCOY<-expand.grid(useDE=FALSE,
                            t_random_slope=FALSE,
                            full_model_out=FALSE,
                            simplecarryover=TRUE,
                            carryover_t1half=1)

analysisparamsCON<-expand.grid(useDE=FALSE,
                               t_random_slope=FALSE,
                               full_model_out=FALSE,
                               simplecarryover=FALSE,
                               carryover_t1half=0)


###### Simple version to generateData for testing:
#  Nreps<-5
mp=coremodelparams2[3,]
mp$N=35
dat=generateData(modelparam=mp,
                 respparam=origrespparamsets[[1]]$param,
                 blparam=blparamsets[[1]]$param,
                 trialdesign=trialdesigns$Nof1[[2]][[1]],
                 empirical=FALSE,makePositiveDefinite=TRUE,seed=NA,scalefactor=2,verbose=FALSE)
td<-trialdesigns$Nof1[[2]]

# If need to step into lme_analysis
#datSingle=dat
#trialdesign=trialdesigns$Nof1[[2]][[1]]

####
# Ok, that worked - this part is focused on figuring out the code to extract a model of carryover from either the actual or simulated data

# Start by creating data (d) and a path (p)
d=dat[,.(PtID=ptID,BL,OL1,OL2,BD1,BD2,BD3,BD4,COd,COp)]
   # Make it analogous to what you'd get out of an actual trial, but without the bm unblinded
p=trialdesigns$Nof1[[2]][[1]]
p$t_wk_cumulative<-cumulative(p$t_wk)

# Now we want to extract a long-form data set ready for a regression mode, which contains:
# id.var: PtID, tsd, Delta(outcome)_tsd (ie, change in your outcome measure since you were last on drug)
# To avoid doublecounting data points, will only use each "off drug" point once

# This is not efficient but I'm more concerned about making a coding error if I try to be fancy!
tsdpos_timepoints=p[tsd>0]$timeptnames
ondrug_timepoints=p[tod>0]$timeptnames
lfd=data.table(PtID=as.character(),tsd=as.numeric(),deltaOutcome_tsd=as.numeric())
  # lfd="long form data"
for(iTSDtp in 1:length(tsdpos_timepoints)){
  thistp=tsdpos_timepoints[iTSDtp]
  lastondrugtp_options=p[timeptnames%in%ondrug_timepoints,.(timeptnames,t_wk_cumulative,deltaT_thistimepoint=(-t_wk_cumulative+p[timeptnames==thistp]$t_wk_cumulative))]
  lastondrugtp_options=lastondrugtp_options[deltaT_thistimepoint>0]
  lastondrugtp=lastondrugtp_options[deltaT_thistimepoint==min(lastondrugtp_options$deltaT_thistimepoint)]$timeptnames
  var2pull=c("PtID",lastondrugtp,thistp)
  data2use=d[,((var2pull)),with=FALSE]
  setnames(data2use,var2pull,c("PtID","lastondrugtp","thistp"))
  data2use[,deltaOutcome_tsd:=(lastondrugtp-thistp)] # negative values for deltaOutcome_tsd mean Sx are increasing, c/w I think expectation from the name
  thistsd=p[timeptnames==thistp]$tsd
  data2use[,tsd:=thistsd]
  lfd=rbind(lfd,data2use[,.(PtID,tsd,deltaOutcome_tsd)])
}
stop()
############################# THIS IS WHERE I'm STUCK - THIS IS SCRATCH FROM ME TRYING TO FIGURE IT OUT
# using eg this: https://rdrr.io/cran/lme4/man/nlmer.html

# Try to use lme4 to fit:
# step 1
startvec=c(f0=mean(lfd$deltaOutcome_tsd,f1=1,t1half=1))
nm1= nlmer(deltaOutcome_tsd~SSlogis(tsd) ~1|PtID, start=startvec)

nform= deltaOutcome_tsd~f0+f1*(1/2)^(deltaOutcome_tsd/t1half)
nfun= deriv(nform,namevec=c("f0","f1","t1half"),
            function.arg=c("input","f0","f1","t1half"))
nm1b= update(nm1,)



startvec=c()

nm1 <- nlmer(deltaOutcome_tsd~SSlogis())

# Use nlme to fit:
m1 <- nlme(deltaOutcome_tsd~f0+f1*(1/2)^(deltaOutcome_tsd/t1half),
           data=lfd,
           fixed=f0+f1+t1half ~ 1,
           random=f0~1|PtID)

#
#####
