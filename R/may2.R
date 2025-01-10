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






## ----SetupTrialDesigns-----------------------------------------------------------------------------------------------------------------------------------------------------------------

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

# trialdesigns<-list(OL=tdOL,Nof1=tdNof11)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(extracted_bp)
extracted_bp


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
TRblparam<-list(
  name="TR",
  verbaldesc="Extracted from data with blank slate assumptions",
  param=extracted_bp
)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(extracted_rp)
extracted_rp


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
TRrespparam<-list(
  name="TR",
  verbaldesc="Extracted from data with blank slate assumptions",
  param=extracted_rp
)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create some alternative response parameters:


origrespparamsets<-list(TRrespparam)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1) ridiculous way to make it a data table
TRlongform<-as.data.table(melt(TRrespparam$param))
TRlongform[,rp:=paste(cat,variable,sep="_")]
TRgrid<-TRlongform$value
TRgrid<-data.table(rbind(TRgrid,TRgrid,TRgrid,TRgrid,TRgrid))
setnames(TRgrid,names(TRgrid),TRlongform$rp)

#2) fill it out:
maxesgrid<-data.table(
  tv_max=c(18,3,3,8),
  pb_max=c(3,18,3,8),
  br_max=c(3,3,18,8)
)
rates<-c(.2,.35,.5)
ratesgrid<-expand.grid(
  tv_rate=rates,
  pb_rate=rates,
  br_rate=rates
)

respparamsets<-vector(mode="list",length=1+dim(maxesgrid)[1]+dim(ratesgrid)[1])
respparamsets[[1]]<-TRrespparam
for(irp in 1:dim(maxesgrid)[1]){
  op<-maxesgrid[irp,]
  newrp<-list(
    name=paste("MXtv",op$tv_max,"pb",op$pb_max,"br",op$br_max,sep=""),
    verbaldesc=paste("MXtv",op$tv_max,"pb",op$pb_max,"br",op$br_max,sep=""),
    param=data.table(
      cat=c("tv","pb","br"),
      max=as.numeric(op),
      disp=extracted_rp$disp,
      rate=extracted_rp$rate,
      sd=as.numeric(op)
    )
  )
  respparamsets[[irp+1]]<-newrp
}
for(irp in 1:dim(ratesgrid)[1]){
  op<-ratesgrid[irp,]
  newrp<-list(
    name=paste("RTtv",op$tv_max,"pb",op$pb_max,"br",op$br_max,sep=""),
    verbaldesc=paste("RTtv",op$tv_max,"pb",op$pb_max,"br",op$br_max,sep=""),
    param=data.table(
      cat=c("tv","pb","br"),
      max=extracted_rp$max,
      disp=extracted_rp$disp,
      rate=as.numeric(op),
      sd=extracted_rp$sd
    )
  )
  respparamsets[[irp+1+dim(maxesgrid)[1]]]<-newrp
}

# One more version, to focus further on maxes:
TRlongform<-as.data.table(melt(TRrespparam$param))
TRlongform[,rp:=paste(cat,variable,sep="_")]
TRgrid<-TRlongform$value
TRgrid<-data.table(rbind(TRgrid,TRgrid,TRgrid,TRgrid,TRgrid))
setnames(TRgrid,names(TRgrid),TRlongform$rp)
maxes<-c(5,8,11)
maxesgrid<-expand.grid(
  tv_max=maxes,
  pb_max=maxes,
  br_max=maxes
)
respparamsetsm<-vector(mode="list",length=dim(maxesgrid)[1])
for(irp in 1:dim(maxesgrid)[1]){
  op<-maxesgrid[irp,]
  newrp<-list(
    name=paste("MXtv",op$tv_max,"pb",op$pb_max,"br",op$br_max,sep=""),
    verbaldesc=paste("MXtv",op$tv_max,"pb",op$pb_max,"br",op$br_max,sep=""),
    param=data.table(
      cat=c("tv","pb","br"),
      max=as.numeric(op),
      disp=extracted_rp$disp,
      rate=extracted_rp$rate,
      sd=as.numeric(op)
    )
  )
  respparamsetsm[[irp]]<-newrp
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
censorparams<-data.table(
  names=c("balanced","more of flat","more of biased","high dropout"),
  beta0=c(.05,.05 ,.05,.15),
  beta1=c(.5,.2,.8 ,.5),
  eb1=  c(2,  2  ,2  ,2 )
)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# coremodelparams<-expand.grid(
#   N=c(70),
#   c.bm=c(0),
#   carryover_t1half=c(.2),
#   c.tv=.8,c.pb=.8,c.br=.8,
#   c.cf1t=.2,c.cfct=.1
# )

coremodelparams<-expand.grid(
  N=c(35,70),
  c.bm=c(0,.3,.6),
  carryover_t1half=c(0,.5,1),
  c.tv=.8,c.pb=.8,c.br=.8,
  c.cf1t=.2,c.cfct=.1
)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Parameters on the analysis side
analysisparams<-expand.grid(useDE=FALSE,t_random_slope=FALSE,full_model_out=FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set the number of reptitions. Was 1,000 in the publication
Nreps<-5

# Flag to keep this code from actually running for now - flip to try it
rerun_simulations<-TRUE

if(rerun_simulations){
  # This block runs the paramater sets that explore the impact of N, biomarker effect size,
  # and carryover
  simresults<-generateSimulatedResults(
    trialdesigns=list(trialdesigns$Nof1,trialdesigns$OL,trialdesigns$OLBDC, trialdesigns$PG, trialdesigns$CO),
    respparamsets=origrespparamsets[1],
    blparamsets=blparamsets[1],
    censorparams=censorparams,
    modelparams=coremodelparams,
    simparam=list(Nreps=Nreps,
                  progressiveSave=TRUE,
                  basesavename="coreresults",
                  nRep2save=5,
                  saveunit2start=1,
                  savedir=getwd()),
    analysisparams=analysisparams,
    rawdataout=FALSE)

}
# Our core parameters
# filename <- file.choose()
 core4 = reknitsimresults("coreresults")
 simresults <- core4
 save(core4, file = "core4_1000reps_2.RData")
 # load("core3_1000reps.RData")
 # data(coreresults_save1)
 # simresults<-results_core
 param2vary<-c("trialdesign","N","c.bm","censorparamset")
 param2hold<-data.table(blparamset=1,respparamset=1,carryover_t1half=0)
 param2nothold<-c("c.cfct","modelparamset")
 param2plot<-"power" # Options: power, mean_frac_NA, mean_beta, mean_betaSE, sd_beta...
 p1<-PlotModelingResults(simresults,param2plot,param2vary,param2hold,param2nothold)

param2vary<-c("trialdesign","N","carryover_t1half","censorparamset")
param2hold<-data.table(blparamset=1,respparamset=1,c.bm=.6)
param2nothold<-c("c.cfct","modelparamset")
param2plot<-"power" # Options: power, mean_frac_NA, mean_beta, mean_betaSE, sd_beta...
p2<-PlotModelingResults(simresults,param2plot,param2vary,param2hold,param2nothold)

p1<-p1+ggtitle("A. Effect of trial design, censoring and biomarker effect on power")+
   theme(plot.title=element_text(hjust=0))
p2<-p2+ggtitle("B. Effect of nonzero carryover term on power")+
  theme(plot.title=element_text(hjust=0))

lay <- rbind(c(1),
             c(2))
plots<-list(p1,p2)
grid.arrange(grobs = plots, layout_matrix = lay)
#ml <- arrangeGrob(grobs = plots, layout_matrix = lay)
#setwd(sourcedir)
#ggsave("Fig4_CorePowerResults.pdf",ml,width=9,height=9,units="in",dp=600)

