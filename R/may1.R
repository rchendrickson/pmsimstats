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



#' #' Generate simulated data
#' #'
#' #' \code{generateData} generates a set of simulated clinical trials' data
#' #'
#' #' This is the core data simulation code. See vignettes for additional details.
#' #' This function is primarily called by \link{generateSimulatedResults}.
#' #'
#' #' @param modelparam a datatable with a single entry per named column:
#' #' \itemize{
#' #'  \item{\code{N}}{  Number of simulated participants}
#' #'  \item{\code{c.bm}}{  Correlation between the biomarker and the biologic response}
#' #'  \item{\code{carryover_t1half}}{  Halflife of the carryover effect}
#' #'  \item{\code{c.tv}}{  Autocorrelation for the tv factor across timepoints}
#' #'  \item{\code{c.pb}}{ Autocorrelation for the pb factor across timepoints}
#' #'  \item{\code{c.br}}{  Autocorrelation for the br factor across timepoints}
#' #'  \item{\code{c.cf1t}}{  Correlation between different factors at a single timepoint}
#' #'  \item{\code{c.cfct}}{  Correlation between different factors at different timepoints}
#' #' }
#' #' @param respparam Data table with 5 columns and 3 rows. The first row lists the 3 factors
#' #'   defining the response trajectories (tv, pb, and br). The rest of the columns are the
#' #'   input parameters for the \link{modgompertz} function:
#' #' \itemize{
#' #'  \item{\code{max}}{  Maximum response value}
#' #'  \item{\code{disp}}{  Displacement}
#' #'  \item{\code{rate}}{  Rate}
#' #'  \item{\code{sd}}{  Standard deviation}
#' #' }
#' #' @param blparam Data table with 3 colums and 3 rows. First row lists the varaibles being
#' #'   defined, which consist of \code{BL} for the baseline value of the outcome measure, and
#' #'   \code{bm} for the biomarker values. Remaining columns:
#' #'   \itemize{
#' #'     \item{\code{m}}{  Mean}
#' #'     \item{\code{sd}}{  Standard deviation}
#' #'   }
#' #' @param trialdesign The information defining a single path through the clinical trial
#' #'   design, as defined by the \code{output$trialpaths} output of \link{buildtrialdesign}
#' #' @param empirical Should the correlations be empirical? (Usually \code{FALSE} unless
#' #'   you're doing something odd for testing purposes)
#' #' @param makePositiveDefinite Should the covariance matrix be forced to be positive definite?
#' #'   (Usually yes, although you may want to turn this off at times to test the impact of
#' #'   this step on your covariance sturcture)
#' #' @param seed Randomization seed (defaults to NA)
#' #' @returns A \code{dat} file that contains both the total symptom scores at each timepoint
#' #'   and also all the individual factors that were used to generate those total scores
#' #' @examples
#' #'   #See vignettes for examples of how to use this
#' #' @export
#'
#'
#' generateData<-function(modelparam,respparam,blparam,trialdesign,empirical,makePositiveDefinite,seed=NA,scalefactor=3,verbose=FALSE){
#'
#'   # I. Turn the trial design information into something easier to use
#'   d<-data.table(trialdesign)
#'   d$t_wk_cumulative<-cumulative(d$t_wk)
#'   d[,onDrug:=(tod>0)]
#'   nP<-dim(trialdesign)[1]
#'
#'   # II. Now set up a whole host of variables that we're going to want to track - essentially our baseline
#'   #     parameters for each subject ("bm","BL"), and the three modeled factors for each stage of the trial.
#'
#'   # Set up the variable names
#'   cl<-c("tv","pb","br")
#'   labels<-c(c("bm","BL"),
#'             paste(trialdesign$timeptname,cl[1],sep="."),
#'             paste(trialdesign$timeptname,cl[2],sep="."),
#'             paste(trialdesign$timeptname,cl[3],sep="."))
#'
#'   # Set up vectors with the standard deviations and means
#'   sds<-c(blparam[cat=="bm"]$sd,blparam[cat=="BL"]$sd)
#'   sds<-c(sds,rep(respparam[cat=="tv"]$sd,nP))
#'   sds<-c(sds,rep(respparam[cat=="pb"]$sd,nP)*trialdesign$e)
#'   sds<-c(sds,rep(respparam[cat=="br"]$sd,nP))
#'   means<-c(blparam[cat=="bm"]$m,blparam[cat=="BL"]$m)
#'   for (c in cl){
#'     rp<-respparam[cat==c]
#'     if(c=="tv"){
#'       means<-c(means,modgompertz(d$t_wk_cumulative,rp$max,rp$disp,rp$rate))
#'     }
#'     if(c=="pb"){
#'       means<-c(means,modgompertz(d$tpb,rp$max,rp$disp,rp$rate)*trialdesign$e)
#'     }
#'     if(c=="br"){
#'       brmeans<-modgompertz(d$tod,rp$max,rp$disp,rp$rate)
#'       ## Code added in by Ron Thomas:
#'       brtest <- brmeans == 0
#'       rawbrmeans <- brmeans
#'       names(brtest) <- labels[19:26]
#'       names(rawbrmeans) <- labels[19:26]
#'       ## End new code
#'       if(nP>1){
#'         for(p in 2:nP){
#'           if(!d[p]$onDrug){
#'             if(d[p]$tsd>0){
#'               brmeans[p]<-brmeans[p]+brmeans[p-1]*(1/2)^(scalefactor * d$tsd[p]/modelparam$carryover_t1half)
#'             }
#'           }
#'         }
#'       }
#'       means<-c(means,brmeans)
#'     }
#'   }
#'
#'   # Turn this into a matrix of correlations
#'   correlations<-diag(length(labels))
#'   rownames(correlations)<-labels
#'   colnames(correlations)<-labels
#'   # Give some output if in verbose mode:
#'   if(verbose==TRUE){
#'     aa <- data.frame(modelparam$carryover_t1half, rawbrmeans, brmeans, diff = brmeans - rawbrmeans)
#'     cat("brmeans before and after adj:\n ")
#'     print(aa)
#'   }
#'   for(c in cl){
#'     # build in the autocorrlations across time
#'     if(nP>1){
#'       ac<-modelparam[(paste("c",c,sep="."))][,]
#'       for(p in 1:(nP-1)){
#'         for(p2 in (1+p):nP){
#'           n1<-paste(trialdesign$timeptname[p],c,sep=".")
#'           n2<-paste(trialdesign$timeptname[p2],c,sep=".")
#'           correlations[n1,n2]<-ac
#'           correlations[n2,n1]<-ac
#'         }
#'       }
#'     }
#'     # build in the autocorrelations across factors
#'     for(c2 in setdiff(cl,c)){
#'       for(p in 1:nP){
#'         n1<-paste(trialdesign$timeptname[p],c,sep=".")
#'         n2<-paste(trialdesign$timeptname[p],c2,sep=".")
#'         correlations[n1,n2]<-modelparam$c.cf1t
#'         correlations[n1,n2]<-modelparam$c.cf1t
#'       }
#'       for(p in 1:(nP-1)){
#'         for(p2 in (1+p):nP){
#'           n1<-paste(trialdesign$timeptname[p],c,sep=".")
#'           n2<-paste(trialdesign$timeptname[p2],c2,sep=".")
#'           correlations[n1,n2]<-modelparam$c.cfct
#'           correlations[n2,n1]<-modelparam$c.cfct
#'         }
#'       }
#'     }
#'     # correlation with biomarker
#'     for(p in 1:nP){
#'       n1<-paste(trialdesign$timeptname[p],"br",sep=".")
#'       ## RON THOMAS VERSION:
#'       if (p > 1) {
#'         n0 <- paste(trialdesign$timeptname[p - 1], "br", sep = ".")
#'         mm1 <- means[which(n1 == labels)]
#'         mm0 <- means[which(n0 == labels)]
#'         correlations["bm", n1] <- correlations[n1, "bm"] <- ifelse(brtest[p], ifelse(brmeans[p] == 0, 0, (mm1 / mm0) * modelparam$c.bm), modelparam$c.bm)
#'       }
#'       ##
#'       ## Following commented out in Ron Thomas version:
#'       #if(means[which(n1==labels)]!=0){
#'       #  correlations[n1,'bm']<-modelparam$c.bm
#'       #  correlations['bm',n1]<-modelparam$c.bm
#'       #}
#'       ## End commented out by Ron
#'     }
#'   }
#'   # Again, some output if in verbose mode:
#'   if(verbose==TRUE){
#'     cat("carryover: ")
#'     print(modelparam$carryover_t1half)
#'     cat("br correlations: \n")
#'     print(correlations[19:26, 1])
#'   }
#'   # Turn correlation matrix into covariance matrix
#'   sigma<-outer(sds,sds)*correlations
#'   #  should be faster: outer(S,S)*R where S is SDs and R is correlation matrix
#'   #  previous: sigma<-correlations*sds%o%sds
#'
#'   # If turned on, force to be positive definite:
#'   if(makePositiveDefinite){
#'     if(!is.positive.definite(sigma)){
#'       sigma<-make.positive.definite(sigma, tol=1e-3)
#'     }
#'   }
#'
#'   # Set seed:
#'   #if(is.na(seed)){seed<-round(rnorm(1,10000,10000))}
#'   #set.seed(seed)
#'
#'   # Make the componant data!
#'   dat<-mvrnorm(n=modelparam$N,mu=means,Sigma=sigma,empirical=empirical)
#'   dat<-data.table(dat)
#'   setnames(dat,names(dat),labels)
#'
#'   # TESTING
#'   #tdat1<-dat[, lapply(.SD, mean)]
#'   #plot(c(d$t_wk_cumulative),tdat1[,.(OL.br,BD.br,UD.br,COd.br,COp.br)])
#'
#'
#'   #tdat2<-dat[, lapply(.SD, mean)]
#'   #plot(c(d$t_wk_cumulative),tdat2[,.(OL.br,BD.br,UD.br,COd.br,COp.br)])
#'
#'   # Turn it into actual fake data... calc intervals separately, because will use
#'   dat[,ptID:=1:modelparam$N]
#'   for(p in 1:nP){
#'     n<-trialdesign$timeptname[p]
#'     evalstring<-paste(
#'       "dat[,D_",n,":=sum(",paste(paste(n,cl,sep='.',collapse=','),sep=','),"),by='ptID']",
#'       sep="")
#'     eval(parse(text=evalstring))
#'   }
#'   for(p in 1:nP){
#'     n<-trialdesign$timeptname[p]
#'     evalstring<-paste(
#'       "dat[,",n,":=sum(BL,-",paste(paste(n,cl,sep='.',collapse=',-'),sep=','),"),by='ptID']",
#'       sep="")
#'     eval(parse(text=evalstring))
#'   }
#'
#'   # TESTING
#'   #tdat3<-dat[, lapply(.SD, mean)]
#'   #plot(c(0,d$t_wk_cumulative),tdat3[,.(BL,OL,BD,UD,COd,COp)])
#'   #plot(c(d$t_wk_cumulative),tdat3[,.(D_OL,D_BD,D_UD,D_COd,D_COp)])
#'
#'   return(dat)
#' }
#'


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

