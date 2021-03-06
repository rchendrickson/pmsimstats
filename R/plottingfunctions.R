#' Plot factor trajectories
#'
#' \code{PlotModelingResults} plots basic output relating to the peformance of each
#' trial design across a parameter space
#'
#' @param data the \code{$results} component of the output of \link{generateSimulatedResults}
#' @param param2plot What value to plot as the heatmap out put. Can be power,
#'   mean_frac_NA, mean_beta, mean_betaSE, sd_beta
#' @param param2vary 4 params to vary and plot across the created space
#' @param param2hold Params to keep constant
#' @param param2nothold Things to let vary freely (if e.g. they are linked to another
#'   intentionally varied parameter)
#' @param labelcode A mapping between interal variable names and figure labels
#'
#' @return a ggplot object containing the plot
#' @examples
#' data(results_core)
#'   param2vary<-c("trialdesign","N","c.bm","censorparamset")
#'   param2hold<-data.table(blparamset=1,respparamset=1,carryover_t1half=0)
#'   param2nothold<-c("c.cfct","modelparamset")
#'   param2plot<-"power" # Options: power, mean_frac_NA, mean_beta, mean_betaSE, sd_beta...
#'   p1<-PlotModelingResults(simresults,param2plot,param2vary,param2hold,param2nothold)
#'  # See vignettes for additional details
#' @export

PlotModelingResults<-function(data,param2plot,param2vary,param2hold,param2nothold,labelcode){

  # This just for labeling the plot
  if(missing(labelcode)){
    labelcode<-data.table(handle=c("N","co","carryover","call","ctv","cpb","cbr",
                                   "mtv","mpb","mbr","stv","spb","sbr",
                                   "rtv","rpb","rbr","cbm","sbl","sbm",
                                   "trialdesignopt","beta0","beta1","eb1",
                                   "c.cf1t","carryover_t1half",
                                   "tv_rate","pb_rate","br_rate",
                                   "tv_max","pb_max","br_max",
                                   "c.bm","t_random_slope","use_DE"),
                          label=c("N subjects","Carryover","Carryover","Autocorrelations",
                                  "tv autocorrelation","pb autocorrelation","br autocorrelation",
                                  "tv mean","pb mean","br mean","tv SD","pb SD","br SD",
                                  "tv rate","pb rate","br rate","Correlation with biomarker",
                                  "SD of baseline CAPS","SD of SBP","Trial Design",
                                  "Baseline dropout (beta0)","Beta1","eb1",
                                  "cross-factor within timepoint autocorrelation","Carryover (t_1/2)",
                                  "TR rate","ER rate","BR rate",
                                  "TR max"," ER max","BR max",
                                  "Biomarker effect","Random Slope","Include expectancy in model"
                          ))
  }

  # Narrow the data down to what we will actually use
  d<-data$results

  # If we're breaking up respparams, do so now so can use same plotting machinery below
  if(length(intersect(param2vary,c("tv_rate","pb_rate","br_rate")))>1){
    # Build map from respparamste to rates
    cw<-data.table(respparamset=integer(),tv_rate=numeric(),pb_rate=numeric(),br_rate=numeric())
    for(irp in 1:length(data$parameterselections$respparamsets)){
      rps<-data$parameterselections$respparamsets[[irp]]$param
      cwn<-data.table(respparamset=irp,
                      tv_rate=rps[cat=="tv"]$rate,
                      pb_rate=rps[cat=="pb"]$rate,
                      br_rate=rps[cat=="br"]$rate)
      cw<-rbind(cw,cwn)
    }
    d<-merge(d,cw,by="respparamset",all.x=TRUE,all.y=FALSE)
  }
  if(length(intersect(param2vary,c("tv_max","pb_max","br_max")))>1){
    # Build map from respparamste to rates
    cw<-data.table(respparamset=integer(),tv_max=numeric(),pb_max=numeric(),br_max=numeric())
    for(irp in 1:length(data$parameterselections$respparamsets)){
      rps<-data$parameterselections$respparamsets[[irp]]$param
      cwn<-data.table(respparamset=irp,
                      tv_max=rps[cat=="tv"]$max,
                      pb_max=rps[cat=="pb"]$max,
                      br_max=rps[cat=="br"]$max)
      cw<-rbind(cw,cwn)
    }
    d<-merge(d,cw,by="respparamset",all.x=TRUE,all.y=FALSE)
  }

  # What didn't have a section made, but isn't part of what's being intentionally varied?
  otherholds<-setdiff(names(d),
                      c(param2hold,param2vary,param2nothold,"modelparamset",
                        "frac_NA","beta","betaSE","p","tID","irep","ETbeta","ETbetaSE",
                        "issingular"))
  for(iP in 1:length(param2hold)){
    n<-names(param2hold)[iP]
    d<-d[get(n)==param2hold[[n]]]
  }
  for(iP in 1:length(otherholds)){
    param2pick<-d[[otherholds[iP]]][1] # TAKING THE FIRST ONE
    d<-d[get(otherholds[iP])==param2pick]
  }

  # Format the parameters to vary and their labels. Hacky - could have designed more flexibly...
  op<-param2vary
  oplabels<-vector(mode="list",length=(length(op)))
  for(iop in 1:length(op)){
    if(op[iop]=="trialdesign"){
      axlabel<-"Trial design"
      acttds<-unique(d$trialdesign)
      bklabels<-vector(mode="character",length=length(acttds))
      for(iT in 1:length(bklabels)){
        itd<-acttds[iT]
        bklabels[iT]<-data$parameterselections$trialdesigns[[itd]]$metadata$name_shortform[[1]]
      }
    }else if(op[iop]=="respparamset"){
      axlabel<-"Response parameters"
      bklabels<-vector(mode="character",length=length(data$parameterselections$respparamsets))
      for(iT in 1:length(bklabels)){
        bklabels[iT]<-data$parameterselections$respparamsets[[iT]]$name
      }
    }else if(op[iop]=="blparamset"){
      axlabel<-"Baseline parameters"
      bklabels<-vector(mode="character",length=length(data$parameterselections$blparamsets))
      for(iT in 1:length(bklabels)){
        bklabels[iT]<-data$parameterselections$blparamsets[[iT]]$name
      }
    }else if(op[iop]=="censorparamset"){
      axlabel<-"Censoring parameters"
      bklabels<-c("No censoring",data$parameterselections$censorparams$names)
      d$censorparamset<-d$censorparamset+1
    }else if(op[iop]=="carryover_t1half"){
      axlabel<-"Carryover (in weeks)"
      bklabels<-unique(d[,get(op[iop])])
      conv<-data.table(carryover_t1half=bklabels,new=1:(length(bklabels)))
      d<-merge(d,conv,by="carryover_t1half")
      d[,carryover_t1half:=new]
      d[,new:=NULL]
    }else{
      axlabel<-merge(data.table(handle=op[iop]),labelcode,by="handle",all=FALSE)$label
      bklabels<-unique(d[,get(op[iop])])
      if(class(d[,get(op[iop])])=="logical"){
        evalstring<-paste("d$",op[iop],"<-as.integer(d[,get(op[iop])])",sep="")
        eval(parse(text=evalstring))
        }
    }
    oplabels[[iop]]<-list(axlabel=axlabel,bklabels=bklabels)
  }

  # Change the labels on the stuff that will be faceted
  for(iop in 1:2){
    d[[op[iop]]]<-as.factor(d[[op[iop]]])
    levels(d[[op[iop]]])<-oplabels[[iop]]$bklabels
  }

  d$tID<-1:dim(d)[1]
  d[,mean_beta:=mean(beta),by=op]
  d[,mean_betaSE:=mean(betaSE),by=op]
  d[,sd_beta:=sd(beta),by=op]
  d[,sig_p:=(p<.05),by=tID]
  d[,power:=mean(sig_p),by=op]
  d[,mean_frac_NA:=mean(frac_NA),by=op]
  d[,CV:=sd_beta/mean_beta,by=op]
  d[,fractionSingular:=sum(issingular==TRUE,na.rm=TRUE)/length(issingular),by=op]
  d[,fractionNA:=sum(is.na(issingular))/length(issingular),by=op]
  d[,fractionNotSingular:=sum(issingular==FALSE,na.rm=TRUE)/length(issingular),by=op]
  d[,toplot:=get(param2plot)]
  ds<-d
  ds[,c("tID","beta","betaSE","p","sig_p"):=NULL]
  ds<-unique(ds)
  palettevalues=c(0,.7,1)

  if(param2plot=="power"){
    legendtitle<-"Power"
  }else if(param2plot=="sd_beta"){
    legendtitle<-"SD of beta"
  }else if(param2plot=="mean_betaSE"){
    legendtitle<-"Mean SE of \ncoefficient"
  }else{
    legendtitle<-param2plot
  }
    # suppress the warnings thrown by the scale_y_continuous line:
  suppressWarnings(
    p<-ggplot(data=ds,aes_string(op[3],op[4])) +
      geom_tile(aes(fill=toplot),colour="white") +
      scale_fill_distiller(type="seq",palette="RdYlBu",values=palettevalues,legendtitle) +
      geom_text(aes(label=round(toplot,dig=2)),size=3,na.rm=TRUE) +
      scale_x_continuous(name=oplabels[[3]]$axlabel,breaks=unique(ds[,get(op[3])]),
                         labels=oplabels[[3]]$bklabels) +
      scale_y_continuous(name=oplabels[[4]]$axlabel,breaks=unique(ds[,get(op[4])]), # This line is where the warnings are coming from....??
                         labels=oplabels[[4]]$bklabels,
                         sec.axis=sec_axis(trans=~.,labels=NULL,breaks=NULL,
                                           name=oplabels[[2]]$axlabel)) +
      ggtitle(oplabels[[1]]$axlabel) +
      theme(plot.title = element_text(size=12,hjust=0.5))+
      facet_grid(reformulate(op[1], op[2]))
  )
  return(p)
}

#' Plot factor trajectories
#'
#' \code{plotfactortrajectories} plots the averaged output of the factors
#' used to generate simulated trial data
#'
#' @param data the \code{$rawdata$precensor} component of the output of \link{generateSimulatedResults}
#'   (run with rawdataout=TRUE)
#' @param tinfo the \code{$results} component of the output of \link{generateSimulatedResults}
#' @param tds the \code{$parameterselections$trialdesigns} component of the output of
#'   \link{generateSimulatedResults}
#' @param opt2plot a data.table with a single row using parameter names to instruct the
#'   function which parameters to hold constant and plot the results of
#' @param mergeacrossreps=TRUE logical input of whether to merge across repititions
#' @return a ggplot object containing the plot
#' @examples
#'   data(results_trajectories)
#'   data<-simresults$rawdata$precensor # this gives a list of length (total reps done)
#'   tinfo<-simresults$results # dim[1] of tinfo gives the parameter space that went in
#'   tds<-simresults$parameterselections$trialdesigns
#'   mergeacrossreps<-TRUE
#'   plots1<-vector(mode="list",length=length(simresults$parameterselections$trialdesigns))
#'   for(ip in 1:length(plots1)){
#'     param2hold<-data.table(carryover_t1half=0,trialdesign=ip)
#'     plots1[[ip]]<-plotfactortrajectories(data,tinfo,tds,param2hold,mergeacrossreps)
#'   }
#'   # See vignettes for additional details
#' @export
plotfactortrajectories<-function(data,tinfo,tds,opt2plot,mergeacrossreps=TRUE){

  # Pull out the trial design we're plotting
  if("trialdesign"%in%names(opt2plot)){
    td<-tds[[opt2plot$trialdesign]]
  }else{
    warning("You need to specify the trial design by index number")
  }

  # Turn tinfo into, essentially VPG from generateEsimulatedResults.R:
  tinfo<-unique(tinfo[,.(trialdesign,respparamset,blparamset,modelparamset,N,c.bm,
                       carryover_t1half,c.tv,c.pb,c.br,c.cf1t,c.cfct,censorparamset,
                       use_DE,t_random_slope)])
  # Since crossreferencing indexing, make it explicit:
  tinfo<-cbind(iR=1:(dim(tinfo)[1]),tinfo)
  # Only plot pre-censoring:
  tinfo<-tinfo[censorparamset==0]

  # Pull out the reps we're going to use
  # 1) What didn't have a section made, but isn't part of what's being intentionally varied?
  otherholds<-setdiff(names(tinfo),
                      c(names(opt2plot),"modelparamset","trialdesign",
                        "frac_NA","beta","betaSE","p","iR","irep",
                        "ETbea","ETbetaSE","issingular","warning"))
  keepR<-1:dim(tinfo)[1]
  for(iP in 1:length(param2hold)){
    n<-names(param2hold)[iP]
    kkeepR<-which(tinfo[[n]]==param2hold[[n]])
    keepR<-intersect(keepR,kkeepR)
  }
  for(iP in 1:length(otherholds)){
    param2pick<-tinfo[[otherholds[iP]]][1] # TAKING THE FIRST ONE
    kkeepR<-which(tinfo[[otherholds[iP]]]==param2pick)
    keepR<-intersect(keepR,kkeepR)
  }

  # keepR<-1:dim(tinfo)[1]
  # for(op in names(opt2plot)){
  #   kkeepR<-which(tinfo[[op]]==opt2plot[[op]])
  #   keepR<-intersect(keepR,kkeepR)
  # }

  # Compile the data we want to use and melt it for flexible plotting
  d<-data[[keepR[1]]][0,]
  for(kR in keepR){
    d<-rbind(d,data[[kR]])
  }
  if(!mergeacrossreps) d<-d[replicate==1]
  #d[,ptID:=NULL] # not unique after merge
  d<-cbind(ptID=1:dim(d)[1],d)
  suppressWarnings(d.m1<-melt(d,id.var=c("ptID","path","bm"),value.name="pointvalue"))
  d.m1$variable<-as.character(d.m1$variable)
  d.m1[,c("temp1","factor"):=tstrsplit(variable,"[.]")]
  d.m1[,c("temp2","temp3"):=tstrsplit(temp1,"_")]
  d.m1[,timept:=as.character(NA)]
  d.m1[,L_delta:=as.logical(NA)]
  # Check, as this got way more complicated than expected, fast
  #unique(d.m1[,.(variable,timept,factor,L_delta,temp1,temp2,temp3)])
  # Fix up the timept column
  d.m1[temp2=="D"]$timept<-d.m1[temp2=="D"]$temp3
  d.m1[temp2!="D"]$timept<-d.m1[temp2!="D"]$temp1
  #unique(d.m1[,.(variable,timept,factor,L_delta,temp1,temp2,temp3)])
  # Fix up the L_delta column
  d.m1[temp2=="D"]$L_delta<-TRUE
  d.m1[!is.na(factor)]$L_delta<-TRUE
  d.m1[is.na(L_delta)]$L_delta<-FALSE
  # Fix up the factor column
  d.m1[is.na(factor)]$factor<-"all"
  # Last check - HIGH RISH POINT FOR ERRORS
  #unique(d.m1[,.(variable,timept,factor,L_delta,temp1,temp2,temp3)])
  d.m1[,c("temp1","temp2","temp3"):=NULL]

  # Merge in the weeks, and do a version with mean/SDs
  timecrosswalk<-data.table(timept=td$metadata$timeptname,wk=td$metadata$timepoints)
  d.m2<-merge(d.m1,timecrosswalk,by="timept",all.x=TRUE,all.y=FALSE)
  d.m2[,meanvalue:=mean(pointvalue),by=c("variable","path")]
  d.m2[,sdvalue:=sd(pointvalue),by=c("variable","path")]
  d.m3<-unique(d.m2[,.(timept,path,factor,L_delta,wk,meanvalue,sdvalue)])

  # add in the baseline values...
  EG<-expand.grid(unique(d.m3$path),unique(d.m3$factor))
  setnames(EG,names(EG),c("path","factor"))
  BLvalues<-cbind(timept="BL",EG,L_delta=TRUE,wk=0,meanvalue=0,sdvalue=0)
  d.m3<-rbind(d.m3,BLvalues)

  # Order the factors, so will be consistent coloring:
  d.m3$factor<-factor(d.m3$factor,levels=c("all","br","pb","tv"))

  # Create the info for the bar at the bottom ID'ing when on drug
  mod<-td$metadata$ondrug
  w<-td$metadata$timepoints
  ondrug<-data.table(starts=numeric(),stops=numeric(),path=integer())
  for(path in 1:length(mod)){
    for(ep in 1:length(mod[[path]])){ # ep is the endpoint index
      if(mod[[path]][ep]==1){ # only add to list to plot if on drug at end of segment
        if(ep==1){start<-0}else{start<-w[ep-1]}
        stop<-w[ep]
        ondrug<-rbind(ondrug,data.table(starts=start,stops=stop,path=path))
      }
    }
  }

  # Plot by path
  p<-ggplot()+
    # Plot by factor, then sum...
    geom_point(data=d.m3[L_delta==TRUE],aes(x=wk,y=meanvalue,color=factor),size=.3)+
    geom_line(data=d.m3[L_delta==TRUE],aes(x=wk,y=meanvalue,color=factor),size=.3)+
    facet_wrap(~path)+
    labs(x="Weeks",y="Improvement in symptom intensity from baseline")+
    #    theme_minimal()+
    scale_color_manual(name="Factor",
                       values=c("red","blue","brown","black"),
                       breaks=c("all","br","pb","tv"),
                       labels=c("Sum","BR","ER","TR"))+
    # add the bar indicating when on drug...
    geom_segment(data=ondrug,aes(x=starts,y=-1.5,xend=stops,yend=-1.5),size=1)

  return(p)

}


