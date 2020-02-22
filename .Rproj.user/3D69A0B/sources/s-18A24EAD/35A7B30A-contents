#' Censor the data to simulate a particular pattern of dropouts
#'
#' \code{censordata} simulates a pattern of participant dropouts
#'
#' This function inputs a \code{dat} file as would be produced by
#' \link{generateData} and selects a subset to drop out (censor).
#' The probability of a particular timepoint being selected for
#' censoring in the initial pass is the sum of two factors: one
#' that censors at a set rate as a function of time, and one that
#' biases the rate of censoring based on the trajectory of symptoms,
#' with a large improvement in symptoms from baseline decreasing the
#' probability of dropout, while a small change or a worsening
#' increases the probability of dropout. In a separate, second pass
#' all dropouts are carried forward, such that once a datapoint
#' has been censored, all future datapoints for that particular
#' participant are also censored
#'
#' @param dat dat file as produced by \code{generateData}
#' @param trialdesign a trial design as defined by \code{buildtrialdesign}
#' @param censorparam a data.table provides 3 named parameters:
#'   (1) beta0 - the total rate (approximately) of censoring (0-1)
#'   (2) beta1 - the proportion of the total dropout that should be biased (0-1)
#'   (3) eb1 - the exponent used to define the shape of the curve defining biased dropout.
#'   See example, below.
#' @return A new data file, with the censored data points replaced by \code{NA},
#'   will be returned
#' @examples
#' # Create a set of censoring params to
#' # feed to censordata one at a time:
#' censorparams<-data.table(
#'   names=c("balanced","more of flat","more of biased","high dropout"),
#'   beta0=c(.05,.05 ,.05,.15),
#'   beta1=c(.5,.2,.8 ,.5),
#'   eb1=  c(2,  2  ,2  ,2 )
#'   )
#' @export


censordata<-function(dat,trialdesign,censorparam){
  #Stuff that should eventually be streamlined in trial def:
  d<-data.table(trialdesign)
  d$t_wk_cumulative<-cumulative(d$t_wk)
#  d[,onDrug:=(tod>0)]
  nP<-dim(trialdesign)[1]
  labels<-d$timeptname

  #Stuff to set up here
  deltas<-paste("D",labels,sep="_")
  evalstring<-paste("cdt<-dat[,.(",paste(deltas, collapse=', ' ),")]",sep="")
  eval(parse(text=evalstring))

  #Implement:
  frac_NA<-censorparam$beta0 #.3
  frac_NA_biased<-censorparam$beta1 #.6
  fna1<-frac_NA*(1-frac_NA_biased)
  fna2<-frac_NA*frac_NA_biased

  cdt_ps1<-cdt
  cdt_ps1[]<-1
  cdt_ps2<-sapply(cdt,function(x) ((x+100)^(censorparam$eb1)))
  cdt_p1<-t(t(cdt_ps1)*d$t_wk)
  cdt_p2<-t(t(cdt_ps2)*d$t_wk)
  cdt_r1<-runif(dim(cdt_p1)[1]*dim(cdt_p1)[2],min=0,max=2*mean(cdt_p1)*(.5/fna1))
  cdt_r2<-runif(dim(cdt_p2)[1]*dim(cdt_p2)[2],min=0,max=2*mean(cdt_p2)*(.5/fna2))
  do1<-as.data.table(cdt_p1>cdt_r1) # not yet cumulative, do this in masking stage
  do2<-as.data.table(cdt_p2>cdt_r2) # not yet cumulative, do this in masking stage
  do<-as.data.table(do1|do2)
  #sum(do==TRUE)/(dim(do1)[1]*dim(do1)[2])

  # TESTING:
  #  sum(do1==TRUE)/(dim(do1)[1]*dim(do1)[2])
  #  sum(do2==TRUE)/(dim(do2)[1]*dim(do2)[2])
  #test1<-melt(cdt)
  #test2<-melt(cdt_ps2)
  #test3<-data.table(x=test1$value,y=test2$value)
  #plot(test3$x,test3$y)

  # Apply to dat:
  for(itp in 1:length(labels)){
    for(tp in labels[1:itp]){
      masking_tp<-paste("D",tp,sep="_")
      masked_tp<-labels[itp]
      evalstring<-paste("dat$",masked_tp,"[do$",masking_tp,"]<-NA",sep="")
      eval(parse(text=evalstring))
    }
  }

  return(dat)
}
