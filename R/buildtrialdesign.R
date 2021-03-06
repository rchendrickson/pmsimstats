#' Build your proposed clinical trial design
#'
#' \code{buildtrialdesign} inputs the details of a proposed clinical trial
#' design in an intuitive way and outputs those details in a structured form
#' that can be used by the pmsimstat package simulation tools
#'
#' @seealso
#' \link{ggplot2} for what it's using to plot
#'
#' @param name_longform A name that will clearly remind you what this trial
#'   design is. Can have spaces, etc. If not provided, will default to "Trial
#'   Design 1"
#' @param name_shortform An abbreviated version of the name that can be used
#'   on plots, in your code when you want to select a certain trial design, etc.
#' @param timepoints A numeric vector containing the timepoints (not including
#'   baseline) at which anything of relevance happens in your trial design. You
#'   must specify any timepoint where a measurement will be taken, the participant's
#'   expectancy about what intervention they are receiving changes, or the actual
#'   intervention itself (e.g. all randomizationpoints) can change.
#'   NOTE: The unit used is not specified - you must just ensure that the unit you
#'   use here is consistent with the unit used for e.g. the halflife of any
#'   carryover effect. Typical units would be weeks or days.
#' @param timeptnames A character vector containing brief labels for each of the timepoints.
#'   If your trial has different phases, it can be helpful to incorporate those into the
#'   timepoint names for later easy reference. It is helpful if they are short enough to
#'   use as x-axis labels on a plot. If not specified, will default to "V1, V2, V3..." for
#'   Visit 1, Visit 2, etc.
#' @param expectancies A numeric vector with all values ranging from 0 to 1, of the same length
#'   as timepoints. These values represent the "expectancy" a participant has at any given
#'   point that they are receiving active, effective treatment. It can match the probability
#'   that a participant is receiving active treatment (e.g. 0.5 for 1:1 randomization of
#'   active drug to placebo), but does not have to. It is used to scale the degree of the
#'   expectancy-related response factor.
#' @param ondrug A list containing binary vectors that describe when participants are on active
#'   treatment or not (whether they are on a placebo or not is irrelevant for this paramater).
#'   Each vector is same length as the number of timepoints, and specificies that someone
#'   is either on active treatment at that timepoint (1) or not on active treatment at that
#'   timepoint (0). Each possible path through the trial is described by a separate vector.
#'   E.g., an open label trial will only have one path (a list with one vector, which contains
#'   all 1's), a traditional parallel group RCT would have 2 paths (all 1's and all 0's), and
#'   a trial with two randomization points would have 4 paths.
#' @return \code{output$metadata} contains the input variables you used for future reference,
#'   named as above.
#' @return \code{output$trialpaths} contains a list whose length is defined by the number
#'   of paths through the clinical trial (ie, the length of the input variable ondrug, above),
#'   containing the information about the trial in the form required by the pmsimstat tools.
#' @examples
#' tdOL<-buildtrialdesign(
#'         name_longform="open label",
#'         name_shortform="OL",
#'         timepoints=cumulative(rep(2.5,8)),
#'         timeptname=paste("OL",1:8,sep=""),
#'         expectancies=rep(1,8),
#'         ondrug=list(
#'           pathA=rep(1,8)
#'           )
#'         )
#'   #Builds a 20 week entirely open-label trial
#'
#'tdCO<-buildtrialdesign(
#'         name_longform="traditional crossover",
#'         name_shortform="CO",
#'         timepoints=cumulative(rep(2.5,8)),
#'         timeptname=c(paste("COa",1:4,sep=""),paste("COb",1:4,sep="")),
#'         expectancies=rep(.5,8),
#'         ondrug=list(
#'           # Assumes on drug entire previous interval and this measurement point
#'           pathA=c(1,1,1,1,0,0,0,0),
#'           pathB=c(0,0,0,0,1,1,1,1)
#'           )
#'         )
#'    # Builds a traditional crossover trial, 20 weeks long, with no washout period
#' @export

buildtrialdesign<-function(name_longform,name_shortform,
                           timepoints,timeptnames,expectancies,ondrug){

  # Add defaults for unspecified parameters
  if(missing(name_longform)){name_longform="Trial Design 1"}
  if(missing(name_shortform)){name_shortform=name_longform}
  if(missing(timeptnames)){timeptnames=paste("V",1:(length(timepoints)),sep="")}

  # Build list of trial designs, one for each path through
  nTD<-length(ondrug)
  tds<-vector(mode="list",length=nTD)
  t_wk<-c(timepoints[1],
          timepoints[2:length(timepoints)]-timepoints[1:(length(timepoints)-1)])
  for(iTD in 1:nTD){
    od<-ondrug[[iTD]]
    od_intervals<-t_wk*od
    tod<-od_intervals
    tsd<-t_wk*(od!=1)
    tpb<-t_wk*(expectancies>0)
    for(iTP in 2:length(timepoints)){
      if(od_intervals[iTP]>0){
        tod[iTP]<-tod[iTP]+tod[iTP-1]
      }else{
        tsd[iTP]<-tsd[iTP]+tsd[iTP-1]
      }
      if(tpb[iTP]>0){
        tpb[iTP]<-tpb[iTP]+tpb[iTP-1]
      }
    }
    everondrug<-(cumulative(od)>0)
    tsd<-everondrug*tsd
    tds[[iTD]]<-data.table(timeptnames=timeptnames,t_wk=t_wk,e=expectancies,tod=tod,tsd=tsd,tpb=tpb)
  }

  # Retain the metadata in structured form in the output
  metadata<-list(name_longform=name_longform,
                 name_shortform=name_shortform,
                 timepoints=timepoints,
                 timeptnames=timeptnames,
                 expectancies=expectancies,
                 ondrug=ondrug)

  # Output
  out<-list(metadata=metadata,trialpaths=tds)
}
