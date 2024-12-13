% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plottingfunctions.R
\name{plotfactortrajectories}
\alias{plotfactortrajectories}
\title{Plot factor trajectories}
\usage{
plotfactortrajectories(data, tinfo, tds, opt2plot, mergeacrossreps = TRUE)
}
\arguments{
\item{data}{the \code{$rawdata$precensor} component of the output of \link{generateSimulatedResults}
(run with rawdataout=TRUE)}

\item{tinfo}{the \code{$results} component of the output of \link{generateSimulatedResults}}

\item{tds}{the \code{$parameterselections$trialdesigns} component of the output of
\link{generateSimulatedResults}}

\item{opt2plot}{a data.table with a single row using parameter names to instruct the
function which parameters to hold constant and plot the results of}

\item{mergeacrossreps=TRUE}{logical input of whether to merge across repititions}
}
\value{
a ggplot object containing the plot
}
\description{
\code{plotfactortrajectories} plots the averaged output of the factors
used to generate simulated trial data
}
\examples{
  data(results_trajectories)
  data<-simresults$rawdata$precensor # this gives a list of length (total reps done)
  tinfo<-simresults$results # dim[1] of tinfo gives the parameter space that went in
  tds<-simresults$parameterselections$trialdesigns
  mergeacrossreps<-TRUE
  plots1<-vector(mode="list",length=length(simresults$parameterselections$trialdesigns))
  for(ip in 1:length(plots1)){
    param2hold<-data.table(carryover_t1half=0,trialdesign=ip)
    plots1[[ip]]<-plotfactortrajectories(data,tinfo,tds,param2hold,mergeacrossreps)
  }
  # See vignettes for additional details
}
