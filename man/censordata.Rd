% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/censordata.R
\name{censordata}
\alias{censordata}
\title{Censor the data to simulate a particular pattern of dropouts}
\usage{
censordata(dat, trialdesign, censorparam)
}
\arguments{
\item{dat}{dat file as produced by \code{generateData}}

\item{trialdesign}{a trial design as defined by \code{buildtrialdesign}}

\item{censorparam}{a data.table provides 3 named parameters:
(1) beta0 - the total rate (approximately) of censoring (0-1)
(2) beta1 - the proportion of the total dropout that should be biased (0-1)
(3) eb1 - the exponent used to define the shape of the curve defining biased dropout.
See example, below.}
}
\value{
A new data file, with the censored data points replaced by \code{NA},
  will be returned
}
\description{
\code{censordata} simulates a pattern of participant dropouts
}
\details{
This function inputs a \code{dat} file as would be produced by
\link{generateData} and selects a subset to drop out (censor).
The probability of a particular timepoint being selected for
censoring in the initial pass is the sum of two factors: one
that censors at a set rate as a function of time, and one that
biases the rate of censoring based on the trajectory of symptoms,
with a large improvement in symptoms from baseline decreasing the
probability of dropout, while a small change or a worsening
increases the probability of dropout. In a separate, second pass
all dropouts are carried forward, such that once a datapoint
has been censored, all future datapoints for that particular
participant are also censored
}
\examples{
# Create a set of censoring params to
# feed to censordata one at a time:
censorparams<-data.table(
  names=c("balanced","more of flat","more of biased","high dropout"),
  beta0=c(.05,.05 ,.05,.15),
  beta1=c(.5,.2,.8 ,.5),
  eb1=  c(2,  2  ,2  ,2 )
  )
}
