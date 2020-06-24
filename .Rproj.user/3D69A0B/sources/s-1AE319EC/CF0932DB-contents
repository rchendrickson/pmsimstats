# Short helper functions

#' Cumulative
#'
#' \code{cumulative} turns a vector of intervals into a cumulative running time
#'
#' @export
cumulative<-function(x){
  I<-length(x)
  y<-rep(NA,I)
  y[1]<-x[1]
  if(I>1){
    for(i in 2:I){
      y[i]<-x[i]+y[i-1]
    }
  }
  return(y)
}

#' Modified Gompertz
#'
#' \code{modgompertz} takes response parameters and returns change in symptoms as a function of time
#'
#' This is a slight modification of a standard gompertz function that is designed to model
#' how symptoms can change as a function of time.
#'
#' @param t time - no specific units, just make sure it's consistant across your modeling
#' @param maxr maximum response that can be attained (ceiling)
#' @param disp vertical displacement
#' @param rate rate
#' @return A numeric value indicating how much "improvement" there has been since baseline
#'   as of point \code{t} in time. A negative value indicates worsening rather than improvement.
#' @export
modgompertz<-function(t,maxr,disp,rate){
  y<-maxr*exp(-disp*exp(-rate*t))
  vert_offset<-maxr*exp(-disp*exp(-rate*0))
  y<-y-vert_offset
  y<-y*(maxr/(maxr-vert_offset)) # return the assymptotic max to what it was
  return(y)
}

#' Reknit simulated results
#'
#' \code{reknitsimresults} recombines the results of simulations that have the
#' exact same parameter space set, but were run as separate chunks - e.g., you
#' ran 100 repititions, realized you needed more, and ran 150 more to have 250
#' total. Does not work with rawdataout.
#'
#' @param basesavename The basesavename as described in \link{generateSimulatedResults}
#' @param savedir The directory the files are saved in
#' @return The output is a file just like you would have received from
#'   \link{generateSimulatedResults} if it had all been run at once.
#' @export
reknitsimresults<-function(basesavename,savedir){
  if(missing(savedir)){
    initialdirectory<-getwd()
  }else{
    initialdirectory<-getwd()
    setwd(savedir)
  }
  sr<-vector(mode="list",length=length(basesavename))
  for(iSN in 1:length(basesavename)){
    simresults<-readRDS(paste(basesavename[1],"1",sep="_save"))
    nextfilenum<-2
    while(nextfilenum>0){
      nextfile<-paste(basesavename[1],nextfilenum,sep="_save")
      if(file.exists(nextfile)){
        newresults<-readRDS(nextfile)
        simresults$results<-rbind(simresults$results,newresults$results)
        if(length(names(simresults))>2){
          warning("Haven't implemented adding rawdata together yet")
        }
        nextfilenum<-nextfilenum+1
      }else{
        nextfilenum<-0
      }
    }
    sr[[iSN]]<-simresults
  }
  if(length(sr)>1){
    # test the parameter settings are the same
    for(iSN in 2:length(sr)){
      if(!identical(sr[[1]]$parameterselections$trialdesigns,
          sr[[iSN]]$parameterselections$trialdesigns)){
        warning("Trial design settings don't match")
      }
      if(!identical(sr[[1]]$parameterselections$respparamsets,
                    sr[[iSN]]$parameterselections$respparamsets)){
        warning("Response paramset settings don't match")
      }
      if(!identical(sr[[1]]$parameterselections$blparamsets,
                    sr[[iSN]]$parameterselections$blparamsets)){
        warning("Baseline paramset settings don't match")
      }
    }
    # Merge (inefficiently, if had saved differently could use rbindlist...)
    simresults<-sr[[1]]
    for(iSN in 2:length(sr)){
      simresults$results<-rbind(simresults$results,sr[[iSN]]$results)
    }
  }else{
    simresults<-sr[[1]]
  }
  return(simresults)
}
