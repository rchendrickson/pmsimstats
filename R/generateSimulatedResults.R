#' Generate Simulated Results
#'
#' \code{generateSimulatedResults} rotates through a parameter space, creating
#'   and analyizing simulated data and collating the results
#'
#' This is a wrapper for \link{generateData} and \link{lme_analysis} that
#' systematically works its way through a set of parameters and creates
#' a specified number of repetitions of simulated data and results of
#' analyizing that data for each set of parameter specifications.
#'
#' @param trialdesigns A list of trial designs of the form generated
#'   by \link{buildtrialdesigns}
#' @param respparamsets A list of options for \code{respparam} as
#'   described in \link{generateData}
#' @param blparamsets A list of options for \code{blparam} as
#'   described in \link{generateData}
#' @param censorparams A data.table where each row is a set of options for
#'   \code{censorparam} as described in \link{censordata}
#' @param modelparams A data.table where each row is a set of options for
#'   \code{modelparams} as described in \link{generateData}
#' @param simparam The options that control the logistics of the simulations:
#'   \itemize{
#'     \item{\code{Nreps}}{  Number of repititions for a given set of parameters}
#'     \item{\code{progressiveSave=TRUE}}{  Do you want to save chunks as you go?
#'       Helpful if the run is going to take a long time, and you don't want the
#'       run to be interrupted 8 hours in to a 9 hour run and lose everything.}
#'     \item{\code{basesavename}}{  If you are using progressivesave, what do you
#'       want the base of the filename to be?  Will have _save1, _save2, etc appended}
#'     \item{\code{nRep2save}}{  How many parameter sets to you want to run at
#'       a time before saving?}
#'     \item{\code{saveunit2start}}{  If you got through part of your parameter
#'       space but not all of it, you can start partway through. If starting
#'       from the beginning, set to 1}
#'     \item{\code{savedir}}{  What directory do you want to save to?}
#'   }
#' @param analysisparams The options used by \link{lme_analysis}. Has the
#'   following components:
#'   \itemize{
#'     \item{\code{useDE=TRUE}}{  Binary: should the LME model use expectnacy
#'       information if available?}
#'     \item{\code{t_random_slope=FALSE}}{  Binary: should the LME model use
#'       random slopes, as well as random intercepts, for participants?}
#'   }
#' @param rawdataout=FALSE Binary - output the raw data for all
#'   simulations?  This is memory intensive for large runs.
#' @return Returns a list with three named parts:
#'   \itemize{
#'     \item{\code{results}}{  A large data table that tells you the parameters used
#'       and the analysis results for every simulated trial, one row per simualated trial.}
#'     \item{\code{parameterselections}}{  A list desribing the parameterspace used.
#'       This is important because some of the information in \code{out$results} is
#'       the index of the parameter set selection, and you will need this additional
#'       information to interpret it. For example, \code{trialdesigns}, \code{respparamset},
#'       \code{blparamset}, and \code{modelparamset} are all indexed.}
#'     \item{\code{rawdataout}}{  If you have rawdataout set to \code{TRUE}, you will
#'       also get (1) \code{drawdataout$precensor}, a list of simulated trial data of
#'       a length corresponding to the number of parameter permulations, with the order
#'       parallel to that of the main output results. Repititions of a particular
#'       parameter set are mixed, identified by \code{irep}. And, \code{rawdataout$postcensor},
#'       which has the same format but with the psotcensoring data}
#'   }
#' @examples
#'   # See vignettes for examples
#' @export

generateSimulatedResults<-function(trialdesigns,respparamsets,blparamsets,
                                   censorparams,modelparams,simparam,analysisparams,
                                   rawdataout=FALSE){

  # defaults
  if(missing(analysisparams)) analysisparams<-list(useDE=TRUE,
                                                   t_random_slope=FALSE,
                                                   full_model_out=FALSE)

  # Hold original directory in case we change it
  initialdirectory<-getwd()

  # Setup our path through the variable parameters and
  # initialize counts used for tracking on the screen:
  tic("Total time:")
  VPGmaster<-expand.grid(
    trialdesign=1:length(trialdesigns),
    respparamset=1:length(respparamsets),
    blparamset=1:length(blparamsets),
    modelparamset=1:dim(modelparams)[1]
  )
  nV<-dim(VPGmaster)[1]
  # note that censor param not included because done after data generation
  irep<-1
  totrep<-nV*simparam$Nreps
  totparamsets<-nV

  # If we're doing a progressive save, have a "large loop" that controls the smaller inside loop
  if(simparam$progressiveSave==FALSE){
    nLargeLoops<-1
    LLstarts<-1
    LLstops<-nV
  }else{
    nLargeLoops<-ceiling(nV/simparam$nRep2save)
    if(nLargeLoops>1){
      LLstarts<-c(1,1+simparam$nRep2save*(1:(nLargeLoops-1)))
      LLstops<-c(LLstarts[2:nLargeLoops]-1,nV)
    }else{
      LLstarts<-1
      LLstops<-nV
    }
  }

  for(iLL in simparam$saveunit2start:nLargeLoops){
    tic(paste("***Progressive Save Unit ",(iLL+1-simparam$saveunit2start)," of ",(nLargeLoops+1-simparam$saveunit2start),
              " complete",sep=""))
    VPG<-VPGmaster[LLstarts[iLL]:LLstops[iLL],]
    iparamset<-LLstarts[iLL]

    # Create output structure:
    out<-cbind(VPG[0,],data.table(censorparamset=integer(),use_DE=logical(),t_random_slope=logical(),
                                  irep=integer(),frac_NA=numeric(),beta=numeric(),betaSE=numeric(),
                                  p=numeric(),issingular=logical(),warning=character()))
    if(rawdataout){d_out<-list(precensor=list(),postcensor=list())}

    # Loop through the different variable parameters
    for(iR in 1:dim(VPG)[1]){
      tic(paste("Parameter set ",iR," of ",dim(VPG)[1]," in this save unit now complete (on ",iparamset," of ",nV," total sets)",sep=""))
      td<-trialdesigns[[VPG[iR,"trialdesign"]]][[2]]
      p<-list()
      p$respparam<-respparamsets[[VPG[iR,"respparamset"]]]$param
      p$blparam<-blparamsets[[VPG[iR,"blparamset"]]]$param
      p$modelparam<-modelparams[VPG[iR,"modelparamset"],]

      # Run this set of params 1 time to create N*Nrep simuated participants then subset, for efficiency;
      # have to create the dat for each path and merge
      #
      nP<-length(td)
      Ns<-p$modelparam$N%/%nP # How many in each path run?
      Ns<-Ns+c(rep(1,p$modelparam$N%%nP),rep(0,nP-p$modelparam$N%%nP)) # distribute the remainder
      NNs<-Ns*simparam$Nreps # here adjust so doing all with this paramset at once
      p$modelparam$N<-NNs[[1]]
      dat<-generateData(p$modelparam,p$respparam,p$blparam,td[[1]],FALSE,TRUE)
      dat[,path:=1]
      dat[,replicate:=rep(1:simparam$Nreps,Ns[1])]
      if(nP>1){
        for(iP in 2:nP){
          p$modelparam$N<-NNs[[iP]]
          dat2<-generateData(p$modelparam,p$respparam,p$blparam,td[[iP]],FALSE,TRUE)
          dat2[,path:=iP]
          dat2[,replicate:=rep(1:simparam$Nreps,Ns[iP])]
          dat<-rbind(dat,dat2)
        }
      }
      p$modelparam$N<-sum(Ns)

      # Analysis chunk repeat x nAnalysisParametersets:
      for(iAP in 1:dim(analysisparams)[1]){
        # Analyze entire population to get "empircal true beta" (ETbeta)
        #tic("analysis-whole population")
        ETanalysisout<-lme_analysis(td,dat,analysisparams[iAP,])
        #toc()
        # Now, for each replicate: analyize and save output
        #tic("analysis-replicates")
        for(iS in 1:simparam$Nreps){
          # tryCatch(
          #   expr = {
          #     analysisout<-lme_analysis(td,dat[replicate==iS],analysisparams[iAP,])
          #     analysisout$warning<-NA
          #   },
          #   error=function(e){
          #     message('Error in call to lme_analysis')
          #     print(e)
          #     print(paste("Error occurred on VPG line ",iR," on analysis parameterset ",AP," on replicate ",iS,sep=""))
          #   },
          #   warning=function(w){
          #     message('Error in call to lme_analysis')
          #     print(w)
          #     print(paste("Error occurred on VPG line ",iR," on analysis parameterset ",AP," on replicate ",iS,sep=""))
          #     analysisout<-data.table(beta=NA,betaSE=NA,p=NA,issingular=analysisout$issingular,warning=w)
          #   }
          # )
          analysisout<-lme_analysis(td,dat[replicate==iS],analysisparams[iAP,])
          o<-cbind(VPG[iR,],as.data.table(p$modelparam),
                   data.table(censorparamset=0,use_DE=analysisparams[iAP,]$useDE,t_random_slope=analysisparams[iAP,]$t_random_slope,
                              irep=((iR-1)*simparam$Nreps+iS),frac_NA=0,ETbeta=ETanalysisout$beta,
                              ETbetaSE=ETanalysisout$betaSE,beta=analysisout$beta,betaSE=analysisout$betaSE,
                              p=analysisout$p,issingular=analysisout$issingular,warning=analysisout$warning))
          out<-rbind(out,o)
        }
        # Repeat for each set of censor parameters
        nocensoringflag<-FALSE
        if(length(censorparams)<2){
          if(is.na(censorparams)){
            nocensoringflag<-TRUE
          }
        }
        if(!nocensoringflag){
          h_datc<-vector(mode="list",length=(dim(censorparams)[1]))
          for(iC in 1:dim(censorparams)[1]){
            h_datc[[iC]]<-censordata(dat,td[[1]],censorparams[iC,])
            for(iS in 1:simparam$Nreps){
              frac_NA<-sum(is.na(h_datc[[iC]][replicate==iS]))/(p$modelparam$N*length(td[[1]]$t_wk))
              analysisout<-lme_analysis(td,h_datc[[iC]][replicate==iS],analysisparams[iAP,])
              o<-cbind(VPG[iR,],as.data.table(p$modelparam),
                       data.table(censorparamset=iC,use_DE=analysisparams[iAP,]$useDE,t_random_slope=analysisparams[iAP,]$t_random_slope,
                                  irep=((iR-1)*simparam$Nreps+iS),frac_NA=frac_NA,ETbeta=ETanalysisout$beta,
                                  ETbetaSE=ETanalysisout$betaSE,beta=analysisout$beta,betaSE=analysisout$betaSE,
                                  p=analysisout$p,issingular=analysisout$issingular,warning=analysisout$warning))
              out<-rbind(out,o)
            }
          }
          #toc()
        }else{
          h_datc<-NA
        }
      }

      if(rawdataout){
        d_out$precensor[[iparamset]]<-dat
        d_out$postcensor[[iparamset]]<-h_datc
      }

      iparamset<-iparamset+1
      toc()
      #progress(iparamset,max.value=totparamsets)
    } # end iR

    # Organize the output and return it
    if(rawdataout){
      outpt<-list(results=as.data.table(out),
                  parameterselections=list(trialdesigns=trialdesigns,
                                           respparamsets=respparamsets,
                                           blparamsets=blparamsets,
                                           censorparams=censorparams,
                                           modelparams=modelparams,
                                           analysisparams=analysisparams,
                                           simparam=simparam),
                  rawdata=d_out)
    }else{
      outpt<-list(results=as.data.table(out),
                  parameterselections=list(trialdesigns=trialdesigns,
                                           respparamsets=respparamsets,
                                           blparamsets=blparamsets,
                                           censorparams=censorparams,
                                           modelparams=modelparams,
                                           analysisparams=analysisparams,
                                           simparam=simparam))
    }

    if(simparam$progressiveSave){
      setwd(simparam$savedir)
      saveRDS(outpt,paste(simparam$basesavename,iLL,sep="_save"))
    }
    toc()
  } # end iLL
  toc()
  setwd(initialdirectory)
  if(!simparam$progressiveSave) return(outpt)
}
