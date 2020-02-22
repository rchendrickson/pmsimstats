#' Extracted baseline parameters for the vignette that reproduces the publication results.
#'
#' This contains the mean and standard deviation of (a) the standing systolic blood
#' pressures taken during orthostatic vital signs, and (b) the CAPS-IV PTSD assessment
#' total severity score.
#'
#' @docType data
#'
#' @usage data(extracted_bp)
"extracted_bp"

#' Extracted response parameters for the vignette that reproduces the publication results.
#'
#' This contains a set of maximum, displacement, rate and standard deviation values that, when
#' fed into the function \link{modgompertz} produces a combination of response factors
#' (biologic response to the drug, br here or BR in the publication; expectancy related response,
#' pb here or ER in the publication; and the non-treatment related time-varying factor, tv here
#' or TR in the publication) that together could plausibly describe our existing RCT data set.
#' This set of parameters was designed to make no other particular assumptions about how these
#' factors would perform - to be a 'tabula rasa' parameter set - where e.g. the timecourse and
#' maximum values of the expectancy related and tv/TR factors were assumed to be equal.
#'
#' @docType data
#'
#' @usage data(extracted_rp)
"extracted_rp"

#' The results of the vigenette Part 1 used to plot the trajectories
#'
#' @docType data
#'
#' @usage data(results_trajectories)
"results_trajectories"

#' The results of the vigenette Part 1 used to plot basic paramater space
#'
#' @docType data
#'
#' @usage data(results_core)
"results_core"

#' The results of the vigenette Part 1 used to plot rate response paramater space
#'
#' @docType data
#'
#' @usage data(results_rates)
"results_rates"

#' The results of the vigenette Part 1 used to plot maxes respone paramater space
#'
#' @docType data
#'
#' @usage data(results_maxes)
"results_maxes"

