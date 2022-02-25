#' @title
#' Estimation Functions
#'
#' @description
#' These functions carry out direct, model-assisted, and small area estimation
#' through three different modules: "Green Book", "Model-Assisted", and
#' "Small Area".
#' 
#' @return 
#' List object containing estimates produced. If GB or PB estimation, output is
#' directly from FIESTAutils, while outputs for MA or SA will be list objects
#' from packages such as `mase`, `sae`, `hbsae`, or `JoSAE`.
#'
#' @details
#' These functions can carry out estimation with data from a variety of domains, 
#' but are designed for USFS FIA data.
#'
#' @author
#' Tracey S. Frescino, Grayson W. White
#'
#' @keywords
#' internal
#' @name estimation_desc
NULL