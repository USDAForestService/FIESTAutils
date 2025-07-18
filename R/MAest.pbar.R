#' @rdname estimation_desc
#' @export
MAest.ht <- function(y,
                     N,
                     FIA = TRUE,
                     getweights = FALSE,
                     var_method = "LinHTSRS") {

  ## Set global variables
  nhat.var <- NULL

  NBRPLT <- length(y)
  NBRPLT.gt0 <- sum(y > 0)

  estht <- mase::horvitzThompson(y,
                                 pi = NULL,
                                 N = N,
                                 pi2 = NULL,
                                 var_est = TRUE,
                                 var_method = var_method,
                                 messages = FALSE,
                                 fpc = !FIA,
                                 B = 1000)

  esthtdt <- data.table(estht$pop_mean, estht$pop_mean_var, NBRPLT, NBRPLT.gt0)
  setnames(esthtdt, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
  returnlst <- list(est=esthtdt)

  ## Return survey weights
  if (getweights) {
    if (any(estht$weights < 0)) {
      message("model resulted in negatives... indicating model instability")
    }
    returnlst$weights <- estht$weights / N
  }

  return(returnlst)

}

#' @rdname estimation_desc
#' @export
MAest.ps <- function(y,
                     N,
                     x_sample,
                     x_pop,
                     FIA = TRUE,
                     save4testing = FALSE,
                     getweights = FALSE,
                     var_method = "SRSunconditional") {

  ## Set global variables
  nhat.var <- NULL

  NBRPLT <- length(y)
  NBRPLT.gt0 <- sum(y > 0)

  estps <- tryCatch(mase::postStrat(y = y,
                                    xsample = x_sample,
                                    xpop = x_pop,
                                    pi = NULL,
                                    N = N,
                                    pi2 = NULL,
                                    var_est = TRUE,
                                    var_method = var_method,
                                    datatype = "means",
                                    messages = FALSE,
                                    fpc = !FIA,
                                    B = 1000),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
  if (is.null(estps)) {
    if (save4testing) {
      message("saving objects to working directory for testing: y, x_sample, x_pop, N")

      save(y, file=file.path(getwd(), "y.rda"))
      save(x_sample, file=file.path(getwd(), "x_sample.rda"))
      save(x_pop, file=file.path(getwd(), "x_pop.rda"))
      save(N, file=file.path(getwd(), "N.rda"))
    }
    message("error in mase::postStrat function... returning NA")
    estps <- data.table(matrix(c(NA, NA, NBRPLT, NBRPLT.gt0), 1,4))
    setnames(estps, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
    returnlst <- list(est=estps)

    if (getweights) {
      weights <- rep(NA, length(y))
      returnlst$weights <- weights
    }
    return(returnlst)
  }

  estpsdt <- data.table(estps$pop_mean, estps$pop_mean_var, NBRPLT, NBRPLT.gt0)
  setnames(estpsdt, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
  returnlst <- list(est=estpsdt)

  ## Return survey weights
  if (getweights) {
    if (any(estps$weights < 0)) {
      message("model resulted in negatives... indicating model instability")
    }
    returnlst$weights <- estps$weights / N
  }

  return(returnlst)

}

#' @rdname estimation_desc
#' @export
MAest.greg <- function(y,
                       N,
                       x_sample,
                       x_pop,
                       FIA = TRUE,
                       save4testing = TRUE,
                       modelselect = FALSE,
                       getweights = FALSE,
                       var_method = "LinHTSRS") {

  ## Set global variables
  nhat.var <- NULL

  NBRPLT <- length(y)
  NBRPLT.gt0 <- sum(y > 0)

  ## define empty data frame for storing selected predictors
  predselect <- x_pop[FALSE, , drop=FALSE]

  if (modelselect) {
    ## Get model-selected variables using mase::gregElasticNet()
    preds.selected <- gregEN.select(y, x_sample, x_pop, N, alpha=0.5)
  } else {
    preds.selected <- names(predselect)
  }

  ## Note: GREG cannot have more predictors than plots
  if (length(y) <= length(names(predselect))) {
    preds.selected <- na.omit(preds.selected[1:(length(y)-1)])
  }

  if (length(preds.selected) == 0) {
    xsample <- x_sample[, names(predselect), drop=FALSE]
    xpop <- x_pop[, names(predselect), drop=FALSE]
  } else {
    xsample <- x_sample[, preds.selected, drop=FALSE]
    xpop <- x_pop[, preds.selected, drop=FALSE]
  }
  estgreg <- tryCatch(mase::greg(y = y,
                                 xsample = xsample,
                                 xpop = x_pop,
                                 pi = NULL,
                                 N = N,
                                 pi2 = NULL,
                                 model = "linear",
                                 var_est = TRUE,
                                 var_method = var_method,
                                 datatype = "means",
                                 modelselect = FALSE,
                                 lambda = "lambda.min",
                                 messages = FALSE,
                                 fpc = !FIA,
                                 B = 1000),
			error=function(err) {
				message(err, "\n")
				return(NULL)
			} )
  if (is.null(estgreg)) {
    if (save4testing) {
      message("saving objects to working directory for testing: y, x_sample, x_pop, N")

      save(y, file=file.path(getwd(), "y.rda"))
      save(x_sample, file=file.path(getwd(), "x_sample.rda"))
      save(x_pop, file=file.path(getwd(), "x_pop.rda"))
      save(N, file=file.path(getwd(), "N.rda"))
    }
    message("multicolinearity exists in predictor data set...  try MAmethod = gregEN")
    estgreg <- data.table(matrix(c(NA, NA, NBRPLT, NBRPLT.gt0), 1,4))
    setnames(estgreg, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
    predselect[1,] <- NA
    returnlst <- list(est=estgreg, predselect=predselect)

    if (getweights) {
      weights <- rep(NA, length(y))
      returnlst$weights <- weights
    }
    return(returnlst)
  }

  selected <- data.frame(t(estgreg$coefficients))[,-1]
  if (!is(selected, "data.frame")) {
    predselect <- rbindlist(list(predselect, as.list(selected)), fill=TRUE)
  } else {
    predselect <- rbindlist(list(predselect, selected), fill=TRUE)
  }
  estgregdt <- data.table(estgreg$pop_mean, estgreg$pop_mean_var, NBRPLT, NBRPLT.gt0)
  setnames(estgregdt, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
  returnlst <- list(est=estgregdt, predselect=predselect)


  ## Return survey weights
  if (getweights) {
    if (any(estgreg$weights < 0)) {
      message("model resulted in negatives... indicating model instability")
    }
    returnlst$weights <- estgreg$weights / N
  }

  return(returnlst)

}

#' @rdname estimation_desc
#' @export
MAest.ratio <- function(y,
                        N,
                        x_sample,
                        x_pop,
                        FIA = TRUE,
                        save4testing = TRUE,
                        var_method = "LinHTSRS") {

  ## Set global variables
  nhat.var <- NULL

  NBRPLT <- length(y)
  NBRPLT.gt0 <- sum(y > 0)

  estratio <- tryCatch(mase::ratioEstimator(y = y,
                                            xsample = x_sample,
                                            xpop = x_pop,
                                            pi = NULL,
                                            N = N,
                                            pi2 = NULL,
                                            var_est = TRUE,
                                            var_method = var_method,
                                            datatype = "means",
                                            messages = FALSE,
                                            fpc = !FIA,
                                            B = 1000),
                       error=function(err) {
                         message(err, "\n")
                         return(NULL)
                       })

  if (is.null(estratio)) {
    if (save4testing) {
      message("saving objects to working directory for testing: y, x_sample, x_pop, N")

      save(y, file=file.path(getwd(), "y.rda"))
      save(x_sample, file=file.path(getwd(), "x_sample.rda"))
      save(x_pop, file=file.path(getwd(), "x_pop.rda"))
      save(N, file=file.path(getwd(), "N.rda"))
    }
    message("error in mase::ratioEstimator function... returning NA")
    estratio <- data.table(matrix(c(NA, NA, NBRPLT, NBRPLT.gt0), 1,4))
    setnames(estratio, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
    returnlst <- list(est=estratio)

#    if (getweights) {
#      weights <- rep(NA, length(y))
#      returnlst$weights <- weights
#    }
    return(returnlst)
  }

  estratiodt <- data.table(estratio$pop_mean, estratio$pop_mean_var, NBRPLT, NBRPLT.gt0)
  setnames(estratiodt, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
  returnlst <- list(est=estratiodt)

  ## Return survey weights
#  if (getweights) {
#    if (any(estratio$weights < 0)) {
#      message("model resulted in negatives... indicating model instability")
#    }
#    returnlst$weights <- estratio$weights / N
#  }
  return(returnlst)
}

#' @rdname estimation_desc
#' @export
MAest.gregEN <- function(y,
                         N,
                         x_sample,
                         x_pop,
                         FIA = TRUE,
                         model = "linear",
                         save4testing = TRUE,
                         getweights = FALSE,
                         var_method = "LinHTSRS") {

  ## define empty data frame for storing selected predictors
  predselect <- x_pop[FALSE, ]

  ## Set global variables
  nhat.var <- NULL

  NBRPLT <- length(y)
  NBRPLT.gt0 <- sum(y > 0)
  # var_method <- "LinHTSRS"
  estgregEN <- tryCatch(mase::gregElasticNet(y = y,
                                             xsample = x_sample,
                                             xpop = x_pop,
                                             pi = NULL,
                                             N = N,
                                             pi2 = NULL,
                                             model = model,
                                             var_est = TRUE,
                                             var_method = var_method,
                                             datatype = "means",
                                             lambda = "lambda.min",
                                             messages = FALSE,
                                             fpc = !FIA,
                                             B = 1000),
                        error=function(err) {
                          message(err, "\n")
                          return(NULL)
                        })

  if (is.null(estgregEN)) {
    if (save4testing) {
      message("saving objects to working directory for testing: y, x_sample, x_pop, N")

      save(y, file=file.path(getwd(), "y.rda"))
      save(x_sample, file=file.path(getwd(), "x_sample.rda"))
      save(x_pop, file=file.path(getwd(), "x_pop.rda"))
      save(N, file=file.path(getwd(), "N.rda"))
    }

    message("error in mase::gregElasticNet function... returning NA")

    estgregEN <- data.table(matrix(c(NA, NA, NBRPLT, NBRPLT.gt0), 1,4))
    setnames(estgregEN, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
    predselect[1,] <- NA
    returnlst <- list(est=estgregEN, predselect=predselect)

    if (getweights) {
      weights <- rep(NA, length(y))
      returnlst$weights <- weights
    }
    return(returnlst)
  }

  selected <- data.frame(t(estgregEN$coefficients))[,-1]
  predselect <- rbindlist(list(predselect, selected), fill=TRUE)

  estgregENdt <- data.table(estgregEN$pop_mean, estgregEN$pop_mean_var, NBRPLT, NBRPLT.gt0)
  setnames(estgregENdt, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))

  returnlst <- list(est=estgregENdt, predselect=predselect)

  ## Return survey weights
  if (getweights) {
    if (any(estgregEN$weights < 0)) {
      message("model resulted in negatives... indicating model instability")
    }
    returnlst$weights <- estgregEN$weights / N
  }

  return(returnlst)

}

#' @rdname estimation_desc
#' @export
MAest.gregRatio <- function(yn,
                            yd,
                            N,
                            area,
                            x_sample,
                            x_pop,
                            FIA = TRUE,
                            save4testing = FALSE,
                            modelselect = FALSE,
                            getweights = FALSE,
                            var_method = "LinHTSRS") {

  nhat.var <- NULL

  NBRPLT <- length(yn)
  NBRPLT.gt0 <- sum(yn > 0)
  gt0.ratio <- (NBRPLT - NBRPLT.gt0)/NBRPLT

  # don't trust these estimates
  if (NBRPLT < 5 || NBRPLT.gt0 < 2 || gt0.ratio > 0.9){
    estgregRatio <- data.table(matrix(c(NA, NA, NA, NA), 1, 4))
    setnames(estgregRatio, c("rhat", "rhat.var", "NBRPLT", "NBRPLT.gt0"))
    returnlst <- list(est = estgregRatio)
    return(returnlst)
  }

  estgregRatio <- tryCatch(
    {
      mase::ratio(y_num = yn,
                  y_den = yd,
                  xsample = x_sample,
                  xpop = x_pop,
                  N = N,
                  estimator = "greg",
                  datatype = "means",
                  var_est = TRUE,
                  var_method = var_method,
                  messages = FALSE,
                  fpc = !FIA,
                  B = 1000)
    },
    error = function(cond) {
      message(cond, "\n")
      return(NULL)
    }
  )

  if (is.null(estgregRatio)) {
    if (save4testing) {
      message("saving objects to working directory for testing: yn, yd, x_sample, x_pop, N")

      save(yn, file=file.path(getwd(), "yn.rda"))
      save(yd, file=file.path(getwd(), "yd.rda"))
      save(x_sample, file=file.path(getwd(), "x_sample.rda"))
      save(x_pop, file=file.path(getwd(), "x_pop.rda"))
      save(N, file=file.path(getwd(), "N.rda"))
    }

    message("error in mase::ratio function... returning NA")

    estgregRatio <- data.table(matrix(c(NA, NA, NBRPLT, NBRPLT.gt0), 1, 4))
    setnames(estgregRatio, c("rhat", "rhat.var", "NBRPLT", "NBRPLT.gt0"))
    returnlst <- list(est = estgregRatio)

    return(returnlst)
  }

  # don't trust estimates in this case
  if (estgregRatio$ratio_est <= 0 || estgregRatio$ratio_var_est <= 0) {
    estgregRatio <- data.table(matrix(c(NA, NA, NA,NA), 1, 4))
    setnames(estgregRatio, c("rhat", "rhat.var", "NBRPLT", "NBRPLT.gt0"))
    returnlst <- list(est = estgregRatio)
    return(returnlst)
  }

  estgregRatiodt <- data.table(estgregRatio$ratio_est, estgregRatio$ratio_var_est/(area^2), NBRPLT, NBRPLT.gt0)
  setnames(estgregRatiodt, c("rhat", "rhat.var", "NBRPLT", "NBRPLT.gt0"))


  returnlst <- list(est = estgregRatiodt)

  return(returnlst)

}



########################################################################
## Get estimates
########################################################################
#' @rdname estimation_desc
#' @export
MAest <- function(yn = "CONDPROP_ADJ",
                  dat.dom,
                  cuniqueid,
                  unitlut = NULL,
                  pltassgn,
                  esttype = "ACRES",
                  MAmethod,
                  strvar = NULL,
                  prednames = NULL,
                  yd = NULL,
                  ratiotype = "PERACRE",
                  N,
                  area,
                  FIA = TRUE,
                  modelselect = FALSE,
                  getweights = FALSE,
                  var_method = ifelse(MAmethod %in% c("PS"), "SRSunconditional", "LinHTSRS")) {

  ########################################################################################
  ## DESCRIPTION: Gets estimates from mase::horvitzThompson
  ## PARAMETERS:
  ## yn 		- response (numerator)
  ## dat 		- domain-level data set
  ## yd		- response (denominator)
  ## N		- Number of pixels in estimation unit
  ## n		- Number of total plots in estimation unit
  ##
  ## VALUES:
  ## nhat		- estimate proportion of land covered by condition, for numerator
  ## nhat.var	- variance of estimated proportion, for numerator
  ## dhat		- estimate proportion of land covered by condition, for denominator
  ## dhat.var	- variance of estimated proportion, for denominator
  ## covar		- covariance of numerator and denominator
  ########################################################################################

  ## Merge dat.dom to pltassgn
  pltdat.dom <- as.data.frame(dat.dom[pltassgn])
  pltdat.dom[is.na(pltdat.dom[[yn]]), yn] <- 0

  if (MAmethod == "gregRatio") {
    pltdat.dom[is.na(pltdat.dom[[yd]]), yd] <- 0
    yd.vect <- pltdat.dom[[yd]]
  }

  ## Subset response vector and change NA values of response to 0
  yn.vect <- pltdat.dom[[yn]]

  if (MAmethod == "HT") {
    estlst <- MAest.ht(yn.vect,
                       N,
                       FIA = FIA,
                       getweights = getweights,
                       var_method = var_method)

  } else if (MAmethod == "PS") {

    x_sample <- pltdat.dom[, strvar][[1]]
    x_pop <- unitlut[, c(strvar, "Prop"), with=FALSE]

    estlst <- MAest.ps(yn.vect,
                       N,
                       x_sample,
                       x_pop,
                       FIA = FIA,
                       getweights = getweights,
                       var_method = var_method)

  } else {

    x_sample <- setDF(pltdat.dom)[, prednames, drop=FALSE]
    x_pop <- setDF(unitlut)[, prednames, drop=FALSE]
    
    if (MAmethod == "greg") {
     
      estlst <- MAest.greg(yn.vect,
                           N,
                           x_sample,
                           x_pop,
                           FIA = FIA,
                           modelselect = modelselect,
                           getweights = getweights,
                           var_method = var_method)

    } else if (MAmethod == "gregEN") {

      estlst <- MAest.gregEN(yn.vect,
                             N,
                             x_sample,
                             x_pop,
                             FIA = FIA,
                             getweights = getweights,
                             var_method = var_method)

    } else if (MAmethod == "ratio") {

      if (length(prednames) > 1) {
        stop("only one continuous predictor is allowed")
      } else {
        x_sample <- x_sample[[prednames]]
        x_pop <- x_pop[[prednames]]
      }
      est <- MAest.ratio(yn.vect,
                         N,
                         x_sample,
                         x_pop,
                         FIA = FIA,
                         var_method = var_method)

    } else if (MAmethod == "gregRatio") {

      estlst <- MAest.gregRatio(yn.vect,
                                yd.vect,
                                N = N,
                                area = area,
                                x_sample,
                                x_pop,
                                FIA = FIA,
                                getweights = getweights,
                                var_method = var_method)

    } else {
      stop("invalid MAmethod")
    }
  }


  if (getweights) {
    estlst$weights <- data.frame(pltdat.dom[[cuniqueid]], estlst$weights)
    names(estlst$weights) <- c(cuniqueid, "weights")
  }

  return(estlst)

}


########################################################################
## By domain
########################################################################
#' @rdname estimation_desc
#' @export
MAest.dom <- function(dom,
                      dat,
                      cuniqueid,
                      unitlut,
                      pltassgn,
                      esttype,
                      MAmethod,
                      strvar = NULL,
                      prednames = NULL,
                      domain,
                      N,
                      area = NULL,
                      response = NULL,
                      response_d = NULL,
                      FIA = TRUE,
                      modelselect = FALSE,
                      getweights = FALSE,
                      var_method = ifelse(MAmethod %in% c("PS"), "SRSunconditional", "LinHTSRS"),
                      quiet = FALSE) {

  ## Subset tomdat to domain=dom
  dat.dom <- dat[dat[[domain]] == dom,]

  if (nrow(dat.dom) == 0 || sum(!is.na(dat.dom[[domain]])) == 0) {

    domest <- data.table(dom, matrix(c(NA, NA, 0, 0), 1,4))
    setnames(domest, c(domain, "nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))

    predselect <- data.table(dom, unitlut[FALSE, unique(c(prednames, strvar), with=FALSE)])
    setnames(predselect, "dom", domain)
    returnlst <- list(est = domest, predselect = predselect)

    if (getweights) {
      weights <- data.frame(dom, id=cuniqueid, weights=NA)
      setnames(weights, "id", cuniqueid)
      setnames(weights, "dom", domain)
      returnlst$weights <- weights
    }

    return(returnlst)

  }

  if (quiet) {
    ## Apply function to each dom
    domestlst <- suppressMessages(
      MAest(yn = response,
            yd = response_d,
            dat.dom = dat.dom,
            pltassgn = pltassgn,
            cuniqueid = cuniqueid,
            esttype = esttype,
            unitlut = unitlut,
            strvar = strvar,
            prednames = prednames,
            MAmethod = MAmethod,
            N = N,
            area = area,
            FIA = FIA,
            modelselect = modelselect,
            getweights = getweights,
            var_method = var_method))

  } else {
    ## Apply function to each dom
    domestlst <- MAest(yn = response,
                       yd = response_d,
                       dat.dom = dat.dom,
                       pltassgn = pltassgn,
                       cuniqueid = cuniqueid,
                       esttype = esttype,
                       unitlut = unitlut,
                       strvar = strvar,
                       prednames = prednames,
                       MAmethod = MAmethod,
                       N = N,
                       area = area,
                       FIA = FIA,
                       modelselect = modelselect,
                       getweights = getweights,
                       var_method = var_method)
  }

  domestlst <- lapply(domestlst, function(x, dom, domain) {
                                    dt <- data.table(dom, x)
                                    setnames(dt, "dom", domain)
                                 }, dom, domain)

  return(domestlst)

}



########################################################################
## By estimation unit
########################################################################
#' @rdname estimation_desc
#' @export
MAest.unit <- function(unit,
                       dat,
                       cuniqueid,
                       unitlut,
                       unitvar,
                       esttype,
                       MAmethod = "HT",
                       strvar = NULL,
                       prednames = NULL,
                       domain,
                       response,
                       response_d = NULL,
                       npixels,
                       unitarea = NULL,
                       FIA = TRUE,
                       modelselect = FALSE,
                       getweights = FALSE,
                       var_method = ifelse(MAmethod %in% c("PS"), "SRSunconditional", "LinHTSRS"),
                       quiet = FALSE) {


  # if gregRatio, aggregate by both response variables
  if (MAmethod == "gregRatio") {
    dat.unit <- dat[dat[[unitvar]] == unit, c(cuniqueid, domain, response, response_d), with=FALSE]
  } else {
    dat.unit <- dat[dat[[unitvar]] == unit, c(cuniqueid, domain, response), with=FALSE]
  }


  ## get prednames if modelselect_bydomain = FALSE
  if (!modelselect && is.data.frame(prednames)) {
    prednames.unit <- prednames[prednames[[unitvar]] == unit, ]
    prednames.unit <- names(prednames.unit)[!is.na(prednames.unit) & names(prednames.unit) != unitvar]
    prednames <- names(prednames)
  } else {
    prednames.unit <- prednames
  }


  if (nrow(dat.unit) == 0 || sum(!is.na(dat.unit[[domain]])) == 0) {

    if (domain == "TOTAL") {
      unitest <- data.table(unit=unit, domain=1, nhat=0, nhat.var=0, NBRPLT=0, NBRPLT.gt0=0)
    } else {
      unitest <- data.table(unit=unit, domain="TOTAL", nhat=0, nhat.var=0, NBRPLT=0, NBRPLT.gt0=0)
    }

    setnames(unitest, c("unit", "domain"), c(unitvar, domain))

    predselect <- data.table(unit=unit, domain=1, unitlut[FALSE, c(prednames.unit,strvar), with=FALSE])
    setnames(predselect, c("unit", "domain"), c(unitvar, domain))
    returnlst <- list(unitest = unitest, predselect = predselect)

    if (getweights) {
      weights <- data.frame(unit=unit, domain=1, id=cuniqueid, weights=NA)
      setnames(weights, "id", cuniqueid)
      returnlst$weights <- weights
    }

    return(returnlst)

  }

#print(c(cuniqueid, strvar, prednames.unit))
  setkeyv(dat.unit, cuniqueid)
  pltassgn.unit <- unique(dat[dat[[unitvar]] == unit, c(cuniqueid, strvar, prednames.unit), with=FALSE])
  setkeyv(pltassgn.unit, cuniqueid)

  unitlut.unit <- unitlut[unitlut[[unitvar]] == unit, ]
  N.unit <-  npixels[["npixels"]][npixels[[unitvar]] == unit]

  if (MAmethod == "gregRatio") {
    area.unit <- unitarea[["AREAUSED"]][unitarea[[unitvar]] == unit]
  } else {
    area.unit <- NULL
  }

  doms <- unique(dat.unit[!is.na(get(domain)) & get(domain) != "NA NA"][[domain]])

  unitestlst <- lapply(doms, MAest.dom,
                       dat = dat.unit,
                       cuniqueid = cuniqueid,
                       unitlut = unitlut.unit,
                       pltassgn = pltassgn.unit,
                       esttype = esttype,
                       MAmethod = MAmethod,
                       strvar = strvar,
                       prednames = prednames.unit,
                       domain = domain,
                       N = N.unit,
                       area = area.unit,
                       response = response,
                       response_d = response_d,
                       FIA = FIA,
                       modelselect = modelselect,
                       getweights = getweights,
                       var_method = var_method,
                       quiet = quiet)


  unitest <- data.table(unit=unit, do.call(rbind, sapply(unitestlst, '[', "est")))
  setnames(unitest, "unit", unitvar)

  returnlst <- list(unitest=unitest)

  if (getweights) {
    weights <- data.table(unit=unit, do.call(rbind, sapply(unitestlst, '[', "weights")))
    setnames(weights, "unit", unitvar)
    returnlst$weights <- weights
  }

  if (MAmethod %in% c("greg", "gregEN")) {
    predselect <- data.table(unit=unit, do.call(rbind, sapply(unitestlst, '[', "predselect")))
    setnames(predselect, "unit", unitvar)
    returnlst$predselect <- predselect
  }

  return(returnlst)

}


