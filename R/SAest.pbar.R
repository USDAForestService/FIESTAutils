#' @rdname estimation_desc
#' @export
SAest.unit <- function(fmla.dom.unit, pltdat.dom, dunitlut.dom, yn, SApackage,
	dunitvar, predselect.unit, prior = NULL) {

  ## Set global variables
  DOMAIN <- NULL

  pltdat.unit <- data.frame(pltdat.dom)
  dunitlut.unit <- data.frame(dunitlut.dom)


  if (SApackage == "JoSAE") {

    ## create linear mixed model
    ## note: see http://www.win-vector.com/blog/2018/09/r-tip-how-to-pass-a-formula-to-lm/
    dom.lme <- eval(bquote( nlme::lme(.(fmla.dom.unit), data=pltdat.unit, random=~1|DOMAIN)))

    ## calculate the variance of the EBLUP estimate
    est.unit <- tryCatch(JoSAE::eblup.mse.f.wrap(domain.data = dunitlut.unit,
                                        lme.obj = dom.lme, debug=FALSE),
                         error=function(err) {
                           message("there was an error in unit-level JoSAE model", "\n")
                           return(NULL)
                         } )
    if (is.null(est.unit)) {
      return(NULL)
    }

    rm(dunitlut.unit)
    rm(fmla.dom.unit)
    rm(pltdat.unit)
    gc()

    return(est.unit)
  }

  if (SApackage == "sae") {
    xpop <- dunitlut.unit[,c('DOMAIN', predselect.unit)]
    popsize <- dunitlut.unit[, c("DOMAIN", "npixels")]

    est.unit <- tryCatch(suppressMessages(sae::pbmseBHF(formula = fmla.dom.unit,
                              dom = DOMAIN,
                              selectdom = unique(xpop$DOMAIN),
                              meanxpop = xpop,
                              popnsize = popsize,
                            method = "REML",
                              data = pltdat.unit,
                              B = 100)),
                         error=function(err) {
                           message("there was an error in unit-level sae model", "\n")
                           return(NULL)
                         } )
    if (is.null(est.unit)) {
      return(NULL)
    }

    rm(dunitlut.unit)
    rm(fmla.dom.unit)
    rm(pltdat.unit)
    gc()

    return(est.unit)
  }

  if (SApackage == "hbsae") {
    #prior = function(x) 1 / (sqrt(x) * (1 + x))
    xpophb <- model.matrix(fmla.dom.unit[-2], dunitlut.unit)
    rownames(xpophb) <- dunitlut.unit[[dunitvar]]

    if (is.null(prior)) {
      est.unit <- tryCatch(hbsae::fSAE.Unit(
        y = pltdat.unit[[yn]],
        X = model.matrix(fmla.dom.unit[-2], pltdat.unit),
        area = pltdat.unit$DOMAIN,
        #Narea = dunitlut.unit$npixels,
        Xpop = xpophb,
        silent = FALSE
      ),
      error=function(err) {
        #message(err, "\n")
        return(NULL)
      } )
    } else {
      est.unit <- tryCatch(hbsae::fSAE.Unit(
        y = pltdat.unit[[yn]],
        X = model.matrix(fmla.dom.unit[-2], pltdat.unit),
        area = pltdat.unit$DOMAIN,
        #Narea = dunitlut.unit$npixels,
        fpc = FALSE,
        Xpop = xpophb,
        prior = prior,
        silent = FALSE
      ),
      error=function(err) {
        message("there was an error in unit-level hbsae model", "\n")
        return(NULL)
      } )
    }
    if (is.null(est.unit)) {
      return(NULL)
    }

    rm(dunitlut.unit)
    rm(fmla.dom.unit)
    rm(pltdat.unit)
    gc()

    return(est.unit)
  }
}

#' @rdname estimation_desc
#' @export
SAest.area <- function(fmla.dom.area, pltdat.area, dunitlut.area, cuniqueid,
	dunitvar="DOMAIN", predselect.area, yn, SApackage, prior=NULL) {

  if (SApackage == "JoSAE") {
    xpop.dom <- paste0(predselect.area, ".X.pop")
    fmla.dom.area2 <- as.formula(paste(paste0(yn, ".ybar.i"),
                                  paste(xpop.dom, collapse= "+"), sep="~"))
    res <-
      tryCatch(JoSAE::sae.ul.f(samp.data = pltdat.area,
                      population.data = dunitlut.area,
                      k.ij = rep(1,nrow(pltdat.area)),
                      formula = fmla.dom.area,
                      domain.col = dunitvar,
                      sample.id.col = cuniqueid,
                      neg.sfrac = TRUE),
               error=function(err) {
                 message("there was an error in area-level JoSAE model", "\n")
                 return(NULL)
               } )

    if (is.null(res)) {
      return(NULL)
    }
    ## To add space to messages
    cat("\n")

    #building pieces
    partA <- res$data$samp.agg.X.pop[,c("domain.id","n.i",
                                        paste0(yn,".ybar.i"),
                                        xpop.dom)]
    partB <- res$est$se[,c("domain.id","se.srs")]
    dat.al <- merge(partA, partB)
    est.area <-
      tryCatch(JoSAE::sae.al.f(
                domain.id=dat.al$domain.id,
				        n.i=dat.al$n.i,
				        psi.i=dat.al$se.srs^2,
				        formula=fmla.dom.area2,
				        data=dat.al,
				        b.i=rep(1, nrow(dat.al)),
				        type="RE"),
      error=function(err) {
                message("there was an error in area-level JoSAE model", "\n")
                return(NULL)
              } )
    if (is.null(est.area)) {
      return(NULL)
    }

    rm(dunitlut.area)
    rm(fmla.dom.area)
    rm(pltdat.area)
    gc()

    return(list(JoSAEest=est.area, JoSAE.al=dat.al))
  }

  if (SApackage == "sae") {

    nm.var <- paste0(yn, ".var")
    dunitlut.area$var <- dunitlut.area[[nm.var]] / (dunitlut.area$n.total)

    est.area <- tryCatch(sae::mseFH(
      formula = fmla.dom.area,
      vardir = var,
      #method = "FH",
      method = "REML",
      data = dunitlut.area,
      MAXITER=250
      ),
      error=function(err) {
        message("there was an error in area-level sae model", "\n")
        return(NULL)
      } )

    if (is.null(est.area)) {
      return(NULL)
    }

    rm(dunitlut.area)
    rm(fmla.dom.area)
    rm(pltdat.area)
    gc()

    return(est.area)
  }

  if (SApackage == "hbsae") {

    prior = function(x) 1 / (sqrt(x) * (1 + x))
    nm.var <- paste0(yn, ".var")
    dunitlut.area$var <- dunitlut.area[[nm.var]] / dunitlut.area$n.total

    y <- dunitlut.area[[yn]]
    names(y) <- dunitlut.area[[dunitvar]]

    X <- model.matrix(fmla.dom.area[-2], dunitlut.area)
    rownames(X) <- dunitlut.area[[dunitvar]]

    if (is.null(prior)) {
      est.area <- tryCatch(hbsae::fSAE.Area(
        est.init = y,
        var.init = dunitlut.area[["var"]],
        X = X,
        silent=TRUE
      ),
      error=function(err) {
        message("there was an error in area-level hbsae model", "\n")
        return(NULL)
      } )

    } else {
      est.area <- tryCatch(hbsae::fSAE.Area(
        est.init = y,
        var.init = dunitlut.area[["var"]],
        X = X,
        prior = prior,
        silent=TRUE
      ),
      error=function(err) {
        message("there was an error in area-level hbsae model", "\n")
        return(NULL)
      } )
    }

    if (is.null(est.area)) {
      return(NULL)
    }

    rm(dunitlut.area)
    rm(fmla.dom.area)
    rm(pltdat.area)
    gc()

    return(est.area)
  }
}

#' @rdname estimation_desc
#' @export
SAest <- function(yn="CONDPROP_ADJ", dat.dom, cuniqueid, pltassgn,
	dunitlut, prednames=NULL, dunitvar="DOMAIN",
	SAmethod="unit", SApackage="JoSAE", yd=NULL, ratiotype="PERACRE",
	largebnd.val=NULL, showsteps=FALSE, savesteps=FALSE, stepfolder=NULL,
	prior=NULL, modelselect=TRUE, multest=TRUE) {

  ########################################################################################
  ## DESCRIPTION: Gets estimates from JoSAE
  ## PARAMETERS:
  ## yn 		- response (numerator)
  ## pdomdat 	- plt/domain-level data set
  ## dunitlut	- predictor summaries
  ## prednames	- predictor variable names to use in model
  ##
  ## VALUES:
  ## nhat		- estimate proportion of land covered by condition, for numerator
  ## nhat.var	- variance of estimated proportion, for numerator
  ## dhat		- estimate proportion of land covered by condition, for denominator
  ## dhat.var	- variance of estimated proportion, for denominator
  ## covar		- covariance of numerator and denominator
  ########################################################################################
  #dunitvar <- "DOMAIN"
  predselect.area=predselect.unit <- NULL
  SAobjlst <- list()

  if (modelselect) {
    if (!"mase" %in% rownames(installed.packages()))
    stop("variable selection requires package mase")
  }

  ## Merge dat.dom to pltassgn
  pltdat.dom <- dat.dom[pltassgn]
  pltdat.dom[is.na(pltdat.dom[[yn]]), (yn) := 0]

  ## Add mean response to dunitlut for Area-level estimates
  datmean <- pltdat.dom[, list(mean=mean(get(yn), na.rm=TRUE),
			mean.var=var(get(yn), na.rm=TRUE)), by=dunitvar]
  setkeyv(datmean, dunitvar)
  dunitlut.dom <- merge(dunitlut, datmean, by=dunitvar)
  setnames(dunitlut.dom, c("mean", "mean.var"), c(yn, paste0(yn, ".var")))


###################
##### TESTING #####
###################
#pltdom <- fread("E:/workspace/FIESTA/FIESTA_SA/ecosubsection_test/pltdom.csv")
#dunitlut <- fread("E:/workspace/FIESTA/FIESTA_SA/ecosubsection_test/dunitlut.csv")
#pltdat.dom <- pltdom[pltdom$PROVINCE == 251, ]
#dunitlut.dom <- dunitlut[dunitlut$PROVINCE == 251, ]
#dunitvar <- "DOMAIN"
#yn <- "DRYBIO_AG_TPA_ADJ"
#prednames <- c("tcc", "elev", "ppt", "tmean", "tmin01", "tnt2")
#standardize <- TRUE
#modelselect <- TRUE
#SApackage <- "JoSAE"
#SAmethod <- "area"
#cuniqueid <- "PLT_CN"

  if (!"data.table" %in% class(pltdat.dom)) {
    pltdat.dom <- setDT(pltdat.dom)
  }
  if (!"data.table" %in% class(dunitlut.dom)) {
    dunitlut.dom <- setDT(dunitlut.dom)
  }

  ## Calculate number of non-zero plots
  NBRPLT.gt0 <- pltdat.dom[, sum(get(yn) > 0), by=dunitvar]
  setnames(NBRPLT.gt0, "V1", "NBRPLT.gt0")
  setkeyv(NBRPLT.gt0, dunitvar)


  ## Check if all plots are zero
  if (sum(pltdat.dom[[yn]]) == 0) {
    message(yn, " has all 0 values... returning NULL")
    if (multest) {
      est <- data.table(dunitlut.dom[[dunitvar]], AOI=dunitlut.dom$AOI,
		DIR=NA, DIR.se=NA, JU.Synth=NA, JU.GREG=NA, JU.GREG.se=NA,
		JU.EBLUP=NA, JU.EBLUP.se.1=NA, 
		hbsaeU=NA, hbsaeU.se=NA, 
		JA.synth=NA, JA.synth.se=NA, 
		saeA=NA, saeA.se=NA, 
		hbsaeA=NA, hbsaeA.se=NA, NBRPLT=dunitlut.dom$n.total)
      setnames(est, "V1", dunitvar)

    } else {
      est <- data.table(dunitlut.dom[[dunitvar]], DIR=NA, DIR.se=NA)
      setnames(est, "V1", dunitvar)

      if (SAmethod == "unit") {
        if (SApackage == "JoSAE") {
          est <- data.table(est, JU.Synth=NA, JU.GREG=NA, JU.GREG.se=NA,
			JU.EBLUP=NA, JU.EBLUP.se.1=NA, NBRPLT=dunitlut.dom$n.total)
        } else if (SApackage == "hbsae") {
          est <- data.table(est, hbsaeU=NA, hbsaeU.se=NA, 
			NBRPLT=dunitlut.dom$n.total)
        }
      } else if (SAmethod == "area") {
        if (SApackage == "JoSAE") {
          est <- data.table(est, JA.synth=NA, JA.synth.se=NA, 
			NBRPLT=dunitlut.dom$n.total)
        } else if (SApackage == "sae") {
          est <- data.table(est, sae=NA, sae.se=NA, 
			NBRPLT=dunitlut.dom$n.total)
        } else if (SApackage == "hbsae") {
          est <- data.table(est, hbsae=NA, hbsae.se=NA, 
			NBRPLT=dunitlut.dom$n.total)
        }
      }
    }
    ## Merge NBRPLT.gt0
    est <- merge(est, NBRPLT.gt0, by="DOMAIN")

    ## Merge AOI
    if (!"AOI" %in% names(est) && "AOI" %in% names(dunitlut.dom)) {
      est <- merge(est, dunitlut.dom[, c("DOMAIN", "AOI")], by="DOMAIN")
    }
    return(list(est=est, predselect.unit=NULL, predselect.area=NULL,
		pltdat.dom=pltdat.dom, dunitlut.dom=dunitlut.dom))
  }

  ## Variable selection for area and unit-level estimators
  ## Check number of predictors... must be n-1 less than number of dunits
  ## using variable selection - mase::gregElasticNet.

  if (multest || SAmethod == "unit") {
    predselect.unitdt <- dunitlut.dom[FALSE, prednames, with=FALSE, drop=FALSE]

    ## Define number of maximum predictors for unit-level models
    maxpreds.unit <- nrow(pltdat.dom) - 2

    ## Variable selection unit-level estimators
    if (modelselect) {

      ## Define number of maximum predictors for unit-level model selection (n-1)
      maxpreds.unit.modselect <- nrow(pltdat.dom) - 1

      ## Check number of predictors using correlation with response
      ## (n-1 less than number of plots)
      if (length(prednames) > maxpreds.unit.modselect) {
        warning("number of predictors is greater than number of domains... ", 
			"subsetting based on correlation coefficients")
        prednames.cor <- names(sort(abs(cor(pltdat.dom[[yn]], 
				pltdat.dom[, prednames, with=FALSE]))[1,], decreasing=TRUE))
        prednames <- prednames.cor[1:maxpreds.unit.modselect]
      }

      ## Define number cvfolds for area-level model selection
      cvfolds <- ifelse(nrow(pltdat.dom) <= 40, round(nrow(pltdat.dom)/4), 10)

      ## Select predictors for unit-level models using elastic net via mase
      predselect.unitlst <- suppressMessages(preds.select(y=yn,
                            plt=pltdat.dom, auxlut=dunitlut.dom, 
                            prednames=prednames, cvfolds=cvfolds))
      predselect.unit <- predselect.unitlst$preds.enet
      predselect.unit.coef <- predselect.unitlst$preds.coef

      ## Get number of predictors with elastic net coefficients greater than 0
      predselect.unit.coef.absgt0 <- predselect.unit[abs(predselect.unit.coef) > 0]

      ## Check number of predictors... must be n-2 less than number of dunits
      ## because of the area random effect term
      if (length(predselect.unit.coef.absgt0) > maxpreds.unit) {
        warning("number of predictors must be n-2 less than number of domains... ", 
			"subsetting base on model-selection coefficients")
        prednames.unit <- names(sort(abs(predselect.unit.coef), decreasing=TRUE)[1:maxpreds.unit])
      } else {
        prednames.unit <- predselect.unit.coef.absgt0
      }
    } else {

      ## Check number of predictors using correlation with response
      ## (n-1 less than number of plots)
      if (length(prednames) > maxpreds.unit) {
        warning("number of predictors must be n-2 less than number of domains... ", 
			"subsetting base on correlation coefficients")
        prednames.cor <- names(sort(abs(cor(pltdat.dom[[yn]], 
				pltdat.dom[, prednames, with=FALSE]))[1,], decreasing=TRUE))
        prednames <- prednames.cor[1:maxpreds.unit]
      } else {
        predselect.unit <- prednames
      }
    } 
  }
  if (multest || SAmethod == "area") {
    predselect.areadt <- dunitlut.dom[FALSE, prednames, with=FALSE, drop=FALSE]

    ## Define number of maximum predictors for area-level models
    maxpreds.area <- length(unique(dunitlut.dom[[dunitvar]])) - 2

    ## Variable selection for area and unit-level estimators
    if (modelselect) {

      ## Define number of maximum predictors for area-level model selection (n-1)
      maxpreds.area.modselect <- length(unique(dunitlut.dom[[dunitvar]])) - 1

      ## Check number of predictors using correlation with response
      ## (n-1 less than number of plots)
      if (length(prednames) > maxpreds.area.modselect) {
        warning("number of predictors is greater than number of domains... ", 
			"subsetting based on correlation coefficients")
        prednames.cor <- names(sort(abs(cor(dunitlut.dom[[yn]], 
				dunitlut.dom[, prednames, with=FALSE]))[1,], decreasing=TRUE))
        prednames <- prednames.cor[1:maxpreds.area.modselect]
      }

      ## Define number cvfolds for area-level model selection
      cvfolds <- ifelse(nrow(dunitlut.dom) <= 40, round(nrow(dunitlut.dom)/4), 10)

      ## Select predictors for area-level models using elastic net via mase
      predselect.arealst <- suppressMessages(preds.select(y=yn,
                            plt=dunitlut.dom, auxlut=dunitlut.dom, 
                            prednames=prednames, cvfolds=cvfolds))
      predselect.area <- predselect.arealst$preds.enet
      predselect.area.coef <- predselect.arealst$preds.coef

      ## Get number of predictors with elastic net coefficients greater than 0
      predselect.area.coef.absgt0 <- predselect.area[abs(predselect.area.coef) > 0]

      ## Check number of predictors... must be n-2 less than number of dunits
      ## because of the area random effect term
      if (length(predselect.area.coef.absgt0) > maxpreds.area) {
        warning("number of predictors must be n-2 less than number of domains... ", 
			"subsetting base on model-selection coefficients")
        prednames.area <- names(sort(abs(predselect.area.coef), decreasing=TRUE)[1:maxpreds.area])

      } else {
        prednames.area <- predselect.area.coef.absgt0
      }

    } else {
      ## Check number of predictors using correlation with response
      ## (n-1 less than number of plots)
      if (length(prednames) > maxpreds.area) {
        warning("number of predictors must be n-2 less than number of domains... ", 
			"subsetting base on correlation coefficients")
        prednames.cor <- names(sort(abs(cor(dunitlut.dom[[yn]], 
				dunitlut.dom[, prednames, with=FALSE]))[1,], decreasing=TRUE))
        prednames <- prednames.cor[1:maxpreds.area]
      } else {
        predselect.area <- prednames
      }
    }
  }

    # NOTE: still need to check for equivalent unit-level issue. Much less common though
    ## Check number of predictors... must be n-2 less than number of dunits
    ########################################################################
#    if (SAmethod == "area") {
#      maxpreds <- length(unique(dunitlut[[dunitvar]])) - 2
#      if (length(prednames) > maxpreds) {
#        maxtxt <- ifelse(maxpreds == 1, "1 predictor", paste(maxpreds, "predictors"))
#        stop("can only use ", maxtxt, " (number of domain units - 2)")
#     }
#    }

  if (length(predselect.area) > 0 || length(predselect.unit) > 0) {
    if (showsteps || savesteps) {
      ylab <- ifelse(yn == "CONDPROP_ADJ", "FOREST_prob",
			ifelse(yn == "TPA_UNADJ", "COUNT", yn))
      pheight <- 6
      pwidth <- 6
      rnbr=cbnr <- 1
      if (length(prednames) > 1) {
        nbrpreds <- length(prednames)
        if (nbrpreds == 2) {
          rnbr <- 1
          cnbr <- 2
          pwidth <- 8
        } else if (nbrpreds == 3) {
          rnbr <- 1
          cnbr <- 3
          pwidth <- 10
        } else if (nbrpreds == 4) {
          rnbr <- 2
          cnbr <- 2
          pwidth <- 8
          pheight <- 8
        } else if (nbrpreds %in% 5:6) {
          rnbr <- 2
          cnbr <- 3
          pwidth <- 10
          pheight <- 8
        } else if (nbrpreds %in% 7:8) {
          rnbr <- 2
          cnbr <- 4
          pwidth <- 12
          pheight <- 8
        } else if (nbrpreds %in% 9) {
          rnbr <- 3
          cnbr <- 3
          pwidth <- 10
          pheight <- 8
        } else if (nbrpreds > 9) {
          rnbr <- 4
          cnbr <- 4
          pwidth <- 12
          pheight <- 10
        }
      }
      if (showsteps) {
        if (length(predselect.unit) > 0) {
          ## unit-level selection
          grDevices::dev.new()
          graphics::par(mfrow=c(rnbr,cnbr))
          for (pred in prednames) {
            main2 <- ifelse(pred %in% predselect.unit, "selected", "not selected")
            if (!is.null(largebnd.val) && largebnd.val != 1) {
              #main <- paste0(largebnd.val, ": ", ylab, " - ", main2)
              main <- paste0(largebnd.val, " - ", main2)
            } else {
              main <- main2
            }
            plot(dunitlut.dom[[pred]], dunitlut.dom[[yn]], xlab=pred, ylab=ylab, main=main)
          }
        }

        if (length(predselect.area) > 0) {
          ## area-level selection
          grDevices::dev.new()
          graphics::par(mfrow=c(rnbr,cnbr))
          for (pred in prednames) {
            main2 <- ifelse(pred %in% predselect.area, "selected", "not selected")
            if (!is.null(largebnd.val) && largebnd.val != 1) {
              #main <- paste0(largebnd.val, ": ", ylab, " - ", main2)
              main <- paste0(largebnd.val, " - ", main2)
            } else {
              main <- main2
            }
            plot(dunitlut.dom[[pred]], dunitlut.dom[[yn]], xlab=pred, ylab=ylab, main=main)
          }
        }
      }

      if (savesteps) {
        if (length(predselect.unit) > 0) {
          ## unit-level selection
          out_layer <- paste0("SApred_unit_", ylab)
          jpgfn <- paste0(stepfolder, "/", out_layer, ".jpg")
          grDevices::jpeg(jpgfn, res=300, units="in", width=pwidth, height=pheight)

          graphics::par(mfrow=c(rnbr,cnbr))
          for (pred in prednames) {
            main2 <- ifelse(pred %in% predselect.unit, "selected", "not selected")
            if (!is.null(largebnd.val) && largebnd.val != 1) {
              #main <- paste0(largebnd.val, ": ", ylab, " - ", main2)
              main <- paste0(largebnd.val, " - ", main2)
            } else {
              main <- main2
            }
            plot(dunitlut.dom[[pred]], dunitlut.dom[[yn]], xlab=pred, ylab=ylab, main=main)
          }
          grDevices::dev.off()
        }

        if (length(predselect.area) > 0) {

          ## area-level selection
          out_layer <- paste0("SApred_area", ylab)
          jpgfn <- paste0(stepfolder, "/", out_layer, ".jpg")
          grDevices::jpeg(jpgfn, res=300, units="in", width=pwidth, height=pheight)

          graphics::par(mfrow=c(rnbr,cnbr))
          for (pred in prednames) {
            main2 <- ifelse(pred %in% predselect.area, "selected", "not selected")
            if (!is.null(largebnd.val) && largebnd.val != 1) {
              #main <- paste0(largebnd.val, ": ", ylab, " - ", main2)
              main <- paste0(largebnd.val, " - ", main2)
            } else {
              main <- main2
            }
            plot(dunitlut.dom[[pred]], dunitlut.dom[[yn]], xlab=pred, ylab=ylab, main=main)
          }
          grDevices::dev.off()
        }
      }
    }
  }

  ## Get direct estimate
  nm.var <- paste0(yn, ".var")
  dunitlut.dom$DIR <- dunitlut.dom[[yn]]
  dunitlut.dom$DIR.se <- sqrt(dunitlut.dom[[nm.var]] / dunitlut.dom$n.total)
  est <- dunitlut.dom[,c("DOMAIN", "n.total", "DIR", "DIR.se")]  
  setnames(est, "n.total", "NBRPLT")

  if (length(predselect.unit) > 0) {
    message("using following predictors for unit-level models...", toString(predselect.unit))

    ## create model formula with predictors
    ## note: the variables selected can change depending on the order in original formula (fmla)
    fmla.dom.unit <- stats::as.formula(paste(yn, paste(predselect.unit, collapse= "+"), sep="~"))

    ### unit-level - JoSAE estimates               
    if (multest || SApackage == "JoSAE") {
      ## NOTE: changed prednames=prednames.select to prednames
      unit.JoSAE.obj <- tryCatch(SAest.unit(fmla.dom.unit=fmla.dom.unit, 
                              pltdat.dom=pltdat.dom, 
                              dunitlut.dom=dunitlut.dom, 
                              yn=yn, 
                              SApackage="JoSAE", 
                              dunitvar=dunitvar, 
                              predselect.unit=predselect.unit, 
                              prior=prior),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )

      if (is.null(unit.JoSAE.obj)) {
        unit.JoSAE <- data.frame(DOMAIN=dunitlut.dom[[dunitvar]], 
                               JU.Synth=NA, JU.GREG=NA, JU.GREG.se=NA, 
                               JU.EBLUP=NA, JU.EBLUP.se.1=NA)
        setnames(unit.JoSAE, "DOMAIN", dunitvar)
      } else {
        ## subset dataframe before returning
#        unit.JoSAE <- unit.JoSAE.obj[,c("DOMAIN.domain", "n.i.sample",
#                         yn, "sample.se", "Synth",
#                         "GREG", "GREG.se",
#                         "EBLUP","EBLUP.se.1")]
#        names(unit.JoSAE) <- c("DOMAIN", "NBRPLT", "DIR", "DIR.se", "JU.Synth", "JU.GREG",
#                      "JU.GREG.se", "JU.EBLUP", "JU.EBLUP.se.1")

        unit.JoSAE <- unit.JoSAE.obj[,c("DOMAIN.domain", 
                         "Synth", "GREG", "GREG.se",
                         "EBLUP","EBLUP.se.1")]
        names(unit.JoSAE) <- c("DOMAIN", "JU.Synth", "JU.GREG",
                      "JU.GREG.se", "JU.EBLUP", "JU.EBLUP.se.1")

      }  

      est <- merge(est, unit.JoSAE, by=dunitvar)
      SAobjlst$unit.JoSAE.obj <- unit.JoSAE.obj
    }

    ## unit-level - hbsae estimates               
    if (multest || SApackage == "hbsae") {
      unit.hbsae.obj <- tryCatch(SAest.unit(fmla.dom.unit=fmla.dom.unit, 
                              pltdat.dom=pltdat.dom, 
                              dunitlut.dom=dunitlut.dom, 
                              yn=yn, SApackage="hbsae", 
                              dunitvar=dunitvar, 
                              predselect.unit=predselect.unit, 
                              prior=prior),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
      if (is.null(unit.hbsae.obj)) {
        unit.hbsae <- data.frame(DOMAIN=dunitlut.dom[[dunitvar]], 
                               hbsaeU=NA, hbsaeU.se=NA)
        setnames(unit.hbsae, "DOMAIN", dunitvar)
      } else {
        unit.hbsae <- data.frame(
          DOMAIN = unit.hbsae.obj$sampledAreaNames,
          hbsaeU = unit.hbsae.obj$est,
          hbsaeU.se = sqrt(unit.hbsae.obj$mse)
        )
        #unit.hbsae <- merge(unit.hbsae, dunitlut.dom[, c(dunitvar, "n.total"), with=FALSE],
        #           by.x="DOMAIN", by.y=dunitvar)
        #names(unit.hbsae)[names(unit.hbsae) == "DOMAIN"] <- dunitvar
        #names(unit.hbsae)[names(unit.hbsae) == "n.total"] <- "NBRPLT"
        unit.hbsae <- unit.hbsae[, c(dunitvar, "hbsaeU", "hbsaeU.se")]
      } 

      ## Merge estimates
      est <- merge(est, unit.hbsae[, c(dunitvar, "hbsaeU", "hbsaeU.se")], by=dunitvar)
      SAobjlst$unit.hbsae.obj <- unit.hbsae.obj
  
      rm(unit.JoSAE)
      rm(unit.hbsae)
    }
  } else {

    #message("no predictors were selected for unit-level models... returning NAs")

    if (multest) {
      est <- data.frame(est, JU.Synth=NA, 
                      JU.GREG=NA, JU.GREG.se=NA, 
                      JU.EBLUP=NA, JU.EBLUP.se.1=NA, 
                      hbsaeU=NA, hbsaeU.se=NA)
      setnames(est, "DOMAIN", dunitvar)

    } else {
      if (SAmethod == "unit") {
        if (SApackage == "JoSAE") {
          est <- data.frame(est, JU.Synth=NA, 
                      JU.GREG=NA, JU.GREG.se=NA, 
                      JU.EBLUP=NA, JU.EBLUP.se.1=NA)
        } else if (SApackage == "hbsae") {
          est <- data.frame(est, hbsaeU=NA, hbsaeU.se=NA)
        }
      }
    }   
  }
 
  if (length(predselect.area) > 0) {
    message("using following predictors for area-level models...", toString(predselect.unit))

    ## create model formula with predictors
    ## note: the variables selected can change depending on the order in original formula (fmla)
    fmla.dom.area <- stats::as.formula(paste(yn, paste(predselect.area, collapse= "+"), sep="~"))
    
    
    dunitlut.dom <- data.frame(dunitlut.dom)
    nm.var <- paste0(yn, ".var")
    dunitlut.NA <- dunitlut.dom[is.na(dunitlut.dom[[nm.var]]) | dunitlut.dom[[nm.var]] < 0.001, ]
    #dunitlut.NA <- dunitlut.dom[is.na(dunitlut.dom[[nm.var]]) | dunitlut.dom[[nm.var]] == 0, ]
    dunitNAids <- dunitlut.NA[[dunitvar]]
    dunitids <-  dunitlut.dom[!dunitlut.dom[[dunitvar]] %in% dunitNAids, dunitvar]
    dunitlut.area <- dunitlut.dom[dunitlut.dom[[dunitvar]] %in% dunitids, ]
    pltdat.area <- data.frame(pltdat.dom[pltdat.dom[[dunitvar]] %in% dunitids, ])
    
    ## area-level - JoSAE estimates   
    if (multest || SApackage == "JoSAE") {  
 
      area.JoSAE.objlst <- tryCatch(SAest.area(fmla.dom.area=fmla.dom.area,
                                      pltdat.area=pltdat.area,
                                      dunitlut.area=dunitlut.area,
                                      cuniqueid=cuniqueid,
                                      dunitvar=dunitvar,
                                      predselect.area=predselect.area,
                                      yn=yn, SApackage="JoSAE"),
                           error=function(err) {
                             message(err, "\n")
                             return(NULL)
                             } )
      if (is.null(area.JoSAE.objlst)) {
#        area.JoSAE <- data.frame(DOMAIN=dunitlut.dom[[dunitvar]],
#                               NBRPLT=dunitlut.dom$n.total,
#                               DIR=NA, DIR.se=NA, JFH=NA, JFH.se=NA,
#                               JA.synth=NA, JA.synth.se=NA)
        area.JoSAE.obj <- NULL
        area.JoSAE <- data.frame(DOMAIN=dunitlut.dom[[dunitvar]],
#                               NBRPLT=dunitlut.dom$n.total,
                               JFH=NA, JFH.se=NA,
                               JA.synth=NA, JA.synth.se=NA)
        setnames(area.JoSAE, "DOMAIN", dunitvar)
      } else {
        area.JoSAE.obj <- area.JoSAE.objlst$JoSAEest
        area.JoSAE.al <- area.JoSAE.objlst$JoSAE.al
        area.JoSAE <- area.JoSAE.obj$results[,c(1, 4:7)]
#        names(area.JoSAE) <- c("DOMAIN", "DIR", "DIR.se", "JFH", "JFH.se",
#                      "JA.synth", "JA.synth.se")
        names(area.JoSAE) <- c("DOMAIN", "JFH", "JFH.se",
                      "JA.synth", "JA.synth.se")
        ## To add space to messages
        cat("\n")
      
#        area.JoSAE <- merge(area.JoSAE, area.JoSAE.al[, c("domain.id", "n.i")],
#                   by.x="DOMAIN", by.y="domain.id")
#        names(area.JoSAE)[names(area.JoSAE) == "DOMAIN"] <- dunitvar
#        names(area.JoSAE)[names(area.JoSAE) == "n.i"] <- "NBRPLT"
#        area.JoSAE <- area.JoSAE[, c(dunitvar, "NBRPLT", "DIR", "DIR.se", "JFH", "JFH.se",
#                     "JA.synth", "JA.synth.se")]
        area.JoSAE <- area.JoSAE[, c(dunitvar, "JFH", "JFH.se",
                     "JA.synth", "JA.synth.se")]
 
     
        if (nrow(dunitlut.NA) > 0) {
#          est.NA <- data.table(dunitlut.NA[[dunitvar]], NBRPLT=dunitlut.NA$n.total,
#                             DIR=NA, DIR.se=NA,
#                             JFH=NA, JFH.se=NA, JA.synth=NA, JA.synth.se=NA)
          est.NA <- data.table(dunitlut.NA[[dunitvar]], 
                             JFH=NA, JFH.se=NA, JA.synth=NA, JA.synth.se=NA)

          setnames(est.NA, "V1", dunitvar)
          area.JoSAE <- rbindlist(list(area.JoSAE, est.NA))
          setorderv(area.JoSAE, dunitvar)
        }
      }
 
      ## Merge estimates
      est <- merge(est, area.JoSAE[, c("DOMAIN", "JFH", "JFH.se", "JA.synth", "JA.synth.se")],
 			by=dunitvar)
      SAobjlst$area.JoSAE.obj <- area.JoSAE.obj
      rm(area.JoSAE)
    }

    ## area-level - sae estimates   
    if (multest || SApackage == "sae") {            
      area.sae.obj <- tryCatch(SAest.area(fmla.dom.area=fmla.dom.area,
                                    pltdat.area=pltdat.area,
                                    dunitlut.area=dunitlut.area,
                                    cuniqueid=cuniqueid,
                                    dunitvar=dunitvar,
                                    predselect.area=predselect.area,
                                    yn=yn, SApackage="sae"),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
      if (is.null(area.sae.obj)) {
        area.sae <- data.frame(DOMAIN=dunitlut.dom[[dunitvar]],
                             saeA=NA, saeA.se=NA)
        setnames(area.sae, "DOMAIN", dunitvar)
      } else {
      
        if (length(area.sae.obj$est$eblup) == 1 && is.na(area.sae.obj$est$eblup)) {
          area.sae <- data.frame(
            DOMAIN = dunitlut.area[[dunitvar]],
            saeA = rep(NA, length(dunitlut.area[[dunitvar]])),
            saeA.se = rep(NA, length(dunitlut.area[[dunitvar]]))
          )
        } else {
          area.sae <- data.frame(
            DOMAIN = dunitlut.area[[dunitvar]],
            saeA = area.sae.obj$est$eblup[,1],
            saeA.se = sqrt(area.sae.obj$mse)
          )
        }
            
        if (nrow(dunitlut.NA) > 0) {
          est.NA <- data.table(dunitlut.NA[[dunitvar]], 
                             saeA=NA, saeA.se=NA)
          setnames(est.NA, "V1", dunitvar)
          area.sae <- rbindlist(list(area.sae, est.NA))
          setorderv(area.sae, dunitvar)
        }
      } 

      ## Merge estimates
      est <- merge(est, area.sae[, c("DOMAIN", "saeA", "saeA.se")],
 			by=dunitvar)
      SAobjlst$area.sae.obj <- area.sae.obj
      rm(area.sae)
    }

    ## area-level - bsae estimates   
    if (multest || SApackage == "hbsae") {            
      area.hbsae.obj <- tryCatch(SAest.area(fmla.dom.area=fmla.dom.area, 
                                pltdat.area=pltdat.area, 
                                dunitlut.area=dunitlut.area, 
                                cuniqueid=cuniqueid, 
                                dunitvar=dunitvar, 
                                predselect.area=predselect.area, 
                                yn=yn, 
                                SApackage="hbsae", 
                                prior=prior),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
      if (is.null(area.hbsae.obj)) {
        area.hbsae <- data.frame(DOMAIN=dunitlut.dom[[dunitvar]],
		    hbsaeA=NA, hbsaeA.se=NA)
        setnames(area.hbsae, "DOMAIN", dunitvar)
      } else {
      
        area.hbsae <- data.frame(
          DOMAIN = area.hbsae.obj$predAreaNames,
          hbsaeA = area.hbsae.obj$est,
          hbsaeA.se = sqrt(area.hbsae.obj$mse)
        )
      
        names(area.hbsae)[names(area.hbsae) == "DOMAIN"] <- dunitvar
        area.hbsae <- area.hbsae[, c(dunitvar, "hbsaeA", "hbsaeA.se")]
      
        if (nrow(dunitlut.NA) > 0) {
          est.NA <- data.table(dunitlut.NA[[dunitvar]], 
                             hbsaeA=NA, hbsaeA.se=NA)
          setnames(est.NA, "V1", dunitvar)
          area.hbsae <- rbindlist(list(area.hbsae, est.NA))
          setorderv(area.hbsae, dunitvar)
        }
      }
 
      ## Merge estimates
      est <- merge(est, area.hbsae[, c("DOMAIN", "hbsaeA", "hbsaeA.se")],
 			by=dunitvar)
      SAobjlst$area.hbsae.obj <- area.hbsae.obj

      rm(area.hbsae)
    }

  } else {

    if (multest) {
      message("no predictors were selected for area-level models... returning NAs")
      est.NA <- data.frame(DOMAIN=dunitlut.dom[[dunitvar]], AOI=dunitlut.dom$AOI,
			JFH=NA, JFH.se=NA, JA.synth=NA, JA.synth.se=NA,
			saeA=NA, saeA.se=NA,
			hbsaeA=NA, hbsaeA.se=NA)
      setnames(est.NA, "DOMAIN", dunitvar)

    } else {
      est.NA <- data.frame(DOMAIN=dunitlut.dom[[dunitvar]], AOI=dunitlut.dom$AOI)

      if (SApackage == "JoSAE") {
        est.NA <- data.frame(est.NA, JFH=NA, JFH.se=NA, JA.synth=NA, JA.synth.se=NA)
      } else if (SApackage == "sae") {
        est.NA <- data.frame(est.NA, saeA=NA, saeA.se=NA)
      } else if (SApackage == "hbsae") {
        est.NA <- data.frame(est.NA, hbsaeA=NA, hbsaeA.se=NA)
      }
    }
    est <- merge(est, est.NA, by="DOMAIN")
  }


  ## Merge NBRPLT.gt0
  est <- merge(est, NBRPLT.gt0, by="DOMAIN")


  ## Merge AOI
  if ("AOI" %in% names(dunitlut.dom)) {
    if (!"AOI" %in% names(est)) {
      est <- merge(est, dunitlut.dom[, c("DOMAIN", "AOI")], by="DOMAIN")
    }
    if (!"AOI" %in% names(pltdat.dom)) {
      pltdat.dom <- merge(pltdat.dom, dunitlut.dom[, c("DOMAIN", "AOI")], by="DOMAIN")
    }
  }

  gc()

  returnlst <- list(est=est, pltdat.dom=pltdat.dom, dunitlut.dom=dunitlut.dom,
		SAobjlst=SAobjlst)

  if (modelselect) {
    if (multest || SAmethod == "area") {
      predselect.areadt <- rbindlist(list(predselect.areadt,
		data.frame(t(predselect.area.coef))), fill=TRUE)
      returnlst$predselect.area <- predselect.areadt
    }
    if (multest || SAmethod == "unit") {
      predselect.unitdt <- rbindlist(list(predselect.unitdt,
		data.frame(t(predselect.unit.coef))), fill=TRUE)
      returnlst$predselect.unit <- predselect.unitdt
    }
  } else {
    if (multest || SAmethod == "area") {
      preds.area <- data.frame(t(ifelse(names(predselect.areadt) %in% predselect.area, 1, 0)))
      setnames(preds.area, names(predselect.areadt))
      predselect.areadt <- rbindlist(list(predselect.areadt, preds.area), fill=TRUE)
      returnlst$predselect.area <- predselect.areadt
    }
    if (multest || SAmethod == "unit") {
      preds.unit <- data.frame(t(ifelse(names(predselect.unitdt) %in% predselect.unit, 1, 0)))
      setnames(preds.unit, names(predselect.unitdt))
      predselect.unitdt <- rbindlist(list(predselect.unitdt, preds.unit), fill=TRUE)
      returnlst$predselect.unit <- predselect.unitdt
    }
  }

  return(returnlst)
}


########################################################################
## By domain
########################################################################
#' @rdname estimation_desc
#' @export
SAest.dom <- function(dom, dat, cuniqueid, dunitlut, pltassgn, dunitvar="DOMAIN",
		SApackage, SAmethod, prednames=NULL, domain, response=NULL,
		largebnd.val=NULL, showsteps=FALSE, savesteps=FALSE, stepfolder=NULL,
		prior=NULL, modelselect=TRUE, multest=TRUE) {

  ## Subset tomdat to domain=dom
  dat.dom <- dat[dat[[domain]] == dom,]
  if (domain != "TOTAL") {
    print(dom)
  }

  if (nrow(dat.dom) == 0 || sum(!is.na(dat.dom[[domain]])) == 0) {
    domest <- data.table(dom, matrix(c(0, rep(NA,17)), 1, 18), 0, 1)
    setnames(domest, c(domain, "NBRPLT", "DIR", "DIR.se",
		"JU.Synth", "JU.GREG", "JU.GREG.se", "JU.EBLUP", "JU.EBLUP.se.1",
		"hbsaeU", "hbsaeU.se", "JFH", "JFH.se",
		"JA.synth", "JA.synth.se", "saeA", "saeA.se",
		"hbsaeA", "hbsaeA.se", "NBRPLT.gt0", "AOI"))
    return(list(domest, predselect.unit=NULL, predselect.area=NULL, dunitlut.dom=NULL))
  }

#yn=response

  ## Apply function to each dom
  domest <- SAest(yn=response,
			dat.dom=dat.dom, cuniqueid=cuniqueid, pltassgn=pltassgn,
			dunitlut=dunitlut, dunitvar=dunitvar, prednames=prednames,
			SApackage=SApackage, SAmethod=SAmethod, largebnd.val=largebnd.val,
			showsteps=showsteps, savesteps=savesteps, stepfolder=stepfolder,
			prior=prior, modelselect=modelselect, multest=multest)

  domest$est <- data.table(dom, domest$est)
  setnames(domest$est, "dom", domain)
  domest$predselect.unit <- data.table(dom, domest$predselect.unit)
  setnames(domest$predselect.unit, "dom", domain)
  domest$predselect.area <- data.table(dom, domest$predselect.area)
  setnames(domest$predselect.area, "dom", domain)
  domest$SAobjlst <- list(domest$SAobjlst)
  names(domest$SAobjlst) <- dom

  return(domest)
}


########################################################################
## By largebnd
########################################################################
#' @rdname estimation_desc
#' @export
SAest.large <- function(largebnd.val, dat, cuniqueid, largebnd.unique,
		dunitlut, dunitvar="DOMAIN", SApackage="JoSAE",
		SAmethod="unit", domain, response, prednames=NULL,
		showsteps=FALSE, savesteps=FALSE, stepfolder=NULL,
		prior=NULL, modelselect=TRUE, multest=TRUE) {

  ## subset datasets by largebnd value (e.g., ecosection)
  dat.large <- dat[dat[[largebnd.unique]] == largebnd.val,
		c(dunitvar, cuniqueid, domain, response), with=FALSE]
  if (nrow(dat.large) == 0) stop("invalid largebnd.val")
  setkeyv(dat.large, c(dunitvar, cuniqueid))

  pltassgn.large <- unique(dat[dat[[largebnd.unique]] == largebnd.val,
		c(dunitvar, cuniqueid, prednames), with=FALSE])
  setkeyv(pltassgn.large, c(dunitvar, cuniqueid))

  ## get unique domain units and subset domain lut for largebnd value
  dunits <- sort(unique(dat.large[[dunitvar]]))
  dunitlut.large <- dunitlut[dunitlut[[dunitvar]] %in% dunits,]

  ## get unique domains
  doms <- sort(as.character(na.omit(unique(dat.large[[domain]]))))

#dat=dat.large
#dunitlut=dunitlut.large
#pltassgn=pltassgn.large
#dom=doms[i]

  estlst <- lapply(doms, SAest.dom,
			        dat=dat.large, cuniqueid=cuniqueid, pltassgn=pltassgn.large,
     			    	   dunitlut=dunitlut.large, dunitvar=dunitvar,
			        SApackage=SApackage, SAmethod=SAmethod, prednames=prednames,
			        domain=domain, response=response, largebnd.val=largebnd.val,
			        showsteps=showsteps, savesteps=savesteps, stepfolder=stepfolder,
			        prior=prior, modelselect=modelselect, multest=multest)

  if (length(doms) > 1) {
    est.large <- data.table(largebnd=largebnd.val,
				do.call(rbind, do.call(rbind, estlst)[,"est"]))
    setnames(est.large, "largebnd", largebnd.unique)

    predselect.unit <- data.table(largebnd=largebnd.val,
				do.call(rbind, do.call(rbind, estlst)[,"predselect.unit"]))
    setnames(predselect.unit, "largebnd", largebnd.unique)

    predselect.area <- data.table(largebnd=largebnd.val,
				do.call(rbind, do.call(rbind, estlst)[,"predselect.area"]))
    setnames(predselect.area, "largebnd", largebnd.unique)

    pltdat.dom <- data.table(largebnd.val, do.call(rbind,
				do.call(rbind, estlst)[,"pltdat.dom"]))
    dunitlut.dom <- data.table(largebnd.val, do.call(rbind,
				do.call(rbind, estlst)[,"dunitlut.dom"]))

    SAobjlst.dom <- do.call(rbind,
				do.call(rbind, estlst)[,"SAobjlst"])

  } else {

    #print(head(do.call(rbind, estlst)[,"est"]$est))
    est.large <- data.table(largebnd=largebnd.val,
				do.call(rbind, estlst)[,"est"]$est)
    setnames(est.large, "largebnd", largebnd.unique)

    predselect.unit <- data.table(largebnd=largebnd.val,
				do.call(rbind, estlst)[,"predselect.unit"]$predselect.unit)
    setnames(predselect.unit, "largebnd", largebnd.unique)

    predselect.area <- data.table(largebnd=largebnd.val,
				do.call(rbind, estlst)[,"predselect.area"]$predselect.area)
    setnames(predselect.area, "largebnd", largebnd.unique)

    pltdat.dom <- data.table(largebnd=largebnd.val,
				do.call(rbind, estlst)[,"pltdat.dom"]$pltdat.dom)
    setnames(pltdat.dom, "largebnd", largebnd.unique)

    dunitlut.dom <- data.table(largebnd=largebnd.val,
				do.call(rbind, estlst)[,"dunitlut.dom"]$dunitlut.dom)
    setnames(dunitlut.dom, "largebnd", largebnd.unique)
    
    SAobjlst.dom <- do.call(rbind, estlst)[,"SAobjlst"]$SAobjlst
  }

  setkeyv(est.large, dunitvar)
  setkeyv(setDT(pltdat.dom), dunitvar)
  setkeyv(setDT(dunitlut.dom), dunitvar)

  rm(estlst)
  gc()

  return(list(est.large=est.large,
			predselect.unit=predselect.unit,
			predselect.area=predselect.area,
			pltdat.dom=pltdat.dom, dunitlut.dom=dunitlut.dom,
			SAobjlst.dom=SAobjlst.dom))
}

