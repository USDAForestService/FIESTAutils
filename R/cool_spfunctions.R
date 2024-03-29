# getext - get extent of filename
# filter2qry
# recodelut
# strat.pivot
# makedummy
# preds.standardize
# gregEN.select
# preds.select

#' @rdname internal_desc
#' @export
getext <- function(x) {
  xbasename <- basename(x)
  strsplit(xbasename, paste0(basename.NoExt(xbasename), "."))[[1]][2]
}

#' @rdname internal_desc
#' @export
filter2qry <- function(filt, layernm) {
  if (grepl("==", filt)) {
    part2 <- sub("==", "=", filt)
  } else if (grepl("%in%", filt)) {
    part2 <- sub("%in% c", "in", filt)
  } else if (grepl("!=", filt)) {
    part2 <- sub("!=", "<>", filt)
  }
  paste("select * from", layernm, "where", part2)
}

#' @rdname internal_desc
#' @export
recodelut <- function(lut, minvar="min", maxvar="max", classvar="class") {
  ## DESCRIPTION: converts lut with min/max values for continuous data to a
  ## lookup table by value
  lut2 <- lapply(lut[[classvar]], function(x, lut) {
          data.frame(value=c(lut[lut[[classvar]] == x, minvar]:lut[lut[[classvar]] == x, maxvar]),
 				class=rep(x))
      	}, lut)
  lut2 <- do.call(rbind, lut2)
  return(lut2)
}

#' @rdname internal_desc
#' @export
strat.pivot <- function(x, strvar, unitvars, strwtvar="Prop", strat.levels=NULL) {
  ## DESCRIPTION: translates strata table from spGetAuxiliary() to spGetStrata() format

  if (!"data.table" %in% class(x)) {
    x <- setDT(x)
  }
  nmlst <- names(x)
  PScols <- nmlst[grep(strvar, nmlst)]
  PSvalslst <- sapply(strsplit(PScols, paste0(strvar, ".")), "[[", 2)

  strlut <- data.table(PSvalslst, x[, t(.SD), by=unitvars, .SDcols=PScols])
  setnames(strlut, c(strvar, unitvars, strwtvar))
  setcolorder(strlut, c(unitvars, strvar, strwtvar))
  if (is.null(strat.levels)) {
    strlut[[strvar]] <- factor(strlut[[strvar]])
  } else {
    strlut[[strvar]] <- factor(strlut[[strvar]], levels=strat.levels)
  }
  strvars <- strvar
  return(strlut)
}

#' @rdname internal_desc
#' @export
makedummy <- function(dat, auxlut, predfac){
  ## DESCRIPTION: make dummy variables for a list of factors (predfac)
  ## dat - plot-level data, including predfac assignments
  ## auxlut - domain zonal summaries
  ## predfac - one or more names of factors in dat

  ## get column names in auxlut
  auxnmlst <- names(auxlut)

  if (!"data.table" %in% class(dat)) {
    dat <- data.table(dat)
  }

  facnames <- {}
  for (fac in predfac) {
    pltvals <- sort(unique(dat[[fac]]))
    facnmlst <- auxnmlst[grep(fac, auxnmlst)]
    if (length(facnmlst) == 0) {
      message("auxvar not in tables: ", paste(fac, collapse=", "))
    } else {
      pivotstrat <- TRUE
    }
    ## Get factor levels
    fac.levels <- as.numeric(sapply(strsplit(facnmlst,
			paste0(fac,".")), '[', 2))
    dat[[fac]] <- factor(dat[[fac]], levels=fac.levels)

    ## Set factor levels to keep and delete from auxlut.
    fac.unitcol.keep <- paste(fac, fac.levels[-1], sep=".")
    fac.unitcol.del <- paste(fac, fac.levels[1], sep=".")
    auxlut[[fac.unitcol.del]] <- NULL

    ## Rename factor variables and add names to predictor list
    facs <- paste0(fac, fac.levels[-1])
    names(auxlut)[names(auxlut) %in% fac.unitcol.keep] <- facs
    facnames <- c(facnames, facs)

    ## Create dummy variables for factor levels - 1
    dtfac <- dat[, as.data.table(model.matrix(~.,
				data=dat[, fac, with=FALSE]))][,-1]
    dat <- cbind(dat, dtfac)
  }
  return(list(dat=dat, auxlut=auxlut, facnames=facnames))
}

#' @rdname internal_desc
#' @export
preds.standardize <- function(plt, aux, prednames) {
  ## DESCRIPTION: standardize predictors in plt and aux tables
  ## Standardize to the mean and SD of plot-level predictor values in population

  plt.mean <- as.matrix(setDT(plt)[, lapply(.SD, mean, na.rm=TRUE), .SDcols=prednames])
  plt.sd <- as.matrix(plt[, lapply(.SD, sd, na.rm=TRUE), .SDcols=prednames])
  aux.mean.mat <- matrix(rep(plt.mean, nrow(aux)), byrow=TRUE, ncol=length(prednames))
  aux.sd.mat <- matrix(rep(plt.sd, nrow(aux)), byrow=TRUE, ncol=length(prednames))

  plt.mean.mat <- matrix(rep(plt.mean, nrow(plt)), byrow=TRUE, ncol=length(prednames))
  plt.sd.mat <- matrix(rep(plt.sd, nrow(plt)), byrow=TRUE, ncol=length(prednames))

  aux.mat <- as.matrix(setDF(aux)[, prednames])
  plt.mat <- as.matrix(setDF(plt)[, prednames])
  aux[,prednames] <- (aux.mat - aux.mean.mat) / aux.sd.mat
  plt[,prednames] <- (plt.mat - plt.mean.mat) / plt.sd.mat

  return(list(plt=plt, aux=aux))
}

#' @rdname internal_desc
#' @export
gregEN.select <- function(y, x_sample, x_pop, N, alpha=0.5, returncoef=FALSE, cvfolds=10) {

  ## select predictor variables from Elastic Net procedure
  mod <- tryCatch(suppressMessages(mase::gregElasticNet(y=y,
		xsample=x_sample,
		xpop=x_pop, pi = NULL, alpha = 0.5,
  		model = "linear", pi2 = NULL, var_est = FALSE,
  		datatype = "means", N = N,
  		lambda = "lambda.min", cvfolds = cvfolds)),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
  if (is.null(mod)) {
    return(NULL)
  }
  mod$coefficients[-1]
  mod.rank <- rank(-abs(mod$coefficients[-1]))
  preds.coef <- mod$coefficients[-1][order(rank(-abs(mod$coefficients[-1])))]
  preds.enet <- names(preds.coef[abs(preds.coef)>0])

  if (returncoef) {
    return(preds.coef)
  } else {
    return(preds.enet)
  }
}

#' @rdname internal_desc
#' @export
preds.select <- function(y, plt, auxlut, prednames, cvfolds=10) {

  ## Description: Variable selection using area-level Elastic net, where
  ##		y values are mean values (i.e., Small Area y values).

  prednames.select <- prednames

  plt <- setDT(plt)
  auxlut <- setDT(auxlut)

  if (!"npixels" %in% names(auxlut)) {
    stop("need npixels in auxiliary lut")
  }
  N <- sum(auxlut$npixels)
  x_pop <- auxlut[, lapply(.SD, mean), .SDcols=prednames]

  plt <- data.frame(plt)
  x_pop <- data.frame(x_pop)
  x_sample <- plt[, prednames, drop=FALSE]
  y <- plt[[y]]

  ## Variable selection using mase:gregElasticNet()
  preds.coef <- tryCatch(gregEN.select(y=y, x_sample=x_sample, x_pop=x_pop,
		N=N, alpha=0.5, returncoef=TRUE, cvfolds=cvfolds),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
  if (is.null(preds.coef)) {
    #preds.coef <- x_sample[FALSE, ]
    preds.coef <- rep(0, ncol(x_sample))
    names(preds.coef) <- names(x_sample)
  }
  preds.enet <- names(preds.coef[abs(preds.coef)>0])

  if (length(preds.enet) == 0) {
    ## select predictor variables from Elastic Net procedure using lower alpha
    ## alpha=0, indicates no variable selection

    preds.coef <- tryCatch(gregEN.select(y=y, x_sample=x_sample, x_pop=x_pop,
		N=N, alpha=0.2, returncoef=TRUE, cvfolds=cvfolds),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
    if (is.null(preds.coef)) {
      #preds.coef <- x_sample[FALSE, ]
      preds.coef <- rep(0, ncol(x_sample))
      names(preds.coef) <- names(x_sample)
    }
    preds.enet <- names(preds.coef[abs(preds.coef)>0])
  }

  return(list(preds.coef=preds.coef, preds.enet=preds.enet))
}
