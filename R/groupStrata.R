#' @rdname internal_desc
#' @export
groupEstunit <- function(x, minplotnum) {
  ## DESCRIPTION: Groups estimation units with total plots <= minplotnum.
  ## Estimation units that have total plots <= minplotnum are combined with the
  ## estimation unit next in order (numeric or alphabetical). If there are no
  ## estimation units next in order, it is combined with the estimation unit
  ## previous in order.

  ## set global variables
  unitvar <- NULL

  unitltmin <- unique(x[x$n.total <= minplotnum, unitvar])
  unitgtmin <- unique(x[x$n.total > minplotnum, unitvar])
  x[unitvar %in% unitgtmin][["unitnew"]] <- x[unitvar %in% unitgtmin][["unitvar"]]

  agunits <- {}
  for (unit in unitltmin) {
    if (!unit %in% agunits) {
      agunits <- {unit}
      maxag <- sum(x[unitvar %in% agunits][["n.strata"]])
      while (maxag <= minplotnum) {
        if (any(x$unitvar > max(agunits))) {
          unitag <- min(x$unitvar[x$unitvar > max(agunits)])
          agunits <- c(agunits, unitag)
          agnm <- paste(agunits, collapse="-")
        } else {
          unitag <- max(x$unitvar[x$unitvar < min(agunits)])
          unit2 <- unique(x[unitvar == unitag][["unitnew"]])
          if (!unit2 %in% c(-1, unitag)) {
            agunits <- c(agunits, strsplit(unit2, "-")[[1]])
          } else {
            agunits <- c(agunits, unitag)
          }
          agnm <- paste(agunits, collapse="-")
        }
        maxag <- sum(x[unitvar %in% agunits][["n.strata"]])
        x[unitvar %in% agunits][["unitnew"]] <- agnm
      }
    }
  }
  return(x)
}

#' @rdname internal_desc
#' @export
groupStrata <- function(x, minplotnum, nvar = "n.strata", 
                        strvar = NULL, stratalevels = NULL) {
  ## DESCRIPTION: Groups strata with total plots <= minplotnum.
  ## Strata that have less than minplotnum are combined with the strata
  ## next in order (numeric or alphabetical). If there are no strata
  ## next in order, it is combined with the strata previous in order.
  ## NOTE: minplotnum must not be greater than the minimun number
  ##		or plots by estimation unit plus 1.

  ## set global variables
  strat=stratnew <- NULL
   #print(x) # commented out by Grayson... don't think this should be here

  ## make strata factor
  getfactor <- FALSE
  if (!is.null(strvar) && !is.factor(x[[strvar]])) {
    getfactor <- TRUE
    strvarclass <- class(x[[strvar]])
    if (!is.null(stratalevels)) {
      x[[strvar]] <- factor(x[[strvar]], levels=stratalevels)
    } else (
      x[[strvar]] <- factor(x[[strvar]])
    )
  }

  if (any(x[[nvar]] < minplotnum)) {
    strats <- x$strat
    agstrats <- {}

    for (stratum in strats) {
      if (!stratum %in% agstrats) {
        agstrats <- stratum
        if (!is.null(strvar) && strvar %in% names(x)) {
          newnm <- x[strat %in% stratum, get(strvar)]
        } else {
          newnm <- stratum
        }
        if (x[x$strat %in% stratum][[nvar]] >= minplotnum) {
          x[x$strat %in% stratum][["stratnew"]] <- newnm
        } else {
          maxag <- sum(x[strat %in% stratum][[nvar]])
          while (maxag < minplotnum) {
            if (any(x$strat > max(agstrats))) {
              stratag <- min(x$strat[x$strat > max(agstrats)])
              agstrats <- c(agstrats, stratag)
            } else {
              stratag <- max(x$strat[x$strat < min(agstrats)])
              stratnewcd <- x[strat == stratag][["stratnew"]]
              stratag <- x[stratnew == as.character(stratnewcd)][["strat"]]
              agstrats <- c(stratag, agstrats)
            }
            if (!is.null(strvar)) {
              agstratsnm <- paste(x[strat %in% agstrats, get(strvar)], collapse="-")
            } else {
              agstratsnm <- paste(agstrats, collapse="-")
            }
            maxag <- sum(x[strat %in% agstrats][[nvar]])
            x[strat %in% agstrats][["stratnew"]] <- agstratsnm
          }
        }
      }
    }

  } else {
    x$stratnew <- as.character(x$strat)
  }
  
  if (getfactor) {
    if (strvarclass == "integer") {
      x[[strvar]] <- as.integer(as.character(x[[strvar]]))
    } else if (strvarclass == "character") {
      x[[strvar]] <- as.character(x[[strvar]])
    }
  }
  return(x)
}



#' @rdname internal_desc
#' @export
groupClasses <- function(x, minplotnum, 
                         nvar,
                         xvar, 
                         sumvar = "n.strata", 
                         xvarlevels = NULL) {
  ## DESCRIPTION: 
  ## Groups classes with total plots <= minplotnum.
  ## Classes that have less than minplotnum are combined with classes
  ## next in order (numeric or alphabetical). 
  ## If there are no classes next in order, it is combined with the
  ## class previous in order.
  ## If the total classes does not sum to minplotnum... a message 
  ## is given, but the program does not stop.
  ## 
  ## Arguments:
  ## x - data.table 
  ## nvar - the variable to test number of plots
  ## xvar - the variable to group
  ## sumvar - the variable to sum after grouping
  ## xvarlevels - factor levels to order by
  
  ## set global variables
  classo=classnew=classf <- NULL
  
  ## define new variables for grouping strvar
  x$classo <- x[[xvar]]  # original class
  
  ## make strata factor
  if (!is.factor(x$classo)) {
    if (!is.null(xvarlevels)) {
      x$classf <- factor(x$classo, levels=xvarlevels)
    } else (
      x$classf <- factor(x$classo)
    )
  } else if (!is.null(xvarlevels)) {
    x$classf <- factor(x$classo, levels=xvarlevels)
  } else {
    x$classf <- x$classo
  }
  x$classf <- as.numeric(x$classf)  # factored class
  x$classnew <- as.character(x$classo)  # new class
  
  
  ## get classes less than minplotnum
  ltmin <- unique(x[x[[nvar]] <= minplotnum, classf])
  gtmin <- unique(x[x[[nvar]] > minplotnum, classf])
  
  ## define vector of aggregated classes
  agclass <- {}
  for (ltclass in ltmin) {
    if (!ltclass %in% agclass) {
      agclass <- {ltclass}
      
      ## get sum of nvar for aggregated classes
      maxag <- sum(x[classf %in% agclass][[sumvar]])
      
      ## loop thru classes until greater than minplotnum
      while (maxag <= minplotnum) {
        
        ## check if there are any classes is list following
        if (any(x$classf > max(agclass))) {
          classag <- min(x$classf[x$classf > max(agclass)])
          agclass <- c(agclass, classag)
          
          ## get original name of aggregated classes
          agclassnm <- unique(x[classf %in% agclass][["classo"]])
          agnm <- paste(agclassnm, collapse="-")
          
        } else {   ## no classes following...  so group with previous class
          
          ## check if there are any other classes to sum to minplotnum
          otherclass <- x$classf[x$classf < min(agclass)]
          if (length(otherclass) == 0) {
            message("not enough plots to reach minplotnum (", minplotnum, ")... ", maxag)
            break()
          }
          
          ## get highest factored class number in list... 
          classag <- max(x$classf[x$classf < min(agclass)])
          class2 <- unique(x[classf == classag][["classf"]])
          if (!class2 %in% c(-1, classag)) {
            agclass <- c(agclass, strsplit(class2, "-")[[1]])
          } else {
            agclass <- c(agclass, classag)
          }
          agclassnm <- unique(x[classf %in% agclass][["classo"]])
          agnm <- paste(agclassnm, collapse="-")
        }
        maxag <- sum(x[classf %in% agclass][[sumvar]])
        x[classf %in% agclass][["classnew"]] <- agnm
      }
    }
  }
  
  x$classo=x$classf <- NULL
  return(x)
}  



