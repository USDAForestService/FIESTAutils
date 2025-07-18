## allin1f
## crosstabx
## add0unit
## addrowgrp
## crossxtab
## getdomain
## getestvar
## crossxbyunit


#' @rdname internal_desc
#' @export
allin1f <- function(x, y, char.width=NULL, estnull="--", psenull="--",
	estround=NULL, pseround=NULL) {
  ## DESCRIPTION: Gets estimate (% standard error)

  if (all(is.na(x))) {
    x <- estnull
  }
  if (all(is.na(y))) {
    y <- estnull
  }
  if (all(x == 0) && (all(y == 0) || all(y == psenull))) {
    x <- estnull
  }
  if (all(y == 0)) {
    y <- psenull
  }

  if (is.numeric(estnull) || !all(x == estnull)) {
    x <- as.numeric(x)
    if (!is.null(estround))
      x <- round(x, estround)
  }

  if (is.numeric(psenull) || !all(y == psenull)) {
    y <- as.numeric(y)
    if (!is.null(pseround))
      y <- round(y, pseround)
  }
  if (is.null(char.width)) char.width <- max(nchar(y))

  x.nsmall <- ifelse(!is.null(estround), estround, nbrdecimals(x))
  y.nsmall <- ifelse(!is.null(pseround), pseround, nbrdecimals(x))

  paste0(format(x, big.mark=",", digits=nbrdigits(x), nsmall=x.nsmall), " (",
	format(y, width=char.width, justify="right", digits=nbrdigits(y),
	nsmall=y.nsmall), ")")

}
	

#' @rdname internal_desc
#' @export
crosstabx <- function(x, xvar, estnm, psenm, allin1=FALSE, char.width=NULL,
		estround=NULL, pseround=NULL, estnull="--", psenull="--") {

  ## Set global variable
  NBRPLT.gt0 <- NULL

  if (!is.null(estround) && is.numeric(x[[estnm]]))
    x[[estnm]] <- round(x[[estnm]], estround)
  if (!is.null(pseround) && is.numeric(x[[psenm]]))
    x[[psenm]] <- round(x[[psenm]], pseround)

  if (is.character(estnull))
    x[[estnm]] <- as.character(x[[estnm]])
  if (is.character(psenull))
    x[[psenm]] <- as.character(x[[psenm]])

  x[NBRPLT.gt0 == 0, (estnm) := estnull]
  x[NBRPLT.gt0 == 0, (psenm) := psenull]

  if (allin1) {
    if (is.null(char.width)) {
      char.width <- max(nchar(na.omit(x[[psenm]])))
    }
    estpse <- mapply(allin1f, x=x[[estnm]], y=x[[psenm]],
		       MoreArgs=list(char.width = char.width, 
		                     estnull = estnull, psenull = psenull,
		                     estround = estround, pseround = pseround))
    names(estpse) <- x[[xvar]]
	
	  if (is.factor(x[[xvar]])) {
	    xvar.levels <- levels(x[[xvar]])
	    if (length(estpse) < length(xvar.levels)) {
	      misslevels <- xvar.levels[!xvar.levels %in% names(estpse)]
		    missvect <- sapply(rep(0, length(misslevels)), allin1f, 0, 
		                char.width=char.width, estnull=estnull, psenull=psenull)	
        names(missvect) <- misslevels				   

		    estpse <- c(estpse, missvect)
		    estpse <- estpse[order(xvar.levels)]
	    }  
	  }
    return(estpse)
	
  } else {
    est <- x[[estnm]]
    names(est) <- x[[xvar]]
    pse <- x[[psenm]]
	
    if (is.factor(x[[xvar]])) {
      names(pse) <- x[[xvar]]
      xvar.levels <- levels(x[[xvar]])
      if (length(x[[xvar]]) < length(xvar.levels)) {
        misslevels <- xvar.levels[!xvar.levels %in% x[[xvar]]]
        missvect <- rep(estnull, length(misslevels))
        names(missvect) <- misslevels
        
        est <- c(est, missvect)
        pse <- c(pse, missvect)
        est <- est[order(match(names(est), xvar.levels))]
        pse <- pse[order(match(names(pse), xvar.levels))]
      }	
	}
	
    return(list(est=est, pse=pse))
  }
}


#' @rdname internal_desc
#' @export
add0unit <- function(x, xvar, uniquex, unitvar=NULL, xvar.add0=FALSE,
	xvar2=NULL, uniquex2=NULL, xvar2.add0=FALSE) {
  ## DESCRIPTION: Merges a table with all classes to another table and
  ## 			adds 0s if does not match
  ## x - table to join to
  ## xvar - joining variable
  ## uniquex - lut table with unique values
  ## unitvar - estimation unit variable

  ## set global variables
  uvar <- NULL

  xnames <- copy(names(x))
  byvars <- xvar
  if (!"data.table" %in% class(uniquex)) {
    uniquex <- setDT(uniquex)
  }

  if (!is.null(xvar2)) {
    if (is.null(uniquex2)) stop("must include uniquex2")
    if (!"data.table" %in% class(uniquex2)) {
      uniquex2 <- setDT(uniquex2)
	  }
    byvars <- c(byvars, xvar2)

    if (xvar.add0 && xvar2.add0) {
      
      ## Expand uniquex to all combinations
      uniquex.exp <- unique(expand.grid(uniquex[[xvar]], uniquex2[[xvar2]],
		                    stringsAsFactors=FALSE))
      if (!is.null(unitvar)) {
        uniquex.exp <- 
          data.table(uvar = rep(unique(x[[unitvar]]),
			               each = nrow(uniquex.exp)), uniquex.exp)
        setnames(uniquex.exp, c(unitvar, xvar, xvar2))
        chkvars <- c(unitvar, xvar, xvar2)
      } else {
        setnames(uniquex.exp, c(xvar, xvar2))
        chkvars <- c(xvar, xvar2)
      }
      ordvars <- chkvars
      
      ## Match classes of xvar in x with xvar in uniquex
      xchk <- check.matchclass(uniquex, uniquex.exp, xvar)
      uniquex <- xchk$tab1
      uniquex.exp <- xchk$tab2
      
      ## Match classes of xvar2 in x with xvar2 in uniquex2
      xchk <- check.matchclass(uniquex2, uniquex.exp, xvar2)
      uniquex2 <- xchk$tab1
      uniquex.exp <- xchk$tab2
      
      ## Merge uniquex to uniquex.exp if greater than 1 column
      if (ncol(uniquex) > 1) {
        uniquex.exp <- merge(uniquex.exp, uniquex, by=xvar)
        ordvars <- c(names(uniquex), xvar2)
      }

      ## Merge uniquex2 to uniquex.exp if greater than 1 column
      if (ncol(uniquex2) > 1) {
        uniquex.exp <- merge(uniquex.exp, uniquex2, by=xvar2)
        ordvars <- c(names(uniquex), names(uniquex2))
      }

      ## Match classes of chkvars in x with checkvars in uniquex.exp
      xchk <- check.matchclass(uniquex.exp, x, chkvars)
      uniquex.exp <- xchk$tab1
      x <- xchk$tab2

      ## Merge uniquex.exp to x
      x <- merge(uniquex.exp, x, by=chkvars, all.x=TRUE)
      x[is.na(x)] <- 0

    } else if (xvar.add0) {

      ## Expand uniquex to all combinations
      if (!is.null(unitvar)) {
        uniquex.exp <- unique(x[, expand.grid(uniquex[[xvar]], get(xvar2)), by=unitvar])
        setnames(uniquex.exp, c(unitvar, xvar, xvar2))
        chkvars <- c(unitvar, xvar, xvar2)
      } else {
        uniquex.exp <- unique(x[, expand.grid(uniquex[[xvar]], get(xvar2))])
        setnames(uniquex.exp, c(xvar, xvar2))
        chkvars <- c(xvar, xvar2)
      }
      
      ## Match classes of xvar in x with xvar in uniquex
      xchk <- check.matchclass(uniquex, uniquex.exp, xvar)
      uniquex <- xchk$tab1
      uniquex.exp <- xchk$tab2
      
      ## Match classes of xvar2 in x with xvar2 in uniquex2
      xchk <- check.matchclass(uniquex2, uniquex.exp, xvar2)
      uniquex2 <- xchk$tab1
      uniquex.exp <- xchk$tab2
      
      ## Merge uniquex to uniquex.exp if greater than 1 column
      if (ncol(uniquex) > 1) {
        uniquex.exp <- merge(uniquex.exp, uniquex, by=xvar, all.x=TRUE, all.y=TRUE)
      }
      
      ## Merge uniquex2 to uniquex.exp if greater than 1 column
      if (ncol(uniquex2) > 1) {
        uniquex.exp <- merge(uniquex.exp, uniquex2, by=xvar2, all.x=TRUE)
      }

      ## Match classes of chkvars in x with checkvars in uniquex.exp
      xchk <- check.matchclass(uniquex.exp, x, chkvars)
      uniquex.exp <- xchk$tab1
      x <- xchk$tab2
      
      ## Merge uniquex.exp to x
      x <- unique(merge(x, uniquex.exp, by=chkvars, all.y=TRUE))
      setcolorder(x, c(chkvars, names(x)[!names(x) %in% chkvars]))

      ## Set NA values to 0
      #x[is.na(x)] <- 0

    } else if (xvar2.add0) {
	
      ## Expand uniquex to all combinations
      if (!is.null(unitvar)) {
        uniquex.exp <- unique(x[, expand.grid(uniquex2[[xvar2]], get(xvar)), by=unitvar])
        setnames(uniquex.exp, c(unitvar, xvar2, xvar))
        chkvars <- c(unitvar, xvar, xvar2)
      } else {
        uniquex.exp <- unique(x[, expand.grid(uniquex2[[xvar2]], get(xvar))])
        setnames(uniquex.exp, c(xvar2, xvar))
        chkvars <- c(xvar, xvar2)
      }
      
      ## Match classes of xvar in x with xvar in uniquex
      xchk <- check.matchclass(uniquex, uniquex.exp, xvar)
      uniquex <- xchk$tab1
      uniquex.exp <- xchk$tab2
      
      ## Match classes of xvar2 in x with xvar2 in uniquex2
      xchk <- check.matchclass(uniquex2, uniquex.exp, xvar2)
      uniquex2 <- xchk$tab1
      uniquex.exp <- xchk$tab2
      
      
      ## Merge uniquex to uniquex.exp if greater than 1 column
      if (ncol(uniquex) > 1) {
        uniquex.exp <- merge(uniquex.exp, uniquex, by=xvar)
      }
      
      ## Merge uniquex2 to uniquex.exp if greater than 1 column
      if (ncol(uniquex2) > 1) {
        uniquex.exp <- merge(uniquex.exp, uniquex2, by=xvar2, all.y=TRUE)
      }

      ## Match classes of x with uniquex.exp
      xchk <- check.matchclass(uniquex.exp, x, chkvars)
      uniquex.exp <- xchk$tab1
      x <- xchk$tab2
      
      ## Merge uniquex.exp to x
      x <- merge(uniquex.exp, x, by=chkvars, all.x=TRUE, all.y=TRUE)

      ## Set NA values to 0
      #x[is.na(x)] <- 0

    } else {
	
      ## Match classes of xvar in x with xvar in uniquex
      xchk <- check.matchclass(uniquex, x, xvar)
      uniquex <- xchk$tab1
      x <- xchk$tab2
      
      ## Merge uniquex to x
      x <- merge(uniquex, x, by=xvar, all.y=TRUE)
      
      ## Match classes of xvar2 in x with xvar2 in uniquex2
      xchk <- check.matchclass(uniquex2, x, xvar2)
      uniquex2 <- xchk$tab1
      x <- xchk$tab2
      
      ## Merge uniquex2 to x
      x <- merge(uniquex2, x, by=xvar2, all.y=TRUE)
    }
    
  } else {  ## is.null(xvar2)
    
    if (xvar.add0) {
      xchk <- check.matchclass(uniquex, x, byvars)
      uniquex <- xchk$tab1
      x <- xchk$tab2

      if (!is.null(unitvar)) {
        setnames(x, unitvar, "uvar")
        x <- x[uniquex[rep(1:nrow(uniquex), uniqueN(x$uvar)),
		            c(.SD, list(uvar=rep(unique(x$uvar), each=nrow(uniquex))))],
		            on=c("uvar", xvar)]
        setnames(x, "uvar", unitvar)
        #x[is.na(x)] <- 0
      } else {
        x <- merge(uniquex, x, by=byvars, all.y=TRUE)
      }
    } else {
     
      ## Match classes of byvars in x with byvars in uniquex
      xchk <- check.matchclass(uniquex, x, byvars)
      uniquex <- xchk$tab1
      x <- xchk$tab2
      
      ## Merge uniquex to x
      x <- merge(uniquex, x, by=byvars, all.y=TRUE)
    }
  }

  # ## Set factor levels 
  # if (is.factor(uniquex[[xvar]])) {
  #   xlevels <- levels(uniquex[[xvar]])
  #   if (is.factor(x[[xvar]])) {
  #     setattr(x[[xvar]], "levels", xlevels)
  #   } else {
  #     x[[xvar]] <- factor(x[[xvar]])
  #     setattr(x[[xvar]], "levels", xlevels)
  #   }
  # } else {
  #   x[[xvar]] <- factor(x[[xvar]], levels=uniquex[[xvar]])
  # }
  # 
  # if (!is.null(xvar2)) {
  #   if (is.factor(uniquex2[[xvar2]])) {
  #     xlevels2 <- levels(uniquex2[[xvar2]])
  #     if (is.factor(x[[xvar2]])) {
  #       setattr(x[[xvar2]], "levels", xlevels2)
  #     } else {
  #       x[[xvar2]] <- factor(x[[xvar2]])
  #       setattr(x[[xvar2]], "levels", xlevels2)
  #     }
  #   } else {
  #     x[[xvar2]] <- factor(x[[xvar2]], levels=uniquex2[[xvar2]])
  #   }
  # }

  if (is.null(unitvar)) {
    ordervars <- byvars
  } else {
    ordervars <- c(unitvar, byvars)
  }

  setorderv(x, ordervars)
  setcolorder(x, c(xnames, names(x)[!names(x) %in% xnames]))
  return(data.table(x))
}


#' @rdname internal_desc
#' @export
addrowgrp <- function(x, uniquerow, rowvar, rowgrpnm, title.rnames=NULL) {

   x[[rowgrpnm]] <- uniquerow[match(x[,1], uniquerow[[rowvar]]), get(eval(rowgrpnm))]
   x <- x[, c(ncol(x), 1:(ncol(x)-1))]
   return(x)
}


#' @rdname internal_desc
#' @export
crossxtab <- function (group.est, rowvar.est=NULL, colvar.est=NULL, total.est=NULL,
	rowvar, colvar, estnm, psenm, estround=NULL, pseround=NULL, gtotal=TRUE,
	allin1=FALSE, rowgrp=FALSE, rowgrpnm=NULL, title.rnames=NULL, estnull=0,
	psenull="--", char.width=NULL) {

  ## DESCRIPTION: Internal function to generate 1 table with estimates with percent
  ##		standard errors

  ## Set global variables
  Total=NBRPLT.gt0 <- NULL

  ## COLUMN TOTALS
  if (is.null(title.rnames)) title.rnames <- rowvar
  rnbr <- length(title.rnames)
  totals <- rep("Total", rnbr)

  ## Define function
  unAsIs <- function(X) {
     ## Description: removes AsIs from class
     if("AsIs" %in% class(X)) {
         class(X) <- class(X)[-match("AsIs", class(X))]
     }
     X
  }


  ##############################################################################
  ## Round values and get character width for table
  ## Note: If NBRPLT.gt0 = 0, it is replaced by null value (i.e., estnull, psenull)
  ##############################################################################

  if (!is.null(estround) && is.numeric(group.est[[estnm]])) {
    group.est[[estnm]] <- round(group.est[[estnm]], estround)
  }
  if (!is.null(pseround) && is.numeric(group.est[[psenm]])) {
    group.est[[psenm]] <- round(group.est[[psenm]], pseround)
  }
  if (is.null(char.width)) {
    char.width <- max(nchar(na.omit(group.est[[psenm]])))
  }
  if (is.character(estnull)) {
    group.est[[estnm]] <- as.character(group.est[[estnm]])
  }
  if (is.character(psenull)) {
    group.est[[psenm]] <- as.character(group.est[[psenm]])
  }
  group.est[NBRPLT.gt0 == 0, (estnm) := estnull]
  group.est[NBRPLT.gt0 == 0, (psenm) := psenull]

  if (!is.null(rowvar.est)) {
    if (!is.null(estround) && is.numeric(rowvar.est[[estnm]]))
      rowvar.est[[estnm]] <- round(rowvar.est[[estnm]], estround)
    if (!is.null(pseround) && is.numeric(rowvar.est[[psenm]]))
      rowvar.est[[psenm]] <- round(rowvar.est[[psenm]], pseround)
    row.char.width <- max(nchar(na.omit(rowvar.est[[psenm]])))
    char.width <- max(char.width, row.char.width)

    if (!is.null(rowvar.est)) rowtotal <- sum(rowvar.est[[estnm]])

    if (is.character(estnull))
      rowvar.est[[estnm]] <- as.character(rowvar.est[[estnm]])
    if (is.character(psenull))
      rowvar.est[[psenm]] <- as.character(rowvar.est[[psenm]])

    rowvar.est[NBRPLT.gt0 == 0, (estnm) := estnull]
    rowvar.est[NBRPLT.gt0 == 0, (psenm) := psenull]
  }

  if (!is.null(colvar.est)) {
    if (!is.null(estround) && is.numeric(colvar.est[[estnm]]))
      colvar.est[[estnm]] <- round(colvar.est[[estnm]], estround)
    if (!is.null(pseround) && is.numeric(colvar.est[[psenm]]))
      colvar.est[[psenm]] <- round(colvar.est[[psenm]], pseround)
    col.char.width <- max(nchar(na.omit(colvar.est[[psenm]])))
    char.width <- max(char.width, col.char.width)

    if (is.character(estnull))
      colvar.est[[estnm]] <- as.character(colvar.est[[estnm]])
    if (is.character(psenull))
      colvar.est[[psenm]] <- as.character(colvar.est[[psenm]])

    colvar.est[NBRPLT.gt0 == 0, (estnm) := estnull]
    colvar.est[NBRPLT.gt0 == 0, (psenm) := psenull]
  }

  if (!is.null(total.est)) {
    if (!is.null(estround) && is.numeric(total.est[[estnm]]))
      total.est[[estnm]] <- round(total.est[[estnm]], estround)
    if (!is.null(pseround) && is.numeric(total.est[[psenm]]))
      total.est[[psenm]] <- round(total.est[[psenm]], pseround)
    tot.char.width <- max(nchar(na.omit(total.est[[psenm]])))
    char.width <- max(char.width, tot.char.width)

    if (is.character(estnull))
      total.est[[estnm]] <- as.character(total.est[[estnm]])
    if (is.character(psenull))
      total.est[[psenm]] <- as.character(total.est[[psenm]])

    total.est[NBRPLT.gt0 == 0, (estnm) := estnull]
    total.est[NBRPLT.gt0 == 0, (psenm) := psenull]
  }

  ## Generate tables
  #################################################################################
  dcastdrop <- TRUE
  if (is.factor(rowvar.est[[rowvar]])) {
    #if (any(as.character(levels(rowvar.est[[rowvar]])) == "NA")) {
    if (length(levels(rowvar.est[[rowvar]])) > length(rowvar.est[[rowvar]])) {
      dcastdrop <- FALSE
    }
  } else {
    if (any(is.na(rowvar.est[[rowvar]]))) {
      dcastdrop <- FALSE
    }
  }
  if (is.factor(colvar.est[[colvar]])) {
    #if (any(as.character(levels(colvar.est[[colvar]])) == "NA")) {
    if (length(levels(colvar.est[[colvar]])) > length(colvar.est[[colvar]])) {
      dcastdrop <- FALSE
    }
  } else {
    if (any(is.na(colvar.est[[colvar]]))) {
      dcastdrop <- FALSE
    }
  }
  if (rowgrp) {
    est <- dcast(group.est, get(rowgrpnm) + get(rowvar) ~ get(colvar),
                 value.var = estnm, fill = estnull, drop = dcastdrop)
    pse <- dcast(group.est, get(rowgrpnm) + get(rowvar) ~ get(colvar),
                 value.var = psenm, fill = psenull, drop = dcastdrop)
    crnames <- c("rowgrpnm", "rowvar")
  } else {
    est <- dcast(group.est, get(rowvar) ~ get(colvar),
                 value.var = estnm, fill = estnull, drop = dcastdrop)
    pse <- dcast(group.est, get(rowvar) ~ get(colvar),
                 value.var = psenm, fill = psenull, drop = dcastdrop)
    
    crnames <- "rowvar"
  }
  
  est <- est[, lapply(.SD, unAsIs)]
  pse <- pse[, lapply(.SD, unAsIs)]

  ## Set factor order
  est <- est[order(rowvar), ]
  pse <- pse[order(rowvar), ]
   
  if (allin1) {
    estmat <- as.matrix(est[, -(1:rnbr)])
    psemat <- as.matrix(pse[, -(1:rnbr)])
  }
  
  setnames(est, crnames, title.rnames)
  setnames(pse, crnames, title.rnames)
  cnames <- names(est)[!names(est) %in% crnames]

  ## Convert factors to characters
  est[, (title.rnames) := lapply(.SD, as.character), .SDcols=title.rnames]
  pse[, (title.rnames) := lapply(.SD, as.character), .SDcols=title.rnames]

  if (allin1) {
    #estmat <- as.matrix(est[, -(1:rnbr)])
    #psemat <- as.matrix(pse[, -(1:rnbr)])
    estall1 <- mapply(allin1f, estmat, psemat, 
                      MoreArgs = list(char.width = char.width,
		                                  estnull = estnull, psenull = psenull, 
		                                  estround = estround, pseround = pseround))
    estpse <- data.table(est[, 1:rnbr], matrix(estall1, nrow(estmat), ncol(estmat)))
    names(estpse) <- cnames
  } 

  if (is.null(colvar.est) || is.null(rowvar.est)) {
    if (!is.null(colvar.est)) {
      estpse.col <- crosstabx(colvar.est, colvar, estnm, psenm, allin1=allin1,
		            char.width=char.width, estnull=estnull, psenull=psenull,
		            estround=estround, pseround=pseround)
      if (allin1) {
        estpse <- rbind(setDF(estpse), c(totals, estpse.col))
      } else {
        estpse.col$est <- estpse.col$est[unique(group.est[[colvar]])]
        est <- rbind(setDF(est), c(totals, estpse.col$est))
        pse <- rbind(setDF(pse), c(totals, estpse.col$pse))
      }
    } else if (!is.null(rowvar.est)) {
      estpse.row <- crosstabx(rowvar.est, rowvar, estnm, psenm, allin1=allin1,
		             char.width=char.width, estnull=estnull, psenull=psenull,
		             estround=estround, pseround=pseround)

      if (allin1) {
        estpse$Total <- estpse.row
      } else {
        est$Total <- estpse.row$est
        pse$Total <- estpse.row$pse
      }
    }
  } else if (!is.null(colvar.est) || !is.null(rowvar.est)) {

    estpse.col <- crosstabx(colvar.est, colvar, estnm, psenm, 
                            allin1 = allin1,
		                        char.width = char.width, 
		                        estnull = estnull, psenull = psenull,
		                        estround = estround, pseround = pseround)

    if (allin1) {
      estpse <- rbind(setDF(estpse), c(totals, estpse.col))
    } else {
      est <- rbind(setDF(est), c(totals, estpse.col$est))
      pse <- rbind(setDF(pse), c(totals, estpse.col$pse))
    }
    estpse.row <- crosstabx(rowvar.est, rowvar, estnm, psenm, 
                            allin1 = allin1,
		                        char.width = char.width, 
		                        estnull = estnull, psenull = psenull,
		                        estround = estround, pseround = pseround)
    if (!allin1) {
      est.row <- estpse.row$est
      pse.row <- estpse.row$pse
    }

    ## total.est
    ##############################################################
    if (gtotal) {
      if (is.null(total.est)) {
        if (!allin1) {
          est.tot <- sum(as.numeric(est.row), na.rm=TRUE)
          pse.tot <- psenull
        } else {
          estpse.tot <- paste0(format(psenull, big.mark=","), " (",
			         format(psenull, justify="right", width=char.width), ")")
        }
      } else {
        est.tot <- total.est[[estnm]]
        pse.tot <- total.est[[psenm]]

        if (allin1) {
          estpse.tot <- mapply(allin1f, est.tot, pse.tot,
			          MoreArgs=list(char.width=char.width, estnull=estnull, psenull=psenull,
			                        estround=estround, pseround=pseround))
        }
      }
    } else {
      if (allin1) {
        estpse.tot <- paste0(format(psenull, big.mark=","), " (", format(psenull,
			       justify="right", width=char.width), ")")
      } else {
        est.row <- as.character(est.row)
        est.tot <- 0
        pse.tot <- psenull
      }
    }

    ## Merge row
    ##############################################################
    if (allin1) {
      estpse.row <- c(estpse.row, estpse.tot)
      estpse$Total <- estpse.row
    } else {
      est.row <- c(est.row, est.tot)
      pse.row <- c(pse.row, pse.tot)
      est$Total <- est.row
      pse$Total <- pse.row
    }
  }

  if (allin1) {
    return(estpse)
  } else {
    return(list(est=est, pse=pse))
  }
}


#' @rdname internal_desc
#' @export
getdomain <- function() {
  ref_domain <- ref_domain
  titlelst <- ref_domain$DOMTITLE

  title <- select.list(c("NONE", titlelst), title="Domain?", multiple=FALSE)
  domain <- ref_domain[ref_domain$DOMTITLE == title, "DOMVARNM"]

  return(list(domain=domain, title.domain=title))
}


#' @rdname internal_desc
#' @export
getestvar <- function() {
  ref_estvar <- ref_estvar
  catlst <- ref_estvar$CATEGORY

  category <- select.list(catlst, title="Category?", multiple=FALSE)
  ref <- ref_estvar[ref_estvar$CATEGORY == category, ]

  titlelst <- ref$ESTTITLE
  title <- select.list(titlelst, title="Estimate variable?", multiple=FALSE)
  ref <- ref[ref$ESTTITLE == title, ]

  if (nrow(ref) > 1) stop("more than 1 row selected")

  return(list(estvar=ref$ESTVAR, esttitle=ref$ESTTITLE,
		est.filter=ref$ESTFILTER, units=ref$ESTUNITS))
}


#' @rdname internal_desc
#' @export
crossxbyunit <- function(unit=NULL, unit_grpest=NULL, unit_rowest=NULL,
	unit_colest=NULL, unit_totest=NULL, unitvar, rowvar, colvar,
	estnm, psenm, allin1, char.width, estnull, psenull,
	estround=NULL, pseround=NULL,
	rowgrp=NULL, rowgrpnm=NULL, title.rnames=NULL, numunits,
	savedata, addtitle, returntitle, outfn.estpse, title.estpse,
	title.est, title.pse, title.ref, outfolder, outfn.date, overwrite,
	esttype, phototype, rnames=NULL, title.colvar=NULL, title.unitvar=NULL) {

  ## set global variables
  total.est=rowvar.est=colvar.est=group.est <- NULL

  if (!is.null(unit_totest)) {
    if (!is.null(unit)) {
      total.est <- unit_totest[get(unitvar) == unit,]
    } else {
      total.est <- unit_totest
    }
  }
  if (!is.null(unit_rowest)) {
    if (!is.null(unit)) {
      rowvar.est <- unit_rowest[get(unitvar) == unit,]
    } else {
      rowvar.est <- unit_rowest
    }
  } else {
    rowtotal <- FALSE
  }
  if (!is.null(unit_colest)) {
    if (!is.null(unit)) {
      colvar.est <- unit_colest[get(unitvar) == unit,]
    } else {
      colvar.est <- unit_colest
    }
  } else {
    coltitlerow <- FALSE
  }
  if (!is.null(unit_grpest)) {
    if (!is.null(unit)) {
      group.est <- unit_grpest[get(unitvar) == unit,]
    } else {
      group.est <- unit_grpest
    }
  }
  if (nrow(group.est) == 0) return(NULL)

  ## Get cross tables
  #########################################################
  tabs <- crossxtab(group.est=group.est, rowvar.est=rowvar.est,
	             colvar.est=colvar.est, total.est=total.est, rowvar=rowvar,
	             colvar=colvar, estnm=estnm, psenm=psenm, allin1=allin1,
	             rowgrp=rowgrp, rowgrpnm=rowgrpnm, title.rnames=title.rnames,
 	             estnull=estnull, psenull=psenull, char.width=char.width,
	             estround=estround, pseround=pseround)
  
  if (allin1) {
    estpsetab <- tabs
    if (!is.null(estpsetab)) {

      ## TABLES TO RETURN
      if (numunits > 1) {
        est2return <- cbind(unit=unit, estpsetab)
      } else {
        est2return <- estpsetab
      }
      pse2return <- NULL

      if ((savedata & addtitle) | returntitle) {
        if (numunits > 1) {
          returntitle <- FALSE
          outfn.estpse.unit <- paste(outfn.estpse, unit, sep="_")
          title.estpse.unit <- paste0(title.estpse, ": ", unit, title.ref)
        } else {
          title.estpse <- paste0(title.estpse, title.ref)
          title.estpse.unit <- title.estpse
          outfn.estpse.unit <- outfn.estpse
        }
        if (savedata) {
          ## SAVE TABLE
          suppressWarnings(
          save1tab(tab=estpsetab, tab.title=title.estpse.unit,
 		               outfolder=outfolder, allin1=TRUE, coltitlerow=FALSE,
 		               rnames=rnames, outfn=outfn.estpse.unit, addtitle=addtitle,
		               outfn.date=outfn.date, overwrite=overwrite))
        }
      }
    }
  } else { # not allin1
    esttab <- tabs$est
    psetab <- tabs$pse

    ## RETURN TABLES AND WRITE TO FILE
    cols <- levels(group.est[[colvar]])

    ## TABLES TO RETURN
    if (numunits > 1) {
      est2return <- data.table(unit=unit, esttab)
      pse2return <- data.table(unit=unit, psetab)
    } else {
      est2return <- esttab
      pse2return <- psetab
    }

    if ((savedata & addtitle) | returntitle) {
      ## TABLE TITLES
      if (numunits > 1) {
        returntitle <- FALSE
        outfn.estpse.unit <- paste(outfn.estpse, unit, sep="_")
        title.est.unit <- paste0(title.est, ": ", unit, title.ref)
        title.pse.unit <- paste0(title.pse, ": ", unit, title.ref)
      } else {
        outfn.estpse.unit <- outfn.estpse
        title.est <- paste0(title.est, title.ref)
        title.pse <- paste0(title.pse, title.ref)
        title.est.unit <- title.est
        title.pse.unit <- title.pse
      }

      if(savedata){
        if (esttype == "PHOTO" && phototype == "PCT") {
          suppressWarnings(
          save2tabs(tab1=esttab, tab2=psetab, tab1.title=title.est.unit,
		                tab2.title=title.pse.unit, outfolder=outfolder, 
		                coltitlerow=TRUE, coltitle=title.colvar, rnames=rnames, 
		                outfn.estpse=outfn.estpse.unit, addtitle=addtitle, 
		                rowtotal=FALSE, outfn.date=outfn.date, overwrite=overwrite))
        } else {
          suppressWarnings(
          save2tabs(tab1=esttab, tab2=psetab, tab1.title=title.est.unit,
		                tab2.title=title.pse.unit, outfolder=outfolder, 
		                coltitlerow=TRUE, coltitle=title.colvar, rnames=rnames, 
		                outfn.estpse=outfn.estpse.unit, addtitle=addtitle, 
		                rowtotal=FALSE, outfn.date=outfn.date, overwrite=overwrite))
        }
      }
    }

    if ("unit" %in% names(est2return)) {
      names(est2return)[names(est2return) == "unit"] <- title.unitvar
      if (!is.null(pse2return))
        names(pse2return)[names(pse2return) == "unit"] <- title.unitvar
    }
  }
  returnlst <- list(est2return=est2return, pse2return=pse2return)
  return(returnlst)
}


