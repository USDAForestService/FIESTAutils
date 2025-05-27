# getjoinqry - creates string of for a SQL query joining multiple ids
# classifyqry - creates a string to classify columns to use inside another query 

#' @rdname internal_desc
#' @export
getjoinqry <- function (joinid1, joinid2=NULL, alias1 = "p.", alias2 = "plta.") {
  if (is.null(joinid2)) {
    joinid2 <- joinid1
  }
  joinqry <- "ON ("
  for (i in 1:length(joinid1)) {
    joinqry <- paste0(joinqry, alias1, joinid1[i], " = ", alias2, 
                        joinid2[i])
    if (i == length(joinid1)) {
      joinqry <- paste0(joinqry, ")")
    } else {
      if (length(joinid1) >= 4 && i == 2) {
        joinqry <- paste(joinqry, "\n             AND ")
      } else {
        joinqry <- paste(joinqry, "AND ")
      }
    }
  }
  return(joinqry)
}
 
 
#' @rdname internal_desc
#' @export
classqry <- function(classcol, 
                     fromval,
                     toval, 
                     classnm = NULL,
                     class. = NULL,
                     prefixnm = NULL,
                     fill = NULL) {
  ## DESCRIPTION: creates a string to classify columns to use inside another query
  ## classcol - name of column to classify
  ## fromval - vector of values in classcol to classify
  ## toval - vector of values to classify to
  ## classnm - name of new attribute, if NULL, paste0(classcol, 'CL')
  ## NAto0 - set NULL values to 0

  
  ## Define default classnm
  if (is.null(classnm)) {
    classnm <- paste0(classcol, "CL")
  } else if (!is.character(classnm) || length(classnm) > 1) {
    stop("invalid classnm: ", toString(classnm))
  }
  
  ## Check if fromval and toval have same length
  if (length(fromval) != length(toval)) {
    message("fromval and toval must be same length")
  }
  
  ## check fill
  if (!is.null(fill) && (!is.vector(fill) || length(fill) != 1)) {
    message("fill must be vector with length = 1")
  }
  

  ## Build classify query
  classify1.qry <- paste("  (CASE")

  classify2.qry <- {}
  if (!is.null(fill) && !is.na(fill)) {
    classify2.qry <- paste0(" \n   WHEN ", class., classcol, " IS NULL THEN '", fill, "'")
  }
  for (i in 1:(length(fromval))) {  
    if (!is.na(fromval[i])) {
      classify2.qry <- paste0(classify2.qry, 
            "\n   WHEN ", class., classcol, " = ", fromval[i], " THEN '", toval[i], "'")
    }
  }
  classify.qry <- paste0(classify1.qry, classify2.qry, " END) AS '", prefixnm, classnm, "'")
  return(classify.qry)
}



#' @rdname internal_desc
#' @export
classifyqry <- function(classcol, 
                        cutbreaks,
                        cutlabels = NULL,
                        classnm = NULL,
                        class. = NULL,
                        prefixnm = NULL,
                        fill = NULL) {
  ## DESCRIPTION: creates a string to classify columns to use inside another query
  ## classcol - name of column to classify
  ## cutbreaks - vector of values to split classcol into (e.g., c(10,20) - >=10 and < 20)
  ## cutlabels - vector of class labels, if NULL, creates labels from cutbreaks (e.g., 10-20)
  ## classnm - name of new attribute
  ## NAto0 - set NULL values to 0
  ## fill - fill value

  
  ## Define default classnm
  if (is.null(classnm)) {
    classnm <- paste0(classcol, "CL")
  } else if (!is.character(classnm) || length(classnm) > 1) {
    stop("invalid classnm: ", toString(classnm))
  }
  
  ## Check cutlabels 
  if (!is.null(cutlabels)) {
    ## Check if cutbreaks and cutlabels have same length
    if (length(cutbreaks) != length(cutlabels)) {
      message("cutbreaks and cutlabels must be same length")
	  }
  } else {
    cutlabels <- {}
	  for (i in 1:length(cutbreaks)) {
	    brk <- cutbreaks[i]
	    if (i == length(cutbreaks)) {
	      cutlabels <- c(cutlabels, paste0(brk, "+"))
	    } else {
        cutlabels <- c(cutlabels, paste0(brk, "-", cutbreaks[i+1]))
      }
    }
  }	
  
  ## check fill
  if (!is.null(fill) && (!is.vector(fill) || length(fill) != 1)) {
    message("fill must be vector with length = 1")
  }

  ## Build classify query
  classify1.qry <- paste("  (CASE")

  classify2.qry <- {}
  if (!is.null(fill) && !is.na(fill)) {
    classify2.qry <- paste0(" \n   WHEN ", class., classcol, " IS NULL THEN '", fill, "'")
  }
  for (i in 1:(length(cutbreaks))) {    
    if (i == length(cutbreaks)) {
      classify2.qry <- paste0(classify2.qry, 
                              "\n   WHEN ", class., classcol, " >= ", cutbreaks[i], " THEN '", cutbreaks[i], "+'")
    } else {
      classify2.qry <- paste0(classify2.qry, 
          "\n   WHEN ", class., classcol, " >= ", cutbreaks[i], " AND ", 
          class., classcol, " < ", cutbreaks[i+1], " THEN '", cutlabels[i], "'")
    }
  }
  classify.qry <- paste0(classify1.qry, classify2.qry, " END) AS '", prefixnm, classnm, "'")  
  return(classify.qry)
}



#' @rdname internal_desc
#' @export
getcombineqry <- function(lut, 
                          unitvars,
                          classcols,
                          fromcols,
                          tocols,
                          tab. = "") {
  ## DESCRIPTION: create classification query for combining strata
  classify.qry <- {}
  for (col in 1:length(classcols)) {
    tocol <- classcols[col]
    fromcol <- fromcols[col]

    case.qry <- "\n(CASE"
    for (i in 1:(nrow(lut))) { 
        
      luti <- lut[i,]
      tocolsi <- as.vector(t(luti[, tocols]))
      fromcolsi <- as.vector(t(luti[, fromcols]))
      
      ## value to change
      tocolval <- luti[[tocol]]
      fromcolval <- luti[[fromcol]]
      
      ## Build when query
      when.qry <- paste0("\nWHEN (", tab., fromcols[1], " = '", fromcolsi[1], "'") 
      for (j in 2:length(fromcols)) {
        when.qry <- paste0(when.qry, " AND ", tab., fromcols[j], " = '", fromcolsi[j], "'")
      }
      when.qry <- paste0(when.qry, ")")
      case.qry <- paste0(case.qry, when.qry, " THEN ", tocolval)
      
    }
    case.qry <- paste0(case.qry, " END) AS \"", tocol, "\"")
    classify.qry <- paste0(classify.qry, case.qry)
    if (col < length(classcols)) {
      classify.qry <- paste0(classify.qry, ",")
    }
  }

  return(classify.qry)
}

