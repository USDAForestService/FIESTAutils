# getjoinqry - creates string of for a SQL query joining multiple ids
# classifyqry - creates a string to classify columns to use inside another query 

#' @rdname internal_desc
#' @export
getjoinqry <- function (joinid1, joinid2, alias1 = "p.", alias2 = "plta.") {
  joinqry <- "ON ("
  for (i in 1:length(joinid1)) {
    joinqry <- paste0(joinqry, alias1, joinid1[i], " = ", alias2, 
                        joinid2[i])
    if (i == length(joinid1)) {
      joinqry <- paste0(joinqry, ")")
    } else {
      joinqry <- paste(joinqry, "AND ")
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
  classify.qry <- paste0(classify1.qry, classify2.qry, " END) AS ", prefixnm, classnm)
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
  classify.qry <- paste0(classify1.qry, classify2.qry, " END) AS ", prefixnm, classnm)  
  return(classify.qry)
}
