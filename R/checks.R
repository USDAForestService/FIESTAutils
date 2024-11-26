## check.numeric
## RtoSQL    Convert logical R statement syntax to SQL syntax
## check.logic.vars   Check if variables are in statement
## check.logic
## check.matchclass
## check.matchval

#' @rdname checks_desc
#' @export
check.numeric <- function(x) {
  ## DESCRIPTION: check if vector x is numeric. If not, change to numeric.

  if (!all(is.na(x)) && !is.numeric(x)) {
    if (is.factor(x)) {
      x <- as.numeric(as.character(x))
    } else {
      x <- as.numeric(x)
    }
  }
  return(x)
}

#' @rdname internal_desc
#' @export
RtoSQL <- function(filter, x=NULL) {
  ## DESCRIPTION: Convert logical R statement syntax to SQL syntax

  ## Check filter
  if (is.null(filter) || filter == "") {
    return(NULL)
  }

  ## Check logic
  if (!is.null(x)) {
    sql <- check.logic(x=x, statement=filter, stopifnull=TRUE)
  } else {
    sql <- filter
  }

  checkpart <- function(part) {
    ## Function to convert logical statement by part
    part <- trimws(part)

    ## Check for !
    not <- ifelse(grepl("\\!", part), TRUE, FALSE)

    # Replace R comparison operators with SQL operators
    if (grepl("==", part)) {
      if (not) {
        part <- gsub("!", "", part)
        part <- gsub("==", "<>", part)
      } else {
        part <- gsub("==", "=", part)
      } 
    }
    part <- gsub("!=", "<>", part)

    if (grepl("%in%", part, ignore.case=TRUE)) {
      if (not) {
        part <- gsub("!", "", part)
		if (grepl("%in%c", gsub(" ", "", part), ignore.case=TRUE)) {
          part <- gsub("%in%c", " NOT IN", gsub(" ", "", part))
		} else if (grepl("%in%", gsub(" ", "", part), ignore.case=TRUE)) {
		  stop("invalid statement")
          part <- gsub("%in%", " NOT IN", gsub(" ", "", part))
		}
      } else {
		if (grepl("%in%c", gsub(" ", "", part), ignore.case=TRUE)) {
          part <- gsub("%in%c", " IN", gsub(" ", "", part))
		} else if (grepl("%in%", gsub(" ", "", part), ignore.case=TRUE)) {
		  stop("invalid statement")
          part <- gsub("%in%", " IN", gsub(" ", "", part))
		}
		if (grepl(":", part)) {
		  p1 <- strsplit(part, ":")[[1]][1]
		  p1 <- strsplit(p1, "\\(")[[1]][2]
		  p2 <- strsplit(part, ":")[[1]][2]
		  p2 <- strsplit(p2, "\\)")[[1]][1]
		  part <- gsub(paste0(p1, ":", p2), toString(seq(p1,p2)), part)	
        }		  
      }
    }

    # Replace is.na() and !is.na() with SQL equivalents
    if (grepl("is.na", part)) {
      pre <- strsplit(part, "is.na\\(")[[1]][1]
      basetmp <- strsplit(part, "is.na\\(")[[1]][-1]
      base <- strsplit(basetmp, "\\)")[[1]][1]
      post <- ifelse (is.na(strsplit(basetmp, "\\)")[[1]][2]), "", strsplit(basetmp, "\\)")[[1]][2])

      if (not) {
        part <- paste0(pre, base, " IS NOT NULL", post)
        part <- gsub("!", "", part)
      } else {
        part <- paste0(pre, base, " IS NULL", post)
      }
    }

    return(part)
  }
 
  if (grepl("&", filter)) {
    sql <- paste(sapply(unlist(strsplit(filter, "&")), checkpart), collapse = " AND ")
  } else if (grepl("\\|", filter)) {
    sql <- paste(sapply(unlist(strsplit(sql, "\\|")), checkpart), collapse = " OR ")
  } else {
    sql <- checkpart(filter)
  }

  return(sql)
}

#check.numeric <- function(x, cols) {
#  for (col in cols) {
#    if (!is.numeric(x[[col]])) {
#      message(paste(col, "is not numeric, converting..."))
#      if (is.factor(x[[col]])) {
#        x[[col]] <- as.numeric(as.character(x[[col]]))
#      } else {
#        x[[col]] <- as.numeric(x[[col]])
#      }
#    }
#  }
#  return(x)
#}

#' @rdname checks_desc
#' @export
check.logic.vars <- function(varlst, statement, ignore.case = FALSE, returnVars = FALSE) {
  
  ## Description: checks if variables in varlst are in statement.
  if (all(is.null(varlst))) {
    return(FALSE)
  }
  
  chk <- sapply(varlst, function(x, statement, ignore.case) {
    grepl(paste0("(?<!\\w)", x, "(?!\\w)"), statement, perl = TRUE, ignore.case = ignore.case)},
    statement, ignore.case)
  

  if (any(chk)) {
    if (returnVars) {
      return(names(chk)[chk])
    } else {
      return(as.vector(chk))
    }
  } else {
    if (returnVars) {
      return(NULL)
    } else {
      return(FALSE)
    }
  }
}



#' @rdname checks_desc
#' @export
check.logic <- function(x, statement, filternm=NULL, stopifnull=FALSE,
	stopifinvalid=TRUE, removeinvalid=FALSE, syntax="R", returnvar=FALSE,
	returnpart = FALSE){
  ## DESCRIPTION: checks logical statement
  ## ARGUMENTS"
  ## x 	- data frame or vector of field names to check column names
  ## statement - logical statement
  ## filternm  - name to use in messages
  ## stopifnull - if statement=NULL, stop
  ## stopifinvalid - if statement is not valid (i.e., columns names do not
  ##		match any name in logical statement)
  ## removeinvalid - if TRUE, and a part of a multipart logical statement
  ## 		is invalid, the invalid part is removed, returning only valid part.
  ## 	    	Note: an example is if using statement for trees and seedlings:
  ##		"STATUSCD == 1 & SPCD = 475". Since there is no STATUSCD == 1 in
  ##		the seedling table, it is removed from logical statement.
  ## syntax - syntax of query ('R' or 'sql')

  ## Set global variables
  ignore.case <- TRUE
  syntax <- toupper(syntax)

  ## Define function to check names
  chkpartnm <- function(part, x, logic.chars, returnvar = FALSE) {
    part <- as.vector(part)

    ## Check if there are any variables in x that match filter
    chk <- sum(sapply(x, function(x, y){ grepl(x, y, ignore.case = TRUE) }, part))
    if (chk == 0) {
	    return(NULL)
    } else {
      chkvar <- x[sapply(x, function(x, y){ grepl(x, y, ignore.case = TRUE) }, part)]
	    if (grepl("\n", part)) {
	      part <- gsub("\n", "", part)
	    }
	    if (grepl("\t", part)) {
	      part <- gsub("\t", "", part)
	    }
	    if (grepl("!", part) && !grepl("!=", part)) {
	      part <- gsub("!", "", part)
	    }
	    chklogicchar <- logic.chars[sapply(logic.chars, 
	               function(x, part){ grepl(x, part, ignore.case=TRUE) }, part)]
 
	    if (length(chklogicchar) > 1) { 
	      if (length(chklogicchar) == 2 && all(c("notin", "in") %in% chklogicchar)) {
	        chklogicchar <- "notin"
	      } else if (length(chklogicchar) == 2 && all(c("NOTIN", "IN") %in% chklogicchar)) {
	        chklogicchar <- "NOTIN"
	      } else if (length(chklogicchar) == 3 && all(c("<", ">", "<>") %in% chklogicchar)) {
	        chklogicchar <- "<>"
	      } else if (length(chklogicchar) == 3 && all(c("=", "<=", "<") %in% chklogicchar)) {
	        chklogicchar <- "<="
	      } else if (length(chklogicchar) == 3 && all(c("=", ">=", ">") %in% chklogicchar)) {
	        chklogicchar <- ">="
	      } else if (length(chklogicchar) == 2 && all(c("<=", "<") %in% chklogicchar)) {
	        chklogicchar <- "<="
	      } else if (length(chklogicchar) == 2 && all(c(">=", ">") %in% chklogicchar)) {
	        chklogicchar <- ">="
	      } else if (length(chklogicchar) == 2 && all(c("<", ">") %in% chklogicchar)) {
	        chklogicchar <- "<>"
	      } else {
		    stop("more than one logic char: ", toString(chklogicchar))
		  }
	
	    if (chklogicchar %in% c("is.na", "is.null", "IS.NA", "IS.NULL")) {
	      partsplit <- unlist(strsplit(gsub(" ", "", part), paste0(chklogicchar, "\\(")))[[2]]
	      partsplit <- unlist(strsplit(partsplit, "\\)"))
	    } else {
	      partsplit <- unlist(strsplit(gsub(" ", "", part), chklogicchar))
	    }
        partmatch <- sum(partsplit %in% x)
	    if (partmatch != 1) {
	      return(NULL)
		  } else {
		    if (returnvar) {
		      return(partsplit[partsplit %in% x])
		    }
      }
	  }
	  return(part)
    }
  }

  ## Define function to remove odd parentheses
  remove.paren <- function(x) {
    x <- gsub(" ", "", x)
	  leftp <- sum(as.vector(gregexpr("\\(", x)[[1]]) > 0) 
	  rightp <- sum(as.vector(gregexpr("\\)", x)[[1]])> 0)
	  if (leftp > rightp) {
	    x <- sub("\\(", "", x)
	  } else if (rightp > leftp) {
	    x <- sub("\\)", "", x)
    }
	  return(x)
  }

  ## Return NULL if statement is NULL
  if (is.null(statement) || statement == "") {
    if (stopifnull) {
	    stop()
	  } else {
	    return(NULL)
    }
  }

  ## Defin logic.chars
  Rlogic.chars <- c("==", "<=", ">=", "%in%", "<", ">", "!=", "is.na", "is.null")
  SQLlogic.chars <- c("=", "<=", ">=", "notin", "in", "<>", "<", ">", "is.na", "is.null")
  Rlogic.chars.diff <- c("==", "%in%", "!=")
  SQLlogic.chars.diff <- c("=", "notin", "in", "<>")
  
  ## Define potential characters in logical statement
  if (syntax == "R") {
    logic.chars.diff <- SQLlogic.chars.diff
    logic.chars <- Rlogic.chars
  } else if (syntax == "SQL") {
    logic.chars.diff <- Rlogic.chars.diff
    logic.chars <- SQLlogic.chars
  }
  
  ## Set warning response
  filternm <- ifelse(!is.null(filternm), filternm, statement)
  fwarning <- paste(filternm, "is invalid")

  ## Check if x is data.frame or vector of names
  if (is.data.frame(x)) {
    x <- names(x)
  }

  ## Check statement, assuming not NULL
  if (is.null(statement)) return(NULL)
  if (!is.character(statement) | statement == "") stop(fwarning)
  if (statement != "NONE") {
#    if (!any(unlist(sapply(logic.chars,
#		function(x, statement){grep(x, statement)}, statement)))) {
#      if (grepl(equalother, statement) && sum(gregexpr(equalsign, statement)>0) == 0) {
#        message("must be R syntax.. changing ", equalother, " to ", equalsign)
#        statement <- gsub(equalother, equalsign, statement)
#      }
#    }

    ## Check if R 
    grept <- Rlogic.chars.diff[unlist(sapply(Rlogic.chars.diff,
		          function(x, statement){grepl(x, statement, ignore.case=TRUE)}, statement))]
				  
	  if (length(grept) > 0) {
	    if (syntax == "SQL") {
	      #message("syntax is R")
		    statement <- RtoSQL(statement)
	    }
	    syntax <- "R"
	  } 
    if (grepl("&", statement, ignore.case=TRUE) || grepl("\\|", statement, ignore.case=TRUE)) {
      syntax <- "R"
    } else if (grepl(" and ", statement, ignore.case=TRUE) || grepl(" or ", statement, ignore.case=TRUE)) {
      syntax <- "SQL"
    }	  
	
    ## Define potential characters in logical statement
    if (syntax == "R") {
      logic.chars <- Rlogic.chars
    } else if (syntax == "SQL") {
      logic.chars <- SQLlogic.chars
    }
	  if (syntax == "R") {
#      if (grepl("==", statement) && sum(gregexpr(equalsign, statement)>0) == 0) {
#        message("must be R syntax.. changing = to ==")
#        statement <- gsub("=", "==", statement)
#      }	  
      if (grepl("&&", statement)) {
        statement <- gsub("&&", "&", statement)
      }
      if (grepl("\\|\\|", statement)) {
        statement <- gsub("\\|\\|", "\\|", statement)
      }
	  }

    ## Check parentheses
    paren.left <- sum(attr(gregexpr("\\(", statement)[[1]], "match.length") > 0)
    paren.right <- sum(attr(gregexpr("\\)", statement)[[1]], "match.length") > 0)
    if (paren.left < paren.right) {
	    message("invalid logical statement... missing left parenthesis")
	    if (stopifinvalid) {
        stop()
	    } else {
	      return(NULL)
	    }
    } else if (paren.left > paren.right) {
	    message("invalid logical statement... missing right parenthesis")
	    if (stopifinvalid) {
        stop("invalid logical statement... missing right parenthesis")
	    } else {
	      return(NULL)
	    }
    } 

	  #statement <- gsub(" ", "", statement)
	  if (syntax == "R") {
	    andnm <- "&"
      ornm <- "\\|"
	  } else {
      andnm <- "^and$"
	    ornm <- "^or$"
      andnm <- ifelse(grepl(" and ", statement), " and ", " AND ")
      ornm <- ifelse(grepl(" or ", statement), " or ", " OR ")	  
	  }

    if (grepl(andnm, statement) && grepl(ornm, statement)) {
      partsAND <- trimws(unlist(strsplit(statement, andnm)[[1]]))
      partsOR <- trimws(unlist(strsplit(statement, ornm)[[1]]))

      if (sum(attr(gregexpr("\\(", partsAND)[[1]], "match.length")) == 
		    sum(attr(gregexpr("\\)", partsAND)[[1]], "match.length"))) {

        ## Split AND parts
		    parts <- unlist(sapply(partsAND, function(x) strsplit(x, ornm)))
		  
		    ## Remove odd parentheses
		    parts <- sapply(as.vector(parts), remove.paren)

        ## Check if there are any variables in x that match filter
        chkparts <- sapply(parts, chkpartnm, x, logic.chars)
			
      } else if (sum(attr(gregexpr("\\(", partsOR)[[1]], "match.length")) == 
		    sum(attr(gregexpr("\\)", partsOR)[[1]], "match.length"))) {
			
        ## Split OR parts 
		    parts <- unlist(sapply(partsOR, function(x) strsplit(x, andnm)))
		  
		    ## Remove odd parentheses
		    parts <- sapply(as.vector(parts), remove.paren)

        ## Check if there are any variables in x that match filter
        chkparts <- sapply(parts, chkpartnm, x, logic.chars, returnvar)
      }	  
	  
    } else if (grepl(andnm, statement) || grepl(ornm, statement)) {
	
      ## Split AND/OR parts 
      if (grepl(andnm, statement)) {
        parts <- trimws(unlist(strsplit(statement, andnm)[[1]]))
      } else if (grepl(ornm, statement)) {
        parts <- trimws(unlist(strsplit(statement, ornm)[[1]]))
      }
	  
      ## Remove odd parentheses		  
      parts <- sapply(parts, remove.paren)
	  
      ## Check if there are any variables in x that match filter
      chkparts <- sapply(parts, chkpartnm, x, logic.chars, returnvar)

	  } else {
	
	    chkparts <- chkpartnm(statement, x, logic.chars, returnvar)
	  }

    if (is.null(chkparts) || any(sapply(chkparts, is.null))) {
	    message(fwarning)
	    if (returnpart) { 
	      if (all(sapply(chkparts, is.null))) {
		      if (stopifinvalid) {
            stop()
          } else {
            return(NULL)
          }
		    }
	      if (sum(sapply(chkparts, is.null)) > 0 && sum(sapply(chkparts, is.null)) < length(chkparts)) {
		      return(names(chkparts)[!sapply(chkparts, is.null)])
		    }
	    }
      if (stopifinvalid) {
        stop()
      } else {
        return(NULL)
      }
    }
  }

  if (returnvar) {
    return(unlist(names(chkparts))) 
  } else {
    #message(statement)
    return(statement)
  }
}


#' @rdname checks_desc
#' @export
check.matchclass <- function(tab1, tab2, matchcol, var2=NULL, tab1txt=NULL, tab2txt=NULL) {
  ## Description: check that the class of matchcol in tab2 matches class in tab1
  ## 	  If different, changes class2 to class1
  ##	  For multiple variables with same name in both tables,
  ##		or, 1 variable with different name in tables (var2=NULL)
  ## ARGUMENTS:
  ## tab1 - data frame of first table
  ## tab2 - data frame of matching table
  ## matchcol - column to match
  ## var2 - if the matching variable is named different
  ## tab1txt - text for table 1
  ## tab2txt - text for table 2
  
  
  ## Define function
  unAsIs <- function(X) {
    ## Description: removes AsIs from class
    if ("AsIs" %in% class(X) && !any(c("sfc_POINT", "sfc") %in% class(X))) {
      class(X) <- class(X)[-match("AsIs", class(X))]
    }
    X
  }
  
  tab1 <- pcheck.table(tab1)
  tab2 <- pcheck.table(tab2)
  
  if (is.null(tab1txt)) tab1txt <- "tab1"
  if (is.null(tab2txt)) tab2txt <- "tab2"
  
  if (is.null(tab1) || is.null(tab2)) stop("invalid tables")
  
  if (length(matchcol) > 1 && !all(matchcol %in% names(tab1))) {
    nomatch <- matchcol[which(!matchcol %in% names(tab1))]
    stop(paste(toString(nomatch), "not in", tab1txt))
  }
  if (is.null(var2) && any(!matchcol %in% names(tab2)))
    stop(paste(toString(matchcol), "not in", tab2txt))
  if (!is.null(var2) && any(!var2 %in% names(tab2)))
    stop(paste(var2, "not in", tab2txt))
  
  if (is.data.table(tab1)) {
    tab1key <- key(tab1)
    setkey(tab1, NULL)
  }
  if (is.data.table(tab2)) {
    tab2key <- key(tab2)
    setkey(tab2, NULL)
  }
  
  if (!is.null(var2)) {
    #if (length(matchcol) > 1) stop("only 1 matchcol allowed if different names")
    if (length(matchcol) > length(var2)) stop("number of vars in matchcol different than vars2")
  }
  
  for (i in 1:length(matchcol)) {
    v <- matchcol[i]
    if (!is.null(var2)) {
      v2 <- var2[i]
      v1 <- v
    } else {
      v1 <- v
      v2 <- v
    }
    ## if both classes are factors, check levels
    if (inherits(tab1[[v1]], what = "factor") && inherits(tab2[[v2]], what = "factor")) {
      v1levels <- levels(tab1[[v1]])
      v2levels <- levels(tab2[[v2]])
      if (!identical(v1levels, v2levels)) {
        message("factor levels do not match")
        if (all(v2levels %in% v1levels))
          levels(tab2[[v2]]) <- v1levels
      }
    } else if (inherits(tab1[[v1]], what = "factor")) {
      v1levels <- levels(tab1[[v1]])
      if (!all(unique(tab2[[v2]]) %in% v1levels)) {
        misslevel <- unique(tab2[[v2]])[!unique(tab2[[v2]]) %in% v1levels]
        message("missing level values: ", toString(misslevel), "... returning NA factor values")
      }
      if (all(v1levels %in% tab2[[v2]])) {
        tab2[[v2]] <- factor(tab2[[v2]], levels=v1levels)
      } else {
        #message("missing factor levels")
        tab2[[v2]] <- factor(tab2[[v2]], levels=v1levels)
      }
    } else if (inherits(tab2[[v2]], what = "factor")) {
      tab2[[v2]] <- as.character(tab2[[v2]])
    }
    
    #tab1 <- tab1[, lapply(.SD, unAsIs)]
    #tab2 <- tab2[, lapply(.SD, unAsIs)]
    tab1[,] <- lapply(tab1[,], unAsIs)
    tab2[,] <- lapply(tab2[,], unAsIs)
    
    tab1.class <- class(tab1[[v1]])
    tab2.class <- class(tab2[[v2]])
    
    ## coercion order: logical-integer-numeric-complex-character
    coerce.order <- c("logical", "integer", "integer64", "numeric", "complex", "character")
    tab.order <- na.omit(match(c(tab1.class, tab2.class), coerce.order))
  
    if (length(tab.order) == 2) {
      if (tab.order[1] < tab.order[2]) {
        if (coerce.order[tab.order[2]] == "integer64") {
          fun2 <- as.integer
        } else {
          fun2 <- get(paste0("as.", coerce.order[tab.order[2]]))
        }
        tab1[[v1]] <- fun2(tab1[[v1]])
      } else if (tab.order[2] < tab.order[1]) {
        if (coerce.order[tab.order[1]] == "integer64") {
          fun1 <- as.integer
        } else {
          fun1 <- get(paste0("as.", coerce.order[tab.order[1]]))
        }
        tab2[[v2]] <- fun1(tab2[[v2]])
      }
    } else if (tab1.class %in% coerce.order) {
      if (tab1.class == "integer64") {
        fun1 <- as.integer
      } else {
        fun1 <- get(paste0("as.", tab1.class))
      }
      tab1[[v1]] <- fun1(tab1[[v1]])
    } else if (tab2.class != tab1.class) {
      class(tab2[[v2]]) <- tab1.class
    }
  }
  
  if (is.data.table(tab1))
    setkeyv(tab1, tab1key)
  if (is.data.table(tab2))
    setkeyv(tab2, tab2key)
  
  return(list(tab1=tab1, tab2=tab2))
}


#' @rdname checks_desc
#' @export
check.matchval <- function(tab1, tab2, var1, var2=NULL, tab1txt=NULL, tab2txt=NULL,
	gui=FALSE, stopifmiss=FALSE, subsetrows=FALSE) {

  ## Description: check that the values of var1 in tab1 are all in matchcol in tab2
  ## 	  If missing values, stops and sends error.
  ##	  var1 and var2 must be the same length, if greater than 1, columns are concatenated
  ##   Does not return anything
  ## var1 - variable name in tab1
  ## var2 - variable name in tab2
  ## stopifmiss - stop if the number of missing values is greater than 1
  ## subsetrows - remove rows of tab1 with missing values and return table

  nbr.missval=missval=MATCH <- NULL

  ## Check tables
  tab1 <- pcheck.table(tab1, gui=gui)
  if (is.null(tab1)) stop("tab1 is NULL")

  tab2 <- pcheck.table(tab2, gui=gui)
  if (is.null(tab2)) stop("tab2 is NULL")

  if (is.null(tab1txt)) tab1txt <- "tab1"
  if (is.null(tab2txt)) tab2txt <- "tab2"

  ## Check names in tables
  ## Check var1 and var2 parameters
  if (!all(var1 %in% names(tab1))) {
    misscol <- var1[which(!var1 %in% names(tab1))]
    stop(paste0("missing variables in ", tab1txt, ": ", paste(misscol, collapse=", ")))
    #if (is.factor(tab1[[var1]])) tab1[[var1]] <- as.character(tab1[[var1]])
  }
  if (!is.null(var2)) {
    if (!all(var2 %in% names(tab2))) {
      misscol <- var2[which(!var2 %in% names(tab2))]
      stop(paste0("missing variables in ", tab2txt, ": ", paste(misscol, collapse=", ")))
      #if (is.factor(tab2[[var2]])) tab2[[var2]] <- as.character(tab2[[var2]])
    }
    if (length(var2) != length(var1))
      stop("number of variables in var2 must equal number of variables in var1")
  } else {
    var2 <- var1
  }
 
  ## Get unique values of matchcol in tab1
  if (length(var1) > 1) {
    tab1.vals <- unique(na.omit(tab1[, do.call(paste, .SD), .SDcols=var1]))
    tab2.vals <- unique(na.omit(tab2[, do.call(paste, .SD), .SDcols=var2]))

    missval <- tab1.vals[which(!tab1.vals %in% tab2.vals)]
    nbr.missval <- length(missval)
    if (nbr.missval == length(tab1.vals)) {
      stop("no matching values in ", tab1txt, " and ", tab2txt)
    } else if (nbr.missval > 0) {
      if (nbr.missval <= 25) {
        message(paste0(nbr.missval, " values in ", tab1txt, " not in ", tab2txt, ": ",
	 	paste(missval, collapse=", ")))
      } else {
        message(paste("there are", nbr.missval, "values in", tab1txt,
		"not in", tab2txt))
      }
      if (stopifmiss) {
        message("ERROR"); stopQ()  }
    }
    if (subsetrows) {
      tab1[, MATCH:= do.call(paste, .SD), .SDcols=var1]
      tab1 <- tab1[!MATCH %in% missval, ]
      tab1[, MATCH := NULL]
    }
  } else {
    tab1.vals <- na.omit(unique(tab1[[var1]]))
    tab2.vals <- na.omit(unique(tab2[[var2]]))

    missval <- tab1.vals[which(!tab1.vals %in% tab2.vals)]
    nbr.missval <- length(missval)

    if (nbr.missval == length(tab1.vals)) {
      stop("no matching values in ", tab1txt, " and ", tab2txt)
    } else if (nbr.missval > 0) {
      if (nbr.missval < 20) {
        message(paste0(nbr.missval, " values in ", tab1txt, " not in ",
			tab2txt, ": ", paste(missval, collapse=", ")))
      } else {
        message(paste("there are", nbr.missval, "missing values in",
			tab1txt))
      }
      if (stopifmiss) {
        message("ERROR"); stopQ()  }

      if (subsetrows) {
        tab1n <- nrow(tab1)
        tab1 <- tab1[tab1[[var1]] %in% tab2.vals, ]
        message(paste("subsetting", tab1n, "rows of", tab1txt, "to", nrow(tab1), "rows"))
      }
    }
  }
  return(tab1)
}


