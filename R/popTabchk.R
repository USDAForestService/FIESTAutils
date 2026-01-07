#' @rdname pcheck_desc
#' @export
popTabchk <- function(tabnames, tabtext, 
                      tabs, tabIDs, 
                      dbtablst, 
                      dbconn, schema = NULL, 
                      datindb = FALSE) {
  ## DESCRIPTION: check name in tabs list
  tabx=tabnm <- NULL
  
  ## Check if possible names in tabnames is in the list of input tables (tabs)
  tabchk <- sapply(tabnames, findnm, names(tabs), returnNULL = TRUE)
  tabchk <- tabchk[!sapply(tabchk, is.null)]

  if (is.null(tabchk) || length(tabchk) == 0) {
    return(NULL)
  } else {
    
    ## if any tabnames is in tabs, get which tables in tabs matches the list names
    tablst <- lapply(tabchk, function(x) tabs[[x]])

    ## Now, check if any of these tables are data.frames
    tabdfchk <- unlist(lapply(tablst, is.data.frame))
    if (any(tabdfchk)) {
      tab <- names(tabdfchk)[tabdfchk]
      if (length(tab) > 1) tab <- tab[1]   ## if more that one table exists, use the first one
      tabid <- tabIDs[[tab]]
      tabx <- pcheck.table(tablst[[tab]], tabnm = tabtext)
      
      if (!is.null(tabx) && nrow(tabx) > 0) {
        tabnm <- paste0(tabtext, "x")
        tabflds <- names(tabx)
        return(list(tabnm=tabnm, tabid=tabid, tabflds=tabflds, tabx=tabx))
      }
    } else {
      tablst <- tablst[!duplicated(lapply(tablst, findnm, dbtablst, returnNULL = TRUE))]
    }

    if (!is.null(dbconn)) {
      dbtabchk <- sapply(tablst, findnm, dbtablst, returnNULL = TRUE)
      dbtabnmchk <- names(dbtabchk)[!unlist(lapply(dbtabchk, is.null))]
	    if (all(is.na(dbtabnmchk))) {
	      message("invalid name for ", tabtext)
	      stop()
	    } else if (length(dbtabnmchk) > 1) {
	      dbtabnmchk <- dbtabnmchk[1]
	    }
      tab <- tabs[[dbtabnmchk]]
      tabid <- tabIDs[[dbtabnmchk]]
      tabnm <- findnm(tablst[[dbtabnmchk]], dbtablst)
      tabflds <- dbgetflds(conn = dbconn, schema = schema, tabnm = tabnm, upper = TRUE)
      
	  } else {

	    tabchk1 <- unlist(tabchk)[1]
      if (length(tabchk1) > 1) {
        tab <- tabs[[tabchk1]]
        tabid <- tabIDs[[tabchk1]]
        tabx <- pcheck.table(tab, tabnm = tabtext, caption = paste0(tabtext, "?"))

	      if (is.null(tabx)) {
	        tabchk2 <- unlist(tabchk)[2]
	        if (!is.null(tabchk2)) {
	          tab <- tabs[[tabchk2]]
            tabid <- tabIDs[[tabchk2]]
            tabx <- pcheck.table(tab, tabnm = tabtext, caption = paste0(tabtext, "?"))
	        }
	      }
	    } else {
	      tab <- tabs[[tabchk1]]
	      tabid <- tabIDs[[tabchk1]]
        tabx <- pcheck.table(tab, tabnm = tabtext, caption = paste0(tabtext, "?"))
      }
      if (!is.null(tabx)) {
        tabnm <- paste0(tabtext, "x")
        tabflds <- names(tabx)
      } else {
		    return(list(tab = tab, tabid = tabid))
      }
    }
  }

  returnlst <- list(tabnm=tabnm, tabid=tabid, tabflds=tabflds, tabx=tabx)
  return(returnlst)
}
