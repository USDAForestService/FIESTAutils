#' @rdname pcheck_desc
#' @export
popTabchk <- function(tabnames, tabtext, tabs, tabIDs, dbtablst, dbconn, datindb=FALSE) {
  ## DESCRIPTION: check name in tabs list
  
  tabnames <- c("pltu", "plotu", "plt", "plot")
  
  tabx=tabnm <- NULL
  tabchk <- sapply(tabnames, findnm, names(tabs), returnNULL = TRUE)
  tabchk <- tabchk[!sapply(tabchk, is.null)]

  if (is.null(tabchk) || length(tabchk) == 0) {
    return(NULL)
  } else {
    tablst <- lapply(tabchk, function(x) tabs[[x]])

    #tablst <- names(tablst)[!duplicated(lapply(names(tablst), findnm, dbtablst, returnNULL = TRUE))]
    if (!all(unlist(lapply(tablst, is.data.frame)))) {
      tablst <- tablst[!duplicated(lapply(tablst, findnm, dbtablst, returnNULL = TRUE))]
    }

    if (!is.null(dbconn)) {
      dbtabchk <- sapply(tablst, findnm, dbtablst, returnNULL = TRUE)
      dbtabnmchk <- names(dbtabchk)[!unlist(lapply(dbtabchk, is.null))]
	    if (all(is.na(dbtabnmchk))) {
	      message("invalid name for ", tabtext, ": ", tab)
	      return(0)
	    } else if (length(dbtabnmchk) > 1) {
	      dbtabnmchk <- dbtabnmchk[1]
	    }

      tab <- tabs[[dbtabnmchk]]
      tabid <- tabIDs[[dbtabnmchk]]
      tabflds <- DBI::dbListFields(dbconn, tab)
      tabnm <- findnm(tablst[[dbtabnmchk]], dbtablst)
      
	  } else {
	    tabchk1 <- unlist(tabchk)[1]
      if (length(tabchk) > 1) {
        tab <- tabs[[tabchk1]]
        tabid <- tabIDs[[tabchk1]]
        tabx <- pcheck.table(tab, tabnm = tabtext, caption = paste0(tabtext, "?"))
	  
	      if (is.null(tabx)) {
	        tabchk2 <- unlist(tabchk)[2]
	        if (!is.null(tabchk2)) {
	          tab <- tabs[[tabchk2]]
            tabid <- tabIDs[[tabchk2]]
            tabx <- pcheck.table(tab, tabnm = tabtext, paste0(tabtext, "?"))
	        }
	      }
	    } else {
	      tab <- tabs[[tabchk1]]
	      tabid <- tabIDs[[tabchk1]]
        tabx <- pcheck.table(tab, tabnm = tabtext, paste0(tabtext, "?"))
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
