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
  classo=classnew=classf=byvar.val <- NULL
  
  ## define new variables for grouping strvar
  x$classo <- x[[xvar]]  # original class
  
  ## make sure x is a data.table
  if (!"data.table" %in% class(x)) {
    x <- data.table(x)
  }

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
  ltmin <- unique(x[x[[nvar]] <= minplotnum][["classf"]])
  #gtmin <- unique(x[x[[nvar]] > minplotnum[["classf"]])
  
  ## define vector of aggregated classes
  agclass <- {}
  for (ltclass in ltmin) {
    #ltclass = ltmin[i]
    
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
            msg <- paste0("not enough plots to reach minplotnum (", minplotnum, ")... ", maxag)
            message(msg)
            break()
          }
          
          ## get highest factored class number in list... 
          classag <- max(x$classf[x$classf < min(agclass)])
          class2 <- unique(x[classf == classag][["classf"]])
          
          ## get the new class for class2
          classnew <- x[x$classf %in% class2]$classnew
          classnewchk <- strsplit(classnew, "-")[[1]]
          
          ###  here is where the problem is
          if (!class2 %in% c(-1, classag) || class2 %in% classnewchk) {
            #agclass <- c(agclass, strsplit(class2, "-")[[1]])
            agclass <- c(agclass, classnew)
          } else {
            agclass <- c(agclass, classag)
          }

          agclass <- unlist(sapply(agclass, function(ag) strsplit(ag, "-")[[1]]))
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



