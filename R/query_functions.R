# getjoinqry - creates string of for a SQL query joining multiple ids

#' @rdname internal_desc
#' @export
getjoinqry <- function(joinid1, joinid2) {
  ## DESCRIPTION - creates string of for a SQL query joining multiple ids
  joinqry <- "ON ("
  for (i in 1:length(joinid1)) {
    joinqry <- paste0(joinqry, "p.", joinid1[i], " = ppsa.", joinid2[i])
	if (i == length(joinid1)) {
	  joinqry <- paste0(joinqry, ")")
	} else {
	  joinqry <- paste(joinqry, "AND ")
	}
  }
  return(joinqry)
}
 
