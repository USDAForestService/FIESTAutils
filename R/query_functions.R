# getjoinqry - creates string of for a SQL query joining multiple ids

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
 