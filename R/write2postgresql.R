#' @rdname write2_desc
#' @export 
write2postgresql <- function(layer, 
                             dbconn,
                             out_name = NULL,
                             lowernames = FALSE,
                             schema = NULL,
                             append_layer = FALSE, 
                             overwrite = FALSE,
                             index.unique = NULL,
                             index = NULL,
                             index_type = "btree") {
  
  if (is.null(dbconn)) stop("must supply a database object.")
  if (!DBI::dbIsValid(dbconn)) stop("database object is not valid.")
  if (DBI::dbIsReadOnly(dbconn)) stop("database object is read only.")

  if (append_layer && overwrite) {
    stop("append_layer and overwrite cannot both be set to TRUE")
  } else {
    txt <- ifelse(append_layer, "appending ",
                  ifelse(overwrite, "overwriting ", "writing "))
  }
  
  if (is.null(out_name)) out_name <- "layer"
  layer <- setDF(pcheck.table(layer))
  if (lowernames) {
    names(layer) <- tolower(names(layer))
  }
  
  if (!is.null(schema)) {
    out_name_spec <- DBI::Id(table = out_name, schema = schema)
    out_name_str <- paste0(schema, ".", out_name)
  } else {
    out_name_spec <- out_name_str <- out_name
  }
  
  DBI::dbWriteTable(conn = dbconn,
                    name = out_name_spec,
                    value = layer,
                    append = append_layer,
                    overwrite = overwrite)
  
  message(paste0(txt, out_name, " to database"))
  
  if (!is.null(index.unique)) {
    if (!is.list(index.unique)) {
      index.unique <- as.list(index.unique)
    }
    for (i in seq_along(index.unique)) {
      indexu_i <- index.unique[[i]]
      if (lowernames) {
        indexu_i <- tolower(indexu_i)
      }

      if (!all(indexu_i %in% names(layer))) {
        indexuchk <- sapply(indexu_i, findnm, names(layer), returnNULL = TRUE)
        if (all(is.null(unlist(indexuchk)))) {
          stop("invalid index.unique... names not in layer: ", toString(indexu_i))
        } else {
          indexu_i <- indexuchk
        }
      } else {
        indxu_i_nm <- paste0(out_name, tolower(indexu_i), "idx", collapse = "_")
        if (any(duplicated(layer[ , indexu_i]))) {
          warning(indxu_i_nm, " is not unique... creating non-unique index\n")
          idxsql <- paste0("CREATE INDEX ", indxu_i_nm, " ON ",
                           out_name_str, " USING ", index_type,
                           " (", paste0(indexu_i, collapse = ","), ")")
        } else {
          idxsql <- paste0("CREATE UNIQUE INDEX ", indxu_i_nm, " ON ",
                           out_name_str, " USING ", index_type,
                           " (", paste0(indexu_i, collapse = ","), ")")
          
          indexu_add <- tryCatch(
            DBI::dbExecute(dbconn, idxsql),
            error = function(err) {
              message(err, "\n")
            }
          )
          if (!is.null(indexu_add)) {
            message(sub("create", "creating", idxsql))
          }
        }
      }
    }
  }
  if (!is.null(index)) {
    if (!is.list(index)) {
      index <- as.list(index)
    }
    for (i in seq_along(index)) {
      index_i <- index[[i]]
      if (lowernames) {
        index_i <- tolower(index_i)
      }
      index_i_nm <- paste0(out_name, tolower(index_i), "idx", collapse = "_")
      
      if (!all(index_i %in% names(layer))) {
        indexchk <- sapply(index_i, findnm, names(layer), returnNULL = TRUE)
        if (all(is.null(unlist(indexchk)))) {
          stop("invalid index... names not in layer: ", toString(index_i))
        } else {
          index_i <- indexchk
        }
      } else {
        message("adding index: ", index_i_nm, " to ", out_name)
        idxsql <- paste0("CREATE INDEX ", index_i_nm, " ON ",
                         out_name_str, " USING ", index_type, 
                         " (",  paste0(index_i, collapse = ","), ")")
        index_add <- tryCatch(
          DBI::dbExecute(dbconn, idxsql),
          error = function(err) {
            message(err, "\n")
          }
        )
        if (!is.null(index_add)) {
          message(sub("create", "creating", idxsql))
        }
      }
    }
  }
  
}