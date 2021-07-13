sm_names <- function(inputTable){

  myRepairFunc <- function(x){
  stringr::str_trim(
    paste0(
      stringr::str_to_lower(stringr::str_sub(x,1,1)),
      stringr::str_sub(x,2,-1)
    ))}

rename_sm <- function(x = NULL,
                      n.options = 4,
                      n.row.col = 5,
                      show.overview = TRUE,
                      outer.inner.sep = "_",
                      matrix.row.col.sep = ".",
                      name.component.repair = NULL){

  ## Making an SM re-namer
  ## This is not very robust, and makes assumptions about the meanings of characters.
  ### * If you put hyphens in your matrix questions, it will probably break.

  suppressWarnings({

  })

  nameOuters <- tibble::tibble(outers = names(x)) # tibble is convenient for using tidyr::fill
  nameInners <- as.character(x[1,])
  responses <- x[-1,]

  ## Replace "X11" type values in nameOuters$outer with NA
  # ("^X\\d+$" regex is a string starting with 'X', followed only by one or more digits)
  nameOuters[["outers"]][ stringr::str_detect(nameOuters[["outers"]], "^X\\d+$") ] <- NA
  # 'fill' NAs: c("Something", NA, NA, "hmm", NA) becomes c("Something", "Something", "Something", "hmm", "hmm")
  nameOuters <- tidyr::fill(nameOuters, outers, .direction = "down")
  nameOuters <- nameOuters$outers #No longer need this to be a tibble

  ## Intermediate nameData tibble will help to create a list structure

  nameData <- tibble::tibble(nameOuters = factor(nameOuters, levels = unique(nameOuters), ordered = TRUE), nameInners) #factor levels ensure question order is preserved
  rm(nameOuters, nameInners)

  nameData <- dplyr::mutate(nameData, nameInners = replace(nameInners, nameInners == "Response" |
                                                             nameInners == "Open-Ended Response" |
                                                             nameInners == "NA", NA))

  nameData <- dplyr::group_by(nameData, nameOuters)

  ## Label question types for further processing

  nameData <- dplyr::mutate(nameData, responseType =
                              {
                                get_resp_type <- function(inner.fields){
                                  if(length(inner.fields) == 1) type <- "singleResponse"

                                  else if( !all(stringr::str_detect(inner.fields, "[:space:]-[:space:]"))) type <- "multipleResponses"
                                  else {
                                    ## They all contain " - ", so we need to check for matrix question notation
                                    assume <- TRUE

                                    rowFields = stringr::str_extract(inner.fields, "^.+(?=([:space:]-[:space:]))")
                                    colFields = stringr::str_extract(inner.fields, "(?<=([:space:]-[:space:])).+$")

                                    uniqueRows <- dplyr::n_distinct(rowFields)
                                    uniqueCols <- dplyr::n_distinct(colFields)

                                    #
                                    if((length(inner.fields)/uniqueRows) != uniqueCols) assume <- FALSE

                                    for(rowset in 2:(uniqueRows - 1)){
                                      if( !all(colFields[1:uniqueCols] == colFields[rowset * uniqueCols + (1:uniqueCols)])) assume <- FALSE
                                    }

                                    for(colset in 0:(uniqueCols - 1)){
                                      if(!(dplyr::n_distinct(rowFields[colset * uniqueCols + (1:uniqueCols)]) == 1)) assume <- FALSE
                                    }

                                    if(assume) type <- "matrix"
                                    else type <- "multipleResponse"
                                  }
                                  return(type)
                                }
                                as.factor(get_resp_type(nameInners))
                              })

  nameData <- dplyr::mutate(nameData,
                            matrixRow = ifelse(responseType == "matrix", stringr::str_extract(nameInners, "^.+(?=([:space:]-[:space:]))"), NA),
                            matrixCol = ifelse(responseType == "matrix", stringr::str_extract(nameInners, "(?<=([:space:]-[:space:])).+$"), NA))


  ## Show an overview of all the questions if user wishes
  if(show.overview){
    ## This is probably a silly way of doing this
    removeMe <- dplyr::mutate(nameData,
                              newOuterName =
                                {
                                  get_new_outer_dummy <- function(outer.names = NULL, inner.names = NULL, response.types = NULL, matrix.rows = NULL, matrix.cols = NULL){

                                    cat("-----------------------------------------------------------------------------\n\n")
                                    matrixRows <- unique(matrix.rows)
                                    matrixCols <- unique(matrix.cols)

                                    cat("Current question name outer:", "\t")
                                    cat(unique(as.character(outer.names)),"\n\n")

                                    cat("Question type:\t")
                                    cat(unique(as.character(response.types)), "\n\n")

                                    if(all(response.types == "multipleResponses")){
                                      cat("Question options (1-", min(n.options,length(inner.names)), "):", "\n", sep = "")
                                      special <<- inner.names
                                      cat(as.character(inner.names)[1:min(n.options,length(inner.names))], "\n\n", sep = "\n")
                                    } else if (all(responseType == "matrix")){
                                      cat("Matrix rows:", "\n")
                                      cat(matrixRows[1:min(n.row.col, length(matrixRows))], sep = "\n")
                                      cat("\n")
                                      cat("Matrix columns:", "\n")
                                      cat(matrixCols[1:min(n.row.col, length(matrixCols))], sep = "\n")
                                      cat("\n")
                                    } else {}

                                    # out <- readline("Input new question name outer:\n\t")
                                    out <- NA
                                    out <- rep(out, length(outer.names))

                                    return(out)

                                  }

                                  # debugonce(get_new_outer)
                                  get_new_outer_dummy(outer.names = nameOuters, inner.names = nameInners, response.types = responseType,
                                                      matrix.rows = matrixRow, matrix.cols = matrixCol)
                                })
    rm(removeMe)
  }

  ## Get new outers from user
  namingStructure <- dplyr::mutate(nameData,
                                   newOuterName =
                                     {
                                       get_new_outer <- function(outer.names = NULL, inner.names = NULL, response.types = NULL, matrix.rows = NULL, matrix.cols = NULL){

                                         matrixRows <- unique(matrix.rows)
                                         matrixCols <- unique(matrix.cols)
                                         cat("-----------------------------------------------------------------------------\n\n")
                                         cat("Current question name outer:", "\t")
                                         cat(unique(as.character(outer.names)),"\n\n")

                                         cat("Question type:\t")
                                         cat(unique(as.character(response.types)), "\n\n")

                                         if(all(response.types == "multipleResponses")){
                                           cat("Question options (1-", min(n.options,length(inner.names)), "):", "\n", sep = "")
                                           cat(as.character(inner.names)[1:min(n.options,length(inner.names))], "\n\n", sep = "\n")
                                         } else if (all(responseType == "matrix")){
                                           cat("Matrix rows:", "\n")
                                           cat(matrixRows[1:min(n.row.col, length(matrixRows))], sep = "\n")
                                           cat("\n")
                                           cat("\t\tMatrix columns:", "\n")
                                           cat(matrixCols[1:min(n.row.col, length(matrixCols))], sep = "\n")
                                           cat("\n")
                                         } else {}

                                         # out <- readline("Input new question name outer:\n\t")
                                         out <- readline("New outer name: ")
                                         out <- rep(out, length(outer.names))

                                         return(out)

                                       }

                                       # debugonce(get_new_outer)
                                       get_new_outer(outer.names = nameOuters, inner.names = nameInners, response.types = responseType,
                                                     matrix.rows = matrixRow, matrix.cols = matrixCol)
                                     })

  namingStructure <- dplyr::ungroup(namingStructure)
  namingStructure <- dplyr::mutate(namingStructure, nameInners = factor(nameInners, levels = unique(nameInners), ordered = TRUE))
  namingStructure <- dplyr::group_by(namingStructure, nameOuters, responseType, nameInners)

  namingStructure <- dplyr::mutate(namingStructure,
                                   newInner =
                                     {
                                       get_new_inner <- function(new.outer.name = NULL, inner.name = NULL, response.type = NULL){

                                         if(response.type != "multipleResponses"){
                                           out <- NA

                                         } else {

                                           cat("-----------------------------------------------------------------------------\n\n")
                                           cat("Question outer (new):\t", new.outer.name, "\n")
                                           cat("Question inner (old):\t", as.character(inner.name), "\n\n")

                                           out <- readline("New question inner name:")

                                         }
                                         return(out)
                                       }

                                       # debugonce(get_new_outer)
                                       get_new_inner(new.outer.name =  newOuterName, inner.name = nameInners, response.type = responseType)
                                     })

  message("## Renaming matrix question rows:\n\n")
  namingStructure <- dplyr::group_by(namingStructure, nameOuters, matrixRow, responseType)
  namingStructure <- dplyr::mutate(namingStructure,
                                   newMatrixRows =
                                     {
                                       get_new_matrix_rows <- function(new.outer.name = NULL, old.matrix.row = NULL, response.type = NULL, old.matrix.cols = NULL){
                                         if(any(as.character(response.type) != "matrix")){
                                           out <- NA
                                         } else {
                                           cat("--------------------------------------------------------------------\n\n")
                                           cat("Question outer (new):\t", unique(new.outer.name), "\n")
                                           cat("Matrix cols:\n\n")
                                           cat(old.matrix.cols, sep = "\n")
                                           cat("\nCurrent matrix row name:", unique(old.matrix.row), "\n\n")

                                           out <- readline("New matrix row name:")
                                         }
                                         return(out)
                                       }
                                       get_new_matrix_rows(new.outer.name = newOuterName, old.matrix.row = matrixRow,
                                                           response.type = responseType, old.matrix.cols = matrixCol)
                                     })

  namingStructure <- dplyr::group_by(namingStructure, nameOuters, matrixCol, responseType)
  namingStructure <- dplyr::mutate(namingStructure,
                                   newMatrixCols =
                                     {
                                       get_new_matrix_rows <- function(new.outer.name = NULL, old.matrix.col = NULL, response.type = NULL, new.matrix.rows = NULL){
                                         if(any(as.character(response.type) != "matrix")){
                                           out <- NA
                                         } else {
                                           cat("--------------------------------------------------------------------\n\n")
                                           cat("Question outer (new):\t", unique(new.outer.name), "\n")
                                           cat("Matrix rows:\n\n")
                                           cat(new.matrix.rows, sep = "\n")
                                           cat("\nCurrent matrix col name:", unique(old.matrix.col), "\n\n")

                                           out <- readline("New matrix col name:")
                                         }
                                         return(out)
                                       }
                                       get_new_matrix_rows(new.outer.name = newOuterName, old.matrix.col = matrixCol,
                                                           response.type = responseType, new.matrix.rows = newMatrixRows)
                                     })

  ## Join name components into full names

  namingStructure <- dplyr::ungroup(namingStructure)
  if(!is.null(name.component.repair)){
    namingStructure <- dplyr::mutate(namingStructure, dplyr::across(c(dplyr::all_of(c("newOuterName", "newInner", "newMatrixRows", "newMatrixCols"))),
                                                                    name.component.repair))
  }
  namingStructure[["outerInnerSep"]] <- NA
  namingStructure[["outerInnerSep"]][namingStructure[["responseType"]] == "multipleResponses" |
                                       namingStructure[["responseType"]] == "matrix"] <- outer.inner.sep
  namingStructure[["matrixRowColSep"]] <- NA
  namingStructure[["matrixRowColSep"]][namingStructure[["responseType"]] == "matrix"] <- matrix.row.col.sep

  namingStructure <- dplyr::mutate(namingStructure,
                                   fullName =
                                     glue::glue("{newOuterName}{outerInnerSep}{newInner}{newMatrixRows}{matrixRowColSep}{newMatrixCols}",
                                                .na = "")
  )

  out <- setNames(responses, namingStructure[["fullName"]])

  return(out)

}

newData <- rename_sm(inputTable, name.component.repair = myRepairFunc)


## hmm not quite right yet...
### Oh also big mistake to use hyphens lol (guess what '-' does)

fixedNames <- stringr::str_remove_all(names(newData), "NA")
fixedNames <- stringr::str_replace_all(fixedNames, "-", ".")

fixedNames

}

