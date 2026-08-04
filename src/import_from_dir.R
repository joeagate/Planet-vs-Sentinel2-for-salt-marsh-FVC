#' Import files from a directory
#' 
#' @description
#' This function finds all files in a directory matching a regex pattern.
#' 
#' @details
#' The function first sets the working directory to the search directory and
#' then imports all files in the directory.
#' 
#' There is an option to bind the rows of the imported data if it is of type
#' data frame using dplyr::bind_rows.
#' 
#' @param directory The directory in which to search.

# TODO Add the rest of the parameter to the header

import_from_dir <- function(directory, pattern, import_function, 
                            bind_rows = FALSE, verbose = TRUE, 
                            recursive = FALSE, force_character = FALSE,
                            type_convert = FALSE, also_filenames = FALSE,
                            full_names = FALSE){
  
  # Get the current wd
  originalwd <- getwd()
  
  # Set wd to the search 
  setwd(directory)
  files <- list.files(pattern = pattern, recursive = recursive, 
                      full.names = full_names)
  output <- lapply(files, import_function)
  setwd(originalwd)
  
  # Forces all columns to character to help with row binding
  if(force_character == TRUE){
    # TODO Add a check point to throw an error if not a data frame
    output <- lapply(output, function(x) {
      data.frame(lapply(x, as.character), stringsAsFactors = FALSE)
    })
  }
  
  # Bind rows of the output using dplyr bind_rows
  # dplyr::bind_rows is chosen over rbind due to flexibility of missing columns
  if(bind_rows == TRUE){
    # TODO Add a check point to throw an error if not a data frame
    output <- do.call(dplyr::bind_rows, output)
  }
  
  # Convert the columns to optimal types using type.convert
  if(type_convert == TRUE){
    output <- dplyr::mutate(output, dplyr::across(dplyr::everything(), ~ type.convert(., as.is = T)))
  }
  
  # Create two lists with the files includes as well
  if (also_filenames == TRUE) {
    lst <- list()
    
    lst[['filenames']] <- files
    lst[['files']] <- output
    
    output <- lst
  }
  
  if(verbose == TRUE) {
    cat('\n')
    cat('Imported:\n')
    cat(files, sep = '\n')
    # TODO Add a verbose output for the different options?
    cat('\n')
    cat('DONE\n')
  }
  
  
  return(output)
}
