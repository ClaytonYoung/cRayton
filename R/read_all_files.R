#' Read and Clean Multiple Files
#'
#' This function reads in multiple files (CSV and Excel), cleans the column names, 
#' and combines the data into a list.
#'
#' @param filenames A character vector of file paths.
#' @param prefix An optional prefix for the output list names.
#' @param skips Number of rows to skip when reading the file.
#' @param clean_names Logical, whether to clean column names using `janitor::make_clean_names`.
#' @param qualtrics Logical, whether the CSV is in Qualtrics format.
#' @param col_types Column types, passed to `readxl::read_excel`.
#' @return A list of cleaned data frames.
#' @export

read_all_files <- function(filenames, prefix = NULL, skips = 0, clean_names = TRUE, qualtrics = FALSE, col_types = NULL) {
  
  # Ensure filenames is a character vector
  if(!is.character(filenames)) {
    stop("filenames should be a character vector.")
  }
  
  # Ensure filenames are in CSV or Excel format
  invalid_files <- filenames[!stringr::str_detect(filenames, '\\.(csv|xlsx?|xls)$')]
  if (length(invalid_files) > 0) {
    stop("The following files are neither in .csv nor .xlsx/.xls format: ", paste(invalid_files, collapse=", "))
  }
  
  # Naming formatting function
  naming <- function(x) {
    x %>% basename() %>%
      stringr::str_replace_all(c(".csv|.xlsx|.xls|\\-.*"), "") %>%
      janitor::make_clean_names()
  }
  
  # Data cleaning function
  clean_data <- function(df) {
    df <- df %>% 
      janitor::remove_empty(c('cols', 'rows')) %>%
      dplyr::select(-tidyr::all_of(dplyr::matches('pidn\\b')), if(any(dplyr::matches('pidn\\b'))) dplyr::first(dplyr::matches('pidn\\b'))) %>%
      dplyr::relocate(dplyr::matches('pidn\\b')) 
    
    df <- tryCatch(
      dplyr::rename_with(df, .fn = function(name) {
        ifelse(stringr::str_detect(name, 'pidn\\b'), 'PIDN', name)
      }),
      error = function(e) df
    )
    
    if (clean_names) {
      df <- tryCatch(
        df %>%
          dplyr::rename_with(~janitor::make_clean_names(.), .cols = -c(dplyr::matches('pidn\\b'), dplyr::matches('DCDate'))),
        error = function(e) df) 
    }
    return(df)
  }
  
  # Group filenames by type
  csv_files <- filenames[stringr::str_detect(filenames, '\\.csv')]
  excel_files <- filenames[stringr::str_detect(filenames, '\\.xlsx?$')]
  
  # CSV files reader
  read_and_clean_csv <- function(filename) {
    csv_name <- stringr::str_c(dplyr::enexpr(prefix), naming(filename[stringr::str_detect(filename, '\\.csv')]), sep = "_")
    
    df <- if (qualtrics) {
      qualtRics::read_survey(filename)
    } else {
      readr::read_csv(filename, skip = skips, show_col_types = FALSE)
    }
    
    return(dplyr::lst(!!csv_name := clean_data(df)))
  }

  
  # Excel files reader
  read_and_clean_excel <- function(filename) {
    base_name <- naming(filename)
    sheets <- stringr::str_subset(readxl::excel_sheets(filename), "_mp_collection|_mp_datasets", negate = TRUE)
    
    data_list <- purrr::map(sheets, function(the_sheet) {
      df <- readxl::read_excel(filename, sheet = the_sheet, guess_max = 1048576, skip = skips, col_types = col_types)
      clean_data(df)
    })
    
    names(data_list) <- stringr::str_c(prefix, if(length(sheets) > 1) naming(sheets), base_name, sep = "_")
    data_list
  }
  
  # Read and combine all files
  csv_data <- purrr::list_flatten(purrr::map(csv_files, read_and_clean_csv))
  excel_data <- purrr::list_flatten(purrr::map(excel_files, read_and_clean_excel))
  
  output <- append(excel_data, csv_data)
  
  output <- combiner(output)
  
  if(length(output) == 0) {
    stop("No data returned from reading the files. Please check your file paths and contents.")
  }
  
  return(output)
}

