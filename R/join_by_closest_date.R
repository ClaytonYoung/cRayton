#' Join a list of dataframes to timepoints based on the closest date.
#'
#' This function takes a primary dataframe with patient IDs and timepoints, and joins it with a list of 
#' dataframes on the closest date. After joining, duplicates are removed and the resulting dataframes are
#' consolidated into one tibble.
#'
#' @param pidns_and_timepoints A dataframe with PIDN (patient IDs) and DCDate (date/time points).
#' @param list_of_dfs List of dataframes that need to be joined with `pidns_and_timepoints`.
#' @param max_day_difference Maximum day difference allowed for the join on date.
#' @param remove_empty_option Logical, if TRUE, will remove empty rows and columns from the final tibble.
#'
#' @return A consolidated tibble after joining on the closest date.
#' @export
join_list_of_dfs_to_timepoints <- function(pidns_and_timepoints, list_of_dfs, max_day_difference, remove_empty_option = TRUE) {
  # DFs require pidn and dcdate cols
  necessary_cols <- c("PIDN", "DCDate")
  # Initiate progressor (bar showing progress of fxn)
  p <- progressr::progressor(steps = length(list_of_dfs))
  
  # 1. Pre-processing checks 
  
  validate_inputs <- function() {
    message(paste('Validating inputs'))
    
    if (!all(necessary_cols %in% names(pidns_and_timepoints))) {
      stop(paste("The pidns_and_timepoints dataframe must have", paste(necessary_cols, collapse = " and "), "columns."))
    }
    
    if (!is.numeric(max_day_difference) || max_day_difference <= 0) {
      stop("max_day_difference must be a positive number.")
    }
    
    if (!is.logical(remove_empty_option)) {
      stop("remove_empty_option must be a logical value.")
    }
    
    if (any(duplicated(pidns_and_timepoints[, necessary_cols]))) {
      stop("There are duplicated", paste(necessary_cols, collapse = " and "), "combinations in the pidns_and_timepoints dataframe.")
    }
  }
  
  validate_inputs()
  
  # 2. Define Helper Functions
  check_duplicated_data <- function(dataframe_list) {
    message('Checking for duplicated data')
    
    has_pidn_and_date <- function(df) {
      all(necessary_cols %in% names(df))
    }
    
    only_has_pidn <- function(df) {
      "PIDN" %in% names(df) && !"DCDate" %in% names(df)
    }
    
    # Check for duplicated PIDNs with and without DCDate
    duplicated_pidn_date <- dataframe_list %>%
      purrr::keep(has_pidn_and_date) %>%
      purrr::map(~ dplyr::group_by(.x, PIDN, DCDate) %>% dplyr::filter(dplyr::n() > 1)) %>%
      purrr::discard(~ nrow(.x) == 0)
    
    duplicated_pidns <- dataframe_list %>%
      purrr::keep(only_has_pidn) %>%
      purrr::map(~ dplyr::group_by(.x, PIDN) %>% dplyr::filter(dplyr::n() > 1)) %>%
      purrr::discard(~ nrow(.x) == 0)
    
    # Communicate duplicated data to user
    if (length(duplicated_pidn_date) >= 1 || length(duplicated_pidns) >= 1) {
      message('The following data frames have issues:')
      
      if (length(duplicated_pidn_date) >= 1) {
        message('Duplicated DCDates for one or more PIDNs:')
        purrr::walk(names(duplicated_pidn_date), message)
      }
      
      if (length(duplicated_pidns) >= 1) {
        message('Duplicated PIDNs without DCDates:')
        purrr::walk(names(duplicated_pidns), message)
      }
      
      continue <- readline(prompt = 'Do you want to continue? (Y/N) ')
      if (toupper(continue) == 'N') {
        stop('Function terminated.')
      }
    } else {
      message('No data frames contain duplicated data.')
    }
    
    # Clean dataframes
    cleaned_dataframes <- dataframe_list %>%
      purrr::keep(has_pidn_and_date) %>%
      purrr::map(~ .x %>%
                   dplyr::mutate(PIDN = as.double(PIDN)) %>%
                   dplyr::ungroup())
    
    return(cleaned_dataframes)
  }
  
  
  process_and_join_dataframes <- function(cleaned_dataframes) {
    message('Joining on closest date and removing duplicated dates')
    
    # Function to handle joining closest DCDate and processing
    process_dataframe <- function(df, df_suffix, date_suffix) {
      max_diff <- ifelse(!is.null(attr(df, "max_day_diff")), 
                         attr(df, "max_day_diff"), 
                         max_day_difference)
      
      df %>%
        dplyr::left_join(pidns_and_timepoints, df, by = "PIDN", suffix = c(df_suffix, ""), keep = FALSE, relationship = "many-to-many") %>%
        dplyr::mutate(date_diff = abs(lubridate::as_date(DCDate) - lubridate::as_date(dplyr::pull(., dplyr::all_of(date_suffix))))) %>%  
        dplyr::group_by(dplyr::across(c(PIDN, tidyr::all_of(date_suffix)))) %>%
        dplyr::filter((date_diff == min(date_diff, na.rm = FALSE)) &
                        (date_diff <= max_diff)) %>%
        dplyr::relocate(DCDate, date_diff, tidyr::all_of(date_suffix)) %>%
        dplyr::ungroup()
      
    }
    # Map process_dataframe to each dataframe in cleaned_dfs
    purrr::pmap(list(cleaned_dataframes, df_suffixes, date_suffixes), function(df, df_suffix, date_suffix) {
      processed_df <- process_dataframe(df, df_suffix, date_suffix)
      p()  # Increment progress after processing each dataframe
      return(processed_df)
    })    
  }
  
  cleaned_dfs <- check_duplicated_data(list_of_dfs)
  # get names for dfs
  df_suffixes <- stringr::str_c('.', names(cleaned_dfs))
  date_suffixes <- stringr::str_c('DCDate', df_suffixes)
  
  joined_dfs <- process_and_join_dataframes(cleaned_dfs)
  
  # Removing duplicated timepoints
  message('Removing duplicated timepoints')
  dfs_without_duplicates <- purrr::map(joined_dfs, function(df) {
    p()
    df %>%
      dplyr::group_by(PIDN, DCDate) %>%
      dplyr::filter(if (length(date_diff) > 0) date_diff == min(date_diff, na.rm = TRUE) else FALSE) %>% 
      dplyr::arrange(PIDN, DCDate, rowSums(is.na(.))) %>%
      dplyr::distinct(PIDN, DCDate, date_diff, .keep_all = TRUE)
    
  })
  
  # Injecting missing timepoints back into each dataframe
  message('Joining cleaned dataframes to timepoints')
  dfs_with_all_timepoints <- purrr::map(dfs_without_duplicates, function(df) {
    p()
    dplyr::left_join(pidns_and_timepoints, df, by = c('PIDN','DCDate'), relationship = "many-to-many")
  })
  
  # Renaming variables in each dataframe to include the source
  message('Renaming columns to include data source identifier')
  
  suffixed_dfs <- purrr::pmap(list(dfs_with_all_timepoints, df_suffixes, date_suffixes),
                              function(df, df_suffix, date_suffix) {
                                p()
                                dplyr::rename_with(df, ~stringr::str_c(., df_suffix), -c(PIDN, DCDate, tidyr::all_of(date_suffix)))
                              }) 
  
  # Handling empty data
  final_dfs <- suffixed_dfs %>%
    purrr::map(~ dplyr::group_by(.x, PIDN, DCDate)) %>%
    purrr::map(~ {
      p()
      if (remove_empty_option) {
        .x %>% janitor::remove_empty(c('cols', 'rows'))
      } else {
        .x
      }
    }) %>%
    
    purrr::map(~ dplyr::ungroup(.x))
  
  # Consolidating all dataframes into a single tibble
  message('Consolidating to single tibble... this may take some time.')
  resulting_dataframe <- purrr::reduce(final_dfs, 
                                       dplyr::left_join, by = c('PIDN', 'DCDate'))
  
  return(resulting_dataframe)
}
