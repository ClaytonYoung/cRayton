#' Combiner Function for Merging Duplicate Data in List of Data Frames
#'
#' This function detects duplicate data in a list of data frames and merges them.
#'
#' @param dfs A named list of data frames.
#' 
#' @return A named list of data frames with duplicates combined and distinct rows.
#' 
#' @importFrom purrr map set_names
#' @importFrom dplyr bind_rows distinct
#'
#' @export
combiner <- function(dfs) {
  # Check if names of data frames are duplicated
  if (any(duplicated(names(dfs)))) {
    # Get the names of duplicated data frames
    duplicated_names <- unique(names(dfs)[duplicated(names(dfs))])
    
    # Select data frames with duplicated names and bind them together
    dupes <- purrr::map(duplicated_names, function(x) {
      dplyr::bind_rows(dfs[names(dfs) == x])
    }) 
    # Set the names of the merged data frames to the duplicated names
    names(dupes) <- duplicated_names
    
    # Filter out duplicated data frames in the original list
    original <- dfs[!names(dfs) %in% duplicated_names]
    
    # Append the combined duplicated data frames back into the list of data frames
    dfs <- c(original, dupes)
  }
  
  dfs <- purrr::map(dfs, dplyr::distinct)
  
  return(dfs)
}
