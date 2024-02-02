#' Coalesce Join Two Data Frames
#'
#' This function performs a full join between two data frames and then coalesces columns with the same name.
#' It prioritizes the left-hand side data frame in the case of NA values.
#'
#' @param x The first data frame.
#' @param y The second data frame.
#' @param by A vector of variables to join by. If NULL, the default, joins by common key variables.
#' @param suffix A character vector of length 2 to add to the names of the columns in `x` and `y`.
#' @param join The join function to use, by default `dplyr::full_join`.
#' @param ... Additional arguments to pass to the `join` function.
#'
#' @return A joined data frame with coalesced columns.
#'
#' @importFrom dplyr full_join
#' @importFrom dplyr union
#' @importFrom dplyr bind_cols
#' @importFrom dplyr coalesce
#' @importFrom purrr map_dfc
#'
#' @examples
#' df1 <- data.frame(a = 1:3, b = c(NA, NA, 3))
#' df2 <- data.frame(a = 2:4, b = 2:4)
#' coalesce_join(df1, df2)
#'
#' @export
coalesce_join <- function(x, y,
                          by = NULL, suffix = c(".x", ".y"),
                          join = dplyr::full_join, ...) {
  # Perform the join
  joined <- join(x, y, by = by, suffix = suffix, ...)

  # Get the names of desired output
  cols <- dplyr::union(names(x), names(y))

  # Identify columns with the same name from both data frames
  to_coalesce <- names(joined)[!names(joined) %in% cols]

  if (length(to_coalesce) > 0) {
    # Determine which suffix was used
    suffix_used <- suffix[ifelse(grepl(suffix[1], to_coalesce), 1, 2)]

    # Remove suffixes and deduplicate names
    to_coalesce <- unique(substr(
      to_coalesce,
      1,
      nchar(to_coalesce) - nchar(suffix_used)
    ))

    # Coalesce matching columns
    coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
      joined[[paste0(.x, suffix[1])]],
      joined[[paste0(.x, suffix[2])]]
    ))

    # Set column names
    names(coalesced) <- to_coalesce

    # Combine coalesced columns with the original joined data frame
    return(dplyr::bind_cols(joined, coalesced)[cols])
  } else {
    return(joined)
  }
}
