# cRayton

## Overview
**cRayton** is an R package that provides a collection of functions designed to simplify data wrangling and formatting. The package includes utilities for joining data frames based on timepoints, reading multiple file formats, merging and cleaning data, and more. Use cases might be specific to UCSF's Memory and Aging Center. 

## Installation

To install **cRayton**, use:

```{r}
# install locally from source
install.packages("cRayton")
```

## Features

### `join_list_of_dfs_to_timepoints`
Joins a primary dataframe containing patient IDs (PIDN) and timepoints (dates) to a list of dataframes based on the closest date within a specified maximum day difference (`max_day_difference`). After joining, duplicates are removed, and the resulting dataframes are consolidated into a single tibble.

### `read_all_files`
Reads multiple files (CSV and Excel), cleans the column names, and stores the dataframes in an array.

### `read_data`
A file-type agnostic function for importing various file formats, including CSV, TXT, RDS, and XLSX.

### `read_file`
An interactive file importing function that utilizes `read_data` and allows interactive object naming.

### `coalesce_join`
Joins two data frames while coalescing matching columns. In the case of `NA` values, the function prioritizes the left-hand side data frame.

### `combiner`
Removes duplicate dataframes from a list and merges dataframes with the same name together.

## Usage

```r
library(cRayton)

# Example: Using join_list_of_dfs_to_timepoints
result <- join_list_of_dfs_to_timepoints(primary_df, list_of_dfs, max_day_difference = 7)

# Example: Reading all files in a directory
files_data <- read_all_files("path/to/directory")
```

## Contributing
Contributions are welcome! Feel free to submit issues or pull requests.

## License
This project is licensed under the MIT License.


