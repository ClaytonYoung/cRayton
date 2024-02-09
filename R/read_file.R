# Read file function that asks user if they want to read in file--interactively lets user dplyr::select file to read in
# Use read_data within read_file
read_file <- function() {
  read_file <- base::readline(prompt = "Do you want to read a file? (y/n): ")
  if (base::tolower(read_file) == "y") {
    filename <- base::file.choose()
    data_object <- read_data(filename)
    attr(data_object, 'name')  <- base::readline(prompt = "Enter the name for the new object: ")

    # Add return statement to return the data object
    return(data_object)
  }
}
