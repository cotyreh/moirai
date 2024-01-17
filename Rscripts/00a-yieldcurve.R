library(readr)
library(data.table)
library(zoo)

# Pure function to convert rate column names to days
convert_to_days <- function(rate_column) {
  time_values <- strsplit(rate_column, " ")[[1]]
  time_number <- as.numeric(time_values[1])
  time_unit <- time_values[2]
  
  switch(time_unit,
         "Mo" = time_number * 30,
         "Yr" = time_number * 365,
         stop("Unknown time unit"))
}

# Pure function to fetch and process yield data
fetch_yield_curve_function <- function(url) {
  yield_data <- as.data.table(read_csv(url))
  rate_columns <- names(yield_data)[2:length(names(yield_data))]
  maturities <- sapply(rate_columns, convert_to_days)
  rates <-
    as.numeric(unlist(yield_data[1, 2:length(yield_data)])) / 100
  splinefun(maturities, rates, method = "natural")
}

# Function to execute yield curve processing and save the result
execute_yield_curve_processing <- function() {
  url <- "https://shorturl.at/exGJ5"
  yield_curve_function <- fetch_yield_curve_function(url)
  
  # Directory where the RDS file will be saved
  dir_path <- "moirai/data"
  
  # Check if directory exists, if not, create it
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Save the yield curve function as an RDS file in the specified directory
  saveRDS(yield_curve_function, file.path(dir_path, "yield_curve_function.rds"))
}

# Run the process
execute_yield_curve_processing()