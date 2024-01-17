library(quantmod)
library(readr)
library(data.table)
library(zoo)
library(lubridate)

# Load the yield curve function from the .rds file
yield_curve_function <- readRDS("moirai/data/yield_curve_function.rds")

# Function to fetch market data
fetch_market_data <- function() {
  ES <- getQuote("ES=F")
  SPX <- getQuote("^SPX")
  SDA <- getQuote("SDA=F")
  
  list(ES = ES, SPX = SPX, SDA = SDA)
}

# Function to process data
process_data <- function(market_data, days_to_maturity) {
  future <- market_data$ES$Last
  spot <- market_data$SPX$Last
  riskfree <-
    yield_curve_function(days_to_maturity)  # Use the loaded function
  dividend <- market_data$SDA$Last * (days_to_maturity / 365)
  date <- Sys.Date()
  
  data.frame(
    Date = date,
    Future = future,
    Spot = spot,
    Riskfree = riskfree,
    Maturity = days_to_maturity,
    Dividend = dividend
  )
}

write_data_to_file <- function(processed_data, file_path) {
  # Extract directory from file_path
  dir_path <- dirname(file_path)
  
  # Check if directory exists, if not, create it
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Now proceed with writing the file
  if (file.exists(file_path)) {
    write.table(
      processed_data,
      file = file_path,
      row.names = FALSE,
      col.names = FALSE,
      sep = ",",
      append = TRUE
    )
  } else {
    write.table(
      processed_data,
      file = file_path,
      row.names = FALSE,
      col.names = TRUE,
      sep = ","
    )
  }
}

# Main function to orchestrate the flow
main <- function() {
  market_data <- fetch_market_data()
  days_to_maturity <-
    as.numeric(difftime(as.Date("2024-03-15"), Sys.Date(), units = "days"))
  processed_data <- process_data(market_data, days_to_maturity)
  write_data_to_file(processed_data,
                     "moirai/data/historical/initialprint.csv")
}

# Run the main function
main()
