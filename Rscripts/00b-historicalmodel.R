library(readr)
library(data.table)

# Function to read CSV from URL and convert to data table
read_csv_as_data_table <- function(url) {
  as.data.table(read_csv(url))
}

# Function to check and update file if row count changes
update_file_if_needed <-
  function(new_data, file_path, gexvol_path) {
    if (!file.exists(file_path)) {
      write_file(new_data, file_path)
      message("File created for the first time.")
      return(invisible(NULL))
    }
    
    existing_data <- fread(file_path)
    if (nrow(new_data) != nrow(existing_data)) {
      message("Number of rows changed, updating the file.")
      write_file(new_data, file_path)
      update_gexvol(new_data, gexvol_path)
    } else {
      message("No update necessary.")
    }
  }

# Function to write data to a file
write_file <- function(data, file_path) {
  fwrite(data, file = file_path)
}

# Function to update gexvol.csv
update_gexvol <- function(new_data, gexvol_path) {
  latest_data <- new_data[1, .(DATE, GEX, SPX)]
  gexvol_data <- fread(gexvol_path)
  
  if (nrow(gexvol_data) > 0) {
    append_new_row_to_gexvol(gexvol_data, latest_data, gexvol_path)
  } else {
    message("gexvol.csv is empty. No update made.")
  }
}

# Function to append a new row to gexvol.csv
append_new_row_to_gexvol <-
  function(gexvol_data, latest_data, gexvol_path) {
    latest_SPX <- latest_data$SPX
    previous_SPX <- gexvol_data[nrow(gexvol_data), SPX]
    Return <- (latest_SPX - previous_SPX) / previous_SPX
    Magnitude <- abs(Return)
    
    # Round GEX to 2 decimal places, Return and Magnitude to 8 decimal places
    GEX_rounded <- round(latest_data$GEX, 2)
    Return_rounded <- round(Return, 8)
    Magnitude_rounded <- round(Magnitude, 8)
    
    new_row <-
      data.table(
        Date = latest_data$DATE,
        GEX = latest_data$GEX,
        SPX = latest_SPX,
        Return = Return,
        Magnitude = Magnitude
      )
    fwrite(rbindlist(list(gexvol_data, new_row)), file = gexvol_path)
  }

# Main function to orchestrate the script
main <- function() {
  # URL of the CSV file
  url <- "https://t.co/Esw2v7XQii"
  
  # Paths to the files
  file_path <- "moirai/data/historical/squeeze.csv"
  gexvol_path <- "moirai/data/historical/gexvol.csv"
  
  # Reading data and updating files
  GEX_data <- read_csv_as_data_table(url)
  update_file_if_needed(GEX_data, file_path, gexvol_path)
}

# Call the main function to execute the script
main()