library(quantmod)

# Function to extract strike and open interest data
extract_strike_OI <-
  function(option_type_data,
           expiry_date,
           option_type) {
    if (!is.null(option_type_data) && nrow(option_type_data) > 0) {
      option_type_data$LastTradeTime <-
        as.POSIXct(option_type_data$LastTradeTime,
                   format = "%Y-%m-%d %H:%M:%S",
                   tz = "America/Chicago")
      
      expiry_date_converted <-
        as.Date(expiry_date, format = "%b.%d.%Y")
      
      return(
        data.frame(
          Expiry = expiry_date_converted,
          Strike = option_type_data$Strike,
          OI = option_type_data$OI,
          Last = option_type_data$Last,
          LastTradeTime = option_type_data$LastTradeTime,
          Type = option_type
        )
      )
    } else {
      return(
        data.frame(
          Expiry = as.Date(character()),
          Strike = numeric(),
          OI = numeric(),
          Last = numeric(),
          LastTradeTime = POSIXct(),
          Type = character()
        )
      )
    }
  }

# Function to combine data frames efficiently
combine_data_frames_efficient <- function(strike_OI_list) {
  do.call(rbind, strike_OI_list)
}

# Refactored function to process option chain data
process_option_chain <- function(option_chain) {
  process_data <- function(data_type, expiry_dates) {
    data <- lapply(expiry_dates, function(date)
      list(
        data = option_chain[[date]][[data_type]],
        date = date,
        type = data_type
      ))
    data <-
      Filter(function(d)
        ! is.null(d$data) && nrow(d$data) > 0, data)
    lapply(data, function(d)
      extract_strike_OI(d$data, d$date, d$type))
  }
  
  expiry_dates <- names(option_chain)
  call_strike_OI_list <- process_data("calls", expiry_dates)
  put_strike_OI_list <- process_data("puts", expiry_dates)
  
  list(calls = combine_data_frames_efficient(call_strike_OI_list),
       puts = combine_data_frames_efficient(put_strike_OI_list))
}

# Function to retrieve option chain data
get_option_chain_data <- function(symbol) {
  getOptionChain(symbol, NULL)
}

# Function to save data frames as CSV in specified directory
save_data_frames <-
  function(all_call_strike_OI, all_put_strike_OI, stable_dir_path) {
    # Ensure the directory exists
    if (!dir.exists(stable_dir_path)) {
      dir.create(stable_dir_path, recursive = TRUE)
    }
    
    calls_csv_stable_path <- file.path(stable_dir_path, "calls.csv")
    puts_csv_stable_path <- file.path(stable_dir_path, "puts.csv")
    
    write.csv(all_call_strike_OI, calls_csv_stable_path, row.names = FALSE)
    write.csv(all_put_strike_OI, puts_csv_stable_path, row.names = FALSE)
  }

# Main function to orchestrate the workflow
main <- function(symbol, stable_dir_path) {
  option_chain_data <- get_option_chain_data(symbol)
  processed_data <- process_option_chain(option_chain_data)
  
  save_data_frames(processed_data$calls, processed_data$puts, stable_dir_path)
}

# Call the main function
main("^SPX", "moirai/data/opt")
