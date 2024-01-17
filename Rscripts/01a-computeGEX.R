library(quantmod)
library(dplyr)
library(pracma)

# Load the yield curve function
yield_curve_function <- readRDS("moirai/data/yield_curve_function.rds")

# Function to fetch market data
fetch_market_data <- function() {
  SPX <- getQuote("^GSPC")
  VIX <- getQuote("^VIX")
  ES <- getQuote("ES=F")
  list(
    spot_price = as.numeric(SPX$Last),
    reference_time = as.POSIXct(SPX$'Trade Time', tz = "America/Chicago"),
    risk_free_rate = 0,
    volatility = as.numeric(VIX$Last) / 100,
    future = ES$Last
  )
}

# Function to fetch option data
fetch_option_data <- function() {
  list(
    calls_data = read.csv("moirai/data/opt/calls.csv"),
    puts_data = read.csv("moirai/data/opt/puts.csv")
  )
}

# Existing functions: Phi, black_scholes, implied_volatility, time_to_maturity, calculate_T, d1, gamma_BS
# Cumulative normal distribution function
Phi <- function(x) {
  0.5 * (1 + erf(x / sqrt(2)))
}

# Black-Scholes formula
black_scholes <- function(S, K, T, r, sigma, type) {
  d1 <- (log(S / K) + (r + sigma ^ 2 / 2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  if (type == "call") {
    return(S * Phi(d1) - K * exp(-r * T) * Phi(d2))
  } else if (type == "put") {
    return(K * exp(-r * T) * Phi(-d2) - S * Phi(-d1))
  }
}

implied_volatility <-
  function(market_price,
           S,
           K,
           T,
           r,
           sigma_initial,
           type) {
    # Ensure sigma_initial is numeric
    sigma <-
      as.numeric(sigma_initial)  # Convert to numeric if not already
    
    tol <- 1e-5   # tolerance level
    max_iter <- 1000  # maximum iterations
    min_vega <- 1e-5  # minimum vega
    
    for (i in 1:max_iter) {
      price <- black_scholes(S, K, T, r, sigma, type)
      vega <-
        S * sqrt(T) * dnorm((log(S / K) + (r + sigma ^ 2 / 2) * T) / (sigma * sqrt(T)))
      
      if (vega < min_vega) {
        return(NA)  # Vega too small, return NA to indicate failure
      }
      
      price_diff <- market_price - price
      if (is.nan(price_diff) || is.infinite(price_diff)) {
        return(NA)  # Price difference is NaN or infinite, return NA
      }
      
      if (abs(price_diff) < tol) {
        return(sigma)  # Convergence achieved
      }
      
      sigma <- sigma + price_diff / vega
      sigma <-
        max(min(sigma, 3), 0.001)  # Ensure sigma stays within bounds
    }
    
    return(NA)  # Return NA if no convergence after max iterations
  }



time_to_maturity <- function(expiry_date, current_datetime) {
  expiry_datetime <- as.POSIXct(paste(expiry_date, "15:00:00"),
                                format = "%Y-%m-%d %H:%M:%S",
                                tz = "America/Chicago")
  
  TTM <-
    as.numeric(difftime(expiry_datetime, current_datetime, units = "mins")) /
    (365 * 24 * 60)
  
  if (TTM < 0)
    TTM <- 0
  
  return(TTM)
}

calculate_T <- function(expiry_date, reference_time) {
  expiry_datetime <-
    as.POSIXct(paste(expiry_date, "15:00:00"), tz = "America/Chicago")
  diff <-
    as.numeric(difftime(expiry_datetime, reference_time, units = "mins"))
  T <- diff / (365 * 24 * 60)  # Convert minutes to years
  T <- pmax(T, 1e-8)  # Ensure T is at least a small positive value
  return(T)
}

d1 <- function(S, K, T, r, IV) {
  (log(S / K) + (r + (IV ^ 2) / 2) * T) / (IV * sqrt(T))
}

gamma_BS <- function(S, K, T, r, IV) {
  d1_val <- d1(S, K, T, r, IV)
  return(dnorm(d1_val) / (S * IV * sqrt(T)))
}


# Function to process options
process_options <- function(options_data, type, market_data) {
  options_data %>%
    mutate(
      TTM = mapply(
        time_to_maturity,
        Expiry,
        rep(market_data$reference_time, nrow(options_data))
      ),
      risk_free_rate = sapply(TTM, function(t)
        yield_curve_function(t * 365)),
      # Convert TTM to days and use yield curve function
      IV = mapply(
        implied_volatility,
        Last,
        rep(market_data$spot_price, nrow(options_data)),
        Strike,
        TTM,
        risk_free_rate,
        # Use the calculated risk-free rate
        rep(market_data$volatility, nrow(options_data)),
        # Ensure sigma_initial is passed from market_data
        rep(type, nrow(options_data)) # Pass 'type' correctly to each implied volatility calculation
      )
    ) %>%
    mutate(IV = ifelse(is.na(IV) & TTM == 0, 0, IV)) %>%
    group_by(Expiry) %>%
    mutate(IV = na.spline(IV, method = "natural")) %>%
    ungroup() %>%
    mutate(IV = ifelse(IV < 0, 0.0001, IV)) # Replace negative IV values with 0.0001
}

# Function to apply gamma BS
apply_gamma_BS <- function(data, market_data) {
  data %>%
    mutate(Gamma = mapply(function(Expiry, Strike, OI, IV) {
      T_value <- calculate_T(Expiry, market_data$reference_time)
      gamma_BS(
        S = market_data$spot_price,
        K = Strike,
        T = T_value,
        r = market_data$risk_free_rate,
        IV = IV
      ) * OI * 100
    }, Expiry, Strike, OI, IV))
}

# Function to calculate and save GEX
calculate_and_save_GEX <-
  function(processed_calls, processed_puts, market_data) {
    total_call_gamma <- sum(processed_calls$Gamma, na.rm = TRUE)
    total_put_gamma <- sum(processed_puts$Gamma, na.rm = TRUE)
    
    GEX <-
      total_call_gamma * market_data$spot_price - total_put_gamma * market_data$spot_price
    formatted_date <- format(Sys.Date(), "%d%m%Y")
    dir_path <- "moirai/data/intraday"
    filename <-
      paste0(dir_path, "/intradayGEX", formatted_date, ".csv")
    snapshot_data <-
      data.frame(
        ReferenceTime = market_data$reference_time,
        GEX = GEX,
        spot = market_data$spot_price,
        future = market_data$future
      )
    
    # Check if directory exists, if not, create it
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
    
    # Write the file
    if (file.exists(filename)) {
      write.table(
        snapshot_data,
        file = filename,
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,
        append = TRUE
      )
    } else {
      write.table(
        snapshot_data,
        file = filename,
        sep = ",",
        row.names = FALSE,
        col.names = TRUE,
        append = FALSE
      )
    }
  }

# Function to save processed data to CSV
save_processed_data <- function(data, filename) {
  write.csv(data, file = filename, row.names = FALSE)
}

# Main execution function
main <- function() {
  # Fetching market data and option data
  market_data <- fetch_market_data()
  option_data <- fetch_option_data()
  
  # Processing calls and puts data
  processed_calls <-
    process_options(option_data$calls_data, "call", market_data)
  processed_puts <-
    process_options(option_data$puts_data, "put", market_data)
  
  # Applying gamma BS calculation
  processed_calls_with_gamma <-
    apply_gamma_BS(processed_calls, market_data)
  processed_puts_with_gamma <-
    apply_gamma_BS(processed_puts, market_data)
  
  # Calculate and save GEX
  calculate_and_save_GEX(processed_calls_with_gamma,
                         processed_puts_with_gamma,
                         market_data)
}

# Run the script
main()
