library(quantmod)
library(ggplot2)
library(ggthemes)
library(ggimage)
library(lubridate)
library(magick)
library(dplyr)

load_data <- function() {
  gexvol <- read.csv("moirai/data/historical/gexvol.csv")
  initialprint <- read.csv("moirai/data/historical/initialprint.csv")
  
  # Get current date in ddmmyyyy format
  current_date <- format(Sys.Date(), "%d%m%Y")
  
  # Construct file name with the current date
  file_name <- paste0("moirai/data/intraday/intradayGEX", current_date, ".csv")
  
  # Read the file
  real_prices <- read.csv(file_name)
  realized_prices <- real_prices$spot
  
  list(
    gexvol = gexvol,
    initialprint = initialprint,
    real_prices = real_prices,
    realized_prices = realized_prices
  )
}

# Function to set simulation parameters
set_simulation_parameters <- function() {
  time_steps <- 391
  num_paths <- 1000
  set.seed(12012024)
  list(time_steps = time_steps, num_paths = num_paths)
}

# Other function definitions
calculate_implied_open <-
  function(future,
           spot,
           maturity,
           riskfree_rate,
           dividends) {
    FV <- spot * (1 + riskfree_rate * (maturity / 365)) - dividends
    expected_gap <- future - FV
    spot + expected_gap
  }

# Curried calculate_mu function
calculate_mu <- function(maturity, time_steps) {
  function(future, spot) {
    implied_rate <- (future / spot) ^ (1 / (maturity / 365)) - 1
    mu <- implied_rate / maturity / time_steps
    mu
  }
}

calculate_sigma <- function(gamma_exposure, gexvol) {
  nGEX <-
    (gamma_exposure - min(gexvol$GEX)) / (max(gexvol$GEX) - min(gexvol$GEX))
  modelGEX <- sqrt(pi / 2) * exp(-2 * pi * nGEX)
  (modelGEX * max(gexvol$Magnitude)) / sqrt(1440)
}

simulate_price_paths <-
  function(open, mu, sigma, num_paths, time_steps) {
    price_paths <- matrix(0, nrow = time_steps, ncol = num_paths)
    price_paths[1, ] <- open
    for (path in 1:num_paths) {
      for (t in 2:time_steps) {
        price_paths[t, path] <- price_paths[t - 1, path] +
          mu * price_paths[t - 1, path] +
          rnorm(1, 0, 1) * sigma * price_paths[t - 1, path]
      }
    }
    price_paths
  }

# Continuing from generate_snapshot
generate_snapshot <-
  function(time_step,
           snapshot_folder,
           price_df,
           real_prices_df,
           time_labels,
           gexvol,
           initialprint,
           real_prices,
           time_steps,
           num_paths) {
    maturity <- tail(initialprint$Maturity, n = 1)
    mu_function <- calculate_mu(maturity, time_steps)
    market_mu <-
      mu_function(real_prices$future[time_step], real_prices$spot[time_step])
    market_sigma <-
      calculate_sigma(real_prices$GEX[time_step], gexvol)
    
    intraday_price_paths <-
      matrix(NA, nrow = time_steps, ncol = num_paths)
    intraday_price_paths[time_step, ] <-
      real_prices$spot[time_step]
    for (path in 1:num_paths) {
      for (t in (time_step + 1):time_steps) {
        intraday_price_paths[t, path] <- intraday_price_paths[t - 1, path] +
          market_mu * intraday_price_paths[t - 1, path] +
          rnorm(1, 0, 1) * market_sigma * intraday_price_paths[t - 1, path]
      }
    }
    intraday_df <- data.frame(
      time = rep(time_labels, num_paths),
      path = factor(rep(1:num_paths, each = time_steps)),
      price = as.vector(intraday_price_paths),
      type = "Intraday Forecast"
    )
    intraday_df <- intraday_df[!is.na(intraday_df$price), ]
    
    # Calculate the previous day's close
    previous_day_close <- tail(gexvol$SPX, n = 1)
    
    # Sort paths based on the last price
    sorted_paths <- price_df[price_df$type == "Forecast", ] %>%
      group_by(path) %>%
      summarize(last_price = last(price)) %>%
      arrange(last_price)
    
    # Select the bottom 10 and top 10 paths
    bottom_10_paths <- sorted_paths$path[1:10]
    top_10_paths <- sorted_paths$path[(num_paths-9):num_paths]
    
    # Calculate the average for each time step for bottom and top 10 paths
    bottom_10_avg <- price_df %>% 
      filter(path %in% bottom_10_paths) %>% 
      group_by(time) %>% 
      summarize(avg_price = mean(price))
    
    top_10_avg <- price_df %>% 
      filter(path %in% top_10_paths) %>% 
      group_by(time) %>% 
      summarize(avg_price = mean(price))
    
    # Calculate the min and max prices from price_df
    min_price <- min(price_df$price) * 0.999
    max_price <- max(price_df$price) * 1.001
    
    # Get the system date in "dd mmm" format
    current_date <- format(Sys.Date(), "%d %b")
    
    # Get the most recent price at the specified time_step
    recent_price <- real_prices_df$price[time_step]
    
    # Construct the title with the current date, recent price, and time
    title <- paste(current_date, "|", recent_price, "as of", time_labels[time_step])
    
    p <- ggplot() +
      theme_wsj(base_size = 5) +
      ggtitle(title) +
      geom_line(
        data = price_df,
        aes(x = time, y = price, group = path),
        color = "#AAAAAA",
        alpha = 0.125
      ) +
      geom_line(
        data = bottom_10_avg,
        aes(x = time, y = avg_price),
        color = "#aa77cc",
        linetype = "dotdash",
        group = 1
      ) + geom_line(
        data = top_10_avg,
        aes(x = time, y = avg_price),
        color = "#aa77cc",
        linetype = "dotdash",
        group = 1
      ) +
      geom_line(data = real_prices_df[1:time_step, ],
                aes(x = time, y = price, group = path),
                color = "#aa3333") +
      geom_line(
        data = intraday_df,
        aes(x = time, y = price, group = path),
        color = "#aa3333",
        alpha = 0.089
      ) +
      geom_hline(yintercept = previous_day_close, linetype = "dotted") +
      scale_x_discrete(
        name = "",
        breaks = c("08:30", "10:00", "11:30", "13:00", "14:30"),
        labels = c("8:30 AM", "10:00 AM", "11:30 AM", "1:00 PM", "2:30 PM")
      ) +
      ylim(min_price, max_price)
    pp <- ggbackground(p, "moirai/www/paper.JPG")
    
    # Define the file path
    file_path <- "moirai/data/snapshot"
    
    # Create the path if it doesn't exist
    if (!dir.exists(file_path)) {
      dir.create(file_path, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Define the file name
    file_name <- file.path(file_path, "frame.png")
    ggsave(
      file_name,
      plot = pp,
      width = 19,
      height = 11,
      units = "cm"
    )
  }

# Function to perform main simulation
perform_simulation <- function(data, params) {
  open <- calculate_implied_open(
    tail(data$initialprint$Future, n = 1),
    tail(data$initialprint$Spot, n = 1),
    tail(data$initialprint$Maturity, n = 1),
    tail(data$initialprint$Riskfree, n = 1),
    tail(data$initialprint$Dividend, n = 1)
  )
  mu_function <-
    calculate_mu(tail(data$initialprint$Maturity, n = 1), params$time_steps)
  mu <- mu_function(tail(data$initialprint$Future, n = 1),
                    tail(data$initialprint$Spot, n = 1))
  sigma <-
    calculate_sigma(tail(data$gexvol$GEX, n = 1), data$gexvol)
  price_paths <-
    simulate_price_paths(open, mu, sigma, params$num_paths, params$time_steps)
  
  list(price_paths = price_paths, mu_function = mu_function)
}

# Function to prepare data frames
prepare_data_frames <-
  function(data, price_paths, time_steps, num_paths) {
    time_labels <- format(seq(
      from = as.POSIXct("08:30:00", format = "%H:%M:%S"),
      to = as.POSIXct("15:00:00", format = "%H:%M:%S"),
      length.out = time_steps
    ),
    format = "%H:%M")
    
    price_df <- data.frame(
      time = rep(1:time_steps, num_paths),
      path = factor(rep(1:num_paths, each = time_steps)),
      price = as.vector(price_paths),
      type = "Forecast"
    )
    price_df$time <- rep(time_labels, num_paths)
    
    real_prices_df <- data.frame(
      time = seq(
        1,
        by = 1,
        length.out = length(data$realized_prices)
      ),
      path = factor(rep((num_paths + 1), length(data$realized_prices)
      )),
      price = data$realized_prices,
      type = "Actual"
    )
    
    list(
      price_df = price_df,
      real_prices_df = real_prices_df,
      time_labels = time_labels
    )
  }

# Modified main function to only generate a single snapshot
run_simulation <- function() {
  # Load data and set parameters
  data <- load_data()
  params <- set_simulation_parameters()
  
  # Perform main simulation
  simulation_results <- perform_simulation(data, params)
  
  # Prepare data frames
  data_frames <-
    prepare_data_frames(data,
                        simulation_results$price_paths,
                        params$time_steps,
                        params$num_paths)
  
  # Generate and save the snapshot for the most recent data point
  most_recent_time_step <- length(data$realized_prices)
  generate_snapshot(
    most_recent_time_step,
    snapshot_folder,
    data_frames$price_df,
    data_frames$real_prices_df,
    data_frames$time_labels,
    data$gexvol,
    data$initialprint,
    data$real_prices,
    params$time_steps,
    params$num_paths
  )
}

# Execute the modified main function
run_simulation()