# data_pipeline.R
library(rvest)
library(dplyr)
library(jsonlite)
library(lubridate)
library(purrr)
library(fs)
library(readr)
library(tidyr)

setwd("C:/Users/SN/Documents/Sehun Nakama/Projects/Japan Energy Data/Japan_Energy_v1/")

# Create directories if they don't exist
dir_create("data/raw")
dir_create("data/processed")
dir_create("data/historical")

# Function to scrape data from a website
scrape_website <- function(url) {
  tryCatch({
    # get current date
    current_date <- Sys.Date()
    
    # Extract the year and month
    year <- format(current_date, "%Y")
    month <- format(current_date, "%m")
    
    # Construct the URL for the previous month's data
    url <- paste0("https://www.tepco.co.jp/forecast/html/images/eria_jukyu_", year, month, "_03.csv")
    
    # Define the destination file path
    destfile <- paste0("TEPCO_", year, "_", month, ".csv")
    
    # Download the file
    download.file(url, destfile, mode = "wb")
    
    data <- read.csv(paste0("TEPCO_", year, "_", month, ".csv"), skip = 1)
    
    return(data)
  }, error = function(e) {
    message("Error scraping website: ", e$message)
    return(NULL)
  })
}

# Function to parse date and time columns
parse_datetime <- function(date_col, time_col) {
  # Combine date and time columns
  datetime <- paste(date_col, time_col)
  # Parse to POSIXct
  as.POSIXct(datetime, format = "%Y/%m/%d %H:%M")
}

# Function to extract half-hour from time string
get_half_hour <- function(time_col) {
  tryCatch({
    # Handle different time formats
    # For "HH:MM" format
    if (all(grepl("^\\d{1,2}:\\d{2}$", time_col))) {
      return(time_col)
    }
    
    # For "HHMM" format
    if (all(grepl("^\\d{4}$", time_col))) {
      return(sprintf("%02d:%02d", 
                     as.integer(substr(time_col, 1, 2)),
                     as.integer(substr(time_col, 3, 4))))
    }
    
    # For other formats, try to convert using lubridate
    times <- hms::as_hms(time_col)
    return(format(times, "%H:%M"))
    
  }, error = function(e) {
    message("Error processing time format: ", e$message)
    return(time_col)
  })
}



# Function to process raw data
process_data <- function(raw_data) {
  tryCatch({
    # # Example processing steps - modify based on your needs
    # processed <- raw_data %>%
    #   as.data.frame() %>%
    #   # Clean column names
    #   janitor::clean_names() %>%
    #   # Remove duplicates
    #   distinct() %>%
    #   # Handle missing values
    #   mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm = TRUE)))) %>%
    #   # Add derived metrics
    #   mutate(
    #     date = as_date(timestamp),
    #     month = floor_date(date, "month"),
    #     # Add your custom calculations here
    #   )
    
    return(processed)
  }, error = function(e) {
    message("Error processing data: ", e$message)
    return(NULL)
  })
}

# Function to aggregate historical data by year-month
aggregate_by_month_halfhour <- function(data) {
  tryCatch({
    if(!"TIME" %in% names(data)) {
      stop("TIME column not found in data")
    }
    
    if(!"DATE" %in% names(data)) {
      stop("DATE column not found in data")
    }
    
    # Create datetime and extract components
    processed_data <- data %>%
      mutate(
        # Ensure TIME column is character
        TIME = as.character(TIME),
        # Extract half-hour
        half_hour = get_half_hour(TIME),
        # Create month from DATE
        month = format(as.Date(DATE), "%Y-%m")
      )
    
    # Print sample of processed time data for debugging
    message("Sample of processed times:")
    print(head(data.frame(
      original_time = processed_data$TIME,
      half_hour = processed_data$half_hour
    )))
    
    # Proceed with aggregation
    processed_data %>%
      group_by(month, half_hour) %>%
      summarise(
        across(
          where(is.numeric),
          ~mean(., na.rm = TRUE)
        ),
        n_observations = n(),
        .groups = "drop"
      ) %>%
      arrange(month, half_hour) %>%
      mutate(
        across(
          where(is.numeric),
          ~round(.)
        )
      )
  }, error = function(e) {
    message("Error aggregating data: ", e$message)
    message("Data structure:")
    str(data)
    return(NULL)
  })
}

# Helper function to check time format
check_time_format <- function(data) {
  if(!"TIME" %in% names(data)) {
    return(FALSE)
  }
  
  time_sample <- head(data$TIME)
  message("Sample TIME values: ", paste(time_sample, collapse = ", "))
  
  return(TRUE)
}


# Function to prepare data for area plot
prepare_area_plot_data <- function(aggregated_data, selected_months = NULL) {
  plot_data <- aggregated_data
  
  # Filter for selected months if specified
  if (!is.null(selected_months)) {
    plot_data <- plot_data %>%
      filter(month %in% selected_months)
  }
}

# Function to get specific metrics from aggregated data
get_monthly_metrics <- function(aggregated_data, metric_name) {
  # Get all columns related to the specified metric
  metric_cols <- grep(paste0("^", metric_name, "_"), names(aggregated_data), value = TRUE)
  
  if(length(metric_cols) > 0) {
    aggregated_data %>%
      select(year_month, all_of(metric_cols))
  } else {
    message("Metric not found in aggregated data")
    return(NULL)
  }
}

# Function to load historical data
load_historical_data <- function(aggregate = FALSE) {
  historical_file <- paste0(getwd(),"/",file.path("data", "historical", "historical_data.csv"))
  
  if(file_exists(historical_file)) {
    tryCatch({
      # Read the data with specified column types
      data <- read_csv(historical_file, 
                       col_types = cols(.default = col_integer(),
                                        DATE = col_character(),
                                        TIME = col_character()))
      
      # Return aggregated data if requested
      if(aggregate) {
        return(aggregate_by_month_halfhour(data))
      }
      
      return(data)
    }, error = function(e) {
      message("Error loading historical data: ", e$message)
      return(NULL)
    })
  } else {
    message("No historical data file found.")
    return(NULL)
  }
}

# Function to append new data to historical data
append_to_historical <- function(new_data) {
  tryCatch({
    historical_file <- paste0(getwd(), "/", file.path("data", "historical", "historical_data.csv"))
    
    # Check if file exists and read it
    existing_data <- read_historical_csv(historical_file)
    
    # Prepare the data to be written
    if(is.null(existing_data)) {
      data_to_write <- new_data
      message("Creating new historical data file")
    } else {
      data_to_write <- bind_rows(existing_data, new_data) %>%
        # Remove duplicates based on DATE and TIME
        distinct(DATE, TIME, .keep_all = TRUE) %>%
        # Sort by DATE and TIME
        arrange(desc(DATE), desc(TIME))
      message("Combining with existing historical data")
    }
    
    # Create directories if they don't exist
    dir_create(dirname(historical_file))
    
    # Write the data
    write_csv(data_to_write, historical_file)
    
    # Create a backup
    backup_file <- paste0(getwd(), "/", file.path("data", "historical", 
                                                  paste0("historical_data_backup_", 
                                                         format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")))
    file_copy(historical_file, backup_file, overwrite = TRUE)
    
    message("Successfully saved and backed up data")
    return(TRUE)
    
  }, error = function(e) {
    message("Error updating historical data: ", e$message)
    return(FALSE)
  })
}

# Function to save JSON snapshots
save_json_snapshot <- function(data, type = "raw") {
  tryCatch({
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filepath <- file.path("data", type, paste0(type, "_data_", timestamp, ".json"))
    write_json(data, filepath)
    
    if(type == "processed") {
      latest_filepath <- file.path("data", type, "latest_processed_data.json")
      write_json(data, latest_filepath)
    }
    
    message(sprintf("JSON snapshot saved to %s", filepath))
    return(TRUE)
  }, error = function(e) {
    message("Error saving JSON snapshot: ", e$message)
    return(FALSE)
  })
}

# Main scraping function
run_scraping_pipeline <- function(urls) {
  # Log start time
  start_time <- Sys.time()
  message("Starting scraping pipeline at ", start_time)
  
  # Create empty list to store results
  all_raw_data <- list()
  
  # Scrape each URL
  for(url in urls) {
    message("Scraping ", url)
    raw_data <- scrape_website()
    if(!is.null(raw_data)) {
      all_raw_data[[length(all_raw_data) + 1]] <- raw_data
    }
  }
  
  # Combine all raw data
  combined_raw_data <- bind_rows(all_raw_data)
  
  # Save raw JSON snapshot
  save_json_snapshot(combined_raw_data, "raw")
  
  # Process data
  processed_data <- process_data(combined_raw_data)
  
  if(!is.null(processed_data)) {
    # Save processed JSON snapshot
    save_json_snapshot(processed_data, "processed")
    
    # Append to historical CSV
    append_success <- append_to_historical(processed_data)
    
    if(append_success) {
      message("Successfully updated historical data")
    } else {
      message("Failed to update historical data")
    }
  }
  
  # Log completion
  end_time <- Sys.time()
  message("Pipeline completed at ", end_time)
  message("Total time: ", difftime(end_time, start_time, units = "mins"), " minutes")
  
  # Return the processed data
  return(processed_data)
}

# Function to get historical data summary
get_historical_summary <- function() {
  historical_data <- load_historical_data()
  
  if(!is.null(historical_data)) {
    summary <- list(
      total_records = nrow(historical_data),
      date_range = range(historical_data$DATE)
      # last_update = max(historical_data$timestamp)
    )
    return(summary)
  }
  return(NULL)
}

# Example usage:
urls <- c(
 "https://www.tepco.co.jp/forecast/html/area_jukyu-j.html"
)

# Run the pipeline
# new_data <- run_scraping_pipeline(urls)

# Print summary of historical data
print(get_historical_summary())