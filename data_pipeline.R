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

# Function to load historical data
load_historical_data <- function() {
  historical_file <- paste0(getwd(),"/",file.path("data", "historical", "historical_data.csv"))
  
  if(file_exists(historical_file)) {
    tryCatch({
      historical_data <- read_csv(historical_file, show_col_types = FALSE)
      return(historical_data)
    }, error = function(e) {
      message("Error loading historical data: ", e$message)
      return(NULL)
    })
  } else {
    message("No historical data file found. Will create new one.")
    return(NULL)
  }
}

# Function to append new data to historical data
append_to_historical <- function(new_data) {
  tryCatch({
    historical_file <- file.path("data", "historical", "historical_data.csv")
    
    # Load existing historical data
    historical_data <- load_historical_data()
    
    if(is.null(historical_data)) {
      # If no historical data exists, create new file with new data
      write_csv(new_data, historical_file)
      message("Created new historical data file")
    } else {
      # Combine historical and new data
      combined_data <- bind_rows(historical_data, new_data) %>%
        # Remove duplicates based on timestamp and other relevant columns
        distinct(date, .keep_all = TRUE) %>%
        # Sort by timestamp
        arrange(desc(timestamp))
      
      # Write back to CSV
      write_csv(combined_data, historical_file)
      message("Successfully appended new data to historical file")
    }
    
    # Create a backup
    backup_file <- file.path("data", "historical", 
                             paste0("historical_data_backup_", 
                                    format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
    file_copy(historical_file, backup_file)
    
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