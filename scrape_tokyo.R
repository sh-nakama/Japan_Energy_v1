library(rvest)
library(dplyr)
library(jsonlite)
library(lubridate)
library(purrr)
library(fs)
library(readr)
library(tidyr)

# download latest month's data
# https://www.tepco.co.jp/forecast/html/images/eria_jukyu_202412_03.csv

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

# Read the CSV file into R
tepco_data <- read.csv(destfile)

