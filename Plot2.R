# Load Required Libraries -------------------------------------------------

# 'tidyverse' is a collection of R packages designed for data science, including dplyr, ggplot2, and readr.
library(tidyverse)

# 'lubridate' provides functions for easier date and time manipulation.
library(lubridate)

# Data Loading Setup -------------------------------------------------------

# Create a directory named 'data' to store the downloaded dataset if it doesn't already exist
if(!dir.exists("data")) { 
  dir.create("data") 
}

# Define the URL for the dataset and local file paths for download and extraction
data_url   <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
zip_file    <- "data/household_power_consumption.zip"  # Path for the zip file
txt_file    <- "data/household_power_consumption.txt"  # Path for the extracted text file

# Download and unzip the data file if it has not been previously downloaded
if(!file.exists(zip_file) & !file.exists(txt_file)) {
  download.file(data_url, zip_file)  # Download the zip file from the specified URL
  unzip(zip_file, exdir = "data")     # Extract the contents of the zip file into the 'data' directory
}

# Load Data for Specific Dates ---------------------------------------------

# Read the household power consumption data from the text file
data <- read_delim("data/household_power_consumption.txt",
                   delim = ";",  # Specify the delimiter used in the data file
                   na = c("?"),  # Specify how missing values are represented in the dataset
                   col_types = list(  # Define the column types for reading the data
                     col_date(format = "%d/%m/%Y"),  # Date in day/month/year format
                     col_time(format = ""),            # Time without specific formatting
                     col_number(),                     # Numeric columns
                     col_number(),
                     col_number(),
                     col_number(),
                     col_number(),
                     col_number(),
                     col_number()
                   )) %>%
  # Filter the dataset to include only the specified date range (February 1-2, 2007)
  filter(between(Date, as.Date("2007-02-01"), as.Date("2007-02-02")))

# Combine Date and Time into a Single Datetime Variable ---------------------

# Create a new column 'datetime' by combining 'Date' and 'Time' columns
data <- mutate(data, datetime = ymd_hms(paste(Date, Time)))

# Make Plot of Global Active Power Over Time --------------------------------

# Create a line plot of 'Global_active_power' against the 'datetime' variable
plot(Global_active_power ~ datetime, data, type = "l",  # Type "l" specifies a line plot
     ylab = "Global Active Power (kilowatts)",  # Label for the y-axis
     xlab = NA)                                  # No label for the x-axis

# Save the Plot as a PNG File -----------------------------------------------

# Copy the plot to a PNG file with specified dimensions
dev.copy(png, "plot2.png",
         width = 480,   # Width of the image in pixels
         height = 480)  # Height of the image in pixels

# Close the PNG device to save the file
dev.off()

# Clear the Environment ----------------------------------------------------

# Remove all objects from the current R environment to free up memory
rm(list = ls())
