# Load Required Libraries -------------------------------------------------

# 'tidyverse' is a collection of packages for data manipulation and visualization.
library(tidyverse)

# 'lubridate' simplifies date-time manipulation.
library(lubridate)

# Data Loading Setup -------------------------------------------------------

# Create a directory named 'data' to store the downloaded data if it doesn't exist
if(!dir.exists("data")) { 
  dir.create("data") 
}

# Define the URL for the dataset and local file paths
data_url   <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
zip_file    <- "data/household_power_consumption.zip"  # Path for the zip file
txt_file    <- "data/household_power_consumption.txt"  # Path for the unzipped text file

# Download and unzip the data file if it has not been previously downloaded
if(!file.exists(zip_file) & !file.exists(txt_file)) {
  download.file(data_url, zip_file)  # Download the zip file
  unzip(zip_file, exdir = "data")     # Extract the contents of the zip file into 'data' directory
}

# Load data for the dates 2007-02-01 to 2007-02-02 -------------------------

# Read the household power consumption data from the text file
power_data <- read_delim("data/household_power_consumption.txt",
                         delim = ";",  # Specify the delimiter used in the data file
                         na = c("?"),  # Specify how missing values are represented
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
  # Filter the dataset to include only the specified date range
  filter(between(Date, as.Date("2007-02-01"), as.Date("2007-02-02")))

# Generate Histogram -------------------------------------------------------

# Create a histogram of the 'Global_active_power' variable
hist(power_data$Global_active_power,
     xlab = "Global Active Power (kilowatts)",  # Label for the x-axis
     col = "red",                                # Color of the bars
     main = "Global Active Power")               # Title of the histogram

# Save the Histogram as a PNG file ----------------------------------------

# Copy the histogram to a PNG file with specified dimensions
dev.copy(png, "plot1.png",
         width = 480,   # Width of the image in pixels
         height = 480)  # Height of the image in pixels

# Close the PNG device to save the file
dev.off()

# Clear the Environment ----------------------------------------------------

# Remove all objects from the current R environment to free up memory
rm(list = ls())
