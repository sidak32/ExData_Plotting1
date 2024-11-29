# Load Required Libraries -------------------------------------------------

library(tidyverse)  # For data manipulation and visualization
library(lubridate)  # For date and time manipulation
library(data.table) # For efficient data handling

# Load Data ---------------------------------------------------------------

# Create a directory to store the data if it doesn't exist
if(!dir.exists("data")) { 
  dir.create("data") 
}

# Specify the URL and local file paths for downloading and extracting data
file.url   <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
file.path  <- "data/household_power_consumption.zip"
file.unzip <- "data/household_power_consumption.txt"

# Download and unzip the data file if it hasn't been downloaded yet
if(!file.exists(file.path) & !file.exists(file.unzip)) {
  download.file(file.url, file.path)  # Download the ZIP file
  unzip(file.path, exdir = "data")    # Unzip the file into the 'data' directory
}

# Load data for the specified dates (2007-02-01 to 2007-02-02) -------------
# Using fread for efficient reading and better handling of large datasets
data <- fread("data/household_power_consumption.txt", sep = ";", na.strings = "?")

# Convert Date column to Date type
data[, Date := as.Date(Date, format = "%d/%m/%Y")]

# Filter data for the relevant date range
data <- data[Date >= "2007-02-01" & Date <= "2007-02-02"]

# Combine Date and Time into a Single Datetime Variable --------------------
# Use lubridate's ymd_hms for better datetime handling
data[, datetime := ymd_hms(paste(Date, Time))]

# Make Plot ---------------------------------------------------------------

# Open a PNG graphics device to save the plot
png("plot3.png",
    width  = 480,  # Set the width of the output image
    height = 480)  # Set the height of the output image

# Plot the first sub-metering variable
plot(data$datetime, data$Sub_metering_1, type = "l",
     ylab = "Energy sub metering",  # Label for the y-axis
     xlab = NA,                     # No label for the x-axis
     col = "black")                 # Black for Sub_metering_1

# Add the second sub-metering variable as a red line
lines(data$datetime, data$Sub_metering_2, col = "red")

# Add the third sub-metering variable as a blue line
lines(data$datetime, data$Sub_metering_3, col = "blue")

# Add a legend to the plot in the top-right corner
legend("topright",
       col = c("black", "red", "blue"),  # Colors for the lines
       legend = c("Sub_metering_1",      # Labels for each line
                  "Sub_metering_2",
                  "Sub_metering_3"),
       lty = 1)                          # Line type

# Close the PNG device to finalize the plot
dev.off()

# Clear the R Environment -------------------------------------------------
rm(list = ls())  # Remove all objects from the environment
