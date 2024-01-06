# Install and load the readr package
install.packages("readr")
library(readr)

# Read the CSV file and extract the first 10,000 entries
data <- read_csv("Crime_Data_From_2020_to_present.csv", n_max = 11000)

# Save the first 10,000 entries to a new CSV file
write_csv(data, "crimedata.csv")
