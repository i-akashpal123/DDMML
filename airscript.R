# Install and load the readr package
install.packages("readr")
library(readr)

# Read the CSV file and extract the first 10,000 entries
data <- read_csv("Airbnb_Open_Data.csv", n_max = 10000)

# Save the first 10,000 entries to a new CSV file
write_csv(data, "new_air_bnb.csv")
