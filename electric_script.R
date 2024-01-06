# Install and load the readr package
install.packages("readr")
library(readr)

# Read the CSV file and extract the first 10,000 entries
data <- read_csv("Electric_Vehicle_Population_Data.csv", n_max = 11000)

# Save the first 10,000 entries to a new CSV file
write_csv(data, "electric_veh_data_updated.csv")
