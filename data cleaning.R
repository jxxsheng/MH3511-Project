# Load libraries
install.packages("e1071")
library(e1071)

# Import data set
hdb = read.csv(file.choose(), header = TRUE)

############################# Data Cleaning ############################## 
# Drop irrelevant columns
hdb_cleaned = subset(hdb, select = -c(id, block, street_name, lease_commence_date, flatm_name))

# Extract year from 'month' column
hdb_cleaned$year = substr(hdb_cleaned$month, 1, 4)
hdb_cleaned = subset(hdb_cleaned, select = -month)

# Extract years from remaining lease
hdb_cleaned$remaining_lease = substr(hdb_cleaned$remaining_lease, 1, 2)


############################## Town area grouping ############################## 
unique(hdb_cleaned$town_name)

# Drop "CENTRAL AREA"
hdb_cleaned = hdb_cleaned[hdb_cleaned$town_name != "CENTRAL AREA",]

# Classify town_name by their geographical areas
north_towns <- c("YISHUN", "SEMBAWANG", "WOODLANDS")
south_towns <- c("BUKIT MERAH", "QUEENSTOWN", "MARINE PARADE", "BUKIT TIMAH", "CLEMENTI")
east_towns <- c("PASIR RIS", "SENGKANG", "HOUGANG", "BEDOK", "PUNGGOL", "TAMPINES", "GEYLANG")
west_towns <- c("CHOA CHU KANG", "JURONG WEST", "JURONG EAST", "CLEMENTI", "BUKIT BATOK", "BUKIT PANJANG")
central_towns <- c("ANG MO KIO", "BISHAN", "TOA PAYOH", "KALLANG/WHAMPOA", "SERANGOON")

# Assign a new column 'area' based on the town names
hdb_cleaned$town_area <- ifelse(hdb_cleaned$town_name %in% north_towns, "North",
                   ifelse(hdb_cleaned$town_name %in% south_towns, "South",
                          ifelse(hdb_cleaned$town_name %in% east_towns, "East",
                                 ifelse(hdb_cleaned$town_name %in% west_towns, "West",
                                        ifelse(hdb_cleaned$town_name %in% central_towns, "Central", "Unknown")))))


############################## Check skewness for numerical columns ######################################
# Check for abnormal values
sum(hdb_cleaned$floor_area_sqm < 0 | hdb_cleaned$remaining_lease < 0 | hdb_cleaned$resale_price < 0)

str(hdb_cleaned)

# Convert 'year' and 'remaining_lease' to numeric
hdb_cleaned$year = as.numeric(hdb_cleaned$year)
hdb_cleaned$remaining_lease = as.numeric(hdb_cleaned$remaining_lease)

# Check for skewness
numeric_columns = names(hdb_cleaned)[sapply(hdb_cleaned, is.numeric)]

# Histogram plotting function
par(mfrow = c(1,2))
plot_hist_with_npdf = function(x, col_name) {
  hist(hdb_cleaned[[col_name]], main = paste("Histogram of", col_name), xlab = col_name)
  xpt = seq(min(x), max(x), length.out = 50)
  n_den = dnorm(xpt, mean(x), sd(x))
  
  # Calculate the histogram scale factor
  bin_width = diff(range(x)) / length(x)  
  scale = length(x) * bin_width / sum(n_den) 
  
  ypt = n_den * length(x) * scale
  lines(xpt, ypt, col = "blue")
}



for (col in numeric_columns) {
  print(paste("Skewness of", col, "=", skewness(hdb_cleaned[[col]])))
  plot_hist_with_npdf(hdb_cleaned[[col]], col)
  boxplot(hdb_cleaned[[col]], main = paste("Histogram of", col), xlab = col)
}
