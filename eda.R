# Load libraries
install.packages("e1071")
library(e1071)

# Import data set
hdb = read.csv(file.choose(), header = TRUE)

############################# Data Cleaning ##############################

# View the first few row and Drop irrelevant columns
head(hdb)

# Drop irrelevant columns
hdb_cleaned = subset(hdb, select = -c(id, block, street_name, lease_commence_date, flatm_name))

# Extract year from 'month' column
hdb_cleaned$year = substr(hdb_cleaned$month, 1, 4)
hdb_cleaned = subset(hdb_cleaned, select = -month)

# Extract years from remaining lease
hdb_cleaned$remaining_lease = substr(hdb_cleaned$remaining_lease, 1, 2)

hdb_cleaned

############################## Town area grouping ############################## 
unique(hdb_cleaned$town_name)

# Drop "CENTRAL AREA"
hdb_cleaned = hdb_cleaned[hdb_cleaned$town_name != "CENTRAL AREA",]

# Classify town_name by their geographical areas
north_towns = c("YISHUN", "SEMBAWANG", "WOODLANDS")
south_towns = c("BUKIT MERAH", "QUEENSTOWN", "MARINE PARADE", "BUKIT TIMAH", "CLEMENTI")
east_towns = c("PASIR RIS", "SENGKANG", "HOUGANG", "BEDOK", "PUNGGOL", "TAMPINES", "GEYLANG")
west_towns = c("CHOA CHU KANG", "JURONG WEST", "JURONG EAST", "CLEMENTI", "BUKIT BATOK", "BUKIT PANJANG")
central_towns = c("ANG MO KIO", "BISHAN", "TOA PAYOH", "KALLANG/WHAMPOA", "SERANGOON")

# Assign a new column 'area' based on the town names
hdb_cleaned$town_area = ifelse(hdb_cleaned$town_name %in% north_towns, "North",
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

hist(hdb_cleaned$resale_price)

# Log transform 'resale_price'
hdb_cleaned$log_resale_price = log(hdb_cleaned$resale_price)

#check hist and boxplot after log transform
hist(hdb_cleaned$log_resale_price)
boxplot(hdb_cleaned$log_resale_price)

############################## Removing outliers ##############################

# Remove outlier function using IQR method
remove_outliers_iqr = function(df, column) {
  Q1 = quantile(df[[column]], 0.25, na.rm = TRUE)
  Q3 = quantile(df[[column]], 0.75, na.rm = TRUE)
  IQR_value = Q3 - Q1
  lower_bound = Q1 - 1.5 * IQR_value
  upper_bound = Q3 + 1.5 * IQR_value
  
  df = df[df[[column]] >= lower_bound & df[[column]] <= upper_bound, ]
  return(df)
}

numeric_columns = names(hdb_cleaned)[sapply(hdb_cleaned, is.numeric)]

for (col in numeric_columns) {
  if (col != "resale_price") {
    hdb_cleaned = remove_outliers_iqr(hdb_cleaned, col)  
  }
}

boxplot(hdb_cleaned$log_resale_price)

############################## Graph plotting ##############################

# Histogram plotting function
par(mfrow = c(1,2))

#check normality using graph drawing method
plot_hist_with_npdf = function(x, col_name) {
  hist_data = hist(hdb_cleaned[[col_name]], main = paste("Histogram of", col_name), xlab = col_name)
  xpt = seq(min(x), max(x), length.out = length(x))
  n_den = dnorm(xpt, mean(x), sd(x))
  
  # Calculate the histogram scale factor
  bin_width = diff(hist_data$breaks[1:2])
  ypt = n_den * length(x) * bin_width
  lines(xpt, ypt, col = "blue")
}

numeric_columns = names(hdb_cleaned)[sapply(hdb_cleaned, is.numeric)]

for (col in numeric_columns) {
  print(paste("Skewness of", col, "=", skewness(hdb_cleaned[[col]])))
  plot_hist_with_npdf(hdb_cleaned[[col]], col)
  boxplot(hdb_cleaned[[col]], main = paste("Histogram of", col), xlab = col)
}

# Plot Flat Type distribution by Town Area
par(mfrow = c(1,1))

category_count = table(hdb_cleaned$flat_type, hdb_cleaned$town_area)
barplot(category_count, main ="Flat Type distribution by Town Area",
        xlab = "Town", ylab = "Count", 
        legend = rownames(category_count))

#####################################Data Analysis######################################


# #Which flat types are more popular for each town area/year(2 way contingency table)
# town_area = unique(hdb_cleaned$town_area)
# year = sort(unique(hdb_cleaned$year), decreasing = FALSE)
# year
# 
# #form a matrix for both variable
# contingency_table = table(hdb_cleaned$town_area, hdb_cleaned$year)
# contingency_table
# rowsum = matrix(rowSums(contingency_table), nrow = 5)
# colsum = matrix(colSums(contingency_table), nrow = 1)
# ex_area = rowsum %*% colsum / sum(rowsum)
# area_chisq = sum((contingency_table-ex_area)^2/ex_area)
# area_chisq
# dof_area = 16
# pvalue_area = 1-pchisq(area_chisq, dof_area)
# pvalue_area
# #since p-value = 4.761747e-13 < 0.05, we reject H0
# #Alternatively
# chisq.test(contingency_table)
# #since we have known there is relationship between year and area, we need to know the distribution
# barplot(contingency_table, beside=T)
# 
# #form the barplot we see that the sale in 2019 is significantly different and it might due to covid-19. So we drop the 2019 and check the association again
# contingency_table_cleaned = contingency_table[,1:4]
# contingency_table_cleaned
# chisq.test(contingency_table_cleaned)
# #p-value is 0.001 which is still < 0.05 so H0 is still rejected

flat_type_vs_area = table(hdb_cleaned$flat_type,hdb_cleaned$town_area)
flat_type_vs_area
barplot(flat_type_vs_area, beside = T)

#formatted data
colsum_area = matrix(colSums(flat_type_vs_area),nrow = 1)
rowsum_area = matrix(c(rep(1,7)), nrow = 7)
flat_type_vs_area_formatted = round(flat_type_vs_area * 100/ (rowsum_area%*%colsum_area),2)
flat_type_vs_area_formatted
chisq.test(flat_type_vs_area)
#p-value <0.05, we reject H0, there is association between flat_type and town_area

#do anova test first, then if there is no diff for mean  across town area, then we will say it might be other factor from linear regression

str(hdb_cleaned)
central_floor_area = subset(hdb_cleaned[,c("floor_area_sqm","town_area","flat_type")], hdb_cleaned$town_area == 'Central' & hdb_cleaned$flat_type == "3 ROOM")
east_floor_area = subset(hdb_cleaned[,c("floor_area_sqm","town_area","flat_type")], hdb_cleaned$town_area == 'East' & hdb_cleaned$flat_type == "3 ROOM")
mean(central_floor_area$floor_area_sqm)
mean(east_floor_area$floor_area_sqm)


