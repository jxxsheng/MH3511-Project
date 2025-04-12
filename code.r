# Load libraries
install.packages("e1071")
library(e1071)
install.packages("car")
library(car)

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

# Total number of missing values in the entire dataframe
sum(is.na(hdb_cleaned))

# Count of duplicate rows
sum(duplicated(hdb_cleaned))

#View duplicate rows 
hdb_cleaned[duplicated(hdb_cleaned), ]

# Remove duplicate rows
hdb_cleaned <- hdb_cleaned[!duplicated(hdb_cleaned), ]


# Get NUmeric values 
numeric_columns = names(hdb_cleaned)[sapply(hdb_cleaned, is.numeric)]
numeric_columns


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



for (col in numeric_columns) {
  print(paste("Summary for", col))
  print(summary(hdb_cleaned[[col]]))
  print(paste("Skewness of", col, "=", skewness(hdb_cleaned[[col]])))
  plot_hist_with_npdf(hdb_cleaned[[col]], col)
  boxplot(hdb_cleaned[[col]], main = paste("Histogram of", col), xlab = col)
}

# Plot Flat Type distribution by Town Area
par(mfrow = c(1,1))

head(hdb_cleaned)

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


for (col in numeric_columns) {
  if (col != "resale_price") {
    hdb_cleaned = remove_outliers_iqr(hdb_cleaned, col)  
    print(paste("Skewness of", col, "=", skewness(hdb_cleaned[[col]])))
  }
}

hist(hdb_cleaned$resale_price)

# Log transform 'resale_price'
hdb_cleaned$log_resale_price = log(hdb_cleaned$resale_price)
hdb_cleaned = remove_outliers_iqr(hdb_cleaned, "log_resale_price")

# Manually recompute IQR on current data
Q1 <- quantile(hdb_cleaned$log_resale_price, 0.25)
Q3 <- quantile(hdb_cleaned$log_resale_price, 0.75)
IQR_val <- Q3 - Q1
lower <- Q1 - 1.5 * IQR_val
upper <- Q3 + 1.5 * IQR_val

# Remove new "visual" outliers
hdb_cleaned <- hdb_cleaned[
  hdb_cleaned$log_resale_price >= lower &
    hdb_cleaned$log_resale_price <= upper, ]

# Now no outliers will be shown in boxplot
hist(hdb_cleaned$log_resale_price)
boxplot(hdb_cleaned$log_resale_price)
skewness(hdb_cleaned$log_resale_price)


#####################################Data Analysis######################################

##########################Linear regression################


hdb_cleaned$flat_type.factor    <- factor(hdb_cleaned$flat_type)
hdb_cleaned$storey_range.factor <- factor(hdb_cleaned$storey_range)
hdb_cleaned$town_area.factor    <- factor(hdb_cleaned$town_area)

# Build the linear regression model
# Here we use floor_area_sqm, remaining_lease, year, flat_type, storey_range, and town_area as predictors
model <- lm(log_resale_price ~ floor_area_sqm + remaining_lease + year +
              flat_type.factor + storey_range.factor + town_area.factor, data = hdb_cleaned)

# Print the summary of the model to review coefficients and statistics
summary(model)

##########################2-way Contingency Table################


flat_type_vs_area = table(hdb_cleaned$flat_type,hdb_cleaned$town_area)
flat_type_vs_area
barplot(flat_type_vs_area, beside = T, legend.text = T)

#formatted data
colsum_area = matrix(colSums(flat_type_vs_area),nrow = 1)
rowsum_area = matrix(c(rep(1,7)), nrow = 7)
flat_type_vs_area_formatted = round(flat_type_vs_area * 100/ (rowsum_area%*%colsum_area),2)
flat_type_vs_area_formatted
chisq.test(flat_type_vs_area)
#p-value <0.05, we reject H0, there is association between flat_type and town_area


##########################Welch Anova for three room################


central_area = data.frame(area = c(hdb_cleaned$floor_area_sqm[hdb_cleaned$town_area == 'Central' & hdb_cleaned$flat_type == '3 ROOM']), town_area = 'Central')
east_area = data.frame(area = c(hdb_cleaned$floor_area_sqm[hdb_cleaned$town_area == 'East' & hdb_cleaned$flat_type == '3 ROOM']), town_area = 'East')
north_area = data.frame(area = c(hdb_cleaned$floor_area_sqm[hdb_cleaned$town_area == 'North' & hdb_cleaned$flat_type == '3 ROOM']), town_area = 'North')
south_area = data.frame(area = c(hdb_cleaned$floor_area_sqm[hdb_cleaned$town_area == 'South' & hdb_cleaned$flat_type == '3 ROOM']), town_area = 'South')
west_area = data.frame(area = c(hdb_cleaned$floor_area_sqm[hdb_cleaned$town_area == 'West' & hdb_cleaned$flat_type == '3 ROOM']), town_area = 'West')
merged_roomSize_townArea = rbind(central_area, east_area, north_area, south_area,west_area)

boxplot(merged_roomSize_townArea$area~merged_roomSize_townArea$town_area, main = 'Floor Area for 3 room type vs Town Area')

##Code to verify if the merging is correct
length(merged_roomSize_townArea$town_area[merged_roomSize_townArea$town_area=='Central'])
length(merged_roomSize_townArea$town_area[merged_roomSize_townArea$town_area=='East'])
length(merged_roomSize_townArea$town_area[merged_roomSize_townArea$town_area=='North'])
length(merged_roomSize_townArea$town_area[merged_roomSize_townArea$town_area=='South'])
length(merged_roomSize_townArea$town_area[merged_roomSize_townArea$town_area=='West'])
leveneTest(merged_roomSize_townArea$area~ merged_roomSize_townArea$town_area, data = hdb_cleaned)
oneway.test(merged_roomSize_townArea$area ~ merged_roomSize_townArea$town_area, data = hdb_cleaned, var.equal = F)
pairwise.t.test(merged_roomSize_townArea$area,merged_roomSize_townArea$town_area,p.adjust.method = 'bonferroni', var.equal = F)

mean3_central = mean(central_area$area)
mean3_east = mean(east_area$area)
mean3_north = mean(north_area$area)
mean3_south = mean(south_area$area)
mean3_west = mean(west_area$area)

print(paste("Central mean", mean3_central))
print(paste("North mean", mean3_north))
print(paste("South mean", mean3_south))
print(paste("East mean", mean3_east))
print(paste("West mean", mean3_west))

##########################Welch Anova for four room################


central_area = data.frame(area = c(hdb_cleaned$floor_area_sqm[hdb_cleaned$town_area == 'Central' & hdb_cleaned$flat_type == '4 ROOM']), town_area = 'Central')
east_area = data.frame(area = c(hdb_cleaned$floor_area_sqm[hdb_cleaned$town_area == 'East' & hdb_cleaned$flat_type == '4 ROOM']), town_area = 'East')
north_area = data.frame(area = c(hdb_cleaned$floor_area_sqm[hdb_cleaned$town_area == 'North' & hdb_cleaned$flat_type == '4 ROOM']), town_area = 'North')
south_area = data.frame(area = c(hdb_cleaned$floor_area_sqm[hdb_cleaned$town_area == 'South' & hdb_cleaned$flat_type == '4 ROOM']), town_area = 'South')
west_area = data.frame(area = c(hdb_cleaned$floor_area_sqm[hdb_cleaned$town_area == 'West' & hdb_cleaned$flat_type == '4 ROOM']), town_area = 'West')
merged_roomSize_townArea = rbind(central_area, east_area, north_area, south_area,west_area)

boxplot(merged_roomSize_townArea$area~merged_roomSize_townArea$town_area, main = 'Floor Area for 3 room type vs Town Area')

##Code to verify if the merging is correct
length(merged_roomSize_townArea$town_area[merged_roomSize_townArea$town_area=='Central'])
length(merged_roomSize_townArea$town_area[merged_roomSize_townArea$town_area=='East'])
length(merged_roomSize_townArea$town_area[merged_roomSize_townArea$town_area=='North'])
length(merged_roomSize_townArea$town_area[merged_roomSize_townArea$town_area=='South'])
length(merged_roomSize_townArea$town_area[merged_roomSize_townArea$town_area=='West'])
leveneTest(merged_roomSize_townArea$area~ merged_roomSize_townArea$town_area, data = hdb_cleaned)
oneway.test(merged_roomSize_townArea$area ~ merged_roomSize_townArea$town_area, data = hdb_cleaned, var.equal = F)
pairwise.t.test(merged_roomSize_townArea$area,merged_roomSize_townArea$town_area,p.adjust.method = 'bonferroni', var.equal = F)

mean3_central = mean(central_area$area)
mean3_east = mean(east_area$area)
mean3_north = mean(north_area$area)
mean3_south = mean(south_area$area)
mean3_west = mean(west_area$area)

print(paste("Central mean", mean3_central))
print(paste("North mean", mean3_north))
print(paste("South mean", mean3_south))
print(paste("East mean", mean3_east))
print(paste("West mean", mean3_west))


##########################Anova Test################

##try anova on resale price
town_area = c('Central','East','North','South','West')
central_resale = data.frame(resale_price = c(hdb_cleaned$resale_price[hdb_cleaned$town_area == 'Central'& hdb_cleaned$flat_type == '3 ROOM']), town_area = 'Central')
east_resale = data.frame(resale_price = c(hdb_cleaned$resale_price[hdb_cleaned$town_area == 'East'& hdb_cleaned$flat_type == '3 ROOM']), town_area = 'East')
north_resale = data.frame(resale_price = c(hdb_cleaned$resale_price[hdb_cleaned$town_area == 'North'& hdb_cleaned$flat_type == '3 ROOM']), town_area = 'North')
south_resale = data.frame(resale_price = c(hdb_cleaned$resale_price[hdb_cleaned$town_area == 'South'& hdb_cleaned$flat_type == '3 ROOM']), town_area = 'South')
west_resale = data.frame(resale_price = c(hdb_cleaned$resale_price[hdb_cleaned$town_area == 'West'& hdb_cleaned$flat_type == '3 ROOM']), town_area = 'West')
merged_resalePrice_area = rbind(central_resale, east_resale, north_resale, south_resale,west_resale)
anova_resale = aov(merged_resalePrice_area$resale_price~factor(merged_resalePrice_area$town_area))
summary(anova_resale)
pairwise.t.test(merged_resalePrice_area$resale_price,merged_resalePrice_area$town_area,p.adjust.method = 'none')

mean3_central = mean(central_resale$resale_price)
mean3_east = mean(east_resale$resale_price)
mean3_north = mean(north_resale$resale_price)
mean3_south = mean(south_resale$resale_price)
mean3_west = mean(west_resale$resale_price)
boxplot(merged_resalePrice_area$resale_price~merged_resalePrice_area$town_area, main = '3 room type vs Town Area')



central_resale_4 = data.frame(resale_price = c(hdb_cleaned$resale_price[hdb_cleaned$town_area == 'Central'& hdb_cleaned$flat_type == '4 ROOM']), town_area = 'Central')
east_resale_4 = data.frame(resale_price = c(hdb_cleaned$resale_price[hdb_cleaned$town_area == 'East'& hdb_cleaned$flat_type == '4 ROOM']), town_area = 'East')
north_resale_4 = data.frame(resale_price = c(hdb_cleaned$resale_price[hdb_cleaned$town_area == 'North'& hdb_cleaned$flat_type == '4 ROOM']), town_area = 'North')
south_resale_4 = data.frame(resale_price = c(hdb_cleaned$resale_price[hdb_cleaned$town_area == 'South'& hdb_cleaned$flat_type == '4 ROOM']), town_area = 'South')
west_resale_4 = data.frame(resale_price = c(hdb_cleaned$resale_price[hdb_cleaned$town_area == 'West'& hdb_cleaned$flat_type == '4 ROOM']), town_area = 'West')
merged_resalePrice_area_4 = rbind(central_resale_4, east_resale_4, north_resale_4, south_resale_4,west_resale_4)
anova_resale_4 = aov(merged_resalePrice_area_4$resale_price~factor(merged_resalePrice_area_4$town_area))
summary(anova_resale_4)

mean4_central = mean(central_resale_4$resale_price)
mean4_east = mean(east_resale_4$resale_price)
mean4_north = mean(north_resale_4$resale_price)
mean4_south = mean(south_resale_4$resale_price)
mean4_west = mean(west_resale_4$resale_price)
boxplot(merged_resalePrice_area_4$resale_price~merged_resalePrice_area_4$town_area,main = '4 room type vs Town Area')
par(mfrow=c(1,2))

price_increase_central =  (mean4_central-mean3_central)
price_increase_east = (mean4_east- mean3_east)
price_increase_north = (mean4_north- mean3_north)
price_increase_south = (mean4_south - mean3_south)
price_increase_west = (mean4_west- mean3_west)
price_increase_area = rbind(price_increase_central,price_increase_east,price_increase_north,price_increase_south,price_increase_west)                                          
colnames(price_increase_area) = 'Price Increase'

price_increase_area = data.frame(price_increase = c(price_increase_central,price_increase_east,price_increase_north,price_increase_south,price_increase_west), town_area)
price_increase_area

barplot(
  price_increase_area$price_increase,
  names.arg = price_increase_area$town_area,
  main = "Price Increase by Town Area",
  xlab = "Town Area",
  ylab = "Price Increase",
  col = "skyblue"
)
