# Load libraries
install.packages("e1071")
library(e1071)

# Import data set
hdb = read.csv(file.choose(), header = TRUE)

############################# Data Cleaning ##############################

# View the first few row and Drop irrelevant columns
str(hdb)
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

# Remove duplicate rows
hdb_cleaned <- hdb_cleaned[!duplicated(hdb_cleaned), ]

############################## Check skewness for numerical columns ######################################
# Check for abnormal values
sum(hdb_cleaned$floor_area_sqm < 0 | hdb_cleaned$remaining_lease < 0 | hdb_cleaned$resale_price < 0)

str(hdb_cleaned)

# Convert 'year' and 'remaining_lease' to numeric
hdb_cleaned$year = as.numeric(hdb_cleaned$year)
hdb_cleaned$remaining_lease = as.numeric(hdb_cleaned$remaining_lease)

# Check for skewness
numeric_columns = names(hdb_cleaned)[sapply(hdb_cleaned, is.numeric)]
numeric_columns

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
barplot(category_count, main ="Flat Type distribution by Town Area",
        xlab = "Town", ylab = "Count", beside = TRUE,
        legend = rownames(category_count))

#formatted data
colsum_area = matrix(colSums(flat_type_vs_area),nrow = 1)
rowsum_area = matrix(c(rep(1,7)), nrow = 7)
flat_type_vs_area_formatted = round(flat_type_vs_area * 100/ (rowsum_area%*%colsum_area),2)
flat_type_vs_area_formatted
chisq.test(flat_type_vs_area)
#p-value <0.05, we reject H0, there is association between flat_type and town_area




str(hdb_cleaned)
central_floor_area = subset(hdb_cleaned[,c("floor_area_sqm","town_area","flat_type")], hdb_cleaned$town_area == 'Central' & hdb_cleaned$flat_type == "3 ROOM")
east_floor_area = subset(hdb_cleaned[,c("floor_area_sqm","town_area","flat_type")], hdb_cleaned$town_area == 'East' & hdb_cleaned$flat_type == "3 ROOM")
mean(central_floor_area$floor_area_sqm)
mean(east_floor_area$floor_area_sqm)

#do anova test first, then if there is no diff for mean  across town area, then we will say it might be other factor from linear regression
flat_type_vs_area
central_area = data.frame(area = c(hdb_cleaned$floor_area_sqm[hdb_cleaned$town_area == 'Central' & hdb_cleaned$flat_type == '3 ROOM']), town_area = 'Central')
east_area = data.frame(area = c(hdb_cleaned$floor_area_sqm[hdb_cleaned$town_area == 'East' & hdb_cleaned$flat_type == '3 ROOM']), town_area = 'East')
north_area = data.frame(area = c(hdb_cleaned$floor_area_sqm[hdb_cleaned$town_area == 'North' & hdb_cleaned$flat_type == '3 ROOM']), town_area = 'North')
south_area = data.frame(area = c(hdb_cleaned$floor_area_sqm[hdb_cleaned$town_area == 'South' & hdb_cleaned$flat_type == '3 ROOM']), town_area = 'South')
west_area = data.frame(area = c(hdb_cleaned$floor_area_sqm[hdb_cleaned$town_area == 'West' & hdb_cleaned$flat_type == '3 ROOM']), town_area = 'West')
merged_roomSize_townArea = rbind(central_area, east_area, north_area, south_area,west_area)
##Code to verify if the merging is correct
length(merged_roomSize_townArea$town_area[merged_roomSize_townArea$town_area=='Central'])
length(merged_roomSize_townArea$town_area[merged_roomSize_townArea$town_area=='East'])
length(merged_roomSize_townArea$town_area[merged_roomSize_townArea$town_area=='North'])
length(merged_roomSize_townArea$town_area[merged_roomSize_townArea$town_area=='South'])
length(merged_roomSize_townArea$town_area[merged_roomSize_townArea$town_area=='West'])
anova = aov(merged_roomSize_townArea$area~factor(merged_roomSize_townArea$town_area))
summary(anova)
##we find that p-value < 0.05. Thus, we reject H0:m1=m2=m3=m4=m5. We conclude that
##not all the mean are same, that's, mi != mj for some i and j
pairwise.t.test(merged_roomSize_townArea$area,merged_roomSize_townArea$town_area, p.adjust.method = 'none')
mean(west_area$area)
mean(north_area$area)
mean(central_area$area)
mean(east_area$area)
mean(south_area$area)
boxplot(merged_roomSize_townArea$area~merged_roomSize_townArea$town_area, main = '3 room type area vs Town Area')
boxplot(merged_roomSize_townArea$area[merged_roomSize_townArea$town_area == 'Central'], main='3 room area at central')
summary(merged_roomSize_townArea$area[merged_roomSize_townArea$town_area == 'Central'])
IQR(merged_roomSize_townArea$area[merged_roomSize_townArea$town_area == 'Central'])

#########trying to remove the outlier of 3 room area for each individual town area
#code here





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

##########################Linear regression################
# Convert categorical variables to factors
hdb_cleaned$flat_type.factor    <- factor(hdb_cleaned$flat_type)
hdb_cleaned$storey_range.factor <- factor(hdb_cleaned$storey_range)
hdb_cleaned$town_area.factor    <- factor(hdb_cleaned$town_area)

# Build the linear regression model
# Here we use floor_area_sqm, remaining_lease, year, flat_type, storey_range, and town_area as predictors
model <- lm(log_resale_price ~ floor_area_sqm + remaining_lease + year +
              flat_type.factor + town_area.factor, data = hdb_cleaned)

# Print the summary of the model to review coefficients and statistics
summary(model)
