# Load libraries
install.packages("e1071")
library(e1071)
install.packages("car")
library(car)
install.packages("ARTool")
library(ARTool)
library(dplyr)


# Import data set
hdb = read.csv(file.choose(), header = TRUE)


############################# Data Cleaning ##############################

# View the first few row and Drop irrelevant columns
head(hdb)

# Drop irrelevant columns
hdb_cleaned = subset(hdb, select = -c(id, block, street_name, lease_commence_date, flatm_name, storey_range))

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

plot_hist_with_npdf = function(x, col_name) {
  hist_data = hist(
    hdb_cleaned[[col_name]],
    main = paste("Histogram of", col_name),
    xlab = col_name,
    col = "#CDE6F9",
    border = "#9BB8D3"     # orange-red border
  )
  
  xpt = seq(min(x), max(x), length.out = length(x))
  n_den = dnorm(xpt, mean(x), sd(x))
  
  bin_width = diff(hist_data$breaks[1:2])
  ypt = n_den * length(x) * bin_width
  lines(xpt, ypt, col = "blue", lwd = 2)
}




for (col in numeric_columns) {
  print(paste("Summary for", col))
  print(summary(hdb_cleaned[[col]]))
  print(paste("Skewness of", col, "=", skewness(hdb_cleaned[[col]])))
  plot_hist_with_npdf(hdb_cleaned[[col]], col)
  boxplot(
    hdb_cleaned[[col]],
    main = paste("Boxplot of", col),
    xlab = col,
    col = "#CDE6F9",
    border = "#9BB8D3"     
  )
  
}



############################## Removing outliers ##############################
# Remove outliers using IQR method
remove_outliers_iqr = function(df, column) {
  Q1 = quantile(df[[column]], 0.25, na.rm = TRUE)
  Q3 = quantile(df[[column]], 0.75, na.rm = TRUE)
  IQR_value = Q3 - Q1
  lower_bound = Q1 - 1.5 * IQR_value
  upper_bound = Q3 + 1.5 * IQR_value
  
  # Print how many remain within bounds
  print(sum(df[[column]] >= lower_bound & df[[column]]> upper_bound))
  
  # Filter out outliers
  df = df[df[[column]] < lower_bound | df[[column]] <= upper_bound, ]
  
  return(df)
}



for (col in numeric_columns) {
  if (col != "resale_price") {
    hdb_cleaned = remove_outliers_iqr(hdb_cleaned, col)  
    print(paste("Skewness of", col, "=", skewness(hdb_cleaned[[col]])))
    plot_hist_with_npdf(hdb_cleaned[[col]], col)
    boxplot(
      hdb_cleaned[[col]],
      main = paste("Boxplot of", col),
      xlab = col,
      col = "#CDE6F9",
      border = "#9BB8D3"     
    )
  }
}


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
plot_hist_with_npdf(hdb_cleaned[["log_resale_price"]], "log_resale_price")
boxplot(
  hdb_cleaned[["log_resale_price"]],
  main = paste("Boxplot of", "log_resale_price"),
  xlab = "log_resale_price",
  col = "#CDE6F9",
  border = "#9BB8D3"     
)
skewness(hdb_cleaned$log_resale_price)

unique(hdb_cleaned$flat_type)


par(mfrow = c(1,1))


##########################Value count for categorical value################
category_count <- table(hdb_cleaned$flat_type)
barplot(category_count,
        main = "Flat Type Distribution",
        ylab = "Count",
        col = "#A3C4DC",      # pastel blue
        border = "#5D8AA8")   # slightly darker border

category_count <- table(hdb_cleaned$town_area)
barplot(category_count,
        main = "Town Area Distribution",
        xlab = "Town Area",
        ylab = "Count",
        col = "#A3C4DC",      # pastel blue
        border = "#5D8AA8")   # slightly darker border


#####################################Data Analysis######################################
##########################Correlation Heatmap################
# Correlation Heatmap
hdb_subset = hdb_cleaned[, -c(2,6)]  # Drop storey and town name
hdb_subset$flat_type <- factor(hdb_cleaned$flat_type,
                               levels = c("1 ROOM", "2 ROOM", "3 ROOM", "4 ROOM", 
                                          "5 ROOM", "EXECUTIVE", "MULTI-GENERATION"),
                               ordered = TRUE)
hdb_subset$town_area <- factor(hdb_cleaned$town_area,
                               levels = c("East", "West", "Central", "South", "North"),
                               ordered = FALSE)
hdb_subset$flat_type <- as.numeric(hdb_subset$flat_type)
hdb_subset$town_area <- as.numeric(hdb_subset$town_area)


cor_matrix = cor(hdb_subset)
par(mfrow = c(1,1))

# Normal red/blue colour heatmap
heatmap(cor_matrix,
        col = colorRampPalette(c("blue", "white", "red"))(100),
        scale = "none",  
        margins = c(7, 7)) 

# Scatterplot matrix sample(1000)
set.seed(123)  # For reproducibility
sampled_data <- hdb_subset[sample(nrow(hdb_subset), 1000), ]
pairs(sampled_data, main = "Scatterplot Matrix of HDB Data")

##########################Linear regression################
# Convert categorical variables to factors
hdb_cleaned$flat_type_grouped <- as.character(hdb_cleaned$flat_type)
hdb_cleaned$flat_type_grouped[hdb_cleaned$flat_type_grouped %in% c("1 ROOM", "2 ROOM", "MULTI-GENERATION")] <- "OTHER"
hdb_cleaned$flat_type_grouped <- factor(hdb_cleaned$flat_type_grouped)

hdb_cleaned$town_area.factor    <- factor(hdb_cleaned$town_area)

# Build the linear regression model
# Here we use floor_area_sqm, remaining_lease, year, flat_type, and town_area as predictors
model1 <- lm(log_resale_price ~ floor_area_sqm + remaining_lease + year +
               flat_type_grouped + town_area.factor, data = hdb_cleaned)

# Print the summary of the model to review coefficients and statistics
summary(model1)

#try to find out the single most important facotr that is affecting the resale price
#resale price and floor area
model2 <- lm(log_resale_price ~ floor_area_sqm, data = hdb_cleaned)
summary(model2)

#resale price and remaining lease
model3 <- lm(log_resale_price ~ remaining_lease, data = hdb_cleaned)
summary(model3)

#resale price and year
model4 <- lm(log_resale_price ~ year ,data = hdb_cleaned)
summary(model4)

##########################2-way Contingency Table################

# Generate the table
flat_type_vs_area <- table(hdb_cleaned$flat_type, hdb_cleaned$town_area)

# Choose a set of distinct colors for each flat type
colors <- c("#FFADAD", "#FFD6A5", "#FDFFB6", "#CAFFBF", "#9BF6FF", "#BDB2FF")

# Plot with colors
barplot(flat_type_vs_area,
        beside = TRUE,
        col = colors,
        legend.text = TRUE,
        args.legend = list(title = "Flat Type", x = "topright", cex = 0.8),
        main = "Flat Type Distribution by Town Area",
        xlab = "Town Area",
        ylab = "Count",
        las = 2,         # rotate x-axis labels for readability
        cex.names = 0.7  # shrink x-axis labels slightly
)


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
boxplot(
  merged_roomSize_townArea$area~merged_roomSize_townArea$town_area,
  main = 'Floor Area for 3 room type vs Town Area',
  xlab="Town Area",
  ylab="Floor area(sqm)",
  col = "#CDE6F9",
  border = "#9BB8D3"     
)

#Code to verify if the merging is correct
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
boxplot(
  merged_roomSize_townArea$area~merged_roomSize_townArea$town_area,
  main = 'Floor Area for 4 room type vs Town Area',
  xlab="Town Area",
  ylab="Room Size(sqm)",
  col = "#CDE6F9",
  border = "#9BB8D3"     
)
boxplot(merged_roomSize_townArea$area~merged_roomSize_townArea$town_area, main = 'Floor Area for 4 room type vs Town Area')

##Code to verify if the merging is correct
length(merged_roomSize_townArea$town_area[merged_roomSize_townArea$town_area=='Central'])
length(merged_roomSize_townArea$town_area[merged_roomSize_townArea$town_area=='East'])
length(merged_roomSize_townArea$town_area[merged_roomSize_townArea$town_area=='North'])
length(merged_roomSize_townArea$town_area[merged_roomSize_townArea$town_area=='South'])
length(merged_roomSize_townArea$town_area[merged_roomSize_townArea$town_area=='West'])
leveneTest(merged_roomSize_townArea$area~ merged_roomSize_townArea$town_area, data = hdb_cleaned)
oneway.test(merged_roomSize_townArea$area ~ merged_roomSize_townArea$town_area, data = hdb_cleaned, var.equal = F)
pairwise.t.test(merged_roomSize_townArea$area,merged_roomSize_townArea$town_area,p.adjust.method = 'bonferroni', var.equal = F)

mean4_central = mean(central_area$area)
mean4_east = mean(east_area$area)
mean4_north = mean(north_area$area)
mean4_south = mean(south_area$area)
mean4_west = mean(west_area$area)

print(paste("Central mean", mean4_central))
print(paste("North mean", mean4_north))
print(paste("South mean", mean4_south))
print(paste("East mean", mean4_east))
print(paste("West mean", mean4_west))


##########################Price Increase################


town_area = c('Central','East','North','South','West')
central_resale = data.frame(resale_price = c(hdb_cleaned$resale_price[hdb_cleaned$town_area == 'Central'& hdb_cleaned$flat_type == '3 ROOM']), town_area = 'Central')
east_resale = data.frame(resale_price = c(hdb_cleaned$resale_price[hdb_cleaned$town_area == 'East'& hdb_cleaned$flat_type == '3 ROOM']), town_area = 'East')
north_resale = data.frame(resale_price = c(hdb_cleaned$resale_price[hdb_cleaned$town_area == 'North'& hdb_cleaned$flat_type == '3 ROOM']), town_area = 'North')
south_resale = data.frame(resale_price = c(hdb_cleaned$resale_price[hdb_cleaned$town_area == 'South'& hdb_cleaned$flat_type == '3 ROOM']), town_area = 'South')
west_resale = data.frame(resale_price = c(hdb_cleaned$resale_price[hdb_cleaned$town_area == 'West'& hdb_cleaned$flat_type == '3 ROOM']), town_area = 'West')
merged_resalePrice_area = rbind(central_resale, east_resale, north_resale, south_resale,west_resale)

mean3_central = mean(central_resale$resale_price)
mean3_east = mean(east_resale$resale_price)
mean3_north = mean(north_resale$resale_price)
mean3_south = mean(south_resale$resale_price)
mean3_west = mean(west_resale$resale_price)
boxplot(merged_resalePrice_area$resale_price~merged_resalePrice_area$town_area, main = 'Resale Price for 3 room type vs Town Area')



central_resale_4 = data.frame(resale_price = c(hdb_cleaned$resale_price[hdb_cleaned$town_area == 'Central'& hdb_cleaned$flat_type == '4 ROOM']), town_area = 'Central')
east_resale_4 = data.frame(resale_price = c(hdb_cleaned$resale_price[hdb_cleaned$town_area == 'East'& hdb_cleaned$flat_type == '4 ROOM']), town_area = 'East')
north_resale_4 = data.frame(resale_price = c(hdb_cleaned$resale_price[hdb_cleaned$town_area == 'North'& hdb_cleaned$flat_type == '4 ROOM']), town_area = 'North')
south_resale_4 = data.frame(resale_price = c(hdb_cleaned$resale_price[hdb_cleaned$town_area == 'South'& hdb_cleaned$flat_type == '4 ROOM']), town_area = 'South')
west_resale_4 = data.frame(resale_price = c(hdb_cleaned$resale_price[hdb_cleaned$town_area == 'West'& hdb_cleaned$flat_type == '4 ROOM']), town_area = 'West')
merged_resalePrice_area_4 = rbind(central_resale_4, east_resale_4, north_resale_4, south_resale_4,west_resale_4)


mean4_central = mean(central_resale_4$resale_price)
mean4_east = mean(east_resale_4$resale_price)
mean4_north = mean(north_resale_4$resale_price)
mean4_south = mean(south_resale_4$resale_price)
mean4_west = mean(west_resale_4$resale_price)
boxplot(merged_resalePrice_area_4$resale_price~merged_resalePrice_area_4$town_area,main = 'Resale Price for 4 room type vs Town Area')
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
  col = "#CDE6F9",
)
###################Test if price difference across flat types vary#############

# Ensure variables are factors
hdb_cleaned$flat_type <- factor(hdb_cleaned$flat_type)
hdb_cleaned$town_area <- factor(hdb_cleaned$town_area)

# Combine flat_type and town_area into a single group
hdb_cleaned$group_combo <- interaction(hdb_cleaned$flat_type, hdb_cleaned$town_area)

# Perform Levene's Test on resale price (or log_resale_price)
leveneTest(log_resale_price ~ group_combo, data = hdb_cleaned)

# model wont run if the interacted values dont exist
table(hdb_cleaned$flat_type, hdb_cleaned$town_area)

# Remove rare flat types BEFORE filtering
cleaned_subset <- hdb_cleaned %>%
  filter(!(flat_type %in% c("1 ROOM", "MULTI-GENERATION")))

# Then filter for combinations with enough entries, and drop unused levels
valid_combinations <- cleaned_subset %>%
  group_by(flat_type, town_area) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  mutate(
    flat_type = droplevels(flat_type),
    town_area = droplevels(town_area)
  )


levels(valid_combinations$flat_type)
levels(valid_combinations$town_area)

# Fit ART model
art_model <- art(resale_price ~ flat_type * town_area, data = valid_combinations)
anova(art_model)







levels(valid_combinations$flat_type)
levels(valid_combinations$town_area)

# Fit ART model
art_model <- art(resale_price ~ flat_type * town_area, data = valid_combinations)
anova(art_model)
