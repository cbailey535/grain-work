# Sources:  https://github.com/Ishlafakhri/RFM-Customer-Segmentation/tree/main
# https://ishla.medium.com/predicting-customer-lifetime-value-clv-using-probabilistic-model-0330efe4e4b3


# Install libraries
library(tidyverse)
library(tidyquant)
library(ggrepel)
library(factoextra)
library(corrplot)
library(RColorBrewer) 

theme_set(theme_minimal())

df_src <- read_csv('Online_Retail.csv') %>%
  janitor::clean_names()

df_processed <- df_src %>%
  mutate(invoice_date = as.Date(invoice_date)) %>%
  filter(complete.cases(.)) %>%
  filter(!is.na(invoice_date) & !is.na(customer_id)) %>%
  filter(!grepl("C", invoice_no))
         
summary(df_processed)

# EDA
df_long <- df_processed %>%
  pivot_longer(cols = c(quantity, unit_price), names_to = "variable", values_to = "value")

# Create the box plot
ggplot(df_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(
    title = "Box Plot for Quantity and Price",
    x = "Variable",
    y = "Value"
  ) +
  theme_minimal()

# Function to remove outliers
remove_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  data %>% filter(data[[column]] >= lower_bound & data[[column]] <= upper_bound)
}

# Apply the function to both variables
df_no_outliers <- df_processed %>%
  remove_outliers("quantity") %>%
  remove_outliers("unit_price")


df_long_no_outliers <- df_no_outliers %>%
  pivot_longer(cols = c(quantity, unit_price), names_to = "variable", values_to = "value")

# Create the box plot
ggplot(df_long_no_outliers, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(
    title = "Box Plot for Quantity and Price with Outliers Removed",
    x = "Variable",
    y = "Value"
  ) +
  theme_minimal()

# Correlation
scaled_data <- df_no_outliers %>%
  select_if(is.numeric) %>%
  scale()

summary(scaled_data)

scaled_df <- as.data.frame(scaled_data) %>%
  select(c(
    quantity,
    unit_price
  ))

summary(scaled_df)

M<-cor(scaled_df) 
head(round(M,2)) 

# customize the correlogram 

col <- colorRampPalette(c("#BB4444", "#EE9988",  
                          "#FFFFFF", "#77AADD", 
                          "#4477AA")) 

corrplot(M, method = "color", col = col(200),   
         type = "upper", order = "hclust",  
         addCoef.col = "black", # Add coefficient of correlation 
         tl.col="black", tl.srt = 45, # Text label color and rotation 
         
         # Combine with significance 
         # p.mat = p.mat, 
         sig.level = 0.01, insig = "blank",  
         
         # hide correlation coefficient 
         # on the principal diagonal 
         diag = FALSE 
)

# Data Modelling
# Group data by CustomerID--

# Calculating the total sum for each transaction
df_total_sum <- df_no_outliers %>% mutate(total_sum = quantity * unit_price)

# Setting the snapshot date to the day after the last transaction
# Finding the maximum date in the InvoiceDate column
snapshot_date <- max(df_total_sum$invoice_date) + days(1)

# Aggregating data per customer
data_process <- df_total_sum %>%
  group_by(customer_id) %>%
  summarise(
    recency = as.numeric(difftime(snapshot_date, max(invoice_date), units = "days")), # Calculate recency
    frequency = n_distinct(invoice_no),  # Count unique InvoiceNo
    monetary = sum(total_sum)  # Sum TotalSum
  )

#Renaming columns to more descriptive names for RFM analysis
#data_process <- data_process %>%
#  rename(
#    invoice_date = recency,
#    invoice_no = frequency,
#    total_sum = monetary
#  )

# Create the histogram
library(gridExtra) # For arranging multiple plots


# Plot for Recency
plot_recency <- ggplot(data_process, aes(x = recency)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(title = "Recency Distribution", x = "Recency", y = "Density") +
  theme_minimal()

# Plot for Frequency
plot_frequency <- ggplot(data_process, aes(x = frequency)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "green", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(title = "Frequency Distribution", x = "Frequency", y = "Density") +
  theme_minimal()

# Plot for MonetaryValue
plot_monetary <- ggplot(data_process, aes(x = monetary)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "orange", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(title = "Monetary Value Distribution", x = "Monetary Value", y = "Density") +
  theme_minimal()

# Combine plots into a single figure
grid.arrange(plot_recency, plot_frequency, plot_monetary, nrow = 3)

# Data Model

# Define the feature columns to be standardized
feature_cols <- c("recency", "frequency", "monetary")

# Create a copy of the dataframe to hold the standardized values
standardized_data <- data_process

# Perform standardization using scale()
standardized_data[feature_cols] <- scale(data_process[feature_cols])

# Display the standardized data
head(standardized_data)

names(standardized_data)

hunter_df_labels <- standardized_data$customer_id
table(hunter_df_labels)

fcc_df_corr <- standardized_data %>%
  select(
    recency,
    frequency,
    monetary
  )

# Scale data (so distance metrics are unweighted)
#fcc_df_scale <- scale(fcc_df_corr)


# Calculate distance metric
#hunter_df_dist <- dist(hunter_df_scale)

#Calculate k based on WSS
fviz_nbclust(fcc_df_corr, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

# fviz_nbclust(fcc_df_scale, kmeans, method = "silhouette")    #MEMORY INTENSIVE

# Kmeans clustering
set.seed(80108)
km_out <- kmeans(fcc_df_corr, centers = 4, nstart = 100)
print(km_out)

# Visualize the cluster results
km_clusters <- km_out$cluster
rownames(fcc_df_scale) <- paste(src_data_trans$CENSUS_TRACT, 1:dim(src_data_trans)[1], sep = "_")
fviz_cluster(list(data=fcc_df_scale, cluster = km_clusters))
table(km_clusters, src_data_trans$CENSUS_TRACT)

seg_sum <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

seg_sum(src_data_trans,km_clusters)

clus_scores <- km_clusters
sales_scored <- tibble(src_data_trans,clus_scores)
write.csv(sales_scored, "fcc_clus_scores.csv")
