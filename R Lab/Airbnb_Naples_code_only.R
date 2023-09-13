# List of CRAN packages
cran_packages <- c(
  "tidyverse", 
  "stringr", 
  "cluster", 
  "fpc", 
  "clustMixType", 
  "geojsonio", 
  "leaflet", 
  "ggplot2", 
  "FactoMineR", 
  "kmed",
  "mgsub"
)

# Install CRAN packages
new_packages <- cran_packages[!(cran_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# URL of the compressed CSV file
url <- "http://data.insideairbnb.com/italy/campania/naples/2023-06-21/data/listings.csv.gz"

# Temporary file to store the downloaded data
temp_file <- tempfile(fileext = ".csv.gz")

# Download the file
download.file(url, temp_file)

# Read the compressed CSV file into a data frame
airbnb_data <- read_csv(temp_file)

# Clean up
unlink(temp_file)

airbnb_data <- drop_na(airbnb_data, price, review_scores_rating)


airbnb_data <- mutate(airbnb_data, price = as.numeric(gsub("[$,]", "", price)))

# Convert date columns to Date type
airbnb_data <- mutate(airbnb_data, last_review = as.Date(last_review, format="%Y-%m-%d"))

# Convert 'room_type' to factor, remove special characters from 'name'
airbnb_data <- mutate(airbnb_data, room_type = as.factor(room_type),
                      name = gsub("[^[:alnum:][:space:]]", "", name))

# Compute price per guest and filter out extreme values
airbnb_data <- mutate(airbnb_data, price_per_guest = price / accommodates) %>% 
  filter(price_per_guest <= 1000)

# Extract and convert bathroom details
airbnb_data <- airbnb_data %>% 
  mutate(
    bathrooms = as.numeric(str_extract(bathrooms_text, "\\d+")),
    bathrooms = if_else(
      str_detect(bathrooms_text, "shared"), 
      bathrooms / 2, 
      bathrooms
    ),
    bathrooms = if_else(
      str_detect(bathrooms_text, "private"), 
      bathrooms, 
      bathrooms
    )
  )

# Define a function to safely convert the percentage string to numeric
convert_to_numeric <- function(x) {
  num <- as.numeric(str_replace(x, "%", ""))
  ifelse(is.na(num), NA, num / 100)
}

# Use the function to safely convert the columns
airbnb_data <- airbnb_data %>%
  mutate(
    host_acceptance_rate = suppressWarnings(convert_to_numeric(host_acceptance_rate)),
    host_response_rate = suppressWarnings(convert_to_numeric(host_response_rate))
  )

# Compute and add the number of amenities as a new column
airbnb_data <- mutate(airbnb_data, num_amenities = str_count(amenities, ",") + 1)

# Convert 'host_since', 'first_review', and 'last_review' to Date and calculate durations
current_date <- as.Date("2023-06-21")
airbnb_data <- mutate(airbnb_data, host_since = as.Date(host_since),
                      first_review = as.Date(first_review),
                      last_review = as.Date(last_review),
                      host_length = as.integer(current_date - host_since),
                      since_first_review = as.integer(current_date - first_review),
                      since_last_review = as.integer(current_date - last_review))

# Add 'multiple_host_listings' based on the condition
airbnb_data <- mutate(airbnb_data, multiple_host_listings = if_else(calculated_host_listings_count > 1, 1, 0))

amenities <- as.data.frame(mgsub(airbnb_data$amenities, c("\\{", "\\}", "\\,", "\\'", "\\[", "\\]", "\"", "\\/") , c("", "", "  ", "", "", "", "", "")))
host_verifications <- as.data.frame(mgsub(airbnb_data$host_verifications, c("\\[", "\\]", "\\,", "\\'") , c("", "", "", "")))

output_list_verifications <- list()
output_list_verifications_count <- list()
output_list_amenities <- list()
output_list_amenities_count <- list()

for(i in 1:nrow(host_verifications)){
  output_list_verifications[i] <- str_split(host_verifications[i, ], " ")[1]
  output_list_verifications_count[i] <- length(output_list_verifications[[i]])
  output_list_amenities[i] <- str_split(amenities[i, ], "  ")[1]
  output_list_amenities_count[i] <- length(output_list_amenities[[i]])
}

dummy_host_verifications_cols <- output_list_verifications[[which.max(output_list_verifications_count)]]
dummy_host_verifications_df <- matrix(nrow = nrow(airbnb_data), ncol = length(dummy_host_verifications_cols)) %>% as.data.frame()
colnames(dummy_host_verifications_df) <- dummy_host_verifications_cols
dummy_amenities_cols <- output_list_amenities[[which.max(output_list_amenities_count)]]
dummy_amenities_cols
dummy_amenities_df <- matrix(nrow = nrow(airbnb_data), ncol = length(dummy_amenities_cols)) %>% as.data.frame()
colnames(dummy_amenities_df) <- dummy_amenities_cols

airbnb_data <- cbind(airbnb_data, dummy_host_verifications_df, dummy_amenities_df)

for(l in 1:length(output_list_verifications)){
  for(j in 82:84){
    if(colnames(airbnb_data)[j] %in% output_list_verifications[[l]]){
      airbnb_data[l, j] <- "Yes"
    } else{
      airbnb_data[l, j] <- "No"
    }
  }
}

for(l in 1:length(output_list_amenities)){
  for(j in 85:ncol(airbnb_data)){
    if(colnames(airbnb_data)[j] %in% output_list_amenities[[l]]){
      airbnb_data[l, j] <- "Yes"
    } else{
      airbnb_data[l, j] <- "No"
    }
  }
}

for(j in 82:ncol(airbnb_data)){
  for(i in 1:nrow(airbnb_data)){
    if(is.na(airbnb_data[i, j]) == TRUE){
      airbnb_data[i, j] <- "No"
    } 
  }
  # Convert the new dummy columns to factors
  airbnb_data[, j] <- factor(airbnb_data[, j], levels = c("Yes", "No"))
}

factor_cols <- c("instant_bookable", "host_has_profile_pic", "host_identity_verified",
                 "host_is_superhost", "room_type", "property_type", "host_response_time",
                 "host_listings_count", "host_total_listings_count", "neighbourhood_cleansed",
                 "has_availability")

numeric_cols <- c("accommodates", "host_listings_count", "host_total_listings_count", "beds",
                  "minimum_nights", "maximum_nights", "minimum_minimum_nights", "maximum_minimum_nights",
                  "minimum_maximum_nights", "maximum_maximum_nights", "host_length", "since_first_review",
                  "since_last_review", "number_of_reviews", "availability_365", "availability_90",
                  "availability_60", "availability_30", "calculated_host_listings_count")

# Apply the transformations
airbnb_data <- airbnb_data %>% 
  mutate(
    across(all_of(factor_cols), as.factor),
    across(all_of(numeric_cols), as.numeric)
  )

airbnb_data <- airbnb_data |>
  select(-c(id, host_verifications, neighbourhood_group_cleansed, first_review, amenities, number_of_reviews_ltm, number_of_reviews_l30d, last_review, calculated_host_listings_count_entire_homes, calculated_host_listings_count_private_rooms, calculated_host_listings_count_shared_rooms, listing_url, host_picture_url, calendar_updated, calendar_last_scraped, description, neighborhood_overview, neighbourhood, host_neighbourhood, neighbourhood_group_cleansed, picture_url, host_id, host_url, host_thumbnail_url, host_name, host_since, host_location, host_about, scrape_id, last_scraped, license, price, source, name, bathrooms_text))

# Remove rows with missing values
airbnb_data <- airbnb_data[complete.cases(airbnb_data),]

# Identify numeric and factor variables
numeric_vars <- names(airbnb_data)[sapply(airbnb_data, class) == 'numeric']
factor_vars <- names(airbnb_data)[sapply(airbnb_data, class) == 'factor']

# Create new data frames containing only numeric and factor variables
df_numeric <- airbnb_data[, numeric_vars]
df_factor <- airbnb_data[, factor_vars]

# Remove specific numeric columns that are not needed
df_numeric <- df_numeric |> 
  select(-c("minimum_minimum_nights","maximum_minimum_nights", 
            "minimum_maximum_nights", "maximum_maximum_nights",
            "minimum_nights_avg_ntm","maximum_nights_avg_ntm" ))

# Combine the numeric and factor data frames to create the final airbnb_data (4238 x 135)
airbnb_data <- cbind(df_numeric, df_factor)

library(geojsonio)
library(leaflet)

nb_geo <- geojson_read('http://data.insideairbnb.com/italy/campania/naples/2023-06-21/visualisations/neighbourhoods.geojson', what = 'sp')

nb_geo@data$neighbourhood = as.factor(nb_geo@data$neighbourhood)

night_neighbourhood <- airbnb_data %>% group_by(neighbourhood_cleansed) %>% summarize(avg_night_price = mean(price_per_guest)) %>% arrange(desc(avg_night_price))

ggplot(night_neighbourhood[1:10, ], aes(x = reorder(neighbourhood_cleansed, avg_night_price),  y = avg_night_price, 
                                        fill = avg_night_price)) + 
  geom_bar(stat = "identity") + ggtitle("Top 10 most expensive neighbourhoods") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + scale_y_continuous(labels = function(x) paste0("$", x)) +
  xlab("") + ylab("Airbnb Price per Night") + coord_flip() + theme(legend.position = "none") + scale_fill_gradient(low = "yellow", high = "red")

colnames(night_neighbourhood)[1] <- "neighbourhood"
nb_geo@data <- left_join(nb_geo@data, night_neighbourhood[, 1:2]) %>% as.data.frame()

for(i in 1:nrow(nb_geo@data)){
  if(is.na(nb_geo@data[i, "avg_night_price"]) == TRUE){
    nb_geo@data[i, 3] <- mean(nb_geo@data[complete.cases(nb_geo@data), 3])
  } else if(is.na(nb_geo@data[i, "avg_night_price"]) == FALSE){
  }
}

pal <- colorNumeric(
  palette = "YlGnBu",
  domain = nb_geo@data$avg_night_price
)

price_per_neighbourhood <- leaflet(nb_geo) %>%
  addTiles() %>% setView(lng = 14.305573, lat = 40.853294, zoom = 10.5) %>%
  addPolygons(stroke = TRUE, fillColor = ~ pal(avg_night_price), fillOpacity = 0.8,
              highlight = highlightOptions(weight = 2,
                                           color = ~ pal(avg_night_price), 
                                           fillOpacity = 1,
                                           bringToFront = TRUE),
              label = ~neighbourhood,
              smoothFactor = 0.2,
              popup = ~ paste(paste(neighbourhood,":"), "<br/>","<b/>", paste("Avg Night Price: ", "$", round(avg_night_price)))) %>%
  addLegend("bottomright", pal = pal, values = ~avg_night_price, opacity = 1.0, 
            title = "Average Airbnb Night Price",
            labFormat = labelFormat(prefix = "$"), na.label="")
price_per_neighbourhood


# Clustering

library(cluster)
library(fpc)
gower_dist <- daisy(airbnb_data, metric = "gower")

pam_fit <- pamk(gower_dist, krange = 2:10, criterion = "asw")

library(clustMixType)

kpres2 <- kproto(cbind(df_numeric, df_factor), 2, nstart = 1,verbose = FALSE)
kpres3 <- kproto(cbind(df_numeric, df_factor), 3, nstart = 1,verbose = FALSE)
kpres4 <- kproto(cbind(df_numeric, df_factor), 4, nstart = 1,verbose = FALSE)
kpres5 <- kproto(cbind(df_numeric, df_factor), 5, nstart = 1,verbose = FALSE)
kpres6 <- kproto(cbind(df_numeric, df_factor), 6, nstart = 1,verbose = FALSE)

print(table(pam_fit$pamobject$clustering, kpres2$cluster))

require(mclust)
adjustedRandIndex(pam_fit$pamobject$clustering, kpres2$cluster)

library(kmed)
airbnb_dist <- distmix(airbnb_data, method = "ahmad", idnum = 1:32, idcat = 33:103)

#run the sfkm algorihtm on airbnb_dist
(simplekm <- skm(airbnb_dist, ncluster = 3, seeding = 50))

#calculate silhouette of the K-medoids result of airbnb data set 
silairbnb <- sil(airbnb_dist, simplekm$medoid, simplekm$cluster, title = "Silhouette plot of Airbnb data set")

barplotnum(df_numeric[,1:10], simplekm$cluster, alpha = 0.05)

barplotnum(df_numeric[,11:20], simplekm$cluster, alpha = 0.05)

barplotnum(df_numeric[,21:32], simplekm$cluster, alpha = 0.05)

adjustedRandIndex(simplekm$cluster, kpres3$cluster)

#cluster sizes
print(table(kpres3$cluster))

c1 <- cbind(df_numeric, df_factor) %>% 
  filter(simplekm$cluster == 1)

c2 <- cbind(df_numeric, df_factor) %>% 
  filter(simplekm$cluster == 2) 

c3 <- cbind(df_numeric, df_factor) %>% 
  filter(simplekm$cluster == 3)

library(leaflet)
leaflet() %>% setView(lng = 14.305573, lat = 40.853294, zoom = 10) %>%
  addTiles() %>%
  addPolygons(data = nb_geo, color = "#444444", weight = 2, opacity = 1) %>%
  addCircleMarkers(  lng = c1$longitude,
                     lat = c1$latitude,
                     radius = 2,
                     stroke = FALSE,
                     color = "blue",
                     fillOpacity = 0.5,
                     group = "c1"
  ) %>%
  addCircleMarkers(  lng = c2$longitude,
                     lat = c2$latitude,
                     radius = 3,
                     stroke = FALSE,
                     color = "green",
                     fillOpacity = 0.5,
                     group = "c2"
  )%>%
  addCircleMarkers(  lng = c3$longitude,
                     lat = c3$latitude,
                     radius = 3,
                     stroke = FALSE,
                     color = "red",
                     fillOpacity = 0.5,
                     group = "c3"
  )


# Describe the clusters

library(FactoMineR)

desc_clus <- catdes(cbind(airbnb_data,as.factor(simplekm$cluster)),136)
head(desc_clus$quanti$`1`)
head(desc_clus$category$`1`)

head(desc_clus$quanti$`2`)
head(desc_clus$category$`2`)
