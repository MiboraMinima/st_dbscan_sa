library(tidyverse)
library(summarytools)
library(happign)
library(sf)
library(reshape)
library(maptiles)
library(ggspatial)

source("scripts/assets/fun.r")

# ==============================================================================
# Import data
# ==============================================================================
train_sf <- read.csv("data/in/all/traces_groupe1.csv", sep = ";") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 2154)

# # Loading Rennes com to filter outliers
# data("cog_2023")
# insee_code <- cog_2023[cog_2023$LIBELLE == "Rennes", "COM"]
# com <- get_apicarto_cadastre(insee_code, type = "commune")
# com <- st_transform(com, 2154)

# ==============================================================================
# Exploring data
# ==============================================================================
region <- st_read("data/elem_carto/region.shp")
bretagne <- subset(region, NOM_M == 'BRETAGNE')

#one plot with all tracks
ggplot() +
  geom_sf(data = bretagne, color = "black", fill = NA) +
  geom_sf(data = train_sf, color = "red")
  ###there are tracks in Rennes, one in Vezin-le-coquet, one in Concarneau

#one plot by track

  ##filter tracks
  unique_tracks <- unique(train_sf$id_trace)
  
  filtered_datasets <- list()
  
  for (track in unique_tracks) {
    filtered_datasets[[as.character(track)]] <- subset(train_sf, id_trace == track)
  }

  ##plots
  lapply(filtered_datasets, function(train_sf) {
    ggplot(train_sf) +
      geom_sf(color = "red") +
      facet_wrap(~ id_trace)
  })

# ==============================================================================
# Handle outliers
# ==============================================================================

  
# Outliers: Concarneau
# ------------------------------------------------------------------------------

# Get only pings in Rennes
#train_sf$inter <- st_intersects(train_sf, com, sparse = FALSE)

# Checkout
# train_sf %>%
#   filter(inter == "TRUE") %>%
#   ggplot() +
#   geom_sf()

# Filter
# train_clean <- train_sf %>%
#   mutate(
#     lon = st_coordinates(geometry)[, 1],
#     lat = st_coordinates(geometry)[, 2]
#   ) %>%
#   filter(inter == "TRUE") %>%
#   as_tibble() %>%
#   select(-c(inter, geometry))

# ------------------------------------------------------------------------------
# Outliers identification using median filter
# ------------------------------------------------------------------------------
  # NOTE: w = 7 so, time window = [i-7;i+7] (i.e. 7 seconds on either side of i).
  # Default value that seems reasonable. I don't see any reason to change it
  # here.
windows_size <- 7 #Parameter
train_sf <- cbind(train_sf, st_coordinates(train_sf))

# Apply median filter
train_sf[, "dmed"] <- median_filter(
  train_sf$X,
  train_sf$Y,
  w = windows_size
)

#Distribution & statistics
summary(train_sf$dmed)
range <- range(train_sf$dmed)
nrow(train_sf)

  ##full histogram
  hist(train_sf$dmed, breaks=50, xlab ="dmed", main ="Distribution of dmed", xlim= range(train_sf$dmed))
  
  ##truncated histograms
  hist(train_sf$dmed, breaks=50, xlab ="dmed", main ="Distribution of dmed (max freq=150)", xlim= range(train_sf$dmed), ylim=c(0,150))
  hist(train_sf$dmed, breaks=50, xlab ="dmed", main ="Distribution of dmed (max freq=150)", xlim= c(0,60), ylim=c(0,150))
  
  ### those histograms can help choose a threshold for defining outlier. 
  ### it seems there a break at dmed = 12
            
#Plots

  ##Change dmed into coordinates
  train_sf <- train_sf %>%
    mutate(xmed = X - dmed,
           ymed = Y - dmed)

  ##update filters by tracks with medians coordinates
  for (track in unique_tracks) {
    filtered_datasets[[as.character(track)]] <- subset(train_sf, id_trace == track)
  }

lapply(filtered_datasets, function(train_sf) {
  ggplot(train_sf) +
    geom_sf(color = "red", shape=4) +
    geom_point(data = train_sf, aes(x = xmed, y = ymed), color = "blue") +
    facet_wrap(~ id_trace)
})

# Find a threshold for each track. Using percentile. !!Arbitrary 95e pct!!

#METHODE SEUIL FIXE
threshold <- 12

clean_track <- list() #we store only points that are inside the fixed thresholds
outliers <- list() #points considered as outliers

for (i in seq_along(unique_tracks)) {
  lab_id <- unique_tracks[i]
  
  # Compute threshold & Store data
  c_track <- train_sf[train_sf$id_trace == lab_id, ]
  clean_track[[lab_id]] <- c_track[c_track$dmed < threshold, ]
  outliers[[lab_id]] <- c_track[c_track$dmed > threshold, ]
}

# Merge
#df_clean <- reshape::merge_all(clean_track) #fonctionne pas je ne sais pas pourquoi
df_clean <- do.call(rbind, clean_track)
df_outliers <- do.call(rbind, outliers)

#Plot - A FAIRE
train_sf$is_outlier <- apply(st_intersects(train_sf, df_outliers), 1, function(x) any(x))

ggplot(train_sf) +
  geom_sf(color = "green4", shape=8) +
  geom_point(data = train_sf, aes(x = xmed, y = ymed), color = "blue4", shape = 19) +
  geom_point(data = train_sf[train_sf$is_outlier == TRUE,], aes(x = xmed, y = ymed), color = "red4")


# je ne sais pas pourquoi je n'arrive pas à rendre visible les outliers ici :
outlier <- train_sf[train_sf$is_outlier == TRUE,]
unique(outlier$id_trace)

lapply(filtered_datasets, function(train_sf) {
  ggplot(train_sf) +
    geom_sf(color = "green4", shape=8) +
    geom_point(data = train_sf, aes(x = xmed, y = ymed), color = "blue4", shape = 19) +
    if (any(train_sf$is_outlier)) {
      geom_point(data = train_sf[train_sf$is_outlier, ], aes(x = xmed, y = ymed), color = "red4")
    } else {
      NULL
    } +
    facet_wrap(~ id_trace)
})


# Show how many pings have been removed
n_rem <- nrow(train_sf) - nrow(df_clean)
pct_rem <- (n_rem / nrow(df_clean)) * 100
sprintf(
  "%s (%s%% of total) pings removed using 12 as threshold for each track",
  n_rem, round(pct_rem,2)
)

write.csv(df_clean, "data/out/traces_groupe1_clean.csv")