library(tidyverse)
library(summarytools)
library(happign)
library(sf)
library(reshape)

source("scripts/fun.r")

# ==============================================================================
# Import data
# ==============================================================================
train_sf <- read.csv("data/in/all/traces_groupe1.csv", sep = ";") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 2154)

# Loading Rennes com to filter outliers
data("cog_2023")
insee_code <- cog_2023[cog_2023$LIBELLE == "Rennes", "COM"]
com <- get_apicarto_cadastre(insee_code, type = "commune")
com <- st_transform(com, 2154)

# ==============================================================================
# Handle outliers
# ==============================================================================
# Outliers: Concarneau
# ------------------------------------------------------------------------------
ggplot() +
  geom_sf(data = com) +
  geom_sf(data = train_sf)

# Get only pings in Rennes
train_sf$inter <- st_intersects(train_sf, com, sparse = FALSE)

# Checkout
train_sf %>%
  filter(inter == "TRUE") %>%
  ggplot() +
  geom_sf()

# Filter
train_clean <- train_sf %>%
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  ) %>%
  filter(inter == "TRUE") %>%
  as_tibble() %>%
  select(-c(inter, geometry))

# ------------------------------------------------------------------------------
# Outliers identification using median filter
# ------------------------------------------------------------------------------
# NOTE: w = 7 so, time window = [i-7;i+7] (i.e. 7 seconds on either side of i).
# Default value that seems reasonable. I don't see any reason to change it
# here.

# Parameters
windows_size <- 7

# Apply median filter
train_clean[, "dmed"] <- median_filter(
  train_clean$lon,
  train_clean$lat,
  w = windows_size
)

# Find threshold for each track. Using percentile. !!Arbitrary 95e pct!!
track_list <- unique(train_clean$id_trace)

clean_track <- list()
th_list <- list()
for (i in seq_along(track_list)) {
  lab_id <- track_list[i]

  # Compute threshold
  c_track <- train_clean[train_clean$id_trace == lab_id, ]
  q95 <- quantile(c_track$dmed, probs = c(.95))

  # Store data
  clean_track[[lab_id]] <- c_track[c_track$dmed < q95, ]
  th_list[[lab_id]] <- q95
}

# Stats desc on thresholds -> HIGH variability (very interesting for a critical
# analysis)
summarytools::descr(unlist(th_list))

# Merge all df
df_clean <- reshape::merge_all(clean_track)

# Show how many pings has been removed
n_rem <- nrow(train_clean) - nrow(df_clean)
pct_rem <- (n_rem / nrow(df_clean)) * 100
sprintf(
  "%s (%s %% of total) pings removed using 95e pct for each track",
  n_rem, round(pct_rem)
)

write.csv(df_clean, "data/out/traces_groupe1_clean.csv")
