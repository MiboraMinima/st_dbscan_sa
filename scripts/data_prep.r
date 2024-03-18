library(tidyverse)
library(summarytools)
library(happign)
library(sf)
library(reshape)
library(maptiles)
library(ggspatial)
library(patchwork)

source("scripts/assets/fun.r")

# ==============================================================================
# Import data
# ==============================================================================
train_sf <- read.csv("data/in/all/traces_groupe1.csv", sep = ";") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 2154)

# ==============================================================================
# Exploring data
# ==============================================================================
region <- st_read("data/elem_carto/region.shp")
bretagne <- subset(region, NOM_M == 'BRETAGNE')

#one plot with all tracks
ggplot() +
  geom_sf(data = bretagne, color = "black", fill = NA) +
  geom_sf(data = train_sf, color = "purple", shape=3)
  ###there are tracks in Rennes, one in Vezin-le-coquet, one in Concarneau

#one plot by track

  ##filter tracks
  unique_tracks <- unique(train_sf$id_trace)
  
  filtered_datasets <- list()
  
  for (track in unique_tracks) {
    filtered_datasets[[as.character(track)]] <- subset(train_sf, id_trace == track)
  }

  ##plots
plots <- lapply(filtered_datasets, function(train_sf) {
    ggplot(train_sf) +
      geom_sf(color = "purple", shape=3) +
      facet_wrap(~ id_trace) +
      theme_classic()
  })

plot_layout(wrap_plots(plots) +
              plot_annotation(title = "GPS tracks",
                              caption = "Group 1",
                              theme = theme(plot.title = element_text(size = 16, hjust = 0.5, vjust = 0.5, face = "bold"),
                                            plot.caption = element_text(size = 8, hjust = 1, face = "italic"))))
  
# ==============================================================================
# Handle outliers
# ==============================================================================
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

summary(train_sf$dmed) #dmed min = 0, dmed max = 106.6
range <- range(train_sf$dmed)

length(unique(train_sf$id_trace))
nrow(train_sf) #11595 points for 12 tracks in the dataset

  ##full histogram
  hist(train_sf$dmed, breaks=100, xlab ="dmed", main ="Distribution of dmed", xlim= range(train_sf$dmed))
  #most dmed are between 0 and 2
  
  ##truncated histograms (zoomed histograms on shorter dmed values)
  hist(train_sf$dmed, breaks=100, xlab ="dmed", main ="Distribution of dmed (max freq=150)", xlim= range(train_sf$dmed), ylim=c(0,150))
  hist(train_sf$dmed, breaks=100, xlab ="dmed", main ="Distribution of dmed (max freq=150)", xlim= c(0,60), ylim=c(0,150))
  hist(train_sf$dmed, breaks=100, xlab ="dmed", main ="Distribution of dmed (max freq=150)", xlim= c(0,40), ylim=c(0,150))
  ### those histograms can help choose a threshold for defining outlier. 
            
#Plots

  ##Change dmed into coordinates
  train_sf <- train_sf %>%
    mutate(xmed = X - dmed,
           ymed = Y - dmed)

  ##update filters by tracks with medians coordinates
  for (track in unique_tracks) {
    filtered_datasets[[as.character(track)]] <- subset(train_sf, id_trace == track)
  }

plots <- lapply(filtered_datasets, function(train_sf) {
  ggplot(train_sf) +
    geom_sf(color = "purple", shape=3) +
    geom_point(data = train_sf, aes(x = xmed, y = ymed), color = "green4", shape = 1) +
    facet_wrap(~ id_trace) +
    theme_classic() +
    annotation_scale(location = "br", height = unit(0.10, "cm"))+
    theme(legend.position = "none", axis.title = element_blank())
})

plot_layout(wrap_plots(plots) +
              plot_annotation(title = "GPS tracks (purple) and Medians (green)",
                              caption = "Group 1",
                              theme = theme(plot.title = element_text(size = 16, hjust = 0.5, vjust = 0.5, face = "bold"),
                                            plot.caption = element_text(size = 8, hjust = 1, face = "italic"),
                                            legend.position = "bottom")))

# Find a threshold 
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

  ##mark points that are outliers
  train_sf$is_outlier <- apply(st_intersects(train_sf, df_outliers), 1, function(x) any(x))

  
  ##plot outliers
  outlier <- unique(train_sf[train_sf$is_outlier == TRUE,])
  unique_tracks <- unique(outlier$id_trace)
  filtered_outlier <- list()
  
  for (track in unique_tracks) {
    filtered_outlier[[as.character(track)]] <- subset(outlier, id_trace == track)
  }
  
  plots <- lapply(filtered_outlier, function(outlier) {
    ggplot(outlier) +
      geom_sf(data = outlier, color = "red", shape =19) +
      facet_wrap(~ id_trace) +
      theme_classic() +
      annotation_scale(location = "br", height = unit(0.10, "cm")) +
      theme(legend.position = "none", axis.title = element_blank())
  })

  plot_layout(wrap_plots(plots, widths = rep(1, length(plots)), heights = rep(1, length(plots))) +
                plot_annotation(title = "Outliers",
                                caption = "Group 1",
                                theme = theme(plot.title = element_text(size = 16, hjust = 0.5, vjust = 0.5, face = "bold"),
                                              plot.caption = element_text(size = 8, hjust = 1, face = "italic"),
                                              legend.position = "bottom")))
  
# Show how many pings have been removed
n_rem <- nrow(outlier)
pct_rem <- (n_rem / nrow(train_sf)) * 100
sprintf(
  "%s (%s%% of total) pings removed using 12 as threshold for each track",
  n_rem, round(pct_rem,2)
)

# ==============================================================================
# Clean time
# ==============================================================================
# NOTE: We floor millisecond to last second to remove unwanted data such as:
# [1] "2024-03-06 09:36:07.999000+00:00" [2] "2024-03-06 09:36:07+00:00"
# The GPS record at t0 a time superior to the one of t+1. So, we (1) floor to
# last seconds then (2) we removes duplicates.

df_clean$time <- floor_date(as_datetime(df_clean$time), unit = "second")

# Remove duplicated based on time for each track
d_rem <- df_clean %>%
  group_by(id_trace) %>%
  group_modify(~ {
    .x %>%
      distinct(.x$time, .keep_all = TRUE)
  })

sprintf(
  "Removing %s duplicates data based on time (%.2f %% of the sample)",
  nrow(df_clean) - nrow(d_rem),
  ((nrow(df_clean) - nrow(d_rem)) / nrow(df_clean)) * 100
)

write.csv(df_clean, "data/out/traces_groupe1_clean.csv")

