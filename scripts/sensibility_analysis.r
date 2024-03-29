library(tidyverse)
library(summarytools)
library(reshape)
library(foreach)

source("scripts/assets/stdbscan.r")
source("scripts/assets/fun.r")

# ==============================================================================
# Import data
# ==============================================================================
df_clean <- read.csv("data/out/traces_groupe1_clean.csv")

# Convert id trace to factor
df_clean$id_trace <- as_factor(df_clean$id_trace)

# ==============================================================================
# Sensibility analysis
# ==============================================================================
# Generate parameters (not that much)
df_store <- expand.grid(
  minpts = seq(5, 60, 5),
  eps    = seq(1, 15, 0.75),
  eps2   = seq(30, 1000, 200),
  id     = unique(df_clean$id)
)

# NOTE: Many parameters are used, utilizing parallelization (i.e. multiple CPUs
# at the same time). Configure this properly (the default number of CPUs used
# is n-1).
n_cores <- parallel::detectCores() - 1

# Create the cluster for parallelization
my_cluster <- parallel::makeCluster(
  n_cores,
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my_cluster)

out <- foreach(
  i = seq_len(nrow(df_store)),
  .combine = "rbind",
  .packages = c("dplyr", "lubridate")
) %dopar% {
  # Retrieving params
  eps <- df_store[i, "eps"]
  eps2 <- df_store[i, "eps2"]
  minpts <- df_store[i, "minpts"]
  id <- df_store[i, "id"]
  
  # Get the parameters of the track.
  lon <- df_clean[df_clean$id_trace == id, "lon"]
  # Check if the track is empty, if so, next track and return 0 (I know, weird
  # syntax ; it's the foreach loop)
  if (length(lon) <= 1) (return(0))
  lat <- df_clean[df_clean$id_trace == id, "lat"]
  time <- to_num(as_datetime(df_clean[df_clean$id_trace == id, "time"]))
  
  # running st-dbscan
  res <- stdbscan(lon, lat, time, eps, eps2, minpts)
  
  # Add result to df
  df_comp <- df_clean %>%
    filter(id_trace == df_store[i, "id"]) %>%
    select(arret) %>%
    mutate(res = res$cluster)
  
  # Compute vp, vn, fpn and fn
  fdf <- df_comp %>%
    mutate(type_error = case_when(
      arret == "VRAI" & res != 0 ~ "vp",
      arret == "FAUX" & res == 0 ~ "vn",
      arret == "VRAI" & res == 0 ~ "fp",
      arret == "FAUX" & res != 0 ~ "fn"
    ))
  
  # Compute number of vp, fp and fn
  vp <- length(fdf[fdf["type_error"] == "vp"])
  fp <- length(fdf[fdf["type_error"] == "fp"])
  fn <- length(fdf[fdf["type_error"] == "fn"])
  
  # compute f-measure
  f_measure(vp, fp, fn)
}
# Close parallelization
parallel::stopCluster(cl = my_cluster)

# Append result
df_store$f_measure <- out[, 1]

write.csv(df_store, "data/out/params_fmeasures.csv")

# ==============================================================================
# Checkout the results
# ==============================================================================
summarytools::descr(df_store$f_measure)

# Overall best results
df_store %>%
  arrange(desc(f_measure)) %>%
  head()

# Show best params (mean)
df_store2 <- df_store %>%
  group_by(minpts, eps, eps2) %>%
  summarize(mean_f = mean(f_measure), med_f = median(f_measure), sd_f = sd(f_measure), min_f = min(f_measure)) %>%
  arrange(desc(mean_f))

ggplot(df_store2, aes(x=as.factor(minpts), y = mean_f))+
  geom_boxplot()

