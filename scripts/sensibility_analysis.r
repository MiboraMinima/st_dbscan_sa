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
  minpts = seq(5, 30, 5),
  eps    = seq(1, 5, 0.75),
  eps2   = seq(30, 180, 30),
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
  lon <- df_clean[df_clean$id_trace == df_store[i, "id"], "lon"]
  lat <- df_clean[df_clean$id_trace == df_store[i, "id"], "lat"]
  time <- as_datetime(df_clean[df_clean$id_trace == df_store[i, "id"], "time"])

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

# ==============================================================================
# Checkout the results
# ==============================================================================
summarytools::descr(df_store$f_measure)

# Overall best results
df_store %>%
  arrange(desc(f_measure)) %>%
  head()

# Show best params (mean)
df_store %>%
  group_by(minpts, eps, eps2) %>%
  summarize(mean_f = mean(f_measure)) %>%
  arrange(desc(mean_f)) %>%
  head()
