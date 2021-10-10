rm (list =  ls(all = T))

# Librerie
if (!require(ggplot2)) install.packages('ggplot2'); library('ggplot2')
if (!require(dplyr, warn.conflicts = FALSE)) install.packages('dplyr'); library('dplyr')
if (!require(dtw)) install.packages('dtw'); library('dtw')
if (!require(formattable)) install.packages('formattable'); library('formattable')
if (!require(readr)) install.packages('readr'); library('readr')
if (!require(tidyr)) install.packages('tidyr'); library('tidyr')
if (!require(lubridate)) install.packages('lubridate'); library('lubridate')
options(dplyr.summarise.inform = FALSE)
options(scipen=999)

# Pacchetto creato
library(dtwweight)
set.seed('123')

# Variabili
seasons             <- c('201830', '201930', '202030', '202130')
target              <- as.character('202130')
weight              <- list(1, 0, 0)
names(weight)       <- c('201830', '201930', '202130')
weight              <- weight %>% unlist()

# Parametri da modificare, consiglio di modificare le current_dates con i valorei -28, -21, -14, -7, 0
current_dates       <- -7
threshold_alignment <- 100000
n_week_euclidean    <- 3


# Lettura del dato
df_sku <- read_delim(file = "./Data/DATA_THESIS.zip" ,
                   delim=",")
df_sku$season <- as.character(df_sku$season)
backup_df <- df_sku


df_lag <- df_sku %>% calculate_lag_data(season_target = target,
                                        seasons = seasons,
                                        current_dates = current_dates,
                                        threshold = threshold_alignment)

df_sku <- df_sku %>% traslate_curves_covid19(data_lag = df_lag,
                                             drop_orders = TRUE)


weight <- df_sku %>% create_dynamic_weight(season_target = target,
                                           seasons = seasons,
                                           current_dates = current_dates,
                                           n_week_euclidean = n_week_euclidean,
                                           weight = weight)


plot_current(df_sku, current_dates,target,flag_allineamento=TRUE)
plot_current(backup_df, current_dates,target,flag_allineamento=FALSE)

plot_weight(weight)

rm(list = ls())
