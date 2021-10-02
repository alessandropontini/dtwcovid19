rm (list =  ls(all = T))

source(file = "./Funzioni/fixcovid19.r")
set.seed('123')

# variable
seasons             <- c('201830', '201930', '202030', '202130')
target              <- as.character('202130')
weight              <- list(1, 0, 0)
names(weight)       <- c('201830', '201930', '202130')
weight              <- weight %>% unlist()
current_dates_iter  <- -5
threshold_alignment <- 100000
n_week_euclidean    <- 3


df_sku <- read_csv2("./DATA_THESIS.zip")
df_sku$season <- as.character(df_sku$season)
backup_df <- df_sku


df_lag <- df_sku %>% calculate_lag_data(season_target = target,
                                        seasons = seasons,
                                        current_dates = current_dates_iter,
                                        threshold = threshold_alignment)

df_sku <- df_sku %>% traslate_curves_covid19(data_lag = df_lag,
                                             drop_orders = TRUE)


weight <- df_sku %>% create_dynamic_weight(season_target = target,
                                           seasons = seasons,
                                           current_dates = current_dates_iter,
                                           n_week_euclidean = n_week_euclidean,
                                           weight = weight)


plot_current(df_sku, current_dates_iter, 'Curve giornaliere ordinato - allineate')
plot_current(backup_df, current_dates_iter, 'Curve giornaliere ordinato - non allineate')


plot_current_week(df_sku, current_dates_iter, 'Curve settimanali ordinato - allineate')
plot_current_week(backup_df, current_dates_iter, 'Curve settimanali ordinato - non allineate')

rm(list = ls())
