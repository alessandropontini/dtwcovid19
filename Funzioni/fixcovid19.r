require(dplyr, warn.conflicts = FALSE)
require(tidyr)
require(purrr)
require(furrr)
require(readr)
require(lubridate)
require(ggplot2)
require(dtw)
require(formattable)
options(dplyr.summarise.inform = FALSE)

#' Prepare Dataframe
#'
#' @description This functions transform daily SKU orders by wholesaler in weekly data.
#' This is an internal function of \code{\link{create_dynamic_weight}}
#' and \code{\link{calculate_lag_data}}. It is not supposed to be called directly
#'
#' @param data          Dataframe. See \code{details}
#' @param season_target Character. Target Season.
#' @param seasons       Character Vector. A vector with the previous Seasons.
#'
#' @details The input \code{data} has to be a dataframe (or a tibble) with at least the following columns:
#'
#' \describe{
#'   \item{client}{\code{character}. Clients Names}
#'   \item{date}{\code{Date}. Date of the order}
#'   \item{order}{\code{character}. Code of the order}
#'   \item{start}{\code{Date}. Start date of the campaign}
#'   \item{end}{\code{Date}. End date of the campaign}
#'   \item{season}{\code{character}. Code of the season}
#'   \item{gender_3bs}{\code{character}. Details for man or women}
#'   \item{category}{\code{character}. Details of the different types of category}
#'   \item{style_code}{\code{character}. Code of the style}
#'   \item{fabric_code}{\code{character}. Code of the fabric}
#'   \item{color_code}{\code{character}. Code of the color}
#'   \item{qty}{\code{numeric}. Quantity of the order}
#' }
#'
#' @return Returns a new dataframe and is composed by:
#'
#' \describe{
#'   \item{season}{\code{character} class. A tibble with the season detail}
#'   \item{week_count}{\code{numeric} class. A tibble with the week difference from the end date}
#'   \item{qty}{\code{numeric} class. A tibble with the qty by season-date}
#'   \item{qty_cumsum}{\code{numeric} class. A tibble with the cumulative qty by season-date}
#'
#' }
#'
#' @importFrom dplyr %>% arrange mutate select filter group_by bind_rows summarise ungroup arrange
#' @importFrom tidyr drop_na
#' @export
#'
prepare_data_weekly <- function(data, season_target, seasons){
  # 1. pre processing data --------------------------------------------------
  #data <- df_sku
  data <- data %>% filter(season %in% seasons)
  data <- data %>% drop_na(qty)

  # controll dates
  if(class(data$date) != "Date" | class(data$end) != "Date"){

    data <- data %>% mutate(date = ymd(date),
                            start = ymd(start),
                            end = ymd(end))

  }

  # USELESS NOW I HAVE ALL DATES
  # data <- data %>% drop_na(end)

  # this modification of 202130 is to get transform the end of 202130 in 2020-07-24 DONE BEFORE
  # if(season_target == "202130"){
  #
  #   data_ref <- data %>% filter(season == season_target)
  #   data <- data %>% filter(season != season_target)
  #
  #   data_ref <- data_ref %>% mutate(end = case_when(season == season_target ~ as.Date.character("2020-07-24")))
  #   data <- data %>% bind_rows(data_ref)
  #
  # }

  # convert data in week
  data <- data %>%
    mutate(week_count = ceiling(as.numeric(date - end, "days")/7)) %>%
    group_by(season, week_count) %>%
    summarise(qty = sum(qty)) %>%
    arrange(season, week_count) %>%
    ungroup()

  # complete from start
  data <- data %>%
    complete(season, week_count, fill = list(qty = 0))

  # create comulative sum
  data <- data %>% arrange(season, week_count) %>%
    group_by(season) %>%
    mutate(qty_cumsum = cumsum(qty))%>%
    ungroup()



  # 2. return ---------------------------------------------------------------
  return(data)
}

#' Slice SKU weekly Dataframe
#'
#' @description This function take SKU modified Dataframe and make a slice of the data.
#' This is an internal function of \code{\link{create_dynamic_weight}}
#' and \code{\link{calculate_lag_data}}. It is not supposed to be called directly
#'
#' @param data              Dataframe. See \code{details}
#' @param current_dates     Numeric Vector. Dates in which you want to launch the simulation. Preferable more than one.
#' @param season_target     Character. Target Season.
#'
#' @details The input \code{data} has to be a dataframe (or a tibble) with at least the following columns:
#'
#' \describe{
#'   \item{season}{\code{character} class. A tibble with the qty detail by client-season-order-date}
#'   \item{week_count}{\code{numeric} class. A tibble with the qty detail by season-order-date-sku}
#'   \item{qty}{\code{numeric} class. A tibble with the cumulative qty by season-date}
#'   \item{qty_cumsum}{\code{numeric} class. A tibble with the cumulative qty by season-date. These are not used for the simulation.}
#' }
#'
#' @return Returns the dataframe sliced.
#'
#' @importFrom dplyr filter %>% bind_rows select
#' @export
#'
slice_data <- function(data, current_dates, season_target){

  # 1. slice data -----------------------------------------------------------
  data_ref <- data %>% filter(season == season_target)
  data <- data %>% filter(season != season_target)

  # convert current_dates in week and slice the season target
  current_dates_week <- round(current_dates/7, 0)
  data_ref <- data_ref %>% filter(week_count <=  max(current_dates_week))
  data <- data %>% bind_rows(data_ref)

  week_reference <- data %>% filter(season == season_target) %>%
    filter(week_count == max(week_count)) %>%
    select(week_count) %>%
    as.double()

  # get just the 2 weeks after the reference week
  data <- data %>%  filter(week_count < week_reference + 3)

  # 2. return data ----------------------------------------------------------
  return(data)
}

#' Alignment Curves by Dynamic-Time-Warping
#'
#' @description This function is the core of the project. By an allignment if the curves by dtw package.
#' This is an internal function of  \code{\link{calculate_lag_data}}. It is not supposed to be called directly
#'
#' @param data          Dataframe. See \code{details}
#' @param season_target Character. Target Season.
#' @param seasons       Character Vector. A vector with the previous Seasons.
#' @param threshold     Numeric. How high want the accurate alignment. Less value more precision.
#'
#' @details The input \code{data} has to be a dataframe (or a tibble) with at least the following columns:
#'
#' \describe{
#'   \item{season}{\code{character} class. A tibble with the season detail}
#'   \item{week_count}{\code{numeric} class. A tibble with the week difference from the end date}
#'   \item{qty}{\code{numeric} class. A tibble with the qty by season-date}
#'   \item{qty_cumsum}{\code{numeric} class. A tibble with the cumulative qty by season-date.}
#' }
#'
#' @return Returns a new dataframe with the optimal week lag and is composed by:
#'
#' \describe{
#'   \item{season}{\code{character} class. A tibble with the season detail}
#'   \item{week_lag}{\code{numeric} class. A tibble with the week lag}
#' }
#'
#' @importFrom dplyr %>% filter add_row distinct pull slice select arrange
#' @importFrom dtw dtw
#' @importFrom tibble tibble
#'
dtw_align_curves <- function(data, season_target, seasons, threshold){

  # 1. variables ------------------------------------------------------------
  ref_ali_idx = numeric()
  que_ali_idx = numeric()
  lag_var = 0
  lag_data <- tibble(season = character(),
                     week_lag = numeric())

  # 2. reference + lag_data ---------------------------------------------------
  data_stagione_reference <- data %>% filter(season == season_target)

  min_week_reference <- as.integer(min(data_stagione_reference$week_count))
  qty_sum_reference <- as.vector(data_stagione_reference$qty_cumsum)
  index_reference <- as.vector(data_stagione_reference$week_count)
  lag_data <- lag_data %>% add_row(season = season_target, week_lag = 0)

  # 3. get query season -----------------------------------------------------
  season_query_list <- data %>% filter(season %in% seasons & season != season_target) %>% distinct(season) %>% pull(season)

  for(k in 1:length(season_query_list)){
    lag_var = 0
    data_stagione_query <- data %>% filter(season == season_query_list[k])
    min_week_query <- as.integer(min(data_stagione_query$week_count))


    qty_sum_query <- as.vector(data_stagione_query$qty_cumsum)
    index_query <- as.vector(data_stagione_query$week_count)

    alignment <- dtw(qty_sum_query,qty_sum_reference,
                     keep=TRUE)

    new_ref   <- qty_sum_reference[alignment$index2]
    new_query <- qty_sum_query[alignment$index1]

    # last time (first from the end) which the distance of adjusted series is lower than threshold_alignment
    check <-  max( which( abs(new_ref - new_query) <= threshold_alignment ) )

    if(is.finite(check)) {
      x <- alignment$index2[check]
      y <- alignment$index1[check]

      lag_var <- x - y
      lag_var <- min(max(lag_var, -3),3)
    } else {
      lag_var <- 0
    }

    lag_data <- lag_data %>% add_row(season = as.character(season_query_list[k]), week_lag = lag_var) %>% arrange(season)

  }

  # check seasons
  data_check_season <- tibble(season = character(),
                              week_lag_left = numeric())

  data_check_season <- data_check_season %>% add_row(season = seasons,
                                                     week_lag_left = 0)

  data_check_season <- data_check_season %>%
    left_join(lag_data, by='season') %>%
    mutate(week_lag = replace_na(week_lag, 0)) %>%
    mutate(week_lag_left = week_lag) %>%
    select(-week_lag) %>%
    rename(week_lag = week_lag_left)

  lag_data <- data_check_season
  # 5. return ---------------------------------------------------------------
  return(lag_data)
}

#' Calculate Lag Dataframe
#'
#' @description This function is the core of the Covid19 project. It works as showed
#'
#'   1. It prepares the data in weekly date and calculate the cumulative of the orders quantity in this function \code{\link{prepare_data_weekly}}.
#'
#'   2. It slices the seasons not season_target from the the first observation to the second observation after the last observation of the season_target season in this function \code{\link{slice_data}}.
#'
#'   3. It uses the `dtw` package to make a dynamic time warping between the season_target season and the others. Then calculate the optimal lag between them in this function \code{\link{dtw_align_curves}}.
#'
#'   4. It stores the lag data in Dataframe.
#'
#' @param data            Dataframe. See \code{details}
#' @param season_target   Character. Target Season.
#' @param seasons         Character Vector. A vector with the previous Seasons.
#' @param current_dates   Numeric Vector. Dates in which you want to launch the simulation. Preferable more than one.
#' @param threshold       Numeric. How high want the accurate alignment. Less value more precision.
#'
#' @details The input \code{data} has to be a dataframe (or a tibble) with at least the following columns:
#'
#' \describe{
#'   \item{client}{\code{character}. Clients Names}
#'   \item{date}{\code{Date}. Date of the order}
#'   \item{order}{\code{character}. Code of the order}
#'   \item{start}{\code{Date}. Start date of the campaign}
#'   \item{end}{\code{Date}. End date of the campaign}
#'   \item{season}{\code{character}. Code of the season}
#'   \item{gender_3bs}{\code{character}. Details for man or women}
#'   \item{category}{\code{character}. Details of the different types of category}
#'   \item{style_code}{\code{character}. Code of the style}
#'   \item{fabric_code}{\code{character}. Code of the fabric}
#'   \item{color_code}{\code{character}. Code of the color}
#'   \item{qty}{\code{numeric}. Quantity of the order}
#' }
#'
#' @return Returns a new dataframe with the optimal week lag and is composed by:
#'
#' \describe{
#'   \item{season}{\code{character} class. A tibble with the season detail}
#'   \item{week_lag}{\code{numeric} class. A tibble with the week lag}
#' }
#'
#' @importFrom dplyr %>%
#'
#' @export
#'
calculate_lag_data <- function(data, season_target, seasons, current_dates, threshold){

  # 0. check type input -----------------------------------------------------
  if(!is.data.frame(data)) stop("data must be a dataframe")
  if(!is.character(season_target)) stop("season_target must be a character")
  if(!is.vector(seasons)) stop("seasons must be a vector")
  if(!is.vector(current_dates)) stop("current_dates must be a vector")
  if(!is.numeric(threshold)) stop("threshold must be a number")

  # 1. functions ------------------------------------------------------------
  data <- data %>% prepare_data_weekly(season_target, seasons)

  data <- data %>% slice_data(current_dates, season_target)

  if(data %>% nrow() == 0){

    lag_data <- tibble(season = character(),
                       week_lag = numeric())

    lag_data <- lag_data %>% add_row(season = seasons,
                                     week_lag = 0)

    return(lag_data)

  }

  data <- data %>% dtw_align_curves(season_target, seasons, threshold)

  # 2. return ---------------------------------------------------------------
  return(data)
}

#' Traslate curves
#'
#' @description This functions translate the SKU orders curves with the lag calculate in \code{\link{calculate_lag_data}}.
#'
#' @param data        Dataframe. See \code{details}
#' @param data_lag    Dataframe. See \code{details}
#' @param drop_orders Logical. See \code{details}
#'
#' @details The input \code{data} has to be a dataframe (or a tibble) with at least the following columns:
#'
#' \describe{
#'   \item{client}{\code{character}. Clients Names}
#'   \item{date}{\code{Date}. Date of the order}
#'   \item{order}{\code{character}. Code of the order}
#'   \item{start}{\code{Date}. Start date of the campaign}
#'   \item{end}{\code{Date}. End date of the campaign}
#'   \item{season}{\code{character}. Code of the season}
#'   \item{gender_3bs}{\code{character}. Details for man or women}
#'   \item{category}{\code{character}. Details of the different types of category}
#'   \item{style_code}{\code{character}. Code of the style}
#'   \item{fabric_code}{\code{character}. Code of the fabric}
#'   \item{color_code}{\code{character}. Code of the color}
#'   \item{qty}{\code{numeric}. Quantity of the order}
#' }
#'
#' @details The input \code{data_lag} has to be a dataframe (or a tibble) with the following columns:
#'
#' \describe{
#'   \item{season}{\code{character} class. A tibble with the season detail}
#'   \item{week_lag}{\code{numeric} class. A tibble with the week lag}
#' }
#'
#' @details If Drop orders is TRUE then we drop orders that fall between the season end and the estimated lag.
#'
#' @return Returns SKU orders dataframe lagged.
#'
#' @importFrom dplyr %>% inner_join mutate select
#' @importFrom lubridate weeks
#'
#' @export
#'
traslate_curves_covid19 <- function(data, data_lag, drop_orders = FALSE){

  # 0. check type input -----------------------------------------------------
  if(!is.data.frame(data)) stop("data must be a dataframe")
  if(!is.data.frame(data_lag)) stop("data_lag must be a dataframe")

  data$season <- as.character(data$season)
  # 1. load libraries -------------------------------------------------------
  data <- data %>%
    inner_join(data_lag, by = 'season') %>%
    mutate(date = date + weeks(week_lag))

  # 2. drop orders ----------------------------------------------------------

  if(drop_orders){
    # drop orders in the lagged weeks
    data <- data %>%
      filter( date <= end | date >= end + weeks(week_lag))
  }

  # 3. return ---------------------------------------------------------------

  data <- data %>% select(-week_lag)

  return(data)
}

#' Create Weight
#'
#' @description This function create the weights. This is an internal function of \code{\link{create_dynamic_weight}}
#' and it is not supposed to be called directly
#'
#' @param data                Dataframe. See \code{details}
#' @param season_target       Character. Target Season.
#' @param seasons             Character Vector. A vector with the previous Seasons.
#' @param n_week_euclidean    Numeric. The number of observations we want to calculate from the last one.
#'
#'@details The input \code{data} has to be a dataframe (or a tibble) with at least the following columns:
#'
#' \describe{
#'   \item{season}{\code{character} class. A tibble with the season detail}
#'   \item{week_count}{\code{numeric} class. A tibble with the week difference from the end date}
#'   \item{qty}{\code{numeric} class. A tibble with the qty by season-date}
#'   \item{qty_cumsum}{\code{numeric} class. A tibble with the cumulative qty by season-date}
#' }
#'
#' @importFrom tibble tibble
#' @importFrom dplyr %>% filter select slice anti_join arrange row_number pull mutate distinct
#' @importFrom stats dist
#'
#' @return The output is a named numeric vector. Weights to apply for each season when calculating the probs.
#'
create_weight <- function(data, season_target, seasons, n_week_euclidean, weight){

  # 1. create data ----------------------------------------------------------
  data_total_distance <- tibble(
    season = character(),
    euclidean_distance = numeric()
  )

  season_ref <- season_target
  season_query_list <- data %>% filter(season %in% seasons & season != season_target) %>% distinct(season) %>% pull(season)

  data_reference <- data %>% filter(season == season_ref) %>% select(season, week_count, qty_cumsum)

  # error check
  if(nrow(data_reference) == 0){
    # stop("There are no observation to analyse for the season_target season.")
    warning(paste0("There are not enough observation to make the dynamic weights"," We are gonnna use the default\n\n"))

    return(weight)
  }

  if(nrow(data_reference) >=  n_week_euclidean){

    data_reference <- data_reference %>%
      slice(tail(row_number(), n_week_euclidean))

    data_reference <- data_reference[-nrow(data_reference),]
    flag <- TRUE

  } else if (nrow(data_reference) <  n_week_euclidean) {

    warning(paste0("There are not enough observation to make this cut of ", n_week_euclidean, " values.", " We are gonna use ", nrow(data_reference), " values\n\n"))

    n_week_euclidean <- nrow(data_reference) - 1

    if(n_week_euclidean == 0){
      # stop("There are no observation to analyse for the season_target season.")
      warning(paste0("There are not enough observation to make the dynamic weights"," We are gonnna use the default\n\n"))

      return(weight)
    }

  }

  max_value_reference <- data %>% filter(season == season_ref) %>%
    filter(week_count == max(week_count)) %>%
    select(week_count) %>%
    as.numeric()

  max_value_reference <- max_value_reference - 1

  # for each seasons get the vector for the euclidean distance
  for(i in 1:length(season_query_list)){

    data_query <- data %>% filter(season == season_query_list[i] & week_count <= max_value_reference) %>%
      select(season, week_count, qty_cumsum)%>%
      slice(tail(row_number(), n_week_euclidean))

    # check if data reference > data query we need to find the number of week of difference
    if(nrow(data_reference) > nrow(data_query)){

      difference_date <- anti_join(data_reference, data_query, by = "week_count") %>% pull(week_count)

      # put to 0 the other that do not exist in the in the query
      for(j in 1:length(difference_date)){
        data_query <- data_query %>%
          add_row(season = season_query_list[i],
                  week_count = difference_date[j],
                  qty_cumsum = 0)  %>%
          arrange(week_count)
      }
      # put to 0 the other that do not exist in the in the reference
    } else if (nrow(data_reference) < nrow(data_query)){

      difference_date <- anti_join(data_query, data_reference, by = "week_count") %>% pull(week_count)

      for(j in 1:length(difference_date)){
        data_reference <- data_reference %>%
          add_row(season = season_ref,
                  week_count = difference_date[j],
                  qty_cumsum = 0) %>%
          arrange(week_count)
      }
    }


    # 2. calculate euclidean distance -----------------------------------------
    vec_query <- data_query %>% pull(qty_cumsum)
    vec_reference <-  data_reference %>% pull(qty_cumsum)

    distance <- dist(rbind(vec_query,vec_reference)) %>% as.numeric() %>% round(0)
    data_total_distance <- data_total_distance %>%  add_row(season = season_query_list[i],
                                                            euclidean_distance = distance)
  }


  # 3. calculate new weight vector ------------------------------------------
  data_total_distance <- data_total_distance %>% mutate(intensity = 1/euclidean_distance^2) %>%
    mutate(total_intensity = sum(intensity)) %>%
    mutate(dynamic_weight = intensity/total_intensity) %>%
    select(season,dynamic_weight) %>%
    arrange(season)


  weight_vector <- data_total_distance %>% pull(dynamic_weight) %>% round(1)

  # check the weight vector be 1
  if(sum(weight_vector) < 1){

    # if less than 1 calculate the difference from min and give to the difference to the min value
    #  weight_vector[which.min(weight_vector)] <- min(weight_vector) + (1-sum(round(weight_vector,1)))
    less <- 1-sum(weight_vector)
    weight_vector[which.max(weight_vector)] <- max(weight_vector) + less

  } else if(sum(weight_vector) > 1){

    # if more than 1 calculate the difference  between the max value and the difference nb. check it is no negative max(function)
    #  weight_vector[which.min(weight_vector)] <- min(0, min(weight_vector) + (1-sum(round(weight_vector,1))))
    more <- sum(weight_vector) - 1
    weight_vector[which.max(weight_vector)] <- max(weight_vector) - more
  }

  # 4. warnings and errors --------------------------------------------------
  #weight_vector <- weight_vector %>% round(1)

  if(sum(weight_vector) < 1) {

    stop("The sum of the weights are less than 1.")

  } else if (sum(weight_vector) > 1){

    stop("The sum of the weights are more than 1.")

  } else if (length(weight_vector) != length(season_query_list)){

    stop("The weight lenght is not the same as the seasons")

  }

  names(weight_vector) <- season_query_list
  weight_list <- as.list(weight_vector)

  # 5. return ---------------------------------------------------------------
  return(weight_list)
}

#' Create Dynamic Weights
#'
#' @description This function create dynamic weights for the predictive model and it works as follow:
#'
#' 1) It takes the SKU orders lagged by the function \code{\link{traslate_curves_covid19}}
#'
#' 2) It use \code{\link{slice_data}} and \code{\link{prepare_data_weekly}} in order to obtain the weekly dataframe sliced
#'
#' 3) It takes the value of \code{n_week_euclidean} that is set as default at 5. Calculate the last 5 weeks Euclidean distance
#' between season_target season and others
#'
#' 4) It uses the inverse-square law to calculate the intensity and then to allocate the weights
#'
#' @param data              Dataframe. See \code{details}
#' @param season_target     Character. Target Season.
#' @param seasons           Character Vector. A vector with the previous Seasons.
#' @param current_dates     Numeric Vector. Dates in which you want to launch the simulation. Preferable more than one.
#' @param n_week_euclidean  Numeric. The number of observations we want to calculate from the last one of the season_target season.
#'
#' @details The input \code{data} has to be a dataframe (or a tibble) with at least the following columns:
#'
#' \describe{
#'   \item{client}{\code{character}. Clients Names}
#'   \item{date}{\code{Date}. Date of the order}
#'   \item{order}{\code{character}. Code of the order}
#'   \item{start}{\code{Date}. Start date of the campaign}
#'   \item{end}{\code{Date}. End date of the campaign}
#'   \item{season}{\code{character}. Code of the season}
#'   \item{gender_3bs}{\code{character}. Details for man or women}
#'   \item{category}{\code{character}. Details of the different types of category}
#'   \item{style_code}{\code{character}. Code of the style}
#'   \item{fabric_code}{\code{character}. Code of the fabric}
#'   \item{color_code}{\code{character}. Code of the color}
#'   \item{qty}{\code{numeric}. Quantity of the order}
#' }
#'
#' @importFrom dplyr %>%
#'
#' @return The output is a named numeric vector. Weights to apply for each season when calculating the probs.
#' @export
#'
create_dynamic_weight <- function(data, season_target, seasons, current_dates, n_week_euclidean, weight){

  # 0. check type input -----------------------------------------------------
  if(!is.data.frame(data)) stop("data must be a dataframe")
  if(!is.character(season_target)) stop("season_target must be a character")
  if(!is.vector(seasons)) stop("seasons must be a vector")
  if(!is.vector(current_dates)) stop("current_dates must be a vector")
  if(!is.numeric(n_week_euclidean)) stop("n_week_euclidean must be a number")

  # 1. functions ------------------------------------------------------------
  data <- data %>% prepare_data_weekly(season_target, seasons)
  data <- data %>% slice_data(current_dates, season_target)
  weight_list <- data %>% create_weight(season_target, seasons, n_week_euclidean, weight)
  weight_list <- weight_list %>% unlist()
  # 2. return ---------------------------------------------------------------
  return(weight_list)
}

plot_current <- function(data, current_dates, title){
  #data <- df_sku
  data <- data %>%
    mutate(day_count = as.numeric(date - end, "days")) %>%
    group_by(season, day_count) %>%
    summarise(qty = sum(qty)) %>%
    arrange(season, day_count) %>%
    ungroup()

  data <- data %>% group_by(season) %>%
    mutate(qty_cumsum = cumsum(qty))%>%
    ungroup()

  data <- data %>% group_by(season) %>%
    mutate(percentuale = qty/sum(qty))

  data <- data %>% filter(day_count<=current_dates_iter)

  gg <- ggplot(data, aes(x = day_count,
                         y = qty_cumsum,
                         color=season)) +
    geom_line() +
    labs(title = title,
         #subtitle = "subtitle: il sottotitolo",
         #caption = "caption: didascalia (ad es.: \"dati: cars\")",
         x = "Giorni fine campagna",
         y = "Quantita' comulata di ordini")

  print(gg)
}

plot_current_week <- function(data, current_dates, title){

  #data <- df_sku
  data <- data %>%
    mutate(week_count = ceiling(as.numeric(date - end, "days")/7)) %>%
    group_by(season, week_count) %>%
    summarise(qty = sum(qty)) %>%
    arrange(season, week_count) %>%
    ungroup()

  # complete from start
  data <- data %>%
    complete(season, week_count, fill = list(qty = 0))

  # create comulative sum
  data <- data %>% arrange(season, week_count) %>%
    group_by(season) %>%
    mutate(qty_cumsum = cumsum(qty))%>%
    ungroup()

  data <- data %>% filter(week_count<=ceiling(current_dates_iter/7))

  gg <- ggplot(data, aes(x = week_count,
                         y = qty_cumsum,
                         color=season)) +
    geom_line() +
    labs(title = title,
         #subtitle = "subtitle: il sottotitolo",
         #caption = "caption: didascalia (ad es.: \"dati: cars\")",
         x = "Settimane fine campagna",
         y = "Quantita' comulata di ordini")

  print(gg)
}

plot_weight <- function(weight){
  name_weight <- names(weight)
  numb_weight <- c(weight[[1]],weight[[2]],weight[[3]])

  df <- data.frame(
    season = name_weight,
    weight = numb_weight)

  formattable(df, list(
    Season = color_tile("white", "orange"),
    grade = formatter("span", style = x ~ ifelse(x == "A",
                                                 style(color = "green", font.weight = "bold"), NA)),
    area(col = weight) ~ normalize_bar("pink", 0.2)
  ))
}
