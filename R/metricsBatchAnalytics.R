#Functions for Batch analytic one function per metric



# Metric - Batch processing frequency
#' Metric - Batch processing frequency
#'
#' @param activity_log_with_batches
#' @param act
#' @param res
#' @param type_of_batch
#' @param relative
#'
#' @return
#' @export
#'
#' @importFrom dplyr %>%
#'
#' @examples
metric_frequency <- function(activity_log_with_batches, act, res, type_of_batch, relative = FALSE){

  # Select relevant activity instances
  ra_selection <- as.data.frame(activity_log_with_batches %>% filter(activity == act, resource == res))

  # Determine number of batches of this type (having size > 1)
  n_batches <- length(unique(ra_selection[which(ra_selection$batch_type == type_of_batch),"batch_number"]))

  # Return value depending on 'relative' parameter
  if(relative == FALSE){
    return(n_batches)
  } else{
    # Implemented as: number of batches with size > 1 / (number of singleton groups + number of batches with size > 1)

    # Determine number of 'singleton groups'
    n_singl_groups <- nrow(ra_selection) - as.numeric(nrow(as.data.frame(ra_selection %>% filter(batch_type == type_of_batch))))

    # Calculate value
    output <- n_batches / (n_singl_groups + n_batches)
    return(output)
  }
}



# Metric - Batch size

#' Title
#'
#' @param activity_log_with_batches
#' @param act
#' @param res
#' @param type_of_batch
#' @param exclude_singletons
#'
#' @return
#' @export
#'
#' @examples
metric_batch_size <- function(activity_log_with_batches, act, res, type_of_batch, exclude_singletons = TRUE){

  # Select relevant activity instances
  ra_selection <- as.data.frame(activity_log_with_batches %>% filter(activity == act, resource == res))

  # Determine batch sizes (batches with size > 1)
  batch_sizes <- (as.data.frame(ra_selection %>% filter(batch_type == type_of_batch) %>% group_by(batch_number) %>% summarize(size=n())))$size

  if(exclude_singletons == FALSE){
    n_singl_groups <- nrow(ra_selection) - as.numeric(nrow(as.data.frame(ra_selection %>% filter(batch_type == type_of_batch))))
    batch_sizes <- c(batch_sizes, rep(1, n_singl_groups))
  }

  # Determine summary statistics
  if(length(batch_sizes) > 0){
    sumstats <- c(mean(batch_sizes), median(batch_sizes), sd(batch_sizes), min(batch_sizes), max(batch_sizes),
                  as.numeric(quantile(batch_sizes, 0.25)), as.numeric(quantile(batch_sizes, 0.75)))
  } else{
    sumstats <- c(NA, NA, NA, NA, NA, NA, NA)
  }

  names(sumstats) <- c("mean", "median", "sd", "min", "max", "q1", "q3")

  return(sumstats)
}

# Metric - Batch processing prevalence (number of cases contained in a batch)


metric_prevalence <- function(activity_log_with_batches, act, res, type_of_batch, relative = FALSE){

  # Select relevant activity instances
  ra_selection <- as.data.frame(activity_log_with_batches %>% filter(activity == act, resource == res))

  # Determine number of cases included in a batch
  batched_cases <- as.numeric(nrow((as.data.frame(ra_selection %>% filter(batch_type == type_of_batch)))))

  if(relative == FALSE){
    return(batched_cases)
  } else{
    # Determine number of singleton groups
    n_singleton_groups <- nrow(ra_selection) - batched_cases
    relative <- batched_cases / (batched_cases + n_singleton_groups)
    return(relative)
  }
}

# Metric - Activity duration

#' Title
#'
#' @param activity_log_with_batches
#' @param act
#' @param res
#' @param type_of_batch
#'
#' @return
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom lubridate is.POSIXct
#'
#' @examples
metric_activity_duration <- function(activity_log_with_batches, act, res, type_of_batch){


  ####debugging
  print("hello world11")

  # Select relevant activity instances
  ra_selection <- as.data.frame(activity_log_with_batches %>% filter(activity == act, resource == res))

  # Calculate activity durations
  if(is.numeric(ra_selection$start) & is.numeric(ra_selection$complete)){
    ra_selection$dur <- ra_selection$complete - ra_selection$start
  } else if(is.POSIXct(ra_selection$start) & is.POSIXct(ra_selection$complete)){
    ra_selection$dur <- as.numeric(difftime(ra_selection$complete, ra_selection$start, units = "hours"))
  } else{
    stop("Metric activity duration - Timestamp formats should be numeric or POSIXct")
  }

  # Determine duration summary statistics for batched cases
  batched_cases_dur <- (as.data.frame(ra_selection %>% filter(batch_type == type_of_batch)))$dur
  if(length(batched_cases_dur) > 0){
    sumstats <- c(mean(batched_cases_dur), median(batched_cases_dur), sd(batched_cases_dur), min(batched_cases_dur), max(batched_cases_dur),
                  as.numeric(quantile(batched_cases_dur, 0.25)), as.numeric(quantile(batched_cases_dur, 0.75)))
  } else{
    sumstats <- c(NA, NA, NA, NA, NA, NA, NA)
  }

  # Determine duration summary statistics for non-batched cases
  non_batched_cases_dur <- ra_selection[-which(ra_selection$batch_type == type_of_batch),"dur"]
  if(length(non_batched_cases_dur) > 0){
    sumstats <- c(sumstats, mean(non_batched_cases_dur), median(non_batched_cases_dur), sd(non_batched_cases_dur), min(non_batched_cases_dur), max(non_batched_cases_dur),
                  as.numeric(quantile(non_batched_cases_dur, 0.25)), as.numeric(quantile(non_batched_cases_dur, 0.75)))
  } else{
    sumstats <- c(sumstats, NA, NA, NA, NA, NA, NA, NA)
  }

  # Rename sumstats entries
  names(sumstats) <- c("b_mean", "b_median", "b_sd", "b_min", "b_max", "b_q1", "b_q3",
                       "nb_mean", "nb_median", "nb_sd", "nb_min", "nb_max", "nb_q1", "nb_q3")

  return(sumstats)
}


# Metric - Waiting time

#' Title
#'
#' @param activity_log_with_batches
#' @param act
#' @param res
#' @param type_of_batch
#'
#' @return
#' @export
#'
#' @examples
metric_waiting_time <- function(activity_log_with_batches, act, res, type_of_batch){

  # Select relevant activity instances
  ra_selection <- as.data.frame(activity_log_with_batches %>% filter(activity == act, resource == res))

  # Determine (when possible) the waiting times
  if("arrival" %in% names(ra_selection) & !is.na(ra_selection$arrival[1])){

    # Calculate waiting time
    if(is.numeric(ra_selection$arrival) & is.numeric(ra_selection$start)){
      ra_selection$wt <- ra_selection$start - ra_selection$arrival
    } else if(is.POSIXct(ra_selection$arrival) & is.POSIXct(ra_selection$start)){
      ra_selection$wt <- as.numeric(difftime(ra_selection$start, ra_selection$arrival, units = "hours"))
    } else{
      stop("Metric waiting time - Timestamp formats should be numeric or POSIXct")
    }

    # Determine waiting time summary statistics for batched cases
    batched_cases_wt <- (as.data.frame(ra_selection %>% filter(batch_type == type_of_batch)))$wt
    if(length(batched_cases_wt) > 0){
      sumstats <- c(mean(batched_cases_wt), median(batched_cases_wt), sd(batched_cases_wt), min(batched_cases_wt), max(batched_cases_wt),
                    as.numeric(quantile(batched_cases_wt, 0.25)), as.numeric(quantile(batched_cases_wt, 0.75)))
    } else{
      sumstats <- c(NA, NA, NA, NA, NA, NA, NA)
    }

    # Determine waiting time summary statistics for non-batched cases
    non_batched_cases_wt <- ra_selection[-which(ra_selection$batch_type == type_of_batch),"wt"]
    if(length(non_batched_cases_wt) > 0){
      sumstats <- c(sumstats, mean(non_batched_cases_wt), median(non_batched_cases_wt), sd(non_batched_cases_wt), min(non_batched_cases_wt), max(non_batched_cases_wt),
                    as.numeric(quantile(non_batched_cases_wt, 0.25)), as.numeric(quantile(non_batched_cases_wt, 0.75)))
    } else{
      sumstats <- c(sumstats, NA, NA, NA, NA, NA, NA, NA)
    }


  } else{
    # Activity log does not contain arrival times. As a consequence, waiting times cannot be calculated
    sumstats <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  }

  # Rename sumstats entries
  names(sumstats) = c("b_mean", "b_median", "b_sd", "b_min", "b_max", "b_q1", "b_q3",
                      "nb_mean", "nb_median", "nb_sd", "nb_min", "nb_max", "nb_q1", "nb_q3")

  return(sumstats)
}

# Metric - Number of concurrent cases
#' Title
#'
#' @param activity_log_with_batches
#' @param act
#' @param res
#'
#' @return
#' @export
#'
#' @examples
metric_n_concurrent_cases <- function(activity_log_with_batches, act, res){

  # Select relevant activity instances that are in a concurrent batch
  ra_selection <- as.data.frame(activity_log_with_batches %>% filter(activity == act, resource == res,
                                                                     batch_type == "concurrent"))

  if(nrow(ra_selection) > 0){
    # If concurrent batches are present, calculate the number of concurrently handled cases
    # Calculation method explained in PhD Niels
    n_conc_cases <- c()

    # For each batch, determine the number of concurrently handled cases
    conc_batches <- unique(ra_selection$batch_number)

    for(i in 1:length(conc_batches)){
      batch_instances <- as.data.frame(ra_selection %>% filter(batch_number == conc_batches[i]))

      start_times <- batch_instances$start
      names(start_times) <- rep("start", length(start_times))
      complete_times <- batch_instances$complete
      names(complete_times) <- rep("complete", length(complete_times))
      times <- c(start_times, complete_times)
      times <- sort(times)

      conc_case_counter <- 0

      for(j in 1:(length(times)-1)){ # -1 to not include the final complete
        if(names(times)[j] == "start"){
          conc_case_counter <- conc_case_counter + 1
          n_conc_cases <- c(n_conc_cases, conc_case_counter)
        } else{
          conc_case_counter <- conc_case_counter - 1
          n_conc_cases <- c(n_conc_cases, conc_case_counter)
        }
      }
    }

    # Determine summary statistics on number of concurrently handled cases
    sumstats <- c(mean(n_conc_cases), median(n_conc_cases), sd(n_conc_cases), min(n_conc_cases), max(n_conc_cases),
                  as.numeric(quantile(n_conc_cases, 0.25)), as.numeric(quantile(n_conc_cases, 0.75)))

  } else{
    # No concurrent batches detected for this resource-activity combination
    sumstats <- c( NA, NA, NA, NA, NA, NA, NA)
  }

  names(sumstats) <- c("mean", "median", "sd", "min", "max", "q1", "q3")

  return(sumstats)
}


# Metric - Time overlap between concurrent cases

metric_overlap_concurrent_cases <- function(activity_log_with_batches, act, res){

  # Select relevant activity instances that are in a concurrent batch
  ra_selection <- as.data.frame(activity_log_with_batches %>% filter(activity == act, resource == res,
                                                                     batch_type == "concurrent"))

  if(nrow(ra_selection) > 0){
    # If concurrent batches are present, determine time overlap
    pct_time_overlap <- c()

    # Convert timestamps to numeric format
    ra_selection$start <- as.numeric(ra_selection$start)
    ra_selection$complete <- as.numeric(ra_selection$complete)
    activity_log_with_batches$start <- as.numeric(activity_log_with_batches$start)
    activity_log_with_batches$complete <- as.numeric(activity_log_with_batches$complete)

    for(i in 1:nrow(ra_selection)){

      # Select instances that are concurrent in time with the considered instance. Afterwards, "merge" these instances in a single period
      conc_instances <- as.data.frame(ra_selection %>% filter(((start >= ra_selection$start[i] & start <= ra_selection$complete[i]) |
                                                                 (complete >= ra_selection$start[i] & complete <= ra_selection$complete[i]) |
                                                                 (start < ra_selection$start[i] & complete > ra_selection$complete[i])),
                                                              activity_instance_id != ra_selection$activity_instance_id[i]) %>% summarize(start = min(start), complete = max(complete)))

      # Determine overlap
      instance_tot_dur <- ra_selection$complete[i] - ra_selection$start[i]

      if(conc_instances$start[1] <= ra_selection$start[i] & conc_instances$complete[1] >= ra_selection$complete[i]){
        pct_time_overlap <- c(pct_time_overlap, 1)
      } else if(conc_instances$start[1] <= ra_selection$start[i] & conc_instances$complete[1] < ra_selection$complete[i]){
        pct_overlap <- (conc_instances$complete[1] - ra_selection$start[i]) / instance_tot_dur
        pct_time_overlap <- c(pct_time_overlap, pct_overlap)
      } else if(conc_instances$start[1] > ra_selection$start[i] & conc_instances$complete[1] >= ra_selection$complete[i]){
        pct_overlap <- (ra_selection$complete[i] - conc_instances$start[1]) / instance_tot_dur
        pct_time_overlap <- c(pct_time_overlap, pct_overlap)
      } else if(conc_instances$start[1] > ra_selection$start[i] & conc_instances$complete[1] < ra_selection$complete[i]){
        pct_overlap <- (conc_instances$complete[1] - conc_instances$start[1]) / instance_tot_dur
        pct_time_overlap <- c(pct_time_overlap, pct_overlap)
      }

    }

    # Determine summary statistics on percentage time overlap
    sumstats <- c(mean(pct_time_overlap), median(pct_time_overlap), sd(pct_time_overlap), min(pct_time_overlap), max(pct_time_overlap),
                  as.numeric(quantile(pct_time_overlap, 0.25)), as.numeric(quantile(pct_time_overlap, 0.75)))

  } else{
    # No concurrent batches detected for this resource-activity combination
    sumstats <- c( NA, NA, NA, NA, NA, NA, NA)
  }

  names(sumstats) <- c("mean", "median", "sd", "min", "max", "q1", "q3")

  return(sumstats)
}
