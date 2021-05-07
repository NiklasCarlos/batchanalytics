#Functions for Batch analytic one function per metric




#' get batching dataframes
#'
#' @param res_Log
#'
#' @return creates global vars that contain different batching infos and can be used for further analysis
#' @export
#'
#' @examples
get_batching_df_logs <- function(res_log){


  #TODO
  # über globale var für df etc nachdenken statt listen transformation -> evtl einfacher

  # create this as new method -> aber auch daran denken das "no batching" eine option ist

  #head(result_log)

  # refactor one method that gets result_log as input

  # split cases according to there batching behaviour -> dataframe format


  #define global variables
  df_logSim <<- res_log %>%
    group_by(case_id) %>%
    filter(any( batch_type == "simultaneous")) %>% arrange(case_id)


  df_logSeq <<- res_log %>%
    group_by(case_id) %>%
    filter(any( batch_type == "sequential")) %>% arrange(case_id)


  df_logConc <<- res_log %>%
    group_by(case_id) %>%
    filter(any( batch_type == "concurrent")) %>% arrange(case_id)

  #TODO
  #df_log_noBatching


  #how to find cases where no batching is used?

  # log- groupby case - filter any where batchtype != sim || seq|| conc

  #TODO
  #add dfs dynamic to the list depending which batching behaviour is required

  #df_list<-list(df_logSim,df_logSeq, df_logConc)


  #return(df_list)

  #new function for code refactoring
  #get_batching_data(batch_type,result_log)

}

#tidyr gather

#' transform data frame to event log
#'
#' @return creates global vars elogs for each batching type for further analysis
#' @export
#'
#'
#' @examples
transform_df_to_event_log <- function(){

  #TODO
  # über globale var für df etc nachdenken statt listen transformation


  # create this as new method -> aber auch daran denken das "no batching" eine option ist


  #create event log for further analysis elogSim <-....


  #define global variables
  elogSim <<- df_logSim %>%
    gather(status, timestamp,  start, complete)  %>%
    eventlog(
      case_id = "case_id",
      activity_id = "activity",
      activity_instance_id = "instance_id",
      lifecycle_id = "status",
      timestamp = "timestamp",
      resource_id = "resource"
    )

  elogSeq <<- df_logSeq %>%
    gather(status, timestamp,  start, complete)  %>%
    eventlog(
      case_id = "case_id",
      activity_id = "activity",
      activity_instance_id = "instance_id",
      lifecycle_id = "status",
      timestamp = "timestamp",
      resource_id = "resource"
    )


  elogConc <<- df_logConc %>%
    gather(status, timestamp, start, complete)  %>% # arrival omitted , seems to be same as start
    eventlog(
      case_id = "case_id",
      activity_id = "activity",
      activity_instance_id = "instance_id",
      lifecycle_id = "status",
      timestamp = "timestamp",
      resource_id = "resource"
    )

  #log containing all inforamtion
  elog <<- result_log  %>%
    gather(status, timestamp, start, complete)  %>% # arrival omitted , seems to be same as start
    eventlog(
      case_id = "case_id",
      activity_id = "activity",
      activity_instance_id = "instance_id",
      lifecycle_id = "status",
      timestamp = "timestamp",
      resource_id = "resource"
    )

  #TODO
  #elog with only noBatching cases
  #elogNoBatching <<-

}








compare_waiting_times <- function(){
  #TODO

  #implement function that contains waiting times maybe with histogram like in Metrics – Performance measures for Batch Processing.docx described
}




#' cycle time efficiency
#'
#' value defined after duma
#'
#' @return
#' @export
#'
#' @examples
cycle_time_efficiency <- function(){
  return((elog %>% processing_time("log")) / (elog %>% throughput_time("log")))
}



#' compare processing time
#'
#' @return
#' @export
#'
#' @examples
compare_processing_time <- function(){



  #processing time

  sim <- elogSim %>%
    processing_time("log")

  seq <- elogSeq %>%
    processing_time("log")

  conc <- elogConc %>%
    processing_time("log")


  #TODO
  # "no -batching case einfügen und generische zeichen methode je nachdem welches batching verhalten vorhanden ist in den daten -> c( names ) variert <----




  boxplot(sim, seq, conc,xlab = "batch type", ylab = "processing Time", names = c("parallel", "sequential", "concurrent")  )

print("hi processing plot -- ende")

}


#' compare idle times
#'
#' @return
#' @export
#'
#' @examples
compare_idle_time <- function(){

  #TODO
  # "no -batching case einfügen und generische zeichen methode je nachdem welches batching verhalten vorhanden ist in den daten -> c( names ) variert <----


  # create this as new method -> aber auch daran denken das "no batching" eine option ist
  # modularer aufbau der funktionen


  sim <- elogSim %>%
    idle_time("log", units = "days")

  seq <- elogSeq %>%
    idle_time("log", units = "days")

  conc <- elogConc %>%
    idle_time("log", units = "days")

  boxplot(sim, seq, conc,xlab = "batch type", ylab = "idle Time", names = c("parallel", "sequential", "concurrent")  )

}





#' compare troughput time
#'
#' @return
#' @export
#'
#' @examples
compare_throughput_time <- function(){

  sim <- elogSim %>%
    throughput_time("log")


  seq <- elogSeq %>%
    throughput_time("log")

  conc <- elogConc %>%
    throughput_time("log")

  #TODO
  # "no -batching case einfügen und generische zeichen methode je nachdem welches batching verhalten vorhanden ist in den daten -> c( names ) variert <----




  boxplot(sim, seq, conc,xlab = "batch type", ylab = "Throughput Time", names = c("parallel", "sequential", "concurrent")  )

}



#' show batching in process map
#'
#' @return
#' @export
#'
#' @examples
show_batching_in_process_map <- function(){


 class( process_map(elog,))


    #TODO

  #print("noch nicht implementiert")
}





metric_cycleTime <- function(event_log){

}



#' my_detect_batching
#' function to facilitate the batching identification process
#'
#' @param task_log
#'
#' @return result log with batching behaviour
#' @export
#'
#'
#' @examples
my_detect_batching <- function(task_log){
  # Create seq_tolerated_gap_list (gap of 0 seconds is allowed)
  seq_tolerated_gap_list <- seq_tolerated_gap_list_generator(task_log = task_log,
                                                             seq_tolerated_gap_value = 0)

  subsequence_list <- enumerate_subsequences(task_log, 0)
  # Use the following line for using frequent sequence mining instead
  # subsequence_list <- identify_frequent_sequences(task_log, 0)

  # Detect batching behavior
  result_log <- detect_batching(task_log = task_log,
                                act_seq_tolerated_gap_list = seq_tolerated_gap_list,
                                timestamp_format = "yyyy-mm-dd hh:mm:ss",
                                numeric_timestamps = FALSE,
                                log_and_model_based = TRUE,
                                subsequence_list = subsequence_list,
                                subsequence_type = "enum",
                                # use `mine` to use frequence sequence mining
                                # subsequence_type = "mine",
                                within_case_seq_tolerated_gap = 0,
                                between_cases_seq_tolerated_gap = 0,
                                show_progress = F)

  return (result_log)

  }



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
