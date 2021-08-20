


#' get batching dataframes
#'
#' @param res_Log is the log which contains the results form the batchmining algorihm
#'
#' @return creates global vars that contain different logs for each batch type so they can be used for further analysis
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


  #alternative look for is.na in all activities of a case maybe any() not perfect here
    df_logNoBatch <<- res_log %>%
      group_by(case_id) %>%
      filter( all((batch_type != "concurrent") & (batch_type != "sequential") & (batch_type != "simultaneous") )) %>% arrange(case_id)
  # print("dfno batch")
  #TODO
  #df_log_noBatching


  #how to find cases where no batching is used?

  #log- groupby case - filter any where batchtype != sim & seq & conc

  #TODOmy_detect_batching
  #add dfs dynamic to the list depending which batching behaviour is required

  #df_list<-list(df_logSim,df_logSeq, df_logConc)


  #return(df_list)

  #new function for code refactoring
  #get_batching_data(batch_type,result_log)

}

#tidyr gather

#' transform data.frame obj of the specific batch types  to event log obj for further analysis with bupaR
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

  print("in transform df meth")

 # define global variables
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
  #elog containing only noBatching cases
  elogNoBatch <<- df_logNoBatch %>%
    gather(status, timestamp, start, complete)  %>% # arrival omitted , seems to be same as start
    eventlog(
      case_id = "case_id",
      activity_id = "activity",
      activity_instance_id = "instance_id",
      lifecycle_id = "status",
      timestamp = "timestamp",
      resource_id = "resource"
    )

}



#which activities use batching and what type , how often



#' show_batching_activities
#'
#'which activities use batching
#'
#' @return activities that use batching
#' @export
#'
#' @examples
get_batching_activities <- function(){

  #filter col where batchtype != Na. This are the batch activities
  onlyBatchActivities <- result_log %>% filter(!is.na(batch_type))

 res <-  unique(onlyBatchActivities[, "activity"])


 return(res)

}#' show_dataFrame_with_batching_activities
#'
#'show dataframe with  activities use batching
#'
#'get_batching_activities_for_each_type methode is usallz the liste parameter
#'
#' @return activities that use batching
#' @export
#'
#' @examples
show_dataFrame_with_batching_activities <- function(liste){

  #get all activities
  batching_act <-levels(elog$activity)

  #get for each type activities that are contained
  simultan <- liste[[1]]
  sequential <- liste[[2]]
  conc <- liste[[3]]


  sim_col <- batching_act %in% simultan
  seq_col <- batching_act %in% sequential
  conc_col <- batching_act %in% conc


  mydataFrame <- data.frame(activity = batching_act, simultaneous = sim_col, sequential = seq_col,concurrent = conc_col)


  return(mydataFrame)

}





#' show_batching_activities_for_each_type
#'
#'which activities use batching
#'
#' @return activities that use batching in a list first element of list is sim, 2. seq, 3. conc
#' @export
#'
#' @examples
get_batching_activities_for_each_type <- function(){

  #filter col where batchtype != Na. This are the batch activities
  onlyBatchActivities <- result_log %>% filter(!is.na(batch_type))


  sim <- onlyBatchActivities %>% filter(batch_type == "simultaneous")
  seq <- onlyBatchActivities %>% filter(batch_type == "sequential")
  conc <- onlyBatchActivities %>% filter(batch_type == "concurrent")


  resSim <-  unique(sim[, "activity"])
  resSeq <-  unique(seq[, "activity"])
  resConc <-  unique(conc[, "activity"])


  return(list(resSim,resSeq,resConc))

}

##############################################################
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



#' not in use compare processing time for each batching type and see activity duration
#'
#' @return
#' @export
#'
#' @examples
compare_processing_time_of_activites <- function(){

#TODO _> no implmentet -> was impl as comparing each activity duration for each type with each other -< other function does it now

  #processing time

  sim <- elogSim %>%
    processing_time("activity")

  seq <- elogSeq %>%
    processing_time("activity")

  conc <- elogConc %>%
    processing_time("activity")

  #no batching here ..... same t4 <- elogNoBatch %>% processing_time("activity")



  #TODO
  # no -batching case einfügen und generische zeichen methode je nachdem welches batching verhalten vorhanden ist in den daten -> c( names ) variert <----
  # noBatch <- elogNoBatch %>%
  #   processing_time("log")


  #TODO
  #add noBatch maybe generic approach if elogs without vals
  #boxplot(sim, seq, conc,xlab = "batch type", ylab = "processing Time", names = c("parallel", "sequential", "concurrent")  )

 # boxplot(sim, seq, conc)
 # boxplot(sim,xlab = "batch type simultanous" )

  print(sim)
  plot(sim)

}

#######################metrics for ploting####################################

#' compare throughput time for each batching type and see activity duration
#' https://stackoverflow.com/questions/14604439/plot-multiple-boxplot-in-one-graph
#'
#' @return ggplot boxplot
#' @export
#'
#' @examples
compare_throughput_time_of_each_activites <- function(){

  res_with_throughput_Time <- result_log

  res_with_throughput_Time$Throughput_time = result_log$complete - result_log$start

  res_with_throughput_Time$batch_type[is.na(res_with_throughput_Time$batch_type)] <- "no batching"

   return(ggplot(data = res_with_throughput_Time, aes(x=activity, y=Throughput_time)) + geom_boxplot(aes(fill=batch_type)))

}


#' compare processing time
#'
#' @return
#' @export
#'
#' @examples
compare_processing_time <- function(bplot = TRUE){

  #processing time

  sim <- elogSim %>%
    processing_time("log")

  seq <- elogSeq %>%
    processing_time("log")

  conc <- elogConc %>%
    processing_time("log")



  #TODO
  # no -batching case einfügen und generische zeichen methode je nachdem welches batching verhalten vorhanden ist in den daten -> c( names ) variert <----
  noBatch <- elogNoBatch %>% processing_time("log")


  #TODO
  #add noBatch maybe generic approach if elogs without vals
  boxplot(sim, seq, conc,noBatch ,xlab = "batch type", ylab = "processing Time", names = c("parallel", "sequential", "concurrent", "noBatch"), plot = bplot )


  #find type with nrow = null und als print unter graph sagen das dieses verhalten nicht vorhanden ist



}







#' compare idle times
#'
#' @return
#' @export
#'
#' @examples
compare_idle_time <- function(bplot = TRUE){



  sim <- elogSim %>% get_idle_time
  seq <- elogSeq %>% get_idle_time

  conc <- elogConc %>% get_idle_time

  noBatch <- elogNoBatch %>% get_idle_time




  boxplot(sim, seq, conc, noBatch,xlab = "batch type", ylab = "idle Time", names = c("parallel", "sequential", "concurrent", "noBatch") , plot = bplot )

}

#' get_idle_time
#' helper FUN for getting idle time(solve the elog cant be empty problem with replacing null)
#'
#' @param elog
#'
#' @return idle time
#' @export
#'
#' @examples
get_idle_time <- function(elog){

  if(nrow(elog) > 0){

    return( elog %>% idle_time("log", units = "days"))
  }else{
    return  (NULL)

  }

}



#' compare troughput time
#'
#' @return
#' @export
#'
#' @examples
compare_throughput_time <- function(bplot = TRUE){


  sim <- elogSim %>%
    throughput_time("log")


  seq <- elogSeq %>%
    throughput_time("log")

  conc <- elogConc %>%
    throughput_time("log")

  noBatch <- elogNoBatch %>%  throughput_time("log")

  boxplot(sim, seq, conc,noBatch, xlab = "batch type", ylab = "Throughput Time", names = c("parallel", "sequential", "concurrent","noBatch"), plot = bplot  )

}



#' show batching in process map
#'
#' @return
#' @export
#'
#' @examples
show_batching_in_process_map <- function(){


  process_map(elog)


  #TODO

  #print("noch nicht implementiert") color nodes in red where batching or sth similar see documentation of graphic libary that creates the node graph
  # use show batch activites which returns all activites that contain batching
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

#########################################Batch processing frequency


#' batch_frequency_to_dataframe
#'
#'
#' @param relative relative = true ; false = absolute
#'
#' @return dataframe that contains frequency information
#' @export
#'
#' @examples
batch_frequency_to_dataframe <- function(relative = TRUE){

  mylist <- c()
  #Create an empty data frame
  df <- data.frame(batch_type=character(), activity=character(), batch_frequency=double())


  for (b_type in get_batch_types(result_log)) {
    for (b_act in get_act_for_specific_batch_type(b_type)) {
      size_stats <- shiny_metric_batch_frequency(result_log,b_act,b_type,relative)
      mylist <- list(batch_type = b_type, activity = b_act, batch_frequency = size_stats)
      df = rbind(df,mylist, stringsAsFactors=FALSE)

    }
  }

  return(df)

}

#' shiny_metric_batch_frequency
#'
#' @param activity_log_with_batches
#' @param act
#' @param type_of_batch
#' @param relative
#'
#' @return
#' @export
#'
#' @examples
shiny_metric_batch_frequency <- function(activity_log_with_batches, act, type_of_batch, relative = TRUE){

  # Select relevant activity instances
  ra_selection <- as.data.frame(activity_log_with_batches %>% filter(activity == act))

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


#####################################end Batch processing frequency

##################################metric Batch size functions


#' get_batch_types
#'
#' @param res_log
#'
#' @return returns batch types that are found in the result log
#' @export
#'
#' @examples
get_batch_types <- function(res_log) {

  onlyBatchTypes <- res_log %>% filter(!is.na(batch_type))

  res <-  unique(onlyBatchTypes[, "batch_type"])

  return (res)

}

# i.e. call x <- get_act_for_specific_batch_type(get_batch_types(res_log = result_log)[1]) = B,C

#' get_act_for_specific_batch_type
#'
#' @param my_batch_type
#'
#' @return gets for a specific batch types all the activites
#' @export
#'
#' @examples
get_act_for_specific_batch_type <- function(my_batch_type) {

  onlyBatchActivities <- result_log %>% filter(batch_type == my_batch_type)

  res <-  unique(onlyBatchActivities[, "activity"])


  return(res)


}


#' batch size fun optimized for use in shiny app
#'
#' @param activity_log_with_batches
#' @param act
#' @param type_of_batch
#' @param exclude_singletons
#' @importFrom dplyr summarize
#'
#' @return
#' @export
#'
#' @examples
shiny_metric_batch_size <- function(activity_log_with_batches, act, type_of_batch, exclude_singletons = TRUE){

  # Select relevant activity instances
  ra_selection <- as.data.frame(activity_log_with_batches %>% filter(activity == act))

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

  #names(sumstats) <- c("mean", "median", "sd", "min", "max", "q1", "q3")

  return(sumstats)
}

#' batch_size_stats_as_data_frame
#'
#' @return data frame for shiny app that shows for every batch type the corresponiding activity and its size metrics(mean , median)
#' @export
#'
#' @examples
batch_size_stats_as_data_frame <- function() {

  mylist <- c()
  #Create an empty data frame
  df <- data.frame(batch_type=character(), activity=character(), mean=double(), median = double())


  for (b_type in get_batch_types(result_log)) {
    for (b_act in get_act_for_specific_batch_type(b_type)) {
      size_stats <- shiny_metric_batch_size(result_log,b_act,b_type)
      mylist <- list(batch_type = b_type, activity = b_act, mean = size_stats[1], median = size_stats[2])
      df = rbind(df,mylist, stringsAsFactors=FALSE)

    }
  }



  #code to convert list into df

  #change to df
  return(df)



}

######################### batch size end

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
############################################waiting time##############


#' batch_waiting_time_to_data_frame
#'
#' @return
#' @export
#'
#' @examples
batch_waiting_time_to_data_frame <- function(){

  mylist <- c()
  #Create an empty data frame
  df <- data.frame(batch_type=character(), activity=character(), batch_waiting_time=double())


  for (b_type in get_batch_types(result_log)) {
    for (b_act in get_act_for_specific_batch_type(b_type)) {
      size_stats <- shiny_metric_waiting_time(result_log,b_act,b_type)
      mylist <- list(batch_type = b_type, activity = b_act, batch_waiting_time = size_stats[1])
      df = rbind(df,mylist, stringsAsFactors=FALSE)

    }
  }

  return(df)

}


#' shiny_metric_waiting_time
#'
#' @param activity_log_with_batches
#' @param act
#' @param type_of_batch
#'
#'
#' @importFrom lubridate is.POSIXct
#' @return
#' @export
#'
#' @examples
shiny_metric_waiting_time <- function(activity_log_with_batches, act,  type_of_batch){

  # Select relevant activity instances
  ra_selection <- as.data.frame(activity_log_with_batches %>% filter(activity == act))

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


############create recommendations

#get the vals out of matrix which row shows median -> 5r -> https://stackoverflow.com/questions/28173284/extract-statistics-from-boxplot


#' get_metric_stats
#'
#' @param metric_fun metric function like compare_processing_time(bplot = FALSE) as example
#'
#' @return returns the median of all batch types and safes in a c(parallel,seq,conc, noBatch)
#' @export
#'
#' @examples
get_metric_stats <- function(metric_fun){
  p <- metric_fun$stats[3,1]
  s <- metric_fun$stats[3,2]
  c <- metric_fun$stats[3,3]
  nb <- metric_fun$stats[3,4]
  return(c(p,s,c,nb))
}

#' create_recommendations
#'
#' @param metricStats
#'
#' @return recommendation for specific metric how to proceed with process as string
#' @export
#'
#' @examples
create_recommendations <- function(metricStats){

  #replace na with inf if batch types are not existing in data set
  #> v1 <- replace(v1,is.na(v1),Inf)
  metricStats <- replace(metricStats,is.na(metricStats),Inf)


  minTime <- min(metricStats)



  minPos <- which.min(minTime)

  batch_Type <- NULL

  if(minPos == 1){

    batch_Type <- "PARALLEL"

  }else if(minPos == 2){

    batch_Type <- "SEQUENTIAL"

  }else if(minPos == 3){

    batch_Type <- "CONCURRENT"

  }else if(minPos == 4){

    batch_Type <- "NO BATCHING"

  }else{
    print("position not found")

    batch_Type <- "NO INFO check data"
  }

  res <- list(minTime,batch_Type)

  return(res)
}




#give two process times(optimal sol vs avg functime with everything) and function retruns how much faster the optimal solution is
#process_advantage_percent <- function(){}

#function to compare a metric with specific batchtype to its avg time so the time advangtage can be calculated
#avgprocessTime-<



