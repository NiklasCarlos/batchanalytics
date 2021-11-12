#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyr)
library(batchanalytics)
library(bupaR)
library(ggvis)
library(DiagrammeR)

result_log <<- NULL
ready <<- F


# Define server logic required to draw a histogram
shinyServer(function(input, output) {


    output$contents <- renderTable({


        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.

        req(input$file1)

        df <- read.csv(input$file1$datapath)

            print("vor init meth")
        init_batching_analysis(df)
       print("ready after init = ")
        print(ready)

        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }

    })








    #def func evtl verschieben nach package banalyitcs...
    init_batching_analysis <- function(df){
        print("head of df for debugging")
        print(head(df))
        print("init phase begins")

        # result_log <<- my_detect_batching(df)
        res_log <-   my_detect_batching(df)

        result_log <<- res_log

         print("result_log erstellt")
         print(head(result_log))

        get_batching_df_logs(result_log)
        print("bf transform df log")
        transform_df_to_event_log()

        print("init phase ends")
        ready <<- TRUE


    }



    #implement batch_size -> needed input values for this function
    output$result <- renderTable({

        print("check condition summary")

        req(ready)

        print("in result_log ")
        #show summary evtl besser , mit result log und process map und
        if(input$funcTable == "show_summary") {
            print("in result_log methode ")


            if(input$disp == "head") {
                return(head(result_log))
            }
            else {
                return(result_log)
            }

        }else if(input$funcTable == "metric_batch_size") {
            return(batch_size_stats_as_data_frame())
        }else if(input$funcTable == "metric_waiting_times") {
            return(batch_waiting_time_to_data_frame())
        } else if(input$funcTable == "metric_batch_frequency") {
            return(batch_frequency_to_dataframe())
        }else if(input$funcTable == "cycle_time_efficiency") {

          c<- cycle_time_efficiency()


          return(data.frame("median" = c[3], "mean" = c[4] ))
        }


    })

    ###reactive txtoutputs
    output$selected_fun <- renderText({
        paste("You have selected : ", input$funcTable)
    })

    output$selected_fun_plot <- renderText({
        paste("You have selected : ", input$funcPlot)
    })

    #fun explanations for tables
    #explain form bupaR and explain graph -> shows bla with time x achse in min etc
    output$selected_fun_explanation <- renderText({

        if(input$funcPlot == "compare_throughput_time") {

            return("throughput time: the time between the very first event of the case and the very last")

        }else if(input$funcPlot == "compare_processing_time"){

            return("processing time: the sum of the duration of all activity instances")


        }else  if(input$funcPlot == "compare_idle_time"){
            return("idle time: the time when no activity instance is active")
        }else if(input$funcPlot == "show_batching_in_process_map"){
            return("test expl")
        }else if(input$funcPlot == "compare_processing_time_of_activites"){
            return("shows for every activity specifically how long the process time was at this part of process")




        }else if(input$funcPlot == "compare_throughput_time_of_activites"){

            return("shows for every activity specifically how long the process time was at this part of process")



        }

    })



    #future funktion for ploting boxplots, process_maps etc
    output$plot <- renderPlot({
        print("check condition plot")

        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.

        req(ready)

        #init_elog_transformation()
        print("in plot")
        #"show_batching_in_process_map", "compare_throughput_time", "compare_processing_time" , "metric_batch_size"
        if(input$funcPlot == "compare_throughput_time") {
        print("in if plot")

            compare_throughput_time()

        }else if(input$funcPlot == "compare_processing_time"){

            compare_processing_time()


        }else  if(input$funcPlot == "compare_idle_time"){
            compare_idle_time()
        }else if(input$funcPlot == "show_batching_in_process_map"){
            #grViz( show_batching_in_process_map())
        }else if(input$funcPlot == "compare_processing_time_of_activites"){
            #grViz( show_batching_in_process_map())
            compare_processing_time_of_activites()



        }else if(input$funcPlot == "compare_throughput_time_of_activites"){
            #grViz( show_batching_in_process_map())
            compare_throughput_time_of_activites()



        }


    })

    ###table output for plotting
    #str shift c for commenting
    # output$result_plot <- renderTable({
    #     print("check condition plot")
    #
    #     # input$file1 will be NULL initially. After the user selects
    #     # and uploads a file, head of that data file by default,
    #     # or all rows if selected, will be shown.
    #
    #     req(ready)
    #
    #     #init_elog_transformation()
    #     print("in plot")
    #     #"show_batching_in_process_map", "compare_throughput_time", "compare_processing_time" , "metric_batch_size"
    #     if(input$funcPlot == "compare_throughput_time") {
    #         print("in if plot")
    #
    #         compare_throughput_time()
    #
    #     }else if(input$funcPlot == "compare_processing_time"){
    #
    #         compare_processing_time()
    #
    #
    #     }else  if(input$funcPlot == "compare_idle_time"){
    #         compare_idle_time()
    #     }else if(input$funcPlot == "show_batching_in_process_map"){
    #         #grViz( show_batching_in_process_map())
    #     }else if(input$funcPlot == "compare_processing_time_of_activites"){
    #         #grViz( show_batching_in_process_map())
    #         compare_processing_time_of_activites()
    #
    #
    #
    #     }else if(input$funcPlot == "compare_throughput_time_of_activites"){
    #         #grViz( show_batching_in_process_map())
    #         compare_throughput_time_of_activites()
    #
    #
    #
    #     }
    #
    #
    # })


    #graph outputs
    output$process_map <- renderGrViz({

        print("check condition map")
        req(ready)

       # if(input$funcPlot == "show_batching_in_process_map")



        show_batching_in_process_map()
             })

    #table with batch activites outputs
    output$batch_activities <- renderTable({

        print("check condition map")
        req(ready)

        show_dataFrame_with_batching_activities(get_batching_activities_for_each_type())

    })


    #recommendations

    output$rec_metric_processTime <- renderText({
        req(ready)

      resList <-  create_recommendations(get_metric_stats(compare_processing_time(bplot = FALSE)))

        #take string and add processtime

        return(       paste("With a time of: ", paste( round(resList[[1]],3), paste(" +++days+++ and a Batch type of: ", paste(resList[2], paste(" , the Processing time would be improved by around : ",paste(resList[3],paste("percent (compared to avg time of using only other Behaviour)")))) )))
)

    })


    output$rec_metric_throughputTime <- renderText({
        req(ready)

        #string equals result of res <- create_rec_create_recommendations(get_metric_stats(compare_throughput_time(bplot = FALSE)))
        #take string and add processtime
        resList <-    create_recommendations(get_metric_stats(compare_throughput_time(bplot = FALSE)))


return(        paste("With a throughput time of: ", paste( round(resList[[1]],3), paste(" +++days+++ and a Batch type of: ", paste(resList[2], paste(" , the throughput time would be improved by", paste(resList[3],paste("percent (compared to avg time of using only other Behaviour)")))) )))
)

    })

    output$rec_metric_idleTime <- renderText({
        req(ready)

        #take string and add processtime
        resList <-         create_recommendations(get_metric_stats(compare_idle_time(bplot = FALSE)))

return( paste("With a idle time of: ", paste( round(resList[[1]],3), paste(" +++days+++ and a Batch type of: ", paste(resList[2], paste(" , the idle time would be improved by ", paste(resList[3],paste("percent (compared to avg time of using only other Behaviour)")))) )))
)

    })


    output$recommendation <- renderTable({

       return(c("1. ddk", "2.kdkdk"))
    })

   output$recTableActivity <- renderTable({
     return( show_most_eff_act())
   })


    # output$recommendationTable <- renderTable({
    #
    #   #für jeden batchtyp
    #
    #   return(data.frame(processTime, ProcessTime, WaitingTime))
    # })




})
