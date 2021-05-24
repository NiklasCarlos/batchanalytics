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
        print("before")

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
        }


    })

    #graph outputs
    output$process_map <- renderGrViz({

        print("check condition map")
        req(ready)

       # if(input$funcPlot == "show_batching_in_process_map")



        show_batching_in_process_map()
             })


    #recommendations

    output$recommendation <- renderTable({

       return(c("1. ddk", "2.kdkdk"))
    })



})
