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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {


    output$contents <- renderTable({

        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.

        req(input$file1)

        df <- read.csv(input$file1$datapath)


        init_batching_analysis(df)

        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }

    })




    #implement batch_size -> needed input values for this function
    output$result <- renderTable({
        req(input$file1)



        if(input$funcTable == "show_result_log") {
            return(result_log)
        }
        else {
            return(df)
        }


    })




    #def func evtl verschieben nach package banalyitcs...
    init_batching_analysis <- function(df){
        result_log <- my_detect_batching(df)
        get_batching_df_logs(result_log)
        transform_df_to_event_log()


    }


    # init_elog_transformation <- function(){
    #     sim <<- elogSim %>%
    #         throughput_time("log")
    #
    #
    #     seq <<- elogSeq %>%
    #         throughput_time("log")
    #
    #     conc <<- elogConc %>%
    #         throughput_time("log")
    # }




    #future funktion for ploting boxplots, process_maps etc
    output$plot <- renderPlot({

        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.

        req(input$file1)

        #init_elog_transformation()

        #"show_batching_in_process_map", "compare_throughput_time", "compare_processing_time" , "metric_batch_size"
        if(input$funcPlot == "compare_throughput_time") {

            sim <- elogSim %>%
                throughput_time("log")


            seq <- elogSeq %>%
                throughput_time("log")

            conc <- elogConc %>%
                throughput_time("log")


            boxplot(sim, seq, conc,xlab = "batch type", ylab = "Throughput Time", names = c("parallel", "sequential", "concurrent"))
            # show_batching_in_process_map()



        }else if(input$funcPlot == "compare_processing_time"){
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
        }else  if(input$funcPlot == "compare_idle_time"){
            compare_idle_time()
        }


    })




})
