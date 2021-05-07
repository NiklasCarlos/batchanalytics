#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(DiagrammeR)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = shinytheme("superhero"),

    # Application title
    titlePanel("Interactive Batch Analytics"),

    # Sidebar with a input widgets for a user to interact with the server
    sidebarLayout(
        sidebarPanel(
            tags$h3("Input:"),
            fileInput("file1", "Choose CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),

            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),


            hr(),

            # evtl auch eine compare throughput etc implementieren die das boxplot in zahlen ausgeben
            #choices= lsf.str("package:batchanalytics") war vorher c(val1,val2 etc), lsf.str("package:batchanalytics") lists all functions of a package
            selectInput("funcTable", "Choose function:",
                        choices= c("show_result_log",   "metric_batch_size","metric_frequency" ,"compare_throughput_time", "compare_processing_time"  )),

            #TODO
            #input val widget necessary

            hr(),

            selectInput("funcPlot", "Choose function for ploting:",
                        choices= c("compare_throughput_time", "compare_processing_time" , "compare_idle_time","show_batching_in_process_map" )),

            hr(),

            #some helptext maybe necessary when input for functions through one textbox str<- "val1, val2 val3" -> split by semicol um an einzelne vals zu kommen für funktionen
            helpText("Web App To Facilitate The Anaylising Of Batching Behaviour"),

            #debugg try www folder , or maybe only possible in app.R as https://stackoverflow.com/questions/38011285/image-not-showing-in-shiny-app-r/46546344#46546344
            img(src = "TU_logo.png", height = 100, width = 100)
            #C:\\Users\\Niklas\\Desktop\\BachelorArbeit\\R_Tool_Extension\\batchanalytics\\R\\interactiveBatchAnalytics\\www\\


        ),

        # Show Output

         mainPanel(

            # Output: Data file ----



            tabsetPanel(type = "tabs",
                      tabPanel("Table",  tableOutput("contents")),
                      tabPanel("Result", tableOutput("result")),
                      tabPanel("Plot", plotOutput("plot")),
                      tabPanel("Recommendations", h3("recommendations from 1 to n: "), tableOutput("recommendation")),
                      tabPanel("Process Map", grVizOutput("process_map"))


            )

           # before maybe interesting for ploting output plotOutput("distPlot")
        )
    )
))
