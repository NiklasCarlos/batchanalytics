#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Interactive Batch Analytics"),

    # Sidebar with a slider input for number of bins
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

            selectInput("function", "Choose function:",
                        choices= c("test1", "test2")),
            hr(),
            helpText("Data from AT&T (1961) The World's Telephones.")


        ),

        # Show a plot of the generated distribution
        mainPanel(

            # Output: Data file ----



            tabsetPanel(type = "tabs",
                      #  tabPanel("Plot", plotOutput("plot")),
                      tabPanel("Table",  tableOutput("contents")),
                      tabPanel("Summary", verbatimTextOutput("summary"))

            )

           # before maybe interesting for ploting output plotOutput("distPlot")
        )
    )
))
