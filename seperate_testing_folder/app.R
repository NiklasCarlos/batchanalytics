####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang,
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny,
# https://shiny.rstudio.com/articles/reactivity-overview.html


#https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/

library(shiny)
library(batchanalytics)

ui <- shinyUI(fluidPage(
  mainPanel(textInput("func", "Function:", ""),
            submitButton("Plot!"),
            tableOutput("table")
            )
))
server <- function(input, output, session) {
  output$table <-renderTable ({

req(input$func)

    # wenn waiting
batchanalytics::metric_batch_size(result_log, "B", "WorkerB_#0", "concurrent", exclude_singletons = TRUE)

  }
  )
}

shinyApp(ui = ui, server = server)
