####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang,
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny,
# https://shiny.rstudio.com/articles/reactivity-overview.html

library(bupaR)
library(shiny)
library(DiagrammeR)
#Create a data frame
key<-rep("DISCHARGEDATE", 5)
time<-seq(as.POSIXct("2017-09-20 12:07:00",format="%Y-%m-%d %H:%M:%S"),
          by="min",length.out = 5)
patient<-seq(1,5)

df<-as.data.frame(cbind(key=as.character(key),time=as.character(time),
                        patient=as.character(patient)), stringsAsFactors = FALSE)
df$time<- as.POSIXct(df$time, format="%Y-%m-%d %H:%M:%S")

#create a simple log
s.Log<-bupaR::simple_eventlog(eventlog=df,
                              case_id="patient",
                              activity_id="key",
                              timestamp="time")
#shiny
ui <- fluidPage(

  mainPanel(
    #plotOutput("process_map")
    grVizOutput("process_map")
  )
)

server <- function(input, output) {

  output$process_map <- renderGrViz({

    process_map(elog)

  })
}

shinyApp(ui = ui, server = server)
