#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if (!requireNamespace("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
}
library(BiocManager)
options(repos = BiocManager::repositories())
library(BiocParallel)
library(shiny)
library(crosstalk)
library(toastui)
library(DepecheR)
library(plotly)
library(magrittr)
library(ggthemes)
library(dygraphs)
library(tidyverse)
library(htmlwidgets)
library(remotes)
library(testthat)
library(testthat)
library(seasthedata)
library(dataui)
library(fredr)
library(reactablefmtr)
library(ggalluvial)



# Define UI for application that plots release data
require(shinyjs)


load(url("https://github.com/johnkearns617/AEIEconDataRelease/blob/main/Data/release_save/release_data.RData?raw=true"))

ui <- fluidPage(useShinyjs(),
                tabsetPanel(
                tabPanel("Release Data",uiOutput("release_table")),
                tabPanel("Release Calendar",uiOutput("calendar"))))

server <- function(input, output, session) {

  output$release_table = renderUI({
    vis_table(data_release_table,save_table_a,save_table_a)
    })

  output$calendar = renderUI({calendar(release_dates %>%
                               select(release_name,date) %>%
                               rename(title=release_name) %>%
                               mutate(start=date,
                                      end=date,
                                      category="allday") %>%
                               select(-date) %>%
                               filter(!(title%in%series_codes$release_name[grepl("d_",series_codes$growth)])) %>%
                               left_join(series_codes %>% select(release_name,type) %>% distinct(release_name,.keep_all=TRUE),by=c("title"="release_name")) %>%
                               left_join(data.frame(type=unique(series_codes$type[!grepl("d_",series_codes$growth)]),bgColor=dColorVector(unique(series_codes$type[!grepl("d_",series_codes$growth)]),colorScale="plasma")) %>%  filter(!is.na(type))) %>%
                               filter(!is.na(type)&!is.na(bgColor)) %>% mutate(id=1),
                             defaultView = "month", taskView = TRUE, scheduleView = c("allday"),navigation=TRUE) %>%
    cal_props(list(id=1,color="white"))
  })

  observeEvent(input$tableid1, {
    req(input$tableid1)

    if(save_table_a$sid[input$tableid1]=="A191RL1Q225SBEA"){

      showModal(modalDialog(
        title = "Graph",
        renderPlotly({gdp_plotly}),
        downloadButton("downloadData","Download"),
        easyClose = TRUE,
        footer = NULL,
        size="l"))

    }
    else{ if(save_table_a$sid[input$tableid1]=="LNS17400000"){

      showModal(modalDialog(
        title = "Graph",
        renderPlot({labor_flows_plotly}),
        downloadButton("downloadData","Download"),
        easyClose = TRUE,
        footer = NULL,
        size="l"))

    } else{
    df = dfs[[paste0(save_table_a$sid[input$tableid1])]]
    showModal(modalDialog(
      title = "Graph",
      renderDygraph({dygraph(xts::xts(df$level,df$date),ylab=paste0(df$units_short[1])) %>%
        dySeries("V1",label=df$title[1]) %>%
        dyRangeSelector() %>%
        dyLegend(show="always")}),
      downloadButton("downloadData","Download"),
      easyClose = TRUE,
      footer = NULL,
      size="l"))}
  }})

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(save_table_a$sid[input$tableid1],"_",Sys.Date(),".csv")
    },
    content = function(file) {
      write.csv(dfs[[paste0(save_table_a$sid[input$tableid1])]] %>% select(date,series_id,level,title,units_short,seasonal_adjustment_short,last_updated), file, row.names = FALSE)
    }
  )

}


# Run the application
shinyApp(ui = ui, server = server)
