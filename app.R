library(shiny)
library(tidyverse)

cancer <- read_delim("data_ps06.csv")


ui <- fluidPage(

    titlePanel("Analysis of Cancer Population"),

    mainPanel(
      tabsetPanel(
        tabPanel("General information",
                 p("This set of data comes from dataplanet. It is about the number of people with ",
                   "cancer at four sites in all US states ", em('from 2009 to 2018.'), "The four sites are ",
                   em("breast, larynx, liver, and myeloma. "), "This dataset has ", nrow(cancer), 
                   "rows and", ncol(cancer), "columns. In this app I will focus on the", strong("population "),
                   strong("of liver cancer in each state in the United States.")
                   )
        ),
        
        tabPanel("Plot",
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput("time_year", label = "Choose the time",
                                 min = min(cancer$Time),
                                 max = max(cancer$Time),
                                 value = 2013),
                     
                     radioButtons("sex", label = "Binary gender",
                                 choices = list("Male" = " Male", 
                                                "Female" = " Female")),
                     
                     selectInput("color_choose", label = "Dot color", 
                                  choices = list("Red" = "red", 
                                                 "Blue" = "blue",
                                                 "Green" = "green")),
                     
                   ),
                   mainPanel(plotOutput("cancer_plot"),
                             textOutput("plot_text"))
                 )),
        
        tabPanel("Table",
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput("time_table", label = "Choose the time",
                                 min = min(cancer$Time),
                                 max = max(cancer$Time),
                                 value = 2013),
                     
                     radioButtons("max_min", label = "Population rank", 
                                 choices = list("Ascending" = "ascending",
                                                "Descending" = "descending"))
                   ),
                   mainPanel(dataTableOutput("cancer_table"),
                             textOutput("table_text"))
                 )
        ),
      )))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$cancer_plot <- renderPlot({
      cancer %>% 
        filter(Gender %in% input$sex) %>%
        filter(Time == input$time_year) %>%
        ggplot(aes(State, Population)) + 
        geom_point(col = input$color_choose) + 
        labs(x = "State", y = "Population of liver cancer") + theme(axis.text.x = element_text(angle = 90))
        
  })
  
  output$plot_text <- renderText({
    plot_summary_text <- cancer %>% 
      filter(Gender == input$sex) %>%
      filter(Time == input$time_year) %>%
      arrange(desc(Population))
    
      paste("In ",plot_summary_text$Time[1], ", the state with the most liver cancer among", 
            plot_summary_text$Gender[1], " was ", plot_summary_text$State[1], ", with ", 
            plot_summary_text$Population[1], " people.", sep = "")
  })
  
  output$cancer_table <- renderDataTable({
    if (input$max_min == "ascending") {
      cancer_table <- cancer %>% 
        filter(Time == input$time_table,
               Site == " Liver") %>%
        arrange(Population)
    } else {
      cancer %>% 
        filter(Time == input$time_table,
               Site == " Liver") %>%
        arrange(desc(Population))
    }
  })
  
  output$table_text <- renderText({
    if (input$max_min == "ascending") {
      table_summary_text <- cancer %>% 
        filter(Time == input$time_table,
               Site == " Liver") %>%
        arrange(Population)
      paste("In ",table_summary_text$Time[1], ", the state with the least liver cancer was", 
            table_summary_text$Gender[1], " in ", table_summary_text$State[1], ", with ", 
            table_summary_text$Population[1], " people.", "The mortality is ",
            table_summary_text$Death[1], ".", sep = "")
    } else {
      table_summary_text <- cancer %>% 
        filter(Time == input$time_table,
               Site == " Liver") %>%
        arrange(desc(Population))
      paste("In ",table_summary_text$Time[1], ", the state with the most liver cancer was", 
            table_summary_text$Gender[1], " in ", table_summary_text$State[1], ", with ", 
            table_summary_text$Population[1], " people.", "The mortality is ",
            table_summary_text$Death[1], ".", sep = "")
    }
  })
  
}

shinyApp(ui = ui, server = server)

## I spent 6 hours