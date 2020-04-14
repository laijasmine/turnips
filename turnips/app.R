#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(googlesheets4)
library(tidyverse)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("The Underground Stalk Market"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons(inputId = "islandnm",
                         label = "Name of Island:",
                         choices = c("Postelsia" = "Postelsia", 
                                     "Coral Cove" = "Coral Cove",
                                     "Fairfall" = "Fairfall")),
            img(src= "Tommy.png")
        ),

        # Show a plot of the turnips
        mainPanel(
           plotOutput("linePlot"), 
        dataTableOutput("stat"))
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    ss <- "https://docs.google.com/spreadsheets/d/1EpsEX9C8es0CHoPlARewnoqO1-m03MU7k2NKfkTZ2Zc/edit#gid=0"
        
    gs <- read_sheet(ss = ss)
    
    output$stat <- renderDataTable({
        #get the week data collected
        df_wk <- gs %>% 
            mutate(Week = epiweek(Date)) %>% 
            filter(island == input$islandnm)
        
        #max and min prices
        df_stat <- df_wk %>% 
            filter(DayofWeek != "Sunday") %>% 
            group_by(Week) %>% 
            summarise(Max = max(price),
                      Min = min(price),
                      Average = mean(price))
        
    })
    
    
    output$linePlot <- renderPlot({
        
        #colour
        ac_pal <- c("#6FDEAB","#C0C5C3", "#7fCfD8")
        
        #get the week data collected
        df_wk <- gs %>% 
            mutate(wk = epiweek(Date)) %>% 
            filter(island == input$islandnm)
        
        #reorder to make sense
        df_wk$DayofWeek <- ordered(df_wk$DayofWeek,
                                   levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                            "Friday", "Saturday", "Sunday"))
        #get sunday prices
        df_buy <- df_wk %>% 
            filter(DayofWeek == "Sunday")
    
        #plotting
        ggplot(df_wk) +
            geom_line(aes(x = DayofWeek, y = price, group = AMPM, colour = AMPM)) +
            geom_point(aes(x = DayofWeek, y = price, group = AMPM, colour = AMPM)) +
            facet_wrap(~wk) +
            scale_color_manual(values = ac_pal) +
            geom_hline(data = df_buy, aes(yintercept = price)) +
            #geom_hline(data = df_stat, aes(yintercept = top)) +
            theme_minimal() +
            labs(title = paste("Turnip Prices for", input$islandnm),
                 x = "Day of the Week",
                 y = "Price (Bells)",
                 colour = "Time of Day") +
            theme(axis.text.x = element_text(angle = 45))})
}

# Run the application 
shinyApp(ui = ui, server = server)
