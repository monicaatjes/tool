#### main ###

### open libaries ###
library(haven)
library(tidyr)
library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(psych)
library(GPArotation)
library(lm.beta)
library(mctest)
library(plotly)
library(dplyr)
library(stringr)
library(rmarkdown)
#ibrary(ingmarkdowntemplates)
library(ggplot2)
#library(ggflags)
library(lubridate)
library(png)
library(ggimage)
library(readxl)
library(tidyxl)
library(data.table)
library(zoo)
library(shiny)
library(corrr)
library(tidyverse)
library(caret)
library(car)
library(gt)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
#library(shiny)
#library(shinydashboard)
library(xfun)
library(bslib)
#library(stdlib.h)

source("functions/trend_plot_format_function.R")
source("functions/funnel_plot_format_function.R")
source("functions/time_plot_format_function.R")
source("functions/post_stamp_function.R")
source("functions/post_stamp_function_option2.R")
source("functions/tab_function.R")


try_set <- read_csv("./data_tot.csv")
try_set2 <- read_csv("./data_overviews_test_customer_app.csv")

x <- as.data.frame(try_set) %>%
    dplyr::mutate(
        quarter = as.yearqtr(quarter))

z <- as.data.frame(try_set2) %>%
    dplyr::mutate(
        quarter = as.yearqtr(quarter))

#library(shiny)



# Define UI for application that draws a histogram
ui <- fluidPage(
    includeCSS("./ing_styles.css"),

    # App title ----
    titlePanel(title=div(img(src="ING Identifier_RGB.jpg", height='100px',width='100px'), "Conversion Quarterly Report")),
    
    # Sidebar layout with input and output definitions ----
    
    sidebarLayout(
        # Sidebar panel for inputs ----
        
        sidebarPanel(
            tags$style(
                tags$link(rel = "stylesheet", type = "text/css", href = "ing_style.css",
                          ".span8 .well { background-color: #00FFFF; }"),
                tags$style("input {font-family: 'INGMe'; font-size:16px; font-weight:bold;}"),
                tags$style("label {font-family: 'INGMe'; font-size:16px; font-weight:bold;}"),
                tags$style("choices {font-family: 'INGMe'; font-size:14px; font-weight:bold;}"),
                tags$style("buttons {font-family: 'INGMe'; font-size:14px; font-weight:bold;}")
            ),
            # Input: Select the random distribution type ----
            selectInput(inputId= "country", 
                        label= "Country:",
                        choices = unique(x$country)),
            
            # Input: Select the random distribution type ----
            selectInput(inputId = "Category", 
                        label ="Category:",
                        choices = unique(x$Category)),
            
            radioButtons(inputId = "rank_order", 
                         label = "Product_rank 
                         (#visits):",
                         choices = unique(x$rank_order)),
            
            radioButtons(inputId = "NewvsCurrent", 
                         label = "NewvsCurrent:",
                         choices = unique(x$NewvsCurrent)),
            
            selectInput(inputId = "quarter", 
                        label = "Quarter:",
                        choices = unique(x$quarter)),
            width =2
        ),
        
        # Main panel for displaying outputs ----
        
        mainPanel(
            tags$header(
                tags$style(rel = "stylesheet", type = "text/css", href = "ing_style.css")
            ),
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(
                tags$head(
                    tags$link(rel = "stylesheet", type = "text/css", href = "ing_style.css")
                ),
                type = "tabs",
                tabPanel(
                    "Funnel plot", plotlyOutput("plot1"),
                    h6("If plot is empty, data not available", align = "left")),
                tabPanel("Conversion 1", plotlyOutput("plot2"),
                         h6("Conversion 1 == #visits start flow/ #visits product page", align = "left")),
                tabPanel("Conversion 2", plotlyOutput("plot3"),
                         h6("Conversion 2 == #visits finish flow/ #visits start flow", align = "left")),
                tabPanel("Traffic", plotlyOutput("plot4"),
                         h6("If plot is empty, data not available", align = "left")),
                tabPanel("QPC view", plotlyOutput("plot5"),
                         h6("QPC PDF shows product rank 1 & customer", align = "left")),
            )
            , width = 5
        )
    )
) 

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # create graph 1
    output$plot1 <- renderPlotly({
        
        df_dat <- filter(x, country == input$country, 
                         Category ==input$Category,
                         NewvsCurrent== input$NewvsCurrent,
                         rank_order== input$rank_order,
                         quarter ==input$quarter)
        
        plot_ly() %>%
            add_trace(
                type = "funnel",
                name= "Mobile",
                y = c("Product page", "Start flow", "Finishing flow", "Account opening"),
                x = c(
                    round(df_dat$`Product page`[df_dat$Device=="Mobile"], digits=2), 
                    round(df_dat$`Start application`[df_dat$Device=="Mobile"], digits=2), 
                    round(df_dat$`Finish application`[df_dat$Device=="Mobile"], digits=2),
                    round(df_dat$`Account opening`[df_dat$Device=="Mobile"], digits=2)),
                textposition = "inside",
                textinfo = "value",
                opacity = 1.0,
                marker = list(color = c("rgb(82, 81, 153)", "rgb(82, 81, 153)", "rgb(82, 81, 153)",
                                        "rgb(82, 81, 153)", "rgb(82, 81, 153)")),
                textfont = list(family = "ING me", size = 14, color = "white"),
                connector = list(fillcolor = c("white", "white", "white", "white", "white"))) %>%
            
            add_trace(
                type = "funnel",
                name = 'Desktop',
                y = c("Product page", "Start flow", "Finishing flow", "Account opening"),
                x = c(
                    round(df_dat$`Product page`[df_dat$Device=="Desktop"], digits=2), 
                    round(df_dat$`Start application`[df_dat$Device=="Desktop"], digits=2), 
                    round(df_dat$`Finish application`[df_dat$Device=="Desktop"], digits=2),
                    round(df_dat$`Account opening`[df_dat$Device=="Mobile"], digits=2)),
                textposition = "inside",
                textinfo = "value",
                opacity = 1.0,
                marker = list(color = c("rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)",
                                        "rgb(255,098,000)", "rgb(82, 81, 153)")),
                textfont = list(family = "ING me", size = 14, color = "white"),
                connector = list(fillcolor = c("white", "white", "white", "white", "white"))) %>%
            layout(yaxis = list(categoryarray = c("PP", "SF", "FF", "AO"),
                                tickfont =list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')),
                   xaxis = list(tickfont =list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')),
                   legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
    })
    
    # create graph 2
    output$plot2 <- renderPlotly ({
        
        df_dat <- filter(x, country == input$country, 
                         Category ==input$Category,
                         NewvsCurrent== input$NewvsCurrent,
                         rank_order== input$rank_order)
        
        # plotly object
        plot_ly(x = as.character(unique(df_dat$quarter))) %>%
            add_trace(y = ~df_dat$con_1[df_dat$Device=="Desktop"], 
                      type='bar', 
                      name="con_1_Desktop",
                      text = df_dat$con_1[df_dat$Device=="Desktop"],
                      hovertemplate = paste('%{x}', '<br>df_dat$con_1: %{text}<br>'),
                      texttemplate = '%{y}', textposition = 'outside',
                      hoverinfo = 'text',
                      textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                      marker = list(color = 'rgb(255,098,000)')) %>%
            add_trace(y =~df_dat$con_1[df_dat$Device=="Mobile"], 
                      type='bar', 
                      name="con_1_Mobile",
                      text = df_dat$con_1[df_dat$Device=="Mobile"],
                      hovertemplate = paste('%{x}', '<br>df_dat$con_1: %{text}<br>'),
                      texttemplate = '%{y}', textposition = 'outside',
                      hoverinfo = 'text',
                      textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                      marker = list(color = 'rgb(96,166,218)')) %>%
            add_trace(y =~df_dat$conv_tot[df_dat$Device=="Desktop"], 
                      type='bar', 
                      name="con_tot_Desktop",
                      text = df_dat$conv_tot[df_dat$Device=="Desktop"],
                      hovertemplate = paste('%{x}', '<br>df_dat$con_1: %{text}<br>'),
                      texttemplate = '%{y}', textposition = 'outside',
                      hoverinfo = 'text',
                      textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                      marker = list(color = 'rgb(52,150,81)')) %>%
            add_trace(y =~df_dat$conv_tot[df_dat$Device=="Mobile"], 
                      type='bar', 
                      name="con_tot_Mobile",
                      text = df_dat$conv_tot[df_dat$Device=="Mobile"],
                      hovertemplate = paste('%{x}', '<br>df_dat$con_1: %{text}<br>'),
                      texttemplate = '%{y}', textposition = 'outside',
                      hoverinfo = 'text',
                      textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                      marker = list(color = 'rgb(82,81,153)')) %>%
            layout(title = "",
                   xaxis = list(title = "",
                                textfont = list(color = 'rgb(105, 105, 105)', size = 10, family = "ING me"),
                                showline = TRUE,
                                linecolor = 'rgb(204, 204, 204)',
                                linewidth = 2,
                                tickfont = list(family = "ING me",
                                                size = 10,
                                                color = 'rgb(105, 105, 105)')),
                   yaxis = list(title = "",
                                range =c(0,100),
                                showticklabels = TRUE,
                                linecolor = 'rgb(204, 204, 204)',
                                linewidth = 2,
                                # autotick = TRUE,
                                # dtick = 10,
                                ticks = 'inside',
                                tickcolor = 'rgb(204, 204, 204)',
                                # tickwidth = 1,
                                # ticklen = 10,
                                tickfont = list(family = "ING me",
                                                size = 10,
                                                color = 'rgb(105, 105, 105)')),
                   barmode = 'group',
                   legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
    })
    
    # create graph 3
    output$plot3 <- renderPlotly ({
        
        df_dat <- filter(x, country == input$country, 
                         Category ==input$Category,
                         NewvsCurrent== input$NewvsCurrent,
                         rank_order== input$rank_order)
        
        # plotly object
        plot_ly(x = as.character(unique(df_dat$quarter))) %>%
            add_trace(y = ~df_dat$con_2[df_dat$Device=="Desktop"], 
                      type='bar', 
                      name="con_2_Desktop",
                      text = df_dat$con_2[df_dat$Device=="Desktop"],
                      hovertemplate = paste('%{x}', '<br>df_dat$con_2: %{text}<br>'),
                      texttemplate = '%{y}', textposition = 'outside',
                      hoverinfo = 'text',
                      textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                      marker = list(color = 'rgb(255,098,000)')) %>%
            add_trace(y =~df_dat$con_2[df_dat$Device=="Mobile"], 
                      type='bar', 
                      name="con_2_Mobile",
                      text = df_dat$con_2[df_dat$Device=="Mobile"],
                      hovertemplate = paste('%{x}', '<br>df_dat$con_2: %{text}<br>'),
                      texttemplate = '%{y}', textposition = 'outside',
                      hoverinfo = 'text',
                      textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                      marker = list(color = 'rgb(96,166,218)')) %>%
            layout(title = "",
                   xaxis = list(title = "",
                                textfont = list(color = 'rgb(105, 105, 105)', size = 10, family = "ING me"),
                                showline = TRUE,
                                linecolor = 'rgb(204, 204, 204)',
                                linewidth = 2,
                                tickfont = list(family = "ING me",
                                                size = 10,
                                                color = 'rgb(105, 105, 105)')),
                   yaxis = list(title = "",
                                range =c(0,100),
                                showticklabels = TRUE,
                                linecolor = 'rgb(204, 204, 204)',
                                linewidth = 2,
                                # autotick = TRUE,
                                # dtick = 10,
                                ticks = 'inside',
                                tickcolor = 'rgb(204, 204, 204)',
                                # tickwidth = 1,
                                # ticklen = 10,
                                tickfont = list(family = "ING me",
                                                size = 10,
                                                color = 'rgb(105, 105, 105)')),
                   barmode = 'group',
                   legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
    })
    
    # create graph 4
    output$plot4 <- renderPlotly ({
        
        df_dat <- filter(x, country == input$country, 
                         Category ==input$Category,
                         NewvsCurrent== input$NewvsCurrent,
                         rank_order== input$rank_order)
        
        # plotly object
        plot_ly(x = as.character(unique(df_dat$quarter))) %>%
            add_trace(y = ~df_dat$`Product page`[df_dat$Device=="Desktop"], 
                      type='scatter', 
                      mode ='lines',
                      name="visits_desktop",
                      text = df_dat$`Product page`[df_dat$Device=="Desktop"],
                      hovertemplate = paste('%{x}', '<br>df_dat$`Product page`: %{text}<br>'),
                      texttemplate = '%{y}', textposition = 'outside',
                      hoverinfo = 'text',
                      textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                      marker = list(color = 'rgb(255,098,000)'),
                      line = list(color = 'rgb(255,098,000)'))  %>%
            add_trace(y =~df_dat$`Product page`[df_dat$Device=="Mobile"], 
                      type='scatter', 
                      mode ='lines',
                      name="visits_mobile",
                      text = df_dat$`Product page`[df_dat$Device=="Mobile"],
                      hovertemplate = paste('%{x}', '<br>df_dat$`Product page`: %{text}<br>'),
                      texttemplate = '%{y}', textposition = 'outside',
                      hoverinfo = 'text',
                      textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                      marker = list(color = 'rgb(96,166,218)'),
                      line = list(color = 'rgb(96,166,218)')) %>%
            layout(title = "",
                   xaxis = list(title = "",
                                textfont = list(color = 'rgb(105, 105, 105)', size = 10, family = "ING me"),
                                showline = TRUE,
                                linecolor = 'rgb(204, 204, 204)',
                                linewidth = 2,
                                tickfont = list(family = "ING me",
                                                size = 10,
                                                color = 'rgb(105, 105, 105)')),
                   yaxis = list(title = "",
                                #range =c(0,100),
                                showticklabels = TRUE,
                                linecolor = 'rgb(204, 204, 204)',
                                linewidth = 2,
                                # autotick = TRUE,
                                # dtick = 10,
                                ticks = 'inside',
                                tickcolor = 'rgb(204, 204, 204)',
                                # tickwidth = 1,
                                # ticklen = 10,
                                tickfont = list(family = "ING me",
                                                size = 10,
                                                color = 'rgb(105, 105, 105)')),
                   legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
        
    })
    
    # create graph 5
    output$plot5 <- renderPlotly ({
        
        df_dat <- filter(z, 
                         quarter ==input$quarter,
                         country == input$country, 
                         rank_order== input$rank_order,
                         NewvsCurrent== input$NewvsCurrent)
            
            # plotly object
            post_stamp <- 
                plot_ly(y = as.character(unique(df_dat$comb_name))) %>%
                add_trace(x = ~df_dat$Mobile_con_1 *100, 
                          type='bar', 
                          name="con_1_Mobile",
                          text = df_dat$Mobile_con_1,
                          hovertemplate = paste('%{x}', '<br>df_dat$con_1: %{text}<br>'),
                          texttemplate = '%{x}', textposition = 'outside',
                          hoverinfo = 'text',
                          textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                          
                          marker = list(color = 'rgb(96,166,218)')) %>%
                add_trace(x = ~df_dat$Mobile_con_2 *100, 
                          type='bar', 
                          name="con_2_Mobile",
                          text = df_dat$Mobile_con_2,
                          hovertemplate = paste('%{x}', '<br>df_dat$con_2: %{text}<br>'),
                          texttemplate = '%{x}', textposition = 'outside',
                          hoverinfo = 'text',
                          textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                          marker = list(color = 'rgb 168, 168, 168')) %>%
                
                layout(title = "",
                       xaxis = list(title = "",
                                    textfont = list(color = 'rgb(105, 105, 105)', size = 12, family = "ING me"),
                                    range =c(0,100),
                                    showline = TRUE,
                                    linecolor = 'rgb(204, 204, 204)',
                                    linewidth = 2,
                                    tickfont = list(family = "ING me",
                                                    size = 12,
                                                    color = 'rgb(105, 105, 105)')),
                       yaxis = list(title = "",
                                    # range =c(0,100),
                                    showticklabels = TRUE,
                                    linecolor = 'rgb(204, 204, 204)',
                                    linewidth = 2,
                                    # autotick = TRUE,
                                    # dtick = 10,
                                    ticks = 'inside',
                                    tickcolor = 'rgb(204, 204, 204)',
                                    # tickwidth = 1,
                                    # ticklen = 10,
                                    tickfont = list(family = "ING me",
                                                    size = 12,
                                                    color = 'rgb(105, 105, 105)')),
                       barmode = 'group',
                       legend =list(x= 0.8, y= 0.9, font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
            
            
        })
        
        
}

# Run the application 
shinyApp(ui = ui, server = server)
