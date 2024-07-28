## app.R ##
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggmap)
library(plotly)
library(tidyverse)
library(countrycode)
library(readr)
library(VIM)

# Load your dataset
gapminder_internet <- read_csv('gapminder_internet.csv')

# Data Preprocessing
gapminder_internet <- gapminder_internet %>%
  mutate(across(c(incomeperperson, internetuserate), round, 2),
         count_na = rowSums(is.na(.))) %>%
  filter(count_na < 2)

gapminder_internet <- kNN(gapminder_internet, k = 5, imp_var = FALSE) %>%
  select(-count_na)

gapminder_internet <- gapminder_internet %>%
  mutate(code = countrycode(country, origin = 'country.name', destination = 'iso3c'))

ui <- dashboardPage(
  dashboardHeader(title = "Global Internet Usage"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Choropleth Map", tabName = "choroplethMap", icon = icon("globe")),
      menuItem("Histogram", tabName = "histogram", icon = icon("chart-bar")),
      menuItem("Top GDP", tabName = "gdp", icon = icon("dollar-sign"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "choroplethMap",
              fluidRow(box(plotlyOutput("map"), width = 12))
      ),
      tabItem(tabName = "histogram",
              fluidRow(box(plotlyOutput("income"), width = 12))
      ),
      tabItem(tabName = "gdp",
              fluidRow(box(plotlyOutput("gdp"), width = 12))
      )
    )
  )
)

server <- function(input, output) { 
  
  output$map <- renderPlotly({
    fig <- plot_geo(gapminder_internet, locations = ~code) %>%
      add_trace(
        z = ~incomeperperson, zmin = 0, zmax = 50000, color = ~incomeperperson, colors = 'Blues', hoverinfo = 'text',
        text = ~paste(country, "\n$", incomeperperson), visible = TRUE) %>%
      add_trace(
        z = ~internetuserate, zmin = 0, zmax = max(internetuserate), color = ~internetuserate, hoverinfo = 'text',
        text = ~paste(country, "\n", internetuserate), visible = FALSE) %>%
      add_trace(
        z = ~urbanrate, color = ~urbanrate, zmin = 0, zmax = max(urbanrate), hoverinfo = 'text',
        text = ~paste(country, "\n", urbanrate, "%"), visible = FALSE)
    
    chart_types <- list(
      type = "buttons",
      direction = "right",
      xanchor = 'center',
      yanchor = "top",
      pad = list('r' = 10, 't' = 10, 'b' = 10),
      x = 0.5,
      y = 1.27,
      buttons = list(
        list(method = "restyle",
             args = list("visible", list(TRUE, FALSE, FALSE)),
             label = "Income Per Person"),
        list(method = "restyle",
             args = list("visible", list(FALSE, TRUE, FALSE)),
             label = "Internet User Rates"),
        list(method = "restyle",
             args = list("visible", list(FALSE, FALSE, TRUE)),
             label = "Urban Rates")))
    
    fig <- fig %>% layout(
      xaxis = list(domain = c(0.1, 1)),
      yaxis = list(title = "y"),
      updatemenus = list(chart_types),
      title = "Choropleth Map of the World") %>%
      hide_colorbar()
    
    fig
  })
  
  output$income <- renderPlotly({
    histogram <- plot_ly(gapminder_internet, marker = list(color = "lightgrey", line = list(color = "darkgrey", width = 2))) %>%
      add_trace(x = ~log(incomeperperson), type = "histogram", histnorm = "probability", name = 'Log income', visible = TRUE) %>%
      add_trace(x = ~internetuserate, type = "histogram", histnorm = "probability", visible = FALSE, name = 'Internet User Rate') %>%
      add_trace(x = ~urbanrate, type = "histogram", histnorm = "probability", name = 'Urban Rates', visible = FALSE)
    
    chart_types <- list(
      type = "buttons",
      direction = "right",
      xanchor = 'center',
      yanchor = "top",
      pad = list('r' = 10, 't' = 10, 'b' = 10),
      x = 0.5,
      y = 1.27,
      buttons = list(
        list(method = "restyle",
             args = list("visible", list(TRUE, FALSE, FALSE)),
             label = "Income Per Person"),
        list(method = "restyle",
             args = list("visible", list(FALSE, TRUE, FALSE)),
             label = "Internet User Rates"),
        list(method = "restyle",
             args = list("visible", list(FALSE, FALSE, TRUE)),
             label = "Urban Rates")))
    
    histogram <- histogram %>% layout(
      xaxis = list(domain = c(0.1, 1), title = ''),
      yaxis = list(title = ""),
      updatemenus = list(chart_types),
      title = "Histogram of Log Income, Internet User Rates, and Urban Rates",
      bargap = 0.1)
    
    histogram
  })
  
  output$gdp <- renderPlotly({
    plot_ly(gapminder_internet %>% arrange(desc
                                           