#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

# https://shiny.posit.co/blog/posts/bslib-dashboards/#hello-dashboards

library(shiny)
library(bslib)
library(bsicons)
library(thematic)
library(readr)
library(dplyr)
library(ggplot2)

# Setup -------------------------------------------------------------------

#consumption data - 2024 AETR
source_consumption <- "https://raw.githubusercontent.com/acep-uaf/aetr-web-book-2024/main/data/working/consumption/consumption_long.csv"
consumption <- readr::read_csv(url(source_consumption))
#filter data <= 2019
consumption_filtered <- consumption %>%
  filter(year <= 2019, class %in% c("Residential", "Commercial", "Other")) %>%
  mutate(across(where(is.numeric), round, 2))

# Turn on thematic for theme-matched plots
thematic::thematic_shiny(font = "auto")
theme_set(theme_bw(base_size = 16))

total_utilities <- consumption_filtered %>%
  group_by(acep_region) %>%
  summarize(total = length(unique(utility_name)))

# UI ----------------------------------------------------------------------

ui <- page_sidebar(
  title = "Alaska Energy Trends Report Price Comparison",
  sidebar = sidebar(
    checkboxInput(
      inputId = "make_art", label = "Turn Data into Art",
      value = FALSE
    )
  ),
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Coastal Utilities",
      value = total_utilities[total_utilities$acep_region == "Coastal",2],
      showcase = shiny::icon("sailboat"),
      theme_color = "text-success"
    ),
    value_box(
      title = "Rural Remote Utilities",
      value = total_utilities[total_utilities$acep_region == "Rural Remote",2],
      showcase = shiny::icon("tower-cell"),
      theme_color = "text-purple"
    ),
    value_box(
      title = "Railbelt Utilities",
      value = total_utilities[total_utilities$acep_region == "Railbelt",2],
      showcase = shiny::icon("sitemap"),
      theme_color = "text-yellow"
    )
  ),
  layout_columns(
    card(
      full_screen = TRUE,
      card_header("Coastal"),
      plotOutput("coastal")
    ),
    card(
      full_screen = TRUE,
      card_header("Rural Remote"),
      plotOutput("rural_remote")
    )
  ),
  card(
    full_screen = TRUE,
    card_header("Railbelt"),
    plotOutput("railbelt")
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
  # gg_plot <- reactive({
  #   ggplot(consumption_filtered, aes(year, price, size = customers)) +
  #     geom_point() #+
  #     #theme_bw(base_size = 16) +
  #     #theme(axis.title = element_blank())
  # })

  output$coastal <- renderPlot({
    if(input$make_art == FALSE)
    consumption_filtered %>%
      filter(acep_region == "Coastal") %>%
      ggplot(aes(year, price, colour = class, size = customers)) +
      geom_point() +
      scale_x_continuous(breaks = c(2011:2019)) +
      ylab("price per KWh\n") +
      xlab("\nyear")
    else
      consumption_filtered %>%
      filter(acep_region == "Coastal") %>%
      ggplot(aes(year, price, colour = class, size = customers)) +
      geom_point(show.legend = FALSE) +
      coord_polar() +
      theme_void()
    })

  output$rural_remote <- renderPlot({
    if(input$make_art == FALSE)
      consumption_filtered %>%
      filter(acep_region == "Rural Remote") %>%
      ggplot(aes(year, price, colour = class, size = customers)) +
      geom_point() +
      scale_x_continuous(breaks = c(2011:2019)) +
      ylab("price per KWh\n") +
      xlab("\nyear")
    else
      consumption_filtered %>%
      filter(acep_region == "Rural Remote") %>%
      ggplot(aes(year, price, colour = class, size = customers,
                 xend = sales, yend = revenue)) +
      geom_segment(show.legend = FALSE) +
      coord_polar() +
      theme_void()
  })
  output$railbelt <- renderPlot({
    if(input$make_art == FALSE)
      consumption_filtered %>%
      filter(acep_region == "Railbelt") %>%
      ggplot(aes(year, price, colour = class, size = customers)) +
      geom_point() +
      scale_x_continuous(breaks = c(2011:2019)) +
      ylab("price per KWh\n") +
      xlab("\nyear")
    else
      consumption_filtered %>%
      filter(acep_region == "Railbelt") %>%
      ggplot(aes(year, price, fill = class)) +
      geom_area(alpha = 1, show.legend = FALSE) +
      #coord_polar() +
      theme_void()
  })
}


# Shiny App ---------------------------------------------------------------

shinyApp(ui, server)
