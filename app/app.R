library(shiny)
library(dplyr)
library(htmltools)
library(glue)
library(rlang)
library(shinythemes)
library(shinyWidgets)

# Data
load(url("https://raw.githubusercontent.com/nrennie/30DayChartChallenge/main/data/all_data.RData"))
all_years <- unique(all_data$year)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  theme = shinytheme("darkly"),

  titlePanel("30 Day Chart Challenge"),

  sidebarLayout(

    sidebarPanel(
      markdown("[Nicola Rennie](https://github.com/nrennie)

The 30 Day Chart Challenge is an annual data visualization challenge that takes place throughout April. Each day, participants create and share a chart based on a given daily prompt. The challenge encourages creativity, data storytelling, and improving data visualisation skills. It typically follows a structured theme for each week, such as comparisons, distributions, or time series. People usually share their work on social media using the hashtag #30DayChartChallenge.

My contributions can be found on [GitHub](https://github.com/nrennie/30DayChartChallenge), and you can use this Shiny app to explore my visualisations with links to code for each individual plot. You can also follow my attempts on [Mastodon](https://fosstodon.org/@nrennie) and [BlueSky](https://bsky.app/profile/nrennie.bsky.social).
"),
htmltools::hr(),
shinyWidgets::pickerInput(
  inputId = "year_select",
  "Choose a year:",
  choices = all_years
),
# choose a plot
shiny::uiOutput("select_img"),
shiny::textOutput("tools_used"),
htmltools::br(),
# display information
shiny::htmlOutput("code_link"),
htmltools::br(),
width = 6
    ),

mainPanel(
  shiny::htmlOutput("plot_img"),
  htmltools::br(),
  width = 6
)
  )
)

server <- function(input, output) {
  # Get data
  all_titles <- reactive({
    req(input$year_select)
      all_titles <- all_data %>%
        dplyr::filter(year == (input$year_select)) %>%
        dplyr::pull(prompt)
  })

  # Select title
  output$select_img <- renderUI({
    shinyWidgets::pickerInput(
      inputId = "plot_title",
      "Select a plot:",
      choices = all_titles()
    )
  })

  # Get data
  plot_data <- reactive({
    req(input$plot_title)
    dplyr::filter(all_data, year == input$year_select, prompt == input$plot_title)
  })

  ## Image display
  img_path <- shiny::reactive({
    glue::glue("{plot_data()$image_url}")
  })

  output$plot_img <- shiny::renderText({
    c('<img src="', img_path(), '" width="100%">')
  })

  ## Tools used
  output$tools_used <- shiny::renderText({
    glue::glue(
      "This plot was made with {plot_data()$tool}."
    )
  })

  ### Code link
  code_path <- shiny::reactive({
    if (plot_data()$tool == "R") {
      glue::glue(
        "https://github.com/nrennie/30DayChartChallenge/tree/main/{plot_data()$year}/scripts/day_{plot_data()$number}.R"
      )
    } else if (plot_data()$tool == "Python") {
      glue::glue(
        "https://github.com/nrennie/30DayChartChallenge/tree/main/{plot_data()$year}/scripts/day_{plot_data()$number}.py"
      )

    } else if (plot_data()$tool == "ObservableJS") {
      glue::glue(
        "https://github.com/nrennie/30DayChartChallenge/tree/main/{plot_data()$year}/scripts/day_{plot_data()$number}/index.qmd"
      )
    } else {
      NA
    }
  })

  output$code_link <- shiny::renderText({
    if (!is.na(code_path())) {
      glue::glue(
        '<b>Code is available at</b>: <a href="{code_path()}"  target="_blank">{code_path()}</a>.'
      )
    } else {
      "Code is not available for this visualisation."
    }
  })

}

shinyApp(ui = ui, server = server)
