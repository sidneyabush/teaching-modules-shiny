# Teaching Modules Shiny App

if (!require("librarian")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  install.packages("librarian")
}
librarian::shelf(shiny, dplyr, ggplot2, leaflet, plotly)

data_path <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/CUAHSI-teaching-modules-shiny/data"

# UI
ui <- navbarPage(
  title = "River Hydrology Teaching Module",

  # Overview Tab
  tabPanel("Overview",
    fluidPage(
      h2("Teaching Module: River Hydrology and Watershed Controls"),
      br(),
      fluidRow(
        column(6,
          h3("Dataset Overview"),
          tableOutput("overview_table")
        ),
        column(6,
          h3("About This Module"),
          p("This interactive module explores river hydrology metrics and their relationship
            to watershed characteristics across North American rivers."),
          br(),
          h4("Key Metrics:"),
          tags$ul(
            tags$li(strong("RBI (Richards-Baker Flashiness Index):"),
                   "Measures how rapidly streamflow changes over time"),
            tags$li(strong("Recession Curve Slope (RCS):"),
                   "Describes how quickly discharge decreases after peak flow"),
            tags$li(strong("Climate & Land Use:"),
                   "Köppen-Geiger classification, precipitation, land cover")
          )
        )
      )
    )
  ),

  # Maps Tab
  tabPanel("Site Maps",
    fluidPage(
      h2("Study Sites Across North America"),
      br(),
      fluidRow(
        column(3,
          wellPanel(
            h4("Map Controls"),
            checkboxGroupInput("map_lter", "LTER Sites:",
                             choices = NULL,
                             selected = NULL),
            checkboxInput("map_show_complete", "Show only complete data sites",
                         value = TRUE)
          )
        ),
        column(9,
          leafletOutput("site_map", height = 600)
        )
      )
    )
  ),

  # Activity 1 Tab
  tabPanel("Activity 1: Hydrographs & Flashiness",
    fluidPage(
      h2("Exploring Discharge Patterns and Flashiness"),
      br(),

      fluidRow(
        column(3,
          wellPanel(
            h4("Site Selection"),
            selectInput("activity1_lter", "LTER:",
                       choices = NULL),
            selectInput("activity1_sites", "Sites (select up to 5):",
                       choices = NULL,
                       multiple = TRUE),
            br(),
            h4("Snow Fraction Filter"),
            sliderInput("snow_fraction", "Maximum Snow Days:",
                       min = 0, max = 365, value = 365, step = 10)
          )
        ),
        column(9,
          tabsetPanel(
            tabPanel("Hydrographs",
              plotlyOutput("hydrograph_plot", height = 500)
            ),
            tabPanel("RCS vs RBI",
              fluidRow(
                column(12,
                  selectInput("rcs_rbi_color", "Color by:",
                             choices = c("Land Use" = "major_land",
                                       "Snow Fraction" = "snow_fraction",
                                       "Mean Annual Precip" = "mean_annual_precip",
                                       "Climate Zone" = "ClimateZ"),
                             selected = "major_land")
                )
              ),
              plotlyOutput("rcs_rbi_plot", height = 500)
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Load harmonized data
  harmonized_complete <- reactive({
    read.csv(file.path(data_path, "harmonized_north_america_complete.csv"),
             stringsAsFactors = FALSE)
  })

  harmonized_partial <- reactive({
    read.csv(file.path(data_path, "harmonized_north_america_partial.csv"),
             stringsAsFactors = FALSE)
  })

  discharge_data <- reactive({
    read.csv(file.path(data_path, "20260106_masterdata_discharge.csv"),
             stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date),
             Stream_ID = paste(LTER, Stream_Name, sep = "_"))
  })

  # Overview table
  output$overview_table <- renderTable({
    data.frame(
      Metric = c("Total North American Sites",
                 "Sites with Discharge Data",
                 "Sites with Complete Data",
                 "LTER Networks Represented"),
      Count = c(nrow(harmonized_partial()),
               sum(!is.na(harmonized_partial()$RBI)),
               nrow(harmonized_complete()),
               length(unique(harmonized_complete()$LTER)))
    )
  })

  # Update LTER choices for maps
  observe({
    lter_choices <- sort(unique(harmonized_complete()$LTER))
    updateCheckboxGroupInput(session, "map_lter",
                            choices = lter_choices,
                            selected = lter_choices)
  })

  # Update LTER choices for Activity 1
  observe({
    lter_choices <- sort(unique(harmonized_complete()$LTER))
    updateSelectInput(session, "activity1_lter",
                     choices = c("All" = "all", lter_choices))
  })

  # Update site choices when LTER changes
  observe({
    req(input$activity1_lter)

    if (input$activity1_lter == "all") {
      sites <- harmonized_complete()$Stream_Name
    } else {
      sites <- harmonized_complete() %>%
        filter(LTER == input$activity1_lter) %>%
        pull(Stream_Name)
    }

    updateSelectInput(session, "activity1_sites",
                     choices = sort(unique(sites)))
  })

  # Site map
  output$site_map <- renderLeaflet({
    req(input$map_lter)

    map_data <- harmonized_complete() %>%
      filter(LTER %in% input$map_lter) %>%
      filter(!is.na(Latitude), !is.na(Longitude))

    leaflet(map_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 5,
        color = ~colorFactor("Set1", LTER)(LTER),
        popup = ~paste0("<b>", Stream_Name, "</b><br>",
                       "LTER: ", LTER, "<br>",
                       "RBI: ", round(RBI, 3), "<br>",
                       "RCS: ", round(recession_slope, 3), "<br>",
                       "Climate: ", ClimateZ),
        label = ~Stream_Name
      ) %>%
      addLegend("bottomright",
               pal = colorFactor("Set1", map_data$LTER),
               values = ~LTER,
               title = "LTER")
  })

  # Hydrograph plot
  output$hydrograph_plot <- renderPlotly({
    req(input$activity1_sites)

    if (length(input$activity1_sites) > 5) {
      return(NULL)
    }

    plot_data <- discharge_data() %>%
      filter(Stream_Name %in% input$activity1_sites)

    p <- ggplot(plot_data, aes(x = Date, y = Qcms, color = Stream_Name)) +
      geom_line() +
      labs(x = "Date", y = "Discharge (cms)", color = "Site") +
      theme_minimal() +
      theme(legend.position = "bottom")

    ggplotly(p)
  })

  # RCS vs RBI plot
  output$rcs_rbi_plot <- renderPlotly({
    req(input$rcs_rbi_color)

    plot_data <- harmonized_complete() %>%
      filter(!is.na(RBI), !is.na(recession_slope))

    p <- ggplot(plot_data, aes(x = RBI, y = recession_slope,
                               color = .data[[input$rcs_rbi_color]])) +
      geom_point(size = 3, alpha = 0.7) +
      labs(x = "Richards-Baker Flashiness Index (RBI)",
           y = "Recession Curve Slope (RCS)",
           color = input$rcs_rbi_color) +
      theme_minimal()

    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
