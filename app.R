# Teaching Modules Shiny App

suppressPackageStartupMessages({
  if (!require("librarian")) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
    install.packages("librarian")
  }
  librarian::shelf(shiny, bslib, dplyr, ggplot2, leaflet, plotly)
})

data_path <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/CUAHSI-teaching-modules-shiny/data"

# Define color palette - earth-toned scheme
module_colors <- c(
  "primary"   = "#6b9bd1",  # soft blue
  "secondary" = "#5a7fa8",  # deeper blue
  "success"   = "#7fb069",  # sage green
  "danger"    = "#d67e7e",  # soft red/coral
  "warning"   = "#e6c79c"   # warm tan
)

# UI
ui <- page_navbar(
  title = "River Hydrology Teaching Module",
  theme = bs_theme(
    base_font = font_google("Work Sans"),
    bg = "#fefcfb",
    fg = "#2d2926",
    navbar_bg = "#ffffff",
    navbar_fg = "#2d2926",
    primary = "#6b9bd1",
    secondary = "#5a7fa8",
    success = "#7fb069",
    danger = "#d67e7e",
    "card-bg" = "#ffffff",
    "card-border-color" = "#d4e3f0"
  ),

  header = tags$head(
    tags$style(HTML("
      body {
        background: #fefcfb !important;
        font-family: 'Work Sans', sans-serif !important;
      }

      #map, .leaflet-container {
        background: #ffffff !important;
      }

      .card {
        border: 1px solid #d4e3f0 !important;
        box-shadow: 0 4px 20px rgba(107,155,209,0.08) !important;
        border-radius: 12px !important;
        background: #ffffff !important;
      }

      .card-header {
        background: #f5f9fc !important;
        border-bottom: 1px solid #d4e3f0 !important;
        color: #2d2926 !important;
        font-weight: 600 !important;
        border-radius: 12px 12px 0 0 !important;
      }

      .bslib-value-box {
        border: 1px solid #d4e3f0 !important;
        box-shadow: 0 4px 20px rgba(107,155,209,0.08) !important;
        border-radius: 12px !important;
        background: #ffffff !important;
      }

      .sidebar {
        background: #ffffff !important;
        border: 1px solid #d4e3f0 !important;
        box-shadow: 0 4px 20px rgba(107,155,209,0.08) !important;
        border-radius: 12px !important;
      }

      .navbar {
        box-shadow: 0 2px 8px rgba(107,155,209,0.1) !important;
        background-color: #ffffff !important;
        border-bottom: 1px solid #d4e3f0 !important;
      }

      .nav-link.active {
        color: #6b9bd1 !important;
        border-bottom: 2px solid #6b9bd1 !important;
      }

      .btn-primary {
        background-color: #6b9bd1 !important;
        border-color: #6b9bd1 !important;
        border-radius: 8px !important;
      }

      .card, .btn, .bslib-value-box {
        transition: all 0.3s ease !important;
      }
    "))
  ),

  # Overview Tab with Map
  nav_panel(
    "Overview",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Dataset Overview"),
        tableOutput("overview_table"),
        hr(),
        h4("Map Controls"),
        checkboxGroupInput("map_lter", "LTER Sites:",
                         choices = NULL,
                         selected = NULL),
        checkboxInput("map_show_complete", "Show only complete data sites",
                     value = TRUE)
      ),

      layout_columns(
        col_widths = c(12),
        card(
          card_header("Study Sites Across North America"),
          leafletOutput("site_map", height = 600)
        )
      ),

      layout_columns(
        col_widths = c(12),
        card(
          card_header("About This Module"),
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
                   "Köppen-Geiger classification, precipitation, snow fraction, land cover")
          )
        )
      )
    )
  ),

  # Activity 1 Tab
  nav_panel(
    "Activity 1: Hydrographs & Flashiness",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Site Selection"),
        selectInput("activity1_lter", "LTER:",
                   choices = NULL),
        selectInput("activity1_sites", "Sites (select up to 5):",
                   choices = NULL,
                   multiple = TRUE),
        p("Select up to 5 sites to compare in the hydrograph",
          style = "font-size: 0.85em; color: #666; margin-top: -8px;"),
        hr(),
        h4("Snow Fraction Filter"),
        sliderInput("snow_fraction", "Maximum Snow Days:",
                   min = 0, max = 365, value = 365, step = 10),
        hr(),
        h4("RCS vs RBI Plot Options"),
        selectInput("rcs_rbi_color", "Color by:",
                   choices = c("Land Use" = "major_land",
                             "Snow Fraction" = "snow_fraction",
                             "Mean Annual Precip" = "mean_annual_precip",
                             "Climate Zone" = "ClimateZ"),
                   selected = "major_land")
      ),

      navset_card_tab(
        nav_panel("Hydrographs",
          card(
            full_screen = TRUE,
            card_header("Discharge Time Series"),
            plotlyOutput("hydrograph_plot", height = 600)
          )
        ),
        nav_panel("RCS vs RBI",
          card(
            full_screen = TRUE,
            card_header("Recession Curve Slope vs Richards-Baker Flashiness Index"),
            plotlyOutput("rcs_rbi_plot", height = 600)
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

    # Earth-toned color palette for LTER sites
    lter_colors <- c(
      "#6b9bd1", "#7fb069", "#d67e7e", "#e6c79c", "#5a7fa8",
      "#8b9f7a", "#e8a083", "#c96e6e", "#d4a574", "#7cadd8",
      "#6fa85b", "#eeb394", "#ddb785", "#f2c2a7", "#e08585"
    )

    pal <- colorFactor(lter_colors, domain = map_data$LTER)

    leaflet(map_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 6,
        fillColor = ~pal(LTER),
        color = "#2d2926",
        weight = 1,
        opacity = 0.8,
        fillOpacity = 0.7,
        popup = ~paste0("<b>", Stream_Name, "</b><br>",
                       "LTER: ", LTER, "<br>",
                       "RBI: ", round(RBI, 3), "<br>",
                       "RCS: ", round(recession_slope, 3), "<br>",
                       "Climate: ", ClimateZ),
        label = ~Stream_Name
      ) %>%
      addLegend("bottomright",
               pal = pal,
               values = ~LTER,
               title = "LTER",
               opacity = 0.7)
  })

  # Hydrograph plot
  output$hydrograph_plot <- renderPlotly({
    req(input$activity1_sites)

    if (length(input$activity1_sites) > 5) {
      return(NULL)
    }

    plot_data <- discharge_data() %>%
      filter(Stream_Name %in% input$activity1_sites)

    # Earth-toned color palette for lines
    line_colors <- c("#6b9bd1", "#7fb069", "#d67e7e", "#e6c79c", "#5a7fa8")

    p <- ggplot(plot_data, aes(x = Date, y = Qcms, color = Stream_Name)) +
      geom_line(linewidth = 0.7, alpha = 0.8) +
      labs(x = "Date", y = "Discharge (cms)", color = "Site") +
      theme_minimal(base_family = "Work Sans") +
      theme(
        plot.background = element_rect(fill = "#fefcfb", color = NA),
        panel.background = element_rect(fill = "#ffffff", color = NA),
        panel.grid.major = element_line(color = "#d4e3f0", linewidth = 0.3),
        panel.grid.minor = element_line(color = "#d4e3f0", linewidth = 0.15),
        text = element_text(color = "#2d2926"),
        axis.text = element_text(color = "#2d2926"),
        legend.position = "bottom"
      ) +
      scale_color_manual(values = line_colors)

    ggplotly(p) %>%
      layout(
        paper_bgcolor = "#fefcfb",
        plot_bgcolor = "#ffffff"
      )
  })

  # RCS vs RBI plot
  output$rcs_rbi_plot <- renderPlotly({
    req(input$rcs_rbi_color)

    plot_data <- harmonized_complete() %>%
      filter(!is.na(RBI), !is.na(recession_slope))

    p <- ggplot(plot_data, aes(x = RBI, y = recession_slope,
                               color = .data[[input$rcs_rbi_color]],
                               text = paste0("Site: ", Stream_Name, "<br>",
                                           "RBI: ", round(RBI, 3), "<br>",
                                           "RCS: ", round(recession_slope, 3)))) +
      geom_point(size = 3, alpha = 0.7) +
      labs(x = "Richards-Baker Flashiness Index (RBI)",
           y = "Recession Curve Slope (RCS)",
           color = gsub("_", " ", input$rcs_rbi_color)) +
      theme_minimal(base_family = "Work Sans") +
      theme(
        plot.background = element_rect(fill = "#fefcfb", color = NA),
        panel.background = element_rect(fill = "#ffffff", color = NA),
        panel.grid.major = element_line(color = "#d4e3f0", linewidth = 0.3),
        panel.grid.minor = element_line(color = "#d4e3f0", linewidth = 0.15),
        text = element_text(color = "#2d2926"),
        axis.text = element_text(color = "#2d2926")
      ) +
      scale_color_viridis_d(option = "viridis", begin = 0.2, end = 0.9)

    ggplotly(p, tooltip = "text") %>%
      layout(
        paper_bgcolor = "#fefcfb",
        plot_bgcolor = "#ffffff"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
