# Teaching Modules Shiny App

suppressPackageStartupMessages({
  if (!require("librarian")) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
    install.packages("librarian")
  }
  librarian::shelf(shiny, bslib, dplyr, ggplot2, leaflet, plotly, viridis)
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
        selectInput("map_color_by", "Color sites by:",
                   choices = c("Climate Zone" = "ClimateZ",
                             "Snow Fraction" = "snow_fraction",
                             "RBI (Flashiness)" = "RBI",
                             "Major Land Use" = "major_land",
                             "LTER Network" = "LTER"),
                   selected = "ClimateZ"),
        checkboxGroupInput("map_lter", "Filter by LTER:",
                         choices = NULL,
                         selected = NULL),
        checkboxInput("map_show_complete", "Show only complete data sites",
                     value = TRUE)
      ),

      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("Study Sites Across North America"),
          leafletOutput("site_map", height = 600)
        ),
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
        selectInput("activity1_sites", "Select Sites (up to 5):",
                   choices = NULL,
                   multiple = TRUE),
        p("Choose sites with varying snow fractions to compare",
          style = "font-size: 0.85em; color: #666; margin-top: -8px;"),
        hr(),
        h4("Filter Sites by Snow Category"),
        checkboxGroupInput("snow_category", "Show sites with:",
                          choices = c("Low Snow (0-60 days)" = "low",
                                    "Medium Snow (60-180 days)" = "medium",
                                    "High Snow (180+ days)" = "high"),
                          selected = c("low", "medium", "high")),
        hr(),
        h4("RCS vs RBI Plot Options"),
        selectInput("rcs_rbi_color", "Color by:",
                   choices = c("Snow Fraction" = "snow_fraction",
                             "Land Use" = "major_land",
                             "Mean Annual Precip" = "mean_annual_precip",
                             "Climate Zone" = "ClimateZ"),
                   selected = "snow_fraction")
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
    # Get list of North American LTER sites from harmonized data
    na_lter <- c("Canada", "USGS", "AND", "ARC", "BcCZO", "BNZ", "ColoradoAlpine",
                 "CZO-Catalina Jemez", "Catalina Jemez", "EastRiverSFA", "GRO", "HBR",
                 "Ipswitch(Carey)", "KNZ", "LMP", "LMP(Wymore)", "LUQ", "NWT", "PIE",
                 "Sagehen", "Sagehen(Sullivan)", "UMR", "UMR(Jankowski)",
                 "WalkerBranch", "Walker Branch")

    read.csv(file.path(data_path, "20260106_masterdata_discharge.csv"),
             stringsAsFactors = FALSE) %>%
      filter(LTER %in% na_lter) %>%
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

  # Filtered sites based on snow category
  filtered_sites <- reactive({
    req(input$snow_category)

    harmonized_complete() %>%
      filter(!is.na(snow_fraction), !is.na(mean_snow_days)) %>%
      mutate(
        snow_cat = case_when(
          mean_snow_days < 60 ~ "low",
          mean_snow_days >= 60 & mean_snow_days < 180 ~ "medium",
          mean_snow_days >= 180 ~ "high",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(snow_cat %in% input$snow_category) %>%
      arrange(Stream_Name)
  })

  # Update site choices based on snow category filter
  observe({
    req(filtered_sites())

    site_choices <- filtered_sites() %>%
      mutate(
        label = paste0(Stream_Name, " (", LTER, ", ", round(mean_snow_days, 0), " snow days)")
      ) %>%
      pull(label, name = Stream_Name)

    updateSelectInput(session, "activity1_sites",
                     choices = site_choices)
  })

  # Site map
  output$site_map <- renderLeaflet({
    req(input$map_lter, input$map_color_by)

    map_data <- harmonized_complete() %>%
      filter(LTER %in% input$map_lter) %>%
      filter(!is.na(Latitude), !is.na(Longitude))

    # High-contrast distinguishable color palette
    distinct_colors <- c(
      "#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00",
      "#ffff33", "#a65628", "#f781bf", "#66c2a5", "#fc8d62",
      "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494",
      "#b3b3b3", "#1b9e77", "#d95f02", "#7570b3", "#e7298a"
    )

    # Create color palette based on selected variable
    color_var <- map_data[[input$map_color_by]]

    if (input$map_color_by %in% c("RBI", "snow_fraction")) {
      # Numeric variables - use continuous color scale
      pal <- colorNumeric(
        palette = c("#d67e7e", "#e6c79c", "#7fb069", "#6b9bd1", "#5a7fa8"),
        domain = color_var,
        na.color = "#cccccc"
      )
      legend_title <- gsub("_", " ", input$map_color_by)
    } else {
      # Categorical variables - use factor colors
      pal <- colorFactor(
        palette = distinct_colors,
        domain = color_var,
        na.color = "#cccccc"
      )
      legend_title <- gsub("_", " ", input$map_color_by)
    }

    leaflet(map_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 6,
        fillColor = ~pal(color_var),
        color = "#2d2926",
        weight = 1,
        opacity = 0.8,
        fillOpacity = 0.7,
        popup = ~paste0("<b>", Stream_Name, "</b><br>",
                       "LTER: ", LTER, "<br>",
                       "RBI: ", round(RBI, 3), "<br>",
                       "RCS: ", round(recession_slope, 3), "<br>",
                       "Climate: ", ClimateZ, "<br>",
                       "Snow Fraction: ", round(snow_fraction, 3), "<br>",
                       "Mean Annual Precip: ", round(mean_annual_precip, 1), " mm"),
        label = ~Stream_Name
      ) %>%
      addLegend("bottomright",
               pal = pal,
               values = color_var,
               title = legend_title,
               opacity = 0.7)
  })

  # Hydrograph plot
  output$hydrograph_plot <- renderPlotly({
    req(input$activity1_sites)

    if (length(input$activity1_sites) == 0) {
      return(plotly_empty() %>%
        layout(title = list(text = "Select up to 5 sites to view hydrographs",
                           font = list(color = "#666"))))
    }

    if (length(input$activity1_sites) > 5) {
      return(plotly_empty() %>%
        layout(title = list(text = "Please select 5 or fewer sites",
                           font = list(color = "#d67e7e"))))
    }

    plot_data <- discharge_data() %>%
      filter(Stream_Name %in% input$activity1_sites)

    if (nrow(plot_data) == 0) {
      return(plotly_empty() %>%
        layout(title = list(text = "No discharge data available for selected sites",
                           font = list(color = "#666"))))
    }

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
      scale_color_manual(values = line_colors[1:length(input$activity1_sites)])

    ggplotly(p) %>%
      layout(
        paper_bgcolor = "#fefcfb",
        plot_bgcolor = "#ffffff",
        legend = list(orientation = "h", y = -0.2)
      )
  })

  # RCS vs RBI plot
  output$rcs_rbi_plot <- renderPlotly({
    req(input$rcs_rbi_color, input$snow_category)

    plot_data <- filtered_sites() %>%
      filter(!is.na(RBI), !is.na(recession_slope))

    if (nrow(plot_data) == 0) {
      return(plotly_empty() %>%
        layout(title = list(text = "No sites with complete RBI and RCS data in selected snow categories",
                           font = list(color = "#666"))))
    }

    # Use different color scales for numeric vs categorical
    if (input$rcs_rbi_color %in% c("snow_fraction", "mean_annual_precip")) {
      p <- ggplot(plot_data, aes(x = RBI, y = recession_slope,
                                 color = .data[[input$rcs_rbi_color]],
                                 text = paste0("Site: ", Stream_Name, "<br>",
                                             "LTER: ", LTER, "<br>",
                                             "RBI: ", round(RBI, 3), "<br>",
                                             "RCS: ", round(recession_slope, 3), "<br>",
                                             "Snow Days: ", round(mean_snow_days, 0)))) +
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
        scale_color_gradientn(colors = c("#d67e7e", "#e6c79c", "#7fb069", "#6b9bd1", "#5a7fa8"))
    } else {
      p <- ggplot(plot_data, aes(x = RBI, y = recession_slope,
                                 color = .data[[input$rcs_rbi_color]],
                                 text = paste0("Site: ", Stream_Name, "<br>",
                                             "LTER: ", LTER, "<br>",
                                             "RBI: ", round(RBI, 3), "<br>",
                                             "RCS: ", round(recession_slope, 3), "<br>",
                                             "Snow Days: ", round(mean_snow_days, 0)))) +
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
    }

    ggplotly(p, tooltip = "text") %>%
      layout(
        paper_bgcolor = "#fefcfb",
        plot_bgcolor = "#ffffff"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
