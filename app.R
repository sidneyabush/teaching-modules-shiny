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
        h4("Filter Sites"),
        selectInput("filter_by", "Filter sites by:",
                   choices = c("Snow Fraction" = "snow",
                             "Climate Zone" = "climate",
                             "Land Use" = "landuse"),
                   selected = "snow"),
        conditionalPanel(
          condition = "input.filter_by == 'snow'",
          checkboxGroupInput("snow_category", "Snow categories:",
                            choices = c("Low (0-60 days/year)" = "low",
                                      "Medium (60-180 days/year)" = "medium",
                                      "High (180+ days/year)" = "high"),
                            selected = c("low", "medium", "high"))
        ),
        conditionalPanel(
          condition = "input.filter_by == 'climate'",
          checkboxGroupInput("climate_filter", "Climate zones:",
                            choices = NULL,
                            selected = NULL)
        ),
        conditionalPanel(
          condition = "input.filter_by == 'landuse'",
          checkboxGroupInput("landuse_filter", "Land use types:",
                            choices = NULL,
                            selected = NULL)
        ),
        hr(),
        h4("Site Selection"),
        selectInput("activity1_sites", "Select Sites (up to 5):",
                   choices = NULL,
                   multiple = TRUE),
        p("Choose sites from different categories to compare",
          style = "font-size: 0.85em; color: #666; margin-top: -8px;"),
        hr(),
        h4("RCS vs RBI Plot Options"),
        selectInput("rcs_rbi_color", "Color by:",
                   choices = c("Snow Category" = "snow_cat",
                             "Snow Fraction" = "snow_fraction",
                             "Climate Zone" = "ClimateZ",
                             "Land Use" = "major_land",
                             "Mean Annual Precip" = "mean_annual_precip"),
                   selected = "snow_cat")
      ),

      navset_card_tab(
        nav_panel("Hydrographs",
          card(
            full_screen = TRUE,
            card_header("Discharge Time Series - Compare Sites with Different Snow Influence"),
            plotlyOutput("hydrograph_plot", height = 600)
          )
        ),
        nav_panel("RCS vs RBI",
          card(
            full_screen = TRUE,
            card_header("Recession Curve Slope vs Flashiness Index"),
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

  # Update climate filter choices
  observe({
    climate_zones <- harmonized_complete() %>%
      filter(!is.na(ClimateZ)) %>%
      pull(ClimateZ) %>%
      unique() %>%
      sort()

    updateCheckboxGroupInput(session, "climate_filter",
                            choices = climate_zones,
                            selected = climate_zones)
  })

  # Update land use filter choices
  observe({
    landuse_types <- harmonized_complete() %>%
      filter(!is.na(major_land)) %>%
      pull(major_land) %>%
      unique() %>%
      sort()

    updateCheckboxGroupInput(session, "landuse_filter",
                            choices = landuse_types,
                            selected = landuse_types)
  })

  # Add snow category to harmonized data
  harmonized_with_categories <- reactive({
    harmonized_complete() %>%
      filter(!is.na(RBI), !is.na(recession_slope)) %>%
      mutate(
        snow_cat = case_when(
          is.na(mean_snow_days) ~ "Unknown",
          mean_snow_days < 60 ~ "Low (0-60 days)",
          mean_snow_days >= 60 & mean_snow_days < 180 ~ "Medium (60-180 days)",
          mean_snow_days >= 180 ~ "High (180+ days)",
          TRUE ~ "Unknown"
        ),
        snow_cat = factor(snow_cat, levels = c("Low (0-60 days)", "Medium (60-180 days)", "High (180+ days)", "Unknown"))
      )
  })

  # Filtered sites based on selected filter type
  filtered_sites <- reactive({
    req(input$filter_by)

    data <- harmonized_with_categories()

    if (input$filter_by == "snow") {
      req(input$snow_category)

      data <- data %>%
        filter(
          (snow_cat == "Low (0-60 days)" & "low" %in% input$snow_category) |
          (snow_cat == "Medium (60-180 days)" & "medium" %in% input$snow_category) |
          (snow_cat == "High (180+ days)" & "high" %in% input$snow_category)
        )

    } else if (input$filter_by == "climate") {
      req(input$climate_filter)
      data <- data %>% filter(ClimateZ %in% input$climate_filter)

    } else if (input$filter_by == "landuse") {
      req(input$landuse_filter)
      data <- data %>% filter(major_land %in% input$landuse_filter)
    }

    data
  })

  # Update site choices based on filters
  observe({
    req(filtered_sites())

    if (input$filter_by == "snow") {
      site_choices <- filtered_sites() %>%
        arrange(snow_cat, Stream_Name) %>%
        mutate(
          label = paste0(Stream_Name, " [", gsub(" \\(.*", "", snow_cat), ", ", LTER, "]")
        ) %>%
        pull(label, name = Stream_ID)

    } else if (input$filter_by == "climate") {
      site_choices <- filtered_sites() %>%
        arrange(ClimateZ, Stream_Name) %>%
        mutate(
          label = paste0(Stream_Name, " [", ClimateZ, ", ", LTER, "]")
        ) %>%
        pull(label, name = Stream_ID)

    } else {
      site_choices <- filtered_sites() %>%
        arrange(major_land, Stream_Name) %>%
        mutate(
          label = paste0(Stream_Name, " [", major_land, ", ", LTER, "]")
        ) %>%
        pull(label, name = Stream_ID)
    }

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

    # Get site info for selected Stream_IDs
    selected_sites <- harmonized_with_categories() %>%
      filter(Stream_ID %in% input$activity1_sites) %>%
      select(Stream_ID, Stream_Name, LTER, snow_cat)

    # Filter discharge data by Stream_ID
    plot_data <- discharge_data() %>%
      filter(Stream_ID %in% input$activity1_sites) %>%
      left_join(selected_sites, by = "Stream_ID") %>%
      mutate(site_label = paste0(Stream_Name, " (", gsub(" \\(.*", "", snow_cat), ")"))

    if (nrow(plot_data) == 0) {
      return(plotly_empty() %>%
        layout(title = list(text = "No discharge data available for selected sites",
                           font = list(color = "#666"))))
    }

    # Earth-toned color palette for lines
    line_colors <- c("#6b9bd1", "#7fb069", "#d67e7e", "#e6c79c", "#5a7fa8")

    p <- ggplot(plot_data, aes(x = Date, y = Qcms, color = site_label)) +
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
      scale_color_manual(values = line_colors[1:length(unique(plot_data$site_label))])

    ggplotly(p) %>%
      layout(
        paper_bgcolor = "#fefcfb",
        plot_bgcolor = "#ffffff",
        legend = list(orientation = "h", y = -0.2)
      )
  })

  # RCS vs RBI plot
  output$rcs_rbi_plot <- renderPlotly({
    req(input$rcs_rbi_color)

    plot_data <- filtered_sites()

    if (nrow(plot_data) == 0) {
      return(plotly_empty() %>%
        layout(title = list(text = "No sites match the selected filters",
                           font = list(color = "#666"))))
    }

    # Use different color scales for numeric vs categorical
    if (input$rcs_rbi_color %in% c("snow_fraction", "mean_annual_precip")) {
      p <- ggplot(plot_data, aes(x = RBI, y = recession_slope,
                                 color = .data[[input$rcs_rbi_color]],
                                 text = paste0("Site: ", Stream_Name, "<br>",
                                             "LTER: ", LTER, "<br>",
                                             "Snow: ", snow_cat, "<br>",
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
        scale_color_gradientn(colors = c("#d67e7e", "#e6c79c", "#7fb069", "#6b9bd1", "#5a7fa8"))
    } else if (input$rcs_rbi_color == "snow_cat") {
      # Special colors for snow categories
      p <- ggplot(plot_data, aes(x = RBI, y = recession_slope,
                                 color = snow_cat,
                                 text = paste0("Site: ", Stream_Name, "<br>",
                                             "LTER: ", LTER, "<br>",
                                             "Snow: ", snow_cat, "<br>",
                                             "RBI: ", round(RBI, 3), "<br>",
                                             "RCS: ", round(recession_slope, 3)))) +
        geom_point(size = 3, alpha = 0.7) +
        labs(x = "Richards-Baker Flashiness Index (RBI)",
             y = "Recession Curve Slope (RCS)",
             color = "Snow Category") +
        theme_minimal(base_family = "Work Sans") +
        theme(
          plot.background = element_rect(fill = "#fefcfb", color = NA),
          panel.background = element_rect(fill = "#ffffff", color = NA),
          panel.grid.major = element_line(color = "#d4e3f0", linewidth = 0.3),
          panel.grid.minor = element_line(color = "#d4e3f0", linewidth = 0.15),
          text = element_text(color = "#2d2926"),
          axis.text = element_text(color = "#2d2926")
        ) +
        scale_color_manual(values = c("Low (0-60 days)" = "#d67e7e",
                                     "Medium (60-180 days)" = "#e6c79c",
                                     "High (180+ days)" = "#6b9bd1",
                                     "Unknown" = "#999999"))
    } else {
      p <- ggplot(plot_data, aes(x = RBI, y = recession_slope,
                                 color = .data[[input$rcs_rbi_color]],
                                 text = paste0("Site: ", Stream_Name, "<br>",
                                             "LTER: ", LTER, "<br>",
                                             "Snow: ", snow_cat, "<br>",
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
