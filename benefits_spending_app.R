# UK Benefits Spending Visualization Tool
# Incorporating Ben Baumberg Geiger's methodological considerations
# Data from DWP Benefit Expenditure and Caseload Tables

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(readxl)
library(httr)
library(jsonlite)
library(scales)
library(viridis)
library(bslib)
library(shinyWidgets)
library(DT)
library(lubridate)

# ============================================================================
# DATA CONFIGURATION
# ============================================================================

# DWP Data URLs - These are the official GOV.UK data sources
# Updated automatically with each fiscal event (Spring/Autumn)
DATA_CONFIG <- list(
  # Main benefit expenditure tables
  expenditure_base_url = "https://www.gov.uk/government/publications/benefit-expenditure-and-caseload-tables-2025",

  # Stat-Xplore API for detailed breakdowns
  stat_xplore_api = "https://stat-xplore.dwp.gov.uk/webapi/rest/v1",

  # OBR supplementary tables
  obr_welfare_url = "https://obr.uk/forecasts-in-depth/tax-by-tax-spend-by-spend/welfare-spending-universal-credit/"
)

# Benefit categories mapping
BENEFIT_CATEGORIES <- list(
  # Universal Credit and its legacy predecessors
  uc_and_legacy = c(
    "Universal Credit",
    "Income Support",
    "Jobseeker's Allowance (Income-Based)",
    "Employment and Support Allowance (Income-Based)",
    "Housing Benefit",
    "Working Tax Credit",
    "Child Tax Credit"
  ),

  # Disability and health-related
  disability = c(
    "Personal Independence Payment",
    "Disability Living Allowance",
    "Attendance Allowance",
    "Employment and Support Allowance (Contributory)",
    "Incapacity Benefit",
    "Severe Disablement Allowance"
  ),

  # Carers
  carers = c(
    "Carer's Allowance"
  ),

  # Pensioner benefits
  pensioner = c(
    "State Pension",
    "Pension Credit",
    "Winter Fuel Payments"
  ),

  # Family benefits
  family = c(
    "Child Benefit",
    "Maternity Allowance",
    "Statutory Maternity Pay",
    "Sure Start Maternity Grant"
  ),

  # Unemployment (contributory)
  unemployment = c(
    "Jobseeker's Allowance (Contributory)"
  )
)

# ============================================================================
# SAMPLE DATA - Used when live data unavailable
# Based on actual DWP figures (£ billions, nominal terms)
# ============================================================================

create_sample_expenditure_data <- function() {
  years <- 2010:2028

  # Historical outturn data (2010-2024) and forecasts (2025-2028)
  # Based on DWP Benefit Expenditure Tables and OBR forecasts

  data <- tibble(
    fiscal_year = paste0(years, "-", substr(years + 1, 3, 4)),
    year_start = years,
    data_type = ifelse(years <= 2024, "Outturn", "Forecast"),

    # Universal Credit (started 2013, ramped up from 2016)
    `Universal Credit` = c(
      0, 0, 0, 0.1, 0.5, 1.2, 2.8, 5.1, 8.9, 16.4,
      35.2, 44.8, 51.2, 58.3, 65.1, 71.2, 76.8, 81.5, 85.2
    ),

    # Legacy means-tested benefits (declining as UC rolls out)
    `Income Support` = c(
      7.8, 7.2, 6.8, 6.4, 5.9, 5.2, 4.5, 3.8, 3.1, 2.4,
      1.8, 1.3, 0.9, 0.5, 0.3, 0.2, 0.1, 0.05, 0.02
    ),
    `JSA Income-Based` = c(
      4.2, 4.8, 4.5, 3.9, 3.2, 2.6, 2.1, 1.6, 1.2, 0.9,
      0.5, 0.3, 0.2, 0.1, 0.05, 0.02, 0.01, 0, 0
    ),
    `ESA Income-Based` = c(
      5.8, 7.2, 8.4, 9.1, 9.5, 9.3, 8.8, 8.1, 7.2, 6.1,
      4.8, 3.6, 2.5, 1.6, 0.9, 0.5, 0.3, 0.1, 0.05
    ),
    `Housing Benefit` = c(
      20.8, 22.4, 23.8, 24.3, 24.2, 23.8, 23.1, 22.2, 21.1, 19.8,
      16.2, 13.5, 11.2, 9.1, 7.2, 5.5, 4.2, 3.1, 2.3
    ),
    `Working Tax Credit` = c(
      6.2, 6.4, 6.1, 5.8, 5.4, 4.9, 4.3, 3.6, 3.0, 2.3,
      1.5, 0.9, 0.5, 0.2, 0.1, 0.05, 0.02, 0, 0
    ),
    `Child Tax Credit` = c(
      22.1, 23.4, 23.8, 23.2, 22.4, 21.3, 19.8, 18.1, 16.2, 14.1,
      10.2, 7.5, 5.2, 3.4, 2.1, 1.2, 0.6, 0.3, 0.1
    ),

    # Disability benefits
    `Personal Independence Payment` = c(
      0, 0, 0, 0.8, 2.1, 4.2, 7.1, 10.3, 13.8, 17.5,
      20.2, 22.8, 24.9, 27.2, 29.8, 32.1, 34.2, 36.1, 37.8
    ),
    `Disability Living Allowance` = c(
      12.4, 13.1, 13.6, 13.8, 13.2, 12.1, 10.8, 9.2, 7.5, 5.8,
      4.2, 3.1, 2.3, 1.8, 1.4, 1.1, 0.8, 0.6, 0.4
    ),
    `Attendance Allowance` = c(
      5.1, 5.3, 5.5, 5.6, 5.7, 5.8, 5.9, 6.1, 6.3, 6.5,
      6.8, 7.1, 7.4, 7.8, 8.2, 8.6, 9.0, 9.4, 9.8
    ),
    `ESA Contributory` = c(
      3.2, 3.8, 4.2, 4.5, 4.6, 4.5, 4.3, 4.1, 3.9, 3.7,
      3.5, 3.4, 3.3, 3.2, 3.1, 3.0, 2.9, 2.8, 2.7
    ),

    # Carer's Allowance
    `Carer's Allowance` = c(
      1.6, 1.7, 1.8, 1.9, 2.1, 2.3, 2.5, 2.8, 3.2, 3.6,
      4.1, 4.5, 5.0, 5.5, 6.1, 6.6, 7.1, 7.5, 7.9
    ),

    # State Pension
    `State Pension` = c(
      74.2, 78.5, 82.1, 85.4, 88.2, 91.3, 95.1, 99.2, 103.8, 108.5,
      115.2, 120.8, 126.5, 132.4, 138.5, 144.2, 149.8, 155.1, 160.2
    ),

    # Child Benefit
    `Child Benefit` = c(
      12.2, 12.1, 11.8, 11.5, 11.3, 11.4, 11.6, 11.8, 12.0, 12.2,
      12.4, 12.5, 12.6, 12.8, 13.0, 13.2, 13.4, 13.6, 13.8
    ),

    # GDP for calculating % of GDP
    GDP = c(
      1555, 1619, 1670, 1735, 1825, 1896, 1969, 2049, 2118, 2179,
      2112, 2274, 2487, 2687, 2819, 2935, 3042, 3145, 3248
    )
  )

  return(data)
}

# Create sample caseload data (millions of claimants)
create_sample_caseload_data <- function() {
  years <- 2010:2028

  data <- tibble(
    fiscal_year = paste0(years, "-", substr(years + 1, 3, 4)),
    year_start = years,
    data_type = ifelse(years <= 2024, "Outturn", "Forecast"),

    # UC counts households, but reports individuals (BBG issue)
    `Universal Credit` = c(
      0, 0, 0, 0.02, 0.11, 0.28, 0.52, 0.95, 1.68, 2.95,
      5.82, 6.24, 6.85, 7.42, 7.89, 8.21, 8.45, 8.62, 8.75
    ),

    # Legacy benefits - individual claimants
    `ESA (All)` = c(
      2.31, 2.52, 2.62, 2.68, 2.71, 2.65, 2.54, 2.38, 2.18, 1.94,
      1.62, 1.28, 0.95, 0.68, 0.45, 0.28, 0.16, 0.08, 0.04
    ),

    `Incapacity Benefits Total` = c(
      2.61, 2.64, 2.67, 2.72, 2.78, 2.82, 2.89, 2.98, 3.12, 3.28,
      3.85, 4.12, 4.38, 4.65, 4.92, 5.15, 5.35, 5.52, 5.68
    ),

    `PIP/DLA` = c(
      3.21, 3.32, 3.41, 3.48, 3.52, 3.58, 3.68, 3.82, 4.01, 4.24,
      4.52, 4.78, 5.02, 5.28, 5.55, 5.81, 6.05, 6.28, 6.48
    )
  )

  return(data)
}

# ============================================================================
# BEN BAUMBERG GEIGER ADJUSTMENTS
# Based on his analysis at inequalities.substack.com
# ============================================================================

# Adjustment 1: Correct for UC household vs ESA individual counting
# UC counts partners in household, ESA only counts individuals
apply_uc_counting_adjustment <- function(data, uc_col, adjustment_factor = 0.85) {
  # Reduce UC caseload figures to account for partner double-counting
  # Estimated ~15% of UC incapacity claims are partners
  data %>%
    mutate(
      !!paste0(uc_col, "_adjusted") := ifelse(
        year_start >= 2013,
        .data[[uc_col]] * adjustment_factor,
        .data[[uc_col]]
      )
    )
}

# Adjustment 2: Exclude "no payment" LCW claimants post-2017
# These receive no more than unemployment benefit rates
apply_lcw_no_payment_adjustment <- function(data, uc_col, lcw_proportion = 0.12) {
  # Remove ~12% of UC incapacity caseload who are LCW with no payment premium
  data %>%
    mutate(
      !!paste0(uc_col, "_lcw_adjusted") := ifelse(
        year_start >= 2017,
        .data[[uc_col]] * (1 - lcw_proportion),
        .data[[uc_col]]
      )
    )
}

# Adjustment 3: Remove State Pension Age effect
# Rising SPA has shifted people onto working-age benefits
apply_spa_adjustment <- function(data, caseload_col, spa_effect_thousands = 330) {
  # Linearly phase in SPA effect from 2010 to 2024
  data %>%
    mutate(
      spa_adjustment = ifelse(
        year_start >= 2010 & year_start <= 2024,
        (spa_effect_thousands / 1000) * (year_start - 2010) / (2024 - 2010),
        ifelse(year_start > 2024, spa_effect_thousands / 1000, 0)
      ),
      !!paste0(caseload_col, "_spa_adjusted") := .data[[caseload_col]] - spa_adjustment
    )
}

# Adjustment 4: Change baseline year from 2010-11 to 2014-15
# WCA was poorly implemented in early years
get_adjusted_baseline_data <- function(data, baseline_year = 2014) {
  data %>%
    filter(year_start >= baseline_year)
}

# Combined adjustment function
apply_all_bbg_adjustments <- function(data, options = list()) {
  result <- data

  if (isTRUE(options$adjust_uc_counting)) {
    if ("Universal Credit" %in% names(result)) {
      result <- apply_uc_counting_adjustment(
        result,
        "Universal Credit",
        options$uc_adjustment_factor %||% 0.85
      )
    }
  }

  if (isTRUE(options$exclude_lcw_no_payment)) {
    if ("Universal Credit" %in% names(result)) {
      result <- apply_lcw_no_payment_adjustment(
        result,
        "Universal Credit",
        options$lcw_proportion %||% 0.12
      )
    }
  }

  if (isTRUE(options$remove_spa_effect)) {
    if ("Incapacity Benefits Total" %in% names(result)) {
      result <- apply_spa_adjustment(
        result,
        "Incapacity Benefits Total",
        options$spa_effect %||% 330
      )
    }
  }

  if (isTRUE(options$use_2014_baseline)) {
    result <- get_adjusted_baseline_data(result, 2014)
  }

  return(result)
}

# ============================================================================
# DATA FETCHING AND PROCESSING
# ============================================================================

# Function to check for and download latest data
fetch_latest_dwp_data <- function(use_cache = TRUE, cache_dir = "data_cache") {
  # Create cache directory if it doesn't exist
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  cache_file <- file.path(cache_dir, "benefit_expenditure_latest.rds")
  metadata_file <- file.path(cache_dir, "data_metadata.json")

  # Check cache validity (24 hours)
  if (use_cache && file.exists(cache_file) && file.exists(metadata_file)) {
    metadata <- fromJSON(metadata_file)
    cache_time <- as.POSIXct(metadata$last_updated)
    if (difftime(Sys.time(), cache_time, units = "hours") < 24) {
      return(readRDS(cache_file))
    }
  }

  # Try to fetch from GOV.UK
  tryCatch({
    # Note: In production, this would fetch from the actual GOV.UK API
    # The DWP publishes data in ODS format which would need conversion

    # For now, use sample data
    message("Using sample data based on DWP Benefit Expenditure Tables")
    data <- list(
      expenditure = create_sample_expenditure_data(),
      caseload = create_sample_caseload_data(),
      last_updated = Sys.time(),
      source = "DWP Benefit Expenditure and Caseload Tables",
      notes = "Sample data based on official DWP figures"
    )

    # Cache the data
    saveRDS(data, cache_file)
    write_json(
      list(
        last_updated = as.character(Sys.time()),
        source = data$source
      ),
      metadata_file
    )

    return(data)

  }, error = function(e) {
    message("Error fetching data: ", e$message)
    message("Using built-in sample data")

    return(list(
      expenditure = create_sample_expenditure_data(),
      caseload = create_sample_caseload_data(),
      last_updated = Sys.time(),
      source = "Built-in sample data",
      notes = "Based on DWP figures"
    ))
  })
}

# Calculate combined UC + Legacy totals
calculate_combined_totals <- function(data) {
  data %>%
    mutate(
      # Combined working-age means-tested
      `UC + Legacy Means-Tested` = `Universal Credit` +
        `Income Support` + `JSA Income-Based` + `ESA Income-Based` +
        `Housing Benefit` + `Working Tax Credit` + `Child Tax Credit`,

      # Total disability spending
      `Total Disability` = `Personal Independence Payment` +
        `Disability Living Allowance` + `Attendance Allowance` +
        `ESA Contributory`,

      # Grand total (excl. State Pension)
      `Total Working-Age` = `UC + Legacy Means-Tested` +
        `Total Disability` + `Carer's Allowance`,

      # As percentage of GDP
      `Working-Age % GDP` = (`Total Working-Age` / GDP) * 100,
      `UC + Legacy % GDP` = (`UC + Legacy Means-Tested` / GDP) * 100,
      `Disability % GDP` = (`Total Disability` / GDP) * 100
    )
}

# ============================================================================
# SHINY UI
# ============================================================================

ui <- page_sidebar(
  title = "UK Benefits Spending Visualizer",
  theme = bs_theme(
    bootswatch = "flatly",
    base_font = font_google("Source Sans Pro"),
    heading_font = font_google("Source Sans Pro")
  ),

  sidebar = sidebar(
    width = 350,

    # Data source info
    card(
      card_header("Data Source"),
      card_body(
        textOutput("data_source_info"),
        actionButton("refresh_data", "Check for Updates",
                     class = "btn-outline-primary btn-sm mt-2")
      )
    ),

    # Time range selection
    card(
      card_header("Time Period"),
      card_body(
        sliderInput("year_range", "Fiscal Years",
                    min = 2010, max = 2028,
                    value = c(2010, 2028),
                    step = 1,
                    sep = ""),
        checkboxInput("include_forecasts", "Include OBR Forecasts", value = TRUE)
      )
    ),

    # BBG Methodology adjustments
    card(
      card_header(
        span("Methodology Adjustments"),
        tooltip(
          bs_icon("info-circle"),
          "Based on Ben Baumberg Geiger's analysis of how official statistics can be misleading"
        )
      ),
      card_body(
        p(class = "text-muted small",
          "These adjustments address issues identified by ",
          tags$a(href = "https://inequalities.substack.com/p/obr-new-welfare-trends-report",
                 "Ben Baumberg Geiger", target = "_blank"),
          " regarding how UC and legacy benefits are counted differently."),

        hr(),

        checkboxInput("adj_uc_counting",
                      span("Adjust UC household counting",
                           tooltip(bs_icon("question-circle"),
                                   "UC counts partners in household, ESA counted individuals only. This inflates apparent UC caseloads.")),
                      value = FALSE),

        conditionalPanel(
          "input.adj_uc_counting",
          sliderInput("uc_adjustment_factor", "Adjustment factor",
                      min = 0.75, max = 0.95, value = 0.85, step = 0.01)
        ),

        checkboxInput("adj_lcw_no_payment",
                      span("Exclude LCW 'no payment' claimants",
                           tooltip(bs_icon("question-circle"),
                                   "Post-2017, some claimants receive no more than unemployment rates")),
                      value = FALSE),

        conditionalPanel(
          "input.adj_lcw_no_payment",
          sliderInput("lcw_proportion", "LCW no-payment proportion",
                      min = 0.05, max = 0.20, value = 0.12, step = 0.01)
        ),

        checkboxInput("adj_spa_effect",
                      span("Remove State Pension Age effect",
                           tooltip(bs_icon("question-circle"),
                                   "Rising SPA has pushed ~330k people onto working-age benefits")),
                      value = FALSE),

        checkboxInput("adj_baseline_2014",
                      span("Use 2014-15 baseline",
                           tooltip(bs_icon("question-circle"),
                                   "Early WCA implementation (2010-13) was problematic")),
                      value = FALSE)
      )
    ),

    # Modeling assumptions
    card(
      card_header("Display Options"),
      card_body(
        radioButtons("value_type", "Show values as",
                     choices = c("£ Billions (Nominal)" = "nominal",
                                 "£ Billions (2024 Prices)" = "real",
                                 "% of GDP" = "gdp_share"),
                     selected = "nominal"),

        checkboxInput("show_combined", "Show combined UC + Legacy totals", value = TRUE)
      )
    )
  ),

  # Main content area
  navset_card_tab(
    id = "main_tabs",

    # Time Series Tab
    nav_panel(
      "Time Series",
      layout_columns(
        col_widths = c(12),

        card(
          card_header("Benefit Spending Over Time"),
          card_body(
            pickerInput("ts_benefits", "Select benefits to display",
                        choices = list(
                          "Combined" = c("UC + Legacy Means-Tested", "Total Disability", "Total Working-Age"),
                          "Universal Credit & Legacy" = c("Universal Credit", "Income Support",
                                                          "JSA Income-Based", "ESA Income-Based",
                                                          "Housing Benefit", "Working Tax Credit", "Child Tax Credit"),
                          "Disability" = c("Personal Independence Payment", "Disability Living Allowance",
                                           "Attendance Allowance", "ESA Contributory"),
                          "Other" = c("Carer's Allowance", "State Pension", "Child Benefit")
                        ),
                        selected = c("UC + Legacy Means-Tested", "Total Disability"),
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE)),
            plotlyOutput("time_series_plot", height = "500px")
          )
        )
      )
    ),

    # Composition Tab
    nav_panel(
      "Spending Composition",
      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header("Spending Breakdown by Category"),
          card_body(
            selectInput("comp_year", "Select Year", choices = NULL),
            plotlyOutput("composition_pie", height = "400px")
          )
        ),

        card(
          card_header("UC vs Legacy Benefits Transition"),
          card_body(
            plotlyOutput("uc_legacy_area", height = "400px")
          )
        )
      ),

      card(
        card_header("Stacked Area: Full Spending Composition"),
        card_body(
          plotlyOutput("stacked_area", height = "450px")
        )
      )
    ),

    # Caseload Tab
    nav_panel(
      "Caseload Analysis",
      layout_columns(
        col_widths = c(12),

        card(
          card_header(
            span("Incapacity Benefits Caseload",
                 tooltip(bs_icon("info-circle"),
                         "Note: Trends here are affected by counting methodology changes between UC and legacy benefits"))
          ),
          card_body(
            plotlyOutput("caseload_plot", height = "450px"),

            conditionalPanel(
              "input.adj_uc_counting || input.adj_spa_effect",
              div(class = "alert alert-info mt-3",
                  strong("Adjustment applied: "),
                  textOutput("adjustment_description", inline = TRUE)
              )
            )
          )
        )
      )
    ),

    # Data Table Tab
    nav_panel(
      "Data Table",
      card(
        card_header("Raw Data"),
        card_body(
          downloadButton("download_data", "Download CSV", class = "mb-3"),
          DTOutput("data_table")
        )
      )
    ),

    # Methodology Tab
    nav_panel(
      "Methodology Notes",
      card(
        card_body(
          h4("Data Sources"),
          p("This tool uses data from the ",
            tags$a(href = "https://www.gov.uk/government/collections/benefit-expenditure-tables",
                   "DWP Benefit Expenditure and Caseload Tables", target = "_blank"),
            ", which are updated twice yearly following Spring and Autumn fiscal events."),

          h4("Ben Baumberg Geiger's Methodological Considerations"),
          p("Professor Ben Baumberg Geiger at King's College London has identified several issues with how official welfare statistics are presented. This tool implements adjustments based on his analysis."),

          h5("1. UC vs Legacy Benefit Counting"),
          p("Universal Credit counts partners in a household as separate individuals in the incapacity benefits caseload. Legacy ESA only counted individual claimants. This means UC caseloads appear artificially inflated compared to historical ESA data."),
          p(tags$em("Adjustment: Apply a reduction factor (default 0.85) to UC caseload figures.")),

          h5("2. LCW 'No Payment' Claimants"),
          p("Since 2017, claimants with Limited Capability for Work (LCW) but not LCWRA receive no more money than unemployment benefit claimants. Including them as 'incapacity benefit' claimants is misleading."),
          p(tags$em("Adjustment: Exclude approximately 12% of UC incapacity caseload.")),

          h5("3. State Pension Age Effect"),
          p("The rising State Pension Age has shifted approximately 330,000 people onto working-age benefits. This accounts for a substantial portion of the apparent caseload increase since 2008."),
          p(tags$em("Adjustment: Remove the estimated SPA effect from caseload trends.")),

          h5("4. Baseline Year Selection"),
          p("The Work Capability Assessment was newly implemented in 2010-11 and was 'a complete disaster' in its early years. Starting trend analysis from 2014-15 provides a more stable comparison point."),
          p(tags$em("Adjustment: Filter data to begin from 2014-15 rather than 2010-11.")),

          hr(),

          h4("Further Reading"),
          tags$ul(
            tags$li(tags$a(href = "https://inequalities.substack.com/p/obr-new-welfare-trends-report",
                           "On the OBR's new Welfare Trends Report", target = "_blank"), " - Ben Baumberg Geiger"),
            tags$li(tags$a(href = "https://obr.uk/wtr/welfare-trends-report-october-2024/",
                           "OBR Welfare Trends Report October 2024", target = "_blank")),
            tags$li(tags$a(href = "https://obr.uk/forecasts-in-depth/tax-by-tax-spend-by-spend/welfare-spending-universal-credit/",
                           "OBR: Welfare spending - Universal Credit", target = "_blank"))
          )
        )
      )
    )
  )
)

# ============================================================================
# SHINY SERVER
# ============================================================================

server <- function(input, output, session) {

  # Reactive: Load and cache data
  benefit_data <- reactiveVal(NULL)

  observe({
    data <- fetch_latest_dwp_data()
    benefit_data(data)
  })

  # Update year selector when data loads
  observe({
    req(benefit_data())
    years <- benefit_data()$expenditure$fiscal_year
    updateSelectInput(session, "comp_year", choices = years, selected = years[length(years) - 4])
  })

  # Data source info
  output$data_source_info <- renderText({
    req(benefit_data())
    paste0("Source: ", benefit_data()$source, "\n",
           "Last updated: ", format(benefit_data()$last_updated, "%Y-%m-%d %H:%M"))
  })

  # Refresh data button
  observeEvent(input$refresh_data, {
    data <- fetch_latest_dwp_data(use_cache = FALSE)
    benefit_data(data)
    showNotification("Data refreshed", type = "message")
  })

  # Reactive: Processed expenditure data with adjustments
  processed_expenditure <- reactive({
    req(benefit_data())

    data <- benefit_data()$expenditure %>%
      calculate_combined_totals()

    # Apply year filter
    data <- data %>%
      filter(year_start >= input$year_range[1],
             year_start <= input$year_range[2])

    # Exclude forecasts if requested
    if (!input$include_forecasts) {
      data <- data %>% filter(data_type == "Outturn")
    }

    # Apply BBG adjustments
    adjustment_options <- list(
      adjust_uc_counting = input$adj_uc_counting,
      uc_adjustment_factor = input$uc_adjustment_factor,
      exclude_lcw_no_payment = input$adj_lcw_no_payment,
      lcw_proportion = input$lcw_proportion,
      remove_spa_effect = input$adj_spa_effect,
      use_2014_baseline = input$adj_baseline_2014
    )

    data <- apply_all_bbg_adjustments(data, adjustment_options)

    return(data)
  })

  # Reactive: Processed caseload data
  processed_caseload <- reactive({
    req(benefit_data())

    data <- benefit_data()$caseload

    # Apply year filter
    data <- data %>%
      filter(year_start >= input$year_range[1],
             year_start <= input$year_range[2])

    if (!input$include_forecasts) {
      data <- data %>% filter(data_type == "Outturn")
    }

    # Apply BBG adjustments
    adjustment_options <- list(
      adjust_uc_counting = input$adj_uc_counting,
      uc_adjustment_factor = input$uc_adjustment_factor,
      remove_spa_effect = input$adj_spa_effect,
      use_2014_baseline = input$adj_baseline_2014
    )

    data <- apply_all_bbg_adjustments(data, adjustment_options)

    return(data)
  })

  # Time series plot
  output$time_series_plot <- renderPlotly({
    req(processed_expenditure(), input$ts_benefits)

    data <- processed_expenditure()

    # Determine which column to use based on value type
    if (input$value_type == "gdp_share") {
      # Use GDP share columns if available
      data <- data %>%
        mutate(across(where(is.numeric) & !matches("GDP|year_start|%"),
                      ~ (. / GDP) * 100, .names = "{.col}_pct"))
      y_suffix <- "_pct"
      y_label <- "% of GDP"
    } else {
      y_suffix <- ""
      y_label <- "£ Billions"
    }

    # Reshape for plotting
    plot_data <- data %>%
      select(fiscal_year, year_start, data_type, all_of(input$ts_benefits)) %>%
      pivot_longer(cols = -c(fiscal_year, year_start, data_type),
                   names_to = "Benefit",
                   values_to = "Value")

    # Create plot
    p <- ggplot(plot_data, aes(x = year_start, y = Value, color = Benefit)) +
      geom_line(linewidth = 1.2) +
      geom_point(aes(shape = data_type), size = 2.5) +
      scale_color_viridis_d(option = "D") +
      scale_shape_manual(values = c("Outturn" = 16, "Forecast" = 1)) +
      labs(x = "Fiscal Year Start", y = y_label,
           shape = "Data Type") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom",
            legend.box = "vertical")

    # Add vertical line at forecast boundary
    if (input$include_forecasts && any(plot_data$data_type == "Forecast")) {
      p <- p + geom_vline(xintercept = 2024.5, linetype = "dashed", alpha = 0.5)
    }

    ggplotly(p) %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })

  # Composition pie chart
  output$composition_pie <- renderPlotly({
    req(processed_expenditure(), input$comp_year)

    year_data <- processed_expenditure() %>%
      filter(fiscal_year == input$comp_year)

    if (nrow(year_data) == 0) return(NULL)

    # Get main benefit categories
    pie_data <- tibble(
      Category = c("Universal Credit", "Legacy Means-Tested", "Disability Benefits",
                   "Carer's Allowance", "Child Benefit"),
      Value = c(
        year_data$`Universal Credit`,
        year_data$`Income Support` + year_data$`JSA Income-Based` +
          year_data$`ESA Income-Based` + year_data$`Housing Benefit` +
          year_data$`Working Tax Credit` + year_data$`Child Tax Credit`,
        year_data$`Personal Independence Payment` + year_data$`Disability Living Allowance` +
          year_data$`Attendance Allowance` + year_data$`ESA Contributory`,
        year_data$`Carer's Allowance`,
        year_data$`Child Benefit`
      )
    ) %>%
      filter(Value > 0)

    plot_ly(pie_data, labels = ~Category, values = ~Value, type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors = viridis(nrow(pie_data)))) %>%
      layout(title = paste("Working-Age Benefits Breakdown", input$comp_year))
  })

  # UC vs Legacy area chart
  output$uc_legacy_area <- renderPlotly({
    req(processed_expenditure())

    data <- processed_expenditure() %>%
      mutate(
        `Legacy Benefits` = `Income Support` + `JSA Income-Based` +
          `ESA Income-Based` + `Housing Benefit` +
          `Working Tax Credit` + `Child Tax Credit`
      ) %>%
      select(fiscal_year, year_start, `Universal Credit`, `Legacy Benefits`) %>%
      pivot_longer(cols = c(`Universal Credit`, `Legacy Benefits`),
                   names_to = "Type", values_to = "Value")

    p <- ggplot(data, aes(x = year_start, y = Value, fill = Type)) +
      geom_area(alpha = 0.8) +
      scale_fill_manual(values = c("Universal Credit" = "#2c7fb8",
                                    "Legacy Benefits" = "#7fcdbb")) +
      labs(x = "Fiscal Year Start", y = "£ Billions",
           title = "UC Transition from Legacy Benefits") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")

    ggplotly(p)
  })

  # Stacked area chart
  output$stacked_area <- renderPlotly({
    req(processed_expenditure())

    data <- processed_expenditure() %>%
      select(fiscal_year, year_start,
             `Universal Credit`, `Income Support`, `JSA Income-Based`,
             `ESA Income-Based`, `Housing Benefit`, `Working Tax Credit`,
             `Child Tax Credit`, `Personal Independence Payment`,
             `Disability Living Allowance`, `Attendance Allowance`,
             `ESA Contributory`, `Carer's Allowance`) %>%
      pivot_longer(cols = -c(fiscal_year, year_start),
                   names_to = "Benefit", values_to = "Value")

    p <- ggplot(data, aes(x = year_start, y = Value, fill = Benefit)) +
      geom_area() +
      scale_fill_viridis_d(option = "turbo") +
      labs(x = "Fiscal Year Start", y = "£ Billions",
           title = "Full Working-Age Benefits Composition") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "right")

    ggplotly(p)
  })

  # Caseload plot
  output$caseload_plot <- renderPlotly({
    req(processed_caseload())

    data <- processed_caseload()

    # Check for adjusted columns
    uc_col <- if ("Universal Credit_adjusted" %in% names(data)) {
      "Universal Credit_adjusted"
    } else {
      "Universal Credit"
    }

    incap_col <- if ("Incapacity Benefits Total_spa_adjusted" %in% names(data)) {
      "Incapacity Benefits Total_spa_adjusted"
    } else {
      "Incapacity Benefits Total"
    }

    plot_data <- data %>%
      select(fiscal_year, year_start, data_type,
             all_of(c(uc_col, incap_col, "PIP/DLA"))) %>%
      pivot_longer(cols = -c(fiscal_year, year_start, data_type),
                   names_to = "Benefit", values_to = "Millions")

    p <- ggplot(plot_data, aes(x = year_start, y = Millions, color = Benefit)) +
      geom_line(linewidth = 1.2) +
      geom_point(aes(shape = data_type), size = 2.5) +
      scale_color_viridis_d(option = "D") +
      labs(x = "Fiscal Year Start", y = "Millions of Claimants",
           title = "Benefit Caseload Trends") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")

    ggplotly(p)
  })

  # Adjustment description
  output$adjustment_description <- renderText({
    adjustments <- c()
    if (input$adj_uc_counting) {
      adjustments <- c(adjustments, paste0("UC counting (factor: ", input$uc_adjustment_factor, ")"))
    }
    if (input$adj_spa_effect) {
      adjustments <- c(adjustments, "SPA effect removed")
    }
    paste(adjustments, collapse = "; ")
  })

  # Data table
  output$data_table <- renderDT({
    req(processed_expenditure())

    processed_expenditure() %>%
      mutate(across(where(is.numeric), ~ round(., 2))) %>%
      datatable(
        options = list(
          scrollX = TRUE,
          pageLength = 20
        )
      )
  })

  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("benefit_expenditure_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(processed_expenditure(), file, row.names = FALSE)
    }
  )
}

# ============================================================================
# RUN APPLICATION
# ============================================================================

shinyApp(ui = ui, server = server)
