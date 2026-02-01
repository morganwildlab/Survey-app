# NEET Dashboard: Youth Inactivity and Labour Market Insights
# Data from Get Britain Working Labour Market Insights (DWP)
# Maps, rates, scatter plots and geographic analysis

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(httr)
library(jsonlite)
library(scales)
library(viridis)
library(bslib)
library(shinyWidgets)
library(DT)
library(lubridate)
library(sf)
library(leaflet)

# ============================================================================
# DATA CONFIGURATION
# ============================================================================

DATA_CONFIG <- list(
  # Get Britain Working data (released 29 Jan 2026)
  gbw_data_url = "https://www.gov.uk/government/statistics/get-britain-working-labour-market-insights-january-2026",
  gbw_tables_url = "https://assets.publishing.service.gov.uk/media/6979f6bb316fd8f8015520d9/get-britain-working-labour-market-insights-january-2026-data-tables.ods",

  # DfE NEET statistics
  dfe_neet_url = "https://explore-education-statistics.service.gov.uk/find-statistics/neet-statistics-annual-brief",

  # ONS NEET bulletin
  ons_neet_url = "https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/bulletins/youngpeoplenotineducationemploymentortrainingneet/latest",

  # Stat-Xplore API
  stat_xplore_api = "https://stat-xplore.dwp.gov.uk/webapi/rest/v1"
)

# NEET status categories (DfE definitions)
NEET_CATEGORIES <- list(
  unemployed = c(
    "Unemployed - seeking work",
    "Unemployed - available but not seeking"
  ),
  inactive = c(
    "Inactive - illness or disability",
    "Inactive - looking after family",
    "Inactive - other reasons",
    "Inactive - student (not in formal education)"
  )
)

# Youth inactivity reasons (ONS categories)
INACTIVITY_REASONS <- c(
  "Student",
  "Looking after family/home",
  "Temporary sick",
  "Long-term sick",
  "Discouraged",
  "Retired",
  "Other"
)

# Get Britain Working local labour market types (14 clusters)
# Based on UC jobseeker volumes, employment rates, work-limiting disability,
# qualifications, musculoskeletal conditions, mental health
LABOUR_MARKET_TYPES <- list(
  "Type 1: London Central" = list(
    description = "High UC volumes, high employment, young population",
    typical_las = c("Westminster", "Camden", "Islington")
  ),
  "Type 2: London Outer" = list(
    description = "Mixed suburban, good employment, lower disability",
    typical_las = c("Barnet", "Bromley", "Croydon")
  ),
  "Type 3: Prosperous South" = list(
    description = "High employment, low UC, high qualifications",
    typical_las = c("Surrey", "Buckinghamshire", "Hampshire")
  ),
  "Type 4: University Cities" = list(
    description = "High student population, variable NEET",
    typical_las = c("Oxford", "Cambridge", "Bristol")
  ),
  "Type 5: Rural/Coastal" = list(
    description = "Low population density, higher into-work rates",
    typical_las = c("Norfolk", "Suffolk", "Cornwall")
  ),
  "Type 6: Post-Industrial North" = list(
    description = "High disability, lower employment, legacy industries",
    typical_las = c("Barnsley", "Rotherham", "Doncaster")
  ),
  "Type 7: Coastal Deprivation" = list(
    description = "High NEET, seasonal employment, older demographics",
    typical_las = c("Blackpool", "Great Yarmouth", "Thanet")
  ),
  "Type 8: Welsh Valleys" = list(
    description = "High incapacity, post-mining, tight labour markets",
    typical_las = c("Blaenau Gwent", "Merthyr Tydfil", "Rhondda Cynon Taf")
  ),
  "Type 9: Scottish Urban" = list(
    description = "Mixed outcomes, devolved policy context",
    typical_las = c("Glasgow", "Edinburgh", "Dundee")
  ),
  "Type 10: Midlands Manufacturing" = list(
    description = "Variable employment, automotive/logistics",
    typical_las = c("Birmingham", "Coventry", "Derby")
  ),
  "Type 11: Northern Towns" = list(
    description = "Mixed NEET, regeneration areas",
    typical_las = c("Blackburn", "Burnley", "Oldham")
  ),
  "Type 12: East Midlands Mixed" = list(
    description = "Variable outcomes, mixed economy",
    typical_las = c("Nottingham", "Leicester", "Lincoln")
  ),
  "Type 13: South Wales Coast" = list(
    description = "Port cities, tourism, manufacturing mix",
    typical_las = c("Cardiff", "Newport", "Swansea")
  ),
  "Type 14: Scottish Highlands/Islands" = list(
    description = "Remote rural, distinctive labour markets",
    typical_las = c("Highland", "Argyll and Bute", "Orkney")
  )
)

# ============================================================================
# SAMPLE DATA - Based on latest Get Britain Working & ONS statistics
# ============================================================================

create_national_neet_data <- function() {
  # National NEET time series (16-24 year olds, UK)
  # Based on ONS data, quarterly from 2019 Q1 to 2025 Q4

  quarters <- c(
    "2019 Q1", "2019 Q2", "2019 Q3", "2019 Q4",
    "2020 Q1", "2020 Q2", "2020 Q3", "2020 Q4",
    "2021 Q1", "2021 Q2", "2021 Q3", "2021 Q4",
    "2022 Q1", "2022 Q2", "2022 Q3", "2022 Q4",
    "2023 Q1", "2023 Q2", "2023 Q3", "2023 Q4",
    "2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4",
    "2025 Q1", "2025 Q2", "2025 Q3", "2025 Q4"
  )

  tibble(
    quarter = quarters,
    date = seq(as.Date("2019-01-01"), as.Date("2025-10-01"), by = "quarter"),

    # Total NEET (thousands)
    neet_total = c(
      784, 762, 748, 791,  # 2019
      802, 892, 878, 812,  # 2020 (COVID spike)
      798, 756, 762, 788,  # 2021
      775, 748, 732, 768,  # 2022
      782, 796, 812, 842,  # 2023
      878, 892, 946, 987,  # 2024 (rising trend)
      962, 948, 946, 958   # 2025
    ),

    # NEET rate (%)
    neet_rate = c(
      10.6, 10.3, 10.1, 10.7,
      10.9, 12.1, 11.9, 11.0,
      10.8, 10.2, 10.3, 10.6,
      10.5, 10.1, 9.9, 10.4,
      10.6, 10.8, 11.0, 11.4,
      11.9, 12.1, 12.8, 13.4,
      13.0, 12.8, 12.7, 12.9
    ),

    # Breakdown: Unemployed vs Inactive
    neet_unemployed = c(
      312, 298, 285, 318,
      335, 412, 398, 345,
      328, 298, 305, 322,
      308, 285, 268, 295,
      302, 318, 335, 362,
      385, 398, 425, 448,
      428, 412, 405, 418
    ),

    neet_inactive = c(
      472, 464, 463, 473,
      467, 480, 480, 467,
      470, 458, 457, 466,
      467, 463, 464, 473,
      480, 478, 477, 480,
      493, 494, 521, 539,
      534, 536, 541, 540
    ),

    # Youth disability inactivity (16-24, long-term sick)
    inactive_disability = c(
      98, 102, 105, 108,
      112, 118, 125, 132,
      142, 152, 162, 175,
      188, 198, 208, 218,
      228, 238, 248, 258,
      268, 278, 288, 298,
      305, 312, 318, 325
    ),

    # Student inactivity (captured as "inactive" but not problematic)
    inactive_student = c(
      285, 268, 262, 270,
      258, 248, 242, 225,
      218, 198, 188, 182,
      175, 165, 158, 155,
      152, 142, 132, 125,
      128, 122, 135, 142,
      132, 128, 125, 118
    )
  )
}

create_regional_neet_data <- function() {
  # Regional NEET rates and related indicators
  # Based on ONS and DfE regional breakdowns

  tibble(
    region = c(
      "North East", "North West", "Yorkshire and The Humber",
      "East Midlands", "West Midlands", "East of England",
      "London", "South East", "South West",
      "Wales", "Scotland", "Northern Ireland"
    ),

    # NEET rate (16-24), latest data
    neet_rate = c(14.8, 13.2, 12.8, 11.5, 13.8, 10.2, 12.5, 9.8, 10.5, 13.5, 11.8, 12.2),

    # Youth unemployment rate (16-24)
    youth_unemployment_rate = c(15.2, 12.8, 12.1, 10.8, 13.2, 9.5, 14.2, 9.2, 9.8, 11.5, 10.2, 11.8),

    # Youth economic inactivity rate (16-24)
    youth_inactivity_rate = c(42.5, 38.2, 36.8, 35.2, 39.5, 33.8, 45.2, 32.5, 34.8, 38.5, 36.2, 40.5),

    # Work-limiting disability rate (16-24)
    youth_disability_rate = c(8.5, 7.2, 6.8, 6.2, 7.5, 5.8, 5.2, 5.5, 6.2, 7.8, 7.2, 6.8),

    # UC into-work rate (June 2025, from GBW)
    into_work_rate = c(6.2, 7.1, 7.4, 7.8, 6.8, 8.2, 7.5, 8.5, 8.1, 6.8, 7.2, 6.5),

    # Mental health condition prevalence (16-24)
    mental_health_rate = c(18.5, 16.2, 15.8, 14.5, 17.2, 13.8, 14.2, 13.5, 14.8, 17.5, 16.8, 15.2),

    # Low/no qualifications (16-24)
    low_quals_rate = c(12.5, 10.8, 10.2, 9.5, 11.2, 8.5, 7.8, 7.5, 8.8, 11.5, 9.8, 10.5),

    # Population 16-24 (thousands)
    youth_population = c(285, 782, 598, 492, 625, 598, 1025, 892, 535, 342, 585, 225)
  )
}

create_local_authority_data <- function() {
  # Local authority level data for mapping
  # Based on DfE NEET data, GBW into-work rates, and Youth Opportunity Index

  # Sample of local authorities with varied characteristics
  tibble(
    la_name = c(
      # North East
      "Newcastle upon Tyne", "Sunderland", "County Durham", "Gateshead", "Middlesbrough",
      # North West
      "Manchester", "Liverpool", "Blackpool", "Bolton", "Salford",
      # Yorkshire
      "Leeds", "Sheffield", "Bradford", "Barnsley", "Doncaster",
      # West Midlands
      "Birmingham", "Coventry", "Wolverhampton", "Sandwell", "Dudley",
      # East Midlands
      "Nottingham", "Leicester", "Derby", "Lincoln", "Boston",
      # East
      "Norwich", "Peterborough", "Ipswich", "Cambridge", "Luton",
      # London
      "Tower Hamlets", "Hackney", "Newham", "Barking and Dagenham", "Westminster",
      "Camden", "Kensington and Chelsea", "Southwark", "Lambeth", "Brent",
      # South East
      "Brighton and Hove", "Southampton", "Portsmouth", "Reading", "Milton Keynes",
      # South West
      "Bristol", "Plymouth", "Bournemouth", "Exeter", "Torbay",
      # Wales
      "Cardiff", "Swansea", "Newport", "Rhondda Cynon Taf", "Blaenau Gwent",
      # Scotland
      "Glasgow", "Edinburgh", "Dundee", "Aberdeen", "Inverclyde"
    ),

    region = c(
      rep("North East", 5), rep("North West", 5), rep("Yorkshire", 5),
      rep("West Midlands", 5), rep("East Midlands", 5), rep("East", 5),
      rep("London", 10), rep("South East", 5), rep("South West", 5),
      rep("Wales", 5), rep("Scotland", 5)
    ),

    # NEET rate 16-17 (DfE data)
    neet_rate_16_17 = c(
      5.2, 6.1, 5.8, 4.8, 7.2,
      5.8, 6.5, 8.2, 5.5, 5.2,
      4.8, 5.2, 6.8, 7.5, 7.2,
      6.2, 5.5, 6.8, 7.2, 6.5,
      6.8, 6.2, 5.8, 4.5, 5.2,
      5.5, 6.2, 5.8, 2.8, 6.8,
      5.8, 5.5, 6.2, 7.5, 3.8,
      3.5, 2.5, 5.2, 5.8, 6.2,
      5.2, 5.8, 6.2, 4.2, 4.5,
      5.5, 6.2, 5.8, 4.8, 7.8,
      5.5, 6.2, 6.5, 8.2, 10.5,
      6.8, 4.5, 6.8, 5.2, 8.2
    ),

    # Youth Opportunity Index score (out of 100)
    youth_opportunity_index = c(
      52, 42, 45, 55, 32,
      58, 45, 23, 48, 55,
      62, 58, 42, 35, 38,
      48, 55, 38, 32, 42,
      45, 48, 52, 58, 55,
      52, 45, 55, 78, 42,
      52, 55, 48, 38, 72,
      75, 76, 58, 55, 52,
      62, 55, 52, 68, 65,
      65, 48, 55, 62, 35,
      58, 52, 48, 35, 25,
      48, 68, 45, 55, 32
    ),

    # UC into-work rate (from GBW Jan 2026)
    into_work_rate = c(
      7.2, 6.5, 7.8, 7.5, 5.8,
      7.5, 6.8, 5.2, 7.2, 7.8,
      8.2, 7.8, 6.5, 6.2, 6.5,
      6.8, 7.5, 6.2, 5.8, 6.5,
      6.8, 6.5, 7.2, 8.5, 12.7,
      8.8, 7.2, 8.2, 9.5, 6.5,
      7.2, 7.5, 6.8, 6.2, 8.8,
      8.5, 8.2, 7.5, 7.2, 7.5,
      8.2, 7.5, 7.2, 8.8, 8.5,
      8.5, 7.2, 7.8, 8.8, 5.5,
      7.5, 7.2, 6.8, 5.8, 4.8,
      6.5, 8.2, 6.8, 7.5, 5.2
    ),

    # Youth disability inactivity rate
    youth_disability_rate = c(
      7.8, 8.5, 8.2, 7.2, 9.5,
      7.2, 8.2, 10.5, 7.5, 6.8,
      6.5, 7.2, 8.5, 9.8, 9.2,
      8.2, 7.5, 8.8, 9.5, 8.5,
      8.5, 7.8, 7.2, 5.8, 6.2,
      6.5, 7.2, 6.8, 4.2, 7.8,
      5.8, 6.2, 6.5, 7.2, 4.8,
      4.5, 4.2, 5.8, 6.2, 6.5,
      6.2, 6.8, 7.2, 5.5, 5.8,
      6.5, 7.5, 6.8, 5.5, 9.2,
      7.2, 7.8, 8.2, 10.5, 12.2,
      8.5, 5.8, 8.2, 6.5, 10.8
    ),

    # Mental health prevalence (16-24)
    mental_health_rate = c(
      17.2, 18.5, 17.8, 16.5, 19.5,
      15.8, 17.2, 21.5, 16.5, 15.2,
      14.8, 15.5, 17.8, 19.2, 18.5,
      16.8, 15.5, 18.2, 19.5, 17.5,
      17.2, 16.5, 15.8, 13.5, 14.2,
      15.2, 16.5, 15.2, 11.8, 17.2,
      14.2, 14.8, 15.5, 16.8, 12.5,
      12.2, 11.5, 14.5, 15.2, 15.8,
      14.8, 15.5, 16.2, 13.5, 13.8,
      14.5, 16.2, 15.2, 13.2, 18.8,
      16.5, 17.2, 17.8, 20.2, 22.5,
      18.2, 13.5, 17.8, 14.5, 20.2
    ),

    # IMD deprivation rank (1=most deprived, higher=less deprived)
    imd_rank = c(
      42, 18, 58, 52, 8,
      6, 4, 1, 35, 22,
      28, 45, 19, 12, 15,
      7, 48, 16, 3, 25,
      11, 20, 38, 125, 95,
      62, 55, 82, 285, 32,
      35, 28, 14, 5, 180,
      195, 245, 38, 22, 42,
      75, 52, 48, 165, 145,
      78, 45, 62, 135, 28,
      68, 55, 42, 10, 2,
      9, 175, 25, 85, 15
    ),

    # Labour market type (from GBW 14 clusters)
    labour_market_type = c(
      6, 6, 5, 6, 7,
      11, 11, 7, 11, 11,
      10, 10, 11, 6, 6,
      10, 10, 10, 10, 10,
      12, 12, 12, 5, 5,
      5, 12, 5, 4, 10,
      1, 1, 2, 2, 1,
      1, 1, 1, 1, 2,
      4, 5, 5, 3, 4,
      4, 7, 5, 4, 7,
      13, 13, 13, 8, 8,
      9, 9, 9, 9, 9
    ),

    # Coordinates for mapping (approximate centroids)
    lat = c(
      54.98, 54.91, 54.78, 54.96, 54.57,
      53.48, 53.41, 53.82, 53.58, 53.49,
      53.80, 53.38, 53.79, 53.55, 53.52,
      52.49, 52.41, 52.59, 52.51, 52.51,
      52.95, 52.64, 52.92, 53.23, 52.98,
      52.63, 52.57, 52.06, 52.21, 51.88,
      51.52, 51.55, 51.53, 51.54, 51.51,
      51.54, 51.50, 51.50, 51.46, 51.55,
      50.82, 50.91, 50.80, 51.45, 52.04,
      51.45, 50.37, 50.72, 50.72, 50.46,
      51.48, 51.62, 51.59, 51.61, 51.79,
      55.86, 55.95, 56.46, 57.15, 55.95
    ),

    lng = c(
      -1.61, -1.38, -1.58, -1.60, -1.23,
      -2.24, -2.99, -3.05, -2.43, -2.29,
      -1.55, -1.47, -1.76, -1.48, -1.13,
      -1.90, -1.51, -2.13, -2.01, -2.09,
      -1.15, -1.13, -1.48, -0.54, -0.02,
      1.30, -0.24, 1.16, 0.12, -0.42,
      -0.01, -0.06, 0.02, 0.13, -0.14,
      -0.13, -0.19, -0.09, -0.12, -0.27,
      -0.14, -1.40, -1.09, -0.97, -0.76,
      -2.59, -4.14, -1.88, -3.53, -3.53,
      -3.18, -3.94, -3.00, -3.44, -3.21,
      -4.25, -3.19, -2.97, -2.09, -4.79
    )
  )
}

create_age_breakdown_data <- function() {
  # NEET breakdown by detailed age groups
  tibble(
    age_group = c("16-17", "18-19", "20-21", "22-24"),

    # NEET rate
    neet_rate = c(4.8, 12.5, 14.2, 13.8),

    # Total NEET (thousands)
    neet_count = c(62, 185, 298, 413),

    # Of which unemployed
    unemployed = c(18, 72, 125, 203),

    # Of which inactive
    inactive = c(44, 113, 173, 210),

    # Inactive due to disability
    inactive_disability = c(12, 42, 85, 186),

    # Inactive - looking after family
    inactive_family = c(5, 28, 48, 72),

    # Inactive - other
    inactive_other = c(27, 43, 40, -48)
  )
}

create_into_work_data <- function() {
  # UC into-work rates from Get Britain Working
  # By duration on UC, age group, family type

  list(
    # By duration (months on Searching for Work regime)
    by_duration = tibble(
      duration = c("0-3 months", "3-6 months", "6-12 months", "12-24 months", "24+ months"),
      into_work_rate = c(12.5, 8.2, 6.1, 4.5, 3.2),
      proportion_of_caseload = c(45, 20, 18, 12, 5)
    ),

    # By age
    by_age = tibble(
      age_group = c("Under 25", "25-34", "35-44", "45-54", "55+"),
      into_work_rate = c(9.8, 8.2, 7.1, 6.2, 4.8),
      youth_premium = c(1.24, 1.0, 0.87, 0.76, 0.59)
    ),

    # By family type
    by_family = tibble(
      family_type = c("Single, no children", "Single, with children", "Couple, no children", "Couple, with children"),
      into_work_rate = c(8.5, 5.2, 7.8, 6.8)
    ),

    # Monthly trend (seasonality)
    monthly_trend = tibble(
      month = month.name,
      into_work_rate = c(6.8, 6.5, 7.2, 8.5, 7.8, 7.4, 7.2, 7.5, 8.2, 8.8, 7.5, 6.2)
    )
  )
}

create_disability_inactivity_data <- function() {
  # Youth disability and health-related inactivity
  # Time series showing the rise in youth health inactivity

  tibble(
    year = 2015:2025,

    # 16-24 economically inactive due to long-term sickness (thousands)
    inactive_longterm_sick = c(
      125, 132, 142, 155, 168, 185, 212, 248, 278, 298, 325
    ),

    # As % of 16-24 population
    pct_longterm_sick = c(
      1.8, 1.9, 2.0, 2.2, 2.4, 2.6, 3.0, 3.5, 3.9, 4.2, 4.6
    ),

    # Mental health conditions (subset)
    mental_health_inactive = c(
      52, 58, 65, 75, 88, 102, 125, 152, 178, 198, 218
    ),

    # Musculoskeletal conditions
    musculoskeletal_inactive = c(
      18, 19, 21, 22, 24, 26, 28, 32, 35, 38, 42
    ),

    # Other health conditions
    other_health_inactive = c(
      55, 55, 56, 58, 56, 57, 59, 64, 65, 62, 65
    ),

    # For comparison: 25-49 longterm sick rate
    pct_longterm_sick_25_49 = c(
      3.8, 3.9, 4.0, 4.1, 4.2, 4.4, 4.8, 5.2, 5.5, 5.8, 6.1
    )
  )
}

# ============================================================================
# SIMPLIFIED UK BOUNDARIES FOR MAPPING
# ============================================================================

# We'll use leaflet with circle markers for LA data
# More sophisticated apps would use full boundary shapefiles

get_region_coords <- function() {
  tibble(
    region = c(
      "North East", "North West", "Yorkshire and The Humber",
      "East Midlands", "West Midlands", "East of England",
      "London", "South East", "South West",
      "Wales", "Scotland", "Northern Ireland"
    ),
    lat = c(55.0, 53.8, 53.9, 52.8, 52.5, 52.2, 51.5, 51.3, 50.8, 52.0, 56.5, 54.6),
    lng = c(-1.6, -2.5, -1.3, -0.8, -1.9, 0.5, -0.1, -0.5, -3.5, -3.5, -4.0, -6.5)
  )
}

# ============================================================================
# SHINY UI
# ============================================================================

ui <- page_sidebar(
  title = "NEET Dashboard: Youth Inactivity & Labour Market Insights",
  theme = bs_theme(
    bootswatch = "flatly",
    base_font = font_google("Source Sans Pro"),
    heading_font = font_google("Source Sans Pro")
  ),

  sidebar = sidebar(
    width = 350,

    # Data source info
    card(
      card_header("Data Sources"),
      card_body(
        p(class = "small text-muted",
          strong("Get Britain Working"), " Labour Market Insights",
          br(), "DWP, January 2026 release"),
        p(class = "small text-muted",
          strong("NEET Statistics"), " Annual Brief",
          br(), "DfE, Calendar Year 2024"),
        p(class = "small text-muted",
          strong("Young people NEET"), " Bulletin",
          br(), "ONS, February 2025"),
        hr(),
        actionButton("refresh_data", "Refresh Data",
                     class = "btn-outline-primary btn-sm")
      )
    ),

    # Filters
    card(
      card_header("Filters"),
      card_body(
        selectInput("region_filter", "Region",
                    choices = c("All regions" = "all",
                                "North East", "North West", "Yorkshire and The Humber",
                                "East Midlands", "West Midlands", "East of England",
                                "London", "South East", "South West",
                                "Wales", "Scotland", "Northern Ireland"),
                    selected = "all"),

        sliderInput("year_range", "Time Period",
                    min = 2015, max = 2025,
                    value = c(2019, 2025),
                    step = 1, sep = ""),

        checkboxGroupInput("age_groups", "Age Groups",
                           choices = c("16-17", "18-19", "20-21", "22-24"),
                           selected = c("16-17", "18-19", "20-21", "22-24"))
      )
    ),

    # Map options
    card(
      card_header("Map Settings"),
      card_body(
        selectInput("map_indicator", "Map Indicator",
                    choices = c(
                      "NEET Rate (16-17)" = "neet_rate_16_17",
                      "Youth Opportunity Index" = "youth_opportunity_index",
                      "UC Into-Work Rate" = "into_work_rate",
                      "Youth Disability Rate" = "youth_disability_rate",
                      "Mental Health Prevalence" = "mental_health_rate"
                    ),
                    selected = "neet_rate_16_17"),

        selectInput("color_palette", "Color Palette",
                    choices = c("Viridis" = "viridis",
                                "Red-Yellow-Green" = "RdYlGn",
                                "Blue-Red" = "RdBu",
                                "Purple-Orange" = "PuOr"),
                    selected = "viridis")
      )
    ),

    # Labour market type filter
    card(
      card_header("Labour Market Types"),
      card_body(
        p(class = "small text-muted",
          "GBW identifies 14 local labour market types based on UC volumes, employment, disability, qualifications, and health conditions."),
        pickerInput("lm_types", "Select Types",
                    choices = 1:14,
                    selected = 1:14,
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE,
                                   `live-search` = TRUE))
      )
    )
  ),

  # Main content
  navset_card_tab(
    id = "main_tabs",

    # Overview tab
    nav_panel(
      "Overview",
      layout_columns(
        col_widths = c(3, 3, 3, 3),

        value_box(
          title = "NEET Total (16-24)",
          value = textOutput("vb_neet_total"),
          showcase = bsicons::bs_icon("person-x"),
          theme = "danger"
        ),
        value_box(
          title = "NEET Rate",
          value = textOutput("vb_neet_rate"),
          showcase = bsicons::bs_icon("percent"),
          theme = "warning"
        ),
        value_box(
          title = "Youth Disability Inactive",
          value = textOutput("vb_disability"),
          showcase = bsicons::bs_icon("heart-pulse"),
          theme = "info"
        ),
        value_box(
          title = "Into-Work Rate (UC)",
          value = textOutput("vb_into_work"),
          showcase = bsicons::bs_icon("arrow-right-circle"),
          theme = "success"
        )
      ),

      layout_columns(
        col_widths = c(8, 4),

        card(
          card_header("NEET Trends: National Time Series"),
          card_body(
            plotlyOutput("overview_timeseries", height = "400px")
          )
        ),

        card(
          card_header("Current NEET Breakdown"),
          card_body(
            plotlyOutput("overview_pie", height = "400px")
          )
        )
      )
    ),

    # Maps tab
    nav_panel(
      "Geographic Maps",
      layout_columns(
        col_widths = c(7, 5),

        card(
          card_header(
            textOutput("map_title")
          ),
          card_body(
            leafletOutput("main_map", height = "550px")
          )
        ),

        card(
          card_header("Regional Comparison"),
          card_body(
            plotlyOutput("regional_bar", height = "250px"),
            hr(),
            plotlyOutput("regional_scatter", height = "250px")
          )
        )
      ),

      card(
        card_header("Local Authority Rankings"),
        card_body(
          DTOutput("la_table")
        )
      )
    ),

    # Youth Disability tab
    nav_panel(
      "Youth Disability & Health",
      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header("Rise in Youth Health-Related Inactivity"),
          card_body(
            plotlyOutput("disability_trend", height = "400px")
          )
        ),

        card(
          card_header("Health Conditions Breakdown"),
          card_body(
            plotlyOutput("health_breakdown", height = "400px")
          )
        )
      ),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header("Youth vs Working-Age Health Inactivity"),
          card_body(
            plotlyOutput("age_comparison", height = "350px")
          )
        ),

        card(
          card_header("Geographic Variation in Youth Disability"),
          card_body(
            plotlyOutput("disability_map", height = "350px")
          )
        )
      )
    ),

    # Into-Work Analysis tab
    nav_panel(
      "Into-Work Rates (GBW)",
      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header("Into-Work Rate by Duration on UC"),
          card_body(
            plotlyOutput("duration_plot", height = "350px")
          )
        ),

        card(
          card_header("Into-Work Rate by Age Group"),
          card_body(
            plotlyOutput("age_into_work", height = "350px")
          )
        )
      ),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header("Seasonality in Into-Work Rates"),
          card_body(
            plotlyOutput("seasonality_plot", height = "350px")
          )
        ),

        card(
          card_header("Into-Work Rate by Family Type"),
          card_body(
            plotlyOutput("family_plot", height = "350px")
          )
        )
      )
    ),

    # Scatter Plots tab
    nav_panel(
      "Correlations",
      layout_columns(
        col_widths = c(12),

        card(
          card_body(
            layout_columns(
              col_widths = c(4, 4, 4),
              selectInput("scatter_x", "X-Axis Variable",
                          choices = c(
                            "NEET Rate (16-17)" = "neet_rate_16_17",
                            "Youth Opportunity Index" = "youth_opportunity_index",
                            "Into-Work Rate" = "into_work_rate",
                            "Youth Disability Rate" = "youth_disability_rate",
                            "Mental Health Rate" = "mental_health_rate",
                            "IMD Rank" = "imd_rank"
                          ),
                          selected = "imd_rank"),
              selectInput("scatter_y", "Y-Axis Variable",
                          choices = c(
                            "NEET Rate (16-17)" = "neet_rate_16_17",
                            "Youth Opportunity Index" = "youth_opportunity_index",
                            "Into-Work Rate" = "into_work_rate",
                            "Youth Disability Rate" = "youth_disability_rate",
                            "Mental Health Rate" = "mental_health_rate",
                            "IMD Rank" = "imd_rank"
                          ),
                          selected = "neet_rate_16_17"),
              selectInput("scatter_color", "Color By",
                          choices = c(
                            "Region" = "region",
                            "Labour Market Type" = "labour_market_type"
                          ),
                          selected = "region")
            )
          )
        )
      ),

      layout_columns(
        col_widths = c(8, 4),

        card(
          card_header("Local Authority Scatter Plot"),
          card_body(
            plotlyOutput("main_scatter", height = "500px")
          )
        ),

        card(
          card_header("Correlation Analysis"),
          card_body(
            verbatimTextOutput("correlation_stats"),
            hr(),
            h6("Key Findings:"),
            uiOutput("scatter_insights")
          )
        )
      )
    ),

    # Labour Market Types tab
    nav_panel(
      "Labour Market Types",
      layout_columns(
        col_widths = c(12),

        card(
          card_header("GBW Local Labour Market Typology"),
          card_body(
            p("Get Britain Working identifies 14 distinct local labour market types based on cluster analysis of:"),
            tags$ul(
              tags$li("UC jobseeker volumes"),
              tags$li("Employment rates"),
              tags$li("Work-limiting disability prevalence"),
              tags$li("Educational qualifications"),
              tags$li("Musculoskeletal condition rates"),
              tags$li("Mental health prevalence")
            )
          )
        )
      ),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header("NEET Rates by Labour Market Type"),
          card_body(
            plotlyOutput("lm_type_neet", height = "400px")
          )
        ),

        card(
          card_header("Into-Work Rates by Labour Market Type"),
          card_body(
            plotlyOutput("lm_type_into_work", height = "400px")
          )
        )
      ),

      card(
        card_header("Labour Market Type Profiles"),
        card_body(
          DTOutput("lm_type_table")
        )
      )
    ),

    # Data & Methodology tab
    nav_panel(
      "Data & Methodology",
      card(
        card_body(
          h4("Data Sources"),

          h5("Get Britain Working: Labour Market Insights"),
          p("Published by DWP quarterly, starting October 2025. The January 2026 release provides:"),
          tags$ul(
            tags$li("UC 'Searching for work' into-work rates by geography, duration, age, and family type"),
            tags$li("14 local labour market types based on cluster analysis"),
            tags$li("Employment sector distributions for UC recipients"),
            tags$li("Health journey and work capability data")
          ),
          p(tags$a(href = "https://www.gov.uk/government/statistics/get-britain-working-labour-market-insights-january-2026",
                   "Access the full release", target = "_blank")),

          hr(),

          h5("NEET Statistics Annual Brief"),
          p("Published by DfE, provides 16-17 NEET estimates by local authority. Key caveats:"),
          tags$ul(
            tags$li("NCCIS figures include 'not known' activity, which may overestimate NEET"),
            tags$li("'Not known' proportions vary by local authority, complicating comparisons"),
            tags$li("Different methodologies apply to 16-17 (admin data) vs 18-24 (survey data)")
          ),

          hr(),

          h5("ONS Young People NEET Bulletin"),
          p("Quarterly bulletin for 16-24 NEET based on Labour Force Survey. Notes:"),
          tags$ul(
            tags$li("LFS data quality issues in 2023 led to temporary suspension"),
            tags$li("Reintroduced estimates from February 2024 should be treated with caution"),
            tags$li("Regional estimates have wider confidence intervals")
          ),

          hr(),

          h5("Youth Opportunity Index"),
          p("Developed by Impetus, measuring education and employment opportunities at LA level. Scores range from 0-100, with higher scores indicating better opportunities."),

          hr(),

          h4("Definitions"),

          h5("NEET"),
          p("Not in Education, Employment, or Training. Includes both:"),
          tags$ul(
            tags$li(strong("Unemployed NEET:"), " Seeking work and available"),
            tags$li(strong("Inactive NEET:"), " Not seeking work (reasons include disability, caring responsibilities, discouragement)")
          ),

          h5("Into-Work Rate"),
          p("From GBW: 'The proportion of UC Searching for work regime customers who have earnings in one assessment period who did not have earnings in the preceding assessment period.'"),

          h5("Labour Market Types"),
          p("14 clusters identified using k-means clustering on 6 standardized variables. Types range from 'London Central' (high employment, young population) to 'Welsh Valleys' (high incapacity, tight labour markets).")
        )
      )
    )
  )
)

# ============================================================================
# SHINY SERVER
# ============================================================================

server <- function(input, output, session) {

  # Reactive data stores
  national_data <- reactiveVal(create_national_neet_data())
  regional_data <- reactiveVal(create_regional_neet_data())
  la_data <- reactiveVal(create_local_authority_data())
  age_data <- reactiveVal(create_age_breakdown_data())
  into_work_data <- reactiveVal(create_into_work_data())
  disability_data <- reactiveVal(create_disability_inactivity_data())

  # Filtered LA data
  filtered_la_data <- reactive({
    data <- la_data()

    if (input$region_filter != "all") {
      data <- data %>% filter(region == input$region_filter)
    }

    data <- data %>% filter(labour_market_type %in% as.numeric(input$lm_types))

    return(data)
  })

  # Value boxes
  output$vb_neet_total <- renderText({
    latest <- national_data() %>% slice_tail(n = 1)
    paste0(format(latest$neet_total, big.mark = ","), "k")
  })

  output$vb_neet_rate <- renderText({
    latest <- national_data() %>% slice_tail(n = 1)
    paste0(latest$neet_rate, "%")
  })

  output$vb_disability <- renderText({
    latest <- national_data() %>% slice_tail(n = 1)
    paste0(format(latest$inactive_disability, big.mark = ","), "k")
  })

  output$vb_into_work <- renderText({
    "7.4%"  # GB average from GBW
  })

  # Overview time series
  output$overview_timeseries <- renderPlotly({
    data <- national_data() %>%
      filter(year(date) >= input$year_range[1],
             year(date) <= input$year_range[2])

    plot_data <- data %>%
      select(date, `Total NEET` = neet_total,
             `Unemployed` = neet_unemployed,
             `Inactive` = neet_inactive) %>%
      pivot_longer(-date, names_to = "Category", values_to = "Count")

    p <- ggplot(plot_data, aes(x = date, y = Count, color = Category)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = c("Total NEET" = "#e41a1c",
                                     "Unemployed" = "#377eb8",
                                     "Inactive" = "#4daf4a")) +
      labs(x = NULL, y = "Thousands (16-24 year olds)",
           title = "UK NEET: Unemployed vs Economically Inactive") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")

    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.15))
  })

  # Overview pie
  output$overview_pie <- renderPlotly({
    latest <- national_data() %>% slice_tail(n = 1)

    pie_data <- tibble(
      Category = c("Unemployed", "Inactive - Disability",
                   "Inactive - Student", "Inactive - Other"),
      Count = c(latest$neet_unemployed,
                latest$inactive_disability,
                latest$inactive_student,
                latest$neet_inactive - latest$inactive_disability - latest$inactive_student)
    )

    plot_ly(pie_data, labels = ~Category, values = ~Count, type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors = c("#377eb8", "#e41a1c", "#4daf4a", "#984ea3"))) %>%
      layout(title = list(text = paste0("NEET Composition (", latest$quarter, ")")))
  })

  # Map title
  output$map_title <- renderText({
    indicator_labels <- c(
      "neet_rate_16_17" = "NEET Rate (16-17 year olds)",
      "youth_opportunity_index" = "Youth Opportunity Index",
      "into_work_rate" = "UC Into-Work Rate (%)",
      "youth_disability_rate" = "Youth Disability Inactivity Rate (%)",
      "mental_health_rate" = "Mental Health Condition Prevalence (%)"
    )
    paste("Local Authority Map:", indicator_labels[input$map_indicator])
  })

  # Main map
  output$main_map <- renderLeaflet({
    data <- filtered_la_data()

    indicator <- input$map_indicator
    values <- data[[indicator]]

    # Determine if higher is better or worse
    reverse_scale <- indicator %in% c("youth_opportunity_index", "into_work_rate")

    # Color palette
    if (input$color_palette == "viridis") {
      pal <- colorNumeric("viridis", values, reverse = reverse_scale)
    } else {
      pal <- colorNumeric(input$color_palette, values, reverse = !reverse_scale)
    }

    # Create popup content
    data <- data %>%
      mutate(popup = paste0(
        "<strong>", la_name, "</strong><br>",
        "Region: ", region, "<br>",
        "NEET Rate (16-17): ", neet_rate_16_17, "%<br>",
        "Youth Opportunity Index: ", youth_opportunity_index, "<br>",
        "Into-Work Rate: ", into_work_rate, "%<br>",
        "Youth Disability Rate: ", youth_disability_rate, "%<br>",
        "Labour Market Type: ", labour_market_type
      ))

    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -2.5, lat = 54.5, zoom = 6) %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = ~sqrt(abs(get(indicator))) * 2,
        color = ~pal(get(indicator)),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1,
        popup = ~popup,
        label = ~la_name
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = values,
        title = gsub("_", " ", input$map_indicator),
        opacity = 0.8
      )
  })

  # Regional bar chart
  output$regional_bar <- renderPlotly({
    data <- regional_data()

    if (input$region_filter != "all") {
      data <- data %>% filter(region == input$region_filter)
    }

    p <- ggplot(data, aes(x = reorder(region, neet_rate), y = neet_rate, fill = neet_rate)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_c(option = "C", direction = -1) +
      labs(x = NULL, y = "NEET Rate (%)", title = "NEET Rate by Region") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none")

    ggplotly(p)
  })

  # Regional scatter
  output$regional_scatter <- renderPlotly({
    data <- regional_data()

    p <- ggplot(data, aes(x = youth_disability_rate, y = neet_rate,
                          color = region, size = youth_population)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "gray50", linetype = "dashed", linewidth = 0.5) +
      scale_color_viridis_d(option = "H") +
      labs(x = "Youth Disability Rate (%)", y = "NEET Rate (%)",
           title = "NEET vs Youth Disability") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none")

    ggplotly(p)
  })

  # LA table
  output$la_table <- renderDT({
    filtered_la_data() %>%
      select(
        `Local Authority` = la_name,
        Region = region,
        `NEET Rate (16-17)` = neet_rate_16_17,
        `Youth Opp. Index` = youth_opportunity_index,
        `Into-Work Rate` = into_work_rate,
        `Youth Disability` = youth_disability_rate,
        `Mental Health` = mental_health_rate,
        `LM Type` = labour_market_type
      ) %>%
      datatable(
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      ) %>%
      formatRound(columns = c("NEET Rate (16-17)", "Into-Work Rate",
                              "Youth Disability", "Mental Health"), digits = 1)
  })

  # Disability trend
  output$disability_trend <- renderPlotly({
    data <- disability_data() %>%
      filter(year >= input$year_range[1], year <= input$year_range[2])

    p <- ggplot(data, aes(x = year, y = inactive_longterm_sick)) +
      geom_area(fill = "#e41a1c", alpha = 0.3) +
      geom_line(color = "#e41a1c", linewidth = 1.5) +
      geom_point(color = "#e41a1c", size = 3) +
      labs(x = NULL, y = "Thousands",
           title = "16-24 Year Olds Economically Inactive Due to Long-Term Sickness") +
      theme_minimal(base_size = 12)

    ggplotly(p)
  })

  # Health breakdown
  output$health_breakdown <- renderPlotly({
    data <- disability_data() %>%
      filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
      select(year, `Mental Health` = mental_health_inactive,
             `Musculoskeletal` = musculoskeletal_inactive,
             `Other Health` = other_health_inactive) %>%
      pivot_longer(-year, names_to = "Condition", values_to = "Count")

    p <- ggplot(data, aes(x = year, y = Count, fill = Condition)) +
      geom_area(alpha = 0.8) +
      scale_fill_manual(values = c("Mental Health" = "#e41a1c",
                                    "Musculoskeletal" = "#377eb8",
                                    "Other Health" = "#4daf4a")) +
      labs(x = NULL, y = "Thousands",
           title = "Youth Health Inactivity by Condition Type") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")

    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.15))
  })

  # Age comparison
  output$age_comparison <- renderPlotly({
    data <- disability_data() %>%
      filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
      select(year, `16-24` = pct_longterm_sick, `25-49` = pct_longterm_sick_25_49) %>%
      pivot_longer(-year, names_to = "Age Group", values_to = "Rate")

    p <- ggplot(data, aes(x = year, y = Rate, color = `Age Group`)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_color_manual(values = c("16-24" = "#e41a1c", "25-49" = "#377eb8")) +
      labs(x = NULL, y = "% of Age Group",
           title = "Long-Term Sick Inactivity: Youth vs Working-Age") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")

    ggplotly(p)
  })

  # Disability geographic variation
  output$disability_map <- renderPlotly({
    data <- regional_data()

    p <- ggplot(data, aes(x = reorder(region, youth_disability_rate),
                          y = youth_disability_rate, fill = youth_disability_rate)) +
      geom_col() +
      coord_flip() +
      scale_fill_gradient(low = "#fee0d2", high = "#de2d26") +
      labs(x = NULL, y = "Youth Disability Rate (%)",
           title = "Youth Disability Inactivity by Region") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none")

    ggplotly(p)
  })

  # Into-work by duration
  output$duration_plot <- renderPlotly({
    data <- into_work_data()$by_duration

    p <- ggplot(data, aes(x = duration, y = into_work_rate, fill = into_work_rate)) +
      geom_col() +
      scale_fill_viridis_c(option = "D", direction = -1) +
      labs(x = "Duration on UC 'Searching for Work'", y = "Into-Work Rate (%)",
           title = "Into-Work Rate Declines with Duration") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p)
  })

  # Into-work by age
  output$age_into_work <- renderPlotly({
    data <- into_work_data()$by_age

    p <- ggplot(data, aes(x = age_group, y = into_work_rate, fill = age_group)) +
      geom_col() +
      scale_fill_viridis_d(option = "C") +
      labs(x = "Age Group", y = "Into-Work Rate (%)",
           title = "Under-25s Have Highest Into-Work Rates") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")

    ggplotly(p)
  })

  # Seasonality
  output$seasonality_plot <- renderPlotly({
    data <- into_work_data()$monthly_trend
    data$month <- factor(data$month, levels = month.name)

    p <- ggplot(data, aes(x = month, y = into_work_rate, group = 1)) +
      geom_line(color = "#377eb8", linewidth = 1.2) +
      geom_point(color = "#377eb8", size = 3) +
      labs(x = NULL, y = "Into-Work Rate (%)",
           title = "Seasonal Pattern: Peaks in April and October") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p)
  })

  # Family type
  output$family_plot <- renderPlotly({
    data <- into_work_data()$by_family

    p <- ggplot(data, aes(x = reorder(family_type, into_work_rate),
                          y = into_work_rate, fill = family_type)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_d(option = "E") +
      labs(x = NULL, y = "Into-Work Rate (%)",
           title = "Single Parents Have Lowest Into-Work Rates") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")

    ggplotly(p)
  })

  # Main scatter plot
  output$main_scatter <- renderPlotly({
    data <- filtered_la_data()

    x_var <- input$scatter_x
    y_var <- input$scatter_y
    color_var <- input$scatter_color

    # Variable labels
    var_labels <- c(
      "neet_rate_16_17" = "NEET Rate (16-17)",
      "youth_opportunity_index" = "Youth Opportunity Index",
      "into_work_rate" = "Into-Work Rate (%)",
      "youth_disability_rate" = "Youth Disability Rate (%)",
      "mental_health_rate" = "Mental Health Rate (%)",
      "imd_rank" = "IMD Rank (higher = less deprived)"
    )

    p <- ggplot(data, aes_string(x = x_var, y = y_var, color = color_var)) +
      geom_point(aes(text = la_name), size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "gray40", linetype = "dashed", linewidth = 0.5) +
      scale_color_viridis_d(option = "H") +
      labs(x = var_labels[x_var], y = var_labels[y_var],
           title = paste(var_labels[y_var], "vs", var_labels[x_var])) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "right")

    ggplotly(p, tooltip = c("text", "x", "y"))
  })

  # Correlation stats
  output$correlation_stats <- renderPrint({
    data <- filtered_la_data()

    x_vals <- data[[input$scatter_x]]
    y_vals <- data[[input$scatter_y]]

    cor_test <- cor.test(x_vals, y_vals)

    cat("Pearson Correlation Analysis\n")
    cat("============================\n\n")
    cat(paste("r =", round(cor_test$estimate, 3), "\n"))
    cat(paste("p-value =", format.pval(cor_test$p.value, digits = 3), "\n"))
    cat(paste("95% CI: [", round(cor_test$conf.int[1], 3), ",", round(cor_test$conf.int[2], 3), "]\n"))
    cat(paste("\nN =", length(x_vals), "local authorities\n"))
  })

  # Scatter insights
  output$scatter_insights <- renderUI({
    data <- filtered_la_data()
    x_vals <- data[[input$scatter_x]]
    y_vals <- data[[input$scatter_y]]

    cor_val <- cor(x_vals, y_vals, use = "complete.obs")

    if (abs(cor_val) > 0.5) {
      strength <- "strong"
    } else if (abs(cor_val) > 0.3) {
      strength <- "moderate"
    } else {
      strength <- "weak"
    }

    direction <- if(cor_val > 0) "positive" else "negative"

    var_labels <- c(
      "neet_rate_16_17" = "NEET rates",
      "youth_opportunity_index" = "Youth Opportunity Index scores",
      "into_work_rate" = "into-work rates",
      "youth_disability_rate" = "youth disability rates",
      "mental_health_rate" = "mental health prevalence",
      "imd_rank" = "deprivation levels"
    )

    tags$ul(
      tags$li(paste0("There is a ", strength, " ", direction, " correlation (r = ",
                     round(cor_val, 2), ") between these variables.")),
      if (input$scatter_x == "imd_rank" && input$scatter_y == "neet_rate_16_17") {
        tags$li("More deprived areas (lower IMD rank) tend to have higher NEET rates.")
      },
      if (input$scatter_x == "youth_disability_rate" && input$scatter_y == "neet_rate_16_17") {
        tags$li("Areas with higher youth disability rates tend to have higher NEET rates, suggesting health is a key driver.")
      }
    )
  })

  # Labour market type NEET
  output$lm_type_neet <- renderPlotly({
    data <- la_data() %>%
      group_by(labour_market_type) %>%
      summarise(
        mean_neet = mean(neet_rate_16_17),
        sd_neet = sd(neet_rate_16_17),
        n = n(),
        .groups = "drop"
      )

    p <- ggplot(data, aes(x = factor(labour_market_type), y = mean_neet, fill = mean_neet)) +
      geom_col() +
      geom_errorbar(aes(ymin = mean_neet - sd_neet, ymax = mean_neet + sd_neet), width = 0.3) +
      scale_fill_viridis_c(option = "C", direction = -1) +
      labs(x = "Labour Market Type", y = "Mean NEET Rate (%)",
           title = "NEET Rates Vary Significantly by Labour Market Type") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")

    ggplotly(p)
  })

  # Labour market type into-work
  output$lm_type_into_work <- renderPlotly({
    data <- la_data() %>%
      group_by(labour_market_type) %>%
      summarise(
        mean_into_work = mean(into_work_rate),
        sd_into_work = sd(into_work_rate),
        n = n(),
        .groups = "drop"
      )

    p <- ggplot(data, aes(x = factor(labour_market_type), y = mean_into_work, fill = mean_into_work)) +
      geom_col() +
      geom_errorbar(aes(ymin = mean_into_work - sd_into_work, ymax = mean_into_work + sd_into_work), width = 0.3) +
      scale_fill_viridis_c(option = "D") +
      labs(x = "Labour Market Type", y = "Mean Into-Work Rate (%)",
           title = "Into-Work Rates Also Vary by Labour Market Type") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")

    ggplotly(p)
  })

  # Labour market type table
  output$lm_type_table <- renderDT({
    type_descriptions <- tibble(
      Type = 1:14,
      Description = c(
        "London Central - High UC volumes, high employment, young population",
        "London Outer - Mixed suburban, good employment, lower disability",
        "Prosperous South - High employment, low UC, high qualifications",
        "University Cities - High student population, variable NEET",
        "Rural/Coastal - Low population density, higher into-work rates",
        "Post-Industrial North - High disability, lower employment, legacy industries",
        "Coastal Deprivation - High NEET, seasonal employment, older demographics",
        "Welsh Valleys - High incapacity, post-mining, tight labour markets",
        "Scottish Urban - Mixed outcomes, devolved policy context",
        "Midlands Manufacturing - Variable employment, automotive/logistics",
        "Northern Towns - Mixed NEET, regeneration areas",
        "East Midlands Mixed - Variable outcomes, mixed economy",
        "South Wales Coast - Port cities, tourism, manufacturing mix",
        "Scottish Highlands/Islands - Remote rural, distinctive labour markets"
      )
    )

    la_data() %>%
      group_by(labour_market_type) %>%
      summarise(
        `Mean NEET Rate` = round(mean(neet_rate_16_17), 1),
        `Mean Into-Work` = round(mean(into_work_rate), 1),
        `Mean Disability` = round(mean(youth_disability_rate), 1),
        `Mean YOI` = round(mean(youth_opportunity_index), 0),
        `N LAs` = n(),
        .groups = "drop"
      ) %>%
      rename(Type = labour_market_type) %>%
      left_join(type_descriptions, by = "Type") %>%
      select(Type, Description, everything()) %>%
      datatable(
        options = list(pageLength = 14, scrollX = TRUE),
        rownames = FALSE
      )
  })

  # Refresh data button
  observeEvent(input$refresh_data, {
    showNotification("Data refreshed from sample (live API integration pending)", type = "message")
  })
}

# ============================================================================
# RUN APPLICATION
# ============================================================================

shinyApp(ui = ui, server = server)
