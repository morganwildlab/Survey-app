# UK Benefits Data Fetcher Module
# Automatically downloads and processes data from DWP/GOV.UK
# Supports automatic integration of new data releases

library(httr)
library(jsonlite)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

# ============================================================================
# CONFIGURATION
# ============================================================================

DWP_CONFIG <- list(
  # GOV.UK Publications API
  publications_api = "https://www.gov.uk/api/content",

  # Collection page for benefit expenditure tables
  collection_path = "/government/collections/benefit-expenditure-tables",

  # Known publication paths (updated with each release)
  expenditure_tables = list(
    "2025" = "/government/publications/benefit-expenditure-and-caseload-tables-2025",
    "2024" = "/government/publications/benefit-expenditure-and-caseload-tables-2024",
    "2023" = "/government/publications/benefit-expenditure-and-caseload-tables-2023"
  ),

  # Stat-Xplore API (requires registration)
  stat_xplore = list(
    base_url = "https://stat-xplore.dwp.gov.uk/webapi/rest/v1",
    # Note: API key required for full access
    tables = list(
      uc = "str:folder:fuc",
      esa = "str:folder:fesa",
      pip = "str:folder:fpip"
    )
  ),

  # OBR data sources
  obr = list(
    welfare_spending = "https://obr.uk/forecasts-in-depth/tax-by-tax-spend-by-spend/welfare-spending-universal-credit/",
    welfare_trends = "https://obr.uk/wtr/welfare-trends-report-october-2024/"
  ),

  # Cache settings
  cache_dir = "data_cache",
  cache_duration_hours = 24
)

# ============================================================================
# GOV.UK API FUNCTIONS
# ============================================================================

#' Check for new data releases from GOV.UK
#' @return List with information about available data releases
check_govuk_releases <- function() {
  tryCatch({
    # Query the collection page API
    url <- paste0(DWP_CONFIG$publications_api, DWP_CONFIG$collection_path)

    response <- GET(url, timeout(30))

    if (status_code(response) == 200) {
      content <- fromJSON(content(response, "text", encoding = "UTF-8"))

      # Extract document links
      documents <- content$links$documents

      if (!is.null(documents)) {
        releases <- lapply(documents, function(doc) {
          list(
            title = doc$title,
            path = doc$base_path,
            updated = doc$public_updated_at,
            description = doc$description
          )
        })

        return(list(
          success = TRUE,
          releases = releases,
          checked_at = Sys.time()
        ))
      }
    }

    return(list(success = FALSE, message = "No documents found"))

  }, error = function(e) {
    return(list(success = FALSE, message = e$message))
  })
}

#' Get document details and download links from GOV.UK
#' @param publication_path Path to the publication
#' @return List with document details and attachment URLs
get_publication_attachments <- function(publication_path) {
  tryCatch({
    url <- paste0(DWP_CONFIG$publications_api, publication_path)

    response <- GET(url, timeout(30))

    if (status_code(response) == 200) {
      content <- fromJSON(content(response, "text", encoding = "UTF-8"))

      # Extract attachments from the document
      if (!is.null(content$details$documents)) {
        attachments <- lapply(content$details$documents, function(doc) {
          # Parse the embedded attachment HTML or structured data
          list(
            title = doc$title %||% "Unknown",
            url = doc$url %||% doc$attachment_url,
            content_type = doc$content_type
          )
        })

        return(list(
          success = TRUE,
          title = content$title,
          updated = content$public_updated_at,
          attachments = attachments
        ))
      }
    }

    return(list(success = FALSE, message = "Could not retrieve attachments"))

  }, error = function(e) {
    return(list(success = FALSE, message = e$message))
  })
}

# ============================================================================
# DATA DOWNLOAD AND PARSING
# ============================================================================

#' Download and parse ODS/Excel file from GOV.UK
#' @param url URL of the file to download
#' @param dest_file Local destination file path
#' @return Tibble with parsed data or NULL on error
download_and_parse_expenditure_file <- function(url, dest_file = NULL) {
  if (is.null(dest_file)) {
    dest_file <- tempfile(fileext = tools::file_ext(url))
  }

  tryCatch({
    # Download the file
    download.file(url, dest_file, mode = "wb", quiet = TRUE)

    # Determine file type and parse
    ext <- tolower(tools::file_ext(dest_file))

    if (ext %in% c("xlsx", "xls")) {
      # Read Excel file
      sheets <- excel_sheets(dest_file)

      # Look for main expenditure sheet
      exp_sheet <- sheets[grepl("expenditure|outturn", sheets, ignore.case = TRUE)][1]

      if (!is.na(exp_sheet)) {
        data <- read_excel(dest_file, sheet = exp_sheet, skip = 2)
        return(clean_expenditure_data(data))
      }
    } else if (ext == "ods") {
      # ODS files require readODS package
      if (requireNamespace("readODS", quietly = TRUE)) {
        data <- readODS::read_ods(dest_file, skip = 2)
        return(clean_expenditure_data(data))
      } else {
        message("Install 'readODS' package to read ODS files: install.packages('readODS')")
      }
    } else if (ext == "csv") {
      data <- read.csv(dest_file, stringsAsFactors = FALSE)
      return(clean_expenditure_data(data))
    }

    return(NULL)

  }, error = function(e) {
    message("Error downloading/parsing file: ", e$message)
    return(NULL)
  })
}

#' Clean and standardize expenditure data
#' @param raw_data Raw data from file
#' @return Cleaned tibble
clean_expenditure_data <- function(raw_data) {
  # This function handles various formats of DWP expenditure data

  # Convert to tibble
  data <- as_tibble(raw_data)

  # Remove empty rows and columns
  data <- data %>%
    select(where(~ !all(is.na(.)))) %>%
    filter(rowSums(!is.na(.)) > 0)

  # Try to identify the benefit name column
  name_col <- names(data)[1]

  # Identify year columns (format: YYYY-YY or just years)
  year_cols <- names(data)[grepl("^\\d{4}", names(data))]

  if (length(year_cols) > 0) {
    # Reshape to long format
    data_long <- data %>%
      select(all_of(c(name_col, year_cols))) %>%
      rename(Benefit = all_of(name_col)) %>%
      pivot_longer(
        cols = all_of(year_cols),
        names_to = "fiscal_year",
        values_to = "expenditure"
      ) %>%
      mutate(
        expenditure = as.numeric(expenditure),
        year_start = as.integer(substr(fiscal_year, 1, 4))
      ) %>%
      filter(!is.na(expenditure))

    return(data_long)
  }

  return(data)
}

# ============================================================================
# STAT-XPLORE API FUNCTIONS (Requires API Key)
# ============================================================================

#' Initialize Stat-Xplore connection
#' @param api_key Your Stat-Xplore API key
#' @return Connection object
init_stat_xplore <- function(api_key = NULL) {
  if (is.null(api_key)) {
    # Try to get from environment variable
    api_key <- Sys.getenv("STAT_XPLORE_API_KEY")
    if (api_key == "") {
      message("No Stat-Xplore API key found. Set STAT_XPLORE_API_KEY environment variable.")
      message("Register at: https://stat-xplore.dwp.gov.uk/")
      return(NULL)
    }
  }

  return(list(
    api_key = api_key,
    base_url = DWP_CONFIG$stat_xplore$base_url
  ))
}

#' Query Stat-Xplore API
#' @param connection Connection object from init_stat_xplore
#' @param table Table identifier
#' @param dimensions List of dimensions to query
#' @return Data from API
query_stat_xplore <- function(connection, table, dimensions = list()) {
  if (is.null(connection)) return(NULL)

  tryCatch({
    # Build query
    query <- list(
      database = table,
      measures = list("str:count:fuc:v_f_uc"),
      dimensions = dimensions
    )

    response <- POST(
      paste0(connection$base_url, "/table"),
      add_headers(APIKey = connection$api_key),
      body = toJSON(query, auto_unbox = TRUE),
      content_type_json(),
      timeout(60)
    )

    if (status_code(response) == 200) {
      return(fromJSON(content(response, "text", encoding = "UTF-8")))
    }

    return(NULL)

  }, error = function(e) {
    message("Stat-Xplore query error: ", e$message)
    return(NULL)
  })
}

# ============================================================================
# CACHING FUNCTIONS
# ============================================================================

#' Get cached data if valid
#' @param cache_name Name of the cache file
#' @return Cached data or NULL
get_cached_data <- function(cache_name) {
  cache_dir <- DWP_CONFIG$cache_dir
  cache_file <- file.path(cache_dir, paste0(cache_name, ".rds"))
  meta_file <- file.path(cache_dir, paste0(cache_name, "_meta.json"))

  if (file.exists(cache_file) && file.exists(meta_file)) {
    meta <- fromJSON(meta_file)
    cache_time <- as.POSIXct(meta$cached_at)

    if (difftime(Sys.time(), cache_time, units = "hours") < DWP_CONFIG$cache_duration_hours) {
      return(list(
        data = readRDS(cache_file),
        meta = meta
      ))
    }
  }

  return(NULL)
}

#' Save data to cache
#' @param data Data to cache
#' @param cache_name Name of the cache file
#' @param source Source description
save_to_cache <- function(data, cache_name, source = "Unknown") {
  cache_dir <- DWP_CONFIG$cache_dir

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  cache_file <- file.path(cache_dir, paste0(cache_name, ".rds"))
  meta_file <- file.path(cache_dir, paste0(cache_name, "_meta.json"))

  saveRDS(data, cache_file)

  write_json(
    list(
      cached_at = as.character(Sys.time()),
      source = source,
      rows = if(is.data.frame(data)) nrow(data) else NA
    ),
    meta_file
  )

  return(TRUE)
}

#' Clear all cached data
clear_cache <- function() {
  cache_dir <- DWP_CONFIG$cache_dir

  if (dir.exists(cache_dir)) {
    files <- list.files(cache_dir, full.names = TRUE)
    file.remove(files)
    message("Cache cleared: ", length(files), " files removed")
  }
}

# ============================================================================
# MAIN DATA FETCHING FUNCTION
# ============================================================================

#' Fetch latest benefit expenditure data
#' Uses cache if valid, otherwise attempts to download fresh data
#' Falls back to built-in sample data if download fails
#'
#' @param use_cache Whether to use cached data
#' @param force_refresh Force download of fresh data
#' @return List with expenditure data, caseload data, and metadata
fetch_benefit_data <- function(use_cache = TRUE, force_refresh = FALSE) {

  # Check cache first
  if (use_cache && !force_refresh) {
    cached <- get_cached_data("benefit_data")
    if (!is.null(cached)) {
      message("Using cached data from ", cached$meta$cached_at)
      return(cached$data)
    }
  }

  # Try to fetch from GOV.UK
  message("Checking for new data from GOV.UK...")

  releases <- check_govuk_releases()

  if (releases$success && length(releases$releases) > 0) {
    # Get the most recent release
    latest <- releases$releases[[1]]
    message("Found release: ", latest$title)

    # Get attachments
    attachments <- get_publication_attachments(latest$path)

    if (attachments$success) {
      # Look for expenditure file
      exp_attachment <- NULL
      for (att in attachments$attachments) {
        if (grepl("expenditure|outturn", att$title, ignore.case = TRUE)) {
          exp_attachment <- att
          break
        }
      }

      if (!is.null(exp_attachment) && !is.null(exp_attachment$url)) {
        message("Downloading: ", exp_attachment$title)
        exp_data <- download_and_parse_expenditure_file(exp_attachment$url)

        if (!is.null(exp_data)) {
          result <- list(
            expenditure = exp_data,
            source = paste("DWP:", latest$title),
            source_url = paste0("https://www.gov.uk", latest$path),
            last_updated = Sys.time(),
            release_date = latest$updated
          )

          # Cache the result
          save_to_cache(result, "benefit_data", result$source)

          return(result)
        }
      }
    }
  }

  # Fall back to sample data
  message("Using built-in sample data")

  # Source the main app file to get sample data functions
  # (In production, these would be in a shared module)

  result <- list(
    expenditure = NULL,  # Will use sample data from main app
    caseload = NULL,
    source = "Built-in sample data based on DWP figures",
    source_url = "https://www.gov.uk/government/collections/benefit-expenditure-tables",
    last_updated = Sys.time(),
    notes = "Live data fetching unavailable. Using sample data."
  )

  return(result)
}

# ============================================================================
# DATA VALIDATION FUNCTIONS
# ============================================================================

#' Validate expenditure data structure
#' @param data Data to validate
#' @return List with validation results
validate_expenditure_data <- function(data) {
  issues <- c()

  # Check required columns
  required_cols <- c("fiscal_year", "year_start")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    issues <- c(issues, paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  # Check for numeric data
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  if (length(numeric_cols) == 0) {
    issues <- c(issues, "No numeric columns found")
  }

  # Check for negative values
  for (col in numeric_cols) {
    if (any(data[[col]] < 0, na.rm = TRUE)) {
      issues <- c(issues, paste("Negative values in", col))
    }
  }

  # Check year range
  if ("year_start" %in% names(data)) {
    year_range <- range(data$year_start, na.rm = TRUE)
    if (year_range[1] < 1990 || year_range[2] > 2040) {
      issues <- c(issues, paste("Unusual year range:", year_range[1], "-", year_range[2]))
    }
  }

  return(list(
    valid = length(issues) == 0,
    issues = issues
  ))
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Get data update schedule
#' Returns information about when new data is typically released
get_update_schedule <- function() {
  list(
    description = "DWP Benefit Expenditure Tables are typically updated twice yearly",
    timing = c(
      "Spring: Usually March/April following Spring Statement",
      "Autumn: Usually November/December following Autumn Statement"
    ),
    next_expected = "Check GOV.UK for latest release dates"
  )
}

#' Format fiscal year string
#' @param year Start year of fiscal year
#' @return Formatted string (e.g., "2023-24")
format_fiscal_year <- function(year) {
  paste0(year, "-", substr(year + 1, 3, 4))
}

#' Parse fiscal year string
#' @param fy_string Fiscal year string (e.g., "2023-24")
#' @return Start year as integer
parse_fiscal_year <- function(fy_string) {
  as.integer(substr(fy_string, 1, 4))
}

# ============================================================================
# EXPORT
# ============================================================================

# List of exported functions for use in other scripts
DATA_FETCHER_EXPORTS <- list(
  fetch_benefit_data = fetch_benefit_data,
  check_govuk_releases = check_govuk_releases,
  get_cached_data = get_cached_data,
  clear_cache = clear_cache,
  validate_expenditure_data = validate_expenditure_data,
  get_update_schedule = get_update_schedule
)
