# Load necessary libraries
library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)       # For interactive plots
library(DT)
library(survey)       # For survey-specific analyses
library(haven)        # For data import and handling labelled data
library(dplyr)        # For data manipulation
library(reshape2)     # For data reshaping
library(rcompanion)   # For statistical tests on ordinal data

# Increase the maximum request size to 50 MB (adjust as needed)
options(shiny.maxRequestSize = 50*1024^2)

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Advanced Survey Analysis App with Audit Trail"),
  sidebarLayout(
    sidebarPanel(
      fileInput('datafile', 'Upload Survey Data',
                accept = c('.csv', '.xlsx', '.sav')),
      tags$hr(),
      uiOutput("global_options_ui")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", 
                 DTOutput("data_preview")),
        tabPanel("Descriptive Statistics", 
                 uiOutput("desc_stats_ui"),
                 DTOutput("descriptive_table")),
        tabPanel("Visualizations", 
                 uiOutput("visualization_ui"),
                 plotlyOutput("plot_output")),
        tabPanel("Statistical Tests", 
                 uiOutput("stat_tests_ui"),
                 verbatimTextOutput("stat_tests_output")),
        tabPanel("Regression Models", 
                 uiOutput("regression_ui"),
                 verbatimTextOutput("regression_output")),
        tabPanel("Audit Trail", 
                 verbatimTextOutput("audit_trail"),
                 downloadButton("download_code", "Download R Script")),
        tabPanel("Report", downloadButton("download_report", "Download Report"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive expression to read and preprocess the uploaded data
  survey_data <- reactive({
    req(input$datafile)
    ext <- tools::file_ext(input$datafile$name)
    
    # Read data based on file extension
    df <- switch(ext,
                 csv = read.csv(input$datafile$datapath, stringsAsFactors = FALSE),
                 xlsx = readxl::read_excel(input$datafile$datapath),
                 sav = {
                   data_spss <- haven::read_spss(input$datafile$datapath)
                   # Convert haven_labelled variables to factors
                   df_converted <- haven::as_factor(data_spss)
                   df_converted
                 },
                 stop("Invalid file type"))
    
    # Convert character variables to factors
    df <- df %>% mutate_if(is.character, as.factor)
    
    # Store data loading code for audit trail
    code$load_data <<- paste0("data <- read_", ext, "('", input$datafile$name, "')")
    
    df
  })
  
  # Initialize an environment to store code snippets
  code <- new.env()
  
  # Global options UI
  output$global_options_ui <- renderUI({
    req(survey_data())
    tagList(
      checkboxInput("show_labels", "Show Variable Labels (if available)", FALSE)
    )
  })
  
  # Display data preview
  output$data_preview <- renderDT({
    req(survey_data())
    datatable(head(survey_data()))
  })
  
  ### Descriptive Statistics Tab ###
  
  # Descriptive Statistics UI
  output$desc_stats_ui <- renderUI({
    req(survey_data())
    tagList(
      selectInput("desc_vars", "Select Variables", choices = names(survey_data()), multiple = TRUE)
    )
  })
  
  # Descriptive Statistics Output
  output$descriptive_table <- renderDT({
    req(input$desc_vars)
    
    data <- survey_data()[, input$desc_vars, drop = FALSE]
    
    # Initialize an empty list to store results
    desc_list <- list()
    
    for (var in input$desc_vars) {
      var_data <- data[[var]]
      if (is.numeric(var_data)) {
        # Numeric variable: show mean, median, SD, etc.
        desc <- data.frame(
          Statistic = c("Mean", "Median", "Standard Deviation", "Variance", "Range"),
          Value = c(
            round(mean(var_data, na.rm = TRUE), 3),
            round(median(var_data, na.rm = TRUE), 3),
            round(sd(var_data, na.rm = TRUE), 3),
            round(var(var_data, na.rm = TRUE), 3),
            paste(range(var_data, na.rm = TRUE), collapse = " - ")
          ),
          stringsAsFactors = FALSE
        )
        desc_list[[var]] <- desc
      } else {
        # Categorical or ordinal variable: show frequency table
        freq_table <- as.data.frame(table(var_data, useNA = "ifany"))
        colnames(freq_table) <- c("Category", "Count")
        freq_table$Percentage <- round(100 * freq_table$Count / sum(freq_table$Count), 2)
        desc_list[[var]] <- freq_table
      }
    }
    
    # Combine results into one table
    desc_results <- do.call(rbind, lapply(names(desc_list), function(var) {
      df <- desc_list[[var]]
      df$Variable <- var
      df
    }))
    
    # Rearrange columns
    desc_results <- desc_results[, c("Variable", setdiff(names(desc_results), "Variable"))]
    
    # Store code for audit trail
    code$descriptive <<- paste0(
      "# Descriptive Statistics\n",
      "# For numeric variables, calculated mean, median, SD, variance, and range.\n",
      "# For categorical/ordinal variables, provided frequency counts and percentages."
    )
    
    datatable(desc_results, options = list(pageLength = 10))
  })
  
  ### Visualizations Tab ###
  
  # Visualization UI
  output$visualization_ui <- renderUI({
    req(survey_data())
    tagList(
      selectInput("plot_type", "Select Plot Type",
                  choices = c("Histogram", "Bar Chart", "Box Plot", "Scatter Plot", "Line Plot")),
      selectInput("x_var", "X-axis Variable", choices = names(survey_data())),
      selectInput("y_var", "Y-axis Variable (for Scatter, Line, Box plots)", 
                  choices = c(None = '', names(survey_data())), selected = ''),
      selectInput("color_var", "Color By (Optional)", choices = c(None = '', names(survey_data()))),
      selectInput("facet_var", "Facet By (Optional)", choices = c(None = '', names(survey_data()))),
      checkboxInput("apply_filter", "Filter Data", FALSE),
      conditionalPanel(
        condition = "input.apply_filter == true",
        textInput("filter_condition", "Filter Condition (e.g., age > 30)")
      )
    )
  })
  
  # Visualization Output (same as before)
  # [Code remains unchanged, unless there are specific enhancements requested]
  
  ### Statistical Tests Tab ###
  
  # Statistical Tests UI
  output$stat_tests_ui <- renderUI({
    req(survey_data())
    tagList(
      selectInput("test_type", "Select Statistical Test",
                  choices = c("t-test", "ANOVA", "Chi-Square Test", "Correlation Test", "Mann-Whitney U Test", "Kruskal-Wallis Test")),
      conditionalPanel(
        condition = "input.test_type == 't-test' || input.test_type == 'ANOVA' || input.test_type == 'Mann-Whitney U Test' || input.test_type == 'Kruskal-Wallis Test'",
        selectInput("response_var", "Response Variable", choices = names(survey_data())),
        selectInput("group_var", "Grouping Variable", choices = names(survey_data()))
      ),
      conditionalPanel(
        condition = "input.test_type == 'Chi-Square Test'",
        selectInput("chi_var1", "Variable 1", choices = names(survey_data())),
        selectInput("chi_var2", "Variable 2", choices = names(survey_data()))
      ),
      conditionalPanel(
        condition = "input.test_type == 'Correlation Test'",
        selectInput("cor_var1", "Variable 1", choices = names(survey_data())),
        selectInput("cor_var2", "Variable 2", choices = names(survey_data())),
        selectInput("method", "Correlation Method", choices = c("pearson", "spearman", "kendall"))
      )
    )
  })
  
  # Statistical Tests Output
  output$stat_tests_output <- renderPrint({
    req(input$test_type)
    data <- survey_data()
    
    test_result <- NULL
    
    if (input$test_type == "t-test") {
      # [Same as before, with appropriate checks]
    } else if (input$test_type == "ANOVA") {
      # [Same as before]
    } else if (input$test_type == "Chi-Square Test") {
      # [Same as before]
    } else if (input$test_type == "Correlation Test") {
      # [Same as before]
    } else if (input$test_type == "Mann-Whitney U Test") {
      req(input$response_var, input$group_var)
      # Ensure response variable is at least ordinal
      if (!is.numeric(data[[input$response_var]]) && !is.ordered(data[[input$response_var]])) {
        showNotification("Response variable must be numeric or ordinal for Mann-Whitney U Test.", type = "error")
        return(NULL)
      }
      if (!is.factor(data[[input$group_var]])) {
        data[[input$group_var]] <- as.factor(data[[input$group_var]])
      }
      if (nlevels(data[[input$group_var]]) != 2) {
        showNotification("Grouping variable must have exactly two levels for Mann-Whitney U Test.", type = "error")
        return(NULL)
      }
      formula <- as.formula(paste(input$response_var, "~", input$group_var))
      test_result <- wilcox.test(formula, data = data)
      # Store code for audit trail
      code$stats <<- paste0(
        "# Mann-Whitney U Test\n",
        "wilcox.test(", input$response_var, " ~ ", input$group_var, ", data = data)"
      )
    } else if (input$test_type == "Kruskal-Wallis Test") {
      req(input$response_var, input$group_var)
      # Ensure response variable is at least ordinal
      if (!is.numeric(data[[input$response_var]]) && !is.ordered(data[[input$response_var]])) {
        showNotification("Response variable must be numeric or ordinal for Kruskal-Wallis Test.", type = "error")
        return(NULL)
      }
      if (!is.factor(data[[input$group_var]])) {
        data[[input$group_var]] <- as.factor(data[[input$group_var]])
      }
      if (nlevels(data[[input$group_var]]) < 2) {
        showNotification("Grouping variable must have at least two levels for Kruskal-Wallis Test.", type = "error")
        return(NULL)
      }
      formula <- as.formula(paste(input$response_var, "~", input$group_var))
      test_result <- kruskal.test(formula, data = data)
      # Store code for audit trail
      code$stats <<- paste0(
        "# Kruskal-Wallis Test\n",
        "kruskal.test(", input$response_var, " ~ ", input$group_var, ", data = data)"
      )
    }
    
    test_result
  })
  
  ### Regression Models Tab ###
  
  # Regression Models UI
  output$regression_ui <- renderUI({
    req(survey_data())
    tagList(
      selectInput("reg_type", "Select Regression Type",
                  choices = c("Linear Regression", "Logistic Regression", "Ordinal Logistic Regression")),
      selectInput("dep_var", "Dependent Variable", choices = names(survey_data())),
      selectInput("indep_vars", "Independent Variables", choices = names(survey_data()), multiple = TRUE),
      conditionalPanel(
        condition = "input.reg_type == 'Logistic Regression'",
        checkboxInput("recode_dep_var", "Recode Dependent Variable to Binary", FALSE),
        conditionalPanel(
          condition = "input.recode_dep_var == true",
          selectInput("dep_var_value", "Value for 'Success' (others will be 'Failure')", choices = NULL)
        )
      )
    )
  })
  
  # Update choices for 'dep_var_value' when 'dep_var' changes
  observeEvent(input$dep_var, {
    updateSelectInput(session, "dep_var_value", choices = unique(survey_data()[[input$dep_var]]))
  })
  
  # Regression Models Output
  output$regression_output <- renderPrint({
    req(input$reg_type, input$dep_var, input$indep_vars)
    data <- survey_data()
    
    # Ensure dependent and independent variables are available
    if (!(input$dep_var %in% names(data))) {
      showNotification("Dependent variable not found in data.", type = "error")
      return(NULL)
    }
    if (!all(input$indep_vars %in% names(data))) {
      showNotification("One or more independent variables not found in data.", type = "error")
      return(NULL)
    }
    
    # Recode dependent variable for logistic regression if requested
    if (input$reg_type == "Logistic Regression" && input$recode_dep_var) {
      req(input$dep_var_value)
      data[[input$dep_var]] <- factor(ifelse(data[[input$dep_var]] == input$dep_var_value, input$dep_var_value, "Other"))
    }
    
    formula_text <- paste(input$dep_var, "~", paste(input$indep_vars, collapse = " + "))
    formula <- as.formula(formula_text)
    
    if (input$reg_type == "Linear Regression") {
      # Ensure dependent variable is numeric
      if (!is.numeric(data[[input$dep_var]])) {
        showNotification("Dependent variable must be numeric for Linear Regression.", type = "error")
        return(NULL)
      }
      
      model <- lm(formula, data = data)
      # Store code for audit trail
      code$regression <<- paste0(
        "# Linear Regression\n",
        "model <- lm(", formula_text, ", data = data)\n",
        "summary(model)"
      )
    } else if (input$reg_type == "Logistic Regression") {
      # Ensure dependent variable is binary factor
      if (!is.factor(data[[input$dep_var]])) {
        data[[input$dep_var]] <- as.factor(data[[input$dep_var]])
      }
      if (nlevels(data[[input$dep_var]]) != 2) {
        showNotification("Dependent variable must be binary for Logistic Regression.", type = "error")
        return(NULL)
      }
      
      model <- glm(formula, data = data, family = binomial)
      # Store code for audit trail
      code$regression <<- paste0(
        "# Logistic Regression\n",
        "model <- glm(", formula_text, ", data = data, family = binomial)\n",
        "summary(model)"
      )
    } else if (input$reg_type == "Ordinal Logistic Regression") {
      # Ensure dependent variable is ordered factor
      if (!is.ordered(data[[input$dep_var]])) {
        data[[input$dep_var]] <- as.ordered(data[[input$dep_var]])
      }
      
      model <- MASS::polr(formula, data = data, Hess = TRUE)
      # Store code for audit trail
      code$regression <<- paste0(
        "# Ordinal Logistic Regression\n",
        "model <- MASS::polr(", formula_text, ", data = data, Hess = TRUE)\n",
        "summary(model)"
      )
    }
    
    summary(model)
  })
  
  ### Audit Trail ###
  
  # Display Audit Trail
  output$audit_trail <- renderPrint({
    # Combine all code snippets
    code_snippets <- unlist(as.list(code))
    if (length(code_snippets) == 0) {
      "No analyses performed yet."
    } else {
      cat("# Audit Trail - Reproducible R Code\n")
      cat("# Data Loading\n")
      cat(code$load_data, "\n\n")
      if (!is.null(code$descriptive)) {
        cat(code$descriptive, "\n\n")
      }
      if (!is.null(code$visualizations)) {
        cat(code$visualizations, "\n\n")
      }
      if (!is.null(code$stats)) {
        cat(code$stats, "\n\n")
      }
      if (!is.null(code$regression)) {
        cat(code$regression, "\n\n")
      }
    }
  })
  
  # Downloadable Audit Trail
  output$download_code <- downloadHandler(
    filename = function() {
      paste("audit_trail_", Sys.Date(), ".R", sep = "")
    },
    content = function(file) {
      code_snippets <- unlist(as.list(code))
      if (length(code_snippets) == 0) {
        writeLines("No analyses performed yet.", con = file)
      } else {
        writeLines("# Audit Trail - Reproducible R Code", con = file)
        writeLines("# Data Loading", con = file, sep = "\n")
        writeLines(code$load_data, con = file, sep = "\n\n")
        if (!is.null(code$descriptive)) {
          writeLines(code$descriptive, con = file, sep = "\n\n")
        }
        if (!is.null(code$visualizations)) {
          writeLines(code$visualizations, con = file, sep = "\n\n")
        }
        if (!is.null(code$stats)) {
          writeLines(code$stats, con = file, sep = "\n\n")
        }
        if (!is.null(code$regression)) {
          writeLines(code$regression, con = file, sep = "\n\n")
        }
      }
    }
  )
  
  # Report Generation (Placeholder)
  output$download_report <- downloadHandler(
    filename = function() { "survey_analysis_report.pdf" },
    content = function(file) {
      # Code to generate and save the report
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
