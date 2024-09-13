library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(DT)
library(haven)
library(dplyr)
library(rcompanion)
library(MASS)
library(readxl)
library(RColorBrewer)
library(viridis)
library(shinyWidgets)
library(bslib)

# Increase the maximum request size to 50 MB
options(shiny.maxRequestSize = 50*1024^2)

# Define UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "lux"),
  tags$head(
    tags$style(HTML("
      /* Custom CSS to enhance the appearance */
      body {
        font-family: 'Roboto', sans-serif;
      }
      .navbar-brand {
        font-size: 24px;
        font-weight: bold;
      }
      .shiny-input-container {
        margin-bottom: 15px;
      }
      .tab-content {
        padding-top: 20px;
      }
      /* Style adjustments for inputs within tabs */
      .tab-pane .form-group {
        margin-bottom: 15px;
      }
      /* Improve dropdown appearance */
      .selectize-input, .selectize-dropdown {
        font-size: 14px;
      }
    "))
  ),
  titlePanel("Advanced Survey Analysis App with Audit Trail"),
  sidebarLayout(
    sidebarPanel(
      fileInput('datafile', 'Upload Survey Data',
                accept = c('.csv', '.xlsx', '.sav'),
                buttonLabel = "Browse...",
                placeholder = "No file selected"),
      tags$hr(),
      uiOutput("global_options_ui"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", 
                 DTOutput("data_preview")),
        tabPanel("Descriptive Statistics", 
                 fluidRow(
                   column(12, uiOutput("desc_stats_ui"))
                 ),
                 DTOutput("descriptive_table")),
        tabPanel("Visualizations", 
                 fluidRow(
                   column(12, uiOutput("visualization_ui"))
                 ),
                 plotlyOutput("plot_output", height = "700px"),
                 tags$br(),
                 downloadButton("download_plot", "Download Plot", class = "btn btn-success"),
                 downloadButton("download_plot_data", "Download Plot Data", class = "btn btn-info")),
        tabPanel("Statistical Tests", 
                 fluidRow(
                   column(12, uiOutput("stat_tests_ui"))
                 ),
                 verbatimTextOutput("stat_tests_output")),
        tabPanel("Regression Models", 
                 fluidRow(
                   column(12, uiOutput("regression_ui"))
                 ),
                 verbatimTextOutput("regression_output")),
        tabPanel("Audit Trail", 
                 verbatimTextOutput("audit_trail"),
                 downloadButton("download_code", "Download R Script")),
        tabPanel("Report", downloadButton("download_report", "Download Report"))
      ),
      width = 9
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
      prettyCheckbox(
        inputId = "show_labels",
        label = "Show Variable Labels (if available)",
        status = "info",
        shape = "round",
        animation = "pulse"
      )
    )
  })
  
  # Display data preview
  output$data_preview <- renderDT({
    req(survey_data())
    datatable(head(survey_data()), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  ### Descriptive Statistics Tab ###
  
  # Descriptive Statistics UI
  output$desc_stats_ui <- renderUI({
    req(survey_data())
    tagList(
      pickerInput(
        inputId = "desc_vars",
        label = "Select Variables",
        choices = names(survey_data()),
        options = list(`actions-box` = TRUE),
        multiple = TRUE
      )
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
        # Numeric variable: show mean, median, SD, variance, and range
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
    
    datatable(desc_results, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  ### Visualizations Tab ###
  
  # Visualization UI
  output$visualization_ui <- renderUI({
    req(survey_data())
    tagList(
      fluidRow(
        column(4,
               selectInput("plot_type", "Select Plot Type",
                           choices = c("Histogram", "Bar Chart", "Box Plot", "Scatter Plot", "Line Plot"))
        ),
        column(4,
               selectInput("x_var", "X-axis Variable", choices = names(survey_data()))
        ),
        column(4,
               selectInput("y_var", "Y-axis Variable (for Scatter, Line, Box plots)", 
                           choices = c(None = '', names(survey_data())), selected = '')
        )
      ),
      fluidRow(
        column(4,
               selectInput("color_var", "Color By (Optional)", choices = c(None = '', names(survey_data())))
        ),
        column(4,
               selectInput("facet_var", "Facet By (Optional)", choices = c(None = '', names(survey_data())))
        ),
        column(4,
               selectInput("weight_var", "Weight Variable (Optional)", choices = c(None = '', names(survey_data())))
        )
      ),
      fluidRow(
        column(4,
               prettyCheckbox(
                 inputId = "facet_slider",
                 label = "Enable Facet Slider View",
                 status = "primary",
                 shape = "round",
                 animation = "pulse"
               )
        ),
        column(4,
               conditionalPanel(
                 condition = "input.facet_slider == true && input.facet_var != ''",
                 uiOutput("facet_level_ui")
               )
        ),
        column(4,
               prettyCheckbox(
                 inputId = "apply_filter",
                 label = "Filter Data",
                 status = "primary",
                 shape = "round",
                 animation = "pulse"
               )
        )
      ),
      conditionalPanel(
        condition = "input.apply_filter == true",
        textInput("filter_condition", "Filter Condition (e.g., age > 30)")
      )
    )
  })
  
  # Facet Level UI for slider view
  output$facet_level_ui <- renderUI({
    req(input$facet_var)
    data <- survey_data()
    facet_levels <- unique(data[[input$facet_var]])
    pickerInput(
      inputId = "facet_level",
      label = "Select Facet Level",
      choices = facet_levels,
      options = list(`live-search` = TRUE)
    )
  })
  
  # Visualization Output
  plot_obj <- reactiveVal(NULL)  # To store the plot object for downloading
  plot_data_reactive <- reactiveVal(NULL)  # To store the plot data for downloading
  
  # Visualization Output
  plot_obj <- reactiveVal(NULL)  # To store the plot object for downloading
  plot_data_reactive <- reactiveVal(NULL)  # To store the plot data for downloading
  
  output$plot_output <- renderPlotly({
    req(input$plot_type, input$x_var)
    data <- survey_data()
    
    # Apply filter if selected
    if (input$apply_filter && input$filter_condition != "") {
      data <- tryCatch({
        data %>% filter(eval(parse(text = input$filter_condition)))
      }, error = function(e) {
        showNotification("Invalid filter condition in Visualization tab.", type = "error")
        data
      })
    }
    
    # Handle weight variable
    weight_var <- if (input$weight_var != '') input$weight_var else NULL
    
    # Prepare the data for plotting
    plot_data <- data
    
    # For bar charts, compute counts and percentages
    if (input$plot_type == "Bar Chart") {
      group_vars <- input$x_var
      if (input$color_var != '') {
        group_vars <- c(group_vars, input$color_var)
      }
      if (input$facet_var != '') {
        group_vars <- c(group_vars, input$facet_var)
      }
      if (!is.null(weight_var)) {
        plot_data <- plot_data %>%
          group_by(across(all_of(group_vars))) %>%
          summarise(Count = sum(!!sym(weight_var), na.rm = TRUE), .groups = 'drop')
      } else {
        plot_data <- plot_data %>%
          group_by(across(all_of(group_vars))) %>%
          summarise(Count = n(), .groups = 'drop')
      }
      total_count <- sum(plot_data$Count)
      plot_data <- plot_data %>%
        mutate(Percentage = 100 * Count / total_count)
      
      # Round counts to 2 decimal places
      plot_data$Count <- round(plot_data$Count, 2)
    }
    
    # Now, create the plot
    p <- ggplot(plot_data)
    
    # For bar chart with percentages and counts
    if (input$plot_type == "Bar Chart") {
      p <- p + aes_string(x = input$x_var, y = "Count")
      if (input$color_var != '') {
        p <- p + aes_string(fill = input$color_var)
      }
      p <- p + geom_bar(stat = "identity", position = "dodge")
      p <- p + geom_text(aes(label = paste0(Count, "\n", round(Percentage, 2), "%")), 
                         position = position_dodge(width = 0.9), vjust = -0.25, size = 3.5)
    } else if (input$plot_type == "Histogram") {
      # Handle Histogram
      p <- p + aes_string(x = input$x_var)
      if (input$color_var != '') {
        p <- p + aes_string(fill = input$color_var)
      }
      if (!is.null(weight_var)) {
        p <- p + geom_histogram(aes(weight = !!sym(weight_var)), fill = "#0073C2FF", color = "white")
      } else {
        p <- p + geom_histogram(fill = "#0073C2FF", color = "white")
      }
    } else {
      # For plots requiring y_var
      req(input$y_var != '')
      p <- p + aes_string(x = input$x_var, y = input$y_var)
      if (input$color_var != '') {
        p <- p + aes_string(color = input$color_var)
      }
      if (input$plot_type == "Box Plot") {
        p <- p + geom_boxplot()
      } else if (input$plot_type == "Scatter Plot") {
        p <- p + geom_point()
      } else if (input$plot_type == "Line Plot") {
        p <- p + geom_line()
      }
    }
    
    # Implement facets as a long series of single charts
    if (input$facet_var != '') {
      p <- p + facet_wrap(as.formula(paste('~', input$facet_var)), ncol = 1)
    }
    
    # Apply themes and adjust x-axis labels
    p <- p + theme_minimal(base_size = 15) + 
      theme(
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 12, face = "bold")
      )
    
    # Apply color palette
    if (input$color_var != '') {
      if (input$plot_type == "Bar Chart") {
        p <- p + scale_fill_viridis_d()
      } else {
        p <- p + scale_color_viridis_d()
      }
    }
    
    # Store code for audit trail
    code$visualizations <<- paste0(
      "# Visualization\n",
      "ggplot(data, aes(x = ", input$x_var,
      ifelse(input$plot_type == "Bar Chart", ", y = Count", ""),
      ifelse(input$y_var != '' && input$plot_type != "Bar Chart", paste0(", y = ", input$y_var), ""),
      ifelse(input$color_var != '', paste0(", fill = ", input$color_var), ""),
      ifelse(!is.null(weight_var), paste0(", weight = ", weight_var), ""),
      ")) + ...")
    
    # Store plot data for downloading
    plot_data_reactive(plot_data)
    
    plot_obj(p)  # Store the plot object for downloading
    
    ggplotly(p)
  })
  
  # Download Plot Handler
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_obj(), device = "png", width = 10, height = 7)
    }
  )
  
  # Download Plot Data Handler
  output$download_plot_data <- downloadHandler(
    filename = function() {
      paste("plot_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(plot_data_reactive(), file, row.names = FALSE)
    }
  )
  
  ### Statistical Tests Tab ###
  
  # Statistical Tests UI
  output$stat_tests_ui <- renderUI({
    req(survey_data())
    tagList(
      fluidRow(
        column(6,
               selectInput("test_type", "Select Statistical Test",
                           choices = c("t-test", "ANOVA", "Chi-Square Test", "Correlation Test", "Mann-Whitney U Test", "Kruskal-Wallis Test"))
        ),
        column(6,
               conditionalPanel(
                 condition = "input.test_type == 'Correlation Test'",
                 selectInput("method", "Correlation Method", choices = c("pearson", "spearman", "kendall"))
               )
        )
      ),
      fluidRow(
        column(6,
               conditionalPanel(
                 condition = "input.test_type == 't-test' || input.test_type == 'ANOVA' || input.test_type == 'Mann-Whitney U Test' || input.test_type == 'Kruskal-Wallis Test'",
                 selectInput("response_var", "Response Variable", choices = names(survey_data()))
               ),
               conditionalPanel(
                 condition = "input.test_type == 'Chi-Square Test'",
                 selectInput("chi_var1", "Variable 1", choices = names(survey_data()))
               ),
               conditionalPanel(
                 condition = "input.test_type == 'Correlation Test'",
                 selectInput("cor_var1", "Variable 1", choices = names(survey_data()))
               )
        ),
        column(6,
               conditionalPanel(
                 condition = "input.test_type == 't-test' || input.test_type == 'ANOVA' || input.test_type == 'Mann-Whitney U Test' || input.test_type == 'Kruskal-Wallis Test'",
                 selectInput("group_var", "Grouping Variable", choices = names(survey_data()))
               ),
               conditionalPanel(
                 condition = "input.test_type == 'Chi-Square Test'",
                 selectInput("chi_var2", "Variable 2", choices = names(survey_data()))
               ),
               conditionalPanel(
                 condition = "input.test_type == 'Correlation Test'",
                 selectInput("cor_var2", "Variable 2", choices = names(survey_data()))
               )
        )
      )
    )
  })
  ### Statistical Tests Tab ###
  
  # Statistical Tests UI
  output$stat_tests_ui <- renderUI({
    req(survey_data())
    tagList(
      fluidRow(
        column(6,
               selectInput("test_type", "Select Statistical Test",
                           choices = c("t-test", "ANOVA", "Chi-Square Test", "Correlation Test", "Mann-Whitney U Test", "Kruskal-Wallis Test"))
        ),
        column(6,
               conditionalPanel(
                 condition = "input.test_type == 'Correlation Test'",
                 selectInput("method", "Correlation Method", choices = c("pearson", "spearman", "kendall"))
               )
        )
      ),
      fluidRow(
        column(6,
               conditionalPanel(
                 condition = "input.test_type == 't-test' || input.test_type == 'ANOVA' || input.test_type == 'Mann-Whitney U Test' || input.test_type == 'Kruskal-Wallis Test'",
                 selectInput("response_var", "Response Variable", choices = names(survey_data()))
               ),
               conditionalPanel(
                 condition = "input.test_type == 'Chi-Square Test'",
                 selectInput("chi_var1", "Variable 1", choices = names(survey_data()))
               ),
               conditionalPanel(
                 condition = "input.test_type == 'Correlation Test'",
                 selectInput("cor_var1", "Variable 1", choices = names(survey_data()))
               )
        ),
        column(6,
               conditionalPanel(
                 condition = "input.test_type == 't-test' || input.test_type == 'ANOVA' || input.test_type == 'Mann-Whitney U Test' || input.test_type == 'Kruskal-Wallis Test'",
                 selectInput("group_var", "Grouping Variable", choices = names(survey_data()))
               ),
               conditionalPanel(
                 condition = "input.test_type == 'Chi-Square Test'",
                 selectInput("chi_var2", "Variable 2", choices = names(survey_data()))
               ),
               conditionalPanel(
                 condition = "input.test_type == 'Correlation Test'",
                 selectInput("cor_var2", "Variable 2", choices = names(survey_data()))
               )
        )
      )
    )
  })
  
  output$stat_tests_output <- renderPrint({
    req(input$test_type)
    data <- survey_data()
    
    test_result <- NULL
    
    if (input$test_type == "t-test") {
      req(input$response_var, input$group_var)
      if (!is.numeric(data[[input$response_var]])) {
        showNotification("Response variable must be numeric for t-test.", type = "error")
        return(NULL)
      }
      if (!is.factor(data[[input$group_var]])) {
        data[[input$group_var]] <- as.factor(data[[input$group_var]])
      }
      formula <- as.formula(paste(input$response_var, "~", input$group_var))
      test_result <- t.test(formula, data = data)
      code$stats <<- paste0(
        "# t-test\n",
        "t.test(", input$response_var, " ~ ", input$group_var, ", data = data)")
    } else if (input$test_type == "ANOVA") {
      req(input$response_var, input$group_var)
      if (!is.numeric(data[[input$response_var]])) {
        showNotification("Response variable must be numeric for ANOVA.", type = "error")
        return(NULL)
      }
      if (!is.factor(data[[input$group_var]])) {
        data[[input$group_var]] <- as.factor(data[[input$group_var]])
      }
      formula <- as.formula(paste(input$response_var, "~", input$group_var))
      test_result <- summary(aov(formula, data = data))
      code$stats <<- paste0(
        "# ANOVA\n",
        "summary(aov(", input$response_var, " ~ ", input$group_var, ", data = data))")
    } else if (input$test_type == "Chi-Square Test") {
      req(input$chi_var1, input$chi_var2)
      if (!is.factor(data[[input$chi_var1]])) {
        data[[input$chi_var1]] <- as.factor(data[[input$chi_var1]])
      }
      if (!is.factor(data[[input$chi_var2]])) {
        data[[input$chi_var2]] <- as.factor(data[[input$chi_var2]])
      }
      test_result <- chisq.test(table(data[[input$chi_var1]], data[[input$chi_var2]]))
      code$stats <<- paste0(
        "# Chi-Square Test\n",
        "chisq.test(table(data$", input$chi_var1, ", data$", input$chi_var2, "))")
    } else if (input$test_type == "Correlation Test") {
      req(input$cor_var1, input$cor_var2, input$method)
      if (!is.numeric(data[[input$cor_var1]]) || !is.numeric(data[[input$cor_var2]])) {
        showNotification("Both variables must be numeric for Correlation Test.", type = "error")
        return(NULL)
      }
      test_result <- cor.test(data[[input$cor_var1]], data[[input$cor_var2]], method = input$method)
      code$stats <<- paste0(
        "# Correlation Test\n",
        "cor.test(data$", input$cor_var1, ", data$", input$cor_var2, ", method = '", input$method, "')")
    } else if (input$test_type == "Mann-Whitney U Test") {
      req(input$response_var, input$group_var)
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
      code$stats <<- paste0(
        "# Mann-Whitney U Test\n",
        "wilcox.test(", input$response_var, " ~ ", input$group_var, ", data = data)")
    } else if (input$test_type == "Kruskal-Wallis Test") {
      req(input$response_var, input$group_var)
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
      code$stats <<- paste0(
        "# Kruskal-Wallis Test\n",
        "kruskal.test(", input$response_var, " ~ ", input$group_var, ", data = data)")
    }
    
    test_result
  })
  
  
  
  ### Regression Models Tab ###
  
  output$regression_ui <- renderUI({
    req(survey_data())
    tagList(
      fluidRow(
        column(6,
               selectInput("reg_type", "Select Regression Type",
                           choices = c("Linear Regression", "Logistic Regression", "Ordinal Logistic Regression"))
        ),
        column(6,
               selectInput("dep_var", "Dependent Variable", choices = names(survey_data()))
        )
      ),
      fluidRow(
        column(12,
               pickerInput(
                 inputId = "indep_vars",
                 label = "Independent Variables",
                 choices = names(survey_data()),
                 options = list(`actions-box` = TRUE),
                 multiple = TRUE
               )
        )
      ),
      fluidRow(
        column(12,
               conditionalPanel(
                 condition = "input.reg_type == 'Logistic Regression'",
                 prettyCheckbox(
                   inputId = "recode_dep_var",
                   label = "Recode Dependent Variable to Binary",
                   status = "primary",
                   shape = "round",
                   animation = "pulse"
                 ),
                 conditionalPanel(
                   condition = "input.recode_dep_var == true",
                   selectInput("dep_var_value", "Value for 'Success' (others will be 'Failure')", choices = NULL)
                 )
               )
        )
      )
    )
  })
  
  # Update choices for 'dep_var_value' when 'dep_var' changes
  observeEvent(input$dep_var, {
    updateSelectInput(session, "dep_var_value", choices = unique(survey_data()[[input$dep_var]]))
  })
  

  
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
        "summary(model)")
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
        "summary(model)")
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
        "summary(model)")
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
