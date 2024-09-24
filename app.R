library(shiny)
library(rvest)
library(stringr)
library(readr)
library(countrycode)
library(data.table)
library(bslib)
library(ggplot2)
library(shinycssloaders)

# Function to fetch and scrape country names for the filter
fetch_country_names <- function() {
    base_url <- 'https://data.humdata.org/dataset/?organization=undp-human-development-reports-office&q=Human+Development+Indicators&page='
    num_pages <- 8
    
    all_country_names <- c()
    
    for (page_num in 1:num_pages) {
        url <- paste0(base_url, page_num)
        page <- read_html(url)
        country_nodes <- page %>%
            html_text(trim = TRUE)
        country_names <- str_match_all(country_nodes, "(.+?) - Human Development Indicators")[[1]][,2]
        all_country_names <- c(all_country_names, country_names)
    }
    
    all_country_names <- str_trim(all_country_names)
    all_country_names <- unique(all_country_names)
    
    country_names_list <- as.list(all_country_names)
    
    entries_to_remove <- c("UNDP Human Development Reports Office", "This dataset is part of the data series [?]: UNDP Human Development Reports Office", "Bissau","Leste","s Democratic Republic","s Republic of Korea")
    
    country_names_list <- setdiff(country_names_list, entries_to_remove)
    
    return(country_names_list)
}

# Function to read in the data set from the website
cread <- function(countries) {
    if (countries == "Côte d'Ivoire") {
        countries <- "cote-d-ivoire"
    } else {
        countries <- gsub(" ", "-", countries)
        countries <- gsub("[^a-zA-Z0-9-]", "", countries)
    }
    
    countries <- tolower(countries)
    data_list <- list()
    
    coltypes <- list(
        '#country+code' = readr::col_factor(),
        '#country+name' = readr::col_factor(),
        "#indicator+id" = readr::col_factor(),
        "#indicator+name" = readr::col_factor(),
        "#index+id" = readr::col_factor(),
        "#index+name" = readr::col_factor(),
        "#indicator+value+num" = readr::col_number(),
        "#date+year" = readr::col_character()
    )
    
    for (country in countries) {
        tryCatch({
            url <- paste0('https://data.humdata.org/dataset/hdro-data-for-', country)
            page <- rvest::read_html(url)
            csv_links <- page %>%
                rvest::html_nodes("a") %>%
                rvest::html_attr("href")
            csv_url <- csv_links[stringr::str_detect(csv_links, 'hdro_indicators')]
            csv_url <- stats::na.omit(csv_url)
            csv_base_url <- "https://data.humdata.org"
            data <- readr::read_csv(paste0(csv_base_url, csv_url[1]), skip = 1, show_col_types = FALSE, col_types = coltypes)
            colnames(data) <- names(readr::read_csv(paste0(csv_base_url, csv_url[1]), n_max = 1, show_col_types = FALSE))
            colnames(data) <- gsub("_", " ", colnames(data))
            colnames(data) <- str_to_title(colnames(data))
            data_list[[country]] <- data
        })
    }
    
    combined_data <- do.call(rbind, data_list)
    row.names(combined_data) <- NULL
    return(combined_data)
}

# Define UI for application
ui <- page_fillable(
    tabsetPanel(
        tabPanel("Dataset", 
                 uiOutput("dynamic_title1"),
                 layout_columns(
                     card(uiOutput("summary")|>withSpinner(),
                          uiOutput('instructions')|>withSpinner()),
                     
                     card(selectInput("country", "Select a country:", choices = NULL),
                          actionButton("load_data", "Load Data"),
                          fileInput("file_input", "Choose CSV File", accept = ".csv"),
                          actionButton("load_file", "Load File")),
                     
                     card(numericInput("num_rows", "Number of rows to display:", 5, min = 1),
                          uiOutput("column_selector")|>withSpinner()),
                     
                     card(tableOutput("country_data")|>withSpinner()),
                     
                     col_widths = c(12, 6, 6, 12)
                 )
        ),
        tabPanel("Time Series", 
                 uiOutput("dynamic_title2"),
                 sidebarLayout(
                     sidebarPanel(
                         uiOutput("indicator_selector")|>withSpinner(),
                         selectInput("colour", "Colour:", choices = c("red", "blue", "green")),
                         uiOutput("slider_ui") |>withSpinner() 
                     ),
                     mainPanel(
                         plotOutput("timePlot")|>withSpinner()
                     )
                 )
        ),
        tabPanel("Average", 
                 uiOutput("dynamic_title3"),
                 sidebarLayout(
                     sidebarPanel(
                         selectizeInput("selected_indicators_bar", "Select indicators to display:", choices = NULL, selected = NULL, multiple = TRUE),
                         selectInput("colour_bar", "Colour:", choices = c("red", "blue", "green")),
                         textInput(inputId ="graph", "Graph description:", value = "bar plot")
                     ),
                     mainPanel(
                         plotOutput("barPlot") %>% withSpinner(),
                         textOutput("label")
                     )
                 )
        )
    )
)


server <- function(input, output, session) {
    # Store the dataset
    data <- reactiveVal(NULL)
    # Store the basic titles as base for reactive titels
    rv <- reactiveValues(title = "Human Development Indicator Data")
    
    # Update country names in the selectInput when the app loads
    observe({
        country_names <- fetch_country_names()
        updateSelectInput(session, "country", choices = country_names)
    })
    
    # Render the dynamic title for the Dataset tab - had to define it separately or else it wasn't showing in all tabs
    output$dynamic_title1 <- renderUI({
        titlePanel(rv$title)
    })
    
    # Dynamic title for the Time Series tab
    output$dynamic_title2 <- renderUI({
        titlePanel(rv$title)
    })
    
    # Dynamic title for the Average tab
    output$dynamic_title3 <- renderUI({
        titlePanel(rv$title)
    })
    
    # Preprocess year data by converting to numeric (to deal with values like 2022/23)
    preprocess_years <- function(df) {
        if ("Year" %in% names(df)) {
            df$Year <- as.numeric(sub("(/.*)?$", "", df$Year))
        } else {
            stop("The data does not contain a 'Year' column.")
        }
        df
    }
    
    # Web scrape data based on selected country
    observeEvent(input$load_data, {
        req(input$country)  # Ensure a country is selected
        rv$title <- paste("Human Development Indicator Data for", input$country)
        loaded_data <- cread(input$country)
        loaded_data <- as.data.frame(loaded_data)
        processed_data <- preprocess_years(loaded_data)
        data(processed_data)
        
        # Update the year slider based on loaded data
        if (!is.null(data()) && "Year" %in% names(data())) {
            output$slider_ui <- renderUI({
                sliderInput("range", 
                            label = "Choose a start and end year:",
                            min = min(data()$Year, na.rm = TRUE),
                            max = max(data()$Year, na.rm = TRUE),
                            value = c(min(data()$Year, na.rm = TRUE), max(data()$Year, na.rm = TRUE)),
                            step = 1,
                            sep = "")
            })
        }
        # Update the bar plot indicator selection
        if (!is.null(data())) {
            updateSelectizeInput(session, "selected_indicators_bar", choices = unique(data()[, "Indicator Name"]), selected = unique(data()[, "Indicator Name"])[2:5])
        }
    })
    
    # Load data from a CSV file
    observeEvent(input$load_file, {
        req(input$file_input)  # Ensure a file is selected
        filename <- input$file_input$name
        if (!grepl("^hdro_indicators_", filename)) {
            showNotification("Invalid file. The file must be downloaded from the HDX website.", type = "error")
            return()
        }
        column_types <- list(
            factor = c("V1", "V2", "V3", "V4", "V5", "V6"),
            numeric = c("V7"),
            character = c("V8")
        )
        column_names <- names(fread(input$file_input$datapath, nrows = 1))
        column_names <- gsub("_", " ", column_names)
        column_names <- str_to_title(column_names)
        file_data <- fread(input$file_input$datapath, skip = 2, colClasses = column_types, header = FALSE, col.names = column_names)
        file_data <- as.data.frame(file_data)
        file_country <- as.character(unique(file_data[[2]])[1])
        rv.title <- paste("Human Development Indicator Data for", file_country)
        processed_data <- preprocess_years(file_data)
        data(processed_data)
        
        # Update the year slider based on uploaded data
        if (!is.null(data()) && "Year" %in% names(data())) {
            output$slider_ui <- renderUI({
                sliderInput("range", 
                            label = "Choose a start and end year:",
                            min = min(data()$Year, na.rm = TRUE),
                            max = max(data()$Year, na.rm = TRUE),
                            value = c(min(data()$Year, na.rm = TRUE), max(data()$Year, na.rm = TRUE)),
                            step = 1,
                            sep = "")
            })
        }
        # Update the bar plot indicator selection
        if (!is.null(data())) {
            updateSelectizeInput(session, "selected_indicators_bar", choices = unique(data()[, "Indicator Name"]), selected = unique(data()[, "Indicator Name"])[2:5])
        }
    })
    
    # Column selector checkboxes dynamically based on loaded data
    output$column_selector <- renderUI({
        req(data())
        checkboxGroupInput("selected_columns", "Select columns to display:", choices = names(data()), selected = names(data()))
    })
    
    # Data table showing the selected number of rows and columns
    output$country_data <- renderTable({
        req(data())
        num_rows <- input$num_rows
        selected_columns <- input$selected_columns
        df <- data()[, selected_columns, drop = FALSE]
        df$Year <- format(df$Year, nsmall = 0)  # Format the Year column to remove decimals
        head(df, num_rows)
    })
    
    # Indicator selector dropdown dynamically based on loaded data
    output$indicator_selector <- renderUI({
        req(data())
        selectizeInput("selected_indicators", "Select indicators to display:", choices = unique(data()[, "Indicator Name"]), selected = NULL, multiple = FALSE) 
    })
    
    # Time series plot based on user selections
    output$timePlot <- renderPlot({
        req(data())
        filtered_data <- data()[data()[, "Indicator Name"] == input$selected_indicators, ]
        filtered_data <- filtered_data[filtered_data[, "Year"] >= input$range[1] & filtered_data[, "Year"] <= input$range[2], ]
        
        ggplot(filtered_data, aes(x = Year, y = Value, group = 1)) + 
            geom_point(aes(color = input$colour)) +
            geom_line(aes(color = input$colour)) +
            scale_color_manual(values = c("red" = "red", "blue" = "blue", "green" = "green")) +
            scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = "", decimal.mark = ".")) +
            theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none") +
            labs(
                title = input$selected_indicators,
                x = "Year",
                y = input$selected_indicators
            )
    })
    
    #Bar plot showing average values for selected indicators
    output$barPlot <- renderPlot({
        req(data())
        selected_indicator <- input$selected_indicators_bar
        filtered_data <- data()[data()[, "Indicator Name"] %in% selected_indicator, ]
        avg_data <- aggregate(Value ~ `Indicator Name`, data = filtered_data, FUN = mean, na.rm = TRUE)
        
        ggplot(avg_data, aes(x = reorder(`Indicator Name`, Value), y = Value, fill = input$colour_bar)) +
            geom_bar(stat = "identity") +
            scale_fill_manual(values = c("red" = "red", "blue" = "blue", "green" = "green")) +
            theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none") +
            labs(
                title = "Average Value for Selected Indicators",
                x = "Indicator",
                y = "Average Value"
            )
    })
    
    # Descriptive label for the bar plot
    output$label <- renderText({
        selected_indicators <- input$selected_indicators_bar
        if (is.null(selected_indicators)) {
            "No indicators selected."
        } else {
            paste("This", input$graph, "shows the average values for the following indicators:", paste(selected_indicators, collapse = ", "))
        }
    })
    
    #Data set summary text
    output$summary <- renderUI({
        HTML(paste("This app is based on the Human Development Indicator Data. The Human Development Index (HDI) is a summary measure of average achievement in key dimensions of human development: a long and healthy life, being knowledgeable and having a decent standard of living. The HDI is the geometric mean of normalized indices for each of the three dimensions.", 
                   "Data and description is taken from:", 
                   '<a href="https://data.humdata.org/dataset/?organization=undp-human-development-reports-office&q=Human+Development+Indicators" target="_blank">Human Development Indicators</a>',
                   sep = "<br/>"))
    })
    
    # Instructions text
    output$instructions <- renderUI({
        HTML(paste(
            "The data can be either loaded by choosing a country or by uploading your own CSV file.",
            "<b>Please click on the button 'Load Data' or 'Load File' to see the data.</b>",
            sep = "<br/>"
        ))
    })
}

shinyApp(ui = ui, server = server)



