library(shinyjs)
library(shiny)
library(ggplot2)
library(corrplot)

options(shiny.maxRequestSize = 100 * 1024^2)

ui <- fluidPage(
  tags$head(
    tags$style(HTML('
            body {
        background-color: #f0f0f0; /* Light gray background */
        color: #333333; /* Dark gray text color */
      }
      .navbar {
        background-color: navy; /* Dark blue navbar */
      }
      .navbar-inverse .navbar-nav > li > a {
        color: #ffffff; /* White text color for navbar links */
      }
      .navbar-inverse .navbar-brand {
        color: navy; /* White text color for navbar brand */
      }
      .navbar-inverse .navbar-toggle {
        border-color: navy; /* White border for navbar toggle */
      }
      .navbar-inverse .navbar-toggle .icon-bar {
        background-color: #ffffff; /* White color for the toggle bars */
      }
      .navbar-inverse .navbar-text {
        color: #ffffff; /* White text color for navbar text */
      }
      .navbar-inverse .navbar-form .form-group {
        color: navy; /* White text color for form groups */
      }
      .nav-tabs > li > a {
        background-color: navy; /* Dark blue tabs */
        color: #ffffff; /* White text color for tabs */
      }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
        background-color: #ffffff; /* White background for active tab */
        color: navy; /* Dark blue text color for active tab */
      }
    '))
  ),
  titlePanel(
    h1("Data Analysis on Article Popularity", style = "color: navy;")
  ),

  sidebarLayout(
    sidebarPanel(
      style = "background-color: #ffffff; color: navy;",
      fileInput("file",
                label = "Upload your dataset (CSV)",
                accept = c("text/csv"),
                multiple = FALSE,
                width = "80%"),
      # Add a link to download the PDF file
      downloadLink("dataDescriptionLink", "Download Data Description"),
      br(),
      downloadLink("dataDownloadLink", "Download Data (CSV)")
    ),

    mainPanel(
      style = "background-color: #ffffff; color: #333333;",
      tabsetPanel(type = "tabs",
                  tabPanel("Data", dataTableOutput("outFile")),
                  tabPanel("Statistics", h1("Statistics"), verbatimTextOutput("statistics")),
                  tabPanel("Shares vs. Day Published", h1("Shares vs. Day Published"), plotOutput("histograms")),
                  tabPanel("Shares vs. Title Length", h1("Shares vs. Title Length"), plotOutput("sharesVsTitleLength")),
                  tabPanel("Shares vs Topic Heatmap", h1("Shares vs Topic Heatmap"), plotOutput("categoricalHeatmap")),
                  tabPanel("Shares vs. Number of Links", h1("Shares vs. Number of Links"), plotOutput("interestingPlot"))
      )
    )
  )
)

server <- function(input, output) {

  inFile <- reactive({
    tmp <- input$file
    if (is.null(tmp)) {
      return(NULL)
    } else {
      df <- read.csv(tmp$datapath, header = TRUE)
      return(df)
    }
  })

  output$outFile <- renderDataTable({
    data.frame(inFile())
  })

  observeEvent(input$outFile_rows_all, {
    shinyjs::animate("outFile", animation = "fadeIn")
  })

  output$statistics <- renderPrint({
    data <- inFile()
    if (!is.null(data)) {
      summary(data)
    }
  })

  output$histograms <- renderPlot({
    data <- inFile()
    if (!is.null(data)) {
      day_columns <- c("weekday_is_monday", "weekday_is_tuesday", "weekday_is_wednesday",
                       "weekday_is_thursday", "weekday_is_friday", "weekday_is_saturday", "weekday_is_sunday")

      day_names <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
      shares_by_day <- sapply(data[, day_columns], function(col) {
        sum(data$shares * col)
      })
      mean_shares_by_day <- sapply(data[, day_columns], function(col) {
        mean(data$shares * col)
      })

      day_shares <- data.frame(Day = day_names, Mean_Shares = mean_shares_by_day)

      ggplot(day_shares, aes(x = Day, y = Mean_Shares, fill = Day)) +
        geom_bar(stat = "identity") +
        labs(title = "Histogram of Mean Shares per Day of the Week", x = "Day of the Week", y = "Mean Shares") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })

  output$sharesVsTitleLength <- renderPlot({
    data <- inFile()
    if (!is.null(data)) {
      ggplot(data, aes(x = n_tokens_title, y = shares)) +
        geom_point() +
        labs(title = "Scatterplot of Shares vs. Title Length",
             x = "Number of Words in Title",
             y = "Number of Shares") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })

output$categoricalHeatmap <- renderPlot({
  data <- inFile()
  if (!is.null(data)) {
    subset_data <- data[, c(
      "data_channel_is_lifestyle",
      "data_channel_is_entertainment",
      "data_channel_is_bus",
      "data_channel_is_socmed",
      "data_channel_is_tech",
      "data_channel_is_world",
      "shares" # Target variable
    )]

    correlation_matrix <- cor(subset_data, method = "pearson")

    colnames(correlation_matrix) <- c(
      "Lifestyle",
      "Entertainment",
      "Business",
      "Social Media",
      "Tech",
      "World",
      "Shares" # Target variable
    )

    rownames(correlation_matrix) <- colnames(correlation_matrix)

    par(mar = c(1, 1, 1, 1))
    corrplot(
      correlation_matrix,
      method = "color",
      type = "upper",
      tl.col = "black",
      tl.cex = 0.7,
      tl.srt = 45,
      addrect = 6,
      is.corr = FALSE # Show legend
    )
  }
})

  output$interestingPlot <- renderPlot({
    data <- inFile()
    if (!is.null(data)) {
      ggplot(data, aes(x = num_hrefs, y = shares)) +
        geom_point() +
        labs(title = "Scatterplot of Shares vs. Number of Links",
             x = "Number of Links",
             y = "Number of Shares") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })

  output$dataDescriptionLink <- downloadHandler(
    filename = function() {
      "dataDescription.pdf"
    },
    content = function(file) {
      file.copy("/Users/keiichiro_watanabe/Desktop/CSC324/IndividualProject/data/dataDescription.pdf", file)
    }
  )

  output$dataDownloadLink <- downloadHandler(
    filename = function() {
    "data.csv"
    },
    content = function(file) {
      file.copy("/Users/keiichiro_watanabe/Desktop/CSC324/IndividualProject/data/OnlineNewsPopularity.csv", file)
    }
  )
}

shinyApp(ui, server)
