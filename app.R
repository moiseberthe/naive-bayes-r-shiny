#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(bs4Dash)
library(DT)
library(NaiveBayes)
library(readxl)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  class = "main-container",
  includeCSS("www/style.css"),
  tagList(
    bs4DashPage(
      title = "Naive Bayes",
      # navigation bar
      header = bs4DashNavbar(
        h1("Naive Bayes", class = "head-title"),
        skin = "dark",
        status = "primary"
      ),

      # left sidebar
      sidebar = bs4DashSidebar(
        skin = "dark",
        status = "primary",
        title = "Naive Bayes",
        elevation = 3,

        # left sidebar menu
        bs4SidebarMenu(
          id = "current_tab",
          bs4SidebarMenuItem(
            "Data",
            tabName = "welcome",
            icon = icon("table")
          ),
          bs4SidebarMenuItem(
            "Train a model",
            tabName = "train",
            icon = icon("palette")
          ),
          bs4SidebarMenuItem(
            "Prediction",
            tabName = "prediction",
            icon = icon("book-open")
          )
          ,
          bs4SidebarMenuItem(
            "Settings",
            tabName = "settings",
            icon = icon("gear")
          )
        )
      ),

      # main body
      body = bs4DashBody(
        bs4TabItems(
          bs4TabItem(
            tabName = "welcome",
            box(width = NULL, status = "gray",
              solidHeader = TRUE, title = "Load",
              fileInput("datafile", "Upload data", multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           ".xls",
                           ".xlsx"),
                placeholder = "Choose a file",
              ),

              conditionalPanel(
                condition = "output.fileformat == 'xlsx'",
                fluidRow(
                  column(3,
                    textInput("sheetname",
                              label = "Nom de la feuille",
                              placeholder = "Feuil 1"),
                  )
                ),
              ),
              conditionalPanel(
                condition = "output.fileformat == 'csvtxt'",
                fluidRow(
                  column(3,
                    selectInput("header", "Headers ?",
                      choices = c(Yes = TRUE, No = FALSE)
                    )
                  ),
                  column(3,
                    selectInput("sep", "Separator",
                      choices = c("Comma" = ",",
                        "Semicolon" = ";",
                        "Tabulation" = "\t",
                        "Pipe" = "|"
                      )
                    )
                  ),
                  column(3,
                    selectInput("quote", "String separator",
                      choices = c("Pas de séparateur" = " ",
                        "Single quote" = "'",
                        "Double quote" = "\""
                      )
                    )
                  ),
                  column(3,
                    selectInput("dec", "Decimal separator",
                      choices = c("Dot" = ".", "Comma" = ",")
                    )
                  ),
                ),
              )
            ),
            box(width = NULL, status = "primary",
              solidHeader = TRUE, title = "Preview", collapsed = FALSE,
              DT::dataTableOutput("input_file")
            ),
            box(width = NULL, status = "primary",
              solidHeader = TRUE, title = "Structure", collapsed = FALSE,
              verbatimTextOutput("structure"),
              plotlyOutput("varTypesBar")
            ),
            box(width = NULL, status = "primary",
              solidHeader = TRUE,
              title = "Statistics",
              collapsed = TRUE,
              verbatimTextOutput("summary")
            )
          ),
          bs4TabItem(
            tabName = "train",
            fluidRow(
              column(8,
                box(width = NULL, status = "primary",
                  solidHeader = TRUE,
                  title = "Train/Test split",
                  collapsed = FALSE,
                  sliderInput(
                    "train_size",
                    label = "Training size",
                    value = 0.7,
                    min = 0.5,
                    max = 1,
                    step = 0.05
                  ),
                  fluidRow(
                    column(8,
                      selectInput(
                        "stratify",
                        "Stratify ?",
                        choices = c("Non" = "no-stratify")
                      )
                    ),
                    column(4,
                       numericInput(
                         'seed', 'Set seed',
                         value = NULL
                         )
                     )
                  )
                ),
              ),
              column(4,
                box(
                  solidHeader = TRUE,
                  title = "Split statistics",
                  background = NULL,
                  width = NULL,
                  status = "secondary",
                  fluidRow(
                    column(
                      width = 6,
                      descriptionBlock(
                        number = textOutput("percent_train"),
                        numberColor = "secondary",
                        header = textOutput("nb_train"),
                        text = "TRAINING SET",
                        rightBorder = FALSE,
                        marginBottom = FALSE
                      )
                    ),
                    column(
                      width = 6,
                      descriptionBlock(
                        number = textOutput("percent_test"),
                        numberColor = "secondary",
                        header = textOutput("nb_test"),
                        text = "TESTING SET",
                        rightBorder = FALSE,
                        marginBottom = FALSE
                      )
                    )
                  )
                ),
              ),
            ),
            box(width = NULL, status = "gray",
              solidHeader = TRUE, title = "Configure model",
              fluidRow(
                column(6,
                  selectInput("target", "Target variable", choices = c())
                ),
                column(6,
                  selectInput("explanatory",
                    "Explainatory variables",
                    choices = c(),
                    multiple = TRUE
                  ),
                  verbatimTextOutput("explainOut")
                )
              )
            ),
          ),
          bs4TabItem(
            tabName = "prediction",
            fluidRow(
              column(8,
                box(width = NULL,
                  status = "secondary",
                  solidHeader = TRUE,
                  title = "Confusion matrix",
                  verbatimTextOutput("predictionOutput")
                )
              ),
              column(4,
                box(width = NULL, status = "secondary",
                  solidHeader = TRUE, title = "Metrics",
                  tableOutput("metricsOutput")
                )
              )
            ),
            fluidRow(
              column(12,
                box(width = NULL, status = "secondary",
                  solidHeader = TRUE, title = "Predictions",
                  fluidRow(
                    column(2,
                      checkboxInput(
                        inputId = "predict",
                        label = "Display predicted class",
                        value = TRUE,
                        width = NULL
                      )
                    ),
                    column(3,
                      checkboxInput(
                        inputId = "predict_proba",
                        label = "Display class probabilities",
                        value = FALSE,
                        width = NULL
                      )
                    )
                  ),
                  plotlyOutput("predictedProbasHist"),
                  DT::dataTableOutput("predictTable")
                )
              ),
            ),
            fluidRow(
              column(12,
                box(width = NULL, status = "secondary",
                  solidHeader = TRUE, title = "Export model",
                  "Click on the button to save the current model",
                  br(),
                  downloadButton("downloadModel", "Download model")
                )
              ),
            )
          ),
          bs4TabItem(
            tabName = "settings",
            fluidRow(
              column(12,
                selectInput("cbEnableSplit","Enable data split",choices=c("Enable"="enable","Input my own test data to predict"="disable"))
              ),
              column(12,
                selectInput("cbEnableSuggestion","Enable explainatory variables suggestions",choices=c("Enable (can slow down performance)"="enable","Disable"="disable"))
              )
            )
          )
        )
      ),

      # footer
      footer = bs4DashFooter(
        left = a(
          href = "https://github.com/Naghan1132/naive_bayes_R",
          target = "_blank",
          "Naive Bayes"
        ),
        right = "2023-2024"
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  datafile <- reactive({
    datafile <- input$datafile
    if (is.null(datafile)) {
      return()
    }
    file_extension <- tail(strsplit(datafile$datapath, "\\.")[[1]], 1)
    if (file_extension %in% c("csv", "txt", "tsv")) {
      output$fileformat <- renderText("csvtxt")
      outputOptions(output, "fileformat", suspendWhenHidden = FALSE)
      quote <- strtrim(input$quote, 1)
      data <- read.delim(
        datafile$datapath,
        header = as.logical(input$header),
        sep = input$sep,
        quote = quote,
        dec = input$dec
      )
    } else {
      output$fileformat <- renderText("xlsx")
      outputOptions(output, "fileformat", suspendWhenHidden = FALSE)
      sheetname <- if (input$sheetname != "") input$sheetname else 1
      data <- read_excel(datafile$datapath, sheet = sheetname)
    }
    if("index" %in% colnames(data)){
      stop("Cannot have column index in the dataframe")
    }
    data$index = 1:nrow(data)

    factor_vars <- colnames(Filter(Negate(is.numeric), data))
    
    updateSelectInput(inputId = "stratify",
      selected = NULL,
      choices = c("Non" = "no-stratify", factor_vars),
    )
    
    updateSelectInput(inputId = "target",
      choices = factor_vars
    )
    
    updateSelectInput(inputId = "explanatory",
      choices = colnames(data)
    )
    list(data = data)
  })

  datasets <- reactive({
    if (is.null(datafile()$data)) {
      return()
    }
    stratify <- if (input$stratify == "no-stratify") NULL else input$stratify
    seed <- if (is.na(input$seed)) NULL else input$seed
    
    train_test <- train_test_split(
      datafile()$data,
      train_size = input$train_size,
      stratify = stratify,
      seed = seed
    )
    return(list(
      Xtrain = train_test$train_set[input$explanatory],
      ytrain = train_test$train_set[[input$target]],
      Xtest = train_test$test_set[input$explanatory],
      ytest = train_test$test_set[[input$target]]
    ))
  })

  model <- reactive({
    if(length(intersect(input$explanatory, input$target)) > 0) {
      stop("Cannot have target variable as explainatory variable")
    }
    naive_bayes_cls <- naive_bayes$new()
    ypred <- NULL
    yproba <- NULL
    if (length(input$explanatory) >= 2) {
      naive_bayes_cls$fit(datasets()$Xtrain, as.factor(datasets()$ytrain))
      ypred <- naive_bayes_cls$predict(datasets()$Xtest)
      yproba <- naive_bayes_cls$predict_proba(datasets()$Xtest)
      yproba <- round(yproba, 2)
    }
    return(list(model = naive_bayes_cls, ypred = ypred, yproba = yproba))
  })

  output$input_file <- DT::renderDataTable({
    head(datafile()$data)
  }, options = list(
    searching = FALSE,
    paging = FALSE
  ))

  output$structure <- renderPrint({
    data = datafile()$data
    if(!is.null(data)){
      str(data)
      dfDataTypes = data.frame(
        Discrete = length(which(sapply(data, function(x){is.factor(x) || is.character(x)}))),
        Continuous= length(which(sapply(data, function(x){is.numeric(x)})))
      )
      output$varTypesBar <- renderPlotly(({
        plot = plot_ly(dfDataTypes, x = colnames(dfDataTypes), y = unname(dfDataTypes), type = 'bar', name = 'Values')
        plot = layout(
          plot,
          title= "Distribution of imported variables by type",
          xaxis = list(title="Number of variables"),
          yaxis = list(title="Type")
        )
      }))
    }
  })

  output$summary <- renderPrint({
    summary(datafile()$data)
  })

  output$percent_train <- renderText({
    paste((input$train_size * 100), "%", sep = "")
  })

  output$nb_train <- renderText({
    nrow(datasets()$Xtrain)
  })

  output$percent_test <- renderText({
    paste((1 - input$train_size) * 100, "%", sep = "")
  })

  output$nb_test <- renderText({
    nrow(datasets()$Xtest)
  })

  output$explainOut <- renderPrint({
    model()$model$summary()
  })

  output$predictionOutput <- renderPrint({
    metrics$confusion_matrix(datasets()$ytest, model()$ypred)
  })

  output$metricsOutput <- renderTable({
    c(
      "Accuracy" = metrics$accuracy_score(datasets()$ytest, model()$ypred),
      "Rappel" = mean(
        metrics$recall_score(datasets()$ytest, model()$ypred)
      ),
      "Precision" = mean(
        metrics$precision_score(datasets()$ytest, model()$ypred)
      )
    )
  }, colnames = FALSE, rownames = TRUE)

  output$predictTable <- DT::renderDataTable({
    data <- cbind(datasets()$Xtest, ytrue = datasets()$ytest)
    if (input$predict) {
      data <- cbind(data, ypred = model()$ypred)
    }
    if (input$predict_proba) {
      data <- cbind(data, yproba = model()$yproba)
    }
    return(datatable(data,selection = 'single'))
  },
  options = list(
    searching = FALSE,
    scrollCollapse = TRUE,
    scrollY = "400px",
    footer = TRUE
  ))

  output$downloadModel <- downloadHandler(
    filename = function() {
      "naive_bayes_classifier.rds"
    },
    content = function(file) {
      model <- model()$model
      saveRDS(model, file)
    }
  )
  
  # Display clicked row information
  observeEvent(input$predictTable_rows_selected, {
    selected_row_index <- input$predictTable_rows_selected
    data = datasets()$Xtest
    
    if (length(selected_row_index) > 0) {
      clicked_row_values <- data[selected_row_index,]
      
      output$predictedProbasHist <- renderPlotly({
        probas = round(model()$model$predict_proba(data)[selected_row_index,],2)
        plot = plot_ly(x = names(probas), y = unname(probas), type = 'bar')
        plot = layout(plot,
          title="Distribution of class probas",
          xaxis=list(title = "Class"),
          yaxis=list(title = "probas")
        )
        return(plot)
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
