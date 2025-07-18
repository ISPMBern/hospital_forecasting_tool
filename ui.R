# This is the user-interface definition of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here: https://shiny.posit.co/

# Author: Laura Di Domenico, ISPM, University of Bern
# Date: May 2025

library(DT) # to render tables

fluidPage(
  
  title = "Forecasting tool for hospital demand during heat periods",
  
  # Application title
  titlePanel(
    div("Forecasting tool for hospital demand during heat periods",
        style = "color: #e63946; font-size: 38px;"),
  ),
  
  # project logos
  img(src='logo_combined.png', height="25%", width="25%", align = "right"),
  
  # tool description
  p("This tool has been developed by researchers at ISPM, University of Bern, 
    in the context of the NCCS project about impacts of climate change on health in Switzerland.", 
    style = "font-size: 125%"),
  
  p("This forecasting tool allows to:",
    tags$br(),
  "(i) quantify the association between visits to emergency department and daily temperature, based on historical data provided;",
    tags$br(),
  "(ii) generate short-term forecasts of the expected number of patient visits on new data, given temperature and number of visits 
  observed in the past week and temperature forecasts for the next days.", 
  style = "font-size: 125%"),
  
  p("We showcase an application of the tool to data from the emergency department at Bern University Hospital, 
    and mean temperature data for the city of Bern.",  style = "font-size: 125%"),
    
  p("You can upload your own data to test the tool for your case study. Check the user guide for instructions on how to use this tool.",
    style = "font-size: 125%"),
  
  actionButton("open_instructions", "USER GUIDE", onclick = "window.open('USER-GUIDE.html', '_blank')",
               style = "color: white; background-color: #e63946; border-color: #e63946; font-size: 18px; padding: 10px 20px;"),
  
  p(""),
  p("The tool has been implemented as an R shiny app by Laura Di Domenico.", 
    tags$br(),
    "If you use this tool, please refer to the following publication:",
    tags$br(),
    "Di Domenico L, Wohlfender MS, Hautz WE, Vicedo-Cabrera AM, Althaus CL,",
    tags$br(),
    em("A forecasting tool of hospital demand during heat periods: a case study in Bern, Switzerland"),
    tags$br(),
    "The R code is available on GitHub", 
    a("in this repository", href = "https://github.com/ISPMBern/hospital_forecasting_tool/tree/main"),
    style = "font-size: 100%"),
  
  # horizontal bar as divider
  tags$hr(),

  # SECTION 1: DATA ###############################################################################
  headerPanel("Section 1: DATA"),
  
  sidebarLayout(
    sidebarPanel(
      # data source
      selectInput("data", "Choose data source", choices = c("Bern case study", "Upload your own data")),
      # upload data from user
      conditionalPanel(
        condition = "input.data == 'Upload your own data'",
        fileInput("file", "Upload CSV File", accept = c(".csv")),
        textOutput("file_path"),
        selectInput("country", "Select a country:", choices = c("Austria", "Belgium", "France", "Germany", 
                                                                "Italy", "Netherlands", "Spain", 
                                                                "Switzerland(canton)", "Switzerland(national)",
                                                                "UnitedKingdom"),
                    selected = "Switzerland(canton)"),
        conditionalPanel(
          condition = "input.country == 'Switzerland(canton)'",
          selectInput("canton", "Select a Swiss canton:",
                      choices = c("AG", "AI", "AR", "BE", "BL", "BS", "FR", "GE",
                                  "GL", "GR", "JU", "LU", "NE", "NW", "OW", "SG",
                                  "SH", "SO", "SZ", "TG", "TI", "UR", "VD", "VS",
                                  "ZG", "ZH"),
                      selected = "BE")
        ),
        p("The information on the country and canton is used to provide a list of public holidays to the model. 
          If you do not want to specify a canton, switch to Switzerland(national) in the dropdown menu.
          Only a limited list of countries outside Switzerland are currently supported in this version of the tool.")
      ),
    ),
    mainPanel(
      div(
        style = "background-color: #f9f9f9; 
                 border-left: 6px solid #e63946; 
                 padding: 10px; 
                 margin-bottom: 15px; font-size: 110%;",
      p("In this section, you can choose the data source and the time period of interest for the analysis.", style = "font-size: 120%"),
      p("You can also define the training and test set for the model.", style = "font-size: 120%"),
      ),
      conditionalPanel(
        condition = "input.data == 'Upload your own data'",
        fluidRow(
          column(6,         
                 p("Upload your own data in CSV format, following the template on the right."),
                 p("Include at least three columns, named 'date' (YYYY-MM-DD), 
                      'hosp_counts' (number of daily visits to the hospital), 
                       and a column with temperature of interest (e.g., 'mean_temp' with the average daily temperature)."),),
          column(6, DTOutput("viewDataRawTemplate"),)
        ),
      )
    )
  ),
  sidebarLayout(
    # Sidebar panel with user inputs
    sidebarPanel(
      # choose column to be used for temperature indicator
      uiOutput("temp_indicator_ui"),
      # define time period of interest
      selectInput("months", "Time period of interest", choices = c("Summer months (June - August)", "Other")),
      uiOutput("show_choice_period_of_interest"),
      
      # define training set with date inputs
      tags$label("Training Set", style = "color: darkcyan;"),
      fluidRow(
        column(6, dateInput("train_start_date", "Start Date", value = "2014-01-08",
                            min = "2014-01-08", max = "2022-12-31",, format = "yyyy-mm-dd"),  ),
        column(6, dateInput("train_end_date", "End Date", value = "2021-12-31",
                            min = "2014-01-08", max = "2022-12-31", format = "yyyy-mm-dd"))
      ),
    
      # define test set with date inputs
      tags$label("Test Set", style = "color: chocolate;"),
      fluidRow(
        column(6, dateInput("test_start_date", "Start Date", value = "2022-01-01",
                            min ="2014-01-08", max = "2022-12-31", format = "yyyy-mm-dd"),),
        column(6, dateInput("test_end_date", "End Date", value = "2022-12-31",
                            min = "2014-01-08", max = "2022-12-31", format = "yyyy-mm-dd"))
      ),
    
      # check if train and test set overlap
      uiOutput("check_date_train_test_set")
    ),
    # Main panel with plot of data
    mainPanel(
      # plot with daily time-series of hospital visits and temperature
      plotOutput("renderHistoricalDataPlot", height=600),
      # donwload plot
      downloadButton("downloadHistoricalDataPlot", "Download Plot"),
      p(),
      # table to inspect the dataset
      DTOutput("viewDataRaw"),
    )
  ),
  
  # horizontal bar as divider
  tags$hr(),
  
  # SECTION 2: MODEL ###############################################################################
  
  headerPanel("Section 2: MODEL"),

  sidebarLayout(
    # Sidebar panel with user inputs
    sidebarPanel(
      # User mode selection
      selectInput("mode", "Select user mode", choices = c("", "Beginner", "Expert")),
    ),
    # Main panel 
    mainPanel(
      div(
        style = "background-color: #f9f9f9; 
                 border-left: 6px solid #e63946; 
                 padding: 10px; 
                 margin-bottom: 15px; font-size: 110%;",
        p("In this section, we estimate the association between temperature and hospital visits.", 
        style = "font-size: 120%"),
        p("The 'Beginner' mode allows you to run the regression model using a set of predefined predictors. 
        The 'Expert' mode allows you to change predictors and obtain additional details on the model fit.", 
          style = "font-size: 100%"),
      ),
    )
  ),
  
  ## Empty UI Panel #########################################################################
  # if no mode is selected, show empty panel
  conditionalPanel(
    condition = "input.mode == ''",
  ),
  
  ## Beginner / Expert UI Panel #########################################################################
  # if beginner or expert mode is selected, show the following panel
  conditionalPanel(
    condition = "input.mode == 'Beginner' || input.mode == 'Expert'",

    sidebarLayout(
      # Sidebar panel with user input for model predictors
        sidebarPanel(
          
          # if user selects 'Expert' mode, show the following inputs
          conditionalPanel (
            condition = "input.mode == 'Expert'",
            
            strong("Define regression model", style = "color:black; font-size: 150%"),
            p(),
            
            strong("Predictors not related to temperature", style = "color:darkblue"),
            p(),
            selectInput("past.obs", "7 days of past hospital data", choices = c("Yes", "No")),
            selectInput("day.of.week.hol", "Day of week & holiday", choices = c("Yes", "No")),
            selectInput("pandemic", "Change of level for pandemic period (2020-2022)", choices = c("Yes", "No")),
            
            strong("Parameters related to temperature", style = "color:darkred"),
            p(),
            sliderInput("maxlag", "Maximum lag for temperature", min = 1, max = 7, value = 3, step = 1),
            selectInput("tknots", "Internal knots", choices = c("3 (10%, 50%, 90%)", "3 (5%, 50%, 95%)", "3 (25%, 50%, 75%)", "2 (equally spaced)", "manual")),
            uiOutput("show_choice_knots"),
            textOutput("degree_of_freedom"),
        ),

        # if user selects 'Beginner' mode, show the following panel
        conditionalPanel (
          condition = "input.mode == 'Beginner'",
          
          p("In the 'Beginner mode', the tool uses a regression model with the following predictors: seven days of past hospital data, day of the week and holiday, change of level during the pandemic period, and temperature. 
          The association with temperature is non-linear and modelled with natural cubic splines with 4 degrees of freedom, including up to 3 days of lag."),
          p(""),
          p("The red line represents the estimated change in the number of hospital visits for a given temperature with respect to a temperature of minimum risk. The gray shaded area represents the confidence interval."),
          
        ),        
      ),

      # Main panel with plot of the estimated exposure - response function
      mainPanel(
        textOutput("checkModelDof"), # check model degrees of freedom
        
        # show plot with the exposure response function
        plotOutput("renderExposureResponsePlot", height=425),
        # download plot
        downloadButton("downloadExposureResponsePlot", "Download Plot"),
        p(),
        
        # if user selects 'Expert' mode, show the following additional information
        conditionalPanel(
          condition = "input.mode == 'Expert'",
          
          p("The association with temperature is non-linear and modelled with natural cubic splines. The dotted vertical lines represent the position of the knots for the splines. The red line represents the estimated change in the number of hospital visits for a given temperature with respect to a temperature of minimum risk. 
          The gray shaded area represents the confidence interval."),
          # The confidence interval is computed by sampling (1000 times) from a multinormal distribution 
          # with vector of fitted coefficients as mean and covariance matrix as variance.
          # This procedure similar to function _crossbasis_ from library DLNM.
          
          # horizontal bar as divider
          tags$hr(),
          
          # additional tabs with model results, forecast errors and evaluation metrics
          p("In the following tabs, you can find additional details on the model fit, predictions and evaluation metrics."),
          p(""),
          tabsetPanel(type="tab",
                      tabPanel("Temperature data (aggregated)",
                               p(""),
                               p("In this panel, we show histograms of the observed temperature data, for the training set and the test set. 
                                 Temperature ranges were data in the training set are scarce will lead to higher uncertainty in the estimate of the exposure response function."),
                               plotOutput("histogramData", height=250),
                               ),
                      tabPanel("Temperature data (per year)",
                               fluidRow(
                                 column(
                                   width = 8,
                                   p(""),
                                   p("In this panel, we show the time series of daily observed temperature per year, for the time period of interest. 
                                    Temperature values above a given threshold (default 25 °C) are shown in red. You can change the default threshold value in the input field."),
                                 ),
                                 column(
                                   width = 4,
                                   numericInput("thresholdHotDay", "Temperature threshold", 25),
                                 )
                               ),
                               uiOutput("dynamicPlotHotDays")
                      ),
                      tabPanel("Model inputs", 
                               p(""),
                               p("The table shows an overview of the user inputs for the data and the regression model. You can save this settings along with the model results."),
                               tableOutput("modelInputs")
                      ),
                      tabPanel("Model summary", 
                               p(""),
                               p("This panel displays the output of the model", em("summary( )"), "function in R."),
                               verbatimTextOutput("modelSummary"),
                      ),
                      tabPanel("Other regression coefficients", 
                               p(""), 
                               p("In the plot above, we used the regression coefficients estimated for the temperature indicator to build the exposure-response function."),
                               p("In the panels below, we show the regression coefficients for the other predictors in the model, in particular the autoregressive terms (left) and the effect of day of the week and holiday (right)."),
                               plotOutput("plotOtherRegresCoef", height=400)
                      ),
                      tabPanel("Evaluation metrics", 
                               p(""),
                               p("In this panel, we show evaluation metrics for the model fit on the training set and 1-step ahead predictions on the test set. 
                                 The evaluation metrics are computed using the root mean square error (RMSE), mean absolute error (MAE) and Akaike Information Criterion (AIC). 
                                 We show results for the reference model (with temperature as predictor), for a model without temperature, and for two naive models of last observatoon carried forrward (LOCF)."),
                               shiny::fluidRow(
                                 column(
                                   width = 6,
                                   strong("Train set", style="color:darkcyan; font-size: 120%"),
                                   tableOutput("modelMetricsTrain") 
                                 ),
                                 column(
                                   width = 6,
                                   strong("Test set", style="color:chocolate; font-size: 120%"),
                                   tableOutput("modelMetricsTest") 
                                 )
                               ),
                      ),
                      tabPanel("Fit on train set", 
                               p(""),
                               p("This panel shows the fit of the model on the training set. 
                                 Green represents the fitted values, while black indicate the data. Dots indicate daily values; lines indicate a weekly rolling mean as a guide for the eye. "),
                               uiOutput("dynamicFitPlot")
                      ),
                      tabPanel("Forecasts on test set",
                               p(""),
                               p("In left panel, we show examples of model forecasts at three time points in the test set. 
                               Dots represent the raw data. The black line indicate 1-step ahead prediction and it is shown as a reference. 
                               The blue line indicate forecats for the next 7 days (continuous line) or 21 days (dashed line). 
                               Shaded area indicate prediction intervals (50% and 90% PI). Prediction interval for step h assumes a point prediction on step h-1, without propagating uncertainty from one day to the next. 
                               A method for propagating uncertainty in the prediction interval is implemented in Section 3. 
                               "),
                               p("In the right panel, we show forecast errors (computed as root mean squared error (RMSE)) 
                                 for different forecast horizons."),
                               fluidRow(
                                 column(
                                   width = 8,
                                   plotOutput("forecastWindow", height=500),
                                 ),
                                 column(
                                   width = 4,
                                   div(
                                     style = "padding-top: 60px;",  # adjust the number to control spacing
                                     plotOutput("forecastErrorBoxPlotSimple", height = 430)
                                   )
                                 ),
                               ),
                      ),
                      tabPanel("Forecast errors",
                               p(""),
                               p("In the plot on the left, we show forecast errors (computed as root mean squared error (RMSE)) 
                                 for different forecast horizons (first, second or third week in the future), 
                                 for the reference model and for a model neglecting temperature as predictor. We consider here all possible forecasting windows in the test set, each one with a given strating date."),
                               p("On the right, we consider a subset of forecasting windows, corresponding to the ones 
                                 where the average temperature in the 7-day period of the forecasting window is above a given threshold value."),
                               fluidRow(
                                 column(
                                   width = 6,
                                   div(
                                     style = "padding-top: 80px;",  # adjust the number to control spacing
                                     plotOutput("forecastErrorBoxPlot", height=300),
                                   ),
                                 ),
                                 column(
                                   width = 6,
                                   numericInput("threshold", "Temperature threshold", NA),
                                   plotOutput("forecastErrorBoxPlotSelectedWindows", height=300),
                                 )
                               ),
                      ),
                      tabPanel("Forecast errors (cross validation)",
                               p(""),
                               p("In this panel we show forecast errors on the test set, after cross validation across years. 
                                 For each year in the data, we train the model on all years except one year, 
                                 and test the model on the remaining data. Then, we compute forecast errors on the test set, 
                                 analogously to what is done in the 'Forecast errors' tab."),
                               fluidRow(
                                 column(
                                   width = 6,
                                   div(
                                     style = "padding-top: 75px;",  # adjust the number to control spacing
                                     plotOutput("forecastErrorBoxPlotCrossValidation", height=300),
                                   ),
                                 ),
                                 column(
                                   width = 6,
                                   numericInput("threshold_cv", "Temperature threshold", NA),
                                   plotOutput("forecastErrorBoxPlotCrossValidationSelectedWindows", height=300),
                                 )
                               ),
                               
                      )

          ),
        ),
      ),
    ),
  ),

  # horizontal bar as divider
  tags$hr(),

  # SECTION 3: FORECASTS on test set ###############################################################################
  headerPanel("Section 3: FORECASTS"),
  
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel with user inputs
    sidebarPanel(
      selectInput("mode_temperature", "Select dataset for forecasts", choices = c("", "Test set", "New data (upload)")),
    ),
    # Main panel 
    mainPanel(
      div(
        style = "background-color: #f9f9f9; 
                 border-left: 6px solid #e63946; 
                 padding: 10px; 
                 margin-bottom: 15px; font-size: 110%;",
        p("In this section, we generate forecasts of future hospital visits based on the model estimated above.", 
          style = "font-size: 120%"),
        p("The default mode 'Test set' will generate retrospective forecasts on the test set. 
            To generate forecasts in real time, choose the 'New data (upload)' option 
          and upload a csv file with temperature and hospital data in the latest week and expected temperature for the upcoming days.", 
            style = "font-size: 100%")
        )
    )
  ),
  
  conditionalPanel(
    condition = "input.mode_temperature == ''",
  ),
  
  ## Default Panel #########################################################################
  conditionalPanel(
    condition = "input.mode_temperature == 'Test set'",
    
    sidebarLayout(
      # Sidebar panel with input for forecasts
      sidebarPanel(
        sliderInput("window_length", "Length of forecast window (days)", min = 7, max = 21, value = 14, step = 7),
        dateInput("start_date_forecast", "Starting date of forecast window", value = NULL,
                  min = NULL, max = NULL, format = "yyyy-mm-dd",
                  startview = "month", weekstart = 0, language = "en",
                  width = NULL, autoclose = TRUE, datesdisabled = NULL, daysofweekdisabled = NULL),
        p("The default starting date for the forecast window is two weeks after the starting date of the test set. 
          To change this, click on the date field."),
      ),
      mainPanel(
        plotOutput("renderRetrospectiveForecastPlot", height=500),
        downloadButton("downloadRetrospectiveForecastPlot", "Download Plot"),
      ),
    ),
  ),
  
  ## Panel upload data from user #########################################################################
  conditionalPanel(
    condition = "input.mode_temperature == 'New data (upload)'",
    
    sidebarLayout(
      # Sidebar panel with input for model predictors
      sidebarPanel(
        fileInput("file_temperature", "Upload CSV File", accept = c(".csv")),
        textOutput("file_temperature_path"),
      ),
      mainPanel(
        # if file is not uploaded yet, show the following panel
        uiOutput("file_upload_help_panel"),
        plotOutput("renderNewForecastPlot", height=500),
        downloadButton("downloadNewForecastPlot", "Download Plot"),
        p(" "),
        DTOutput("viewDataRawTemperature"),
      ),
    )
  ),
)