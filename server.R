# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here: https://shiny.posit.co/

# Author: Laura Di Domenico, ISPM, University of Bern
# Date: May 2025

source("functions.R")

server <- function(input, output, session) {
  
  col_train <- "darkcyan"
  col_test <- "chocolate"
  
  # SECTION 1: DATA ############################################################################
  
  ## if user uploads their own dataset
  ## we store it in the data folder
  observeEvent(input$file, {
    req(input$file)  # Ensure a file is uploaded
    
    # Define file path
    file_path <- file.path("data", input$file$name)
    
    # Save the uploaded file to the "data" folder
    file.copy(input$file$datapath, file_path, overwrite = TRUE)
    
    # Display the saved file path
    output$file_path <- renderText({
      paste("File saved to:", file_path)
    })
  })
  
  ## if user changes the dataset mode, we reinitialize the model and forecast section to empty
  observeEvent(input$data, {
    updateSelectInput(session, "mode", selected = "")
    updateSelectInput(session, "mode_temperature", selected = "")
  })
  
  ## read raw data
  data_raw <- reactive({
    ## see data_read() function in the script functions.R
    data_raw <- data_read(input$data, input$file$datapath)
    
    # Check for error
    validate(
      need(!is.list(data_raw) || is.null(data_raw$error), data_raw$error)
    )
    
    # check that data_raw includes at least three columns named "date", "hosp_counts", and "mean_temp"
    if (!all(c("date", "hosp_counts") %in% colnames(data_raw))) {
      stop("The dataset must include columns named 'date', 'hosp_counts'.")
    }
    
    # check that data_raw contains continuous dates, and there are no missing dates
    if (check_continuous_dates(data_raw)==FALSE) {
      stop("The dataset must include continuous dates, without missing dates.")
    }
    
    data_raw
  })
  
  # update the list of possible temperature indicator in the UI
  # the list will include all available column names except date and hosp_count
  output$temp_indicator_ui <- renderUI({
    req(data_raw())  # make sure it's available
    selectInput(
      inputId = "temp_indicator",
      label = "Temperature indicator",
      choices = setdiff(names(data_raw()), c("date", "hosp_counts")),
      selected = "mean_temp"
    )
  })
  
  # display table in the user interface to inspect the raw data 
  output$viewDataRaw <- DT::renderDataTable({
    df <- data_raw()
    datatable(df, options = list(scrollX = TRUE, pageLength = 3, searching = FALSE))
  })
  
  # display table of template data
  output$viewDataRawTemplate <- DT::renderDataTable({
    # read dataframe contained in "data" folder, called public_austin.csv
    df <- read.csv("data/hospital_dataset_template.csv")
    # filter on dates posterior to 2019-06-01
    df <- df[df$date >= "2019-06-01", ]
    # re-index the dataset starting from 1
    rownames(df) <- seq(1, nrow(df))
    # select only columns date, hosp_counts, mean_temp, and select only first three rows
    df <- df[1:3, c("date", "hosp_counts", "mean_temp")]
    # round mean_temp to 2 digits
    df$mean_temp <- round(df$mean_temp, 2)
    # display table
    datatable(df, class = 'compact',
              options = list(scrollX = FALSE, lengthChange = FALSE, pageLength = 3, searching = FALSE,  
                             info = FALSE, paging = FALSE))
  })
  
  # update default dates of training and test set
  observe({
    
    req(input$file) # trigger when user uploads a file 
    
    # extract the first date in data_raw(), and add seven days 
    first_date <- as.character(data_raw()[8, 'date'])
    
    # extract last date in data_raw()
    last_date <- as.character(data_raw()[length(data_raw()$date), 'date'])
    
    updateDateInput(session, inputId = "train_start_date",
                    value = first_date,  min = first_date, max = last_date)
    
    updateDateInput(session, inputId = "test_end_date",
                    value = last_date,  min = first_date, max = last_date)
    
    # extract number of years in the dataset, and select the last yar available
    years <- unique(format(as.Date(data_raw()$date), "%Y"))
    last_year <- tail(years, 1)
    # extract the first date of the last year
    first_date_last_year <- paste0(last_year, "-01-01")
    # extract the last date of the previous year
    last_date_prev_year <- paste0(as.numeric(last_year)-1, "-12-31")
    # update the date input for the start of test set and end of training set
    updateDateInput(session, inputId = "test_start_date",
                    value = first_date_last_year,  min = first_date, max = last_date)
    updateDateInput(session, inputId = "train_end_date",
                    value = last_date_prev_year,  min = first_date, max = last_date)
  })
  
  
  output$show_choice_period_of_interest <- renderUI({
    if (input$months != "Other") {
      return(NULL)
    } else {
      tagList(
        fluidRow(
          column(6, selectInput("start_date_period_of_interest","Start Date", selected = "06-01",
                                        choices = format(seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day"), "%m-%d"))),
          column(6, selectInput("end_date_period_of_interest","End Date", selected = "08-31",
                                choices = format(seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day"), "%m-%d"))),
        ),
        p("The default period ranges from June 1st to August 31st.")
      )
    }
  })
  
  # extract input of starting date for period of interest
  date_period_of_interest <- reactive({
    date_period_of_interest <- c(input$start_date_period_of_interest, input$end_date_period_of_interest)
    
    if ((input$start_date_period_of_interest < input$end_date_period_of_interest)==FALSE) {
      stop("The start date of the time period of interest must be earlier than the end date.")
    }
    
    date_period_of_interest 
  })
  
  # extract input of starting date for train and test set 
  date_train_set <- reactive({
    date_train_set <- c(input$train_start_date, input$train_end_date)
    
    if ((as.Date(input$train_start_date, format = "%Y-%m-%d")<as.Date(input$train_end_date, format = "%Y-%m-%d"))==FALSE) {
      stop("The start date of the train set must be earlier than the end date.")
    }
    # check that start of training set is at least 7 days posterior to the first date in the dataset
    if ((as.Date(input$train_start_date, format = "%Y-%m-%d")>=min(as.Date(data_raw()$date, format = "%Y-%m-%d")) + 7)==FALSE) {
      stop("The start date of the training set must be at least 7 days posterior to the first date in the dataset.")
    }
    
    date_train_set 
  })
  
  date_test_set <- reactive({
    date_test_set <- c(input$test_start_date, input$test_end_date)
    
    if ((as.Date(input$test_start_date, format = "%Y-%m-%d")<as.Date(input$test_end_date, format = "%Y-%m-%d"))==FALSE) {
      stop("The start date of the test set must be earlier than the end date.")
    }
    
    date_test_set 
  })
  
  #check date train and test sets do not overlap; if they overlap, display a warning
  # make the text in color red
  output$check_date_train_test_set <- renderUI({
    
    train_start <- as.Date(date_train_set()[1], format = "%Y-%m-%d")
    train_end   <- as.Date(date_train_set()[2], format = "%Y-%m-%d")
    test_start  <- as.Date(date_test_set()[1], format = "%Y-%m-%d")
    test_end    <- as.Date(date_test_set()[2], format = "%Y-%m-%d")
    
    # Check for overlap
    overlap <- !(train_end < test_start || test_end < train_start)
    
    if (overlap) {
      tags$div(
        style = "border: 1px solid red; background-color: #ffe6e6; color: red; padding: 10px; border-radius: 5px;",
        "⚠️ Warning: the test set overlaps with the training set. The forecasting performance of the model on the current test set should be interpreted carefully; forecasts on a completely new test set may not be as good."
      )
    } else {
      NULL  # Don't display anything
    }
  })
  
  ## pre-processing data
  data_preprocessed <- reactive({
    ## see data_preprocessing() function in the script functions.R
    data_preprocessed <- data_preprocessing(data = data_raw(), 
                                            temp_col = input$temp_indicator, 
                                            input_months = input$months, 
                                            input_data = input$data,
                                            country = input$country,
                                            canton = input$canton, 
                                            date_period_of_interest = date_period_of_interest()) 
    
    data_preprocessed
  })
  
  # display table in the user interface to inspect the dataset
  output$viewDataPreprocessed <- DT::renderDataTable({
    df <- data_preprocessed()
    datatable(df, options = list(scrollX = TRUE, pageLength = 3, searching = FALSE))
  })
  
  # define train set
  data_train <- reactive({
    data_train  <- subset(data_preprocessed(), (as.Date(date, format="%Y-%m-%d") >= as.Date(date_train_set()[1], format="%Y-%m-%d"))) 
    data_train  <- subset(data_train, (as.Date(date, format="%Y-%m-%d") <= as.Date(date_train_set()[2], format="%Y-%m-%d")))
    
    # if dataset is empty, raise an error to the user
    if (nrow(data_train) == 0) {
      stop("The training set is empty. It is likely that the selected range of start/end date does not overlap with the time period of interest (summer months). Please adjust the start/end date or the time period of interest to allow overlap.")
    }

    data_train
  })
  
  # define test set
  data_test <- reactive({
    
    data_test  <- subset(data_preprocessed(), (as.Date(date, format="%Y-%m-%d") >= as.Date(date_test_set()[1], format="%Y-%m-%d"))) 
    data_test  <- subset(data_test, (as.Date(date, format="%Y-%m-%d") <= as.Date(date_test_set()[2], format="%Y-%m-%d")))

    # if dataset is empty, raise an error to the user
    if (nrow(data_test) == 0) {
      stop("The test set is empty. It is likely that the selected range of start/end date does not overlap with the time period of interest (summer months). Please adjust the start/end date or the time period of interest to allow overlap.")
    }
    
    data_test
  })
  
  # plot the data
  drawHistoricalDataPlot <- function () {
    
    req(input$temp_indicator)
    
    # get temperature indicator
    temp_col <- input$temp_indicator
    
    # ensure that every date between the minimum and maximum date in your data appears in the prepocessed dataset
    # this is done in order to plot the moving average with null values in between years
    data_preprocessed_complete <- data_preprocessed() %>%
      complete(date = seq(min(date), max(date), by = "day"))  
    # If any days are missing (e.g., no data recorded on weekends or holidays), complete() inserts those dates as new rows.
    #The values in other columns for these newly created rows will be filled with NA.
    
    # Build plot
    
    # top plot with hospital data
    low <- min(data_raw()$hosp_counts)
    up <- max(data_raw()$hosp_counts)
    
    a <- ggplot(data = data_raw(), aes(x = date)) + 
      geom_rect(aes(xmin = as.Date(date_train_set()[1], format="%Y-%m-%d"), 
                    xmax = as.Date(date_train_set()[2], format="%Y-%m-%d")),
                ymin = up-(up-low)/20, ymax = up, # rectangle spanning over the train set 
                fill = col_train) + 
      geom_rect(aes(xmin = as.Date(date_test_set()[1], format="%Y-%m-%d"), 
                    xmax = as.Date(date_test_set()[2], format="%Y-%m-%d")),
                ymin = up-2*(up-low)/20, ymax = up-(up-low)/20, # rectangle spanning over the test set 
                fill = col_test) + 
      geom_point(data = data_raw(), aes(y = hosp_counts, color="raw data")) + # scatter plot data raw
      geom_point(data = data_train(), aes(y = hosp_counts, color="train set")) + # scater plot data train
      geom_point(data = data_test(), aes(y = hosp_counts, color="test set")) + # scatter plot data test
      geom_line(data = data_preprocessed_complete, aes(y = rollmean(hosp_counts, 7, na.pad = TRUE, 
                                                             align = "right"), color = "weekly moving average"), 
                linewidth = 0.5, na.rm = FALSE) + # line plot moving average, with null values in between years
      xlab("date") +
      ylab(sprintf("hospital visits")) +
      geom_vline(aes(xintercept=as.numeric(as.Date(date_train_set()[1], format="%Y-%m-%d"))),
                 color = col_train, linetype = "dashed") +
      geom_vline(aes(xintercept=as.numeric(as.Date(date_train_set()[2], format="%Y-%m-%d"))),
                 color = col_train, linetype = "dashed") +
      geom_vline(aes(xintercept=as.numeric(as.Date(date_test_set()[1], format="%Y-%m-%d"))),
                 color = col_test, linetype = "dashed") +
      geom_vline(aes(xintercept=as.numeric(as.Date(date_test_set()[2], format="%Y-%m-%d"))),
                 color = col_test, linetype = "dashed") +
      scale_colour_manual("", 
                          breaks = c("raw data",  "weekly moving average", "train set", "test set"),
                          values = c("lightgrey",  "black", col_train, col_test),
                          guide = guide_legend(nrow = 2, byrow = TRUE)) +
      theme_minimal(base_size=14) + 
      theme(legend.position="top", legend.justification="right", 
            legend.text = element_text(size = 12),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) # Add black border to plot
    
    # bottom panel with temperature data
    low <- floor(min(data_raw()[,temp_col]-1))
    up <- ceiling(max(data_raw()[,temp_col]+1))
    
    b <- ggplot(data = data_raw(), aes(x = date)) +
      geom_point(aes(y = !!sym(temp_col), color="raw data")) +
      geom_point(data = data_train(), aes(y = !!sym(temp_col), color="train set")) +
      geom_point(data = data_test(), aes(y = !!sym(temp_col), color="test set")) +
      geom_line(data = data_preprocessed_complete, aes(y = rollmean(!!sym(temp_col), 7, na.pad = TRUE, 
                                                             align = "right"), color = "weekly moving average"), 
                linewidth = 0.5, na.rm = TRUE) +
      xlab("date") +
      ylab(sprintf("temperature (°C)")) +
      scale_colour_manual("", 
                          breaks = c("raw data", "weekly moving average", "train set", "test set"),
                          values = c("lightgray", "black", col_train, col_test),
                          guide = guide_legend(nrow = 2, byrow = TRUE)) +
      theme_minimal(base_size=14)  + 
      theme(legend.position="top", legend.justification="right",   legend.text = element_text(size = 12),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) + # Add black border
      ylim(c(low,up))
    
    # right panels with boxplots
    c <- ggplot(data = data_preprocessed(), aes(x = as.factor(year), y = hosp_counts)) +
      geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.5) +  # Boxplot with transparent fill
      geom_jitter(color="darkslateblue", width = 0.2, alpha = 0.2) +  # Scatter plot with jitter
      labs(x = "", y = "hospital visits") +
      theme_minimal(base_size=14)  + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
            panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))  # Add black border
    
    d <- ggplot(data = data_preprocessed(), aes(x = as.factor(year), y = !!sym(temp_col))) +
      geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.5) +  # Boxplot with transparent fill
      geom_jitter(color="darkslateblue", width = 0.2, alpha = 0.2) +  # Scatter plot with jitter
      labs(x = "", y = "temperature (°C)") +
      theme_minimal(base_size=14)  + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
            panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))  # Add black border
    
    # Add top margin (e.g., 1 cm) to the plots c and d
    c <- c + theme(plot.margin = margin(t = 1.5, r = 0.5, b = 0., l = 0.5, unit = "cm"))
    d <- d + theme(plot.margin = margin(t = 1.5, r = 0.5, b = 0., l = 0.5, unit = "cm"))
    
    ggarrange(a, c, b, d, 
                            labels = c("Hospital data", 
                                       " ", 
                                       "Temperature data", 
                                       " "),
                            ncol = 2, nrow = 2, widths = c(2.5, 1))
}
  
  
  output$renderHistoricalDataPlot <- renderPlot({
    withProgress(message = "Computing...", value = 0, {
      drawHistoricalDataPlot()
    })
  })
  
  ## add download button
  output$downloadHistoricalDataPlot <- downloadHandler(
    filename = function() {
      "1_historical_data_plot.png"
    },
    content = function(file) {
      ggsave(file, drawHistoricalDataPlot(), 
             width = 12, height = 8, dpi = 300, bg = "white")
    }
  )
  
  
  # SECTION 2a: MODEL ############################################################################
  
  
  # if user mode == "Beginner", we set default values for the model parameters
  observeEvent(input$mode, {
    if (input$mode == "Beginner") {
      updateSelectInput(session, "past.obs", selected = "Yes")
      updateSelectInput(session, "day.of.week.hol", selected = "Yes")
      updateSelectInput(session, "pandemic", selected = "Yes")
      updateSelectInput(session, "maxlag", selected = 3)
      updateSelectInput(session, "tknots", selected = "3 (10%, 50%, 90%)")
    }
  })
  
  # choose type of splines to be used for modeling temperature
  input_spline_type <- "Natural (cubic) splines"  # B-Splines
  
  # choose link function for glm ( )
  input_family <- "gaussian" 
  
  # define boundary knots 
  bound_knots <- reactive({
    req(input$temp_indicator)
    # boundary knots are chosen conservatively, 
    # by rounding min and max values of observed temperature in the training set
    # to the closest value in a sequence with 0.5 increments
    # e.g., if min = 10.9 then the lower boundary knot will be 11 (rounding up)
    # and if max = 27.7 then the upper boundary knot will be 27.5 (rounding down)
    bound_knots <- c(ceiling(min(data_train()[,input$temp_indicator]) * 2) / 2, # round up to the closest .5
                    floor(max(data_train()[,input$temp_indicator]) * 2) / 2) # round down to the closest .5

    bound_knots
  })
  
  # get vector of internal knots
  output$show_choice_knots <- renderUI({
    if (input$tknots != 'manual') 
      return(NULL) # if user chooses a pre-defined set of knots, do not show the text input
    else {
      # if user chooses "manual", show the text input
      textInput('vector_knots', 'Choose internal knots (comma delimited)', "15,25")
    }
  })
  
  # define internal knots
  tknots <- reactive({
    if (input$tknots == "2 (equally spaced)") {
      tknots <- quantile(seq(bound_knots()[1], bound_knots()[2], length.out = 1000),
                         probs = c(1./3.,2./3.))
    }
    else if (input$tknots == "3 (10%, 50%, 90%)") {
      tknots <- quantile(seq(bound_knots()[1], bound_knots()[2], length.out = 1000), probs = c(0.1,0.5,0.9))
    }
    else if (input$tknots == "3 (5%, 50%, 95%)") {
      tknots <- quantile(seq(bound_knots()[1], bound_knots()[2], length.out = 1000), probs = c(0.05,0.5,0.95))
    }
    else if (input$tknots == "3 (25%, 50%, 75%)") {
      tknots <- quantile(seq(bound_knots()[1], bound_knots()[2], length.out = 1000), probs = c(0.25,0.5,0.75))
    }
    else if (input$tknots == "manual") {
      tknots <- as.numeric(unlist(strsplit(input$vector_knots,",")))
    }
    else {
      stop("Internal knots not defined.")
    }
    tknots
  })
  
  # compute degrees of freedom of the splines
  # see function compute_dof in the script functions.R
  dof <- reactive({
    dof <- compute_dof(input_spline_type, tknots())
    dof
  })
  
  # display degrees of freedom
  output$degree_of_freedom <- renderText({
    s <- paste("Degrees of freedom for the splines: ", dof())
    s
  })
  
  # check degrees of freedom
  output$checkModelDof <- renderText({
    validate(
      need(dof() %in% c(3,4,5), "Error: cannot run a model with other degrees of freedom for the spline basis except 3, 4 and 5. 
           Please adjust the number of internal knots or the degree of the splines.")
    )
  })
  
  # define regression model, using the function build_model() from functions.R
  model <- reactive({
  model <- build_model(data_train(), input_family, 
                       dof(), input_spline_type, tknots(), bound_knots(), input$maxlag, 
                       input$past.obs, input$day.of.week.hol, 
                       input$pandemic)
    model
  })
  
  # define regression model without temperature, for comparison 
  # using the function build_model_wo_temp() from functions.R
  model_wo_temp <- reactive({
    model <- build_model_wo_temp(data_train(), input_family, 
                         input$past.obs, input$day.of.week.hol, 
                         input$pandemic)
    model
  })
  
  
  # plot the estimated exposure response function
  drawExposureResponsePlot <- function() {
    
    req(input$temp_indicator)
    
    # define vector x to compute spline
    x <- seq(bound_knots()[1], bound_knots()[2], 0.5)
    # define extended vector to project the spline
    ext_x <- seq(bound_knots()[1]-2, bound_knots()[2]+2, 0.5)
    
    # define layout for the plot
    layout(matrix(c(1,2), 1, 2, byrow = T)) 
    # set margins
    par(mar = c(5, 5, 4, 6)) 
    
    ## left panel with cumulative exposure-response function
    
    # generate basis with function compute_basis() from functions.R
    basis <- compute_basis(x, input_spline_type, tknots(), bound_knots())
    # compute polynomial by combining the different lags, with function exp_resp_polynomial() from functions.R     
    polynomial <- exp_resp_polynomial(coef(model()), dof(), basis, input$maxlag)
    # use point of minimum , i.e. min(polynomial), as temperature of reference 
    T_ref <- x[which.min(polynomial)]
    polynomial <- polynomial - polynomial[x==T_ref]
    
    # Generate the spline basis matrix on the extended vector x
    ext_basis <- compute_basis(ext_x, input_spline_type, tknots(), bound_knots())
    # compute polynomial by combining the different lags  
    ext_polynomial <- exp_resp_polynomial(coef(model()), dof(), ext_basis, input$maxlag)
    # shift using the same point of minimum as above
    ext_polynomial <- ext_polynomial - ext_polynomial[ext_x==T_ref]
    
    
    ### ADD CONFIDENCE INTERVALS
    for (s in c(1:1000)){
      # to build confidence intervals, we draw from a multinomial distribution with variance/covariance matrix estimated from the model
      tryCatch({
        samples <- mvrnorm(n = 1, coef(model()), vcov(model()))
      }, error = function(e) {
        # if encountering issues, raise an error to display to the user
        stop("A coefficient in your regression model is not identifiable (probably the one associated to the change of level for pandemic period). Go to the expert mode and remove this predictor, or change the definition of training set in order to include both pandemic periods and pre-/post-pandemic periods.")
      })
      
      # compute polynomial by combining the different lags      
      polynomial_ci <- exp_resp_polynomial(samples, dof(), basis, input$maxlag)
      # shift using the same point of minimum as above
      polynomial_ci <- polynomial_ci - polynomial_ci[x==T_ref]      
      
      if (s == 1){
        # first sample, initialize vector
        list_poly <- polynomial_ci
      }
      else {
        # for folliwing samples, concatenate results with cbind
        list_poly <- cbind(list_poly, polynomial_ci)
      }
    }
    
    # compute quantiles across rows 
    qs <- rowQuantiles(list_poly, probs = c(.025, .5, .975))
    
    # define y_lim for plot
    if (input_family == "gaussian") {
      y_limits <- c(floor(min(qs[,"2.5%"]) / 5) * 5, # round down to the closest multiple of 5 
                    ceiling(max(qs[,"97.5%"]) / 5) * 5) # round up to the closest multiple of 5 
      
      label_y = bquote("absolute change in daily counts wrt to " ~ T[min] ~ "=" ~ .(T_ref) ~ degree*C)
    }
    else {
      stop("Error: only gaussian family is currently supported.")
    }
    
    plot(x, polynomial, type = 'l', col="red", 
         ylim = y_limits, 
         xlim = c(ext_x[1], ext_x[length(ext_x)]),
         main = "Cumulative exposure-response function",
         lwd = 4, xlab="temperature (°C)", 
         ylab = label_y,
         cex.lab = 1.1,     # Axis labels
         cex.axis = 1.1,    # Tick labels
         cex.main = 1.1)    # Title
    
    lines(ext_x, ext_polynomial, col="red", lty="dashed")
    
    polygon(c(x, rev(x)), c(qs[,"2.5%"], rev(qs[,"97.5%"])),
            col = rgb(0.71, 0.71, 0.71, 0.369))
    
    abline(h = seq(y_limits[1]-5, y_limits[2]+5, 5), col = "darkgrey", lty = "dashed")
    abline(h = 0, col = "black", lty = "dashed")

    if (input$mode == "Expert") {
      ## add vertical lines for the internal knots
      for (i in c(1:length(tknots()))) {
        abline(v=tknots()[i], col = "darkgreen", lty = "dotted")
      }
    }
    
    # right y-axis, showe relative change with respect to mean of test set
    
    ## we choose mean of test data as a reference
    H_ref <- mean(data_test()$hosp_0)
    
    # if test set is defined and not empty
    # if H_ref is not NULL
    if (is.na(H_ref) == FALSE) {
      ## compute relative increase as a function of temperature T as 
      ## RR(T) = 100* (H_ref + abs_diff(T) - H_ref) / H_ref = 100 * abs_diff(T)/H_ref, where abs_diff(T) is the absolute change, 
      ## i.e. the exposure response function estimated above, if one uses the gaussian model
      
      # y_limits[1] is a multiple of 10
      if (y_limits[1] %% 10 != 0) {
        y_ticks <- seq(y_limits[1], y_limits[2]+5, 10)
      }
      else {
        y_ticks <- seq(y_limits[1]-5, y_limits[2]+5, 10)
      }
  
      polynomial_rel_incr <- round(100*(y_ticks/H_ref))
      
      # add ticklabels to the right y-axis
      axis(side = 4, at = y_ticks ,
           labels = polynomial_rel_incr, 
           cex.axis = 1.1)
      # Add label to the right y-axis
      mtext(bquote("relative variation (%) with respect to " ~ H[ref] ~ "=" ~ .(round(H_ref))),
            side = 4, line = 3, cex = 1.1)
    }

    ## add right panel with each lag separately (only expert mode)
    if (input$mode == "Expert") {
      
      # Generate spline basis matrix
      if (input_spline_type=="Natural (cubic) splines") {
        basis <- ns(x, knots=tknots(), Boundary.knots = bound_knots())
      }
      else {
        stop("Error: only natural (cubic) splines are currently supported.")
      }
      
      # get model coefficients
      coefficients <- coef(model())[c(2:(dof()+1))]
      # Calculate the polynomial function for lag 0
      polynomial <- basis %*% coefficients
      # use point of minimum , i.e. min(polynomial), as temperature of reference 
      polynomial <- polynomial-polynomial[x==T_ref]
      
      plot(x, polynomial, type = 'l', col="1", 
           xlim = c(ext_x[1], ext_x[length(ext_x)]),
           ylim=y_limits, 
           main = "Exposure-lag-response function",
           lwd=2, xlab="temperature (°C)",  
           ylab=label_y,
           cex.lab = 1.1,
           cex.axis = 1.1, 
           cex.main = 1.1)
      
      # Calculate and plot the polynomial function for other lags
      for (h in c(1:input$maxlag)) {
        coefficients <- coef(model())[c((2+(h*dof())):((dof()+1)+(h*dof())))]
        polynomial <- basis %*% coefficients
        # use point of minimum , i.e. min(polynomial), as temperature of reference 
        polynomial <- polynomial-polynomial[x==T_ref]
        lines(x, polynomial, type = 'l', col=h+1, lwd=2)
      }
      
      abline(h = seq(y_limits[1]-5, y_limits[2]+5, 5), col = "darkgrey", lty = "dashed")
      abline(h = 0, col = "black", lty = "dashed")
      
      for (i in c(1:length(tknots()))) {
        abline(v=tknots()[i], col = "darkgreen", lty = "dotted")
      }
      
      legend(x = "topleft", 
             cex = 1.1, 
             lty = 1, lwd=2, 
             col= c(0:input$maxlag) + 1, 
             legend=paste0("lag ", c(0:input$maxlag))) 
    }
    
    
  }
  
  output$renderExposureResponsePlot <- renderPlot({
    withProgress(message = "Computing...", value = 0, {
      drawExposureResponsePlot()
    })
  })
  
  ## add download button 
  output$downloadExposureResponsePlot <- downloadHandler(
    filename = function() {
      "2_exposure_response_plot.png"
    },
    content = function(file) {
      png(file, width = 12, height = 6, units = "in", res = 300)
      drawExposureResponsePlot()
      dev.off()
    }
  )
  
  
  
  
  # SECTION 2b: additional tabs ############################################################################
  
  ### PLOT TEMPERATURE DATA ############################################################################
  
  # histogram of temperature data, by train and test set
  output$histogramData <- renderPlot({
    
    # define vector x to compute histogram
    ext_x <- seq(floor(min(min(data_train()[,input$temp_indicator]),
                           min(data_test()[,input$temp_indicator]))), 
                 ceiling(max(max(data_train()[,input$temp_indicator]),
                             max(data_test()[,input$temp_indicator]))), 
                 1.)

    par(mfrow = c(1, 2))
    
    df <- data_train()
    
    hist(df[,input$temp_indicator], breaks = ext_x, 
         xlim = c(ext_x[1], ext_x[length(ext_x)]), 
         xlab = "temperature (°C)", ylab = "number of days", main = "training data",
         cex.lab = 1.2,     # Axis labels
         cex.axis = 1.2,    # Tick labels
         cex.main = 1.2)    # Title)
    
    df <- data_test()
    
    hist(df[,input$temp_indicator], breaks = ext_x, 
         xlim = c(ext_x[1], ext_x[length(ext_x)]), 
         xlab = "temperature (°C)", ylab = "number of days", main = "test data",
         cex.lab = 1.2,     # Axis labels
         cex.axis = 1.2,    # Tick labels
         cex.main = 1.2)    # Title
    
  })
  
  # time series plot of temperature data, with days above threshold highlighted in red
  output$plotHotDays <- renderPlot({
    
    req(input$temp_indicator)
    
    # define y_lim 
    ext_y <- c(floor(min(min(data_train()[,input$temp_indicator]),
                           min(data_test()[,input$temp_indicator]))), 
                 ceiling(max(max(data_train()[,input$temp_indicator]),
                             max(data_test()[,input$temp_indicator]))))
    
    df <- rbind(data_train(), data_test())
    
    hot_days <- df %>% filter(!!sym(input$temp_indicator) >= input$thresholdHotDay)
    
    # Add a dummy column to represent a single facet grid row
    df$facet_row <- "row"  # all plots will be in a single row group
    
    p <- ggplot(df, aes(x = date)) +
      geom_point(aes(y = !!sym(input$temp_indicator)), color="black", alpha = 0.8) +
      geom_line(aes(y = !!sym(input$temp_indicator)), color="black", alpha = 0.8, linewidth=1) +
      facet_wrap(~ year, scales = "free", ncol = 2) + # Adjust ncol to control the number of columns
      theme_minimal(base_size = 18) +
      geom_point(data = hot_days, aes(x = date, y = !!sym(input$temp_indicator)), 
                 color = "red", size = 2) +
      geom_hline(yintercept = input$thresholdHotDay, linetype = "dashed", color = "black") +
      theme(strip.text = element_text(size = 16, face = "bold"), 
            panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) + 
      ylab("temperature (°C)") + 
      xlab("date") + 
      ylim(ext_y[1], ext_y[2])
    
    # Print the plot
    p
    
  })
  
  # make the above plot dynamic adjusting the height of the plot based on the number of years in the data
  output$dynamicPlotHotDays <- renderUI({
    df <- rbind(data_train(), data_test())
    num_years <- length(unique(df$year))  # number of facets
    ncol <- 2  # keep consistent with the facet_wrap(~ year, ncol = 2)
    
    nrows <- ceiling(num_years / ncol)
    base_height_per_row <- 200  
    
    plot_height <- nrows * base_height_per_row
    
    plotOutput("plotHotDays", height = paste0(plot_height, "px"))
  })
  
  ### MODEL FIT and EVALUATION ############################################################################
  
  # table with model inputs
  output$modelInputs <- renderTable({
    df <- data.frame(Input = character(), Value = character(), stringsAsFactors = FALSE)
    
    df <- rbind(df, data.frame(Input = "Data source", Value = input$data))
    
    if (input$data == "Upload your own data") {
      df <- rbind(df, data.frame(Input = "Uploaded file", Value = input$file$name))
      df <- rbind(df, data.frame(Input = "Country", Value = input$country))
      if (input$country == "Switzerland(canton)") {
        df <- rbind(df, data.frame(Input = "Canton", Value = input$canton))
      }
    }
    
    df <- rbind(df, data.frame(Input = "Temperature indicator", Value = input$temp_indicator))
    df <- rbind(df, data.frame(Input = "Period of interest", Value = input$months))
    df <- rbind(df, data.frame(Input = "Train set", Value = paste(input$train_start_date, input$train_end_date, sep = ", ")))
    df <- rbind(df, data.frame(Input = "Test set", Value = paste(input$test_start_date, input$test_end_date, sep = ", ")))
    df <- rbind(df, data.frame(Input = "7 days of past hospital data", Value = input$past.obs))
    df <- rbind(df, data.frame(Input = "Day of the week & holiday", Value = input$day.of.week.hol))
    df <- rbind(df, data.frame(Input = "Pandemic change of level", Value = input$pandemic))
    df <- rbind(df, data.frame(Input = "Maximum lag for temperature", Value = input$maxlag))
    df <- rbind(df, data.frame(Input = "Internal knots", Value = paste(tknots(), collapse = ", ")))
    
    df
  })
  
  # model summary ( coefficients, p-values, etc. )
  output$modelSummary <- renderPrint({
    summary(model())
  })
  
  # plot regression coefficients
  output$plotOtherRegresCoef <- renderPlot({
    
    # plot with 1 row and 2 columns with additional space in between columns
    par(mfrow=c(1, 2))
    
    coeff <- ci.lin(model())[,c(1,5,6)]
    
    if (input$past.obs=="Yes") {
      ss <- c("hosp_minus1", "hosp_minus2", "hosp_minus3", "hosp_minus4", "hosp_minus5", "hosp_minus6", "hosp_minus7")
      zz <- c(1:length(ss))
      plot(zz,coeff[ss,1], xaxt = "n", 
           ylab = "coefficient", 
           xlab = "",
           col="blue", ylim=c(min(coeff[ss,2]),max(coeff[ss,3])),
           cex.lab = 1.2,     # Axis labels
           cex.axis = 1.1)
      arrows(zz, coeff[ss,2], zz, coeff[ss,3], length=0.05, angle=90, code=3, col="blue")
      axis(1, at = zz, labels = c("H(t-1)", "H(t-2)", "H(t-3)", "H(t-4)", "H(t-5)", "H(t-6)", "H(t-7)"), 
           cex.axis = 1.2)
      abline(h=0, col="black",  lty = 2) 
    }
    
    if (input$day.of.week.hol=="Yes") {
      ss <- c("reg_TUE", "reg_WED", "reg_THU", "reg_FRI", "SAT", "SUN", "wd_holiday")
      zz <- c(1:length(ss))
      plot(zz,coeff[ss,1], xaxt = "n", 
           ylab = "coefficient",
           xlab = "",
           col="blue", ylim=c(min(coeff[ss,2]),max(coeff[ss,3])),
           cex.lab = 1.2,     # Axis labels
           cex.axis = 1.1)
      arrows(zz, coeff[ss,2], zz, coeff[ss,3], length=0.05, angle=90, code=3, col="blue")
      axis(1, at = zz, labels = c("Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "holiday"), 
           cex.axis = 1.2)
      abline(h=0, col="black",  lty = 2) 
    }
  })
  
  # compute evaluation metrics on training set
  output$modelMetricsTrain <- renderTable({
    
    df <- data.frame(Metric = character(), Value = numeric(), stringsAsFactors = FALSE)
    
    # Add rows using rbind
    if (input_family == "gaussian") {
      AIC_value <- round(AIC(model()),1) 
      
      if (model_wo_temp()[[1]] ==TRUE) {
        AIC_value_wo_temp <- round(AIC(model_wo_temp()[[2]]),1)  
      }
    }
    else {
      stop("Error: only gaussian family is currently supported.")
    }
    
    df <- rbind(df, data.frame(Metric = "RMSE (ref model)", 
                               Value = rmse(data_train()$hosp_0, predict(model(), type="response"))))
    if (model_wo_temp()[[1]] ==TRUE) {
      df <- rbind(df, data.frame(Metric = "RMSE (model w/o temp)", 
                                 Value = rmse(data_train()$hosp_0, predict(model_wo_temp()[[2]], type="response"))))
    }
    
    df <- rbind(df, data.frame(Metric = "RMSE (LOCF, 7 days ago)", 
                               Value = rmse(data_train()$hosp_0, data_train()$hosp_minus7)))
    df <- rbind(df, data.frame(Metric = "RMSE (LOCF, 1 day ago)", 
                               Value = rmse(data_train()$hosp_0, data_train()$hosp_minus1)))
    df <- rbind(df, data.frame(Metric = "MAE (ref model)", 
                               Value = mae(data_train()$hosp_0, predict(model(), type="response"))))
    if (model_wo_temp()[[1]] ==TRUE) {
      df <- rbind(df, data.frame(Metric = "MAE (model w/o temp)", 
                                 Value = mae(data_train()$hosp_0, predict(model_wo_temp()[[2]], type="response"))))
    }
    df <- rbind(df, data.frame(Metric = "MAE (LOCF, 7 days ago)", 
                               Value = mae(data_train()$hosp_0, data_train()$hosp_minus7)))
    df <- rbind(df, data.frame(Metric = "MAE (LOCF, 1 day ago)", 
                               Value = mae(data_train()$hosp_0, data_train()$hosp_minus1)))
    df <- rbind(df, data.frame(Metric = "AIC (ref model)", 
                               Value = AIC_value))
    if (model_wo_temp()[[1]] ==TRUE) {
      df <- rbind(df, data.frame(Metric = "AIC (model w/o temperature)", 
                                 Value = AIC_value_wo_temp))
    }
    
    df
  })
  
  # compute evaluation metrics on training set
  output$modelMetricsTest <- renderTable({
    df <- data.frame(Metric = character(), Value = numeric(), stringsAsFactors = FALSE)
    
    data <- data_test()
    
    # add spline basis to test set
    if (input_spline_type=="Natural (cubic) splines") {
      for (v in c("temp_0", 
                  "temp_minus_1", "temp_minus_2",
                  "temp_minus_3", "temp_minus_4", 
                  "temp_minus_5", "temp_minus_6", "temp_minus_7")) {
        s <- ns(as.numeric(unlist(data[,v])),  
                knots=tknots(), Boundary.knots=bound_knots()) 
        colnames(s) <- paste0(v, "_bs_", colnames(s))
        data <- cbind(data, s)
      }
    }
    else{
      stop("Error: only natural (cubic) splines are currently supported.")
    }
    
    df <- rbind(df, data.frame(Metric = "RMSE (ref model)", 
                               Value = rmse(data_test()$hosp_0, predict(model(), newdata = data, type="response"))))
    if (model_wo_temp()[[1]] ==TRUE) {
      df <- rbind(df, data.frame(Metric = "RMSE (model w/o temp)", 
                                 Value = rmse(data_test()$hosp_0, predict(model_wo_temp()[[2]], newdata = data, type="response"))))
    }
    
    df <- rbind(df, data.frame(Metric = "RMSE (LOCF, 7 days ago)", 
                               Value = rmse(data_test()$hosp_0, data_test()$hosp_minus7)))
    df <- rbind(df, data.frame(Metric = "RMSE (LOCF, 1 day ago)", 
                               Value = rmse(data_test()$hosp_0, data_test()$hosp_minus1)))
    df <- rbind(df, data.frame(Metric = "MAE (ref model)", 
                               Value = mae(data_test()$hosp_0, predict(model(), newdata = data, type="response"))))
    if (model_wo_temp()[[1]] ==TRUE) {
      df <- rbind(df, data.frame(Metric = "MAE (model w/o temp)", 
                                 Value = mae(data_test()$hosp_0, predict(model_wo_temp()[[2]], newdata = data, type="response"))))
    }
    
    df <- rbind(df, data.frame(Metric = "MAE (LOCF, 7 days ago)", 
                               Value = mae(data_test()$hosp_0, data_test()$hosp_minus7)))
    df <- rbind(df, data.frame(Metric = "MAE (LOCF, 1 day ago)", 
                               Value = mae(data_test()$hosp_0, data_test()$hosp_minus1)))
    df
  })
  
  # plot showing fit on training data
  output$fitPlot <- renderPlot({
    
    df <- data_train()
    
    # model fit (prediction on training set)
    df$predictions <- predict(model(), type="response")
    
    p <- ggplot(df, aes(x = date)) +
      geom_point(aes(y = hosp_0, color="raw data"), alpha = 0.5) +
      geom_point(aes(y = predictions, color="model predictions"), alpha = 0.5) +
      geom_line(aes(y = rollmean(hosp_0, 7, na.pad = TRUE, 
                                 align = "right"), color = "raw data \n(weekly rolling mean)"), linewidth=2, na.rm = TRUE) +
      geom_line(aes(y = rollmean(predictions, 7, na.pad = TRUE, 
                                 align = "right"), color = "model predictions \n(weekly rolling mean)"), linewidth=2, na.rm = TRUE) +
      facet_wrap(~ year, scales = "free", ncol = 2) + # Adjust ncol to control the number of columns
      theme_minimal(base_size = 18) +
      scale_colour_manual("", 
                          breaks = c("raw data", "model predictions", 
                                     "raw data \n(weekly rolling mean)", "model predictions \n(weekly rolling mean)"), 
                          values = c("black", col_train,  
                                     "black", col_train),
                          guide = guide_legend(ncol = 1, 
                                               label.theme = element_text(margin = margin(b = 7.5)))) +
      theme(legend.position="right", legend.justification="top", 
            strip.text = element_text(size = 16, face = "bold"),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
            legend.text = element_text(size = 14)) + 
      ylab("hospital visits") + 
      xlab("date") 
    
    p
  })
  
  # make the above plot dynamic adjusting the height of the plot based on the number of years in the data
  output$dynamicFitPlot <- renderUI({
    df <- data_train()
    num_years <- length(unique(df$year))  # number of facets
    ncol <- 2  # keep consistent with the facet_wrap(~ year, ncol = 2)
    
    nrows <- ceiling(num_years / ncol)
    base_height_per_row <- 200  
    
    plot_height <- nrows * base_height_per_row
    
    plotOutput("fitPlot", height = paste0(plot_height, "px"))
  })
  
  ### FORECAST ERRORS ############################################################################
  
  # line plot with examples of forecast windows
  output$forecastWindow <- renderPlot({
    
    # Set wider margin (bottom, left, top, right)
    par(mar = c(4, 5.5, 8, 1.5), xpd = TRUE)
    
    data <- data_test()
    
    # add spline basis to test set
    if (input_spline_type=="Natural (cubic) splines") {
      for (v in c("temp_0", 
                  "temp_minus_1", "temp_minus_2",
                  "temp_minus_3", "temp_minus_4", 
                  "temp_minus_5", "temp_minus_6", "temp_minus_7")) {
        s <- ns(as.numeric(unlist(data[,v])),  
                knots=tknots(), Boundary.knots=bound_knots()) 
        colnames(s) <- paste0(v, "_bs_", colnames(s))
        data <- cbind(data, s)
      }
    }
    else{
      stop("Error: only natural (cubic) splines are currently supported.")
    }
    
    # plot with raw data
    plot(data$date, data$hosp_0, xlab="",
         main="", 
         ylab="hospital visits",
         cex.main=1.5, #change font size of title
         cex.sub=1.5, #change font size of subtitle
         cex.lab=1.5, #change font size of axis labels
         cex.axis=1.5) #change font size of axis text 
    
    # compute one-step ahead predictions
    pred_test <- predict(model(), newdata=data, type="response")
    # plot it
    lines(data$date, pred_test, col="black", lwd=2) 
    
    ##### k-step ahead forecasts
    # choose length of forecast window
    window_length <- 21
    
    # display at maximum 5 examples of time windows
    for (h in c(0:4)) {
      
      forecast_data <- data
      
      day_start <- 8 + 30*h # day start of the first window is at least 7 days after the start of the testing set
      # the subsequent windows are chosen to be 30 days apart
      
      window <- day_start + window_length - 1
      
      if (window > length(data$date)) {
        break
      }
      
      # generate k-step ahead forecast using the function from script functions.R
      # function arguments: k_step_forecast <- function(forecast_data, start_k_forecast, window, model, input_spline_type, internal_knots, boundary_knots, compute_pi)
      # this function computes point prediction and also prediction intervals
      predlag_forecast <- k_step_forecast(forecast_data, day_start, window, 
                                          model(), input_spline_type, tknots(), bound_knots(), TRUE)
      
      # plot prediction interval 50% 
      polygon(c(data$date[c(day_start:window)], rev(data$date[c(day_start:window)])), 
              c(predlag_forecast[day_start:window,"lwr_50"], rev(predlag_forecast[day_start:window,"upr_50"])),
              col = rgb(0.31, 0.71, 0.71, 0.369), border = NA)
      
      # plot prediction interval 90% 
      polygon(c(data$date[c(day_start:window)], rev(data$date[c(day_start:window)])), 
              c(predlag_forecast[day_start:window,"lwr_90"], rev(predlag_forecast[day_start:window,"upr_90"])),
              col = rgb(0.71, 0.71, 0.71, 0.369), border = NA)
      
      # line plot with point predictions
      # dashed line for 21-step ahead prediction
      lines(data$date[c(day_start:window)], 
            predlag_forecast[day_start:window,"predictions"], col='blue', lwd=2, lty="dashed")
      # continuous line for 7-step ahead prediction
      lines(data$date[c(day_start:(day_start+7))], 
            predlag_forecast[day_start:(day_start+7),"predictions"], col='blue', lwd=2)
        
      # the prediction intervals built above for the h-step ahead forecast assume a fixed point prediction in the previous steps (h-1, h-2 etc), 
      # therefore, they do not "propagate" the uncertainty of previous steps forecasts
      # to propagate uncertainty in the prediction interval from h to h+1, we shall use a "bootstrap" method, 
      # simulating trajectories by sampling at each step h from the pediction interval, and then generate forecast h+1 using the sampled preivious steps, 
      # and take quantiles of the ensemble of trajectories at the end
      # this method is encoded in the function "k_step_forecast_uncertainty" which howver is a computationally slow
      # to test the function, uncomment the following lines
      
      #predlag_forecast_uncertainty <- k_step_forecast_uncertainty(forecast_data, day_start, window, 
      #                                                            model(), input_spline_type, tknots(), bound_knots(), TRUE)
      #lines(data$date[c(day_start:window)], 
      #      predlag_forecast_uncertainty[day_start:window,"predictions"], col="red", lwd=2, lty="dashed")
      #lines(data$date[c(day_start:window)], 
      #      predlag_forecast_uncertainty[day_start:window,"lwr_90"], col="red", lwd=2, lty="dashed")
      #lines(data$date[c(day_start:window)], 
      #      predlag_forecast_uncertainty[day_start:window,"upr_90"], col="red", lwd=2, lty="dashed")
    }
    
    legend("bottomleft", inset = c(0., 1.), 
           legend = c("1-step ahead prediction", paste0(7, "-days forecasts"), paste0(window_length, "-days forecasts"), "50% PI", "90% PI"), 
           col = c("black", "blue", "blue", rgb(0.31, 0.71, 0.71, 0.369), rgb(0.71, 0.71, 0.71, 0.369)), 
           lty = c(1, 1, 2, NA, NA), 
           lwd = c(2, 2, 2, NA, NA),
           pch = c(NA, NA, NA, 15, 15), 
           pt.cex = 2, 
           pt.bg = c(NA, NA, NA, rgb(0.31, 0.71, 0.71, 0.369), rgb(0.71, 0.71, 0.71, 0.369)), 
           bty="n")
    
  })
  
  # compute forecast errors
  errors <- reactive({   
    
    data <- data_test()
    
    # add spline basis to test set
    if (input_spline_type=="Natural (cubic) splines") {
      for (v in c("temp_0", 
                  "temp_minus_1", "temp_minus_2",
                  "temp_minus_3", "temp_minus_4", 
                  "temp_minus_5", "temp_minus_6", "temp_minus_7")) {
        s <- ns(as.numeric(unlist(data[,v])),  
                knots=tknots(), Boundary.knots=bound_knots()) 
        colnames(s) <- paste0(v, "_bs_", colnames(s))
        data <- cbind(data, s)
      }
    }
    else{
      stop("Error: only natural (cubic) splines are currently supported.")
    }
    
    ##### k-step ahead forecasts
    # choose length of forecast window
    window_length <- 21
    
    # create a dataset
    errors <- data.frame(name = character(), value = numeric(), avg_temp = numeric())
    
    # iterate over all possible windows with starting date between the 8th day of the test set, and the final date  
    for (h in c(8:length(data$date))) {
      
      forecast_data <- data
      
      day_start <- h
      
      window <- day_start + window_length - 1
      
      if (window <= length(data$date)) {
        # make forecast
        predlag_forecast <- k_step_forecast(forecast_data, day_start, window, model(), 
                                            input_spline_type, tknots(), bound_knots(), FALSE)
        
        # compute forecast error
        errors <- rbind(errors, data.frame(name = "1st week, \nref \nmodel", 
                                           value = rmse(predlag_forecast[day_start:(day_start+7-1),"predictions"], 
                                                        data$hosp_0[day_start:(day_start+7-1)]),
                                           avg_temp = mean(data$temp_0[day_start:(day_start+7-1)])))
        errors <- rbind(errors, data.frame(name = "2nd week, \nref \nmodel", 
                                           value = rmse(predlag_forecast[(day_start+7):(day_start+14-1),"predictions"], 
                                                        data$hosp_0[(day_start+7):(day_start+14-1)]),
                                           avg_temp = mean(data$temp_0[(day_start+7):(day_start+14-1)])))
        errors <- rbind(errors, data.frame(name = "3rd week, \nref \nmodel", 
                                           value = rmse(predlag_forecast[(day_start+14):(day_start+21-1),"predictions"],
                                                        data$hosp_0[(day_start+14):(day_start+21-1)]),
                                           avg_temp = mean(data$temp_0[(day_start+14):(day_start+21-1)])))
        
        # make forecast without temperature as predictor
        if (model_wo_temp()[[1]] == TRUE) {
          
          predlag_forecast <- k_step_forecast(forecast_data, day_start, window, model_wo_temp()[[2]], 
                                              input_spline_type, tknots(), bound_knots(), FALSE)
          # compute forecast error
          errors <- rbind(errors, data.frame(name = "1st week, \nmodel \nw/o temp", 
                                             value = rmse(predlag_forecast[day_start:(day_start+7-1),"predictions"], 
                                                          data$hosp_0[day_start:(day_start+7-1)]),
                                             avg_temp = mean(data$temp_0[day_start:(day_start+7-1)])))
          errors <- rbind(errors, data.frame(name = "2nd week, \nmodel \nw/o temp",
                                             value = rmse(predlag_forecast[(day_start+7):(day_start+14-1),"predictions"], 
                                                          data$hosp_0[(day_start+7):(day_start+14-1)]),
                                             avg_temp = mean(data$temp_0[(day_start+7):(day_start+14-1)])))
          errors <- rbind(errors, data.frame(name = "3rd week, \nmodel \nw/o temp",
                                             value = rmse(predlag_forecast[(day_start+14):(day_start+21-1),"predictions"], 
                                                          data$hosp_0[(day_start+14):(day_start+21-1)]),
                                             avg_temp = mean(data$temp_0[(day_start+14):(day_start+21-1)])))
        }
      }
      else if ((window - 7) <= length(data$date)) {
        # make forecast
        predlag_forecast <- k_step_forecast(forecast_data, day_start, window - 7, model(), 
                                            input_spline_type, tknots(), bound_knots(), FALSE)
        # compute forecast error
        errors <- rbind(errors, data.frame(name = "1st week, \nref \nmodel", 
                                           value = rmse(predlag_forecast[day_start:(day_start+7-1),"predictions"], 
                                                        data$hosp_0[day_start:(day_start+7-1)]),
                                           avg_temp = mean(data$temp_0[day_start:(day_start+7-1)])))
        errors <- rbind(errors, data.frame(name = "2nd week, \nref \nmodel", 
                                           value = rmse(predlag_forecast[(day_start+7):(day_start+14-1),"predictions"], 
                                                        data$hosp_0[(day_start+7):(day_start+14-1)]),
                                           avg_temp = mean(data$temp_0[(day_start+7):(day_start+14-1)])))
        
        # make forecast without temperature as predictor
        if (model_wo_temp()[[1]] == TRUE) {
          
          predlag_forecast <- k_step_forecast(forecast_data, day_start, window - 7, model_wo_temp()[[2]], 
                                              input_spline_type, tknots(), bound_knots(), FALSE)
          # compute forecast error
          errors <- rbind(errors, data.frame(name = "1st week, \nmodel \nw/o temp", 
                                             value = rmse(predlag_forecast[day_start:(day_start+7-1),"predictions"], 
                                                          data$hosp_0[day_start:(day_start+7-1)]),
                                             avg_temp = mean(data$temp_0[day_start:(day_start+7-1)])))
          errors <- rbind(errors, data.frame(name = "2nd week, \nmodel \nw/o temp",
                                             value = rmse(predlag_forecast[(day_start+7):(day_start+14-1),"predictions"], 
                                                          data$hosp_0[(day_start+7):(day_start+14-1)]), 
                                             avg_temp = mean(data$temp_0[(day_start+7):(day_start+14-1)])))
        }
      } 
      else if ((window - 14) <= length(data$date)) {
        # make forecast
        predlag_forecast <- k_step_forecast(forecast_data, day_start, window - 14, model(), 
                                            input_spline_type, tknots(), bound_knots(), FALSE)
        # compute forecast error
        errors <- rbind(errors, data.frame(name = "1st week, \nref \nmodel", 
                                           value = rmse(predlag_forecast[day_start:(day_start+7-1),"predictions"], 
                                                        data$hosp_0[day_start:(day_start+7-1)]),
                                           avg_temp = mean(data$temp_0[day_start:(day_start+7-1)])))
        
        # make forecast without temperature as predictor
        if (model_wo_temp()[[1]] == TRUE) {
          
          predlag_forecast <- k_step_forecast(forecast_data, day_start, window - 14, model_wo_temp()[[2]], 
                                              input_spline_type, tknots(), bound_knots(), FALSE)
          # compute forecast error
          errors <- rbind(errors, data.frame(name = "1st week, \nmodel \nw/o temp", 
                                             value = rmse(predlag_forecast[day_start:(day_start+7-1),"predictions"], 
                                                          data$hosp_0[day_start:(day_start+7-1)]),
                                             avg_temp = mean(data$temp_0[day_start:(day_start+7-1)])))
        }
      }
      else {
        break
      }
      
    }
    
    errors$name <- factor(errors$name, levels = c(
      "1st week, \nref \nmodel",
      "1st week, \nmodel \nw/o temp",
      "2nd week, \nref \nmodel",
      "2nd week, \nmodel \nw/o temp",
      "3rd week, \nref \nmodel",
      "3rd week, \nmodel \nw/o temp"
    ))
    
    #write.csv(errors, file = "errors.csv", row.names = FALSE)
    
    errors
  })
  
  # box plot of forecast errors for the reference model
  output$forecastErrorBoxPlotSimple <- renderPlot({
    
    withProgress(message = "Computing...", value = 0, {
      
      errors <- errors()
      
      # filter on reference model with temperture
      errors <- errors[errors$name %in% c("1st week, \nref \nmodel", 
                                          "2nd week, \nref \nmodel",
                                          "3rd week, \nref \nmodel"),]
      
      # change name removing ", \nref \nmodel"
      errors$name <- gsub(", \nref \nmodel", "", errors$name)
      
      # make a box plot
      errors %>%
        ggplot( aes(x=name, y=value, fill=name)) +
        geom_boxplot() +
        scale_fill_viridis(discrete = TRUE, alpha=0.6) +
        ggtitle(paste0("Number of '1st week' windows: ", sum(errors$name == "1st week"), "\n",
                       "Number of '2nd week' windows: ", sum(errors$name == "2nd week"), "\n",
                       "Number of '3rd week' windows: ", sum(errors$name == "3rd week"))) +
        geom_jitter(color="black", size=0.4, alpha=0.9) +
        xlab("forecast horizon") +
        ylab("RMSE") + 
        theme_minimal() + 
        theme(legend.position = "none",
              panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
              axis.text.y = element_text(size = 16),    # Y-axis tick labels
              axis.text.x = element_text(size = 14),    # separate control for x-axis     
              axis.title = element_text(size = 18),      
              plot.title = element_text(size = 14))  
    })
    
  })
  
  # box plot of forecast errors for the reference model, and comparison with model without temperature
  output$forecastErrorBoxPlot <- renderPlot({
    
    withProgress(message = "Computing...", value = 0, {
      
      errors <- errors()
      
      # make a box plot
      errors %>%
        ggplot( aes(x=name, y=value, fill=name)) +
        geom_boxplot() +
        scale_fill_viridis(discrete = TRUE, alpha=0.6) +
        ggtitle(paste0("Number of '1st week' windows: ", sum(errors$name == "1st week, \nref \nmodel"), "\n",
                       "Number of '2nd week' windows: ", sum(errors$name == "2nd week, \nref \nmodel"), "\n",
                       "Number of '3rd week' windows: ", sum(errors$name == "3rd week, \nref \nmodel"))) +
        geom_jitter(color="black", size=0.4, alpha=0.9) +
        xlab("forecast horizon") +
        ylab("RMSE") + 
        theme_minimal() + 
        theme(legend.position = "none",
              panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
              axis.text.y = element_text(size = 16),    # Y-axis tick labels
              axis.text.x = element_text(size = 12),    # separate control for x-axis        
              axis.title = element_text(size = 18),      
              plot.title = element_text(size = 14))  
    })
    
  })
  
  # box plot of forecast errors on selected windows, with temperature above a given threshold
  output$forecastErrorBoxPlotSelectedWindows <- renderPlot({
    
    withProgress(message = "Computing...", value = 0, {
    
      if (is.na(input$threshold) == FALSE) {
        
        errors <- errors()
        
        # filter on rows with avg_temp > threshold
        errors <- errors[errors$avg_temp > input$threshold,]
        
        # make a box plot
        errors %>%
          ggplot( aes(x=name, y=value, fill=name)) +
          geom_boxplot() +
          scale_fill_viridis(discrete = TRUE, alpha=0.6) +
          ggtitle(paste0("Number of '1st week' windows: ", sum(errors$name == "1st week, \nref \nmodel"), "\n",
                         "Number of '2nd week' windows: ", sum(errors$name == "2nd week, \nref \nmodel"), "\n",
                         "Number of '3rd week' windows: ", sum(errors$name == "3rd week, \nref \nmodel"))) +
          geom_jitter(color="black", size=0.4, alpha=0.9) +
          xlab("forecast horizon") +
          ylab("RMSE") + 
          theme_minimal() + 
          theme(legend.position = "none",
                panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
                axis.text.y = element_text(size = 16),    # Y-axis tick labels
                axis.text.x = element_text(size = 12),    # separate control for x-axis         
                axis.title = element_text(size = 18),      
                plot.title = element_text(size = 14))  
        
      }
      
    })
  }) 
  
  # compute forecast errors with cross validation over the years
  errors_cross_validation <- reactive({   
    
    data_total <- rbind(data_train(), data_test())
    
    errors <- data.frame(name = character(), value = numeric(), avg_temp = numeric(), year_cv = numeric())
    
    # for each year in the possible years in the dataframe
    # create a dataframe with the training set and a dataframe with the test set
    # the test set is the data from the year in question
    # the training set is the data from all other years
    for (y in unique(data_total$year)) {
      
      # create a dataframe with the training set
      data_train_cv <- data_total[data_total$year != y,]
      
      # create a dataframe with the test set
      data_test_cv <- data_total[data_total$year == y,]
      
      # fit the model to the new training set
      model_cv <- build_model(data_train_cv, 
                              input_family, 
                              dof(), input_spline_type, tknots(), bound_knots(), input$maxlag, 
                              input$past.obs, input$day.of.week.hol, 
                              input$pandemic)
      
      # fit model without temperature
      model_wo_temp_cv <- build_model_wo_temp(data_train_cv, input_family, 
                                              input$past.obs, input$day.of.week.hol, 
                                              input$pandemic)
      
      
      data <- data_test_cv
      
      # add spline basis to test set
      if (input_spline_type=="Natural (cubic) splines") {
        for (v in c("temp_0", 
                    "temp_minus_1", "temp_minus_2",
                    "temp_minus_3", "temp_minus_4", 
                    "temp_minus_5", "temp_minus_6", "temp_minus_7")) {
          s <- ns(as.numeric(unlist(data[,v])),  
                  knots=tknots(), Boundary.knots=bound_knots()) 
          colnames(s) <- paste0(v, "_bs_", colnames(s))
          data <- cbind(data, s)
        }
      }
      else{
        stop("Error: only natural (cubic) splines are currently supported.")
      }
      
      ##### k-step ahead forecasts
      # choose length of forecast window
      window_length <- 21
      
      # iterate over all possible windows with starting date between the 8th day of the test set, and the final date  
      for (h in c(8:length(data$date))) {
        
        forecast_data <- data
        
        day_start <- h
        
        window <- day_start + window_length - 1
        
        if (window <= length(data$date)) {
          # make forecast
          predlag_forecast <- k_step_forecast(forecast_data, day_start, window, model_cv, 
                                              input_spline_type, tknots(), bound_knots(), FALSE)
          
          # compute forecast error
          errors <- rbind(errors, data.frame(name = "1st week, \nref \nmodel", 
                                             value = rmse(predlag_forecast[day_start:(day_start+7-1),"predictions"], 
                                                          data$hosp_0[day_start:(day_start+7-1)]),
                                             avg_temp = mean(data$temp_0[day_start:(day_start+7-1)]),
                                             year_cv = y))
          errors <- rbind(errors, data.frame(name = "2nd week, \nref \nmodel", 
                                             value = rmse(predlag_forecast[(day_start+7):(day_start+14-1),"predictions"], 
                                                          data$hosp_0[(day_start+7):(day_start+14-1)]),
                                             avg_temp = mean(data$temp_0[(day_start+7):(day_start+14-1)]),
                                             year_cv = y))
          errors <- rbind(errors, data.frame(name = "3rd week, \nref \nmodel", 
                                             value = rmse(predlag_forecast[(day_start+14):(day_start+21-1),"predictions"],
                                                          data$hosp_0[(day_start+14):(day_start+21-1)]),
                                             avg_temp = mean(data$temp_0[(day_start+14):(day_start+21-1)]),
                                             year_cv = y))
          
          # make forecast without temperature as predictor
          if (model_wo_temp_cv[[1]] == TRUE) {
            
            predlag_forecast <- k_step_forecast(forecast_data, day_start, window, model_wo_temp_cv[[2]], 
                                                input_spline_type, tknots(), bound_knots(), FALSE)
            # compute forecast error
            errors <- rbind(errors, data.frame(name = "1st week, \nmodel \nw/o temp", 
                                               value = rmse(predlag_forecast[day_start:(day_start+7-1),"predictions"], 
                                                            data$hosp_0[day_start:(day_start+7-1)]),
                                               avg_temp = mean(data$temp_0[day_start:(day_start+7-1)]),
                                               year_cv = y))
            errors <- rbind(errors, data.frame(name = "2nd week, \nmodel \nw/o temp",
                                               value = rmse(predlag_forecast[(day_start+7):(day_start+14-1),"predictions"], 
                                                            data$hosp_0[(day_start+7):(day_start+14-1)]),
                                               avg_temp = mean(data$temp_0[(day_start+7):(day_start+14-1)]),
                                               year_cv = y))
            errors <- rbind(errors, data.frame(name = "3rd week, \nmodel \nw/o temp",
                                               value = rmse(predlag_forecast[(day_start+14):(day_start+21-1),"predictions"], 
                                                            data$hosp_0[(day_start+14):(day_start+21-1)]),
                                               avg_temp = mean(data$temp_0[(day_start+14):(day_start+21-1)]),
                                               year_cv = y))
          }
        }
        else if ((window - 7) <= length(data$date)) {
          # make forecast
          predlag_forecast <- k_step_forecast(forecast_data, day_start, window - 7, model_cv, 
                                              input_spline_type, tknots(), bound_knots(), FALSE)
          # compute forecast error
          errors <- rbind(errors, data.frame(name = "1st week, \nref \nmodel", 
                                             value = rmse(predlag_forecast[day_start:(day_start+7-1),"predictions"], 
                                                          data$hosp_0[day_start:(day_start+7-1)]),
                                             avg_temp = mean(data$temp_0[day_start:(day_start+7-1)]),
                                             year_cv = y))
          errors <- rbind(errors, data.frame(name = "2nd week, \nref \nmodel", 
                                             value = rmse(predlag_forecast[(day_start+7):(day_start+14-1),"predictions"], 
                                                          data$hosp_0[(day_start+7):(day_start+14-1)]),
                                             avg_temp = mean(data$temp_0[(day_start+7):(day_start+14-1)]),
                                             year_cv = y))
          
          # make forecast without temperature as predictor
          if (model_wo_temp_cv[[1]] == TRUE) {
            
            predlag_forecast <- k_step_forecast(forecast_data, day_start, window - 7, model_wo_temp_cv[[2]], 
                                                input_spline_type, tknots(), bound_knots(), FALSE)
            # compute forecast error
            errors <- rbind(errors, data.frame(name = "1st week, \nmodel \nw/o temp", 
                                               value = rmse(predlag_forecast[day_start:(day_start+7-1),"predictions"], 
                                                            data$hosp_0[day_start:(day_start+7-1)]),
                                               avg_temp = mean(data$temp_0[day_start:(day_start+7-1)]),
                                               year_cv = y))
            errors <- rbind(errors, data.frame(name = "2nd week, \nmodel \nw/o temp",
                                               value = rmse(predlag_forecast[(day_start+7):(day_start+14-1),"predictions"], 
                                                            data$hosp_0[(day_start+7):(day_start+14-1)]), 
                                               avg_temp = mean(data$temp_0[(day_start+7):(day_start+14-1)]),
                                               year_cv = y))
          }
        } 
        else if ((window - 14) <= length(data$date)) {
          # make forecast
          predlag_forecast <- k_step_forecast(forecast_data, day_start, window - 14, model_cv, 
                                              input_spline_type, tknots(), bound_knots(), FALSE)
          # compute forecast error
          errors <- rbind(errors, data.frame(name = "1st week, \nref \nmodel", 
                                             value = rmse(predlag_forecast[day_start:(day_start+7-1),"predictions"], 
                                                          data$hosp_0[day_start:(day_start+7-1)]),
                                             avg_temp = mean(data$temp_0[day_start:(day_start+7-1)]),
                                             year_cv = y))
          
          # make forecast without temperature as predictor
          if (model_wo_temp_cv[[1]] == TRUE) {
            
            predlag_forecast <- k_step_forecast(forecast_data, day_start, window - 14, model_wo_temp_cv[[2]], 
                                                input_spline_type, tknots(), bound_knots(), FALSE)
            # compute forecast error
            errors <- rbind(errors, data.frame(name = "1st week, \nmodel \nw/o temp", 
                                               value = rmse(predlag_forecast[day_start:(day_start+7-1),"predictions"], 
                                                            data$hosp_0[day_start:(day_start+7-1)]),
                                               avg_temp = mean(data$temp_0[day_start:(day_start+7-1)]),
                                               year_cv = y))
          }
        }
        else {
          break
        }
        
      }
      
    }
    
    errors$name <- factor(errors$name, levels = c(
      "1st week, \nref \nmodel",
      "1st week, \nmodel \nw/o temp",
      "2nd week, \nref \nmodel",
      "2nd week, \nmodel \nw/o temp",
      "3rd week, \nref \nmodel",
      "3rd week, \nmodel \nw/o temp"
    ))
    
    #write.csv(errors, file = "errors_cv.csv", row.names = FALSE)
    
    errors
  })
  
  output$forecastErrorBoxPlotCrossValidation <- renderPlot({
    
    withProgress(message = "Computing... \n This analysis may take long, please be patient.", value = 0, {
      
      errors <- errors_cross_validation()
      
      # make a box plot
      errors %>%
        ggplot( aes(x=name, y=value, fill=name)) +
        geom_boxplot() +
        scale_fill_viridis(discrete = TRUE, alpha=0.6) +
        ggtitle(paste0("Number of '1st week' windows: ", sum(errors$name == "1st week, \nref \nmodel"), "\n",
                       "Number of '2nd week' windows: ", sum(errors$name == "2nd week, \nref \nmodel"), "\n",
                       "Number of '3rd week' windows: ", sum(errors$name == "3rd week, \nref \nmodel"))) +
        geom_jitter(color="black", size=0.4, alpha=0.2) +
        xlab("forecast horizon") +
        ylab("RMSE") + 
        theme_minimal() + 
        theme(legend.position = "none",
              panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
              axis.text.y = element_text(size = 16),    # Y-axis tick labels
              axis.text.x = element_text(size = 12),    # separate control for x-axis       
              axis.title = element_text(size = 18),      
              plot.title = element_text(size = 14))  
    })
    
  })
  
  output$forecastErrorBoxPlotCrossValidationSelectedWindows <- renderPlot({
    
    withProgress(message = "Computing... \n This analysis may take long, please be patient.", value = 0, {
      
      if (is.na(input$threshold_cv) == FALSE) {
        
        errors <- errors_cross_validation()
        
        # filter on rows with avg_temp > threshold
        errors <- errors[errors$avg_temp > input$threshold_cv,]
        
        # make a box plot
        errors %>%
          ggplot( aes(x=name, y=value, fill=name)) +
          geom_boxplot() +
          scale_fill_viridis(discrete = TRUE, alpha=0.6) +
          ggtitle(paste0("Number of '1st week' windows: ", sum(errors$name == "1st week, \nref \nmodel"), "\n",
                         "Number of '2nd week' windows: ", sum(errors$name == "2nd week, \nref \nmodel"), "\n",
                         "Number of '3rd week' windows: ", sum(errors$name == "3rd week, \nref \nmodel"))) +
          geom_jitter(color="black", size=0.4, alpha=0.2) +
          xlab("forecast horizon") +
          ylab("RMSE") + 
          theme_minimal() + 
          theme(legend.position = "none",
                panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
                axis.text.y = element_text(size = 16),    # Y-axis tick labels
                axis.text.x = element_text(size = 12),    # separate control for x-axis         
                axis.title = element_text(size = 18),      
                plot.title = element_text(size = 14))  
        
      }
      
    })
    
  })
  
  # SECTION 3: FORECASTS  ############################################################################
  
  ## retrospective scenarios on test set ##########################################################################
  observe({
    # Update the default of the start date input based on the test data
    if (input$mode_temperature == "Test set") {
      
      tryCatch({
        req(input$window_length)
        
        start_date_default <- as.character(data_test()[15, 'date'])
        min_date <- as.character(data_test()[7, 'date'])
        max_date <- as.character(data_test()[length(data_test()$date)-input$window_length, 'date'])
        
        # Preserve user-selected value if it's within the new range
        current_value <- input$start_date_forecast
        if (is.null(current_value)) {
          new_value <-start_date_default
        }
        else {
          if (current_value >= min_date && current_value <= max_date) {
            new_value <- current_value
          }
          else {
            new_value <-start_date_default
          }
        }
        
        updateDateInput(
          session,
          inputId = "start_date_forecast",
          value = new_value,  # Set the default value dynamically
          #update min value
          min = min_date, 
          max = max_date)
        
      },
      # if the file has not been uploaded and we cannot read it, we throw an error
      error = function(e) {
        # Optional: show an error in the UI
        showNotification("Waiting for file to be uploaded...", type = "error")
        
      })
    }
  })
  
  drawRetrospectiveForecastPlot <- function () {
    
      # Define the layout: 1 plot in the first row, 2 plots in the second row
      layout(matrix(c(1, 1, 
                      2, 3), 
                    nrow = 2, byrow = TRUE), 
             heights = c(1.2, 1))
      
      par(mar = c(3, 5, 1, 12))
      
      ##### PLOT 1 - line plot with forecast
      
      # starting date for forecast
      day_start <- as.Date(input$start_date_forecast, format="%Y-%m-%d") 
      
      data <- data_test()
      
      window_length <- input$window_length
      
      # filter data around the starting date of the forecast window
      data <- subset(data, as.Date(date) >= day_start - 14)
      data <- subset(data, as.Date(date) <= day_start + input$window_length - 1)
      
      # check that length data$date > day_start is at least long as window length
      validate(
        need(( !is.na(input$start_date_forecast) && length(data$date) >= 14 + window_length ), 
             paste0("Restoring to default starting date..."))
      )
      
      # add spline basis 
      if (input_spline_type=="Natural (cubic) splines") {
        for (v in c("temp_0", 
                    "temp_minus_1", "temp_minus_2",
                    "temp_minus_3", "temp_minus_4", 
                    "temp_minus_5", "temp_minus_6", "temp_minus_7")) {
          s <- ns(as.numeric(unlist(data[,v])),  
                  knots=tknots(), Boundary.knots=bound_knots()) 
          colnames(s) <- paste0(v, "_bs_", colnames(s))
          data <- cbind(data, s)
        }
      }
      else{
        stop("Error: only natural (cubic) splines are currently supported.")
      }
      
      # get index of day_start in dataframe data
      day_start_index <-  which(data$date == day_start)
      
      pred_test <- predict(model(), newdata=data, type="response")
      
      ##### k-step ahead forecast
      window <- day_start_index + window_length - 1
      
      col_scenario <- "blue"
      
      # apply k_step_forecast function
      predlag_scenario <- k_step_forecast_uncertainty(data, day_start_index, window, model(), 
                                           input_spline_type, tknots(), bound_knots(), TRUE)
      
      predlag_scenario_cum_unc <- k_step_forecast_uncertainty_cumulative(data, day_start_index, day_start_index+6, model(), 
                                                          input_spline_type, tknots(), bound_knots(), TRUE)
      
      predlag_scenario[day_start_index-1,"predictions"] <- pred_test[day_start_index-1]

      #### make plot
      space = 0.1*(max(data$hosp_0, na.rm = TRUE)-min(data$hosp_0, na.rm = TRUE))
      
      plot(data$date[c(1:(day_start_index - 1))], data$hosp_0[c(1:(day_start_index - 1))], xlab="", pch = 16,
           main="", 
           ylab="hospital visits",
           cex.main=1.4, #change font size of title
           cex.lab=1.5, #change font size of axis labels
           cex.axis=1.4, #change font size of axis text 
           xlim=c(data$date[1], data$date[nrow(data)]),
           ylim=c(min(predlag_scenario[,"lwr_90"], na.rm = TRUE)-space, 
                  max(predlag_scenario[,"upr_90"], na.rm = TRUE)+space)) 
      
      points(data$date[c(day_start_index:nrow(data))], data$hosp_0[c(day_start_index:nrow(data))],   
             pch = 21, bg = "white", col = "black", cex = 1                       
      )
      
      lines(data$date[1:(day_start_index-1)], pred_test[1:(day_start_index-1)], col="black", lwd=2) 
      
      # add prediction interval 50% with ciTools
      polygon(c(data$date[c((day_start_index-1):window)], rev(data$date[c((day_start_index-1):window)])), 
              c(predlag_scenario[(day_start_index-1):window,"lwr_50"], rev(predlag_scenario[(day_start_index-1):window,"upr_50"])),
              col = rgb(0.31, 0.71, 0.71, 0.369), border = NA)
      
      # add prediction interval 90% with ciTools
      polygon(c(data$date[c((day_start_index-1):window)], rev(data$date[c((day_start_index-1):window)])), 
              c(predlag_scenario[(day_start_index-1):window,"lwr_90"], rev(predlag_scenario[(day_start_index-1):window,"upr_90"])),
              col = rgb(0.71, 0.71, 0.71, 0.369), border = NA)
      
      lines(data$date[c((day_start_index-1):window)], 
            predlag_scenario[(day_start_index-1):window,"predictions"], col=col_scenario, lwd=2, lty="dashed") 
      points(data$date[c((day_start_index):window)], 
             predlag_scenario[(day_start_index):window,"predictions"], col=col_scenario, pch=16, cex=1)
      
      legend(x = "topleft",
             inset = c(1.02, 0),  # shift legend outside plot 
             legend = c("data", paste0(window_length, "-days \nforecasts"), "50% PI", "90% PI"), 
             col = c("black", "blue", rgb(0.31, 0.71, 0.71, 0.369), rgb(0.71, 0.71, 0.71, 0.369)), 
             lty = c(NA, 2, NA, NA), 
             lwd = c(NA, 2, NA, NA),
             pch = c(16, NA, 15, 15), 
             pt.cex = 2, 
             cex = 1.4,
             pt.bg = c(NA, NA, rgb(0.31, 0.71, 0.71, 0.369), rgb(0.71, 0.71, 0.71, 0.369)), 
             bty="n",
             xpd = TRUE)
      
      # PLOT 2: temperature input
      
      par(mar = c(3, 5, 1, 2))
      h<-0
      input_temperature <- data.frame(date = data$date)
      input_temperature[[paste0("scenario_", h)]] <- data[,input$temp_indicator]
      
      plot(data$date[c(1:(day_start_index-1))], data$temp_0[c(1:(day_start_index-1))], 
           xlab="", main="", ylab="temperature (°C)", pch=16, 
           cex.main=1.4, #change font size of title
           cex.lab=1.5, #change font size of axis labels
           cex.axis=1.4, #change font size of axis text 
           xlim=c(data$date[1], data$date[nrow(data)]), 
           ylim=c(min(sapply(input_temperature, function(x) if(is.numeric(x)) min(x, na.rm=TRUE) else NA), na.rm=TRUE), 
                  max(sapply(input_temperature, function(x) if(is.numeric(x)) max(x, na.rm=TRUE) else NA), na.rm=TRUE)))
      
      lines(data$date[c(1:(day_start_index-1))], data$temp_0[c(1:(day_start_index-1))], lwd=2)
      
      lines(input_temperature$date[c((day_start_index-1):window)], 
            input_temperature[c((day_start_index-1):window), paste0("scenario_", h)], 
            col=col_scenario, lwd=2, lty="dashed")
      points(input_temperature$date[c((day_start_index):window)], 
            input_temperature[c((day_start_index):window), paste0("scenario_", h)], 
            col=col_scenario, pch=16)
      
      # PLOT 3: barplot of cumulative counts
      
      par(mar = c(3, 5, 1, 2))
      
      counts <- c(sum(data[(day_start_index-14):(day_start_index-8), "hosp_0"]),
                  sum(data[(day_start_index-7):(day_start_index-1), "hosp_0"]),
                  sum(predlag_scenario[day_start_index:(day_start_index+7-1),"predictions"]))
      
      names(counts) <- c("last week", 
                         "current week",
                         "next week")
      
      bp <- barplot(counts,  
                    ylab = "weekly hospital visits", 
                    beside=TRUE, 
                    ylim=range(pretty(c(0.95*min(counts, predlag_scenario_cum_unc[,'lwr_90_cum']),
                                        1.01*max(counts, predlag_scenario_cum_unc[,'upr_90_cum'])))),
                    col = "white", 
                    border=c( "black", "black", "black"),
                    cex.names = 1.0,
                    cex.lab=1.5,  #change font size of axis labels
                    cex.axis=1.4) #change font size of axis text)
      
      arrows(x0 = bp[3], y0 = predlag_scenario_cum_unc[,'lwr_90_cum'],
             x1 = bp[3], y1 = predlag_scenario_cum_unc[,'upr_90_cum'],
             angle = 90, code = 3, length = 0.1, col = "darkgray", lwd = 1.5)
      arrows(x0 = bp[3], y0 = predlag_scenario_cum_unc[,'lwr_50_cum'],
             x1 = bp[3], y1 = predlag_scenario_cum_unc[,'upr_50_cum'],
             angle = 90, code = 3, length = 0.1, col = "lightblue", lwd = 1.5)
      
      points(bp[1], counts[1], cex = 2, pch = 16, col="black")
      points(bp[2], counts[2], cex = 2, pch = 16, col="black")
      points(bp[3], counts[3], cex = 2, pch = 16, col="blue")
      
      text(bp-0.25, counts + counts*0.01, 
           labels = round(counts), cex = 1.0)
      
      bp
    
    }

  output$renderRetrospectiveForecastPlot <- renderPlot({
    withProgress(message = "Computing...", value = 0, {
      drawRetrospectiveForecastPlot()
    })
  })
  
  output$downloadRetrospectiveForecastPlot <- downloadHandler(
    filename = function() {
      "3a_retrospective_forecast_plot.png"
    },
    content = function(file) {
      png(file, width = 9, height = 6, units = "in", res = 300)
      drawRetrospectiveForecastPlot()
      dev.off()
    }
  )
  
  ## customized scenario with temperature from external file ############################################################################
  
  ## if user uploads his dataset
  ## we store it in the data folder
  observeEvent(input$file_temperature, {
    req(input$file_temperature)  # Ensure a file is uploaded
    
    # Define file path
    file_temperature_path <- file.path("data", input$file_temperature$name)
    
    # Save the uploaded file to the "data" folder
    file.copy(input$file_temperature$datapath, file_temperature_path, overwrite = TRUE)
    
    # Display the saved file path
    output$file_temperature_path <- renderText({
      paste("File saved to:", file_temperature_path)
    })
  })
  
  # display table of template data
  output$viewDataRawTemplateForecast <- DT::renderDataTable({
    # read dataframe contained in "data" folder, called public_austin.csv
    df <- read.csv("data/forecast_temperature_dataset_template.csv", sep=';')
    # display table
    datatable(df, class = 'compact',
              options = list(scrollX = FALSE, lengthChange = FALSE, pageLength = 3, searching = FALSE,  
                             info = FALSE, paging = FALSE))
  })
  
  # Render help panel only when file is not uploaded
  output$file_upload_help_panel <- renderUI({
    if (is.null(input$file_temperature)) {
      fluidRow(
        column(6,         
               p(""),
               p("Upload your own data in CSV format, following the template on the right."),
               p("Include at least three columns ('date', 'hosp_counts' and temperature as in SECTION 1)."),
               p("Include at least 7 days of observed data in the past, for both temperature and hospital visits."),
               p("Include at least 7 days of temperature data in the future. Column with future hospital visits shall contain null values."),
               p("The starting date of the forecast will be set by default at the first date with missing hospital data.")
        ),
        column(6, DTOutput("viewDataRawTemplateForecast"))
      )
    } else {
      NULL
    }
  })
  
  ## read raw temperature data for forecasts
  data_raw_temperature <- reactive({
    
    if (input$mode_temperature == "New data (upload)") {
      ## see data_read() function in the script functions.R
      data_raw_temperature <- data_read_temperature(input$mode_temperature, input$file_temperature$datapath)
      
      # check that data_raw includes at least three columns named "date", "hosp_counts", and "mean_temp"
      if (!all(c("date", "hosp_counts") %in% colnames(data_raw_temperature))) {
        stop("The dataset must include columns named 'date', 'hosp_counts'.")
      }
      
      # check that data_raw contains continuous dates, and there are no missing dates
      if (check_continuous_dates(data_raw_temperature)==FALSE) {
        stop("The dataset must include continuous dates without missing dates.")
      }
      
      data_raw_temperature
    }

  })
  
  # display table in the user interface to inspect the dataset
  output$viewDataRawTemperature <- DT::renderDataTable({
    df <- data_raw_temperature()
    datatable(df, options = list(scrollX = TRUE, pageLength = 14, searching = FALSE))
  })

  drawNewForecastPlot <- function () {
    
    if (input$mode_temperature == "New data (upload)") {
    
      withProgress(message = "Computing...", value = 0, {
        
        # Define the layout: 1 plot in the first row, 2 plots in the second row
        layout(matrix(c(1, 1, 
                        2, 3), 
                      nrow = 2, byrow = TRUE), 
               heights = c(1.2, 1))
        
        par(mar = c(3, 5, 1, 12))
        
        ##### PLOT 1 - line plot with forecast
        
        # starting date for forecast, we set it equal to the first date for which hospital data are missing
        
        # Find the first index where 'hosp_counts' is NA
        day_start_index <- which(is.na(data_raw_temperature()$hosp_counts))[1]
        # Get the corresponding date
        day_start <- data_raw_temperature()$date[day_start_index]
        
        # filter data_raw for date > day_start - 7
        data <- data_raw_temperature()[, c("date", "hosp_counts", input$temp_indicator)]
        
        if (nrow(subset(data, as.Date(date) < day_start)) < 7) {
          stop("Include at least 7 days in the past.")
        }
        
        window_length <- nrow(subset(data, as.Date(date) >= day_start))
        
        if (window_length < 7) {
          stop("Include at least 7 days of expected temperature.")
        }
        
        # apply function for data preprocessing
        data <- data_preprocessing(data = data, 
                                   temp_col = input$temp_indicator, 
                                   input_months = input$months, 
                                   input_data = input$data,
                                   country = input$country,
                                   canton = input$canton, 
                                   date_period_of_interest = input$date_period_of_interest) 
        
        # add spline basis 
        if (input_spline_type=="Natural (cubic) splines") {
          for (v in c("temp_0", 
                      "temp_minus_1", "temp_minus_2",
                      "temp_minus_3", "temp_minus_4", 
                      "temp_minus_5", "temp_minus_6", "temp_minus_7")) {
            s <- ns(as.numeric(unlist(data[,v])),  
                    knots=tknots(), Boundary.knots=bound_knots()) 
            colnames(s) <- paste0(v, "_bs_", colnames(s))
            data <- cbind(data, s)
          }
        }
        else{
          stop("Error: only natural (cubic) splines are currently supported.")
        }
        
        # get index of day_start in dataframe data
        day_start_index <-  which(data$date == day_start)
        
        ##### k-step ahead forecast
        window <- day_start_index + window_length - 1
        
        col_scenario <- "blue"
        
        input_temperature <- data.frame(date = data$date)
        
        h<-0
        h<-h+1
        scenario_data <- data[, c("date", "hosp_counts", input$temp_indicator)]
        input_temperature[[paste0("scenario_", h)]] <- scenario_data[,input$temp_indicator]
  
        scenario_data <- data_preprocessing(data = scenario_data, 
                                            temp_col = input$temp_indicator, 
                                            input_months = input$months, 
                                            input_data = input$data,
                                            country = input$country,
                                            canton = input$canton, 
                                            date_period_of_interest = date_period_of_interest()) 
        
        # apply k_step_forecast function
        predlag_scenario<- k_step_forecast_uncertainty(scenario_data, day_start_index, window, model(), 
                                           input_spline_type, tknots(), bound_knots(), TRUE)
        
        predlag_scenario_cum_unc <- k_step_forecast_uncertainty_cumulative(scenario_data, day_start_index, day_start_index+6, model(), 
                                                                           input_spline_type, tknots(), bound_knots(), TRUE)
        
        space = 0.1*(max(data$hosp_0, na.rm = TRUE)-min(data$hosp_0, na.rm = TRUE))
        
        if (nrow(subset(data, as.Date(date) < day_start)) < 14) {
          plot(data$date[c((day_start_index-7):nrow(data))], data$hosp_0[c((day_start_index-7):nrow(data))], xlab="", pch = 16,
               main="", 
               ylab="hospital visits",
               cex.main=1.4, 
               cex.lab=1.5, 
               cex.axis=1.4, 
               ylim=c(min(predlag_scenario[,"lwr_90"], na.rm = TRUE)-space, 
                      max(predlag_scenario[,"upr_90"], na.rm = TRUE)+space)) #change font size of axis text
          
          pred_test <- predict(model(), newdata=data, type="response")
          lines(data$date[(day_start_index-7):(day_start_index-1)], 
                pred_test[(day_start_index-7):(day_start_index-1)], col="black", lwd=2) 
          
        }
        else { 
          plot(data$date[c((day_start_index-14):nrow(data))], 
               data$hosp_0[c((day_start_index-14):nrow(data))], xlab="", pch = 16,
               main="", 
               ylab="hospital visits",
               cex.main=1.4, #change font size of title
               cex.lab=1.5, #change font size of axis labels
               cex.axis=1.4, 
               ylim=c(min(predlag_scenario[,"lwr_90"], na.rm = TRUE)-space, 
                      max(predlag_scenario[,"upr_90"], na.rm = TRUE)+space)) #change font size of axis text
          
          pred_test <- predict(model(), newdata=data, type="response")
          lines(data$date[(day_start_index-14):(day_start_index-1)], pred_test[(day_start_index-14):(day_start_index-1)], col="black", lwd=2) 
        }
        
        
        predlag_scenario[day_start_index-1,"predictions"] <- pred_test[day_start_index-1]
        
        # add prediction interval 50% with ciTools
        polygon(c(data$date[c((day_start_index-1):window)], rev(data$date[c((day_start_index-1):window)])), 
                c(predlag_scenario[(day_start_index-1):window,"lwr_50"], rev(predlag_scenario[(day_start_index-1):window,"upr_50"])),
                col = rgb(0.31, 0.71, 0.71, 0.369), border = NA)
        
        # add prediction interval 90% with ciTools
        polygon(c(data$date[c((day_start_index-1):window)], rev(data$date[c((day_start_index-1):window)])), 
                c(predlag_scenario[(day_start_index-1):window,"lwr_90"], rev(predlag_scenario[(day_start_index-1):window,"upr_90"])),
                col = rgb(0.71, 0.71, 0.71, 0.369), border = NA)
        
        lines(data$date[c((day_start_index-1):window)], 
              predlag_scenario[(day_start_index-1):window,"predictions"], col=col_scenario, lwd=2, lty="dashed") 
        points(data$date[c((day_start_index):window)], 
              predlag_scenario[(day_start_index):window,"predictions"], col=col_scenario, pch=16) 

        legend(x = "topleft",
               inset = c(1.02, 0),  # shift legend outside plot 
               legend = c("data", paste0(window_length, "-days \nforecasts"), "50% PI", "90% PI"), 
               col = c("black", "blue", rgb(0.31, 0.71, 0.71, 0.369), rgb(0.71, 0.71, 0.71, 0.369)), 
               lty = c(NA, 2, NA, NA), 
               lwd = c(NA, 2, NA, NA),
               pch = c(16, NA, 15, 15), 
               pt.cex = 2, 
               cex = 1.4,
               pt.bg = c(NA, NA, rgb(0.31, 0.71, 0.71, 0.369), rgb(0.71, 0.71, 0.71, 0.369)), 
               bty="n",
               xpd = TRUE)
        
        # PLOT 2: temperature input
        
        par(mar = c(3, 5, 1, 2))
        
        if (nrow(subset(data, as.Date(date) < day_start)) < 14) {
          plot(data$date[c((day_start_index-7):(day_start_index-1))], data$temp_0[c((day_start_index-7):(day_start_index-1))], 
             xlab="", main="", ylab="temperature (°C)", pch=16, 
             cex.main=1.4, #change font size of title
             cex.lab=1.5, #change font size of axis labels
             cex.axis=1.4, #change font size of axis text 
             xlim=c(data$date[day_start_index-7], data$date[nrow(data)]), 
             ylim=c(min(sapply(input_temperature, function(x) if(is.numeric(x)) min(x, na.rm=TRUE) else NA), na.rm=TRUE), 
                    max(sapply(input_temperature, function(x) if(is.numeric(x)) max(x, na.rm=TRUE) else NA), na.rm=TRUE)))
          lines(data$date[c((day_start_index-7):(day_start_index-1))], data$temp_0[c((day_start_index-7):(day_start_index-1))], lwd=2)
          
        }
        else {
          plot(data$date[c((day_start_index-14):(day_start_index-1))], data$temp_0[c((day_start_index-14):(day_start_index-1))], 
               xlab="", main="", ylab="temperature (°C)", pch=16, 
               cex.main=1.4, #change font size of title
               cex.lab=1.5, #change font size of axis labels
               cex.axis=1.4, #change font size of axis text 
               xlim=c(data$date[day_start_index-14], data$date[nrow(data)]), 
               ylim=c(min(sapply(input_temperature, function(x) if(is.numeric(x)) min(x, na.rm=TRUE) else NA), na.rm=TRUE), 
                      max(sapply(input_temperature, function(x) if(is.numeric(x)) max(x, na.rm=TRUE) else NA), na.rm=TRUE)))
          lines(data$date[c((day_start_index-14):(day_start_index-1))], data$temp_0[c((day_start_index-14):(day_start_index-1))], lwd=2)
          
        }
      
        for (h in c(1:3)){
          lines(input_temperature$date[c((day_start_index-1):window)], 
                input_temperature[c((day_start_index-1):window), paste0("scenario_", h)], 
                col=col_scenario, lwd=2, lty="dashed")
          points(input_temperature$date[c((day_start_index):window)], 
                input_temperature[c((day_start_index):window), paste0("scenario_", h)], 
                col=col_scenario, pch=16)
        }
        
        # PLOT 3: barplot of cumulative counts
        
        par(mar = c(3, 5, 1, 2))
        
        if (nrow(subset(data, as.Date(date) < day_start)) < 14) {
          counts <- c(NaN,
                      sum(data[(day_start_index-7):(day_start_index-1), "hosp_0"]),
                      sum(predlag_scenario[day_start_index:(day_start_index+7-1),"predictions"]))
        } 
        else {
          counts <- c(sum(data[(day_start_index-14):(day_start_index-8), "hosp_0"]),
                      sum(data[(day_start_index-7):(day_start_index-1), "hosp_0"]),
                      sum(predlag_scenario[day_start_index:(day_start_index+7-1),"predictions"]))
        }
        names(counts) <- c("last week", 
                           "current week",
                           "next week")
        
        
        bp <- barplot(counts,  
                      ylab = "weekly hospital visits", 
                      beside=TRUE, 
                      ylim=range(pretty(c(0.95*min(counts, predlag_scenario_cum_unc[,'lwr_90_cum'], na.rm=TRUE),
                                          1.01*max(counts, predlag_scenario_cum_unc[,'upr_90_cum'], na.rm=TRUE)))),
                      col = "white", 
                      border=c( "black", "black", "black"),
                      cex.names = 1.,
                      cex.lab=1.5,  #change font size of axis labels
                      cex.axis=1.4) #change font size of axis text)
        
        arrows(x0 = bp[3], y0 = predlag_scenario_cum_unc[,'lwr_90_cum'],
               x1 = bp[3], y1 = predlag_scenario_cum_unc[,'upr_90_cum'],
               angle = 90, code = 3, length = 0.1, col = "darkgray", lwd = 1.5)
        arrows(x0 = bp[3], y0 = predlag_scenario_cum_unc[,'lwr_50_cum'],
               x1 = bp[3], y1 = predlag_scenario_cum_unc[,'upr_50_cum'],
               angle = 90, code = 3, length = 0.1, col = "lightblue", lwd = 1.5)
        
        points(bp[1], counts[1], cex = 2, pch = 16, col="black")
        points(bp[2], counts[2], cex = 2, pch = 16, col="black")
        points(bp[3], counts[3], cex = 2, pch = 16, col="blue")

        text(bp-0.25, counts + counts*0.01, 
             labels = round(counts), cex = 1.0)
        
        bp
        
      })
    
    }
  }
  
  output$renderNewForecastPlot <- renderPlot({
    withProgress(message = "Computing...", value = 0, {
      drawNewForecastPlot()
    })
  })
  
  output$downloadNewForecastPlot <- downloadHandler(
    filename = function() {
      "3b_new_forecast_plot.png"
    },
    content = function(file) {
      png(file, width = 9, height = 6, units = "in", res = 300)
      drawNewForecastPlot()
      dev.off()
    }
  )
  
}