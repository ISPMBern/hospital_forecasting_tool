# this is an R script with a list of functions that are used in the server logic

# load libraries
library(DT) # to render tables
#library(tidyverse) # general library for data manipulation
library(lubridate) # for function wday()
library(tidyr) # for function complete()
library(dplyr) # for function filter()
library(ggplot2) # for plotting
library(ggpubr) # for function ggarrange()
library(zoo) # for computing rolling mean
library(timeDate) # for function holiday()
library(splines) # for function ns()
library(MASS) # for mvrnorm function
library(Epi) # to compute confidence intervals for regression coeff using ci.lin
library(matrixStats) # for rowQuantiles function
library(Metrics) # to compute rmse
library(ciTools) # to compute PREDICTION intervals
library(viridis) # for colors in boxplot

# function to read data
data_read <- function(input_data, path_data) {
  
  if (input_data == "Bern case study") { # if user chooses the Bern case study
    #input_type, input_clinic, input_age
    input_type <- "Visits"
    input_clinic <- "EmergencyDeptUni"
    input_age <- "All"
    
    path_data <- sprintf("./data/INSEL_daily_type%s_clinic%s_age%s.csv", 
                         input_type, 
                         input_clinic, 
                         input_age)
    
    first_line <- tryCatch(
      readLines(path_data, n = 1),  # Read the first line
      # if the file has not been uploaded and we cannot read it, we throw an error
      error = function(e) {
        stop("You do not have access to the data for the Bern case study. Please change the data source and upload your own data.")
      }
    )

  } else if (input_data == "Upload your own data") { ### if user chooses to upload their own dataset
    path_data <- path_data
    
    first_line <- tryCatch(
      readLines(path_data, n = 1),
      error = function(e) {
        return(list(error = "The file has not been uploaded yet. Please upload a file."))
      }
    )
    # If first_line is a list with an error, return it
    if (is.list(first_line) && !is.null(first_line$error)) return(first_line)
  }
  else {
    stop("Error in data source.") 
  }
  
  if (grepl(";", first_line)) {
    sep <- ";"  # Semicolon-separated
  } else if (grepl(",", first_line)) {
    sep <- ","  # Comma-separated
  } else if (grepl("\t", first_line)) {
    sep <- "\t" # Tab-separated
  } else {
    stop("Unsupported file format. Please upload a CSV file.")
  }
  
  data_raw <- read.csv(path_data, sep = sep, header=TRUE)
  
  # transfor the date column in date format
  data_raw$date <- as.Date(data_raw$date, format="%Y-%m-%d")

  data_raw
}

# check that data_raw contains continuous dates, and that there are no missing dates.
check_continuous_dates <- function(data) {
  # Ensure the date column is in Date format
  data$date <- as.Date(data$date, format="%Y-%m-%d")
  
  # Generate a complete sequence of dates from min to max
  full_dates <- seq(min(data$date, na.rm = TRUE), max(data$date, na.rm = TRUE), by = "day")
  
  # Find missing dates
  missing_dates <- setdiff(full_dates, data$date)
  
  if (length(missing_dates) > 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# function shift to create lagged columns
shift <- function(x,shift_by){
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  
  if (length(shift_by)>1)
    return(sapply(shift_by,shift, x=x))
  
  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
    out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out<-x
  out
}

swiss_canton_list_holidays_function <- function(selected_canton) {
  
  # Define list of cantons
  cantons <- c(
    "AG", "AI", "AR", "BE", "BL", "BS", "FR", "GE", "GL", "GR", "JU", "LU",
    "NE", "NW", "OW", "SG", "SH", "SO", "SZ", "TG", "TI", "UR", "VD", "VS",
    "ZG", "ZH"
  )
  
  # Define the holidays and where they apply
  holiday_list <- list(
    "NewYearsDay" = cantons,
    "CHBerchtoldsDay" = c("AG", "BE", "FR", "GL", "JU", "LU", "OW", "SG", "SH", "TG", "VD"),
    "Epiphany" = c("SZ", "TI", "UR"),
    "GoodFriday" = setdiff(cantons, c("TI", "VS")),
    "EasterMonday" = setdiff(cantons, c("NE", "SO", "VS", "ZG")),
    "Ascension" = cantons,
    "PentecostMonday" = setdiff(cantons, c("NE", "SO", "VS", "ZG")),
    "CorpusChristi" = c("AG", "AI", "FR", "JU", "LU", "NW", "OW", "SO", "SZ", "TI", "UR", "VS", "ZG"),
    "CHConfederationDay" = cantons,
    "AssumptionOfMary" = c("AG", "AI", "FR", "JU", "LU", "NW", "OW", "SO", "SZ", "TI", "UR", "VS", "ZG"),
    "AllSaints" = setdiff(cantons, c("AR", "BL", "BS", "BE", "GE", "GR", "NE", "SH", "TG", "VD", "ZH")),
    "ITImmaculateConception" = c("AG", "AI", "FR", "LU", "NW", "OW", "SZ", "TI", "UR", "VS", "ZG"),
    "ChristmasDay" = cantons,
    "BoxingDay" = setdiff(cantons, c("GE", "JU", "NE", "SO", "VS", "VD", "ZG"))
  )
  
  # Initialize an empty named list for columns
  canton_holidays <- setNames(vector("list", length(cantons)), cantons)
  
  # Fill in the holidays
  for (canton in cantons) {
    holidays <- names(Filter(function(c) canton %in% c, holiday_list))
    canton_holidays[[canton]] <- holidays
  }
  
  # Determine max length to align columns
  max_len <- max(sapply(canton_holidays, length))
  
  # Pad with NAs
  for (canton in cantons) {
    length(canton_holidays[[canton]]) <- max_len
  }
  
  # Convert to data frame
  holiday_df <- as.data.frame(canton_holidays, stringsAsFactors = FALSE)
  
  # Get holidays for the given canton, removing NAs
  selected_holidays <- holiday_df[[selected_canton]]
  selected_holidays <- selected_holidays[!is.na(selected_holidays)]
  
  return(selected_holidays)
}

country_list_holidays_function <- function(country) {
  
  holiday_df <- data.frame(
    Holiday = c(
      "NewYearsDay", "Epiphany", "GoodFriday", "Easter", "EasterMonday",
      "LaborDay", "Ascension", "PentecostMonday", "CorpusChristi",
      "CHConfederationDay", "AssumptionOfMary", "AllSaints", "ITImmaculateConception",
      "ChristmasDay", "BoxingDay"
    ),
    
    France = c(
      "NewYearsDay", NA, NA, "Easter", "EasterMonday",
      "LaborDay", "Ascension", "PentecostMonday", NA,
       NA, "AssumptionOfMary", "AllSaints", NA,
      "ChristmasDay", NA
    ),
    
    Switzerland = c(
      "NewYearsDay", NA, "GoodFriday", "Easter", "EasterMonday",
       NA, "Ascension", NA, NA,
      "CHConfederationDay", NA, NA, NA,
      "ChristmasDay", NA
    ),
    
    Germany = c(
      "NewYearsDay", NA, "GoodFriday", "Easter", "EasterMonday",
      "LaborDay", "Ascension", "PentecostMonday", NA,
       NA, NA, NA, NA,
      "ChristmasDay", "BoxingDay"
    ),
    
    Italy = c(
      "NewYearsDay", "Epiphany", NA, "Easter", "EasterMonday",
      "LaborDay", NA, NA, NA,
       NA, "AssumptionOfMary", "AllSaints", "ITImmaculateConception",
      "ChristmasDay", "BoxingDay"
    ),
    
    Spain = c(
      "NewYearsDay", "Epiphany", "GoodFriday", "Easter", NA,
      "LaborDay", NA, NA, NA,
       NA, "AssumptionOfMary", "AllSaints", "ITImmaculateConception",
      "ChristmasDay", NA
    ),
    
    Austria = c(
      "NewYearsDay", "Epiphany", NA, "Easter", "EasterMonday",
      "LaborDay", "Ascension", "PentecostMonday", "CorpusChristi",
       NA, "AssumptionOfMary", "AllSaints", "ITImmaculateConception",
      "ChristmasDay", "BoxingDay"
    ),
    
    Belgium = c(
      "NewYearsDay", NA, "GoodFriday", "Easter", "EasterMonday",
      "LaborDay", "Ascension", "PentecostMonday", "CorpusChristi",
       NA, "AssumptionOfMary", "AllSaints", NA,
      "ChristmasDay", NA
    ),
    
    Netherlands = c(
      "NewYearsDay", NA, "GoodFriday", "Easter", "EasterMonday",
      "LaborDay", "Ascension", "PentecostMonday", NA,
       NA, NA, "AllSaints", NA,
      "ChristmasDay", "BoxingDay"
    ),
    
    UnitedKingdom = c(
      "NewYearsDay", NA, "GoodFriday", "Easter", "EasterMonday",
      "LaborDay", NA, NA, NA,
       NA, NA, NA, NA,
      "ChristmasDay", "BoxingDay"
    ),
    
    stringsAsFactors = FALSE
  )
  
  # Get holidays for the given canton, removing NAs
  selected_holidays <- holiday_df[[country]]
  selected_holidays <- selected_holidays[!is.na(selected_holidays)]
  
  return(selected_holidays)
}

# function for data pre-processing
data_preprocessing <- function(data, temp_col, input_months, input_data, country = "Switzerland(canton)", canton="BE", 
                               date_period_of_interest = FALSE) {

  # add columns with time variables of interest
  data <- data %>% mutate(dow = wday(as.Date(date, format="%Y-%m-%d"), week_start=1),
                          year = year(as.Date(date, format="%Y-%m-%d")),
                          month = month(as.Date(date, format="%Y-%m-%d")),
                          dom = format(as.Date(date, format="%Y-%m-%d"), "%d"))
  
  ## workday
  data <- data %>% mutate(wd = ifelse(dow<=5, 1, 0))
  
  ## get list of public holidays
  if (input_data == "Bern case study") {
    list_holidays <- holiday(min(data$year):max(data$year), 
                             swiss_canton_list_holidays_function("BE"))
    
  }
  else if (input_data == "Upload your own data") {
    
    if (country == "Switzerland(canton)") {
      list_holidays <- holiday(min(data$year):max(data$year), 
                               swiss_canton_list_holidays_function(canton))
    }
    else {
      list_holidays <- holiday(min(data$year):max(data$year), 
                               country_list_holidays_function(country))
    }

  }
  else {
    stop("Error in data source.")
  }
    
  ## put workday wd=0 for holidays occurring during a weekday (dow<=5)
  data <- data %>% mutate(wd = ifelse(date %in% as.Date(list_holidays, format="%Y-%m-%d"), 0, wd))
  # create holiday column
  data <- data %>% mutate(holiday = ifelse(date %in% as.Date(list_holidays, format="%Y-%m-%d"), 1, 0))
  
  ### day of week factorized
  data <- data %>% mutate(dow_m = factor(dow, levels = 1:7, labels = c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN")))
  
  data <- data %>% 
    mutate(var = 1) %>%                                   # Put a 1 in all rows of a column
    spread(key = dow_m, value = var, fill = 0)            # Create a dummy variable
  
  ## make some adjustments to control for a regular weekday, i.e. a day that is not a bank holiday
  ## we want an indicator for a holiday on a WEEKDAY
  ## hence, if holiday = 1 but day of the week is Saturday or Sunday (SAT=1 or SUN=1), we shall correct with holiday = 0
  data <- data %>% mutate(wd_holiday = ifelse((holiday == 1 &  SUN == 1) | (holiday == 1 &  SAT == 1), 0, holiday))
  ## on the other hand, if the holiday lies on a weekday, we shall make the weekday tag = 0, because now MON indicates a regular MONDAY
  data <- data %>% mutate(reg_MON = ifelse((holiday == 1) & (MON == 1), 0, MON))
  data <- data %>% mutate(reg_TUE = ifelse((holiday == 1) & (TUE == 1), 0, TUE))
  data <- data %>% mutate(reg_WED = ifelse((holiday == 1) & (WED == 1), 0, WED))
  data <- data %>% mutate(reg_THU = ifelse((holiday == 1) & (THU == 1), 0, THU))
  data <- data %>% mutate(reg_FRI = ifelse((holiday == 1) & (FRI == 1), 0, FRI))
  
  ## during COVID-19
  data <- data %>% mutate(pandemic = ifelse((year %in% c(2020,2021,2022)), 1, 0))
  
  #### lagged hospitalizations
  data <- data %>% mutate(hosp_0 = hosp_counts, 
                          hosp_minus1 = shift(hosp_counts,-1), 
                          hosp_minus2 = shift(hosp_counts,-2), 
                          hosp_minus3 = shift(hosp_counts,-3), 
                          hosp_minus4 = shift(hosp_counts,-4),
                          hosp_minus5 = shift(hosp_counts,-5),
                          hosp_minus6 = shift(hosp_counts,-6),
                          hosp_minus7 = shift(hosp_counts,-7))
  
  ### lagged temperature
  data <- data %>% mutate(temp_0 = !!sym(temp_col),
                          temp_minus_1 = shift(!!sym(temp_col),-1),
                          temp_minus_2 = shift(!!sym(temp_col),-2),
                          temp_minus_3 = shift(!!sym(temp_col),-3),
                          temp_minus_4 = shift(!!sym(temp_col),-4),
                          temp_minus_5 = shift(!!sym(temp_col),-5),
                          temp_minus_6 = shift(!!sym(temp_col),-6),
                          temp_minus_7 = shift(!!sym(temp_col),-7))
  
  # select months of interest
  if (input_months == "Summer months (June - August)"){
    data <- subset(data, (month %in% c(6,7,8)))
  }
  else if (input_months == "Other"){
    start <- date_period_of_interest[1]
    end <- date_period_of_interest[2]

    data$monthday <- format(as.Date(data$date, format="%Y-%m-%d"), "%m-%d")

    data <- subset(data, monthday >= start)
    data <- subset(data, monthday <= end)
  }
  else {
    stop("Error in time period of interest.")
  }
  
  # if dataset is empty, raise an error to the user
  if (nrow(data) == 0) {
    stop("The dataset is empty. Please check that the data you are providing overlap with time period of interest chosen in Section 1, or adjust the time period of interest accordingly.")
  }

  data
}

# function to compute degrees of freedom of the splines
compute_dof <- function(input_spline_type, tknots) {
  if (input_spline_type == "B-splines") {
    stop("Error: B-splines are not supported in this version. You need to define input_degree.")
    dof <- as.numeric(input_degree) + length(tknots) # compute degree of freedom = degree + internal knots for B-splines 
  }
  else {
    if (input_spline_type == "Natural (cubic) splines") {
      dof <- length(tknots) + 1 # compute degree of freedom = internal knots + 1 for natural cubic spline
    }
    else {
      stop("Error in spline type.")
    }
  }
}
  
  
# function to define reference regression model
build_model <- function(data_train, input_family, 
                        dof, input_spline_type, tknots, bound_knots, input_maxlag, 
                        input_past.obs, input_day.of.week.hol, 
                        input_pandemic) {
  
  data <- data_train
  
  # apply spline functions to the column with the temperature
  if (input_spline_type == "Natural (cubic) splines") {
      for (v in c("temp_0", "temp_minus_1", "temp_minus_2", 
                  "temp_minus_3", "temp_minus_4", "temp_minus_5", 
                  "temp_minus_6", "temp_minus_7")) {
        s <- ns(as.numeric(unlist(data[,v])), 
                knots=tknots, Boundary.knots=bound_knots) 
        colnames(s) <- paste0(v, "_bs_", colnames(s))
        data <- cbind(data, s)
      }
  }
  else {
      stop("Error in spline type.")
    }
  
  # initialize model with splines for temperature
  maxlag <- input_maxlag
  
  if (dof == 3) { # we have three basis functions for the spline
    
    # start with maxlag ==1, i.e. add temp_0 and temp_minus_1
    model <- glm(formula = hosp_0 ~ temp_0_bs_1 + temp_0_bs_2 + temp_0_bs_3 + 
                                    temp_minus_1_bs_1 + temp_minus_1_bs_2 + temp_minus_1_bs_3,
                 data = data, family = input_family)  
    
    if (maxlag == 2) {
      ## add temperature lag 2
      model <- update(model, .~.+ temp_minus_2_bs_1 + temp_minus_2_bs_2 + temp_minus_2_bs_3)
    }
    if (maxlag == 3) {
      ## add temperature lag 2 and 3
      model <- update(model, .~.+ temp_minus_2_bs_1 + temp_minus_2_bs_2 + temp_minus_2_bs_3 +
                                  temp_minus_3_bs_1 + temp_minus_3_bs_2 + temp_minus_3_bs_3)
    }
    if (maxlag == 4) {
      ## add temperature lag 2, 3 and 4
      model <- update(model, .~.+ temp_minus_2_bs_1 + temp_minus_2_bs_2 + temp_minus_2_bs_3 +
                                  temp_minus_3_bs_1 + temp_minus_3_bs_2 + temp_minus_3_bs_3 + 
                                  temp_minus_4_bs_1 + temp_minus_4_bs_2 + temp_minus_4_bs_3)
    }
    if (maxlag == 5) {
      ## add temperature lag 2, 3, 4 and 5
      model <- update(model, .~.+ temp_minus_2_bs_1 + temp_minus_2_bs_2 + temp_minus_2_bs_3 +
                                  temp_minus_3_bs_1 + temp_minus_3_bs_2 + temp_minus_3_bs_3 + 
                                  temp_minus_4_bs_1 + temp_minus_4_bs_2 + temp_minus_4_bs_3 +
                                  temp_minus_5_bs_1 + temp_minus_5_bs_2 + temp_minus_5_bs_3)
    }
    if (maxlag == 6) {
      ## add temperature lag 2, 3, 4, 5 and 6
      model <- update(model, .~.+ temp_minus_2_bs_1 + temp_minus_2_bs_2 + temp_minus_2_bs_3 +
                                  temp_minus_3_bs_1 + temp_minus_3_bs_2 + temp_minus_3_bs_3 + 
                                  temp_minus_4_bs_1 + temp_minus_4_bs_2 + temp_minus_4_bs_3 +
                                  temp_minus_5_bs_1 + temp_minus_5_bs_2 + temp_minus_5_bs_3 +
                                  temp_minus_6_bs_1 + temp_minus_6_bs_2 + temp_minus_6_bs_3)
    }
    if (maxlag == 7) {
      ## add temperature lag 2, 3, 4, 5, 6 and 7
      model <- update(model, .~.+ temp_minus_2_bs_1 + temp_minus_2_bs_2 + temp_minus_2_bs_3 +
                                  temp_minus_3_bs_1 + temp_minus_3_bs_2 + temp_minus_3_bs_3 + 
                                  temp_minus_4_bs_1 + temp_minus_4_bs_2 + temp_minus_4_bs_3 +
                                  temp_minus_5_bs_1 + temp_minus_5_bs_2 + temp_minus_5_bs_3 +
                                  temp_minus_6_bs_1 + temp_minus_6_bs_2 + temp_minus_6_bs_3 +
                                  temp_minus_7_bs_1 + temp_minus_7_bs_2 + temp_minus_7_bs_3)
    }
  }
  else if (dof == 4) { # we have four basis functions for the spline
    
    # start with maxlag ==1, therefore add temp_0 and temp_minus_1
    model <- glm(formula = hosp_0 ~ temp_0_bs_1 + temp_0_bs_2 + temp_0_bs_3 + temp_0_bs_4 + 
                                    temp_minus_1_bs_1 + temp_minus_1_bs_2 + temp_minus_1_bs_3 + temp_minus_1_bs_4,
                 data = data, family = input_family)
    if (maxlag == 2) {
      model <- update(model, .~.+ temp_minus_2_bs_1 + temp_minus_2_bs_2 + temp_minus_2_bs_3 + temp_minus_2_bs_4)
      }
    if (maxlag == 3) {
      model <- update(model, .~.+ temp_minus_2_bs_1 + temp_minus_2_bs_2 + temp_minus_2_bs_3 + temp_minus_2_bs_4 + 
                                  temp_minus_3_bs_1 + temp_minus_3_bs_2 + temp_minus_3_bs_3 + temp_minus_3_bs_4)
      }
    if (maxlag == 4) {
      model <- update(model, .~.+ temp_minus_2_bs_1 + temp_minus_2_bs_2 + temp_minus_2_bs_3 + temp_minus_2_bs_4 + 
                                  temp_minus_3_bs_1 + temp_minus_3_bs_2 + temp_minus_3_bs_3 + temp_minus_3_bs_4 + 
                                  temp_minus_4_bs_1 + temp_minus_4_bs_2 + temp_minus_4_bs_3 + temp_minus_4_bs_4)
      }
    if (maxlag == 5) {
      model <- update(model, .~.+ temp_minus_2_bs_1 + temp_minus_2_bs_2 + temp_minus_2_bs_3 + temp_minus_2_bs_4 + 
                                  temp_minus_3_bs_1 + temp_minus_3_bs_2 + temp_minus_3_bs_3 + temp_minus_3_bs_4 + 
                                  temp_minus_4_bs_1 + temp_minus_4_bs_2 + temp_minus_4_bs_3 + temp_minus_4_bs_4 + 
                                  temp_minus_5_bs_1 + temp_minus_5_bs_2 + temp_minus_5_bs_3 + temp_minus_5_bs_4)
      }
    if (maxlag == 6) {
      model <- update(model, .~.+ temp_minus_2_bs_1 + temp_minus_2_bs_2 + temp_minus_2_bs_3 + temp_minus_2_bs_4 + 
                                  temp_minus_3_bs_1 + temp_minus_3_bs_2 + temp_minus_3_bs_3 + temp_minus_3_bs_4 + 
                                  temp_minus_4_bs_1 + temp_minus_4_bs_2 + temp_minus_4_bs_3 + temp_minus_4_bs_4 + 
                                  temp_minus_5_bs_1 + temp_minus_5_bs_2 + temp_minus_5_bs_3 + temp_minus_5_bs_4 +
                                  temp_minus_6_bs_1 + temp_minus_6_bs_2 + temp_minus_6_bs_3 + temp_minus_6_bs_4)
      }
    if (maxlag == 7) {
      model <- update(model, .~.+ temp_minus_2_bs_1 + temp_minus_2_bs_2 + temp_minus_2_bs_3 + temp_minus_2_bs_4 + 
                                  temp_minus_3_bs_1 + temp_minus_3_bs_2 + temp_minus_3_bs_3 + temp_minus_3_bs_4 + 
                                  temp_minus_4_bs_1 + temp_minus_4_bs_2 + temp_minus_4_bs_3 + temp_minus_4_bs_4 + 
                                  temp_minus_5_bs_1 + temp_minus_5_bs_2 + temp_minus_5_bs_3 + temp_minus_5_bs_4 +
                                  temp_minus_6_bs_1 + temp_minus_6_bs_2 + temp_minus_6_bs_3 + temp_minus_6_bs_4 + 
                                  temp_minus_7_bs_1 + temp_minus_7_bs_2 + temp_minus_7_bs_3 + temp_minus_7_bs_4)
      }
    }
  else if (dof == 5){ # we have five basis functions for the spline
    
    # start with maxlag ==1, therefore add temp_0 and temp_minus_1
    model <- glm(formula = hosp_0 ~ temp_0_bs_1 + temp_0_bs_2 + temp_0_bs_3 + temp_0_bs_4 + temp_0_bs_5 + 
                                    temp_minus_1_bs_1 + temp_minus_1_bs_2 + temp_minus_1_bs_3 + temp_minus_1_bs_4 + temp_minus_1_bs_5,
                 data = data, family = input_family)
    if (maxlag == 2) {
      model <- update(model, .~.+ temp_minus_2_bs_1 + temp_minus_2_bs_2 + temp_minus_2_bs_3 + temp_minus_2_bs_4 + temp_minus_2_bs_5)
    }
    if (maxlag == 3) {
      model <- update(model, .~.+ temp_minus_2_bs_1 + temp_minus_2_bs_2 + temp_minus_2_bs_3 + temp_minus_2_bs_4 + temp_minus_2_bs_5 + 
                                  temp_minus_3_bs_1 + temp_minus_3_bs_2 + temp_minus_3_bs_3 + temp_minus_3_bs_4 + temp_minus_3_bs_5)
    }
    if (maxlag == 4) {
      model <- update(model, .~.+ temp_minus_2_bs_1 + temp_minus_2_bs_2 + temp_minus_2_bs_3 + temp_minus_2_bs_4 + temp_minus_2_bs_5 + 
                                  temp_minus_3_bs_1 + temp_minus_3_bs_2 + temp_minus_3_bs_3 + temp_minus_3_bs_4 + temp_minus_3_bs_5 + 
                                  temp_minus_4_bs_1 + temp_minus_4_bs_2 + temp_minus_4_bs_3 + temp_minus_4_bs_4 + temp_minus_4_bs_5)
    }
    if (maxlag == 5) {
      model <- update(model, .~.+ temp_minus_2_bs_1 + temp_minus_2_bs_2 + temp_minus_2_bs_3 + temp_minus_2_bs_4 + temp_minus_2_bs_5 + 
                                  temp_minus_3_bs_1 + temp_minus_3_bs_2 + temp_minus_3_bs_3 + temp_minus_3_bs_4 + temp_minus_3_bs_5 + 
                                  temp_minus_4_bs_1 + temp_minus_4_bs_2 + temp_minus_4_bs_3 + temp_minus_4_bs_4 + temp_minus_4_bs_5 + 
                                  temp_minus_5_bs_1 + temp_minus_5_bs_2 + temp_minus_5_bs_3 + temp_minus_5_bs_4 + temp_minus_5_bs_5)
    }
    if (maxlag == 6) {
      model <- update(model, .~.+ temp_minus_2_bs_1 + temp_minus_2_bs_2 + temp_minus_2_bs_3 + temp_minus_2_bs_4 + temp_minus_2_bs_5 + 
                                  temp_minus_3_bs_1 + temp_minus_3_bs_2 + temp_minus_3_bs_3 + temp_minus_3_bs_4 + temp_minus_3_bs_5 + 
                                  temp_minus_4_bs_1 + temp_minus_4_bs_2 + temp_minus_4_bs_3 + temp_minus_4_bs_4 + temp_minus_4_bs_5 + 
                                  temp_minus_5_bs_1 + temp_minus_5_bs_2 + temp_minus_5_bs_3 + temp_minus_5_bs_4 + temp_minus_5_bs_5 + 
                                  temp_minus_6_bs_1 + temp_minus_6_bs_2 + temp_minus_6_bs_3 + temp_minus_6_bs_4 + temp_minus_6_bs_5)
    }
    if (maxlag == 7) {
      model <- update(model, .~.+ temp_minus_2_bs_1 + temp_minus_2_bs_2 + temp_minus_2_bs_3 + temp_minus_2_bs_4 + temp_minus_2_bs_5 + 
                                  temp_minus_3_bs_1 + temp_minus_3_bs_2 + temp_minus_3_bs_3 + temp_minus_3_bs_4 + temp_minus_3_bs_5 + 
                                  temp_minus_4_bs_1 + temp_minus_4_bs_2 + temp_minus_4_bs_3 + temp_minus_4_bs_4 + temp_minus_4_bs_5 + 
                                  temp_minus_5_bs_1 + temp_minus_5_bs_2 + temp_minus_5_bs_3 + temp_minus_5_bs_4 + temp_minus_5_bs_5 + 
                                  temp_minus_6_bs_1 + temp_minus_6_bs_2 + temp_minus_6_bs_3 + temp_minus_6_bs_4 + temp_minus_6_bs_5 + 
                                  temp_minus_7_bs_1 + temp_minus_7_bs_2 + temp_minus_7_bs_3 + temp_minus_7_bs_4 + temp_minus_7_bs_5)
    }
  }
  else {
        validate(
          need(dof %in% c(3,4,5), "Error: degree of freedom must be either 3, 4, or 5")
        )
      }

  # add additonal predictors
  if (input_past.obs == "Yes") {
    model <- update(model, .~.+hosp_minus1 + hosp_minus2 + hosp_minus3 + hosp_minus4 +
                               hosp_minus5 + hosp_minus6 + hosp_minus7)
  }
  
  if (input_day.of.week.hol == "Yes") {
    model <- update(model, .~.+reg_TUE + reg_WED + reg_THU + reg_FRI + SAT + SUN + wd_holiday) 
  }
  
  if (input_pandemic == "Yes") {
    model <- update(model, .~.+pandemic)
  }
  
  model
}


# function to define regression model w/o temperature
build_model_wo_temp <- function(data_train, input_family, 
                        input_past.obs, input_day.of.week.hol, 
                        input_pandemic) {
  
  data <- data_train
  
  model_created <- FALSE
  
  # add additonal predictors
  if (input_past.obs == "Yes") {
    model <- glm(formula = hosp_0 ~ hosp_minus1 + hosp_minus2 + hosp_minus3 + hosp_minus4 +
                               hosp_minus5 + hosp_minus6 + hosp_minus7,
                 data = data, family = input_family)
    
    model_created <- TRUE
  }
  
  if (input_day.of.week.hol == "Yes") {
    if (model_created == TRUE) {
      model <- update(model, .~.+reg_TUE + reg_WED + reg_THU + reg_FRI + SAT + SUN + wd_holiday) 
    }
    else {
      model <- glm(formula = hosp_0 ~ reg_TUE + reg_WED + reg_THU + reg_FRI + SAT + SUN + wd_holiday,
                   data = data, family = input_family)
    }
  }
  
  if (input_pandemic == "Yes") {
    if (model_created == TRUE) {
      model <- update(model, .~.+pandemic)
    }
    else {
      model <- glm(formula = hosp_0 ~ pandemic,
                   data = data, family = input_family)
    }
  }
  
  if (model_created == TRUE) {
    list(model_created, model) 
  }
  else {
    list(model_created)
  }
}

# functions to estimate the exposure-response function

# generate spline basis matrix
compute_basis <- function(x, input_spline_type, tknots, bound_knots) {
  
  if (input_spline_type=="Natural (cubic) splines"){
    basis <- ns(x, knots=tknots, Boundary.knots = bound_knots)
  }
  else {
    stop("Error in spline type.")
  }
  
  basis
}

# compute cumulative exposure-response function
exp_resp_polynomial <- function(coef_model, dof, basis, input_maxlag) {
  
  # Define the coefficients for the spline basis
  # they are included in the model after the intercept
  # coeffiicients for temp lag 0 are in position 2 onwards and their number is equal to dof
  coefficients <- coef_model[c(2:(dof+1))]
  # Calculate the polynomial function for lag 0
  polynomial <- basis %*% coefficients
  # Calculate the polynomial function for other lags and make a cumulative sum
  for (h in c(1:input_maxlag)) {
    # shift by h*dof for each lag 
    coefficients <- coef_model[c((2+(h*dof)):((dof+1)+(h*dof)))]
    pol <- basis %*% coefficients
    polynomial <- polynomial + pol 
  }
  polynomial
}


# function to make k-step ahead forecasts, with prediction intervals
k_step_forecast <- function(forecast_data, start_k_forecast, window, model, 
                            input_spline_type, internal_knots, boundary_knots, compute_pi){
  
  # compute spline basis on the input forecast temperature data
  if (input_spline_type == "Natural (cubic) splines") {
    for (v in c("temp_0", "temp_minus_1", "temp_minus_2", 
                "temp_minus_3", "temp_minus_4", "temp_minus_5", 
                "temp_minus_6", "temp_minus_7")) {
      s <- ns(as.numeric(unlist(forecast_data[,v])), knots=internal_knots, Boundary.knots = boundary_knots) 
      colnames(s) <- paste0(v, "_bs_", colnames(s))
      forecast_data <- cbind(forecast_data, s)
    }
  }
  else {
    stop("Error in spline type.")
  }
  
  #prepare vectors to store the predictions
  predictions <- rep(NaN,length(forecast_data$date))
  lwr_90 <- rep(NaN,length(forecast_data$date))
  upr_90 <- rep(NaN,length(forecast_data$date))
  lwr_50 <- rep(NaN,length(forecast_data$date))
  upr_50 <- rep(NaN,length(forecast_data$date))
  
  for (h in c(start_k_forecast:window)) {
    # PREDICT THE COUNTS FOR THE TEST SET 
    pred <- predict(model, newdata=forecast_data[h,],
                    type="response") 
    predictions[h] <- pred[1]
    
    if (compute_pi==TRUE) {
      ## add 90% prediction intervals with ciTools
      pred_pi <- add_pi(forecast_data[h,], model, names = c("lpb", "upb"), alpha = 0.1) ## alpha = 0.1 --> 90% prediction interval
      lwr_90[h] <- pred_pi[1,"lpb"]
      upr_90[h] <- pred_pi[1,"upb"]  
      ## add 50% prediction intervals with ciTools
      pred_pi <- add_pi(forecast_data[h,], model, names = c("lpb", "upb"), alpha = 0.5) ## alpha = 0.5
      lwr_50[h] <- pred_pi[1,"lpb"]
      upr_50[h] <- pred_pi[1,"upb"]  
    }
    
    # update the hospital visits in the forecats dataset
    # so that the nex k-step ahead forecast will be based on the (k-1) forecast
    forecast_data[h,"hosp_0"] <- predictions[h]
    forecast_data[h+1,"hosp_minus1"] <- predictions[h]
    forecast_data[h+2,"hosp_minus2"] <- predictions[h]
    forecast_data[h+3,"hosp_minus3"] <- predictions[h]
    forecast_data[h+4,"hosp_minus4"] <- predictions[h]
    forecast_data[h+5,"hosp_minus5"] <- predictions[h]
    forecast_data[h+6,"hosp_minus6"] <- predictions[h]
    forecast_data[h+7,"hosp_minus7"] <- predictions[h]
  }
  
  cbind(predictions,lwr_90,upr_90,lwr_50,upr_50)
}

# function to make k-step ahead forecasts, with prediction intervals, propagating uncertainty from the previous step
k_step_forecast_uncertainty <- function(forecast_data, start_k_forecast, window, model, 
                                        input_spline_type, internal_knots, boundary_knots, compute_pi){
  
  # compute spline basis on the input forecast temperature data
  if (input_spline_type == "Natural (cubic) splines") {
    for (v in c("temp_0", "temp_minus_1", "temp_minus_2", 
                "temp_minus_3", "temp_minus_4", "temp_minus_5", 
                "temp_minus_6", "temp_minus_7")) {
      s <- ns(as.numeric(unlist(forecast_data[,v])), knots=internal_knots, Boundary.knots = boundary_knots) 
      colnames(s) <- paste0(v, "_bs_", colnames(s))
      forecast_data <- cbind(forecast_data, s)
    }
  }
  else {
    stop("Error in spline type.")
  }
  
  #prepare vectors to store the predictions
  predictions <- rep(NaN,length(forecast_data$date))
  lwr_90 <- rep(NaN,length(forecast_data$date))
  upr_90 <- rep(NaN,length(forecast_data$date))
  lwr_50 <- rep(NaN,length(forecast_data$date))
  upr_50 <- rep(NaN,length(forecast_data$date))
  
  # Extract the residual standard error from the model
  # rse <- summary(model)$sigma
  # Extract the residual deviance and degrees of freedom to compute the residual standard error
  residual_deviance <- model$deviance
  df_residual <- model$df.residual
  # Calculate the estimated residual standard error (RSE)
  rse <- sqrt(residual_deviance / df_residual)
  
  # initialize matrix to store the results
  n_traj <- 500 # number of simulated trajectories
  predictions_samples <- matrix(NaN, nrow = length(forecast_data$date), ncol = n_traj)
  
  for (n in seq(n_traj)) {
    
    forecast_data_temp <- forecast_data
    # first step of the forecast
    h <- start_k_forecast
    
    # make prediction
    pred <- predict(model, newdata=forecast_data_temp[h,], type="response", se.fit=TRUE) 
    # Compute the standard error of the prediction as SE_pred = sqrt(SE_fit**2 + RSE**2), where RSE is the residual standard error of the model
    se_prediction <- sqrt(pred$se.fit^2 + rse^2)  
    # sample one prediction from the prediction interval 
    pred_previous_step <- rnorm(1, pred$fit, se_prediction)
    # store it in the matrix
    predictions_samples[h,n] <- pred_previous_step
    
    ## I create 500 point forecasts, by sampling the previous step from the prediction interval
    for (h in c((start_k_forecast+1):window)) {
      # update the (temporary) forecast dataset
      forecast_data_temp[h-1,"hosp_0"] <- pred_previous_step[1]
      forecast_data_temp[h-1+1,"hosp_minus1"] <- pred_previous_step[1]
      forecast_data_temp[h-1+2,"hosp_minus2"] <- pred_previous_step[1]
      forecast_data_temp[h-1+3,"hosp_minus3"] <- pred_previous_step[1]
      forecast_data_temp[h-1+4,"hosp_minus4"] <- pred_previous_step[1]
      forecast_data_temp[h-1+5,"hosp_minus5"] <- pred_previous_step[1]
      forecast_data_temp[h-1+6,"hosp_minus6"] <- pred_previous_step[1]
      forecast_data_temp[h-1+7,"hosp_minus7"] <- pred_previous_step[1]
      # make the next prediction, accounting for the previous update
      pred <- predict(model, newdata=forecast_data_temp[h,],type="response", se.fit=TRUE)  
      # Compute the new standard error of the prediction
      se_prediction <- sqrt(pred$se.fit^2 + rse^2)
      # sample one new prediction
      pred_previous_step <- rnorm(1, pred$fit, se_prediction)
      # store it in the matrix
      predictions_samples[h,n] <- pred_previous_step
    }
  }
  # compute median and 90% and 50% quantile ranges
  for (h in c((start_k_forecast):window)) {
    samples <- predictions_samples[h,]
    predictions[h] <- quantile(samples, 0.5)
    lwr_90[h] <- quantile(samples, 0.05)
    upr_90[h] <- quantile(samples, 0.95)
    lwr_50[h] <- quantile(samples, 0.25)
    upr_50[h] <- quantile(samples, 0.75)
  }
  
  cbind(predictions,lwr_90,upr_90,lwr_50,upr_50)
}


# function to make cumulative k-step ahead forecasts, with prediction intervals on the cumulative numbers
k_step_forecast_uncertainty_cumulative <- function(forecast_data, start_k_forecast, window, model, 
                                                   input_spline_type, internal_knots, boundary_knots, compute_pi){
  
  # compute spline basis on the input forecast temperature data
  if (input_spline_type == "Natural (cubic) splines") {
    for (v in c("temp_0", "temp_minus_1", "temp_minus_2", 
                "temp_minus_3", "temp_minus_4", "temp_minus_5", 
                "temp_minus_6", "temp_minus_7")) {
      s <- ns(as.numeric(unlist(forecast_data[,v])), knots=internal_knots, Boundary.knots = boundary_knots) 
      colnames(s) <- paste0(v, "_bs_", colnames(s))
      forecast_data <- cbind(forecast_data, s)
    }
  }
  else {
    stop("Error in spline type.")
  }
  
  #prepare vectors to store the predictions
  predictions <- rep(NaN,length(forecast_data$date))
  lwr_90 <- rep(NaN,length(forecast_data$date))
  upr_90 <- rep(NaN,length(forecast_data$date))
  lwr_50 <- rep(NaN,length(forecast_data$date))
  upr_50 <- rep(NaN,length(forecast_data$date))
  
  # Extract the residual standard error from the model
  # rse <- summary(model)$sigma
  # Extract the residual deviance and degrees of freedom to compute the residual standard error
  residual_deviance <- model$deviance
  df_residual <- model$df.residual
  # Calculate the estimated residual standard error (RSE)
  rse <- sqrt(residual_deviance / df_residual)
  
  # initialize matrix to store the results
  n_traj <- 500 # number of simulated trajectories
  predictions_samples <- matrix(NaN, nrow = length(forecast_data$date), ncol = n_traj)
  
  for (n in seq(n_traj)) {
    
    forecast_data_temp <- forecast_data
    # first step of the forecast
    h <- start_k_forecast
    
    # make prediction
    pred <- predict(model, newdata=forecast_data_temp[h,], type="response", se.fit=TRUE) 
    # Compute the standard error of the prediction as SE_pred = sqrt(SE_fit**2 + RSE**2), where RSE is the residual standard error of the model
    se_prediction <- sqrt(pred$se.fit^2 + rse^2)  
    # sample one prediction from the prediction interval 
    pred_previous_step <- rnorm(1, pred$fit, se_prediction)
    # store it in the matrix
    predictions_samples[h,n] <- pred_previous_step
    
    ## I create 500 point forecasts, by sampling the previous step from the prediction interval
    for (h in c((start_k_forecast+1):window)) {
      # update the (temporary) forecast dataset
      forecast_data_temp[h-1,"hosp_0"] <- pred_previous_step[1]
      forecast_data_temp[h-1+1,"hosp_minus1"] <- pred_previous_step[1]
      forecast_data_temp[h-1+2,"hosp_minus2"] <- pred_previous_step[1]
      forecast_data_temp[h-1+3,"hosp_minus3"] <- pred_previous_step[1]
      forecast_data_temp[h-1+4,"hosp_minus4"] <- pred_previous_step[1]
      forecast_data_temp[h-1+5,"hosp_minus5"] <- pred_previous_step[1]
      forecast_data_temp[h-1+6,"hosp_minus6"] <- pred_previous_step[1]
      forecast_data_temp[h-1+7,"hosp_minus7"] <- pred_previous_step[1]
      # make the next prediction, accounting for the previous update
      pred <- predict(model, newdata=forecast_data_temp[h,],type="response", se.fit=TRUE)  
      # Compute the new standard error of the prediction
      se_prediction <- sqrt(pred$se.fit^2 + rse^2)
      # sample one new prediction
      pred_previous_step <- rnorm(1, pred$fit,se_prediction)
      # store it in the matrix
      predictions_samples[h,n] <- pred_previous_step
    }
  }
  # compute median and 90% and 50% quantile ranges on the cumulative sum over the forecasting horizon
  cum_predictions <- colSums(predictions_samples[c((start_k_forecast):window),])
  cum_predictions_med <- quantile(cum_predictions, 0.5)
  lwr_90_cum <- quantile(cum_predictions, 0.05)
  upr_90_cum <- quantile(cum_predictions, 0.95)
  lwr_50_cum <- quantile(cum_predictions, 0.25)
  upr_50_cum <- quantile(cum_predictions, 0.75)
  
  cbind(cum_predictions_med,lwr_90_cum,upr_90_cum,lwr_50_cum,upr_50_cum)
}

data_read_temperature <- function(input_data_temperature, path_data_temperature) {
  
  if (input_data_temperature == "New data (upload)") {
    path_data <- path_data_temperature
  }
  else {
    stop("Error in data source.") 
  }
  
  first_line <- tryCatch(
    readLines(path_data, n = 1),  # Read the first line
    # if the file has not been uploaded and we cannot read it, we throw an error
    error = function(e) {
      stop("You have not uploaded you data file yet. Please upload a file to continue.")
    }
  )
  
  if (grepl(";", first_line)) {
    sep <- ";"  # Semicolon-separated
  } else {
    sep <- ","  #Comma-separated
  }
  
  data_raw_temperature <- read.csv(path_data, sep = sep, header=TRUE)
  
  data_raw_temperature$date <- as.Date(data_raw_temperature$date, format="%Y-%m-%d")
  
  data_raw_temperature
}

