#
# Barranquilla Crime Forecasting App
#
#


# Load Libraries ----------------------------------------------------------
#install.packages("sweep_0.2.3.tar.gz", repos = NULL, type = "source")
#devtools::install_github("business-science/anomalize")
#remotes::install_github("deepanshu88/summaryBox")
#install.packages("randomForest")
library(plotly)
library(reactable)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyEffects)
library(fresh)
library(shinyjs)
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)
library(modeltime.resample)
library(tidyquant)
library(timetk)
library(lubridate)
library(tidyverse)
library(anomalize)
library(shinybusy)
library(summaryBox)
library(randomForest)
library(feasts)

# Utils
source("utils/theme.R")


# Read Data
load("data/crimes.RData")
load("data/crimes_ts.RData")



# UI ----------------------------------------------------------------------


ui <- dashboardPage(
  title = "Barranquilla Crime Forecasting App",
  skin  = "green-light",
  freshTheme = theme,
  options = list(sidebarExpandOnHover = FALSE),
  # Header ----
  header = dashboardHeader(title = tagList(
    span(class = "logo-lg", img(
      src = "forecast_baq.png",
      style = "width: 35px", "BAQ-CFD"
    )),
    img(
      src = "forecast_baq.png",
      style = "width: 35px"
    )
  )),
  
  # Sidebar ----
  sidebar = dashboardSidebar(
    minified   = TRUE,
    collapsed  = FALSE,
    br(),
    div(
      class = "text-center",
      p(HTML("<strong>Barranquilla Crime Forecasting Dashboard</strong>"),
        class="text-light-blue hide-sidebar-collapse")
    ),
    sidebarMenu(
      align = "center",
      # Select Crime Type Dropdown
      menuItem(
        text       =  selectInput("crime_type", 
                                  "Select Crime Type", 
                                  choices = c("Homicides"="homicide", 
                                              "Extorsions"="extorsion", 
                                              "Muggings"="mugging", 
                                              "Auto Thefts"="auto_theft", 
                                              "Home Burglaries"="home_burglary")),
        tabName    = "select_data",
        icon       = icon("handcuffs")
      ),
      
      # Select Forecast Slider
      menuItem(
        
        text       =  sliderInput("slider", "Select Forecast Horizon (Months)", min = 6, max = 36, value = 12),
        
        tabName    = "select_time",
        icon       = icon("forward")
      ), 
      menuItem(
        actionButton("button", label = "Run", 
                     icon = icon("play-circle", class = "fa-lg"), 
                     style = "background-color: #FF5C26; color: white; font-size: 14px; padding: 8px 8px; margin: 0px 0px 0px 69px;")
      )
    ),
    br(),
    # Contact and Code social buttons
    div(
      class = "text-center",
      p(HTML("<strong>Contact and Code</strong>"),
        class="text-light-blue hide-sidebar-collapse") %>%
        a(href = "https://github.com/mparoca/barranquilla_crime_forecast_dashboard", target = "_blank"),
      socialButton(
        href = "https://www.linkedin.com/in/maria-paula-aroca-42a0a5166/",
        icon = icon("linkedin")
      ),
      socialButton(
        href = "https://github.com/mparoca/barranquilla_crime_forecast_dashboard",
        icon = icon("github")
      )
    ), 
    
    # Slider Theme
    shinyWidgets::chooseSliderSkin("Flat", color="#002F46"),
    
    # Add Busy Indicators
    add_busy_spinner(spin = "looping-rhombuses", 
                     position="bottom-right",
                     margins = c(50, 50),
                     color="#002F46"),
    add_busy_bar(color = "#106d67", height = "8px")
  ),

  # Body ----  
  body = dashboardBody(
    setShadow(class = "dropdown-menu"),
    setShadow(class = "box"),
    useShinyjs(),
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "styles.css"
      )
    ),
    fluidRow(
      tabBox(
        title = '', width = 12,
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "250px",
        
        # About Tab ----
        tabPanel("About",
                 htmlOutput("description")),
        
        
        # Explore Tab ----
        tabPanel("Explore TS",
                 htmlOutput("button_text3"),
                 br(),
                 textOutput("crime_type_text3"),
                 hr(),
                 uiOutput("ts_plot"),
                 hr(),
                 plotOutput("ggsubseries"), 
                 hr(),
                 plotOutput("ggseason"), 
                 hr(),
                 plotOutput("autocorrelation"), 
                 hr(),
                 plotlyOutput("decomposition", height = '600', width = 'auto')),

        # All Models Tab ----
        tabPanel("All Models",
                 htmlOutput("button_text"),
                 textOutput("crime_type_text"),
                 textOutput("horizon_text"),
                 hr(),
                 uiOutput("forecast_table"),
                 uiOutput("title_forecast_table"),
                 reactableOutput("accuracy_table"),
                 uiOutput("all_models_plot")),
        
        # Ensemble Model Tab ----
        tabPanel("Ensemble Model",
                 htmlOutput("button_text2"),
                 textOutput("crime_type_text2"),
                 textOutput("horizon_text2"),
                 hr(),
                 uiOutput("ensemble_plot"),
                 hr(),
                 uiOutput("summarybox")),
        
        
        
        
      )
    ),
    fluidRow(infoBoxOutput("tabset1Selected")),
    # Favicon
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.png"))
  ),
)


# Server ------------------------------------------------------------------

server <- function(input, output) { 
  
  # Description Text ---- 
  output$description <- renderUI({
    HTML(
      "<div style='text-align:center;'><img src='https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Flag_of_Barranquilla.svg/800px-Flag_of_Barranquilla.svg.png?20100103094823' alt='Barranquilla' style='width:10%;max-width:500px;'></div>",
      "<br>",
      "<div style='text-align:center;'>",
      "<b><span style='font-size:22px;color:#002F46;'>", "Welcome to the Barranquilla Crime Forecasting Dashboard!", "</span></b>", "<br> <br>",
      "<div style='text-align:left;'>",
      "<p style='font-size:22px;'><b>Objective:</b>",
      "<p style='font-size:16px;'>The objective of this app is to allow users to explore and predict crime data in the city of <a href='https://en.wikipedia.org/wiki/Barranquilla' target='_blank'>Barranquilla, Colombia</a>, using historical data obtained from the <a href='https://www.policia.gov.co/grupo-informaci%C3%B3n-criminalidad/estadistica-delictiva' target='_blank'>National Police Crime Database</a> for crimes between 2010 and March of 2023.</p>",
      "<br>",
      "<p style='font-size:20px;'><b>Navigation:</b></p>",
      "<ul>",
      "<li><p style='font-size:16px;'><b><span style='color:#002F46;'>Explore TS Tab:</span></b> Here you can explore the time series for each crime type, its seasonal patterns and autocorrelation, and perform anomaly detection.</p></li>",
      "<li><p style='font-size:16px;'><b><span style='color:#FF5C26;'>All Models Tab:</span></b> Here you can select a crime type and forecast horizon, multiple forecast models will be fitted, and forecasts as well as accuracy metrics will be presented.</p></li>",
      "<li><p style='font-size:16px;'><b><span style='color:#179C94;'>Ensemble Model Tab:</span></b> An ensemble forecasting model will be fitted showing the average prediction across all of the multiple models seen in the All Models tab.</p></li>",   
      "</ul>",
      "<br>",
      "<p style='font-size:20px;'><b>Methods:</b></p>",
      "<p style='font-size:16px;'>For the forecasts, 8 forecasting Models were used:</p>",
      "<ul>",
      "<li>
        <p style='font-size:16px;'>
            <a href='https://business-science.github.io/modeltime/reference/arima_reg.html' target='_blank'>
                Auto ARIMA model
            </a>
        </p>
    </li>",
      "<li>
        <p style='font-size:16px;'>
            <a href='https://business-science.github.io/modeltime/reference/arima_boost.html' target='_blank'>
                'Boosted' ARIMA
            </a>
        </p>
    </li>",
      "<li>
        <p style='font-size:16px;'>
            <a href='https://business-science.github.io/modeltime/reference/exp_smoothing.html' target='_blank'>
                Exponential Smoothing
            </a>
        </p>
    </li>",
      "<li>
        <p style='font-size:16px;'>
            <a href='https://business-science.github.io/modeltime/reference/prophet_reg.html' target='_blank'>
                Prophet
            </a>
        </p>
    </li>",
      "<li>
        <p style='font-size:16px;'>
            <a href='https://parsnip.tidymodels.org/reference/linear_reg.html' target='_blank'>
                GLM
            </a>
        </p>
    </li>",
      "<li>
        <p style='font-size:16px;'>
            <a href='https://cran.r-project.org/web/packages/randomForest/index.html' target='_blank'>
                Random Forest
            </a>
        </p>
    </li>",
      "<li>
        <p style='font-size:16px;'>
            <a href='https://parsnip.tidymodels.org/reference/boost_tree.html' target='_blank'>
                Boosted trees (XGBOOST)
            </a>
        </p>
    </li>",
      "<li>
        <p style='font-size:16px;'>
            <a href='https://business-science.github.io/modeltime/reference/prophet_boost.html' target='_blank'>
                Boosted Prophet
            </a>
        </p>
    </li>",
      "</ul>",
      "<p style='font-size:16px;'>An <a href='https://business-science.github.io/modeltime.ensemble/' target='_blank'>Average Ensemble Forecasting</a> model was fitted that weights all models with the same proportion and selects the average for each timestamp. </p>",
      "<br>",
      "<p style='font-size:20px;'><b>Tools:</b></p>",
      "<p style='font-size:16px;'><a href='https://cran.r-project.org/web/packages/feasts/index.html' target='_blank'>feasts</a> package was used for seasonal summaries and autocorrelation plots. </p>",
      "<p style='font-size:16px;'><a href='https://business-science.github.io/anomalize/' target='_blank'>anomalize</a> package was used for anomaly detection. </p>",
      "<p style='font-size:16px;'><a href='https://business-science.github.io/modeltime/' target='_blank'>modeltime</a> and  <a href='https://parsnip.tidymodels.org/index.html' target='_blank'>parsnip</a> were used for modeling. </p>",
      "<p style='font-size:16px;'>App was created using <a href='https://shiny.rstudio.com/' target='_blank'>shiny</a>, and <a href='https://rinterface.github.io/shinydashboardPlus/' target='_blank'>shinydashboardPlus</a>. Theming was done with 
      <a href='https://github.com/dreamRs/fresh' target='_blank'>fresh</a>, 
      <a href='https://github.com/dreamRs/shinybusy' target='_blank'>shinybusy</a> and,
      <a href='https://github.com/deepanshu88/summaryBox' target='_blank'>summaryBox</a>.
      </p>",
      "<p style='font-size:16px;'> Icons from <a href='https://fontawesome.com/v4/icons/' target='_blank'>Font Awesome</a>. </p>",
      "<div style='text-align:left;'>",
    )
  })
  
  
  # Create reactiveValues object to store model output
  rv <- reactiveValues()
  
  # Create reactiveValues object to store flag
  values <- reactiveValues(clicked = FALSE)
  
  # observeEvent ----  
  observeEvent(input$button, {
    
    # Triggered by button click
    values$clicked <- TRUE
    
    rv$data <-crimes %>% filter(id ==  input$crime_type)
    rv$horizon <-input$slider
    rv$months <- paste0(input$slider, " months")
    
    # Models ----    
    #Split Data
    splits <- initial_time_split(rv$data, prop = 0.8)
    
    rv$splits <-splits
    
    #Arima
    model_fit_arima_no_boost <- arima_reg() %>%
      set_engine(engine = "auto_arima") %>%
      fit(value ~ date, data = training(splits))
    
    #Arima Boost
    model_fit_arima_boosted <- arima_boost(
      min_n = 2,
      learn_rate = 0.015
    ) %>%
      set_engine(engine = "auto_arima_xgboost") %>%
      fit(value ~ date + as.numeric(date) + factor(month(date, label = TRUE), ordered = F),
          data = training(splits))
    
    #ETS
    if (input$crime_type=="extorsion") {
      model_fit_ets <- exp_smoothing() %>%
        set_engine(engine = "ets") %>%
        fit(value ~ date, data = training(splits))
    }
    else {
      model_fit_ets <- exp_smoothing(trend="multiplicative", season="multiplicative") %>%
        set_engine(engine = "ets") %>%
        fit(value ~ date, data = training(splits))
    }
    
    
    #Prophet
    if (input$crime_type=="homicide") {
    model_fit_prophet <- prophet_reg() %>%
      set_engine(engine = "prophet") %>%
      fit(value ~ date, data = training(splits))
    }
    else{
      model_fit_prophet <- prophet_reg(seasonality_yearly=TRUE, 
                                       season = "multiplicative", 
                                       changepoint_range = 0.8, 
                                       prior_scale_changepoints = 0.5) %>%
        set_engine(engine = "prophet") %>%
        fit(value ~ date, data = training(splits))
    }
    #LM
    model_fit_lm <- linear_reg() %>%
      set_engine("glm") %>%
      fit(value ~ as.numeric(date) + factor(month(date, label = TRUE), ordered = FALSE),
          data = training(splits))
    
    #Random Forest
    model_fit_rf <- rand_forest(mode = "regression") %>%
      set_engine("randomForest") %>%
      fit(
        value ~ as.numeric(date) + month(date, label = TRUE), 
        data = training(splits)
      )
    
    #XGBOOST
    model_fit_xgboost <- boost_tree(mode = "regression") %>%
      set_engine("xgboost") %>%
      fit(
        value ~ as.numeric(date) + month(date, label = TRUE), 
        data = training(splits)
      )
    
    #PROPHET BOOST
    if (input$crime_type=="homicide") {
    model_fit_prophet_boost <- prophet_boost() %>%
      set_engine("prophet_xgboost") %>%
      fit(
        value ~ date + as.numeric(date) + month(date, label = TRUE), 
        data = training(splits)
      )
    }
    else{
      model_fit_prophet_boost <- prophet_boost(seasonality_yearly=TRUE, 
                                               season = "multiplicative", 
                                               changepoint_range = 0.8, 
                                               prior_scale_changepoints = 0.5) %>%
        set_engine("prophet_xgboost") %>%
        fit(
          value ~ date + as.numeric(date) + month(date, label = TRUE), 
          data = training(splits)
        )
    }
    
    # Model Accuracy Stats ----
    models_tbl <- modeltime_table(
      model_fit_arima_no_boost,
      model_fit_arima_boosted,
      model_fit_ets,
      model_fit_prophet,
      model_fit_lm, 
      model_fit_rf, 
      model_fit_xgboost, 
      model_fit_prophet_boost
    )
    
    #Calibration
    calibration_tbl <- models_tbl %>%
      modeltime_calibrate(new_data = testing(splits))
    
    
    rv$calibration <- calibration_tbl
    
    # Ensemble Model ----
    ensemble_fit_mean <- models_tbl %>%
      filter(!.model_id %in% c(1)) %>%
      ensemble_average(type = "mean")
    
    ensemble_tbl <- modeltime_table(
      ensemble_fit_mean
    )
    
    ensemble_calibration_tbl <- ensemble_tbl%>%
      modeltime_calibrate(new_data = testing(splits))
    
    ensemble_refit_tbl <- ensemble_calibration_tbl %>%
      modeltime_refit(rv$data)
    
    rv$ensemble_refit_tbl <-ensemble_refit_tbl
    
    ensemble_accuracy <-
      ensemble_tbl %>%
      modeltime_accuracy(testing(splits))
    
    rv$ensemble_accuracy <-ensemble_accuracy
    
  })
  
  # Reactive Tab Text  ----
    
  ## All Models Tab Text
  output$button_text <- renderUI({
    if (values$clicked) {
      HTML("<b><span style='font-size:16px;'>", "Forecasts Calculated!", "</span></b>", "<br>")
    } else {
      
      HTML("<b><span style='font-size:22px;color:#002F46;'>", "Compare Forecasts for Different Models", "</span></b>", "<br> <br>",
           "<b><span style='font-size:20px;color:#002F46;'>", "Instructions:", "</span></b>", "<br>",
           "<span style='font-size:16px;'>", "On the Sidebar Menu select a", "</span>", 
           "<b><span style='font-size:16px;color:#002F46;'>", "Crime Type", "</span></b>", 
           ", and a", "</span>", 
           "<b><span style='font-size:16px;color:#002F46;'>", "Forecast Horizon", "</span></b>", "<br>",
           "<b><span style='font-size:16px;color:#FF5C26;'>", "Click Run!▶", "</span></b>", "<br> <br>", 
           "Please allow the app some time while the models are being fit. This may take a few minutes depending on the series...", "<i class='far fa-clock'></i></span>", )
      
    }
  })
  
  #Type
  output$crime_type_text <-renderText(
    if (values$clicked) {
      paste0("Crime Type: ", input$crime_type)
    }
    else{
      paste0("")
    }
  )
  
  #Horizon
  output$horizon_text <-renderText(
    if (values$clicked) {
      paste0("Horizon: ", input$slider, " months")
    }
    else{
      paste0("")
    }
  )
  
  # Table title
  output$title_forecast_table <-renderUI({
    if (values$clicked) {
      h4("Model Accuracy Table")
    }
  }
  )
  
  
  ## Ensemble Model Text
  output$button_text2 <- renderUI({
    if (values$clicked) {
      HTML("<b><span style='font-size:16px;'>", "Ensemble Forecast Calculated!", "</span></b>", "<br>")
    } else {
      
      HTML("<b><span style='font-size:22px;color:#002F46;'>", "Ensemble Modeling with using Mean Aggregate Method", "</span></b>", "<br> <br>",
           "<b><span style='font-size:20px;'>", "Instructions:", "</span></b>", "<br>",
           "<span style='font-size:16px;'>", "On the Sidebar Menu select a", "</span>", 
           "<b><span style='font-size:16px;color:#002F46;'>", "Crime Type", "</span></b>", 
           ", and a", "</span>", 
           "<b><span style='font-size:16px;color:#002F46;'>", "Forecast Horizon", "</span></b>", "<br>",
           "<b><span style='font-size:16px;color:#FF5C26;'>", "Click Run!▶", "</span></b>", "<br> <br>", 
           "Please allow the app some time while the models are being fit. This may take a few minutes depending on the series...", "<i class='far fa-clock'></i></span>", )
      
    }
  })
  
  # Type
  output$crime_type_text2 <-renderText(
    if (values$clicked) {
      paste0("Crime Type: ", input$crime_type)
    }
    else{
      paste0("")
    }
  )
  
  # Horizon
  output$horizon_text2 <-renderText(
    if (values$clicked) {
      paste0("Horizon: ", input$slider, " months")
    }
    else{
      paste0("")
    }
  )
  
  ## Explore TS Text
  output$button_text3 <- renderUI({
    HTML("<b><span style='font-size:20px;color:#002F46;'>", "Explore the Time Series by Crime Type", "</span></b>", "<br> <br>",
           "<b><span style='font-size:16px;color:#002F46;'>", "Instructions:", "</span></b>", "<br>",
           "<span style='font-size:16px;'>", "On the Sidebar Menu select a", "</span>", 
           "<b><span style='font-size:16px;color:#002F46;'>", "Crime Type", "</span></b>", ".")

  })
  
  #Type
  output$crime_type_text3 <-renderText({
    paste0("Selected Crime Type: ", input$crime_type)
  }
  )

  
  # Forecast vs Data Plot ----
  
  output$all_models_plot <-renderUI({
    req(rv$data)
    req(rv$calibration)
    req(rv$splits)
    
    rv$calibration %>%
      modeltime_forecast(
        new_data    = testing(rv$splits),
        actual_data = rv$data
      ) %>%
      plot_modeltime_forecast(
        .legend_max_width = 25, # For mobile screens
        .interactive      = TRUE,
        .conf_interval_show = FALSE,
        .title = "Forecasting Models compared with Actual Data",
      ) %>% 
      layout(xaxis = list(tickformat = "%Y", hoverformat="%B %Y"), 
             yaxis=list(hoverformat = '.0f')) %>% 
      style(hoverinfo = "x+y")
  })
  
  # Accuracy Table  ----  
  output$accuracy_table <- renderReactable({
    req(rv$calibration)
    
    rv$calibration %>%
      modeltime_accuracy() %>%
      table_modeltime_accuracy(
        .interactive = TRUE,
        .title = "Model Accuracy Table", 
        highlight = TRUE
      )
  })
  
  # Forecast Plot All Models ----
  output$forecast_table <- renderUI({
    req(rv$calibration)
    req(rv$data)
    req(rv$months)
    
    refit_tbl <- rv$calibration %>%
      modeltime_refit(data = rv$data)
    
    refit_tbl %>%
      modeltime_forecast(h = rv$months, actual_data = rv$data) %>%
      plot_modeltime_forecast(
        .legend_max_width = 25, # For mobile screens
        .interactive      = TRUE, 
        .conf_interval_show = FALSE,
        .title = "Forecast comparison for Selected Forecast Horizon",
      ) %>% 
      layout(xaxis = list(tickformat = "%Y", hoverformat="%B %Y"), 
             yaxis=list(hoverformat = '.0f')) %>% 
      style(hoverinfo = "x+y")
    
  })
  
  # Ensemble Plot ----
  output$ensemble_plot <- renderUI({
    req(rv$data)
    req(rv$months)
    req(rv$ensemble_refit_tbl)
    
    rv$ensemble_refit_tbl %>%
      modeltime_forecast(
        h = rv$months,
        actual_data = rv$data,
        conf_interval = 0.80
      )  %>%
      plot_modeltime_forecast(
        .legend_max_width = 25, # For mobile screens
        .interactive      = TRUE
      ) %>% 
      layout(xaxis = list(tickformat = "%Y", hoverformat="%B %Y"), 
             yaxis=list(hoverformat = '.0f')) %>% 
      style(hoverinfo = "x+y")
    
    
    
  })
  
  # Summary Boxes  ----  
  output$summarybox <- renderUI({
    
    if (values$clicked) {
    req(rv$ensemble_accuracy)
    mae <- round(rv$ensemble_accuracy$mae,2)
    mape <- round(rv$ensemble_accuracy$mape,2)
    mase <- round(rv$ensemble_accuracy$mase,2)
    smape <- round(rv$ensemble_accuracy$smape,2)
    rmse <- round(rv$ensemble_accuracy$rmse,2)
    rsq <- round(rv$ensemble_accuracy$rsq,2)
    
    fluidRow(
      summaryBox2("MAE", mae , width = 2, icon = "fas fa-ruler", style = "info"),
      summaryBox2("MAPE", mape, width = 2, icon = "fas fa-percent", style = "success"),
      summaryBox2("MASE", mase, width = 2, icon = "fas fa-weight-scale", style = "danger"),
      summaryBox2("SMAPE", smape, width = 2, icon = "fas fa-percent", style = "primary"), 
      summaryBox2("RMSE", rmse, width = 2, icon = "fas fa-square-root-alt", style = "warning"),
      summaryBox2("RSQ", rsq, width = 2, icon = "fas fa-registered", style = "info")
    )
    }
    
    else {
      fluidRow(
        summaryBox2("MAE", NA , width = 2, icon = "fas fa-ruler", style = "info"),
        summaryBox2("MAPE", NA, width = 2, icon = "fas fa-percent", style = "success"),
        summaryBox2("MASE", NA, width = 2, icon = "fas fa-weight-scale", style = "danger"),
        summaryBox2("SMAPE", NA, width = 2, icon = "fas fa-percent", style = "primary"), 
        summaryBox2("RMSE", NA, width = 2, icon = "fas fa-square-root-alt", style = "warning"),
        summaryBox2("RSQ", NA, width = 2, icon = "fas fa-registered", style = "info")
      )
      
    }
    
    
  })
  
  
  
  
  # Time Series Plot  ----  
  output$ts_plot <-renderUI({
    
    crimes %>% 
      filter(id ==  input$crime_type) %>%
      plot_time_series(date, value, 
                       .interactive = TRUE, 
                       .line_color = "#002F46", 
                       .smooth_color = "#FF5C26",
                       .title = paste0("Time Series Plot: ", input$crime_type, " [with LOESS Smoother]"), 
                       .y_lab = "Total", 
                       .x_lab = "Month", 
                       .plotly_slider = TRUE,
                       ) %>% 
      layout(xaxis = list(tickformat = "%Y", hoverformat="%B %Y"), 
             yaxis=list(hoverformat = '.0f')) %>% 
      style(hoverinfo = "x+y")
    
  })

  # Decomposition Plot ----   
  output$decomposition <-renderPlotly({
    
    crimes_ts <-crimes %>% 
      filter(id ==  input$crime_type)
    
    prep_tbl_time(crimes_ts) %>%
      time_decompose(value, method = "stl", frequency = "auto", trend = "auto") %>%
      anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
      plot_anomaly_decomposition() +
      labs(title="Anomalies Detected in STL Decomposed Series with GESD\n")
  })
  
  # GgSubseries Plot ---- 
  
  output$ggsubseries <- renderPlot({
    crimes_ts %>% 
      select(as.character(input$crime_type)) %>% 
      gg_subseries() +
      labs(title = "Seasonal Subseries Plot") +
      theme_bw() +
      theme(strip.background = element_rect(fill = "#002F46"), 
            strip.text = element_text(color = "white"), 
            plot.title = element_text(size = 18, color = "#002F46"), 
            axis.title.x = element_text(size = 14, color = "#002F46"), 
            axis.title.y = element_text(size = 14, color = "#002F46")) +
      geom_line(color = "#002F46") +
      geom_hline(aes(yintercept = !!sym(".yint")), colour = "#FF5C26")
  })
  
  # GgSeason Plot ---- 
  
  output$ggseason<- renderPlot({
    
    fit <- crimes_ts %>%
      select(as.character(input$crime_type)) %>% 
      model(
        STL() #Default values chosen
      ) %>%
      components()
    
    fit %>% 
      gg_season(season_year) +
      labs(title = "Time Series Seasonal Plot (with STL Decomposition)") +
      theme_minimal() +
      theme(plot.title = element_text(size = 18, color = "#002F46"), 
            axis.title.x = element_text(size = 14, color = "#002F46"), 
            axis.title.y = element_text(size = 14, color = "#002F46"))
  })
  
  # Autocorrelation Plot ---- 
  output$autocorrelation <- renderPlot({
    crimes_ts %>% 
      select(as.character(input$crime_type)) %>%
      ACF() %>%
      autoplot() +
      labs(title = paste0("Autocorrelation Function (ACF) Plot for Crime Type: ", as.character(input$crime_type))) +
      theme_bw() +
      theme(plot.title = element_text(size = 18, color = "#002F46"), 
            axis.title.x = element_text(size = 14, color = "#002F46"), 
            axis.title.y = element_text(size = 14, color = "#002F46"))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
