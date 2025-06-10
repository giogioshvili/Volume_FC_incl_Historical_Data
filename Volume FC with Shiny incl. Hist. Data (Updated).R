# Load required packages
library(shiny)
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)
library(Metrics)
library(lubridate)
library(readr)
library(scales)
library(feasts)
library(fable)
library(fabletools)
library(distributional)
library(ggiraph)

ui <- fluidPage(
  titlePanel("Volume Forecast: ARIMA vs ETS"),
  
  # Controls row (centered)
  fluidRow(
    column(
      width = 6, offset = 3,
      wellPanel(
        fileInput("file", "Upload CSV File", accept = ".csv"),
        downloadButton("download_forecast", "Forecast CSV", class = "btn-primary")
      )
    )
  ),
  
  # Full-width chart row
  fluidRow(
    column(
      width = 12,
      girafeOutput("forecastPlot", width = "100%", height = "600px")
    )
  )
)

server <- function(input, output) {
  forecast_data <- reactive({
    req(input$file)
    
    # Load Data
    data <- read.csv(
      input$file$datapath,
      fileEncoding = "UTF-16LE",
      sep = "\t",
      header = FALSE,
      check.names = FALSE
    )
    
    # Transforming the File
    data <- data[, -c(2, 3)]
    data <- data[c(1, 4), ]
    
    data <- as.data.frame(t(data), stringsAsFactors = FALSE)
    colnames(data) <- as.character(unlist(data[1, ]))
    data <- data[-1, ]
    
    colnames(data)[1] <- "Date"
    colnames(data)[2] <- "Volume"
    
    data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
    
    data$Volume <- gsub(",", "", data$Volume)
    data$Volume <- as.numeric(data$Volume)
    
    clean_data <- data
    
    # Prepare Data Frame
    ts_data <- clean_data %>%
      arrange(Date) %>%
      as_tsibble(index = Date)
    
    model_ARIMA <- ts_data %>%
      model(arima = ARIMA(Volume))
    
    
    # Calculate number of weeks to forecast
    last_date <- max(ts_data$Date)
    target_date <- as.Date("2025-12-31")
    weeks_to_forecast <- max(0, as.integer(difftime(target_date, last_date, units = "weeks")))
    
    # Forecast
    forecast_ARIMA <- model_ARIMA %>%
      forecast(h = weeks_to_forecast)
    
    # Clean the Df
    forecast_clean_ARIMA <- forecast_ARIMA %>%
      as_tibble() %>%
      select(Date = Date, `FC Volume` = .mean) %>%
      mutate(`FC Volume` = round(`FC Volume`, 0))
    
    # ETS Model
    model_ETS <- ts_data %>%
      model(ets = ETS(Volume))
    
    forecast_ETS <- model_ETS %>%
      forecast(h = weeks_to_forecast)
    
    # Clean the Df
    forecast_clean_ETS <- forecast_ETS %>%
      as_tibble() %>%
      select(Date = Date, `FC Volume` = .mean) %>%
      mutate(`FC Volume` = round(`FC Volume`, 0))
    
    forecast_clean_ARIMA <- forecast_clean_ARIMA %>%
      mutate(Model = "ARIMA")
    
    forecast_clean_ETS <- forecast_clean_ETS %>%
      mutate(Model = "ETS")
    
    # Add model label to historical data
    historical_data <- clean_data %>%
      select(Date, `FC Volume` = Volume) %>%
      mutate(Model = "Historical")
    
    combined_forecast <- bind_rows(historical_data, forecast_clean_ARIMA, forecast_clean_ETS)
    
    
    list(
      historical = clean_data,
      forecast = combined_forecast
    )
  })
  
  output$forecastPlot <- renderGirafe({
    req(forecast_data())
    
    df_hist <- forecast_data()$historical
    df_forecast <- forecast_data()$forecast
    
    validate(need(nrow(df_forecast) > 0, "Waiting for forecast data..."))
    
    # Calculate y-axis limits
    y_min <- floor(min(c(df_hist$Volume, df_forecast$`FC Volume`), na.rm = TRUE) / 500) * 500
    y_max <- ceiling(max(c(df_hist$Volume, df_forecast$`FC Volume`), na.rm = TRUE) / 500) * 500
    
    # Base plot
    p <- ggplot() +
      # Historical line
      geom_line_interactive(
        data = df_hist,
        aes(
          x = Date,
          y = Volume,
          tooltip = paste0("Date: ", format(Date, "%d-%b-%Y"), "\nVolume: ", Volume)
        ),
        color = "black",
        size = 1.2
      ) +
      
      # Forecast lines
      geom_line_interactive(
        data = df_forecast,
        aes(
          x = Date,
          y = `FC Volume`,
          color = Model,
          tooltip = paste0("Date: ", format(Date, "%d-%b-%Y"),
                           "\nModel: ", Model,
                           "\nFC Volume: ", `FC Volume`)
        ),
        size = 1.2
      ) +
      geom_point_interactive(
        data = df_forecast,
        aes(
          x = Date,
          y = `FC Volume`,
          color = Model,
          tooltip = paste0("Date: ", format(Date, "%d-%b-%Y"),
                           "\nModel: ", Model,
                           "\nFC Volume: ", `FC Volume`)
        ),
        size = 3
      ) +
      geom_smooth(
        data = df_hist,
        aes(x = Date, y = Volume),
        method = "lm",
        se = FALSE,
        color = "blue",
        linetype = "3313",
        size = 1.2
      )+
      scale_y_continuous(
        limits = c(y_min, y_max),
        breaks = seq(y_min, y_max, 500),
        expand = c(0.01, 0)
      ) +
      scale_x_date(date_labels = "%d-%b") +
      labs(
        title = "ARIMA vs ETS Forecast (with Historical Data)",
        x = "Date",
        y = "Volume",
        color = "Forecast Model"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(
          size = 12,
          angle = 45,
          hjust = 1,
          face = "bold"
        ),
        axis.title = element_text(size = 14, face = "bold")
      )
    
    # Render as interactive
    girafe(
      ggobj = p,
      width_svg = 10,
      height_svg = 5,
      options = list(
        opts_hover(css = "stroke-width:3;"),
        opts_tooltip(css = "background-color:white;padding:6px;border-radius:5px;"),
        opts_toolbar(saveaspng = FALSE)
      )
    )
  })
  
  output$download_forecast <- downloadHandler(
    filename = function() {
      paste0("forecastPlot", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(forecast_data()$forecast, file, row.names = FALSE)
    }
  ) 
}

shinyApp(ui = ui, server = server)
