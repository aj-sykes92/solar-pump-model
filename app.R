library(shiny)
library(tidyverse)

source("model-functions/solar-pump-model-functions.R")

Dat_sim <- read_rds("model-data/simulation-base-data.rds") %>%
  build_sim(mcN = 5, seed = 2605)

ui <- fluidPage(
  
  # app title ----
  titlePanel("Solar pump, pond and river system simulation model"),
  # app subtitle
  h4(HTML("Prepared for Helen Sykes and Andrew McKeever, 12-05-2020<br/> ")),
  
  # sidebar layout
  sidebarLayout(
    
    # sidebar panel
    sidebarPanel(
      
      # calculate model
      titlePanel(title = "Run model"),
      actionButton(inputId = "run_model",
                   label = "Click to run simulation"),
      
      # data input
      titlePanel(title = "Data input"),
      
      h4(HTML(" <br/><b>Solar panel</b>")),
      
      sliderInput(inputId = "solar_pv_eff",
                  label =  "Solar panel efficiency (kWp per m2)",
                  min = 0.1, max = 0.2, value = 0.125, step = 0.001),
      
      sliderInput(inputId = "solar_pv_m2",
                  label = "Solar panel size (m2)",
                  min = 0.1, max = 10, value = 1, step = 0.1),
      
      sliderInput(inputId = "control_eff",
                  label = "Solar panel controller efficiency (%)",
                  min = 70, max = 100, value = 97, step = 0.5),
      
      h4(HTML(" <br/><b>Battery</b>")),
      
      sliderInput(inputId = "battery_cap",
                  label = "Battery capacity (Watt hours)",
                  min = 0, max = 3000, value = 500, step = 10),
      
      h4(HTML(" <br/><b>Pump</b>")),
      
      sliderInput(inputId = "pump_rating",
                  label = "Pump rating (Watts)",
                  min = 10, max = 1000, value = 20, step = 5),
      
      sliderInput(inputId = "pump_eff",
                  label = "Pump efficiency (%)",
                  min = 1, max = 100, value = 80, step = 1),
      
      h4(HTML(" <br/><b>Pipe system</b>")),
      
      sliderInput(inputId = "pump_head",
                  label = "Pumping head (m)",
                  min = 0.1, max = 10, value = 2, step = 0.1),
      
      sliderInput(inputId = "pipe_diameter",
                  label = "Pipe diameter (mm)",
                  min = 10, max = 200, value = 25, step = 1),
      
      sliderInput(inputId = "pipe_length",
                  label = "Pipe length (m)",
                  min = 0.1, max = 50, value = 16.2, step = 0.1),
      
      h4(HTML(" <br/><b>Top pond</b>")),
      
      sliderInput(inputId = "outflow_height",
                  label = "Height of outflow relative to pond bottom (m)",
                  min = 0.01, max = 0.5, value = 0.1, step = 0.01),
      
      sliderInput(inputId = "outflow_diameter",
                  label = "Diameter of outflow orifice (mm)",
                  min = 10, max = 200, value = 50, step = 10),
      
      sliderInput(inputId = "top_L",
                  label = "Top pond length (m)",
                  min = 0.1, max = 10, value = 1.5, step = 0.1),
      
      sliderInput(inputId = "top_W",
                  label = "Top pond width (m)",
                  min = 0.1, max = 10, value = 0.8, step = 0.1),
      
      sliderInput(inputId = "top_D",
                  label = "Top pond depth (m)",
                  min = 0.1, max = 3, value = 0.3, step = 0.1),
      
      h4(HTML(" <br/><b>Bottom pond(s)</b>")),
      
      sliderInput(inputId = "bottom_m3",
                  label = "Bottom pond(s) combined volume (m3)",
                  min = 0.1, max = 5, value = 0.51, step = 0.01),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      h4(HTML("Daily hours with river flow")),
      plotOutput(outputId = "flow_hours_plot"),
      
      h4(HTML("Hourly river flow rate, by month")),
      plotOutput(outputId = "hourly_flow_plot"),
      
      h4(HTML("Hourly water volume in ponds, by month")),
      plotOutput(outputId = "water_vol_plot"),
      
      h4(HTML("Hourly means (std dev)")),
      tableOutput(outputId = "hourly_means_table")
      
    )
  )
)

server <- function(input, output){
  
  # default reactiveValues for simulation
  # run this now to avoid storing large files in repo
  sim <- reactiveValues(model = read_rds("model-data/simulation-base-data.rds") %>%
                          build_sim(mcN = 5,
                                    seed = 2605) %>%
                          run_sim()
  )
  
  # run model
  observeEvent(input$run_model, {
    
    sim$model <- sim$model %>%
      run_sim(solar_pv_eff = input$solar_pv_eff,
              solar_pv_m2 = input$solar_pv_m2,
              control_eff = input$control_eff,
              battery_cap = input$battery_cap,
              pump_rating = input$pump_rating,
              pump_eff = input$pump_eff,
              pump_head = input$pump_head,
              pipe_diameter = input$pipe_diameter,
              pipe_length = input$pipe_length,
              outflow_height = input$outflow_height,
              outflow_diameter = input$outflow_diameter,
              bottom_m3 = input$bottom_m3,
              top_L = input$top_L,
              top_W = input$top_W,
              top_D = input$top_D)
    
  })
  
  # flow hours plot
  output$flow_hours_plot <- renderPlot({
    flow_hours_plot(sim$model)
  })
  
  # hourly flow rate plot
  output$hourly_flow_plot <- renderPlot({
    hourly_flow_plot(sim$model)
  })
  
  # water vol plot
  output$water_vol_plot <- renderPlot({
    water_vol_plot(sim$model)
  })
  
  # hourly means table
  output$hourly_means_table <- renderTable({
    hourly_means_table(sim$model)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
