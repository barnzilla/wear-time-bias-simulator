# Load dependencies
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(readr)
library(scales)
library(shiny)
library(shinycssloaders)

# Create custom functions
custom_round <- function(x, digits) {
  
  return(format(round(x, digits), nsmall = digits))
  
}

get_mvpa <- function(mvpa_p, wear_time_p, minutes_to_sample = 1440) {
  
  # Create a time vector (HH:MM) starting at midnight and extending to minutes_to_sample
  time_vector <- as.POSIXct("23:59", format = "%H:%M") + (1:minutes_to_sample * 60)
  time_vector <- format(time_vector, format = "%H:%M")
  
  # Add time vector to a data frame
  df <- tibble(time = time_vector)
  
  # Randomly sample MVPA, excluding possibility of MVPA between 12am and 5am
  df$mvpa <- c(
    rep(0, 5 * 60),
    sample(
      x = 1:0,
      size = minutes_to_sample - 5 * 60,
      replace = TRUE,
      prob = c(mvpa_p, 1 - mvpa_p)
    )
  )
  
  # Randomly sample wear time
  df$wear_time <- sample(
    x = 1:0,
    size = minutes_to_sample,
    replace = TRUE,
    prob = c(wear_time_p, 1 - wear_time_p)
  )
  
  # Return the sampled data
  return(df)
  
}

get_mvpa_bias <- function(df) {
  
  # Compute aggregate stats
  output <- tibble(
    mvpa = sum(df$mvpa),
    wear_time = sum(df$wear_time),
    mvpa_estimate = sum(df$mvpa[df$wear_time == 1]),
    bias = sum(df$mvpa[df$wear_time == 1]) - sum(df$mvpa),
    bias_percentage = (sum(df$mvpa[df$wear_time == 1]) - sum(df$mvpa)) / sum(df$mvpa) * 100
  )
  
  # Return the results
  return(output)
  
}

# User interface
ui <- fluidPage(
  
  # App title
  titlePanel(""),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      uiOutput("number_of_participants"),
      uiOutput("mvpa_p"),
      uiOutput("wear_time_p"),
      uiOutput("acceptable_bias"),
      actionButton("run_simulations", "Run simulations"),
      width = 3
    ),
    
    # Render a world map
    mainPanel(
      tags$style(
        HTML(
          "a, a:hover { color: #75aadb; }",
          ".dataTables_length label, #DataTables_Table_0_filter label { font-weight: normal; }",
          "table.dataTable thead tr { border-color: none; border: 0; background: #75aadb; color: white; }",
          ".dataTables_filter input { border: 1px solid #dcdcdc; }",
          ".dataTables_filter label { font-weight: normal; }",
          "table.dataTable thead th, table.dataTable tfoot td, table.dataTable.no-footer { border-bottom: none; }",
          "#run_simulations { background-color: #75aadb; color: white; }"
        )
      ),
      tabsetPanel(type = "tabs",
        tabPanel("Scatter plot", br(), br(), plotlyOutput("scatter_plot") %>% withSpinner(color = "#75aadb"), br(), br(), br()),
        tabPanel("Simulation results", br(), br(), DTOutput("simulation_results") %>% withSpinner(color = "#75aadb"), br(), br()),
        tabPanel("Search grid", br(), br(), DTOutput("search_grid") %>% withSpinner(color = "#75aadb"), br(), br()),
        tabPanel("Participant 0", br(), br(), DTOutput("participant0") %>% withSpinner(color = "#75aadb"), br(), br())
      ),
      width = 9
    )
    
  )
)

# Server
server <- function(input, output) {
  
  # Cached object
  cached <- reactiveValues()
  
  # Menu items
  output$number_of_participants <- renderUI({
    sliderInput("number_of_participants", "Number of participants", min = 100, max = 10000, value = 100, step = 100)
  })
  
  output$mvpa_p <- renderUI({
    sliderInput("mvpa_p", "MVPA probability", min = 0.01, max = 1, value = c(0.01, 0.05), step = 0.01)
  })
  
  output$wear_time_p <- renderUI({
    sliderInput("wear_time_p", "Wear time probability", min = 0, max = 1, value = c(0.25, 1), step = 0.01)
  })
  
  output$acceptable_bias <- renderUI({
    sliderInput("acceptable_bias", "Acceptable bias (%)", min = 0, max = 100, value = 3, step = 0.1)
  })
  
  # Get simulation data
  simulations <- eventReactive(input$run_simulations, {
    
    # Simulation settings
    participants_to_sample <- input$number_of_participants
    minutes_to_sample <- 1440
    mvpa_p_values <- seq(from = input$mvpa_p[1], to = input$mvpa_p[2], by = 0.001)
    wear_time_p_values <- seq(from = input$wear_time_p[1], to = input$wear_time_p[2], by = 0.001)
    
    # Run simulations
    simulations <- replicate(
      n = 1,
      expr = {
        
        # Create a grid of scenarios/participants to run 
        cached$search_grid <- tibble(
          participant = 1:participants_to_sample,
          mvpa_p = sample(
            x = mvpa_p_values,
            size = participants_to_sample,
            replace = TRUE,
            prob = rep(1 / length(mvpa_p_values), length(mvpa_p_values))
          ),
          wear_time_p = sample(
            x = wear_time_p_values,
            size = participants_to_sample,
            replace = TRUE,
            prob = rep(1 / length(wear_time_p_values), length(wear_time_p_values))
          )
        )
        
        # Compute aggregrate statistics
        output <- bind_rows(
          lapply(1:nrow(cached$search_grid), function(x) {
            output <- get_mvpa_bias(
              get_mvpa(
                cached$search_grid$mvpa_p[x],
                cached$search_grid$wear_time_p[x],
                minutes_to_sample
              )
            )
          })
        )
        
        
      },
      simplify = FALSE
    )
    
  })
  
  # Scatter plot
  output$scatter_plot <- renderPlotly({
    
    if(is.null(simulations())) {
      return(NULL)
    }
  
    p <- ggplot(simulations()[[1]], aes(x = wear_time / 60, y = bias_percentage)) + 
      scale_x_continuous(breaks = 1:24) +
      geom_point(aes(alpha = 0.3)) + xlab("Wear time (hours)") + ylab("Bias (%)") + theme_minimal() +
      theme(legend.position = "none")
    
    if(sum(simulations()[[1]]$bias_percentage >= input$acceptable_bias * -1) > 0) {
      p <- p +
        geom_point(
          data = simulations()[[1]] %>% filter(bias_percentage >= input$acceptable_bias * -1),
          aes(x = wear_time / 60, y = bias_percentage, color = "red", alpha = 0.3)
        ) 
    }
    
  })
  
  # Simulation results
  output$simulation_results <- renderDT(
    { if(is.null(simulations()[[1]])) {
      return(NULL)
    } else {
      simulations()[[1]] %>%
        rename(
          `MVPA (population parameter)` = mvpa,
          `Wear time` = wear_time,
          `MVPA estimate` = mvpa_estimate,
          `Bias (absolute)` = bias,
          `Bias (relative)` = bias_percentage
        ) %>% mutate(
          across(5, custom_round, 2)
        )
    }
    },
    options = list(
      sDom = '<"top"if>rt<"bottom"p><"clear">',
      pageLength = 100,
      lengthMenu = c(5, 10),
      columnDefs = list(list(className = "dt-center", targets = 0:4)),
      ordering = TRUE
    ),
    rownames = FALSE
  )
  
  # Search grid
  output$search_grid <- renderDT(
    { 
      if(is.null(cached$search_grid)) {
        return(NULL)
      } else {
        cached$search_grid %>% 
          rename(
            Participant = participant,
            `MVPA probability` = mvpa_p,
            `Wear time probability` = wear_time_p
          ) %>% 
            mutate(across(2:3, custom_round, 3))
      }
    },
    options = list(
      sDom = '<"top"if>rt<"bottom"p><"clear">',
      pageLength = 100,
      lengthMenu = c(5, 10),
      columnDefs = list(list(className = "dt-center", targets = 0:2)),
      ordering = TRUE
    ),
    rownames = FALSE
  )
  
  # Participant 0
  output$participant0 <- renderDT(
    { 
      get_mvpa(max(input$mvpa_p), max(input$wear_time_p)) %>%
        rename(
          `Time of day` = time,
          `MVPA (population parameter)` = mvpa,
          `Wear time` = wear_time
        )
    },
    options = list(
      sDom = '<"top"if>rt<"bottom"p><"clear">',
      pageLength = 1440,
      lengthMenu = c(5, 10),
      columnDefs = list(list(className = "dt-center", targets = 0:2)),
      ordering = TRUE
    ),
    rownames = FALSE
  )
  
}

# Run app
shinyApp(ui = ui, server = server)