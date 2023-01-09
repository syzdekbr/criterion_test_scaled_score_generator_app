###*** App to generate scaled score scoring parameters for credentialing exam, given known raw cut-score or known parameters from
###* previous exam.

library(shiny)
library(tidyverse)

# UI ----------------------------------------------------------------------

# Two tabs, one for if raw cut score is known, another if scaling parameters are known
ui <- fluidPage(

    # Application title
    titlePanel("Scaling Parameters Calculator"),
    
    tabsetPanel(
    ###*** Raw cut score known
      tabPanel("From Known Raw Score",
               sidebarLayout(
                 sidebarPanel(
                   h2("Inputs"),
                   numericInput(
                     inputId = "known_raw_score_raw_score",
                     label = "Raw score",
                     value = 70
                   ),
                   numericInput(
                     inputId = "known_raw_score_max_raw_score",
                     label = "Maximum raw score",
                     value = 100
                   ),
                   numericInput(
                     inputId = "known_raw_score_scaled_cut_score",
                     label = "Desired cut score",
                     value = 70
                   ),
                   numericInput(
                     inputId = "known_raw_score_max_scaled_score",
                     label = "Maximum scaled score",
                     value = 100
                   ),
                   # Can input any raw score and will output scaled score for it
                   numericInput(
                     inputId = "known_raw_score_to_check",
                     label = "Raw score to convert to scaled score to check parameters",
                     value = 70
                   )
                 ), # End sidebarPanel
                 mainPanel(
                   h2("Outputs"),
                   DT::DTOutput("known_raw_score_parameter_table"),
                   textOutput("known_raw_score_check")
                 )
               ) # end sidebarLayout
              ), # end tabPanel
      # Known scaled score parameters
      tabPanel("From Known Scaled Score Parameters",
               sidebarLayout(
                 sidebarPanel(
                   h2("Inputs"),
                   shinyWidgets::autonumericInput(
                     inputId = "known_scale_squared",
                     label = "Slope squared",
                     decimalCharacter = ".",
                     value = 0,
                     decimalPlaces = 8
                   ),
                   shinyWidgets::autonumericInput(
                     inputId = "known_scale",
                     label = "Slope linear",
                     decimalCharacter = ".",
                     value = 0,
                     decimalPlaces = 8
                   ),
                   numericInput(
                     inputId = "known_scaled_cut_score",
                     label = "Known scaled cut_score",
                     value = 70
                   ),
                   numericInput(
                     inputId = "maximum_scaled_score",
                     label = "Maximum scaled score",
                     value = 100
                   ),
                   numericInput(
                     inputId = "desired_scaled_cut_score",
                     label = "Desired scaled cut score",
                     value = 75
                   )
                 ),
                 # Output
                 mainPanel(
                   h2("Outputs"),
                   radioButtons(
                     inputId = "select_root",
                     label = "Select raw cut score to use",
                     choices = NA
                   ),
                   textOutput("roots"),
                   textOutput("maximum_raw_score"),
                   radioButtons(
                     inputId = "max_score_to_use",
                     label = "Use estimated max score or input max score",
                     choices = c("Estimated", "Input")
                   ),
                   numericInput(
                     inputId = "input_max_score",
                     label = "Input maximum raw score",
                     value = 50
                   ),
                   DT::dataTableOutput("new_scaling_parameters"),
                   textOutput("score_check")
                 ) # End MainPanel
               ) # end sidebarLayout
              ) # end tabPanel
    ) # End tabsetPanel
) # end UI

# Server ------------------------------------------------------------------

server <- function(input, output, session) {

# Known Raw Score Tab -----------------------------------------------------

# Reavtive to hold scaled score parameters derived from raw cut score
  known_raw_score_dat <- reactiveValues(
    parameters = tibble()
  )
  
# Linear systems of equation, solve for parameters
  known_raw_score_parameter_func <- reactive({
    A <- rbind(c(input$known_raw_score_raw_score^2, input$known_raw_score_raw_score), 
               c(input$known_raw_score_max_raw_score^2, input$known_raw_score_max_raw_score))
    B<- c(input$known_raw_score_scaled_cut_score, input$known_raw_score_max_scaled_score)
    bind_rows(set_names(solve(A,B), c("Squared Slope", "Linear Slope"))) %>% 
      relocate(`Linear Slope`, .before = `Squared Slope`) # Client preferred order
  })
  
# Triggers to update for any inputs in calculation of parameters
  known_raw_score_triggers <- reactive({
    list(input$known_raw_score_raw_score, input$known_raw_score_max_raw_score, input$known_raw_score_scaled_cut_score,
         input$known_raw_score_max_scaled_score)
  })
  
# Calculate scaled scores
  observeEvent(known_raw_score_triggers(),{
    known_raw_score_dat$parameters <- known_raw_score_parameter_func()
  })
  
  output$known_raw_score_parameter_table <- DT::renderDT({
    known_raw_score_dat$parameters
  })
  
# Update raw score to check input to match raw cut score
  observeEvent(input$known_raw_score_raw_score,{
    updateNumericInput(session, "known_raw_score_to_check", label = "Raw score to check scaled scoring parameters",
                       value = input$known_raw_score_raw_score)
  })
  
# Calculate scaled score from input raw score
  known_scale_score_calculator_func <- reactive({
    (input$known_raw_score_to_check ^ 2) * (known_raw_score_dat$parameters %>% select(`Squared Slope`) %>% pull) +
      input$known_raw_score_to_check * (known_raw_score_dat$parameters %>% select(`Linear Slope`) %>% pull)
  })
  
  output$known_raw_score_check <- renderText({
    paste("Estimated scaled score for given raw cut score using new scaling parameters:", round(known_scale_score_calculator_func(), 0))
  })
  
# Known Scaled Score Tab --------------------------------------------------
###*** Calculates scaled scoring parameters from known parameters for another cut score. Calculates raw cut score, then new parameters.
###* Will calculate raw maximum score estimate or allow input of known maximum

# Reactives to hold calculated scaled scoring components
  dat <- reactiveValues(
    roots = NA,
    estimated_max_raw_score = NA,
    max_raw_score = NA,
    new_scaling_parameters = tibble(),
    estimated_cut_score = NA
  )

# Quadratic function to solve for raw cut score
  quadratic <- function(a, b, scaled_cut_score){
    scaled_cut_score <- - scaled_cut_score
    discriminant <- (b^2) - (4 * a * scaled_cut_score)
    req(discriminant)
    root <- if (discriminant < 0){
      NA
    } else{
      x_int_plus <- (-b + sqrt(discriminant)) / (2*a)
      x_int_neg <- (-b - sqrt(discriminant)) / (2*a)
      c(as.numeric(format(round(x_int_plus, 5), nsmall = 5)),
      as.numeric(format(round(x_int_neg, 5), nsmall = 5)))
    }
  }
  
# Inputs that trigger update of scaled scoring parameters
  update_scaled_score_event <- reactive({
    list(input$known_scale_squared, input$known_scale, input$known_scaled_cut_score, input$maximum_scaled_score,
         input$max_score_to_use, input$input_max_score, input$desired_scaled_cut_score)
  })
  
# Raw cut score
  estimated_cut_score <- function(){
    sq_slope <- dat$new_scaling_parameters %>% select(`Squared Slope`) %>% pull
    linear_slope <- dat$new_scaling_parameters %>% select(`Linear Slope`) %>% pull
    estimated_cut_score_numeric <-sq_slope * dat$roots^2 + linear_slope * dat$roots
    quadratic(sq_slope, linear_slope, estimated_cut_score_numeric)
  }
  
# Display both roots of quadratic so user chooses raw score that is sensible
  observeEvent(update_scaled_score_event(),{
    val <- quadratic(input$known_scale_squared, input$known_scale, input$known_scaled_cut_score)
    updateRadioButtons(session, "select_root", choices = val)
  })

# Update all components of scaled score calculation and output raw cut score
  observe({
  # Calculate raw cut score
    dat$roots <- as.numeric(input$select_root) 
  # Calculate maximum raw score
    max_raw <- quadratic(input$known_scale_squared, input$known_scale, input$maximum_scaled_score)
  # Take the estimated maximum raw score (roots) that is closest to the raw cut score estimate
    dat$estimated_max_raw_score <- max_raw[abs(max_raw - dat$roots) == min(abs(max_raw - dat$roots))]
  # Choose either the estimate or allow input maximum score
    dat$max_raw_score <- ifelse(input$max_score_to_use == "Estimated",
                                dat$estimated_max_raw_score,
                                input$input_max_score)
  # Calculate raw cut score
    A <- rbind(c(dat$roots^2, dat$roots), c(dat$max_raw_score^2, dat$max_raw_score))
    B<- c(input$desired_scaled_cut_score, input$maximum_scaled_score)
  # Estimate scoring parameters
    dat$new_scaling_parameters <- bind_rows(set_names(solve(A,B), c("Squared Slope", "Linear Slope"))) %>% 
      relocate(`Linear Slope`, .before = `Squared Slope`)
    dat$estimated_cut_score <- estimated_cut_score()
  })
  
  output$roots <- renderText({
    paste("Estimated raw cut score", round(dat$roots, 0))
  })
  
  output$maximum_raw_score <- renderText({
    paste("Estimated raw maximum score", round(dat$estimated_max_raw_score, 0))
  })
  
  output$new_scaling_parameters <- DT::renderDT({
    dat$new_scaling_parameters
  })
  
  output$score_check <- renderText({
    paste("Estimated raw score for target scaled score using new scaling parameters:", dat$estimated_cut_score)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
