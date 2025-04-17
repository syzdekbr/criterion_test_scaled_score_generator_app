###*** App to generate scaled score scoring parameters for credentialing exam, given known raw cut-score or known parameters from
###* previous exam.

library(shiny)
library(tidyverse)
options(scipen=999)

# dt_func -----------------------------------------------------------------

# Prettify column names
pretty_columns_func <- function(colnames){
  tools::toTitleCase(gsub("[_|.]", " ", colnames))
}

unescape_html <- function(str){ 
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>"))) 
}

## Function to print datatables in nice format with buttons to export

dt_func <- function(dat, colnames = "", caption ="", dom = 'Blfrtip', rownames_opt = FALSE, ...) {
  # Prettify column names 
  colnames <- case_when(
    colnames =="" ~ pretty_columns_func(colnames(dat)), 
    TRUE ~ colnames) 
  dat %>% 
    DT::datatable( 
      extensions = 'Buttons', 
      colnames = colnames, 
      caption = caption, 
      rownames = rownames_opt, 
      editable = TRUE, 
      ..., 
      options = list( 
        dom = dom, 
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), 
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All")),
        scrollX = TRUE
      ) 
    ) 
}

# UI ----------------------------------------------------------------------

# Tabs for different methods, depending on if raw cut score is known
ui <- fluidPage(

    # Application title
    titlePanel("Scaling Parameters Calculator"),
    p("This app will help provide information for scaling and equating. The main sections are:", br(),
      "1. From Known Raw Cut Score- Scaling equation is generated from input cut score", br(),
      "2. Chaining Forms with an Unknown Cut Score- Equate new forms to a form with a known cut score", br(),
      "3. From Known Scaled Score Parameters- Determine the cut score when scaling equations are known", br(),
      "4. Rescaling- Scale scores from one scale to another"
      ),
    
    tabsetPanel(
    ###*** Raw cut score known
      tabPanel("From Known Raw Cut Score",
               sidebarLayout(
                 sidebarPanel(
                   h2("Inputs"),
                   numericInput(
                     inputId = "known_raw_score_raw_score",
                     label = "Raw Cut Score",
                     value = 70
                   ),
                   numericInput(
                     inputId = "known_raw_score_max_raw_score",
                     label = "Maximum Raw Score",
                     value = 100
                   ),
                   numericInput(
                     inputId = "known_raw_score_scaled_cut_score",
                     label = "Desired Scaled Cut Score",
                     value = 70
                   ),
                   numericInput(
                     inputId = "known_raw_score_max_scaled_score",
                     label = "Maximum Scaled Score",
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
                   br(),
                   h4("In this section, you can input a cut score and scaling equations will be output. Enter the cut score,
                     as number correct needed to pass. The maximum scores are number of total items, for both raw and scaled scores."),
                   h2("Outputs"),
                   DT::DTOutput("known_raw_score_parameter_table"),
                   textOutput("known_raw_score_check")
                 )
               ) # end sidebarLayout
              ), # end tabPanel
      ###*** Chaining form with unknown angoff to previous form
      tabPanel("Chaining Forms with an Unknown Cut Score",
               sidebarLayout(
                 sidebarPanel(
                   h2("Inputs"),
                   numericInput(
                     inputId = "anchor_linear_slope",
                     label = "Anchor SLOPE1, or Linear Slope",
                     value = 0
                   ),
                   numericInput(
                     inputId = "anchor_quadratic_slope",
                     label = "Anchor SLOPE2, or Squared Slope",
                     value = 0
                   ),
                   numericInput(
                     inputId = "anchor_mean_p_value",
                     label = "Anchor Mean P-value (as decimal)",
                     value = .70,
                     min = 0, max = 1
                   ),
                   numericInput(
                     inputId = "target_mean_p_value",
                     label = "Target Mean P-value (as decimal)",
                     value = .70,
                     min = 0, max = 1
                   ),
                   numericInput(
                     inputId = "anchor_max_raw_score",
                     label = "Anchor Max Raw Score",
                     value = 100
                   ),
                   numericInput(
                     inputId = "target_max_raw_score",
                     label = "Target Max Raw Score",
                     value = 100
                   ),
                   # Can input any raw score and will output scaled score for it
                   numericInput(
                     inputId = "target_raw_score_to_check",
                     label = "Raw score to equate and convert to scaled score to check parameters",
                     value = 70
                   )
                 ), # End sidebarPanel
                 mainPanel(
                   br(),
                   h4("In this section, you can generate a scaling equation for a second form when a scaling equations is known for
                     some anchor form. Average difficulties (p-values) of the forms must be known and used to equate."),
                   h2("Outputs"),
                   DT::DTOutput("chaining_parameter_table"),
                   textOutput("target_raw_score_check")
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
                   br(),
                   h4("In this tab we can get the cut score for a form when scaling equations are known."),
                   h2("Outputs"),
                   radioButtons(
                     inputId = "select_root",
                     label = "Select raw cut score to use",
                     choices = NA
                   ),
                   textOutput("roots"),
                   textOutput("maximum_raw_score"),
                   p("System will estimate the maximum score from the equations entered. You can use this estimate or your own."),
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
              ), # end tabPanel
      ## Scale from one scale to another
      tabPanel("Rescaling",
               sidebarLayout(
                 sidebarPanel(
                   
                   h4("Starting Scale"),
                   numericInput("starting_scale_min_score", "Starting scale minimum score", 0),
                   numericInput("starting_scale_max_score", "Starting scale maximum score", 0),
                   numericInput("starting_scale_interval", "Starting scale interval", 0),
                   
                   h4("Target Scale"),
                   numericInput("target_scale_min_score", "Target scale minimum score", 0),
                   numericInput("target_scale_max_score", "Target scale maximum score", 0),
                   numericInput("target_scale_interval", "Target scale interval", 0),
                   textOutput("target_scale_validation_check"),
                   
                   h4("Scale score to convert"),
                   numericInput("scale_score_to_convert", "Starting scale score to convert", 0)
                 ),
                 mainPanel(
                   br(),
                   h4("In this tab, we can rescale one or many scores from one scale to another. 
                      Enter scale information and results will show below."),
                   h2("Results"),
                   h3("Target score to convert"),
                   
                   DT::dataTableOutput("starting_target_results"),
                   hr(),
                   br(),
                   
                   h3("Conversion table"),
                   DT::dataTableOutput("scaled_score_conversion_table")
                 )
               )
              )
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
    raw_scores <- rbind(c(input$known_raw_score_raw_score^2, input$known_raw_score_raw_score), 
               c(input$known_raw_score_max_raw_score^2, input$known_raw_score_max_raw_score))
    scale_scores <- c(input$known_raw_score_scaled_cut_score, input$known_raw_score_max_scaled_score)
    bind_rows(set_names(solve(raw_scores, scale_scores), c("Squared Slope", "Linear Slope"))) %>% 
      relocate(`Linear Slope`, .before = `Squared Slope`) # Client preferred order
  })
  
# Triggers to update for any inputs in calculation of parameters
  known_raw_score_triggers <- reactive({
    list(input$known_raw_score_raw_score, input$known_raw_score_max_raw_score, input$known_raw_score_scaled_cut_score,
         input$known_raw_score_max_scaled_score)
  })
  
# Calculate scaled scores and output
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
  
# Calculate scaled score from input raw score- QC check
  known_scale_score_calculator_func <- reactive({
    (input$known_raw_score_to_check ^ 2) * (known_raw_score_dat$parameters %>% select(`Squared Slope`) %>% pull) +
      input$known_raw_score_to_check * (known_raw_score_dat$parameters %>% select(`Linear Slope`) %>% pull)
  })
  
  output$known_raw_score_check <- renderText({
    paste("Estimated scaled score for given raw cut score using new scaling parameters:", round(known_scale_score_calculator_func(), 0))
  })

# Chaining with unknown Angoff --------------------------------------------

  # Reactive to hold scaled score parameters derived from raw cut score
  chaining_score_dat <- reactiveValues(
    parameters = tibble()
  )
  
  # Linear systems of equation, solve for parameters
  target_to_anchor_func <- reactive({
    if(!is.na(input$target_mean_p_value)){
      if(input$target_mean_p_value != 0){
        raw_cut = input$target_mean_p_value*input$target_max_raw_score
        anchor_raw_cut = input$anchor_mean_p_value * input$anchor_max_raw_score
        form_2_parameters <- rbind(c(raw_cut^2, raw_cut), 
                   c(input$target_max_raw_score^2, input$target_max_raw_score))
        anchor_parameters <- c(anchor_raw_cut, input$anchor_max_raw_score)
        bind_rows(set_names(solve(form_2_parameters, anchor_parameters), c("SLOPE2", "SLOPE1")))
      } else{
        tibble(
          SLOPE2 = 0,
          SLOPE1 = 0
        )
      }
    } else{
      tibble(
        SLOPE2 = 0,
        SLOPE1 = 0
      )
    }
  })
  
  # Triggers to update for any inputs in calculation of parameters
  chaining_raw_score_triggers <- reactive({
    list(input$target_mean_p_value, input$target_max_raw_score, input$anchor_mean_p_value,
         input$anchor_max_raw_score)
  })
  
  # Calculate scaled scores- yields 4 parameter model
  observeEvent(chaining_raw_score_triggers(),{
    chaining_score_dat$parameters <- target_to_anchor_func()
  })
  
  chaining_reactive <- reactive({
    tibble(
      SLOPE1 = chaining_score_dat$parameters$SLOPE1 * input$anchor_linear_slope,
      SLOPE2 = chaining_score_dat$parameters$SLOPE2 * input$anchor_linear_slope +
        (chaining_score_dat$parameters$SLOPE1^2) * input$anchor_quadratic_slope,
      SLOPE3 = 2 * chaining_score_dat$parameters$SLOPE1 * 
        chaining_score_dat$parameters$SLOPE2 * input$anchor_quadratic_slope,
      SLOPE4 = (chaining_score_dat$parameters$SLOPE2^2) * input$anchor_quadratic_slope
    )
  })
  
  output$chaining_parameter_table <- DT::renderDT({
  chaining_reactive()
  })
  
  # Calculate scaled score from input raw score
  chaining_scale_score_calculator_func <- reactive({
    (input$target_raw_score_to_check ^ 4) * (chaining_reactive() %>% select(SLOPE4) %>% pull) +
      (input$target_raw_score_to_check ^ 3) * (chaining_reactive() %>% select(SLOPE3) %>% pull) +
    (input$target_raw_score_to_check ^ 2) * (chaining_reactive() %>% select(SLOPE2) %>% pull) + 
      input$target_raw_score_to_check * (chaining_reactive() %>% select(SLOPE1) %>% pull)
  })

  output$target_raw_score_check <- renderText({
    paste("Estimated scaled score for given raw cut score using new scaling parameters:", round(chaining_scale_score_calculator_func(), 0))
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
  

# Rescaling -----------------------------------------------

  ## Check that all inputs for scale conversion are valid
validation_check_function <- function(min_score, max_score, interval, scale){
  
  if(min_score < 0 | max_score <= 0 | interval <= 0){
    validate(paste(scale, "min inputs must be non-negative and others positive"))
  }
  if(min_score%%1 != 0 | max_score%%1 != 0 | interval%%1 !=0){
    validate(paste(scale, "inputs must be integers"))
  }
  if(max_score <= min_score){
    validate(paste(scale, "minimum score must be less than maximum"))
  }
  if(interval > max_score){
    validate(paste(scale, "interval must not be greater than maximum"))
  }
}
  
  ## Rescale starting scale score inputs to output scale scores- 1. Calculate what fraction of starting scale input scale is,
  ## 2. Convert that to the fraction of target scale. 3. Add to minimum of starting scale, 4. Round to desired interval precision
  scale_score_conversion_func <- function(
    any_input_value,
    starting_scale_min_score,
    starting_scale_max_score,
    target_scale_min_score,
    target_scale_max_score,
    target_scale_interval){
    
  # 1. Calculate what fraction of starting scale input scale is
    fraction_above_min_starting <- 
      (any_input_value - starting_scale_min_score) / (starting_scale_max_score - starting_scale_min_score)
    
  # 2. Convert that to the fraction of target scale.
    unrounded_above_min_target <- 
      fraction_above_min_starting * (target_scale_max_score - target_scale_min_score) 
    
  # 3. Add to minimum of starting scale
    round_to_target_interval <- round(unrounded_above_min_target / target_scale_interval, 0) * target_scale_interval
    
  # 4. Round to desired interval precision
    converted_scale_score <- round_to_target_interval + target_scale_min_score 
    
  # Output
    tibble(
      any_input_value,
      starting_scale_min_score,
      starting_scale_max_score,
      target_scale_min_score,
      target_scale_max_score,
      target_scale_interval,
      converted_scale_score
    )
  }
  
  ## Output results of one score to convert
  output$starting_target_results <- DT::renderDataTable({
  ## Validation checks for all starting and target values
    pmap(
      .l = list(
          min_score = c(input$starting_scale_min_score, input$target_scale_min_score), 
          max_score = c(input$starting_scale_max_score, input$target_scale_max_score), 
          interval = c(input$starting_scale_interval, input$target_scale_interval), 
          scale = c("Starting", "Target")
      ),
      .f = validation_check_function
    )
    DT::datatable(
      scale_score_conversion_func(
        any_input_value = input$scale_score_to_convert,
        starting_scale_min_score = input$starting_scale_min_score,
        starting_scale_max_score = input$starting_scale_max_score,
        target_scale_min_score = input$target_scale_min_score,
        target_scale_max_score = input$target_scale_max_score,
        target_scale_interval = input$target_scale_interval
      ) %>% 
        rename_with(~tools::toTitleCase(gsub("_", " ", .x))),
      options = list(dom = 't')
    )
  })
  
  ## Function to rescale, used to input many scores in following
  generate_all_target_scaled_scores_table_func <- function(){
    map_dfr(
    # Sequences through all valid starting score values to generate target scale score
      .x = seq(input$starting_scale_min_score, input$starting_scale_max_score, input$starting_scale_interval),
      .f = ~
          scale_score_conversion_func(
            any_input_value = .x,
            starting_scale_min_score = input$starting_scale_min_score,
            starting_scale_max_score = input$starting_scale_max_score,
            target_scale_min_score = input$target_scale_min_score,
            target_scale_max_score = input$target_scale_max_score,
            target_scale_interval = input$target_scale_interval
          ) %>% 
            select(
              any_input_value, 
              converted_scale_score
            )
    )
  }
  ## All scores output
  output$scaled_score_conversion_table <- DT::renderDataTable({
    pmap(
      .l = list(
        min_score = c(input$starting_scale_min_score, input$target_scale_min_score), 
        max_score = c(input$starting_scale_max_score, input$target_scale_max_score), 
        interval = c(input$starting_scale_interval, input$target_scale_interval), 
        scale = c("Starting", "Target")
      ),
      .f = validation_check_function
    )
    dt_func(generate_all_target_scaled_scores_table_func(), caption = "All converted scale score output")
  })
  
} # Close server

# Run the application 
shinyApp(ui = ui, server = server)
