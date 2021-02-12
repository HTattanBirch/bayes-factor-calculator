################
# Contributors #
################
# R Shiny calculator: Harry Tattan-Birch
# Amendments: Robert West, Jamie Brown, Zoltan Dienes
# Bf function: Bence Palfi, Zoltan Dienes
# Version: 1.0


################################################
#                                              #
#           Section 1. Bf function             #
#                                              #
################################################


Bf<-function(sd, obtained, dfdata = 1, likelihood = c("normal", "t"), modeloftheory= c("normal","t","cauchy", "uniform") ,lower =0, upper=1, modeoftheory = 0, scaleoftheory = 1, dftheory = 1, tail = 2)
{
  if(likelihood=="normal"){
    dfdata=10^10
  }
  if(modeloftheory=="normal"){
    dftheory = 10^10
  } else if(modeloftheory=="cauchy"){
    dftheory = 1
  }
  area <- 0
  normarea <- 0
  if(modeloftheory=="uniform"){
    theta <- lower
    range <- upper - lower
    incr <- range / 2000
    for (A in -1000:1000){
      theta <- theta + incr
      dist_theta <- 1 / range
      height <- dist_theta * dt((obtained-theta)/sd, df=dfdata)
      area <- area + height * incr
    }
    LikelihoodTheory <- area
  }else{
    theta <- modeoftheory - 8 * scaleoftheory
    incr <- scaleoftheory/200
    for (A in -1600:1600){
      theta <- theta + incr
      dist_theta <- dt((theta-modeoftheory)/scaleoftheory, df=dftheory)
      
      # Changed identical(tail, 1) to tail == 1 as needed for shiny
      if(tail==1){
        if (theta <= modeoftheory){
          dist_theta <- 0
        } else {
          dist_theta <- dist_theta * 2
        }
      }
      height <- dist_theta * dt((obtained-theta)/sd, df = dfdata)
      area <- area + height * incr
      normarea <- normarea + dist_theta*incr
    }
    LikelihoodTheory <- area/normarea
  }
  LikelihoodNull <- dt(obtained/sd, df = dfdata)
  BayesFactor <- LikelihoodTheory/LikelihoodNull
  
  BayesFactor
}




################################################
#                                              #
#            Section 2. Load Packages          #
#                                              #
################################################

library(shiny) 
library(shinythemes) # Used to provide theme
library(shinyjs) # Used to hide/show specific options
library(shinydashboard) 
library(shinyBS)

library(knitr)





################################################
#                                              #
#        Section 3. Side panel Inputs          #
#                                              #
################################################

# In this section, we create the user interface for entering values into
# the Bayes factor calculator. For simplicity, the more detailed options 
# have been hidden by default. Pressing a button called "detailed options" 
# shows them.

# Each input has a descriptive "tooltip", which provides guidance about
# how it should be used. For example, the toolkit for the "mean difference" 
# button states how this should be the mean difference found in the study,
# or B coefficient from a regression or GLM.


side_inputs_ui <- sidebarPanel(
  useShinyjs(), # Use javascript from shinyjs
             
  ###  INPUTS  ###
             
      # Detailed Options: Provide detailed options, not default normal centered on zero
      checkboxInput(inputId = "detail", label = "Detailed options",value=FALSE),
             HTML("<h4>Sample results</h4>"),
             
      # Likelihood: Select if sample t- or normal distributed likelihood function
      conditionalPanel('input.detail === true  && input.odds_question === false',      # Only show if detailed options selected
                radioButtons("likelihood", "Likelihood Distribution", 
                                     c("Normal" = "normal", "Student's t" = "t"))),
  
      # OR question: Select if using OR/RR to switch to these boxes
      checkboxInput(inputId = "odds_question", label = tagList(
               tags$span("Using an odds/risk ratio?"), 
               tags$span(icon("info-circle"), id = "odds_question_icon")),value=FALSE),
      
  # Show OR/RR question if OR question selected
  conditionalPanel('input.odds_question === true', box(id="odds_box",width="800px",
                                                       
      # OR mean: Enter OR, which will be converted to log-odds 
      numericInput("odds", label = tagList(
               tags$span("Odds/risk ratio"), 
               tags$span(icon("info-circle"), id = "odds_icon")), value = 1),
      
      # Upper OR: Enter upper CI of OR, to used with OR to calculate log-odds SE
      numericInput("odds_upper", label = tagList(
               tags$span("Upper 95%CI of odds/risk ratio"), 
               tags$span(icon("info-circle"), id = "odds_upper_icon")), value = 1))),
  
  # Show mean difference and SE box if OR question not selected
  conditionalPanel('input.odds_question === false', box(id="mean_box",width="800px",
                                                        
      # Mean difference: Box to enter mean difference
      numericInput("obtained", label = tagList(
               tags$span("Mean difference"), 
               tags$span(icon("info-circle"), id = "obtained_icon")),value = 0),
      
      # SE: Box to enter standard error in mean difference
      numericInput("sd", label = tagList(
               tags$span("Standard error"), 
               tags$span(icon("info-circle"), id = "sd_icon")), value = 0))),   
  
      # Degrees of freedom: Show degrees of freedom if t-distributed likelihood 
      conditionalPanel('input.likelihood == "t"', box(id="detailed",width="800px",
               numericInput("dfdata", label = tagList(
                           tags$span("Degrees of freedom"), 
                           tags$span(icon("info-circle"), id = "dfdata_icon")), value = 1))),
  
  # Heading 2: Section specifying alternative hypothesis
  HTML("<br> <h4>Alternative hypothesis</h4>"),
  
      # H1 Model: Selection for H1 distribution, only if detailed options selected
      conditionalPanel('input.detail === true',radioButtons("modeloftheory", tagList(
               tags$span("Model of hypothesis"), 
               tags$span(icon("info-circle"), id = "modeloftheory_icon")), 
               c("Normal" = "normal", 
                 "Student's t" = "t", 
                 "Cauchy" = "cauchy", 
                 "Uniform" = "uniform"), selected = c("normal"))),
  
  # Show uniform options if selected 
  conditionalPanel('input.modeloftheory == "uniform"',
                   
                   
      # No OR: uniform when there is no OR option selected 
      conditionalPanel('input.odds_question === false', box(id="uniformshow",width="800px",
                                                          
      # Uniform lower: Lower boundary of uniform model of H1
               numericInput("lower", label = tagList(
                          tags$span("Lower boundary of hypothesis"), 
                          tags$span(icon("info-circle"), id = "lower_icon")), value = 0),
      
      # Uniform upper: Upper boundary of uniform model of H1
               numericInput("upper", label = tagList(
                          tags$span("Upper boundary of hypothesis"), 
                          tags$span(icon("info-circle"), id = "upper_icon")), value = 1))),
      
      conditionalPanel('input.odds_question === true', box(id="odds_uniform_hypothesis",width="800px",
               # Uniform lower: Lower boundary of uniform model of H1
               numericInput("or_lower", label = tagList(
                          tags$span("Lower odds/risk ratio of hypothesis"), 
                          tags$span(icon("info-circle"), id = "or_lower_icon")), value = 1),
                                                           
               # Uniform upper: Upper boundary of uniform model of H1
               numericInput("or_upper", label = tagList(
                          tags$span("Upper odds/risk ratio of hypothesis"), 
                          tags$span(icon("info-circle"), id = "or_upper_icon")), value = 2)))),
                                                           
  
  # Show options for normal, t or Cauchy if selected
  conditionalPanel('input.modeloftheory != "uniform"',box(id="normtcaushow",width="800px",
                                                          
      # Mode H1: Only show mode of H1 in detailed options
      conditionalPanel('input.detail === true',
      numericInput("modeoftheory", label = tagList(
        tags$span("Mode of hypothesis"), 
        tags$span(icon("info-circle"), id = "modeoftheory_icon")), value = 0))),
      
      # OR H1: Show OR input if option selected
      conditionalPanel('input.odds_question === true', box(id="odds_hypothesis",width="800px",
                numericInput("odds_scaleoftheory", label = tagList(
                         tags$span("Hypothesised odds/risk ratio"), 
                         tags$span(icon("info-circle"), id = "odds_scaleoftheory_icon")), value = 1.5))),
      
      # No odds: Show scale of H1 if OR optinos not selected
      conditionalPanel('input.odds_question === false', box(id="mean_hypothesis",width="800px",
                numericInput("scaleoftheory", label = tagList(
                         tags$span("Hypothesised mean difference"), 
                         tags$span(icon("info-circle"), id = "scaleoftheory_icon")), value = 1))),
      
      # DF H1: Degrees of freedom of H1, only if t-distribution selected for H1 model
      conditionalPanel('input.modeloftheory == "t"',box(id="tshow",width="800px",
                numericInput("dftheory", label = "Degrees of freedom of hypothesis", value = 1))),
      
      # Tails: Number of tails, negate if predicts negative
      radioButtons("tails", label = tagList(
                tags$span("Does hypothesis predict positive or negative effect?"), 
                tags$span(icon("info-circle"), id = "tails_icon")), 
                          c("Positive" = "pos",
                          "Negative" = "neg",
                          "Either" = "eit"), selected = c("eit"))),

  ###  TOOLTIPS  ###
      
  # Mean diff
  bsTooltip("obtained_icon", 
            title="Mean difference between conditions. For regression and GLMs, this is the coefficient B.", placement = "right", trigger = "hover",
            options = NULL),
  
  # SE
  bsTooltip("sd_icon", 
            title="Standard error of mean difference. For regression and GLMs, this is the standard error of coefficient B.", placement = "right", trigger = "hover",
            options = NULL),
      
  # Degrees of freedom from data
  bsTooltip("dfdata_icon", 
            title="Same as degrees of freedom for t-test.", placement = "right", trigger = "hover",
            options = NULL),
             
  # OR Selection
  bsTooltip("odds_question_icon", 
            title="Click here if entering result as an odds/risk ratio.", placement = "right", trigger = "hover",
            options = NULL),
  # OR
  bsTooltip("odds_icon", 
            title="Enter odds/risk ratio from study. This will be converted into log-odds/log-proportions.", placement = "right", trigger = "hover",
            options = NULL),
  # OR Upper
  bsTooltip("odds_upper_icon", 
            title="Upper 95% confidence interval of odds/risk ratio from study. Used to calculate standard error.", placement = "right", trigger = "hover",
            options = NULL), 
  
  # OR Scale of theory
  bsTooltip("odds_scaleoftheory_icon", 
            title="Expected odds/risk ratio, used as scale (e.g. standard deviation) for model of alternative hypothesis.", placement = "right", trigger = "hover",
            options = NULL),
             
  # Model of hypothesis
  bsTooltip("modeloftheory_icon", 
            title="Use uniform if maximum effect exists. Compared with normal, t & Cauchy give more probability to extreme effects.", placement = "right", trigger = "hover",
            options = NULL),
  
  # Uniform lower
  bsTooltip("lower_icon", 
            title="Zero or minimum meaningful effect size (e.g. mean difference) under the alternative hypothesis.", placement = "right", trigger = "hover",
            options = NULL),
  # Uniform upper
  bsTooltip("upper_icon", 
            title="Maximum plausible effect under alternative hypothesis.", placement = "right", trigger = "hover",
            options = NULL),
  
  # OR Uniform lower
  bsTooltip("or_lower_icon", 
            title="Minimum plausible odds/risk ratio under alternative hypothesis. If you expect positive effects, set this to 1.", placement = "right", trigger = "hover",
            options = NULL),
  
  # OR Uniform upper OR
  bsTooltip("or_upper_icon", 
            title="Maximum plausible odds/risk ratio under alternative hypothesis. If you expect negative effects, set this to 1.", placement = "right", trigger = "hover",
            options = NULL),
  
  # Mode of theory
  bsTooltip("modeoftheory_icon", 
            title="Mode of distribution of hypothesis, usually 0. If using OR/RR, note this is on log-odds scale so 0 is equivalent to OR=1.", placement = "right", trigger = "hover",
            options = NULL),
  
  # Scale of theory
  bsTooltip("scaleoftheory_icon", 
            title="Expected mean difference, used as scale (e.g. standard deviation) for model of alternative hypothesis.", placement = "right", trigger = "hover",
            options = NULL),
  
  # One of two tailed??
  bsTooltip("tails_icon", 
            title="One-tail distribution for positive/negative, two-tail for either. Mean difference negated if negative selected.", placement = "right", trigger = "hover",
            options = NULL),
  
  ###  SUBMIT  ###
  
  actionButton("submitbutton", 
               "Calculate", 
               class = "btn btn-primary"))
  



################################################
#                                              #
#            Section 4. Main panel             #
#                                              #
################################################

# In this section, we create the user interface for the main panel which,
# once the submit button is clicked, shows the calculated Bayes factor.
 
# This section has two tabs, the first shows the calculated Bayes factor.
# The second shows the "interpretation" markdown file. In this file, there
# are details on how Bayes factors should be interpreted. We can also add a
# video here if that would be useful.


main_panel_ui <- mainPanel(
  tabsetPanel(
          
          # Output tab: This shows the Bayes factor, once submit button clicked
          tabPanel("Output",
             
             # Heading: Heading with horizontal rule <hr> to match interpretation.md
             HTML("<hr> <h4>Output from calculator</h4>"),
             
             # Code: Code text letting user know when ready for calculation.
             #verbatimTextOutput('contents'),
             
             # Text: explaining how use calculator.
             #HTML("Enter results from your study and specifcy your hypothesis. Then press 'calculate' to show your Bayes factor below."),
             
             # Bf output: Table of the Bayes factor calculated from Bf
             div(tableOutput('tabledata'), digits=3,style="font-size:150%")#,
             
             # Explanation
             #HTML("<hr> <h4>Confused?<h4>\n"),
             
             #HTML("Click the 'interpretation' tab for help understanding your Bayes factor.")
             ),
          
          # Interpretation: Markdown file explaining how to interpret Bayes factor
          tabPanel("Interpretation", div(includeMarkdown("interpretation.md"), 
                                   align="justify")) 
  )
)




################################################
#                                              #
#          Section 5. User interface           #
#                                              #
################################################

# In this section, we create a user interface "ui" object, which brings
# together the sidebar and main panels we created above. It also includes
# formatting information, such as the theme used, webpage title, position
# of tabs, fonts, and "about" markdown file.



ui <- fluidPage(
  
  useShinyjs(),

  # Theme used: paper
  theme = shinytheme("paper"), 
  
  # Font: Change font used throughout document (as universal * is used)
  # This is written in HTML for css.
  tags$head(
    tags$style(
      # Fonts Imported from google fonts: https://fonts.google.com/
      # Montserrat used for whole document except logo, which is
      # Press Start 2P
      HTML(
      '@import url("https://fonts.googleapis.com/css2?family=Montserrat&display=swap");
      @import url("https://fonts.googleapis.com/css2?family=Press+Start+2P&display=swap");
      
      * {font-family: "Montserrat", sans-serif};'
      #.navbar-nav {float: right;}

    )),
    # Favicon for browser
    tags$link(rel = "shortcut icon", href = "https://icons.iconarchive.com/icons/martz90/circle/128/calculator-icon.png")
    ),
  
  
  
  # Main page
  navbarPage(
    
    # Logo: Formatting of logo. Option for calculator icon or logo
    title = span(tagList(#icon("calculator"), 
                    span(" Bayes", style='color: #1a75ff; font-size: 18px; font-family: "Press Start 2P";'),
                    span("Factor", style='font-size: 18px; font-family: "Press Start 2P"'),
                    span("calc", style='font-size: 10px; font-family: "Press Start 2P"')
                    )),
    
    windowTitle = "BayesFactor - Calculate Bayes Factors",

    # Calculator tab: Contains calculator and interpretation markdown
    tabPanel("Calculator", 
             
             # Sidebar Inputs (See above)
             side_inputs_ui,     
             
             # Main Panel (See above)
             main_panel_ui),
    
    # About tab: Contains about markdown file with info about Bf function
    tabPanel("About", 
             
             # Markdown file about.md
             div(includeMarkdown("about.md"), align="justify"))
  ) 
) 




################################################
#                                              #
#           Section 6. Server Logic            #
#                                              #
################################################

# This section specifies the server logic, including the inputs to the Bf function
# and output of result to be shown on the user interface.


server <- function(input, output, session) {
  
  ### Bf Print
  # Reactive expression which prints output from the
  # Bf function, taking inputs specified by UI.
  datasetInput <- reactive({  
    
    bf_calc <- Bf( 
      
      # sd = Standard error | SE converted from OR and OR upper 
      sd= ifelse(
             # When OR not selected, used mean difference
             input$odds_question == FALSE, input$sd,
             # When OR selected, converts OR and OR upper to log-odds
             ((log(input$odds_upper)-log(input$odds))/1.96)),
      
      # obtained = Mean difference | log-odds, negated if tails = negative
      obtained = ifelse(
             input$odds_question == FALSE, 
                        # Negate mean difference if negative hypothesised effect in tails
                        ifelse(input$tails == "neg", -input$obtained, input$obtained),
                        # Negate log-odds if negative hypothesised effect in tails
                        ifelse(input$tails == "neg",-log(input$odds), log(input$odds))),
      dfdata=input$dfdata,
      likelihood = input$likelihood,
      modeloftheory= input$modeloftheory,
      # Log if using OR
      lower = ifelse(input$odds_question == FALSE, input$lower, log(input$or_lower)),
      upper = ifelse(input$odds_question == FALSE, input$upper, log(input$or_upper)),
      modeoftheory = input$modeoftheory,
      
      # Scale of theory: Scale of theory | Expected OR under H1
      scaleoftheory = ifelse(
             input$odds_question == FALSE, 
             # Taking absolute value as direction sorted by input$tails
             abs(input$scaleoftheory),
             abs(log(input$odds_scaleoftheory))),
      
      # Two tailed if tails is "either", one tailed if "positive" or "negative"
      tail = ifelse(input$tails=="eit",2,1),
      dftheory = input$dftheory)
    
    # Put Bf result in data frame
    bf_calc <- data.frame(bf_calc)
    
    # Name variable
    names(bf_calc) <- "Bayes factor"
    
    # Print dataframe
    print(bf_calc)
    
  })
  
  
  ### Status
  # Gives status, whether calculation complete or ready for calculation. 
  #output$contents <- renderPrint({
  #  if (input$submitbutton>0) { 
  #    isolate("Calculation complete.") 
  #  } else {
  #    return("Server is ready for calculation.")
  #  }
  #})

  
  ### Output
  # Prints Bayes factor in a table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}




################################################
#                                              #
#              Section 7. Run app              #
#                                              #
################################################


#runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
shinyApp(ui = ui, server = server)
