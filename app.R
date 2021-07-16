################################################################################
# Group_name: Technos
# PEOPLE:
# 2429066 Mert Goksel
# 2290757 Nisa Kayacan
# 2218287 Bilge Ozkir
# 2414670 Seyran Amandurdyyev
# 2290591 Kadir CELIK
# data_link: 
# https://archive.ics.uci.edu/ml/datasets/AI4I+2020+Predictive+Maintenance+Dataset
################################################################################

library(shiny)
library(dplyr)
library(magrittr)
library(corrplot)
library(openxlsx)
library(ggiraphExtra)
library(gridExtra)
library(ggplot2)
library(BSDA)
library(shinythemes)
library(shinydashboard)
library(dashboardthemes)

df <- read.csv("./ai4i2020.csv")

ourLogo <- shinyDashboardLogoDIY(
  
  boldText = "FINAL PROJECT"
  ,mainText = ""
  ,textSize = 16
  ,badgeText = " 292"
  ,badgeTextColor = "white"
  ,badgeTextSize = 4
  ,badgeBackColor = "#394293"
  ,badgeBorderRadius = 6
  
)

ui <- dashboardPage(
  dashboardHeader(title = ourLogo),
  dashboardSidebar(
    sidebarMenu( id = "sidebar",
                 menuItem("Section", tabName = "section",
                          menuSubItem("General", tabName = "genel"), startExpanded = TRUE,
                          menuSubItem("Analysis", tabName = "analiz"))
    )
  ),
  dashboardBody(shinyDashboardThemes(
    theme = "grey_dark"
  ),
  conditionalPanel(
    condition = "input.sidebar == 'genel'",
    tags$style(HTML("
    .tabbable > .nav > li > a[data-value='Description'] {background-color:#74D5DD ;   color:white}
    .tabbable > .nav > li > a[data-value='Summary'] {background-color:#08B6CE ;  color:white}
    .tabbable > .nav > li > a[data-value='Corplot'] {background-color:#398AD7 ; color:white}
    .tabbable > .nav > li > a[data-value='Plots'] {background-color: #74D5DD; color:white}
    .tabbable > .nav > li > a[data-value='Logistic Regression Model'] {background-color: #08B6CE; color:white}
    .tabbable > .nav > li > a[data-value='Multiple Regression Model'] {background-color:#6495ED; color:white}
    .tabbable > .nav > li > a[data-value='Hypothesis Test'] {background-color:#398AD7; color:white}
  ")),
    tabsetPanel(
      tabPanel("Description",
               h5("The dataset consists of 10 000 data points stored as rows with 14 features in columns"),
               h5("air temperature [K]: generated using a random walk process later normalized to a standard deviation of 2 K around 300 K"),
               h5("process temperature [K]: generated using a random walk process normalized to a standard deviation of 1 K, added to the air temperature plus 10 K."),
               h5("rotational speed [rpm]: calculated from a power of 2860 W, overlaid with a normally distributed noise"),
               h5("torque [Nm]: torque values are normally distributed around 40 Nm with a If = 10 Nm and no negative values."),
               h5("tool wear [min]: The quality variants H/M/L add 5/3/2 minutes of tool wear to the used tool in the process."),
               h5("'machine failure' label that indicates, whether the machine has failed in this particular datapoint for any of the following failure modes are true."),
      ),
      tabPanel("Summary", verbatimTextOutput("summary")),
      tabPanel("Corplot",h2("Corrplot"),
               plotOutput("corpl")))
  ),
  conditionalPanel(
    condition = "input.sidebar == 'analiz'",
    tabsetPanel(
      tabPanel("Plots",
               selectInput("var_plot", label = "Select a variable to get the plots",
                           choices = c("Air temp" = "Air.temperature..K.",
                                       "Processor temp" = "Process.temperature..K.",
                                       "Rotational speed rpm" = "Rotational.speed..rpm.",
                                       "Torque" = "Torque..Nm.", 
                                       "Tool wear" = "Tool.wear..min.")),
               h2("Histogram"),
               plotOutput("plots1"),
               h2("Boxplot"),
               plotOutput("plots2"),
               h2("QQPlot"),
               plotOutput("plots3")
      ),
      tabPanel("Logistic Regression Model",
               selectInput(inputId = "vars_glm", 
                           label = "Select variables to see relation with machine failure",
                           choices = c("Air temp" = "Air.temperature..K.",
                                       "Processor temp" = "Process.temperature..K.",
                                       "Rotational speed rpm" = "Rotational.speed..rpm.",
                                       "Torque" = "Torque..Nm.", 
                                       "Tool wear" = "Tool.wear..min."),
                           multiple = T),
               verbatimTextOutput("summary_glm")),
      tabPanel("Multiple Regression Model",
               selectInput("vars_lm_dep", "Select the Dependant Variable",
                           choices = c("Air temp" = "Air.temperature..K.",
                                       "Processor temp" = "Process.temperature..K.",
                                       "Rotational speed rpm" = "Rotational.speed..rpm.",
                                       "Torque" = "Torque..Nm.", 
                                       "Tool wear" = "Tool.wear..min."),
                           multiple = F,
                           selected = "Air temp"),
               selectInput("vars_lm_indep", "Select the Independant Variables",
                           choices = c("Air temp" = "Air.temperature..K.",
                                       "Processor temp" = "Process.temperature..K.",
                                       "Rotational speed rpm" = "Rotational.speed..rpm.",
                                       "Torque" = "Torque..Nm.", 
                                       "Tool wear" = "Tool.wear..min."),
                           multiple = T,selected = "Air.temperature..K."),
               verbatimTextOutput("summary_ml")),
      tabPanel("Hypothesis Test",
               selectInput("t_var", "Select a variable to test",
                           choices = c("Air temp" = "Air.temperature..K.",
                                       "Processor temp" = "Process.temperature..K.",
                                       "Rotational speed rpm" = "Rotational.speed..rpm.",
                                       "Torque" = "Torque..Nm.", 
                                       "Tool wear" = "Tool.wear..min."),
                           selected = "Air.temperature..K."),
               verbatimTextOutput("summary_data"),
               radioButtons("which_test", "Select a hypothesized μ",
                            choices = c("Median" = 1,
                                        "Custom" = 2)),
               conditionalPanel(condition = "input.which_test == '2'",
                                numericInput("which_test_a", label = "Enter mu",value = 0)),
               radioButtons("which_type", "Select a hypothesis",
                            choices = c("Greater" = "greater",
                                        "Less" = "less",
                                        "In between" = "two.sided"),
                            selected = "greater"),
               numericInput("alpha", "Enter an alpha level: ", value = 0.05),
               h1("Result:"),
               verbatimTextOutput("summary_test"))
      
    )
  )))

server <- shinyServer(function(input, output){
  
  output$summary <- renderPrint({
    df %>% select(Air.temperature..K., Process.temperature..K., Rotational.speed..rpm., Torque..Nm., Tool.wear..min.) %>% summary(.)
  })
  
  output$corpl <- renderPlot({
    df %>% select(c("Air.temperature..K.", "Process.temperature..K.", "Rotational.speed..rpm.", "Torque..Nm.", "Tool.wear..min.")) %>% `colnames<-`(c("Air temp", "Processor temp", "Rotational speed rpm",
                                                                                                                                                      "Torque", "Tool wear")) %>% 
      cor() %T>% corrplot(tl.srt = 30)
  })
  
  output$summary_glm <- renderPrint({
    dep <- "Machine.failure"
    indep <- input$vars_glm
    temp <- select(df, all_of(dep), all_of(indep))
    glm_model <- glm(data = temp, formula = Machine.failure~.)
    summary(glm_model)
  })
  
  output$summary_ml <- renderPrint({
    lm_ml <- lm(reformulate(input$vars_lm_indep, input$vars_lm_dep), data=df)
    summary(lm_ml)
  })
  
  output$summary_data <- renderPrint({
    df %>% select(input$t_var) %>% summary()
  })
  
  output$summary_test <- renderPrint({
    df_temp <- df %>% select(input$t_var)
    mu <- if(input$which_test == 1){
      median(as.numeric(unlist(df_temp)))
    } else {
      input$which_test_a
    }
    t.test(df_temp, alternative = input$which_type, conf.level = as.numeric(input$alpha), mu = mu)
  })
  
  output$plots1 <- renderPlot({
    par(mfrow = c(2,2))
    df %>% select(input$var_plot) %>% rename(n = input$var_plot) %>% ggplot(aes(x=n)) + geom_histogram(fill = "Blue", color = "Black")
  })
  
  output$plots2 <- renderPlot({
    df %>% select(input$var_plot) %>% rename(n = input$var_plot) %>% ggplot(aes(x=n)) + geom_boxplot(fill = "Orange", color = "Black") + coord_flip()
  })
  
  output$plots3 <- renderPlot({
    df%>%select(input$var_plot)%>%unlist()%T>%qqnorm()%>%qqline()
  })
})
shinyApp(ui, server)