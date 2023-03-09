library(shiny)
library(ggplot2)
library(tidyverse)


deprfile <- read_delim("depression_anxiety_data.csv")

userinster <- fluidPage(
  navbarPage(strong("Diagnostic Score per Test in College Students"),
             tabPanel("About",
                      p("This app uses self diagnosed tests such as a PHQ, GAD, and Epworth test, to determine a persons depression, anxiety, and sleepiness respectively."),
                      p("The individuals range from age 18 to 31 and range from year 1 to year 4 in college."),
                      p("The Patient Health Questionnaire test, measures the severity of depression and is on a scale from 0-27, 0 being 
                        none-minimal and 27 being severe."), 
                      p("The Generalized Anxiety Disorder test measure the severity of ones anxiety. It is measure from on a scale from 0-21, 0 being none-minimal and 21 being severe."), 
                      p("The Epworth Sleepiness Scale, is a test that tells someone how likely they are to zone off. It is measure on a scale of 0-3,"), 
                      p(" 0 being no chance of dozing off, and 3 being high change of dozing off"),
                      mainPanel(
                        tableOutput("aboutTable")
                      )  
             ),
             tabPanel("Table",
                      titlePanel( "Interactive Table"),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("age_range", "Select Age Range:",
                                      min = min(deprfile$age),
                                      max = max(deprfile$age),
                                      value = c(min(deprfile$age), max(deprfile$age)))
                        ),
                        mainPanel(
                          textOutput("selected_age"),
                          tableOutput("myTable")
                        )
                      )
             ),
             tabPanel("Plots",
                      titlePanel("Diagnostic Score per Test in College Students"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "plot_scores", label = "Diagnostic Tool Test:",
                                      choices = list("PHQ score", "GAD score", "Epworth score")),
                        ),
                        mainPanel(
                          textOutput("selected_test"),
                          plotOutput("scatter", height = 400, width = 800))
                      )
             )
  )
)
servering <- function(input, output) {
  output$myTable <- renderTable({
    filterdata <- deprfile %>%
      filter(age >= input$age_range[1] & age <= input$age_range[2])
    data <- bind_rows(
      filterdata %>%
        select(age, phq_score) %>%
        mutate(ScoreType = "PHQ score"),
      filterdata %>%
        select(age, gad_score) %>%
        mutate(ScoreType = "GAD score"),
      filterdata %>%
        select(age, epworth_score) %>%
        mutate(ScoreType = "Epworth score")
    )
    data %>%
      mutate(score = case_when(
        ScoreType == "PHQ score" ~ phq_score,
        ScoreType == "GAD score" ~ gad_score,
        ScoreType == "Epworth score" ~ epworth_score
      )) %>%
      select(ScoreType, age, score) %>%
      mutate(score = ifelse(is.na(score), "NA", score))
  })
  
  output$selected_age <- renderText(
    paste("You have selected the age range", min(input$age_range), "-", max(input$age_range))
  )
  
  output$aboutTable <- renderTable({
    data <- deprfile %>%
      select(age, phq_score) %>%
      mutate(ScoreType = "PHQ score") %>%
      bind_rows(
        deprfile %>%
          select(age, gad_score) %>%
          mutate(ScoreType = "GAD score")
      ) %>%
      bind_rows(
        deprfile %>%
          select(age, epworth_score) %>%
          mutate(ScoreType = "Epworth score")
      ) %>%
      mutate(score = case_when(
        ScoreType == "PHQ score" ~ phq_score,
        ScoreType == "GAD score" ~ gad_score,
        ScoreType == "Epworth score" ~ epworth_score
      )) %>%
      select(ScoreType, age, score) %>%
      sample_n(10)
    data
  })
  
  output$scatter <- renderPlot({
    if(input$plot_scores == "PHQ score") {
      ggplot(data = deprfile, aes(x = age, y = phq_score, color = phq_score)) +
        geom_point(alpha = 0.5) +
        scale_color_gradient(low = "orange", high = "red")
    } else if(input$plot_scores == "GAD score") {
      ggplot(data = deprfile, aes(x = age, y = gad_score, color = gad_score)) +
        geom_point(alpha =0.5) +
        scale_color_gradient(low = "purple", high = "dark blue")
    } else if(input$plot_scores == "Epworth score") {
      ggplot(data = deprfile, aes(x = age, y = epworth_score, color = epworth_score)) +
        geom_point(alpha = 0.5) +
        scale_color_gradient(low = "light green", high = " dark green")
    }
  })
  
  output$selected_test <- renderText({
    paste("You have selected the", input$plot_scores, "results")
  })
}

shinyApp(ui = userinster, server = servering)

