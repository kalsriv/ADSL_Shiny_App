library(shiny)   # Web app development
library(haven)   # Read in SAS dataset
library(ggplot2) # Data visualization
library(scales)  # Improve axis labels
library(bslib)   # The future of Shiny UI
library(plotly)  # For interactive plots


# Read in Data -------------------------------
adsl <- read_xpt("adsl.xpt")

# User Interface -----------------------------
ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "spacelab"),
  titlePanel("ADaM Subject-Level Analysis"),
  card(
    layout_sidebar(
      fill = TRUE,
      fillable = TRUE,
      sidebar = sidebar(
        # Drop down select input
        selectInput(
          inputId = "subject_data",
          label = "Subject Data",
          choices = c(
            "Age" = "AGE",
            "Baseline BMI" = "BMIBL",
            "Baseline Height" = "HEIGHTBL",
            "Baseline Weight" = "WEIGHTBL",
            "Years of Education" = "EDUCLVL",
            "Baseline BMI" = "BMIBL",
            "Duration of Disease " = "DURDIS",
            "MMSE Total" = "MMSETOT",
            "Pooled Baseline" = "BMIBLGR1"
          )
        )
      ),
      plotlyOutput("violplot")
    )
  )
)

# Server Function ---------------------------
server <- function(input, output, session) {
  
  # Create Plot
  output$violplot <- renderPlotly({
    the_plot <- ggplot(data = adsl, aes(
                                        x = .data[[input$subject_data]],
                                        y = DURDIS,
                                        fill = TRT01A)) +
      geom_violin(trim=FALSE) +
      theme_minimal() +
      stat_summary(fun.data="mean_sdl", mult=1, geom="crossbar", width=0.2 ) 

    ggplotly(the_plot)
  })
}

shinyApp(ui, server)

#######################################
