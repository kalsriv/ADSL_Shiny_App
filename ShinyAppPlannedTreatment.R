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
            "MMSE Total" = "MMSETOT"
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
    the_plot <- ggplot(data = adsl, aes(x = TRT01A,
                                        y = .data[[input$subject_data]],
                                        fill = TRT01P)) +
      geom_violin() +
      theme_minimal() +
      theme(legend.position = "none",
            text = element_text(size = 15)) +
      labs(
        title = "ADSL Data",
        subtitle = "Comparing Treatment Groups (Actual Treatment)",
        x = "",
        y = attributes(adsl[[input$subject_data]])$label
      ) +
      scale_x_discrete(labels = label_wrap(10))
    
    ggplotly(the_plot)
  })
}

shinyApp(ui, server)