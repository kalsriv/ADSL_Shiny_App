library(shiny)   # Web app development
library(haven)   # Read in SAS dataset
library(ggplot2) # Data visualization
library(scales)  # Improve axis labels
library(bslib)   # The future of Shiny UI
library(plotly)  # For interactive plots
library(admiral)

# Read in Data -------------------------------
adsl <- admiral::admiral_adsl
adlb <- admiral::admiral_adlb

df <- merge(
  x = adsl,
  y = adlb,
  by = "USUBJID",
  all.x = TRUE
)
adsl <- df

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
            "Duration of Disease " = "TRTDURD"
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
    the_plot <- ggplot(data = adsl, aes(x = DCDECOD,
                                        y = .data[[input$subject_data]],
                                        fill = TRT01A)) +
      geom_violin(trim = FALSE) +
      theme_minimal() +
      theme(legend.position = "right",
            text = element_text(size = 8), axis.text.x=element_text(angle = 90, hjust = 0)) +
      labs(
        title = "ADSL Data",
        subtitle = "Comparing Treatment Groups (Planned Treatment)",
        x = "",
        y = attributes(adsl[[input$subject_data]])$label
      ) +
      scale_x_discrete(labels = label_wrap(10))
      #+ theme(axis.text.x=element_text(angle = -90, hjust = 0))
    
    ggplotly(the_plot)
  })
}

shinyApp(ui, server)