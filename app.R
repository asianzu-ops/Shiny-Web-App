

#Load library

library(shiny)
library(ggplot2)
library(dplyr)

#working directory
setwd("C:/Users/Asianzu Blessing/Documents/Practice R/GroomingApp/GroomApp")
   
data <- read.csv("cleaned_grooming_data.csv")

ui <- fluidPage(
  titlePanel("Chimpanzee Grooming Plot"),
             
  h3("How the GroomApp Works"),
  
  p("The GroomApp visualizes grooming interactions among chimpanzees. It allows for selecting a specific chimp ID to view either all grooming events 
     involving that individual (as jitter points) or a summary of grooming frequency(shown with point sizes and colors based on whether they are groomer or groomed).
     The app reads in a grooming dataset, filters and processes the data based on user inputs, and dynamically generates the appropriate plot."),
  
  
  
  selectInput("chimp_id", "Select Chimpanzee ID:",
              choices = unique(c(data$ID1, data$ID2))),
  
  radioButtons("plot_type", "Plot Type:",
               choices = c("Interactions" = "jitter",
                           "Grooming Frequency" = "count"),
               selected = "jitter"),
  
  plotOutput("grooming_plot")
)

#Running the server
server <- function(input, output, session) {
  
  output$grooming_plot <- renderPlot({
    req(input$chimp_id)
    
    filtered_data <- data |>
      filter(ID1 == input$chimp_id | ID2 == input$chimp_id)
    
    if (nrow(filtered_data) == 0) return(NULL)
    
    if (input$plot_type == "jitter") {
      ggplot(filtered_data, aes(x = ID1, y = ID2)) +
        geom_jitter(width = 0.2, height = 0.2, alpha = 0.7) +
        labs(
          title = paste("Raw Grooming Interactions for", input$chimp_id),
          x = "Groomer (ID1)",
          y = "Groomed (ID2)"
        ) +
        theme_minimal()
    } else {
      summarized_data <- filtered_data |>
        group_by(ID1, ID2) |>
        summarise(Frequency = n(), .groups = "drop") |>
        mutate(Role = case_when(
          ID1 == input$chimp_id ~ "Groomer",
          ID2 == input$chimp_id ~ "Groomed",
          TRUE ~ "Other"
        ))
      
      ggplot(summarized_data, aes(x = ID1, y = ID2, size = Frequency, color = Role)) +
        geom_point(alpha = 0.8) +
        scale_size_continuous(range = c(2, 10)) +
        scale_color_manual(values = c("Groomer" = "forestgreen", "Groomed" = "tomato", "Other" = "gray")) +
        labs(
          title = paste("Grooming Frequency for", input$chimp_id),
          x = "Groomer (ID1)",
          y = "Groomed (ID2)",
          size = "Frequency",
          color = "Role"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}

#Running Shiny 
shinyApp(ui = ui, server = server)
