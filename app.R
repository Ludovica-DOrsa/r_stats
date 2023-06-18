library(shiny)
library(tidyverse)

ui <- fluidPage(

    titlePanel("Function analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("text", h3("Enter a function"), 
                      value = "~ .x + log(.x) + 10"),
            h3("Select the limits of the x axis"),
            numericInput("xlim1",label = 'Lower limit', value = -5),
            numericInput("xlim2",label = 'Upper limit', value = 5),
            ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("functionPlot")
        )
    )
)


server <- function(input, output, session) {
    
    observeEvent(input$xlim1, {
        updateNumericInput(
            session = session,
            inputId = "xlim2",
            min = input$xlim1
        )
    }, ignoreInit = TRUE)
    
    observeEvent(input$xlim2, {
        updateNumericInput(
            session = session,
            inputId = "xlim1",
            max = input$xlim2
        )
    }, ignoreInit = TRUE)

    output$functionPlot <- renderPlot({
        
        func <- rlang::as_function(as.formula(input$text))
        
        ggplot() +
            xlim(input$xlim1, input$xlim2) +
            geom_function(fun=func) + 
            theme_light()+
            labs(y= "", x = "") 
    })
}


shinyApp(ui = ui, server = server)
