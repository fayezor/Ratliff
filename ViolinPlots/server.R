library("shiny")

# Define server logic required to generate and plot a violin plot
shinyServer(function(input, output){
  
  # Expression that generates a violin plot. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$violinPlot <- renderPlot({
    # code for making violin plot
  })
})