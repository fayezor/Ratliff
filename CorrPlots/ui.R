library("shiny")

# Define UI for application that draws a violin plot
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Correlation Gene Expression Plots"),
  
  # Sidebar with a text box input for selecting genes
  sidebarLayout(
    
    sidebarPanel(
      helpText("The correlation plots depict the correlation between gene expression values of two given genes."),
      textInput("XaxisGeneID", label = h5("Enter Ensembl ID for gene you would like on the x axis of the Correlation Plot"), value = "e.g. ENSG..."),
      textInput("yaxisGeneID", label = h5("Enter Ensembl ID for gene you would like on the y axis of the Correlation Plot"), value = "e.g. ENSG..."),
      submitButton("Submit")
    ),
    
    mainPanel(
      
      # Show a plot of the generated distribution
      plotOutput("correlationPlot"),
      br(),
      br(),
      
      # Report Correlation between gene expression values of the two selected genes
      h4("Correlation of Gene Expression Values"),
      textOutput("correlations")
    )
  )
))