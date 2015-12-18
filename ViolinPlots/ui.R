library("shiny")

# Define UI for application that draws a violin plot
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Violin Plot"),
  
  # Sidebar with a text box input for selecting genes
  sidebarLayout(
    
    sidebarPanel(
      helpText("Enter Ensembl ID"),
      textInput("geneID", label = h3("Ensembl ID"), 
                value = "Enter text...")
      ),
    
    # Show a plot of the generated distribution
    mainPanel("panel")
      #plotOutput("violinPlot")
    )
))