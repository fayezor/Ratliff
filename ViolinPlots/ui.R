library("shiny")

# Define UI for application that draws a violin plot
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Violin Plots of Gene Expression"),
  
  # Sidebar with a text box input for selecting genes
  sidebarLayout(
    
    sidebarPanel(
      helpText("Violin plots depict expression of a given gene in Control vs. Knockdown cells. Note that the horizontal range of the points across the x-axis is simply a graphical jitter; it is meant for visual clarity and has no further meaning."),
      textInput("geneID", label = h5("Enter Ensembl ID for gene of interest"), value = "e.g. ENSG..."),
      checkboxInput("log", "Plot gene expression on log scale", 
                    value = FALSE)
      ),
    
    mainPanel(
      
      # Show a plot of the generated distribution
      plotOutput("violinPlot"),
      br(),
      br(),
    
      # Report percentage of zeros in either group
      h4("Percentage of zero counts"),
      textOutput("controlzeros"),
      textOutput("kdzeros")
    )
  )
))