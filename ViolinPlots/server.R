library("shiny")
#library("vioplot")
library("ggplot2")

#ENSG00000000457
#ENSG00000142515

# Get dataset and groups
data <- read.table("data/prostatedata.txt")
groups <- ifelse(substr(names(data),1,1)=="C","Control","Knockdown")

# Custom input validation 
inputValidation <- function(input){
  # If no gene entered
  if (input == "e.g. ENSG..."){
    "Please enter a gene ID"
  }
  # If input ID is not in dataset
  else if (!(input %in% rownames(data))){
    paste("'",input, "'", " is not contained in the dataset.", sep="")
  }
  else{ NULL }
}

shinyServer(function(input, output){
  
  ### Expression that generates a violin plot
  output$violinPlot <- renderPlot(function(){
    
    # Get gene ID's reactively
    geneID <- reactive({
      # Input validation 
      validate(
        inputValidation(input$geneID)
      )
      input$geneID
    })
    
    # Get the input gene reactively
    gene.df <- data.frame(gene=as.vector(as.numeric(data[rownames(data)==geneID(),])), group=groups)
    ylab <- "Expression"
    
    # If log transform box is selected...
    if(input$log==TRUE){
      gene.df$gene <- log2(gene.df$gene + 1)
      ylab <- "log2( Expression )"
    }
    
    # Plotting
    p <- ggplot(gene.df, aes(y=gene, x=group, color=group)) + 
      geom_violin() + 
      geom_jitter(shape=16, position=position_jitter(0.1), cex=1.2) +
      scale_x_discrete(name="") +
      scale_y_continuous(name=ylab) +
      ggtitle(paste("Gene: ", input$geneID)) +
      theme(plot.title = element_text(size=18, face="bold", vjust=2),
            axis.text.x = element_text(colour="grey20",size=18),
            legend.position="none")
    print(p)
  })
    
    ##### Output percentage of control zeros
    output$controlzeros <- renderText({ 
      
      # Get gene ID's reactively
      geneID <- reactive({
        # Input validation 
        validate(
          inputValidation(input$geneID)
        )
        input$geneID
      })
      
      gene.df <- data.frame(gene=as.vector(as.numeric(data[rownames(data)==geneID(),])), group=groups)
      
      controlzeros <- length(which(gene.df[gene.df$group=="Control",]$gene==0)) / length(which(groups=="Control"))
      controlzeros <- round(controlzeros*100,2)

      paste("Control group: ", controlzeros, "%", sep="")
    })
    
    ##### Output percentage of control zeros
    output$kdzeros <- renderText({ 
      
      # Get gene ID's reactively
      geneID <- reactive({
        # Input validation 
        validate(
          inputValidation(input$geneID)
        )
        input$geneID
      })
      
      gene.df <- data.frame(gene=as.vector(as.numeric(data[rownames(data)==geneID(),])), group=groups)
      
      kdzeros <- length(which(gene.df[gene.df$group=="Knockdown",]$gene==0)) / length(which(groups=="Knockdown"))
      kdzeros <- round(kdzeros*100,2)
      paste("Knockdown group: ", kdzeros, "%", sep="")
    })
})