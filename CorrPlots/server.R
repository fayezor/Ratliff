library("shiny")
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
 
  #Expression that generates the correlation between two genes plot 
  output$correlationPlot <- renderPlot(function(){
    
    # Get gene ID's reactively
    geneID1 <- reactive({
       #Input validation 
      validate(
        inputValidation(input$XaxisGeneID)
      )
      input$XaxisGeneID
    })
    
    geneID2 <- reactive({
       #Input validation 
      validate(
        inputValidation(input$yaxisGeneID)
      )
      input$yaxisGeneID
    })
        
#Get the input gene reactively
  gene1.df <- data.frame(gene=as.vector(as.numeric(data[rownames(data)==geneID1(),])), group=groups)
  gene2.df <- data.frame(gene=as.vector(as.numeric(data[rownames(data)==geneID2(),])), group=groups)
  xlab <- paste(input$XaxisGeneID, " Expression" )
  ylab <- paste(input$yaxisGeneID, " Expression" )
  gene1.gene2.df <- data.frame(gene1=as.numeric(data[rownames(data)==input$XaxisGeneID,]), gene2=as.numeric(data[rownames(data)==input$yaxisGeneID,]), group=groups)
  
  #plotting
  set.seed(5749)
  p <- ggplot(gene1.gene2.df, aes(x = gene1, y = gene2)) + geom_point(position="jitter", aes(color=group)) + 
     scale_x_continuous(name=xlab) +
      scale_y_continuous(name=ylab) + 
    theme(legend.title=element_blank()) 
  p
  
})
#####Output correltations
output$correlations <- renderText({ 
  
  correlations<-round(cor(gene1.df$gene, gene2.df$gene),2)
  
  paste("Correlation:", correlations, sep="")
})  
  
 }) 
 
  
  