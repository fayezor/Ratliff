library("shiny")
library("vioplot")
library("ggplot2")

# Get dataset
data <- read.table("data/prostatedata.txt")
groups <- ifelse(substr(names(data),1,1)=="C","Control","Knockdown")

# Define server logic required to generate and plot a violin plot
shinyServer(function(input, output){
  
  # Expression that generates a violin plot. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$violinPlot <- reactivePlot(function(){
    
    # Get the input gene reactively
    geneID <- input$geneID
    
    if(input$log==TRUE){
      gene <- as.vector(as.numeric(data[rownames(data)==geneID,]))
      gene.df <- data.frame(gene=log2(gene+1), group=groups) 
      ylab <- "log2( Expression )"
    }
    else{
      gene.df <- data.frame(gene=as.vector(as.numeric(data[rownames(data)==geneID,])), group=groups) 
      ylab <- "Expression"
    }
    
    p <- ggplot(gene.df, aes(y=gene, x=group, color=group)) + 
      geom_violin() + 
      geom_jitter(shape=16, position=position_jitter(0.1), cex=1.2) +
      #theme(legend.position="none") +
      scale_x_discrete(name=geneID) +
      scale_y_continuous(name=ylab) +
      ggtitle("Violin Plots of Gene Expression") +
      theme(plot.title = element_text(size=12, face="bold", vjust=2))
    print(p)
  })
})