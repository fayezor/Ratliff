library("shiny")
library("vioplot")
library("ggplot2")

# Get dataset
data <- read.table("../prostatedata.txt")
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
    input <- input$geneID
    
    gene.df <- data.frame(gene=as.vector(as.numeric(data[rownames(data)==input,])), group=groups)
    
    p <- ggplot(gene.df, aes(y=gene, x=group, color=group)) + 
      geom_violin() + 
      geom_jitter(shape=16, position=position_jitter(0.1), cex=1.2) +
      #theme(legend.position="none") +
      scale_x_discrete(name=input) +
      scale_y_continuous(name="log2( Expression )") +
      ggtitle("Violin Plots of Gene Expression") +
      theme(plot.title = element_text(size=12, face="bold", vjust=2))
    print(p)
  })
})