if(!require(shiny)){
  install.packages("shiny")
}
if(!require(ggplot2)){
  install.packages("ggplot2")
}
if(!require(DT)){
  install.packages("DT")
}
require(shiny)
require(ggplot2)
require(DT)

#ENSG00000000457
#ENSG00000142515

# Get DE results information
DEresults <- read.table("data/DEresults_full.txt")

# Get biomart mappings of EnsemblID to gene name
mart <- read.table("data/ensembl_name_mappings.txt", header=TRUE, colClasses="character")

# For the dataset IDs that are in mart, enter them manually
mart <- rbind(mart, c("ENSG00000280111", "name1"))
mart <- rbind(mart, c("ENSG00000205246", "name2"))
mart <- rbind(mart, c("ENSG00000231865", "name3"))
mart <- rbind(mart, c("ENSG00000279978", "name4"))
mart <- rbind(mart, c("ENSG00000269131", "name5"))
mart <- rbind(mart, c("ENSG00000260872", "name6"))
mart <- rbind(mart, c("ENSG00000227136", "name7"))

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
  
  output$mytable <- DT::renderDataTable({
    
    # Filter by DE table
    keep <- reactive({
      if (input$filterDEtable == 2){ #DE
        round(subset(DEresults, FDR < 0.05),3)
      }
      else if (input$filterDEtable == 3){ #significantly upregulated
        round(subset(DEresults, logFC > 0 & FDR < 0.05),3)
      }
      else if (input$filterDEtable == 4){ #significantly downregulated
        round(subset(DEresults, logFC < 0 & FDR < 0.05),3)
      }
      else if (input$filterDEtable == 5){ #nonDE
        round(subset(DEresults, FDR > 0.05),3)
      }
      else round(DEresults,3)
    })
    
    table <- keep()
    
    table$GeneID <- rownames(table)
    # For each GeneID in table, get index in mart
    match.idx <- match(table$GeneID, mart$EnsembleID)
    # Create column of mapped gene names next to their IDs
    table$GeneName <- mart$GeneName[match.idx]
    table <- table[, c(7,6,1,2,3,4,5)]
    
    DT::datatable(table, rownames=FALSE)
  }, options = list(orderClasses=TRUE))
  
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
    
    # Get data from the input gene reactively
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
  output$zeros <- renderUI({ 
    
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
    
    kdzeros <- length(which(gene.df[gene.df$group=="Knockdown",]$gene==0)) / length(which(groups=="Knockdown"))
    kdzeros <- round(kdzeros*100,2)
    
    str1 <- paste("Control group: ", controlzeros, "%", sep="")
    str2 <- paste("Knockdown group: ", kdzeros, "%", sep="")
    
    HTML(paste(str1, str2, sep = '<br/>'))
  })
  
  ##### Output differential expression results
  output$DEresults <- renderUI({
    
    # Get gene ID's reactively
    geneID <- reactive({
      # Input validation 
      validate(
        inputValidation(input$geneID)
      )
      input$geneID
    })
    gene.DEinfo <- DEresults[rownames(DEresults)==geneID(),] 
    if (geneID() %in% rownames(DEresults) == FALSE){
      paste("Gene ", geneID(), " was filtered out prior to differential expression, as its average read count across all replicates was < 5.")
    }
    else{
      str1 <- paste("Adjusted p-value: ", round(gene.DEinfo$FDR, 3), sep="")
      str2 <- paste("Log fold-change: ", round(gene.DEinfo$logFC, 3), sep="")
      if (gene.DEinfo$FDR < 0.05){
        str3 <- paste("Gene ", geneID(), " is differentially expressed.", sep="")
        HTML(paste(str1, str2, str3, sep = '<br/>'))
      }
      else{
        str3 <- paste("Gene ", geneID(), " is not differentially expressed.", sep="")
        HTML(paste(str1, str2, str3, sep = '<br/>'))
      }
    }
  })
  
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
    xlab <- paste(input$XaxisGeneID, "Expression", sep=" ")
    ylab <- paste(input$yaxisGeneID, "Expression", sep=" ")
    gene1.gene2.df <- data.frame(gene1=as.numeric(data[rownames(data)==input$XaxisGeneID,]), gene2=as.numeric(data[rownames(data)==input$yaxisGeneID,]), group=groups)
    
    # If log transform box is selected...
    if(input$log2==TRUE){
      gene1.gene2.df$gene1 <- log2(gene1.gene2.df$gene1 + 1)
      gene1.gene2.df$gene2 <- log2(gene1.gene2.df$gene2 + 1)
      xlab <- paste(input$XaxisGeneID, "log2(Expression)", sep=" ")
      ylab <- paste(input$yaxisGeneID, "log2(Expression)", sep=" ")
    }
    
    #plotting
    set.seed(5749)
    p <- ggplot(gene1.gene2.df, aes(x = gene1, y = gene2)) +
      geom_point(aes(color=group)) + 
      scale_x_continuous(name=xlab) +
      scale_y_continuous(name=ylab) + 
      theme(legend.title=element_blank(), 
            axis.title.x=element_text(size=18), 
            axis.title.y=element_text(size=18))
    p
  })
  
  #####Output correlations
  output$correlations <- renderText({ 
    
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
    
    correlations <- round(cor(gene1.df$gene, gene2.df$gene),2)
    paste("Correlation: ", correlations, sep="")
  })  
  
}) 


