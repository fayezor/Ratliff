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

#############################################################
##### IMPORTING/FORMATTING DATA FILES
#############################################################

# Get DE results information (for all genes, not just DE)
DEresults <- read.table("data/DEresults_full.txt")

# DE results information for zero v. nonzero comparison
DEresults_zerovnonzero <- read.table("data/DEresults_full_zerovnonzero.txt")

# Get dataset (containing all unfiltered genes) and groups
data <- read.table("data/prostatedata.txt")
groups <- ifelse(substr(names(data),1,1)=="C","Control","Knockdown")

# Get biomart mappings of EnsemblID to gene name
mart <- read.table("data/ensembl_name_mappings.txt", header=TRUE, colClasses="character")

# For the dataset IDs that are in mart, enter them manually
mart <- rbind(mart, c("ENSG00000280111", "NA"))
mart <- rbind(mart, c("ENSG00000205246", "RPSAP58")) 
mart <- rbind(mart, c("ENSG00000231865", "SIK3-IT1")) 
mart <- rbind(mart, c("ENSG00000279978", "NA"))
mart <- rbind(mart, c("ENSG00000269131", "AC004447.2"))
mart <- rbind(mart, c("ENSG00000260872", "RP11"))
mart <- rbind(mart, c("ENSG00000227136", "C10orf101"))

# Subset mart by just the geneIDs found in the dataset
mart.data <- mart[mart$EnsembleID %in% rownames(data) ,]

### Check out duplicates in mart.data

# # Indices for duplicated gene names
# dup.idx <- duplicated(mart.data$GeneName)
# # Names of duplicated gene names
# dup.names <- mart.data$GeneName[dup.idx]
# # Unique duplicated gene names
# length(unique(dup.names))
# dup.unique.names <- unique(dup.names)
# # Subsetting mart.data by the duplicated gene names, and sort by gene name
# dup.mart.data <- mart.data[mart.data$GeneName %in% dup.unique.names,]
# dup.mart.data <- dup.mart.data[order(dup.mart.data$GeneName),]
# head(dup.mart.data)

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

#############################################################
##### BEGIN SHINY SERVER FUNCTIONALITIES
#############################################################

shinyServer(function(input, output){
  
  ##### ----------------------------------------------- 
  ##### renderPrint: GENE ID - GENE NAME TABLE
  ##### -----------------------------------------------
  
  # output$marttable <- DT::renderDataTable(
  #   DT::datatable(mart.data, rownames=FALSE, 
  #                 options = list(orderClasses=TRUE, pageLength=5))
  #   )
  
  
  ##### ----------------------------------------------- 
  ##### renderPrint: LOOK UP GENE IDS GIVEN GENE NAME
  ##### -----------------------------------------------
  
  # output$genenameout <- renderPrint({
  #     #input$genename
  #     genename <- input$genename
  #     mart.data[mart.data$GeneName==genename,]$EnsembleID
  #   })
  
  ##### -------------------------------------------------------  
  ##### renderDataTable: DE RESULTS TABLE, ZERO V KNOCKDOWN
  ##### ------------------------------------------------------- 
  
  output$mytable_zerovnonzero <- DT::renderDataTable({
    
    # Filter by DE table
    keep <- reactive({
      if (input$filterDEtable_zerovnonzero == 2){ #DE
        round(subset(DEresults_zerovnonzero, FDR < 0.05),3)
      }
      else if (input$filterDEtable_zerovnonzero == 3){ #significantly upregulated
        round(subset(DEresults_zerovnonzero, logFC > 0 & FDR < 0.05),3)
      }
      else if (input$filterDEtable_zerovnonzero == 4){ #significantly downregulated
        round(subset(DEresults_zerovnonzero, logFC < 0 & FDR < 0.05),3)
      }
      else if (input$filterDEtable_zerovnonzero == 5){ #nonDE
        round(subset(DEresults_zerovnonzero, FDR > 0.05),3)
      }
      else round(DEresults_zerovnonzero,3)
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
  
  ##### -------------------------------------------------------
  ##### renderDataTable: DE RESULTS TABLE, CONTROL V KNOCKDOWN
  ##### -------------------------------------------------------
  
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
  
  ##### ------------------------------------------ 
  ##### renderPlot: VIOLIN PLOT
  ##### ------------------------------------------ 
  
  output$violinPlot <- renderPlot(function(){
    
    # Get gene ID's reactively
    geneID <- reactive({
      # Input validation 
      validate(
        inputValidation(input$geneID)
      )
      input$geneID
    })
    
    # Get data from the input geneID reactively
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

  ##### ------------------------------------------ 
  ##### renderUI: PERCENTAGE OF ZEROS
  ##### ------------------------------------------ 
  
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
  
  ##### ------------------------------------------ 
  ##### renderUI: PRINT DE RESULTS FOR GIVEN GENE
  ##### ------------------------------------------ 
  
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
  
  ##### ------------------------------------------ 
  ##### renderPlot: CORRELATION PLOT
  ##### ------------------------------------------ 
  
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
  
  ##### -------------------------------------------------  
  ##### renderText: PRINT CORRELATIONS BETWEEN TWO GENES
  ##### ------------------------------------------------- 
  
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


