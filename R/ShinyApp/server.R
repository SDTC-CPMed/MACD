# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(shiny)
library(ggplot2)
library(pROC)
library(plotROC)



ShinyData = read.csv('ShinyData_clinical_story.csv', row.names = 1)
ShinyData_gen = read.csv('ShinyData_clinical_story_genomics.csv', row.names = 1)
ShinyData_gen[ShinyData_gen=="Obesity"]<- 'obesity'



ShinyData[ShinyData=="met"]<- 'metabolomics'
ShinyData[ShinyData=="prot"]<- 'proteomics'

disease_dataset = read.csv(paste('use_', 'obesity', '_plink.txt', sep = ''), sep = '\t')


ShinyData_genomics = data.frame()
for(disease in c('CD', 'UC', 'RA', 'PSO', 'SLE', 'COPD', 'obesity', 'T2D', 'Atherosclerosis')){
  disease_dataset = read.csv(paste('use_', disease, '_plink.txt', sep = ''), sep = '\t')
  colnames(disease_dataset) = c('SNP', 'effect_allele', 'effect_weight')
  disease_dataset$Disease = disease
  ShinyData_genomics = rbind(ShinyData_genomics, disease_dataset)
}

          

server <- function(input, output) {
  # Define a reactive expression for the document term matrix

  calcHeight <- reactive({
    if(input$omics == 'genomics'){
      
      ShinyData_genomics_subset = ShinyData_genomics[ShinyData_genomics['Disease'] == input$disease,]
      print(dim(ShinyData_genomics_subset))
      
      ShinyData_genomics_subset = ShinyData_genomics_subset[sort(abs(ShinyData_genomics_subset$effect_weight),decreasing=T,index.return=T)[[2]],]
      #ShinyData_genomics_subset = ShinyData_genomics_subset[1:min(input$N, dim(ShinyData_genomics_subset)[1]) ,]
      #print(dim(ShinyData_genomics_subset))
      
      PlotData = data.frame('Features' = ShinyData_genomics_subset$SNP,
                            'Coef' = ShinyData_genomics_subset$effect_weight)
    }
    else{
      SelectedRow = ShinyData['Disease'] == input$disease & ShinyData['N'] == input$N & ShinyData['Data'] == input$omics & ShinyData['Model'] == input$patients
      
      Features = ShinyData[SelectedRow, 'Features']
      Coef = ShinyData[SelectedRow, 'Coef']
      
      # Remove brackets and single quotes and split the string
      Features <- unlist(strsplit(Features, "', '"))
      Features[1] <- gsub("\\['", "",Features[1])
      Features[length(Features)] <- gsub("'\\]", "",Features[length(Features)])
      
      Coef <- unlist(strsplit(Coef, ", "))
      Coef[1] <- gsub("\\[", "",Coef[1])
      Coef[length(Coef)] <- gsub("\\]", "",Coef[length(Coef)])
      Coef <- as.numeric(Coef)
      
      PlotData = data.frame('Features' = Features, 'Coef' = Coef)
      
    }
    (dim(PlotData)[1] +1) *25
  })
  
  output$plot1 <- renderPlot({

    if(input$omics == 'genomics'){
      
      ShinyData_genomics_subset = ShinyData_genomics[ShinyData_genomics['Disease'] == input$disease,]
      print(dim(ShinyData_genomics_subset))
      
      ShinyData_genomics_subset = ShinyData_genomics_subset[sort(abs(ShinyData_genomics_subset$effect_weight),decreasing=T,index.return=T)[[2]],]
      #ShinyData_genomics_subset = ShinyData_genomics_subset[1:min(input$N, dim(ShinyData_genomics_subset)[1]) ,]
      #print(dim(ShinyData_genomics_subset))
      
      PlotData = data.frame('Features' = ShinyData_genomics_subset$SNP,
                            'Coef' = ShinyData_genomics_subset$effect_weight)
    }
    else{
      SelectedRow = ShinyData['Disease'] == input$disease & ShinyData['N'] == input$N & ShinyData['Data'] == input$omics & ShinyData['Model'] == input$patients

      Features = ShinyData[SelectedRow, 'Features']
      Coef = ShinyData[SelectedRow, 'Coef']
      
      # Remove brackets and single quotes and split the string
      Features <- unlist(strsplit(Features, "', '"))
      Features[1] <- gsub("\\['", "",Features[1])
      Features[length(Features)] <- gsub("'\\]", "",Features[length(Features)])
      
      Coef <- unlist(strsplit(Coef, ", "))
      Coef[1] <- gsub("\\[", "",Coef[1])
      Coef[length(Coef)] <- gsub("\\]", "",Coef[length(Coef)])
      Coef <- as.numeric(Coef)
      
      PlotData = data.frame('Features' = Features, 'Coef' = Coef)
      
    }
    


    ggplot(PlotData, aes(x=Coef, y=Features)) +
      geom_segment( aes(y=Features, yend=Features, x=0, xend=Coef), color="grey") +
      geom_point( color="orange", size=4) +
      theme_light() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      ylab("") +
      xlab("Coefficient")
  },
  height = function(){calcHeight()})
  
  
  output$plot2 <- renderPlot({
    if(input$omics == 'genomics'){
      SelectedRow = ShinyData_gen['Disease'] == input$disease & ShinyData_gen['Model'] == input$patients
      Prob = ShinyData_gen[SelectedRow, 'prob']
      Reference = ShinyData_gen[SelectedRow, 'reference']
    }
    else{
      SelectedRow = ShinyData['Disease'] == input$disease & ShinyData['N'] == input$N & ShinyData['Data'] == input$omics & ShinyData['Model'] == input$patients

      Prob = ShinyData[SelectedRow, 'prob']
      Reference = ShinyData[SelectedRow, 'reference']
    }
    
    # Remove brackets and single quotes and split the string
    Reference <- unlist(strsplit(Reference, "', '"))
    Reference[1] <- gsub("\\['", "",Reference[1])
    Reference[length(Reference)] <- gsub("'\\]", "",Reference[length(Reference)])
    
    Prob <- unlist(strsplit(Prob, ", "))
    Prob[1] <- gsub("\\[", "",Prob[1])
    Prob[length(Prob)] <- gsub("\\]", "",Prob[length(Prob)])
    Prob <- as.numeric(Prob)
    
    Prob = Prob[Reference != 'ToBeSick']
    Reference = Reference[Reference != 'ToBeSick']
    
    PlotData = data.frame('Reference' = Reference, 'Prob' = Prob)
    
    
    basicplot <- ggplot(PlotData, aes(d = Reference, m = Prob)) +
      geom_roc(n.cuts = 0)+
      style_roc(theme = theme_gray())
    basicplot
  })
}
  
  
server

