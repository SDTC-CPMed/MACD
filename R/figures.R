library(ggplot2)
library(ggpubr)


######################## Boxplot ###################################

Boxplot_data = read.csv("boxplot_clinical_story.csv", sep=",", header=T, stringsAsFactors = F, row.names = 1) 
Boxplot_data_gen = read.csv('graph_data_clinical_story_genomics.csv', sep=",", header=T, stringsAsFactors = F, row.names = 1)

Boxplot_data = rbind(Boxplot_data,Boxplot_data_gen)


Boxplot_data[Boxplot_data=="incident"]<- 'Incident'
Boxplot_data[Boxplot_data=="Atherosclerosis"]<- 'ASVD'

Boxplot_data[Boxplot_data=="prevalent"]<- 'Prevalent'
Boxplot_data[Boxplot_data=="obesity"]<- 'Obesity'
Boxplot_data[Boxplot_data=="genomics"]<- 'Genomics'
Boxplot_data[Boxplot_data=="met"]<- 'Metabolomics'
Boxplot_data[Boxplot_data=="prot"]<- 'Proteomics'
Boxplot_data[Boxplot_data=="gen"]<- 'Genomics'

colnames(Boxplot_data)[2] = 'Legend'



pd <- position_dodge(0.5)
TH2_COLOR <- c("#A9CD75","orange", '#005292')

labels_AUC<-  paste(round(as.numeric(Boxplot_data$AUC),2))
Ploter_X <- ggplot(data=Boxplot_data, aes(x=Model, y=prob, colour=Legend)) +
  geom_boxplot(outlier.size = 0.2, width = 0.5) +
  theme(text=element_text(family="Arial Narrow", size=12))+
  scale_color_manual(values= TH2_COLOR)+
  labs( x = 'Disease', y="Probability to develop the disease")+
  stat_compare_means(label.y = c(0.95, 0.95), method = "t.test", label = "p.signif")+
  theme(axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"))+
  theme(axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"))+
  theme_bw()+
  ylim(0.2, 1.1)
Ploter_X + facet_grid(disease ~ data_type) +
  theme(strip.text = element_text(face = 'bold')) 





######################## line plot #################################################

AUC_data = read.csv('ShinyApp/ShinyData_clinical_story.csv', row.names = 1)


AUC_data[AUC_data=="obesity"]<- 'Obesity'
AUC_data[AUC_data=="genomics"]<- 'Genomics'
AUC_data[AUC_data=="metabolomics"]<- 'Metabolomics'
AUC_data[AUC_data=="proteomics"]<- 'Proteomics'
AUC_data[AUC_data=="met"]<- 'Metabolomics'
AUC_data[AUC_data=="prot"]<- 'Proteomics'
AUC_data[AUC_data=="Atherosclerosis"]<- 'ASVD'

AUC_data_incident = AUC_data[AUC_data['Model'] == 'incident',]
colnames(AUC_data_incident)[6] = 'AUC_train_incident'
colnames(AUC_data_incident)[7] = 'AUC_test_incident'

AUC_data_prevalent = AUC_data[AUC_data['Model'] == 'prevalent',]
colnames(AUC_data_prevalent)[6] = 'AUC_train_prevalent'
colnames(AUC_data_prevalent)[7] = 'AUC_test_prevalent'

AUC_data = cbind(AUC_data_incident, AUC_data_prevalent)
AUC_data = AUC_data[,colnames(AUC_data)[c(1:10, 16,17)]]

pd <- position_dodge(0.5)
Ploter_X <- ggplot(data=AUC_data) +
  geom_line(aes(x = N, y = AUC_test_incident, color = "Incident"), linetype = 1)+
  geom_line(aes(x = N, y = AUC_test_prevalent, color = "Prevalent"), linetype = 1)+
  
  theme(text=element_text(family="Arial Narrow", size=12))+
  scale_color_manual(name = "Model", values = c("Incident" = "orange", "Prevalent" = "#005292"))+
  labs(x="# Features", y = "AUC")+
  theme(axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"))+
  theme(axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"))+
  
  theme_bw()


Model_Comparision = Ploter_X + facet_grid(Data ~ Disease) +
  theme(legend.position="top")+
  theme(strip.text = element_text(face = 'bold')) 
Model_Comparision

