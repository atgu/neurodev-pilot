#11/2/21

library(tidyverse)
setwd("~/Documents/NeuroDev/trio_pilot")

full_data <- read_csv("./data/symptom_sev.csv")[,2:3]
colnames(full_data)[2] <- "symptom"

nd <- full_data[full_data$study == "NeuroDev",]
ssc <- full_data[full_data$study == "SSC",]

ggplot()+
  geom_density(data = full_data, aes(x= symptom, y = ..density..*1.3, fill = study),  
               adjust = 3.5, alpha = .2, color = "lightgray") +
  geom_bar(data = full_data, aes(x= symptom, y=..prop.., fill = study), 
           position = "dodge", color = "gray35") +
  theme_classic() + 
  expand_limits(x = c(-0.5, 3.5)) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "\nNumber of co-occurring adverse \nneurodevelopmental outcomes\n(delayed walking, seizures, GDD/ID)",
       y = "Proportion of ASD cases\n") +
  scale_fill_manual(values=c( "#4596EC","#EBB73E"), name = "Study", 
                    labels = c("NeuroDev (n = 122)", "SSC (n = 2517)")) +
  theme(legend.position = "top")
#" 
##CB5040"

full_data %>% group_by(study) %>% summarize(mean(`N (count)`))
