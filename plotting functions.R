setwd("/Users/varya/Desktop/R projects/EEG GAMMs")
library(dplyr)
library(mgcv)
library(itsadug)
library(ggplot2)
library(tidyverse)

#Credit: this is a function from Neurokit team
theme_eeg <- function(){
  list(
    see::theme_modern() +
      theme(axis.line.y = element_blank()),
    geom_vline(xintercept = 0, linetype="dashed")
  )
}


plot_avg_by_subject <- function(data, limits){
  data_plot  <-  data %>%
    group_by(time, cond_region_interaction, subject) %>%
    summarize(average.by.subject = mean(data)) %>%
    group_by(time,cond_region_interaction)%>%
    mutate(average.by.region = mean(average.by.subject)) 
  
  ggplot(data_plot, aes(x=time, color=cond_region_interaction)) +
    geom_line(aes(y=average.by.subject), linewidth=0.5, alpha=0.3) +
    geom_line(aes(y=average.by.region), linewidth=0.5, alpha=1) +
    coord_cartesian(ylim=limits) 
}

plot_avg_by_channel<- function(data, limits){
  data_plot  <-  data %>%
    group_by(time, cond_region_interaction, channel) %>%
    summarize(average.by.channel = mean(data)) %>%
    group_by(time,cond_region_interaction)%>%
    mutate(average.by.region = mean(average.by.channel)) 
  
  ggplot(data_plot, aes(x=time, color=cond_region_interaction)) +
    geom_line(aes(y=average.by.channel), linewidth=0.5, alpha=0.3) +
    geom_line(aes(y=average.by.region), linewidth=0.5, alpha=1) +
    coord_cartesian(ylim=limits) 
}

plot_FTF <- function(data, limits){
FTF_data_plot  <-  data %>%
  group_by(FTF, cond_region_interaction, subject) %>%
  summarize(average.by.subject = mean(data)) %>%
  group_by(FTF,cond_region_interaction)%>%
  mutate(average.by.region = mean(average.by.subject))

 ggplot(FTF_data_plot, aes(x=FTF, color=cond_region_interaction)) +
  #geom_line(aes(y=average.by.channel), linewidth=0.5, alpha=0.3) +
  geom_line(aes(y=average.by.region), linewidth=0.5, alpha=1) +
  coord_cartesian(ylim=limits) 
}