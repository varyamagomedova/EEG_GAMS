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


plot_avg_by_subject <- function(data, limits, title=""){
  data_plot  <-  data %>%
    group_by(time, cond_region_interaction, subject) %>%
    summarize(average.by.subject = mean(data)) %>%
    group_by(time,cond_region_interaction)%>%
    mutate(average.by.region = mean(average.by.subject)) 
  
  ggplot(data_plot, aes(x=time, color=cond_region_interaction)) +
    geom_line(aes(y=average.by.subject), linewidth=0.5, alpha=0.3) +
    geom_line(aes(y=average.by.region), linewidth=0.5, alpha=1) +
    coord_cartesian(ylim=limits) +
    ggtitle(title)
}

plot_avg_by_channel<- function(data, limits, title=""){
  data_plot  <-  data %>%
    group_by(time, cond_region_interaction, channel) %>%
    summarize(average.by.channel = mean(data)) %>%
    group_by(time,cond_region_interaction)%>%
    mutate(average.by.region = mean(average.by.channel)) 
  
  ggplot(data_plot, aes(x=time, color=cond_region_interaction)) +
    geom_line(aes(y=average.by.channel), linewidth=0.5, alpha=0.3) +
    geom_line(aes(y=average.by.region), linewidth=0.5, alpha=1) +
    coord_cartesian(ylim=limits)  +
    ggtitle(title)
}

# plot_by_hemisphere <- function(data, limits){
#   data_plot  <-  data %>%
#     group_by(time, channel, condition, hemisphere) %>%
#     summarize(average.by.hemisphere = mean(data)) %>%
#     group_by(time,cond_region_interaction)%>%
#     mutate(average.by.region = mean(average.by.hemisphere)) 
#   
#   ggplot(data_plot, aes(x=time, color=)) +
#     geom_line(aes(y=average.by.hemisphere), linewidth=0.5, alpha=0.3) +
#     geom_line(aes(y=average.by.region), linewidth=0.5, alpha=1) +
#     coord_cartesian(ylim=limits) 
# }

plot_channels <- function(data, limits, title=""){
  data_plot  <-  data %>%
    group_by(time, condition, channel) %>%
    summarize(average.by.channel = mean(data))  %>%
    group_by(time,condition)%>%
    mutate(average.by.condition = mean(average.by.channel),
           ccinteraction = interaction(channel, condition))
  
  ggplot(data_plot, aes(x=time, color=ccinteraction)) +
    geom_line(aes(y=average.by.channel), linewidth=0.5, alpha=0.3) +
    #geom_line(aes(y=average.by.condition), linewidth=0.5, alpha=1) +
    coord_cartesian(ylim=limits) 
}
  
plot_FTF <- function(data, limits, title=""){
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

plot_interaction_diff = function(values, model, picture){
  title = paste(picture, values[1], values[2])
  diff_summary <- capture.output(
    plot_diff(model, "time", comp=list(interCondFTF=values), main=title))
  output_file <- paste("summaries/nouns/reannotated/DIFF", picture, ".txt", sep="")
  writeLines(diff_summary, con = output_file)
}