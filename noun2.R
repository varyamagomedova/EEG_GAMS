setwd("/Users/varya/Desktop/R projects/EEG GAMMs")
library(dplyr)
library(mgcv)
library(itsadug)
library(ggplot2)
library(tidyverse)
#source("preprocessing.R)
#=================================== making up, ordering, releveling variables

noun_data <- read.csv("noun_data_annotatedVM.csv", stringsAsFactors = T)
noun_data$cond_hem_interaction = interaction(noun_data$condition,noun_data$hemisphere)
noun_data$cond_region_interaction = interaction(noun_data$condition,noun_data$region)
noun_data$FTF_fact <-as.factor(ifelse(noun_data$FTF<0.5, "<0.5", 
                                      ifelse(noun_data$FTF>=0.5 & noun_data$FTF<=1,">0.5<1",
                                             ifelse(noun_data$FTF>1 & noun_data$FTF<1.5, ">1<1.5", ">1.5"))))
noun_data$condition = relevel(noun_data$condition, ref="no-conflict")
noun_data$FTF_fact = factor(noun_data$FTF_fact, ordered =T)
noun_data$subject <- as.factor(noun_data$subject)
#============================= NOUNS
# PLOTTING DATA
source("plotting functions.R")

data_plot_by_subject <-  plot_avg_by_subject(noun_data, c(-5,5))
data_plot_by_channel <-  plot_avg_by_channel(noun_data, c(-5,5))
data_FTF <- plot_FTF(noun_data, c(-5,5))


frontal<- droplevels(subset(noun_data,region=="Frontal"))
prefrontal <- droplevels(subset(noun_data,region=="Prefrontal"))
central <-droplevels(subset(noun_data,region=="Central"))
parietal <-droplevels(subset(noun_data,region=="Parietal"))
occipital <- droplevels(subset(noun_data,region=="Occipital"))
temporal <- droplevels(subset(noun_data,region=="Temporal"))
parietal <- droplevels(subset(noun_data,region=="Parietal"))
broca <- droplevels(subset(noun_data,region=="Broca"))
motor <- droplevels(subset(noun_data,region=="Motor"))
fLateral <- droplevels(subset(noun_data,region=="FLateral"))
#plotting frontal

#this looks like a mess
frontal_plot <- plot_avg_by_channel(frontal, c(-1.5,1.5))


#================================================ANALISYS

# try tp interactions for time and FTF, if they can be put on the same scale
#they are not in the package anymore
#or then ti just for the interaction separately
#include random intercepts for channels

#in this file all analysed data will be after onset
#BC adds nothing
#noun_data <- sample_baseline_correction(noun_data, "time", "data", -50, 0)
noun_data_analysis <- noun_data %>%filter(time > 0) 
noun_data_analysis$condition = relevel(noun_data_analysis$condition, ref="no-conflict")
noun_data_analysis$condition = as.ordered(noun_data_analysis$condition)
noun_data_analysis$subject <- as.factor(noun_data_analysis$subject)
noun_data_analysis$interCondFTF <- interaction(noun_data_analysis$condition, noun_data_analysis$FTF_fact)
noun_data_analysis$interCondFTF = as.ordered(noun_data_analysis$interCondFTF)

# onset time can be used for by trial like AC
#this makes no sense, although residuals are highly correlated
#try guessing from ac of residuals - doesn't really help
interval <- 100
noun_data_analysis <- noun_data_analysis %>%
  group_by(condition, channel,subject) %>%
  mutate(
    onset_time = min(time),  # Determine the onset time for each participant
    interval_times = list(seq(from = onset_time[1], to = max(time), by = interval)),  
    start.osc = time %in% unlist(interval_times)  # set osc start time
  ) %>%
  ungroup() %>%select( -interval_times)
# AC may actually decrease de
#ORD no AC is actually a good model, but de ~1


gam_nouns_by_region_hemisphere <- function(noun_data){ 
  region = paste(noun_data$region[1], noun_data$hemisphere[1], sep = " ")
  # model_nouns <- bam(data ~  interCondFTF +
  #                       s(time, by = interCondFTF, k=100, bs="cr") +
  #                       #s(time, by = FTF_fact, bs="cr")+ 
  #                      s(subject, bs="re") , 
  #                     #s(time, subject,  by=interCondFTF, bs="fs", m=1) ,
  #                     #rho=0.9, AR.start = noun_data$start.osc, AR.order = 1,
  #                data=noun_data, family = "scat", discrete = T, method = "fREML")
  model_nouns <- bam(data ~  condition + 
                       s(time, by=condition, k=100, bs="cr") +  s(FTF, by = condition, k=15, bs="tp") +
                       ti(time,FTF,  bs=c("cr","tp"))+   
                     #s(subject, bs="re"), 
                     s(time, subject,  by=condition, bs="fs",  m=1),
                     #s(FTF, subject,  by=condition, bs="fs", m=1),
                     data=noun_data, family = "scat",  
                     discrete = T, method = "fREML")
  
  
save(model_nouns, file = paste("models/nouns/reannotated/gam_numeric_plain", region, ".RData",  sep=""))

jpeg(paste("plots/nouns/reannotated/gam_numeric_plain", region, ".jpeg", sep=""), units='cm', width=16, height=10, res=300)
par(mfrow=c(1,2))
plot_smooth(model_nouns, view='time', rug=F, plot_all = "condition")
plot_smooth(model_nouns, view='FTF', rug=F, plot_all = "condition")
dev.off()
model_summary <- capture.output(summary(model_nouns))
output_file <- paste("summaries/nouns/reannotated/gam_numeric_plain", region, ".txt", sep="")
writeLines(model_summary, con = output_file)
}


subsets_by_region_hemisphere <- split(noun_data_analysis, list(noun_data_analysis$region, noun_data_analysis$hemisphere))
#lapply(subsets_by_region_hemisphere, gam_nouns_by_region_hemisphere)
#test model on a small subset - frontal left is more than occipital
occipital.left <- as_tibble(subsets_by_region_hemisphere[["Occipital.left"]])
frontal.left <- as_tibble(subsets_by_region_hemisphere[["Frontal.left"]])
temporal.left <- as_tibble(subsets_by_region_hemisphere[["Temporal.left"]])
parietal.left <- as_tibble(subsets_by_region_hemisphere[["Parietal.left"]])
motor.left <- as_tibble(subsets_by_region_hemisphere[["Motor.left"]])
broca.left <- as_tibble(subsets_by_region_hemisphere[["Broca.left"]])

#Frontal is ok, 26DE, some little significance periods - have another look
gam_nouns_by_region_hemisphere(frontal.left)
load("~/Desktop/R projects/EEG GAMMs/models/nouns/reannotated/gam_numeric_plainFrontal left.RData")
frontal.left_gam <- model_nouns
gam.check(frontal.left_gam)
plot(frontal.left_gam)
plot_diff(frontal.left_gam, "time", comp=list(condition=c("no-conflict","conflict")), main="Frontal left")
acf_resid <- acf(residuals(frontal.left_gam))
estimated_rho <- acf_resid$acf[2]
#parietal
gam_nouns_by_region_hemisphere(parietal.left)

#temporal is good to go 24.1% of variance explained
# try without interactions
gam_nouns_by_region_hemisphere(temporal.left)
load("~/Desktop/R projects/EEG GAMMs/models/nouns/reannotated/gam_testinterCondFTFFStemporal left.RData")
temporal.left_gam <- model_nouns
gam.check(temporal.left_gam)
plot(temporal.left_gam)
acf_resid <- acf(residuals(temporal.left_gam))
estimated_rho <- acf_resid$acf[2]


#Occ is good to go, except for the lack of data <0.5
#it's ok with plain ME
# try without interactions
gam_nouns_by_region_hemisphere(occipital.left)
load("~/Desktop/R projects/EEG GAMMs/models/nouns/reannotated/gam_testinterCondFTFFSoccipital left.RData")
occipital.left_gam <- model_nouns
gam.check(occipital.left_gam)
plot(occipital.left_gam)
acf_resid <- acf(residuals(occipital.left_gam))
estimated_rho <- acf_resid$acf[2]
#============================= Plotting models

plot_interaction_diff = function(values, model, picture){
  print(values)
  title = paste(picture, values[1], values[2])
  diff_summary <- capture.output(
  plot_diff(model, "time", comp=list(interCondFTF=values), main=title))
  output_file <- paste("summaries/nouns/reannotated/DIFF", picture, ".txt", sep="")
  writeLines(diff_summary, con = output_file)
}
interaction_pairs = apply(combn(levels(noun_data_analysis$interCondFTF),2),2, c)

jpeg('plots/nouns/reannotated/OCC_differences_all_conditions.jpeg', units='cm', width=56, height=32, res=300)
par(mfrow=c(7,4))
apply(interaction_pairs, 2, plot_interaction_diff, model = occipital.left_gam , picture = "gam_testinterCondFTFMEoccipital left")
dev.off()

jpeg('plots/nouns/reannotated/TEMP_differences_all_conditions.jpeg', units='cm', width=56, height=32, res=300)
par(mfrow=c(7,4))
apply(interaction_pairs, 2, plot_interaction_diff, model = temporal.left_gam , picture = "gam_testinterCondFTFMEtemporal left")
dev.off()


jpeg('plots/nouns/reannotated/FRONT_differences_all_conditions.jpeg', units='cm', width=56, height=32, res=300)
par(mfrow=c(7,4))
apply(interaction_pairs, 2, plot_interaction_diff, model = frontal.left_gam , picture = "gam_FS num frontal left")
dev.off()
#=================== experimental section


sample_baseline_correction <- function(data, time_column, value_column, baseline_start, baseline_end) {
  baseline_indices <- which(data[[time_column]] >= baseline_start & data[[time_column]] <= baseline_end)
  baseline_mean <- mean(data[baseline_indices, value_column])
  data[[value_column]] <- data[[value_column]] - baseline_mean
  return(data)
}

corrected_prefrontal <- sample_baseline_correction(prefrontal, "time", "data", -500, 0)
corrected_prefrontal_plot  <-  corrected_prefrontal %>%
  mutate(subject=as.factor(subject)) %>%
  group_by(time, condition, subject) %>%
  summarize(average.by.subject = mean(data)) %>%
  group_by(time,condition)%>%
  mutate(average.by.region = mean(average.by.subject)) %>%
  mutate(cond_subject_interaction = interaction(condition, subject))

ggplot(corrected_prefrontal_plot, aes(x=time, color=(cond_subject_interaction))) +
  geom_line(aes(y=average.by.subject), linewidth=0.5, alpha=0.7) +
  geom_line(aes(y=average.by.region), linewidth=0.5, alpha=1) +
  coord_cartesian(ylim=c(-10, 10)) 
