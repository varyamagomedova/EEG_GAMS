#new preprocessing

coords <- as_tibble(data.table::fread('r_channel_positions.txt',
                                      col.names = c('channel', 
                                                    'hem','reg','z')))
channel_groups <- list(
  #tons of face movement artifats here
  Prefrontal = c( "Fp1", "Fp2", "AFp1", "AFp2", "AF3", "AF4", "AF7", "AF8", "AFF1h", "AFF2h", "AFF5h", "AFF6h"),
  Frontal = c("F1", "F2", "F3", "F4", "F5", "F6",  "F9", "F10"),
  Motor = c("FC1", "FC2", "FC3", "FC4",  "FCC6h","FFC1h",  "FFC2h",  "FFC3h",  "FFC4h",  "FFC5h",  "FFC6h", "FCC1h", "FCC2h", "FCC3h", "FCC4h", "FCC5h", "FCC6h" ),
  Central = c("C1", "C2", "C3", "C4", "C5", "C6", "CCP1h",  "CCP2h" , "CCP3h",  "CCP4h",  "CCP5h",  "CCP6h"),
  Broca = c( "FC5", "FC6","FT9", "FT10"),
  #but also some artifacts here
  FLateral= c("F7", "F8","FFT7h",  "FFT8h","F9", "F10","FFT10h",  "FFT9h"),
  Temporal = c("T7", "T8", "TPP10h", "TPP7h",  "TPP8h" , "TPP9h" ,"FT7", "FT8",  "TP7", "TP8", "TP9", "TP10", "FTT7h", "FTT8h", "FTT9h", "FTT10h", "TTP7h", "TTP8h", "TTP9h", "TTP10h"),
  Parietal = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10",  "PPO10h" ,"PPO1h" , "PPO2h" , "PPO5h",  "PPO6h",  "PPO9h", "CP1", "CP2", "CP3", "CP4", "CP5", "CP6", "CPP1h", "CPP2h", "CPP3h", "CPP4h", "CPP5h", "CPP6h"),
  Occipital = c("O1", "O2", "O9", "O10","OI1h" ,  "OI2h", "PO3", "PO4", "PO7", "PO8", "PO9", "PO10", "POO1", "POO2", "POO9h", "POO10h"),
  NoHem = c("Iz", "AFz", "Fz", "FCz", "CPz", "Pz", "POz","Cz", "Fpz","Oz")
)
# Function to annotate data
annotate_regions <- function(data, channel_groups) {
  data$region <- sapply(data$channel, function(channel) {
    group <- "Unknown"
    for (g in names(channel_groups)) {
      if (channel %in% channel_groups[[g]]) {
        group <- g
        break
      }
    }
    return(group)
  })
  return(data)
}

get_data <- function(file_list){
  data=data.frame(  time = numeric(),
                    event = factor(),
                    condition = factor(),
                    FTF = numeric(),
                    channel = factor(),
                    data = numeric(),
                    subject = factor())
  for (file in file_list) {
    print(file)
    temp_data <- read.csv(file, header = TRUE, stringsAsFactors = TRUE)  
    data <- rbind(data, temp_data)
  }
  return(data)
}

noun_files <- list.files("data_dgame2_2Varya/noun",pattern = "*.csv", full.names=TRUE)
noun_data <- get_data(noun_files)

noun_data<- noun_data  %>% left_join(coords) %>%
  mutate(hemisphere = case_when(hem < 0   ~"left",
                                hem > 0 ~ "right",
                                hem== 0.0000 ~ "central"))%>% 
  annotate_regions(channel_groups) %>% 
  mutate_if(is.character, as.factor)
write.csv(noun_data, "noun_data_annotatedVM.csv")

#old preprocessing

# get_data <- function(file_list){
#   data=data.frame(  time = numeric(),
#                     event = factor(),
#                     condition = factor(),
#                     FTF = numeric(),
#                     channel = factor(),
#                     data = numeric(),
#                     subject = factor())
#   for (file in file_list) {
#     print(file)
#     temp_data <- read.csv(file, header = TRUE, stringsAsFactors = TRUE)  
#     data <- rbind(data, temp_data)
#   }
#   data = data %>% left_join(coords) %>%
#     mutate(hemisphere = case_when(hem < 0   ~"left",
#                                   hem > 0 ~ "right",
#                                   hem== 0.0000 ~ "central"),
#            region = case_when(reg> 0 & reg<= 0.0714 ~ "frontal",
#                               reg> 0.0714~ "prefrontal",
#                               reg< 0 & reg>= -0.0929 ~ "posterior",
#                               reg< -0.0929 ~ "occipital",
#                               reg== 0.0000 ~ "central"))
#   return(data)
# }
# 
# coords <- as_tibble(data.table::fread('r_channel_positions.txt',
#                                       col.names = c('channel', 
#                                                     'hem','reg','z')))
# 
# noun_files <- list.files("data_dgame2_2Varya/noun",pattern = "*.csv", full.names=TRUE)
# noun_data <- get_data(noun_files)%>% mutate_if(is.character, as.factor)