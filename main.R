library(lmer);
library(tidyverse);
library(dplyr);
library(magrittr);
library(ggplot2);


skin_sensation = read.csv('Validation of skin sensation summary 27.04.NB.csv') %>%
  dplyr::select(-X, -X.1, -X.2, -X.3, -X.4, -X.5, -X.6, -X.7, -X.8, -X.9, -X.10, -X.11, -X.12) %>%
  dplyr::filter(diff_max != '_') %>%
  mutate(diff_max = as.numeric(diff_max),
         diff_min = as.numeric(diff_min))

names(skin_sensation)[1] = 'ID';

skin_sensation %<>% dplyr::filter(!is.na(diff_max), !is.na(diff_min)) %>%
  mutate(medial_heel_binary = ifelse(medial_heel_bulb < 5, 0, 1),
         lateral_heel_binary = ifelse(lateral_heel_bulb < 5, 0, 1),
         medial_meta_binary = ifelse(medial_metatarsus < 5, 0, 1),
         both_legs_binary = ifelse(LH != 'no' & RH != 'no', 1, 0),
         response = ifelse(Limb == 'RH', diff_min, diff_max));

skin_sensation_time = skin_sensation %>% group_by(ID) %>%
  summarise(time_switch = min(ifelse(Limb == 'RH', 
                                     ifelse(abs(diff_max) > abs(diff_min) + 2, time, NA), 
                                     ifelse(abs(diff_min) > abs(diff_max) + 2, time, NA)), na.rm = T),
            time_abolish = max(time, na.rm = T),
            time_skin_medial_heel = min(ifelse(medial_heel_binary == 0, NA, time), na.rm = T),
            time_skin_lateral_heel = min(ifelse(lateral_heel_binary == 0, NA, time), na.rm = T),
            time_skin_medial_meta = min(ifelse(medial_meta_binary == 0, NA, time), na.rm = T)) %>%
  replace(. == Inf, NA);

skin_sensation_time$time_abolish = ifelse(!is.na(skin_sensation_time$time_switch), NA,
                                          skin_sensation_time$time_abolish);

df = list();
df[['medial_heel']] = skin_sensation_time %>% 
  dplyr::select(time_switch, time_abolish, time_skin_medial_heel);

df[['lateral_heel']] = skin_sensation_time %>% 
  dplyr::select(time_switch, time_abolish, time_skin_lateral_heel);

df[['medial_meta']] = skin_sensation_time %>% 
  dplyr::select(time_switch, time_abolish, time_skin_medial_meta);




