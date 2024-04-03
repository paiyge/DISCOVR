options(stringsAsFactors = F)

library(ggplot2)
library(broom)
library(nplr)
library(lubridate)
library(tidyverse)

path <- file.choose()
path.comp.19.20.21 <- "/Users/llaurens/Documents/DISCOVR/FY21 SOT/FY19-20-21_SOT_BiomassComposition.csv"

sot.comp.19.20.21 <- read.csv(path.comp.19.20.21, header = T, na.strings = "NA")


sot.filtered <-
  sot.comp.19.20.21 %>% 
  select(c("SPECIES", "STRAIN","DATETIME",
           "REACTOR","ANALYTICAL_ID","Ash","FAME",
           "Protein","Carbs","Sum", "C", "H", "N")) %>% 
  mutate(Ash = as.numeric(Ash)) %>% 
  #filter(Ash != "*") %>% 
  #filter(FAME != "*") %>% 
  #filter(Carbs.. != "*") %>% 
  #filter(STRAIN == "Scenedesmus obliquus") %>% 
  #filter(STRAIN == "Monoraphidium minutum") %>% 
  #filter(STRAIN == "Desmodesmus armatus") %>% 
  #filter(STRAIN == "Desmodesmus intermedius") %>% 
  mutate(DateTime = mdy_hm(DATETIME)) %>% 
  mutate(Month = month(DateTime)) %>% 
  mutate(season = case_when(
    Month %in% c(12,1,2) ~ "Winter",
    Month %in% 3:5 ~ "Spring",
    Month %in% 6:8 ~ "Summer",
    Month %in% 9:11 ~ "Fall",
    TRUE ~ "WTF")) %>% 
  mutate(Year = year(DateTime)) %>% 
  mutate(Carbs.ashFree = Carbs/((100-Ash)/100)) %>% 
  mutate(FAME.ashFree = FAME/((100-Ash)/100)) %>%
  mutate(Protein.ashFree = Protein/((100-Ash)/100)) %>%
  mutate(C.ashFree = C/((100-Ash)/100)) %>%
  mutate(H.ashFree = H/((100-Ash)/100)) %>%
  mutate(N.ashFree = N/((100-Ash)/100)) %>% 
  mutate(mass.balance = Carbs + FAME + Protein + Ash)
#Fix naming convention
sot.filtered[which(sot.filtered$STRAIN == "T.striata"),"STRAIN"] <- "LANL1001"
sot.filtered[which(sot.filtered$STRAIN == "P.celeri"),"STRAIN"] <- "TG2"

dim(sot.filtered)
is.numeric(sot.filtered$FAME.ashFree)
#sot.filtered$DateTime
#able(c(sot.filtered[,"STRAINID"],sot.filtered[,"Month"]))

write.csv(sot.filtered, 
          file = paste("/Users/llaurens/Documents/DISCOVR/FY21 SOT/","composition_full",sep = "_", "summary_19_20_21.csv"))

sot.summary <-  sot.filtered %>%
  group_by(STRAIN, season, Year) %>%
  summarise_if(is.numeric,
               .funs = list(
                 mean = ~ mean(.x, na.rm = TRUE),
                 sd   = ~ sd(.x, na.rm = TRUE),
                 n    = ~ sum(!is.na(.x))
               ))

#
#
#df1 <-
#    sot.filtered %>%
#      group_by(STRAINID, Month) %>%
#      summarise_if(is.numeric, mean, na.rm = T) %>% 
#      rename_at(vars(-(1:2)), paste0, ".mean")
#
#df2 <-
#    sot.filtered %>%
#      group_by(STRAINID, Month) %>%
#      summarise_if(is.numeric, sd, na.rm = T) %>% 
#      rename_at(vars(-(1:2)), paste0, ".sd")
#
##foo <- function(.x) { sum(! is.na(.x))}
#
#df3 <-
#  sot.filtered %>%
#  group_by(STRAINID, Month) %>%
#  summarise_if(is.numeric, ~sum(! is.na(.x))) %>% 
#  rename_at(vars(-(1:2)), paste0, ".n")
#
#sot.summary <- full_join(df1, df2, by = c("STRAINID", "Month"))
#sot.summary <- full_join(sot.summary, df3, by = c("STRAINID", "Month"))
View(sot.summary)
#summary <- 
#  sot.filtered %>% 
#  group_by(STRAINID, Month) %>% 
#  summarise(FAME.mean = mean(FAME, na.rm = T), 
#            Ash.mean = mean(Ash, na.rm = T), 
#            Carbs.mean = mean(Carbs, na.rm = T), 
#            Protein.mean = mean(Protein, na.rm = T), 
#            C.mean = mean(C, na.rm = T), 
#            H.mean = mean(H, na.rm = T), 
#            N.mean = mean(N, na.rm = T),
#            FAME.ashFree.mean = mean(FAME.ashFree, na.rm = T),
#            Carbs.ashFree.mean = mean(Carbs.ashFree, na.rm = T), 
#            Protein.ashFree.mean = mean(Protein.ashFree, na.rm = T), 
#           C.ashFree.mean = mean(C.ashFree, na.rm = T), 
#           H.ashFree.mean = mean(H.ashFree, na.rm = T), 
#           N.ashFree.mean = mean(N.ashFree, na.rm = T),
#           #also standard deviations
#           FAME.sd = sd(FAME, na.rm = T), 
#           Ash.sd = sd(Ash, na.rm = T), 
#           Carbs.sd = sd(Carbs, na.rm = T), 
#           Protein.sd = sd(Protein, na.rm = T), 
#           C.sd = sd(C, na.rm = T), 
#           H.sd = sd(H, na.rm = T), 
#           N.sd = sd(N, na.rm = T),
#           FAME.ashFree.sd = sd(FAME.ashFree, na.rm = T),
#           Carbs.ashFree.sd = sd(Carbs.ashFree, na.rm = T), 
#           Protein.ashFree.sd = sd(Protein.ashFree, na.rm = T), 
#           C.ashFree.sd = sd(C.ashFree, na.rm = T), 
#           H.ashFree.sd = sd(H.ashFree, na.rm = T), 
#           N.ashFree.sd = sd(N.ashFree, na.rm = T),
#           #count = n(),
#           n_FAME = sum( ! is.na(FAME))
#           )

write.csv(sot.summary, 
          file = paste("/Users/llaurens/Documents/DISCOVR/FY21 SOT/", "composition_season",sep = "_", "summary_19_20_21.csv"))
          


sot.filtered.short <- sot.filtered %>% 
  select("STRAIN","DATETIME",
         "REACTOR", "season","ANALYTICAL_ID","Ash","FAME.ashFree",
         "Protein.ashFree","Carbs.ashFree","C.ashFree", "N.ashFree") %>% 
  mutate(DateTime = mdy_hm(DATETIME)) %>% 
  mutate(Month = month(DateTime)) %>% 
  mutate(year = year(DateTime))

sot.comp_long <- pivot_longer(sot.filtered.short, cols = c("FAME.ashFree","Carbs.ashFree", "Protein.ashFree"),
                              names_to = "Comp", values_to = "Conc")

#sot.comp_long <- sot.comp_long %>% 
#  mutate(DateTime = mdy_hm(DATETIME)) %>% 
#  mutate(Month = month(DateTime))


p <- ggplot(sot.comp_long, aes(Conc, Comp, fill = STRAIN)) +
  geom_boxplot() +
  #geom_jitter(height = 0.1, width = 0, alpha = 0.5) +
  #theme_bw() +
  xlim(c(0, 60)) +
  facet_wrap(~season);p


ggplot(sot.comp_long, aes(y = Conc, x = factor(Month), fill = STRAINID)) +
  geom_boxplot() + facet_wrap(~c(Comp, year))

boxplot(sot.filtered[,"Protein.ashFree"] ~ c(sot.filtered[,"Month"]), cex.axis = 1.2, xlab = "Month", ylab = "Protein %AFDW", cex.lab = 1.2)
