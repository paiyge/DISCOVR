library(readxl) 
library(xlsx)
library(dplyr)
library(tidyr)
library(stringr)

setwd("~/Desktop/R/comp standards")

###
#questions for Lieve
#  Are we always going to not use galacturonic acid
#  Will you always want to discard the ASU Nanno QC and will it always be on the form

# input file name
filename = "220323_CompositionSummary_Oceanium_LL[1].xlsx"

# Hardcode correction factor for uronic acids
galacturonic_acid_recovery <- sum(56.88/100)
guluronic_acid_recovery <- sum(56.77/100)
glucuronic_acid_recovery <- sum(78.22/100)
mannuronic_acid_recovery <- sum(76.32/100)

# read in sheets
sheet_names <- excel_sheets(filename)
data <- lapply(sheet_names, read_excel, path=filename)

for (sheet in 1:length(sheet_names)){
  sheet_data <- as.data.frame(data[sheet])
  #names(sheet_data) <- as.matrix(sheet_data[1,])
  #sheet_data <- sheet_data[-1, ]
  sheet_data[] <- lapply(sheet_data, function(x) type.convert(as.character(x)))
  assign(sheet_names[sheet], sheet_data)}

#####
fatty_acids <- c(`C8:0 Caprylic acid` = "C8.0", 
                 `C10:0 Capric acid` = "C10.0..",
                 `C12:0 Lauric acid` = "C12.0..",         
                 `C14:0 Myristic acid`  = "C14.0..", 
                 `C14:1 Myristoleic acid` = "C14.1..",     
                 `C15:0 Pentadecanoic acid` = "C15.0..",  
                 `C16:0 Palmitic acid` = "C16.0..",     
                 `C16:1 Hexadeca-11-enoic acid` = "C16.1n11..",  
                 `C16:1 Hexadeca-9-enoic acid, palmitic acid` = "C16.1n9..",   
                 `C16:1 Hexadeca-7-enoic acid` = "C16.1n7..",   
                 `C16:1 Hexadeca-6-enoic acid` = "C16.1n6..",  
                 `C16:1 Hexadeca-5-enoic acid` = "C16.1n5..",  
                 `C16 unknown acid` = "C16.unknown1..",   
                 `C16:2 Hexadeca-7,10-dienoic acid` = "C16.2..", 
                 `C16:3 Hexadeca-9,11,13-trienoic acid` = "C16.3..",
                 `C16:4 Hexadeca-7,9,11,13-tetraenoic acid` = "C16.4..", 
                 `C17:0 Heptadecanoic acid` = "C17.0..",             
                 `C17:1 Heptadecenoic acid` = "C17.1..",        
                 `C18.0 Stearic acid` = "C18..",          
                 `C18.1 Oleic (n-9) acid` = "C18.1n9..",           
                 `C18.1 Vaccenic (n-7) acid` = "C18.1n7..",      
                 `C18:2 Linoleic acid` = "C18.2n6..",      
                 `C18:3 Gamma Linoleic acid` = "C18.3n6..",      
                 `C18:3 Alpha Linoleic acid` = "C18.3n3..",        
                 `C18:4 Octadecatetraenoic, stearidonic acid` = "C18.4n3..",           
                 `C20:0 Arachidic acid` = "C20.0..",      
                 `C20:1 Eicosenoic acid` = "C20.1..",
                 `C20:2 Eicosadienoic acid` = "C20.2..",          
                 `C20:3 Eicosatrienoic (n-6) acid` = "C20.3n6..",        
                 `C20:4 Arachidonic (n-6) acid` = "C20.4n6..",           
                 #`???` = "C20.3n3..", NOT PRESENT IN PDF   
                 `C20:4 Eicosatetraenoic (n-3) acid` = "C20.4n3..",       
                 `C20:5 Eicosapentaenoic acid` = "C20.5n3..",      
                 `C22:0 Behenic acid` = "C22.0..",        
                 `C22:1 Erucic acid` = "C22.1n9..",           
                 `C22:2 Docosadenienoic acid` = "C22.2..",
                 #`C22:3 Docosatrienoic acid` = "C22.3..", THIS DOES NOT EXIST IN FILE
                 `C22:4 Docosatetraenoic n3 acid` = "C22.4..",         
                 #`???` = "C22.5n6..", NOT PRESENT IN PDF)       
                 `C22:5 Docosapentaenoic n6 acid` = "C22.5n3..",       
                 `C22:6 Docohexaenoic acid` = "C22.6n3..",   
                 `C24:0 Lignoceric acid` = "C24.0..")
                 #`???` = "C11.cyclopentane..", NOT PRESENT IN PDF     
                 #`???` = "C13.cyclopentane.. "NOT PRESENT IN PDF   
                 

########################################
# Table 1: Sample identifiers and short designations
# need to remove"short", "long" headers

sample_names <- `Sample Info`[ , c(1)]
sample_names <- na.omit(sample_names)

# long names from file
for (i in 1:length(sample_names)){
  if (sample_names[i] != "Notebook reference:"){
    sample_names <- sample_names[-1]}}
long_names <- sample_names[ -c(1,2)]

# short names for pdf
for (j in 1:length(sample_names)){
  sample_names[j] <- word(sample_names[[j]], 1)}
short_names <- sample_names[ -c(1,2)]

table_1 <- data.frame(long_names, short_names)

########################################
# Table 2: Mass fraction values for proximate biomass composition [g/100 g, ± stdev]
ash_data <- `Solids and Ash`[ , c(1,5,6)] # need to fix column headers to make one line
ash_data <- ash_data %>% rename("SampleID" = 1, 'ash' = 2, 'ash.stdev' = 3)

carb_data <- Carbohydrates[ , c(1,14,15)]
# carb_data <- Carbohydrates[ , c("Sample ID', "Carbs*", "Carbs")]
carb_data <- carb_data %>% rename("SampleID" = 1, 'carbs' = 2, 'carb.stdev' = 3)

fame <- `FAME and Cyclopentane FA`[ , c(1,2,3)]
# fame <- `FAME and Cyclopentane FA`[ , c("SampleID", "% Total FAME* Mean", "% Total FAME* Std Dev")]
fame <- fame %>% rename("SampleID" = 1, 'fame' = 2, 'fame.stdev' = 3)

if(exists("Uronics")){
  no_stdev <-`Compositional Summary`[ , c("Sample.ID", "Uronics....", "Protein...", "Sum..")]
  no_stdev <- no_stdev %>% rename("SampleID" = 1, "Uronic acids (PAD)" = 2, "Protein (N x 4.92)" = 3, "Mass Closure" = 4)
} else {
  no_stdev <-`Compositional Summary`[ , c("Sample ID", "Protein*", "Sum")]
  no_stdev <- no_stdev %>% rename("SampleID" = 1, "Protein (N x 4.92)" = 2, "Mass Closure" = 3)
}

# Merging 
with_stdev <- merge(ash_data, carb_data, row = "SampleID")
with_stdev <- merge(with_stdev, fame, row = "SampleID") 
table_2 <- merge(with_stdev, no_stdev, row = "SampleID") %>%
  na.omit(table_2)

# Making numeric and rounding to 2 digits
table_2[, 2:ncol(table_2)] <- lapply(2:ncol(table_2), function(x)as.numeric(table_2[[x]]))
table_2[, 2:ncol(table_2)] <- lapply(2:ncol(table_2), function(x)round(table_2[[x]], digits = 2))

# Combining value and stdev columns into one
table_2 <- unite(table_2, col='Ash', c('ash', 'ash.stdev'), sep = " \u00B1 ", remove = TRUE)
table_2 <- unite(table_2, col='Carbohydrates (PAD)', c('carbs', 'carb.stdev'), sep = " \u00B1 ", remove = TRUE)
table_2 <- unite(table_2, col='Lipids (as FAME)', c('fame', 'fame.stdev'), sep = " \u00B1 ", remove = TRUE)

# Removing SampleID column name and changing row names
table_2 <- table_2 %>% rename(' ' = 1)
rownames(table_2) <- c(short_names)
table_2 <-  table_2[, -c(1)] # remove row

########################################
# Table 3:  Relative mass fraction values for monosaccharides comprising the carbohydrate fraction
carb_data <- Carbohydrates[, c('Sample.ID', 'Mannitol..', 'Fucose..','Rhamnose..','Galactosamine..','Arabinose..',
                               'Glucosamine..','Galactose..','Glucose..', 'Mannose..','Xylose..','Ribose..')]
carb_data <- na.omit(carb_data)
carb_data <- carb_data %>% filter(row_number() <= n()-1) # remove unneeded rows
table_3 <- carb_data %>%
  rename(
    'SampleID' = 1,'Mannitol' = 2,'Fucose' = 3,'Rhamnose' = 4,'Galactosamine' = 5,'Arabinose' = 6,
    'Glucosamine' = 7,'Galactose' = 8,'Glucose' = 9, 'Mannose' = 10,'Xylose' = 11,'Ribose' = 12)

rownames(table_3) <- c(short_names)
table_3 <- table_3[, -c(1)]

table_3 <- mutate_all(table_3, function(x) as.numeric(as.character(x)))
table_3 <- round(table_3, digits = 2)

#ISSUE this is only giving to 1 decimal point <---<--<--<--<-- 
corrected <- rowSums(table_3, na.rm = TRUE)

# relative mass calculations
for (r in 1:nrow(table_3)){
  for (c in 1:ncol(table_3)){
    table_3[[c]][[r]] <- sum((table_3[[c]][[r]] / corrected[r]) * 100)}}

if(exists("Uronics")){
  uronics_data <- Uronics[, c('Sample.ID', 'Corrected.Gul..', 'Corrected.Gluc..', 'Corrected.Mann..')] 
  uronics_data <- na.omit(uronics_data)
  uronics_data <- uronics_data %>% filter(row_number() <= n()-1 )%>% # remove last rows %>%
    rename('SampleID' = 1, 'Guluronic acid' = 2, 'Glucuronic acid' = 3, 'Mannuronic acid' = 4)
  
  # Rounding to 2 digits
  rownames(uronics_data) <- c(short_names)
  uronics_data <- uronics_data[, -c(1)] # remove unneeded cols
  uronics_data <- mutate_all(uronics_data, function(x) as.numeric(as.character(x)))
  
  corrected <- rowSums(uronics_data, na.rm = TRUE)
  for (r in 1:nrow(uronics_data)){
    for (c in 1:ncol(uronics_data)){
      uronics_data[[c]][[r]] <- sum(uronics_data[[c]][[r]] / corrected[[r]] * 100)}}
  table_3 <- merge(table_3, uronics_data, by="row.names")
}

rownames(table_3) <- c(short_names)
table_3 <- table_3[, -c(1)]
table_3 <- round(table_3, digits = 2)
table_3 <- as.data.frame(t(table_3)) # switch col and rows
table_3[is.na(table_3)] <- '<loq'

########################################
# Table 4: Relative Mass Fraction Values for Fatty Acids Comprising the Lipid Fraction
# Need to get into numeric form to be able to round

fame <- `FAME and Cyclopentane FA`[, c(10:ncol(`FAME and Cyclopentane FA`))]
fame <- na.omit(fame)
fame <- fame %>% filter(row_number() <= n()-1) # remove unneeded rows
rownames(fame) <- c(short_names)
fame <- round(fame, digits = 2)

fatty_acids <- colnames(fame)

fame %>% 
  rename(any_of(fatty_acids))

  fame_and_cyclopentane_fa <- data.frame(table4[-c(2:9)])
  fame_and_cyclopentane_fa <- na.omit(fame_and_cyclopentane_fa)
  fame_and_cyclopentane_fa <- fame_and_cyclopentane_fa %>%
    rename(
      'C8:0 Caprylic acid' = 1,
      'C10:0 Capric acid' = 3,
      'C12:0 Lauric acid' = 4,
      'C14:0 Myristic acid' = 5,
      'C14:1 Myristoleic acid' = 6,
      'C15:0 Pentadecanoic acid' = 7,
      'C16:0 Palmitic acid' = 8,
      'C16:1 Hexadeca-11-enoic acid' = 9,
      'C16:1 Hexadeca-9-enoic acid, palmitic acid' = 10,
      'C16:1 Hexadeca-7-enoic acid' = 11,
      'C16:1 Hexadeca-6-enoic acid' = 12,
      'C16:1 Hexadeca-5-enoic acid' = 13,
      'C16 unknown acid' = 14,
      'C16:2 Hexadeca-7,10-dienoic acid' = 15,
      'C16:3 Hexadeca-9,11,13-trienoic acid' = 16,
      'C16:4 Hexadeca-7,9,11,13-tetraenoic acid' = 17,
      'C17:0 Heptadecanoic acid' = 18,
      'C17:1 Heptadecenoic acid' = 19,
      'C18:0 Stearic acid' = 20,
      'C18:1 Oleic (n-9) acid' = 21,
      'C18:1 Vaccenic (n-7) acid' = 22,
      'C18:2 Linoleic acid' = 23,
      'C18:3 Gamma Linoleic acid' = 24,
      'C18:3 Alpha Linoleic acid' = 25,
      'C18:4 Octadecatetraenoic, stearidonic acid' = 26,
      'C20:0 Arachidic acid' = 27,
      'C20:1 Eicosenoic acid' = 28,
      'C20:2 Eicosadienoic acid' = 29,
      'C20:3 Eicosatrienoic (n-6) acid' = 30,
      'C20:4 Eicosatetraenoic (n-3) acid' = 31,
      'C20:4 Arachidonic (n-6) acid' = 32,
      'C20:5 Eicosapentaenoic acid' = 33,
      'C22:0 Behenic acid' = 34,
      'C22:1 Erucic acid' = 35,
      'C22:2 Docosadenienoic acid' = 36,
      'C22:3 Docosatrienoic acid' = 37,
      'C22:4 Docosatetraenoic n3 acid' = 38,
      'C22:5 Docosapentaenoic n6 acid' = 39,
      'C22:6 Docohexaenoic acid' = 40,
      'C24:0 Lignoceric acid' = 41,
    )
  fame_and_cyclopentane_fa <- as.data.frame(t(fame_and_cyclopentane_fa)) # switch col and rows
  fame_and_cyclopentane_fa <- fame_and_cyclopentane_fa[-c(1, 10)] # remove unneeded col
  fame_and_cyclopentane_fa <- fame_and_cyclopentane_fa[-c(1),] # remove unneeded row
  fame_and_cyclopentane_fa <- fame_and_cyclopentane_fa %>%
    rename(
      '22020201' = 1, '22020202' = 2, '22020203' = 3, '22020204' = 4, 
      '22020205' = 5, '22020206' = 6, '22020207' = 7, '22020208' = 8,
    )
  
########################################
# Stuff that wasn't needed

# change number columns to numeric when you dont know which are numeric
with_stdev[,1:ncol(with_stdev)]=lapply(1:ncol(with_stdev),function(x) {
  tryCatch({
    as.numeric(with_stdev[[x]])
  },warning = function(w) {
    with_stdev[[x]]}
  )} )

# conver str to num
table_2 <- mutate_all(table_2, function(x) as.numeric(as.character(x)))

fatty_acids <- c('C8:0 %' = 'C8:0 Caprylic acid', 'C10:0 %', 'C12:0 %',	'C14:0 %', 'C14:1 %', 'C15:0 %', 'C16:0 %', 'C16:1n11 %',
                 'C16:1n9 %', 'C16:1n7 %', 'C16:1n6 %', 'C16:1n5 %', 'C16unknown1 %', 'C16:2 %', 'C17:0 %', 'C17:1 %', 'C16:3 %', 'C16:4 %', 'C18 %', 'C18:1n9 %', 'C18:1n7 %',
                 'C18:2n6 %', 'C18:3n6 %', 'C18:3n3 %', 'C18:4n3 %', 'C20:0 %', 'C20:1 %', 'C20:2 %', 'C20:3n6 %', 'C20:4n6 %',	
                 'C20:3n3 %', 'C20:4n3 %', 'C20:5n3 %', 'C22:0 %', 'C22:1n9 %', 'C22:2 %', 'C22:4 %', 'C22:5n6 %', 'C22:5n3 %',
                 'C24:0 %', 'C22:6n3 %', 'C11 cyclopentane %', 'C13 cyclopentane %')

# List of fatty acids to include
fatty_acids_short = list('C8:0',	
                         'C10:0', 
                         'C12:0', 
                         'C14:0', 'C14:1', 
                         'C15:0', 
                         'C16:0', 'C16:1n11', 'C16:1n9', 'C16:1n7', 'C16:1n6', 'C16:1n5', 'C16 unknown1', 'C16:2', 'C16:3', 'C16:4', 
                         'C17:0', 'C17:1', 
                         'C18', 'C18:1n9', 'C18:1n7', 'C18:2n6', 'C18:3n6', 'C18:3n3', 'C18:4n3', 
                         'C20:0', 'C20:1', 'C20:2',	'C20:3n6', 'C20:4n6', 'C20:3n3', 'C20:4n3', 'C20:5n3', 
                         'C22:0', 'C22:1n9', 'C22:2', 'C22:4', 'C22:5n6', 'C22:5n3', 'C22:6n3',
                         'C24:0', 
                         'C11 cyclopentane', 'C13 cyclopentane')

fatty_acids_long = list('C8:0 Caprylic acid',
                        'C10:0 Capric acid',
                        'C12:0 Lauric acid',
                        'C14:0 Myristic acid',
                        'C14:1 Myristoleic acid',
                        'C15:0 Pentadecanoic acid', 
                        'C16:0 Palmitic acid',
                        'C16:1 Hexadeca-11-enoic acid',
                        'C16:1 Hexadeca-9-enoic acid, palmitic acid', 
                        'C16:1 Hexadeca-7-enoic acid',
                        'C16:1 Hexadeca-6-enoic acid',
                        'C16:1 Hexadeca-5-enoic acid',
                        'C16 unknown acid',
                        'C16:2 Hexadeca-7,10-dienoic acid',
                        'C16:3 Hexadeca-9,11,13-trienoic acid',
                        'C16:4 Hexadeca-7,9,11,13-tetraenoic acid',
                        'C17:0 Heptadecanoic acid',
                        'C17:1 Heptadecenoic acid',
                        'C18:0 Stearic acid',
                        'C18:1 Oleic (n-9) acid',
                        'C18:1 Vaccenic (n-7) acid',
                        'C18:2 Linoleic acid',
                        'C18:3 Gamma Linoleic acid',
                        'C18:3 Alpha Linoleic acid',
                        'C18:4 Octadecatetraenoic, stearidonic acid',
                        'C20:0 Arachidic acid',
                        'C20:1 Eicosenoic acid',
                        'C20:2 Eicosadienoic acid',
                        'C20:3 Eicosatrienoic (n-6) acid',
                        'C20:4 Eicosatetraenoic (n-3) acid',
                        'C20:4 Arachidonic (n-6) acid',
                        'C20:5 Eicosapentaenoic acid',
                        'C22:0 Behenic acid',
                        'C22:1 Erucic acid',
                        'C22:2 Docosadenienoic acid',
                        'C22:3 Docosatrienoic acid',
                        'C22:4 Docosatetraenoic n3 acid',
                        'C22:5 Docosapentaenoic n6 acid',
                        'C22:6 Docohexaenoic acid',
                        'C24:0 Lignoceric acid')
