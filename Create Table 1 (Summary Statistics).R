

### Load Necessary Packages
library(data.table) #Version ...
library(bife) #Version ...
library(dplyr) #Version ...
library(tidyr) #Version ...
library(lfe) #Version ...


### Clear any defined objects from session
rm(list=ls())


### Set working directory where .csv data files are located
setwd("")


### Define the function that compiles desired summary statistics
Info_table_func <- function(fData,fData_finish_first,Names, i){
  
  iTotLegs <- nrow(fData%>% group_by(GameNo,LegNo) %>%  slice(1)) #total number of legs
  fPropwin <- fData %>% filter(throw_no==1) %>% group_by(Start) %>% summarise(dProp = mean(Win)) %>% as.data.frame #proportion of starters/responders winning leg
  dPropStartWin <- fPropwin[2,2] #proportion of starters winning leg
  dPropSecondWin <- fPropwin[1,2] #proportion responders winningl leg
  
  
  info_table <- as.data.frame(matrix(0,nrow=1,ncol=0))
  rownames(info_table) <- Names[i]
  info_table$Competition <- Names[i]
  info_table$No_Games <- round(length(unique(fData$GameNo)), digits=0) #number of games
  info_table$No_legs <- iTotLegs #total legs
  info_table$No_Throws <- round(nrow(fData), digits=0) #number of throws
  info_table$No_Players <- round(length(unique(fData$Playername)),digits=0) #number of players
  info_table$Av_no_legs <- mean((fData %>% group_by(GameNo) %>% slice(1))$NumberofLegs) #average legs per match
  info_table$Av_leg_length <- mean((fData %>% group_by(GameNo, LegNo) %>% summarise(Obs=n()))$Obs) #average number of throws per leg
  info_table$Average_score <- mean(fData$score) #average score
  info_table$Average_score_cutoff <- mean(fData[which(fData$throw_no<=3),]$score) #average score in first three throws
  info_table$Prop_start_win <- dPropStartWin #proportion of starters winning leg
  
  #add number of finishing opps, and fraction of those finished
  fin <- fData %>% filter(MinFinishPre==1) %>% summarise(Fin = mean(RemainPost==0),Tot = n())
  info_table$Tot_finish_poss <- fin$Tot
  
  #tot finish opportunities with one dart, only counting first of each leg
  info_table$Tot_finish_poss_first <- nrow(fData_finish_first)
  
  
  info_table$Prop_finish <- fin$Fin
  info_table$Prop_finish_first <- mean(fData_finish_first$Finished)
  
  
  
  return(info_table)
}




### Setup the FOR loop by defining desired competitions and collection object for resulting stats

Names <- c("International","BICC","Super League","Youth") # Vector with names for the data sets

table_collect <- data.frame() # Create object to collect resulting summary stats


### PART 1: EXECUTE FOR LOOP TO COLLECT STATS FOR EACH COMPETITION

for (i in 1:length(Names)) {

  # Read in the data for the competition
  fData <- fread(paste0(gsub(" ","",Names[i]),".csv"))
  
  
  ######### Data Wrangling #########
  
  # Select subset of data for finishing analysis. Add incentive variables
  fData_finish <- fData  %>% 
    select(
      Playername, 
      score, 
      RemainPre, 
      MinFinishPre,
      throw_no, 
      throw_no_match,
      RemainPost,
      MinFinishPost,
      Start,LegNo,
      GameNo,
      Matchtype,
      Win_poss_both,
      NumberofLegs,
      SetNo,
      TotSets,
      SetsWon
    ) %>%
    mutate(
      MinFinishPre_opp = dplyr::lag(MinFinishPost,n=1L, default=9), 
      pgfe = as.factor(paste(Playername,GameNo, sep=" ")), # Number of darts needed to finish by opponent
      Finish_poss_opp = as.numeric(MinFinishPre_opp <=3),  # Dummy variable for whether opponent can finish
      Finished = as.numeric(RemainPost==0),   # Dummy variable for whether player finished
      Finish_poss_opp_1 = as.numeric(MinFinishPre_opp==1), # Dummy for whether opponent can finish with 1 dart
      Finish_poss_opp_2 = as.numeric(MinFinishPre_opp==2),# Dummy for whether opponent can finish with 2 dart
      Finish_poss_opp_3 = as.numeric(MinFinishPre_opp==3)) %>% # Dummy for whether opponent can finish with 3 dart
    filter(
      MinFinishPre==1
    ) %>% 
    as.data.frame() # Restrict data to only 1-dart finish opportunities
  
  
  # Get dataset with only first finishing opportunity per player per leg
  fData_finish_first <- fData_finish %>% 
    group_by(
      pgfe,
      LegNo
    ) %>% 
    slice(
      1
    ) %>% 
    ungroup() %>%
    mutate( #add categorical variables for incentive categories 
      Finish_poss_opp_cat = ifelse(MinFinishPre_opp>=4,0,MinFinishPre_opp), # (both can win match x opp. can finish with 1/2/3 darts)
      Incentive_combined = as.factor(paste(Win_poss_both,Finish_poss_opp, sep="_")), #(both can win match x opp. can finish)
      Incentive_separate = as.factor(paste(Win_poss_both,Finish_poss_opp_cat, sep="_")) #(both can win match x opp. can finish with 1/2/3/4+ darts) 
    ) %>%
    as.data.frame()
  
  
  ######### Produce and Collect Summary Stats ###########
  
  # Run defined function against relevant dataset
  info_table <- Info_table_func(fData,fData_finish_first, Names, i)
  
  # Add function output to collection object
  table_collect <- 
    bind_rows(
      table_collect,
      info_table
    )

  #saving created data before deletion for later use
  save(fData_finish, fData_finish_first, file = paste0(Names[i], "_processed.RData"))
  
  # Remove spent objects
  rm(info_table, fData, fData_finish, fData_finish_first)

}


rm(i)




### PART 2: FORMAT AND EXECUTE FINAL TABLE OF SUMMARY STATISTICS

table_output <- table_collect %>%
  pivot_longer(
    c(2:14),
    names_to = "stat",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = Competition,
    values_from = value
  ) %>%
  mutate(
    Statistic = case_when(
      stat == "No_Games" ~ "Matches",
      stat == "No_legs" ~ "Legs",
      stat == "No_Throws" ~ "Throws",
      stat == "No_Players" ~ "Players",
      stat == "Av_no_legs" ~ "Legs per match",
      stat == "Av_leg_length" ~ "Throws per leg",
      stat == "Average_score" ~ "Points per throw (all)",
      stat == "Average_score_cutoff" ~ "Points per throw (first three)",
      stat == "Prop_start_win" ~ "Starter wins leg (proportion)",
      stat == "Tot_finish_poss" ~ "One-dart finish opportunities",
      stat == "Tot_finish_poss_first" ~ "One-dart finish opportunities (first only)",
      stat == "Prop_finish" ~ "Successful one-dart finish (proportion)",
      stat == "Prop_finish_first" ~ "Successful one-dart finish (proportion, first only)"
    ),
    International = round(International, digits = 3),
    BICC = round(BICC, digits = 3),
    `Super League` = round(`Super League`, digits = 3),
    Youth = round(Youth, digits = 3)
  ) %>%
  select(
    Statistic,
    International,
    BICC,
    `Super League`,
    Youth
  )





