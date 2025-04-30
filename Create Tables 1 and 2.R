

### Load Necessary Packages
library(data.table) #Version ...
library(bife) #Version ...
library(dplyr) #Version ...
library(tidyr) #Version ...
library(lfe) #Version ...



# Clear any defined objects from session
rm(list=ls())


# Set working directory where .csv data files are located
setwd("")


# Define the function that compiles desired summary statistics
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


# Define function to compute optimal polynomial point thrown for model specification
MaxPolynomial_finish <- function(fData_finish_first, iMaxPoly, sSpec_finish){
  
  #find optimal polynomial points thrown bife
  AIC_finish <- as.data.frame(matrix(NA,nrow=iMaxPoly,ncol=1))
  for(i in 1:iMaxPoly){
    tryCatch({
      vPoly <- paste("I(throw_no_match^",1:iMaxPoly,")+",sep="") #string with polynomial
      sPoly_reg <- "" #create here a string for the ith polynomial
      for(j in 1:i){
        sPoly_reg <- paste(sPoly_reg,vPoly[j], sep="")
      }
      
      sReg <- paste0(sSpec_finish[1],sPoly_reg,sSpec_finish[2]) #create string with full regression with ith order polynomial
      bife_poly <- bias_corr(bife(as.formula(paste0(sReg,"|pgfe")), data=fData_finish_first)) #run regression
      fAPE <- get_APEs(bife_poly)
      if(exists("fAPE")){
        AIC_finish[i,1] <- summary(bife_poly)$deviance+2*length(bife_poly$coefficients) #calculate AIC from deviance + 2K
      }
      rm(list="fAPE")
    }, error=function(e){})
  }
  
  iBestPoly <- which(AIC_finish$V1==min(AIC_finish$V1,na.rm=T)) #which model has the lowest AIC
  
  #make a string with the best polynomial
  sPoly_reg_best <- ""
  
  for(j in 1:iBestPoly){
    
    sPoly_reg_best <- paste(sPoly_reg_best,vPoly[j], sep="")
    
  }
  
  sPoly_reg_best <- substr(sPoly_reg_best,1,nchar(sPoly_reg_best)-1) #remove trailing "+" symbol
  
  return(sPoly_reg_best)
  
}




# Setup the FOR loop by defining desired competitions and the collection objects for resulting outputs
Names <- c("International","BICC","Super League","Youth") # Vector with names for the data sets

table1_collect <- data.frame() # Create object to collect resulting summary stats

table2_collect <- data.frame() # Create object to collect resulting regression coefficients


### PART 1: EXECUTE FOR LOOP TO COLLECT STATS AND COEFFICIENTS FOR EACH COMPETITION

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
  
  # Save created data before deletion for later use
  save(fData_finish, fData_finish_first, file = paste0(Names[i], "_processed.RData"))
  
  
  ######### Produce and Collect Summary Stats ###########
  
  # Run defined function against relevant dataset
  info_table <- Info_table_func(fData,fData_finish_first, Names, i)
  
  # Add function output to collection object
  table1_collect <- 
    bind_rows(
      table1_collect,
      info_table
    )

  
  ######### Produce and Collect Regression Estimates ###########
  
  # Define the six model specifications for OLS
  sSpec_1 <- c("Finished~","Win_poss_both") 
  sSpec_2 <- c("Finished~","Finish_poss_opp") 
  sSpec_3 <- c("Finished~","Finish_poss_opp_1+Finish_poss_opp_2+Finish_poss_opp_3") 
  sSpec_4 <- c("Finished~","Win_poss_both")
  sSpec_5 <- c("Finished~","Win_poss_both+Finish_poss_opp+Win_poss_both:Finish_poss_opp") 
  sSpec_6 <- c("Finished~","Win_poss_both+Finish_poss_opp_1+Win_poss_both:Finish_poss_opp_1+Finish_poss_opp_2+Win_poss_both:Finish_poss_opp_2+Finish_poss_opp_3+Win_poss_both:Finish_poss_opp_3")
  
  # Select AIC optimal polynomial point thrown for each specification
  sPoly_finish_1_full <- MaxPolynomial_finish(fData_finish, iMaxPoly=10, sSpec_1)
  sPoly_finish_2_full <- MaxPolynomial_finish(fData_finish_first, iMaxPoly=10, sSpec_2)
  sPoly_finish_3_full <- MaxPolynomial_finish(fData_finish_first, iMaxPoly=10, sSpec_3)
  sPoly_finish_4_full <- MaxPolynomial_finish(fData_finish_first, iMaxPoly=10, sSpec_4)
  sPoly_finish_5_full <- MaxPolynomial_finish(fData_finish_first, iMaxPoly=10, sSpec_5)
  sPoly_finish_6_full <- MaxPolynomial_finish(fData_finish_first, iMaxPoly=10, sSpec_6)
  
  # Run regressions with Fernandez bias correction
  reg_finish_1_full <- bias_corr(bife(as.formula(paste0(sSpec_1[1],sSpec_1[2],"+",sPoly_finish_1_full,"|(pgfe)")),data=fData_finish))
  reg_finish_2_full <- bias_corr(bife(as.formula(paste0(sSpec_2[1],sSpec_2[2],"+",sPoly_finish_2_full,"|(pgfe)")),data=fData_finish_first))
  reg_finish_3_full <- bias_corr(bife(as.formula(paste0(sSpec_3[1],sSpec_3[2],"+",sPoly_finish_3_full,"|(pgfe)")),data=fData_finish_first))
  reg_finish_4_full <- bias_corr(bife(as.formula(paste0(sSpec_4[1],sSpec_4[2],"+",sPoly_finish_4_full,"|(pgfe)")),data=fData_finish_first))
  reg_finish_5_full <- bias_corr(bife(as.formula(paste0(sSpec_5[1],sSpec_5[2],"+",sPoly_finish_5_full,"|(pgfe)")),data=fData_finish_first))
  reg_finish_6_full <- bias_corr(bife(as.formula(paste0(sSpec_6[1],sSpec_6[2],"+",sPoly_finish_6_full,"|(pgfe)")),data=fData_finish_first))
  
  # Get average partial effects for each regression
  APE_reg_finish_1_full <- summary(get_APEs(reg_finish_1_full))
  APE_reg_finish_2_full <- summary(get_APEs(reg_finish_2_full))
  APE_reg_finish_3_full <- summary(get_APEs(reg_finish_3_full))
  APE_reg_finish_4_full <- summary(get_APEs(reg_finish_4_full))
  APE_reg_finish_5_full <- summary(get_APEs(reg_finish_5_full))
  APE_reg_finish_6_full <- summary(get_APEs(reg_finish_6_full))
  
  # Extract vector of coefficients for each model
  model1 <- c(APE_reg_finish_1_full[1], NA, NA, NA, NA, NA, NA, NA, NA)
  model2 <- c(NA, APE_reg_finish_2_full[1], NA, NA, NA, NA, NA, NA, NA)
  model3 <- c(NA, NA, APE_reg_finish_3_full[1], APE_reg_finish_3_full[2], APE_reg_finish_3_full[3], NA, NA, NA, NA)
  model4 <- c(APE_reg_finish_4_full[1], NA, NA, NA, NA, NA, NA, NA, NA)
  model5 <- c(APE_reg_finish_5_full[1], APE_reg_finish_5_full[2], NA, NA, NA, APE_reg_finish_5_full[6], NA, NA, NA)
  model6 <- c(APE_reg_finish_6_full[1], NA, APE_reg_finish_6_full[2], APE_reg_finish_6_full[3], APE_reg_finish_6_full[4], NA, APE_reg_finish_6_full[8], APE_reg_finish_6_full[9], APE_reg_finish_6_full[10])
  
  # Compile panel table of coefficients
  coefficients <-
    c(
      "Decisive leg",
      "Opp. can finish",
      "Opp. can finish with 1 dart",
      "Opp. can finish with 2 darts",
      "Opp. can finish with 3 darts",
      "Decisive leg x Opp. can finish",
      "Decisive leg x Opp. can finish with 1 dart",
      "Decisive leg x Opp. can finish with 2 darts",
      "Decisive leg x Opp. can finish with 3 darts"
    )
  
  regression_table <-
    bind_cols(
      coefficients,
      model1, 
      model2, 
      model3, 
      model4, 
      model5, 
      model6
    ) %>%
    rename(
      `Coefficient` = ...1,
      `Model 1` = ...2,
      `Model 2` = ...3,
      `Model 3` = ...4,
      `Model 4` = ...5,
      `Model 5` = ...6,
      `Model 6` = ...7
    ) %>%
    mutate(
      Panel = Names[i]
    )
  
  # Add panel of coefficients to collection object
  table2_collect <- 
    bind_rows(
      table2_collect,
      regression_table
    )
  
}


# Remove all objects that are no longer needed
rm(list = setdiff(ls(), c("table1_collect", "table2_collect")))





### PART 2: FORMAT AND EXPORT FINAL TABLE OF SUMMARY STATISTICS

# Pivot to longer format and clean up lables
table1_output <- table1_collect %>%
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


rm(table1_collect)


# Write output to .csv
write.csv(table1_output, "table1.csv", row.names = FALSE)



### PART 3: FORMAT AND EXPORT FINAL TABLE OF REGRESSION ESTIMATES

# Reorder columns and round to three decimal places
table2_output <- table2_collect %>%
  select(
    Panel,
    Coefficient,
    `Model 1`,
    `Model 2`,
    `Model 3`,
    `Model 4`,
    `Model 5`,
    `Model 6`,
  ) %>%
  mutate(
    Coefficient,
    `Model 1` = round(`Model 1`, digits = 3),
    `Model 2` = round(`Model 2`, digits = 3),
    `Model 3` = round(`Model 3`, digits = 3),
    `Model 4` = round(`Model 4`, digits = 3),
    `Model 5` = round(`Model 5`, digits = 3),
    `Model 6` = round(`Model 6`, digits = 3),
  )


rm(table2_collect)


# Write output to .csv
write.csv(table2_output, "table2.csv", row.names = FALSE)






