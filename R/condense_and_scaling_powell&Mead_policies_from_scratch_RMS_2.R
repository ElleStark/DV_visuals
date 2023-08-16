# Process policy set data and create barplots representing decision variables
# Script modified from version provided by Reclamation in June 2022
# Modified by E Stark August 2023


rm(list=ls())

options( java.parameters = "-Xmx4g" )
options(scipen = 999)

#require(devtools)
#install_version("XLConnect", version = "1.0.3", repos = "http://cran.us.r-project.org")
#install.packages("XLConnect", type="source", INSTALL_opts = c("--no-multiarch"))
#library(XLConnect)
# install.packages("XLConnectJars")
#library(XLConnectJars)
library(plotrix)
#library(tidyverse)
library(readxl)

workbook.name = "data/inputs/Archive_4Selected.xlsx"
#wb = loadWorkbook(workbook.name)
wb = read_excel(workbook.name)
#df = readWorksheet(wb, sheet = "Sheet1", header = TRUE)
df = wb[1:6,]

#for testing, create snapshot of original df
df_og = df


########################################
####################################### always check the column indices before splitting out policy variables ##
########################################

######################### POWELL ###############

###create a powell df (PTiering) for each policy
for (j in 1:nrow(df)){
  PTierEl = t(df[j, 19:23])
  MeadRefs = t(df[j, 29:33])
  BalMaxOffset = t(df[j, 34:38])
  BalMinOffset = t(df[j, 39:43])
  PRels = t(df[j, 24:28])
  
  PTiering = as.data.frame(cbind(PTierEl, MeadRefs, BalMaxOffset, BalMinOffset, PRels))
  row.names(PTiering) = c("PT1", "PT2", "PT3", "PT4", "PT5")
  colnames(PTiering) = c("PTierEl", "MeadRefEl", "BalMaxOffset", "BalMinOffset", "PRels")
  
  # create examples to test functionality
  
    # PTiering[1:5,1] = c(3700,3390,3380,3370,3370)
    # PTiering[1:5,2] = c(1090,1085,1080,1075,1070)
    # PTiering[1:5,3] = c(1,2,3,4,5)
    # PTiering[1:5,4] = c(1,0,0,1,0)
    # PTiering[1:5,5] = c(5960000,5860000,5760000,5660000,5560000)
  
  #for testing, create snapshot of PTiering before any translations
  
  PTieringOG = PTiering
  
  #replace vals in cols 2-5 w/ row that has highest value of primary release (col 5)
  
  for (i in 1:(nrow(PTiering)-1)){
    #i=1
    if (PTiering[i,1] == PTiering[i+1,1]){
      PTiering[i+1,2] = PTiering[i,2]
      PTiering[i+1,3] = PTiering[i,3]
      PTiering[i+1,4] = PTiering[i,4]
      PTiering[i+1,5] = PTiering[i,5]
    } else { 
      PTiering[i+1,2] = PTiering[i+1,2]
      PTiering[i+1,3] = PTiering[i+1,3]
      PTiering[i+1,4] = PTiering[i+1,4]
      PTiering[i+1,5] = PTiering[i+1,5]
    }
      
  }
  
 
  #create bottom tier (if bottom row elev = 3370, lowest tier > 3370 becomes bottom tier; replace bal var, mead ref, primary rel w/ 99999; just keep elev and release range)
  
  #for testing, create snapshot of variables before bottom tier 
  #PTiering_preBottom = PTiering
  
  if (PTiering[nrow(PTiering),1]==3370 && PTiering[1,1]!=3370){
    next_min = which(PTiering[,1]==min(PTiering$PTierEl[PTiering$PTierEl > 3370]))
    PTiering[next_min,"MeadRefEl"] = PTiering[next_min,"PRels"] = 99999999
    #PTiering[next_min,"BalVars"] = 1
  } 
  
  
  #make MeadRefEl and RElRange = 99999999 if BalVar = 0
  
  # for (i in 1:(nrow(PTiering))){
  #   #i=1
  #   if (PTiering[i,4] == 0){
  #     PTiering[i,2] = PTiering[i,3] = 99999999
  #     }
  #}
  
  
  
  
   #replace repeats w/ 3370 and replace values for rows that were already 3370
  
  for (i in 1:(nrow(PTiering))){
    if (any(PTiering[i,1] == PTiering[-i,1])|PTiering[i,1] == 3370){
      PTiering[i,1] = 3370
      PTiering[i,2] = 99999999
      PTiering[i,3] = 99999999
      PTiering[i,4] = 99999999
      PTiering[i,5] = 99999999
    } else { 
      PTiering[i,1] = PTiering[i,1]
      PTiering[i,2] = PTiering[i,2]
      PTiering[i,3] = PTiering[i,3]
      PTiering[i,4] = PTiering[i,4]
      PTiering[i,5] = PTiering[i,5]
    }
    
  }
  
  #sort based on col 1
  
  PTiering = PTiering[order(PTiering$PTierEl, decreasing = TRUE), ] 
  
  
  
  #distribute columns of powell df back into a single row and combine w/ un-condensed mead variables & objective values

  df[j,19:23] = t(PTiering$PTierEl)
  df[j,29:33] = t(PTiering$MeadRefEl)
  df[j,34:38] = t(PTiering$BalMaxOffset)
  df[j,39:43] = t(PTiering$BalMinOffset)
  df[j,24:28] = t(PTiering$PRels)
  
  #make EqInc = 0 if PT1e = 3700
#   if(df[j,20]==3700){
#     df[j,45] = 99999999
#   }
}



############################## MEAD ####################

short_elev = t(df[,3:10]) 
rownames(short_elev) = c("T1e", "T2e", "T3e", "T4e", "T5e", "T6e", "T7e", "T8e")

df[,11:18] = apply(df[,11:18], 2, function(x) as.numeric(x))
short_vol = df[,11:18]/1000 
short_vol = apply(short_vol, 1, rev) #reverse order of volumes
row.names(short_vol) = c("T1V", "T2V", "T3V", "T4V", "T5V", "T6V","T7V", "T8V")


#create empty data 

compressed_vol = short_vol#[NA,]
compressed_elev = short_elev#[NA,]


#if repeating elevations, replace volume w/ highest **replacing the elevations needs to come first b/c of how RW handles the variables
for(i in 1:ncol(compressed_vol)){
  for(j in 1:nrow(compressed_vol)){
    if (any(compressed_elev[j,i] == compressed_elev[-j,i])){
      compressed_vol[j,i] = max(compressed_vol[which(compressed_elev[,i] == compressed_elev[j,i]), i])
    }
  }
}


#if repeating volumes, replace elevation w/ highest
for(i in 1:ncol(compressed_vol)){
  for(j in 1:nrow(compressed_vol)){
    if (any(compressed_vol[j,i] == compressed_vol[-j,i])){
      compressed_elev[j,i] = max(compressed_elev[which(compressed_vol[,i] == compressed_vol[j,i]), i])
    }
  }
}


###now replace all repeated rows w/ 895 & 99999999, then order descending and ascending to produce condensed tables

for(i in 1:ncol(compressed_vol)){
  for(j in 2:nrow(compressed_vol)){
    if (any(compressed_vol[j,i] == compressed_vol[1:(j-1),i])){
      compressed_elev[j,i] = 895
      compressed_vol[j,i] = 99999999
    }
  }
}


#replace any rows that have vol = 0 w/ 895 and 999999999 (to address 0s in first tiers)
for(i in 1:ncol(compressed_vol)){
  for(j in 1:nrow(compressed_vol)){
    if (compressed_vol[j,i] == 0){
      compressed_elev[j,i] = 895
      compressed_vol[j,i] = 99999999
    }
  }
}


#replace any rows that have elev = 895 w/ 999999999 (to address tiers that originally had volumes w/ elevation = 895)
for(i in 1:ncol(compressed_vol)){
  for(j in 1:nrow(compressed_vol)){
    if (compressed_elev[j,i] == 895){
      compressed_vol[j,i] = 99999999
    }
  }
}


### need to sort based on elevation only ###
#### might need to combine the data frame, then sort everything based on elevaiton ####
compressed_elev = apply(compressed_elev, 2, as.numeric)
condensed_elev = apply(compressed_elev, 2, sort, decreasing=T)
condensed_vol = apply(compressed_vol, 2, sort, decreasing=F)

############################################################


#re-combine
condensed_policies = as.data.frame(rbind(condensed_elev, condensed_vol))
row.names(condensed_policies) = c("T1e", "T2e", "T3e", "T4e", "T5e", "T6e","T7e", "T8e", "T1V", "T2V", "T3V", "T4V", "T5V", "T6V", "T7V", "T8V")
colnames(condensed_policies) = c(1:ncol(condensed_policies))

#turn 99999999 into 0 now that tables have been sorted **leaving this out for now b/c may do policy plotting where anything w/ 9999999 is ignored
#condensed_policies[condensed_policies==99999999] = 0

condensed_policies = t(condensed_policies)

#calculate elevations for two surplus tiers **if domestic surplus distance (d2) = 0, partial surplus goes away
#and surplus takes on the size defined by d1

surplus_sizes = df[,2]#check column indices ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#surplus_sizes2 = surplus_sizes #for comparison

# for(i in 1:nrow(surplus_sizes)){
#   if (surplus_sizes[i,2] == 0){
#     surplus_sizes[i,1:2] = surplus_sizes[i,2:1]
#   }
# }
# 
# #also zero-out partial if d1=d2
# for(i in 1:nrow(surplus_sizes)){
#   if (surplus_sizes[i,2] == surplus_sizes[i,1]){
#     surplus_sizes[i,1] = 0
#   }
# }

#surplus_sizes2[48,]
#surplus_sizes[48,] 



#part_surplus_elev = 1200 - surplus_sizes$d1
surplus_elev = 1200 - as.numeric(surplus_sizes$Mead_Surplus_DV)

#if partial surplus or surplus went away, replace 1200s w/ 99999999
surplus_elev[surplus_elev==1200] = 99999999
#part_surplus_elev[part_surplus_elev==1200] = 99999999

# ID=1:nrow(condensed_policies)

condensed_policies_full = cbind(surplus_elev, condensed_policies)#, df[,45:53]) #check column indices 

#replace Mead variables in original df w/ condensed variables

df = cbind(condensed_policies_full, df[,19:47])


### I didn't see any policies with 8 shortage tiers... need to look into this.... ###

#### write the condensed dataframe to a csv file ####

write.csv(df, "data/outputs/condensed_DVs_6Selected.csv")

### Scaling script #################################################################################################

#specify number of objectives and then create a matrix that has the last n columns of df **assumes you've deleted columns of metrics and constraints

#setwd("Z:/Shared/Research/DMDU continuation/internal work/Mead-and-Powell/RampingRun_01-21-22_10kFE")
#workbook.name = "NewMaster_RampingRun_10kFE_withID.xlsx"
#wb = loadWorkbook(workbook.name)
#df = readWorksheet(wb, sheet = "Sheet1", header = TRUE)

 
numobj = 4

archive.mat = as.matrix(df[,(ncol(df)-(numobj-1)):ncol(df)])

archive.scaled = matrix(999, nrow = nrow(archive.mat), ncol = ncol(archive.mat))

for (i in 1:ncol(archive.mat)){
  archive.scaled[,i] = rescale(archive.mat[,i], c(0,1))
}

colnames(archive.scaled) = paste(colnames(archive.mat), "SCALED", sep = " ")

#scaled.workbook.name = "All_values_RampingRun_5kFE_withID_Objectives_Scaled.xlsx"

write.csv(archive.scaled, "FullHydrology_RampingRun_5kFE_withID_Objectives_Scaled.csv")

#writeWorksheetToFile(scaled.workbook.name, cbind(c(1:nrow(archive.mat)), archive.mat), sheet = "OG", header = T)

#writeWorksheetToFile(scaled.workbook.name, cbind(c(1:nrow(archive.mat)), archive.scaled),  sheet = "SCALED", header = T)





