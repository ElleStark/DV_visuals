# Prepare data frames for stacked bar plotting of policy decision variables
# Phase III robustness calculations
# Nathan Bonham
# 2/10/21

library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)
library(patchwork)

rm(list=ls())

#setwd("Z:/Shared/Research/DMDU continuation/Kasprzyk project 2019/files from Nathan/R code/plotting related")
setwd("D:/PowellPols_data/FullRampingRun_6.6.22")
Archive='FullHydrology_RampingRun_5kFE_withID_after_condensing.csv'

Archive.df=read.table(Archive, header = T, sep = ",")



#################### Create data frames for stacked bar plots ###################



#########################  MEAD  ##################################

lastDV=which(colnames(Archive.df)=='T8V')

dv=Archive.df[,2:lastDV]

# prepare for plotting
elevation=dv[1:10]
elevation=cbind(rep(1220,nrow(elevation)), elevation)
names(elevation) = c("Top",names(elevation[2:11]))
elevation$dead.pool=895 # need to add dead pool or tier 6 is thrown out for policies where it exists

#when one or more surplus tiers does not exist, need to replace number to make e_delta work
for(j in 1:nrow(elevation)){
  if (elevation[j,2]==99999999)
    elevation[j,2] = 1220
  if (elevation[j,3]==99999999)
    elevation[j,3] = elevation[j,2]
}

el_max=unlist(apply(elevation, 1, max)) # save max elevation for ggplotting later

e_delta = elevation[,1:11] #initialize e_delta

for(i in 2:12){
  e_delta[i-1]=elevation[i-1]-elevation[i] # difference between tiers, used for plotting
}

volume=dv[11:18]
volume[volume==99999999] =0
volume$dead.pool=0
maxVol=apply(volume,1, max)

tier_bin=volume>0 # identify actual tiers
nTiers=apply(tier_bin, 1, sum) # sum number of tiers. You want this for filtering in your web app

volume_labs=matrix(NA, nrow=nrow(volume), ncol=ncol(volume)) # create shortage volume labels, ex V = 500 KAF
for (i in 1:ncol(volume_labs)){
  volume_labs[,i]=paste(volume[,i], ' KAF', sep='')
}
volume_labs[volume_labs=="0 KAF"]=NA

volume_labs=data.frame(volume_labs) 
#add 3 columns to volume df so it becomes the same number of columns as e_delta (which has top and surplus tiers now)
volume = cbind(rep(NA,nrow(volume)), rep(NA,nrow(volume)),rep(NA,nrow(volume)), volume)
names(volume) = c("Top","Surplus", "PartSurplus", names(volume[4:12]))

volume_labs = cbind(rep("Surplus",nrow(volume_labs)), rep("Partial Surplus",nrow(volume_labs)), rep("Normal",nrow(volume_labs)), volume_labs)
names(volume_labs) = names(volume)

# add policy ID to each data frame
elevation$policy=as.character(1:nrow(dv))
e_delta$dead_pool=895
e_delta$policy=as.character(1:nrow(dv))
volume$policy=as.character(1:nrow(dv))
volume_labs$policy=as.character(1:nrow(dv))

piv_col=which(colnames(e_delta)=='dead_pool')

# wide to long format for ggplot
elevation=pivot_longer(elevation, cols=1:piv_col, names_to = 'Tier')
e_delta=pivot_longer(e_delta, cols=1:piv_col, names_to = 'Tier') 

e_delta$Tier=factor(e_delta$Tier, levels = c("Top","surplus_elev", "part_surplus_elev", "T1e","T2e","T3e","T4e","T5e", "T6e", "T7e", "T8e", "dead_pool")) # need to reorder the factor levels such that dead pool is last
e_delta=dplyr::rename(e_delta, 'delta'='value')
# stacked bar plot order depends on the level order

volume=pivot_longer(volume, cols=1:piv_col, names_to = 'Tier')
volume$value = as.numeric(volume$value)
volume_labs=pivot_longer(volume_labs, cols=1:piv_col, names_to = 'Tier')
volume_labs=data.frame(volume_labs, elevation=elevation$value)

#remove label for partial surplus when it doesn't really exist (can start at row 2 b/c row 1 is Surplus)
for (i in 2:nrow(volume_labs)){
  if(volume_labs[i,2]=="PartSurplus" && volume_labs[i,4]==volume_labs[i-1,4]){
    volume_labs[i-1,3] = NA
  }
}

#remove label for surplus when it doesn't really exist (can start at row 2 b/c row 1 is Top)
for (i in 2:nrow(volume_labs)){
  if(volume_labs[i,2]=="Surplus" && volume_labs[i,4]==1220){
    volume_labs[i-1,3] = NA
  }
}



#add color to volume df
volume_col = as.data.frame(read.table("vol_gradient3.txt", header = T))
volume_col$color = as.character(volume_col$color)

for(i in 1:nrow(volume_col)){
volume_col[i,2] = paste(as.character("#"), volume_col[i,2], sep = "")
}

volume$color=""

for(i in 1:nrow(volume)){
  if(is.na(volume[i,3]) || volume[i,3]==0){
    volume[i,4] = "#000000"
  }
  else if(volume[i,3]>0){
    p = as.numeric(volume[i,3])
    volume[i,4] = volume_col[which(volume_col$volume==p),2]
  }
}

for(i in 1:nrow(volume)){
  if (volume[i,2] == "Top"){
    volume[i,4] = "#6d46a5"
  }
  if (volume[i,2] == "Surplus"){
    volume[i,4] = "#4659a5"
  }
  if (volume[i,2] == "PartSurplus"){
    volume[i,4] = "#bdbfce"
  }
  
}

df=data.frame(e_delta, v_lab= volume_labs$value, v_col= volume$color, elevation=volume_labs$elevation, volume=volume$value)
df$Tier=rep(c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l'), length(unique(df$policy)))
df$TierName=rep(c('Surplus', 'Partial Surplus', 'Normal', '1', '2', '3', '4', '5', '6', '7', '8', 'Dead Pool'), length(unique(df$policy)))

#change to number and add zeros for image ordering in tableau
df$policy = as.numeric(df$policy)
df$policy = sprintf("%04d", df$policy)

############# Create data frame for policy labels, SOM node labels, and label y position

stacked_bar_plot_data=list(long_data=df)#, wide_data=policy_SOM_labs)
saveRDS(stacked_bar_plot_data, file='data for stacked mead bar plot.rds')

write.csv(df, "df.csv")



#########################  POWELL  ##################################

firstpDV=which(colnames(Archive.df)=='PT1e')
lastpDV=which(colnames(Archive.df)=='EqInc')

pdv=Archive.df[,firstpDV:lastpDV]

rel_ranges.df=read.table("powell_release_range_table.txt", header = T, sep = )

#################### Create data frame for stacked bar plots ###################

#translate dummy variables into NA
pdv[pdv==99999999] = NA


# prepare for plotting
p.elevation=pdv[1:5]
p.elevation=cbind(rep(3700,nrow(p.elevation)), p.elevation)
names(p.elevation) = c("Equalization",names(p.elevation[2:ncol(p.elevation)]))
p.elevation$dead.pool=3370 # need to add dead pool or tier 6 is thrown out for policies where it exists

p.e_delta = p.elevation[,1:6] #initialize e_delta

for(i in 2:ncol(p.elevation)){
  p.e_delta[i-1]=p.elevation[i-1]-p.elevation[i] # difference between tiers, used for plotting
}


mead_ref = pdv[,6:10]
mead_ref=cbind(rep(NA,nrow(mead_ref)), mead_ref)
names(mead_ref) = c("Equalization",names(mead_ref[2:ncol(mead_ref)]))

ranges = pdv[11:15]
ranges=cbind(rep(NA,nrow(ranges)), ranges)
names(ranges) = c("Equalization",names(ranges[2:ncol(ranges)]))

#translate the range variable to the actual release ranges
for(i in 1:nrow(ranges)){
  for(j in 1:ncol(ranges)){
    if (is.na(ranges[i,j]) == F){
      trynum = ranges[i,j]
      ranges[i,j] = paste(rel_ranges.df$Lower_Limit[which(rel_ranges.df$Range_Number==trynum)], 
                          "-", 
                          rel_ranges.df$Upper_Limit[which(rel_ranges.df$Range_Number==trynum)], 
                          "maf",
                          sep = " ")
    }
  }
}

bal_vars = pdv[,16:20]
bal_vars[bal_vars==0] = "Off"
bal_vars[bal_vars==1] = "On"
bal_vars=cbind(rep(NA,nrow(bal_vars)), bal_vars)
names(bal_vars) = c("Equalization",names(bal_vars[2:ncol(bal_vars)]))

Releases = pdv[,21:25]
Releases = Releases/1000000 #change to MAF
Releases=cbind(rep(NA,nrow(Releases)), Releases)
names(Releases) = c("Equalization",names(Releases[2:ncol(Releases)]))

EqInc = as.data.frame(pdv[,26])
names(EqInc) = "EqInc"

#create labels
tier_labs = matrix(NA, nrow=nrow(p.elevation), ncol=6) #hardcoded for now
tier_labs = as.data.frame(tier_labs)


#special case for equalization tier
for(k in 1:nrow(tier_labs)){
  if(p.e_delta[k,1]>0){
    tier_labs[k,1] = paste("Eq Line increases by", EqInc[k,1], "ft/yr", sep = " ")
  }
  
}

for(i in 1:nrow(tier_labs)){
  for(j in 2:ncol(tier_labs)){
    if(is.na(bal_vars[i,j])==F && bal_vars[i,j]=="On"){
      tier_labs[i,j] = paste("Release", Releases[i,j], "maf;", "Balance", ranges[i,j], "if Mead <", mead_ref[i,j], "ft",  sep = " ")
    }
    else if(is.na(bal_vars[i,j])==F && bal_vars[i,j]=="Off"){
      tier_labs[i,j] = paste("Release", Releases[i,j], "maf;", "No Balancing", sep = " ")
    }
    else{#(is.na(bal_vars[i,j])){
      tier_labs[i,j]=NA
    }
  }
}
names(tier_labs) = c("Equalization Label", "Tier1 Label", "Tier2 Label", "Tier3 Label", "Tier4 Label", "Tier5 Label" )


#add dead pool and policy number to every df
p.elevation$policy=as.character(1:nrow(pdv))

p.e_delta$dead.pool=3370
p.e_delta$policy=as.character(1:nrow(pdv))

tier_labs$dead.pool=NA
tier_labs$policy=as.character(1:nrow(pdv))

mead_ref$dead.pool=3370
mead_ref$policy=as.character(1:nrow(pdv))

ranges$dead.pool="3370"
ranges$policy=as.character(1:nrow(pdv))

bal_vars$dead.pool="3370"
bal_vars$policy=as.character(1:nrow(pdv))

Releases$dead.pool=3370
Releases$policy=as.character(1:nrow(pdv))


EqInc$policy=as.character(1:nrow(pdv))


#pivot longer

p.elevation=pivot_longer(p.elevation, cols=1:ncol(p.elevation)-1, names_to = 'Tier')
p.e_delta=pivot_longer(p.e_delta, cols=1:ncol(p.e_delta)-1, names_to = 'Tier')
tier_labs=pivot_longer(tier_labs, cols=1:ncol(tier_labs)-1, names_to = 'Tier')
mead_ref=pivot_longer(mead_ref, cols=1:ncol(mead_ref)-1, names_to = 'Tier')
ranges=pivot_longer(ranges, cols=1:ncol(ranges)-1, names_to = 'Tier')
bal_vars=pivot_longer(bal_vars, cols=1:ncol(bal_vars)-1, names_to = 'Tier')
Releases=pivot_longer(Releases, cols=1:ncol(Releases)-1, names_to = 'Tier')

p.df=data.frame(p.elevation, delta=p.e_delta$value, t_lab= tier_labs$value)
p.df$Tier=rep(c('a', 'b', 'c', 'd', 'e', 'f', 'g'), length(unique(df$policy)))
p.df$TierName=rep(c('Equalization', 'Tier1', 'Tier2', 'Tier3', 'Tier4', 'Tier5', 'Dead Pool'), length(unique(p.df$policy)))

#change to number and add zeros for image ordering in tableau
p.df$policy = as.numeric(p.df$policy)
p.df$policy = sprintf("%04d", p.df$policy)

#########################

stacked_bar_plot_data=list(long_data=p.df)#, wide_data=policy_SOM_labs)
saveRDS(stacked_bar_plot_data, file='data for stacked powell bar plot.rds')

write.csv(p.df, "p.df.csv")


##################### plotting ##########################

for (i in 1:nrow(Archive.df)){
policy_id = sprintf("%04d", i)

powell_pol = ggplot(subset(p.df, policy %in% c(policy_id)), aes(fill=Tier, y=delta, x=policy, label=t_lab))+
  geom_bar(position="stack", stat="identity", color="black", show.legend = FALSE)+
  scale_fill_manual(values=c(
    "#1BBC9B", 
    "#67CC8E",
    "#96ED89",
    "#45BF55",
    "#79BD8F",
    "#289976",
    "#26A69A"))+
  geom_text(position = position_stack(vjust = .5), size=2.5)+
  theme_minimal()+
  ggtitle("Lake Powell")+
  theme(plot.title = element_text(hjust = .5))+
  ylab("PE")+
  scale_y_continuous(breaks = seq(3385, 3700, by = 20))+
  coord_cartesian(ylim=c(3385.5,3700))


mead_col = as.character(df$v_col[which(df$policy==policy_id)])

mead_pol = ggplot(data=subset(df, policy %in% c(policy_id)), aes(fill=Tier, y=delta, x=policy, label=v_lab))+
  geom_bar(position="stack", stat="identity",  color="black",  show.legend = FALSE)+
  scale_fill_manual(values=mead_col)+
  geom_text(position = position_stack(vjust = .5), size=2.5)+
  theme_minimal()+
  ggtitle("Lake Mead")+
  theme(plot.title = element_text(hjust = .5))+
  ylab('PE')+
  scale_y_continuous(breaks = seq(910, 1220, by = 20))+
  coord_cartesian(ylim=c(910,1220))

powell_pol + mead_pol

both.fig = powell_pol + mead_pol

ggsave(paste("ramping images/policy.", policy_id,".png", sep = ""), 
       plot = both.fig, 
       device = "png",
       width = 1650, height = 1420, units = "px",
       dpi = 200)

}



