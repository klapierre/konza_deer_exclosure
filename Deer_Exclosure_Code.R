####### Konza Deer Exclosure Project ######

#### Set all general necessities ####

#Set working directory - Bloodworth PC
setwd("/Users/bloodworthk/Dropbox (Smithsonian)/Konza Prairie/herbivore_removal_plots/deer exclosure resampling/Deer_Exclosure_Resampling_Data")

#Set working directory- Bloodworth Mac
setwd("/Users/kathrynbloodworth/Dropbox (Smithsonian)/Projects/Konza Prairie/herbivore_removal_plots/deer exclosure resampling/Deer_Exclosure_Resampling_Data")

#Load and install libraries
library(MASS)
#install.packages("vegan")
library(vegan)
library(grid)
#install.packages("devtools")
library(devtools)
#install.packages("codyn")
library(codyn)
library(lme4)
#install.packages("nlme")
library(nlme)
#install.packages("lmerTest")
library(lmerTest)
#install.packages("tidyverse")
library(tidyverse)


#Set ggplot2 theme to black and white
theme_set(theme_bw())
#Update ggplot2 theme - make box around the x-axis title size 30, vertically justify x-axis title to 0.35, Place a margin of 15 around the x-axis title.  Make the x-axis title size 30. For y-axis title, make the box size 30, put the writing at a 90 degree angle, and vertically justify the title to 0.5.  Add a margin of 15 and make the y-axis text size 25. Make the plot title size 30 and vertically justify it to 2.  Do not add any grid lines.  Do not add a legend title, and make the legend size 20
theme_update(axis.title.x=element_text(size=40, vjust=-0.35, margin=margin(t=15)),
             axis.text.x=element_text(size=40), axis.title.y=element_text(size=40, angle=90, vjust=0.5,
                                                                          margin=margin(r=15)), axis.text.y=element_text(size=40), plot.title =
               element_text(size=40, vjust=2), panel.grid.major=element_blank(), 
             panel.grid.minor=element_blank(),
             legend.text=element_text(size=30))

#### Read in all data and join data frames ####

#Read in Deer_Removal_Resampling_2017.csv from working directory
Deer_Exclosure_Resampling_Data<-read.csv("3_Deer_Removal_Resampling_2017.csv")%>%
  #Rename watershed 4D to 4B due to data entry error
  mutate(Watershed=ifelse(watershed=="4D","4B", ifelse(watershed=="K1B","K1B", ifelse(watershed=="K4A","K4A", watershed))))%>%
  #Delete the column "watershed"
  select(-watershed)


#Read in konza_spplist.csv from working directory
Konza_Spplist<-read.csv("konza_spplist.csv")%>%
  #Change column name to match across data frames - change sppnum to spnum
  mutate(sppnum=spnum)%>%
  #Delete the column "spnum"
  select(-spnum)


#Make a new data frame called "Deer_Exclosure_Spp_Name and put the data from "Deer_Exclosure_Resampling_Data" in it
Deer_Exclosure_Spp_Name<-Deer_Exclosure_Resampling_Data%>%
  #Join "Konza_Spplist" to the data frame.  Function will add the columns from Konza_Spplist that do not exist in Deer_Exclosure_Spp_Name 
  left_join(Konza_Spplist)%>%
  #In the column labeled taxa, paste the genus and species names and separate them using a space
  mutate(taxa=paste(genus, spp, sep=" "))%>%
  #In the column labled taxa, if the sppnum equals any of the following numbers, make the taxa the species in quotes.  Otherwise, fill the column taxa with what already exists in the column taxa. 
  mutate(taxa=ifelse(sppnum==786,"euphorbia spp", ifelse(sppnum==900,"elymus elymoides", ifelse(sppnum==999, "callirhoe involucrata", ifelse(sppnum==992, "ceanothus herbaceus", ifelse(sppnum==990, "cornus drummondii", ifelse(sppnum==165, "cyperus spp", ifelse(sppnum==900, "elymus elymoides", ifelse(sppnum==996, "elymus elymoides",ifelse(sppnum==991, "eupatorium altissimum", ifelse(sppnum==86, "kuhnia eupatorioides", ifelse(sppnum==995,"ratibida columnifera", ifelse(sppnum==997,"rhus glabra", ifelse(sppnum==980,"salvia azurea",ifelse(sppnum==993,"verbena stricta", ifelse(sppnum==3,"schizachyrium scoparium", ifelse(sppnum==998,"kuhnia eupatorioides",taxa)))))))))))))))))

#Make a new data frame called "Deer Density Data" and put the data from "Density_Ests_2009to2018_KonzaRegion.xlsx"
Deer_Density_Data<-read.csv("Density_Ests_2009to2018_KonzaRegion.csv")

#### Figure 3 30 x 30 m Deer Exclosure Fence Speices Richness ####

#Make a new data table and place the data from "Deer_Exclosure_Spp_Name" in it
Extra_Species_Identity<-Deer_Exclosure_Spp_Name%>%
  mutate(Fire_Regime=ifelse(Watershed=="K1B","Annual", "Four_Year"))%>%
  #Keep only the columns labeled "Watershed", "exclosure", and "taxa"
  select(exclosure,taxa,functional_group,Fire_Regime,Watershed)%>%
  #Keep only the unique data
  unique()%>%
  #Make a new coulmn named "Species_Presence" and place a 1 in every cell so that a wide table can be made later
  mutate(Species_Presence=1)

#Make a new data frame from "Extra_Species_Identity" to generate richness values for each research area
Extra_Species_Richness<-Extra_Species_Identity%>%  
  #group data frame by Watershed and exclosure
  group_by(Fire_Regime,exclosure, Watershed)%>%
  #Make a new column named "Richness" and add the unique number of rows in the column "taxa" according to the groupings
  summarise(Richness=length(taxa))%>%
  #stop grouping by watershed and exclosure
  ungroup()%>%
  #remove column watershed
  select(-Watershed)%>%
  #make a new column for treatment
  mutate(Treatment=paste(Fire_Regime,exclosure, sep = "."))%>%
  #group by treatment, fire_regime, and exclosure
  group_by(Treatment,Fire_Regime,exclosure)%>%
  #take the mean of the unique groupings giving the average richness across burn regimes and treatments
  summarize(Richness_Average=mean(Richness))
  

#T-test comparing research area level species richness across exclosure treatments, using burn regimes as replicates
t.test(Extra_Species_Richness$Richness_Average~Extra_Species_Richness$exclosure)


#Make a new data table using data from "Extra_Species_Richness" to generate Figure 3.
Extra_Species_Richness_Summary<-Extra_Species_Richness%>%
  #Group data by the columns "Watershed" and "exclosure"
  group_by(exclosure)%>%
  #In this data frame, summarize the data.  Make a new column named "Richness_Std" and calculate the standard deviation from the column "Richness".  Also calculate the mean and length from the column "Richness" and place them into their own columns.
  summarize(Richness_Std=sd(Richness_Average),Richness_Mean=mean(Richness_Average),Richness_n=length(Richness_Average))%>%
  #Make a new column called "Richness_St_Error" and divide "Richness_Std" by the square root of "Richness_n"
  mutate(Richness_St_Error=Richness_Std/sqrt(Richness_n))

## Figure 3 - Make a bar graph using ggplot2 ##
#png(filename="Fig 3.png", width =6 ,height =6,units = 'in' ,res =300 )
#Use data from "Extra_Species_Diversity_Summary".  Change the aesthetics x is equal to the data from "exlosure", and y is equal to the "Richness_Mean"
ggplot(Extra_Species_Richness_Summary,aes(x=exclosure,y=Richness_Mean, fill=exclosure))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.   -BW
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values=c("lightcyan3","cadetblue4"))+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Richness_Mean-Richness_St_Error,ymax=Richness_Mean+Richness_St_Error),position=position_dodge(0.9),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Deer Exclosure Treatment")+
  #Label the y-axis "Species Richness"
  ylab(bquote("Species Richness (per 900" *~m^2*")"))+
  scale_x_discrete(labels=c("inside"="Deer Exclusion","outside"="Control"))+
  #Make the y-axis extend to 50
  expand_limits(y=50)+
  theme(legend.position = "none")
#dev.off()
#Save at the graph at 1400x1500

#### Figure 2 Sublot Level Species Richness and Evenness####


###Calculate Relative Cover###

#Calculate Total Cover and put it into a new data tabel named "Total_Cover"
Total_Cover<-Deer_Exclosure_Spp_Name%>%
  #filter out all data where plot = 999 (this will filter out extra research area level species data)
  filter(plot!=999)%>%
  mutate(Fire_Regime=ifelse(Watershed=="K1B","Annual Burn Regime", "Four year Burn Regime"))%>%
  #group by "Watershed" and "plot"
  group_by(Watershed,plot,Fire_Regime)%>%
  #In a column named "Total Cover" summarize the sum of "Cover" per grouping
  summarize(Total_Cover=sum(cover))

#Calculate Relative Cover - make a new file named Relative_Cover using the data from Deer_Exclosure_Spp_Name
Relative_Cover<-Deer_Exclosure_Spp_Name%>%
  #Filter out all data where plot = 999 (this will filter out extra research area level species data)
  filter(plot!=999)%>%
  mutate(Fire_Regime=ifelse(Watershed=="K1B","Annual Burn Regime", "Four year Burn Regime"))%>%
  #Make a new column named "Treatment" and paste "Watershed" and "exclosure" together, separating them by a period.
  mutate(Treatment=paste(Watershed,exclosure,sep="_"))%>%
  mutate(Plot_number=paste(Treatment,plot,sep = "_"))%>%
  #Add Total_Cover data into the Relative_Cover data sheet
  left_join(Total_Cover)%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=cover/Total_Cover)%>%
  #delete column "cover"
  select(-cover)
  
#Make a new data frame to caluclate the average relative cover for each exclosure and burn regime, left join Relative_Cover
Avg_Relative_Cover<-Relative_Cover%>%
  group_by(exclosure,plot,taxa,Fire_Regime)%>%
  summarise(Avg_Relative_Cover=mean(Relative_Cover))

###Calculate Richness For Graph###

#Make a new data frame called Species Richness using data from Relative Cover
Species_Richness<-Relative_Cover%>%
  #do not include zeros
  filter(Relative_Cover!=0)%>%
  #group data frame by Watershed and plot
  group_by(Watershed,plot,Fire_Regime)%>%
  #In a column named richness, give the number of rows in Relative_Cover
  summarize(Richness=length(Relative_Cover))%>%
  #stop grouping by watershed and plot
  ungroup()%>%
  group_by(Fire_Regime,plot)%>%
  summarize(Avg_Richness=mean(Richness))

#Make a new data frame called Wide_Relative_Cover using data from Relative_Cover - Watershed
Wide_Relative_Cover_1<-Relative_Cover%>%
  #Keep only the columns taxa, Relative_Cover, Watershed, plot, and exclosure
  select(taxa,Relative_Cover,Watershed,plot,exclosure)%>%
  mutate(Fire_Regime=ifelse(Watershed=="K1B","Annual","Four Year"))%>%
  #Make a wide table using column taxa as overarching columns, fill with values from Relative_Cover column, if there is no value for one cell, insert a zero
  spread(key=taxa,value=Relative_Cover, fill=0)

#Make a new data frame called Wide_Relative_Cover using data from Relative_Cover - Fire Regime
Wide_Relative_Cover<-Relative_Cover%>%
  group_by(exclosure,taxa,plot,Fire_Regime)%>%
  summarize(Avg_Relative_Cover=mean(Relative_Cover))%>%
  ungroup()%>%
  mutate(Fire_Regime=paste(ifelse(Fire_Regime=="Annual Burn Regime","Annual","Four year")))%>%
  mutate(Treatment=paste(Fire_Regime,plot,sep="."))%>%
  #Keep only the columns taxa, Relative_Cover, Watershed, plot, and exclosure
  select(taxa,Avg_Relative_Cover,plot,exclosure,Fire_Regime,Treatment)%>%
  #Make a wide table using column taxa as overarching columns, fill with values from Relative_Cover column, if there is no value for one cell, insert a zero
  spread(key=taxa,value=Avg_Relative_Cover, fill=0)


## Evar - Smith and Wilson's Evenness Index ##

###For Statistical Analysis###
#Make a new data frame called Community_Metrics and run Evar on the Relative_Cover data
Community_Metrics_LMER<-community_structure(Relative_Cover,replicate.var = "Plot_number",abundance.var = "Relative_Cover")%>%
  #Separate out Plot_number into three separate columns
  separate(Plot_number,c("Watershed","exclosure","plot"), sep = "_")%>%
  mutate(Fire_Regime=ifelse(Watershed%in%c("4B","K4A"),4,1))%>%
  mutate(Watershed_Numerical=ifelse(Watershed=="4B",100, ifelse(Watershed=="K4A",200,300)))%>%
  #Make categorical
  mutate(Plot_Numerical=as.numeric(plot))%>%
  mutate(Watershed.Plot=Watershed_Numerical+Plot_Numerical)



#Watershed, exlosure, subplot, and fire regime must all be stored as factors in order to be considered as that - when you store them as numbers R assumes that they are in a specific order (i.e. 2 is inbetween 1 and 3), when you store them as characters R does not know what to do with them
Community_Metrics_LMER$Watershed_Fact=as.factor(Community_Metrics_LMER$Watershed)
Community_Metrics_LMER$exclosure_Fact=as.factor(Community_Metrics_LMER$exclosure)
Community_Metrics_LMER$subplot_Fact=as.factor(Community_Metrics_LMER$plot)
Community_Metrics_LMER$Fire_Regime_Fact=as.factor(Community_Metrics_LMER$Fire_Regime)
Community_Metrics_LMER$Watershed.Plot.Fact=as.factor(Community_Metrics_LMER$Watershed.Plot)
#From the data fram Community_Metrics form a new column named Watershed_Exclosure and insert in the interaction of the Watershed_Fact and exclosure_Fact so that each exclosure within each watershed has a unique identifier
Community_Metrics_LMER$Watershed_Exclosure=interaction(Community_Metrics_LMER$Watershed_Fact:Community_Metrics_LMER$exclosure_Fact)
Community_Metrics_LMER$Watershed.Plot.Fact_=as.factor(Community_Metrics_LMER$Watershed.Plot)



#LME4 - Sublot Level Species Richness and Evenness

#Give the summary of a new data frame called Mixed_Model_Richness and preform the mixed model function"lmer' using richness, exclosure, and fire regime as fixed effects.  Then use exlosure nested within watershed as a random effect.  Then run an anova 
#Richness
summary (Mixed_Model_Richness <- lmer(richness ~ exclosure_Fact*Fire_Regime_Fact + (1 | Watershed_Fact/exclosure_Fact), data = Community_Metrics_LMER))
anova(Mixed_Model_Richness)

#Evenness
summary (Mixed_Model_Evenness <- lmer(Evar ~ exclosure_Fact*Fire_Regime_Fact + (1 | Watershed_Fact/exclosure_Fact), data = Community_Metrics_LMER))
anova(Mixed_Model_Evenness)

#Make a new dataframe to generate Figure 2 called Diversity_Summary using data from Diversity
Diversity_Summary<-Community_Metrics_LMER%>%
  #group by watershed and exclosure
  group_by(Fire_Regime,exclosure,plot)%>%
  summarise(Avg_Richness=mean(richness),Avg_Evar=mean(Evar))%>%
  ungroup()%>%
  group_by(Fire_Regime,exclosure)%>%
  #In new columns, calculate the standard deviation, mean, and length of Species Richness and Evar
  summarize(Richness_Std=sd(Avg_Richness),Richness_Mean=mean(Avg_Richness), Richness_n=length(Avg_Richness), Evar_Std=sd(Avg_Evar), Evar_Mean=mean(Avg_Evar), Evar_n=length(Avg_Evar))%>%
  #Make a new column and calculate the Standard Error for Species Richness and EQ
  mutate(Richness_St_Error=Richness_Std/sqrt(Richness_n), Evar_St_Error=Evar_Std/sqrt(Evar_n))

##Figure 2 - Make a bar graph of species richness and evenness##

Fire_Regime_Labels<-c("Annual","Four year")

#Figure 2a - Make a dataframe called Species_Richness_Plot using gglpot2.  Use data from Diversity_Summary, making the x-axis Watershed, and y-axis Richness_Mean, the bars should be based on exclosures
Species_Richness_Plot<-ggplot(Diversity_Summary,aes(x=as.factor(Fire_Regime),y=Richness_Mean,fill=exclosure))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and outline the bars with the color black.
  geom_bar(stat="identity", position=position_dodge(),color="black")+
  #make error bars using the Standard error from the mean and place them at 0.9 with a width of 0.2
  geom_errorbar(aes(ymin=Richness_Mean-Richness_St_Error,ymax=Richness_Mean+Richness_St_Error),position=position_dodge(0.9),width=0.2)+
  #Label the x-axis "Watershed"
  xlab("Burn Regime")+
  #Label the y-axis "Species Richness"
  ylab(bquote("Species Richness (per " *~m^2*")"))+
  #Fill the bar graphs with grey and white according and label them "Inside Fence" and "Outside Fence"
  scale_fill_manual(values=c("lightcyan3","cadetblue4"), labels=c("Deer Exclusion","Control"))+
  #Make the y-axis expand to 20
  expand_limits(y=20)+
  scale_x_discrete(labels=Fire_Regime_Labels)+
  #Place the legend at 0.8,0.94 and space it out by 3 lines
  theme(legend.position=c(0.75,0.94), legend.key.size = unit(2.0, 'lines'),legend.title = element_blank())+
  #Add "a." to the graph in size 10 at position 0.6,20
  annotate("text",x=0.55,y=20,label="a.",size=20)+
  #Add "A" to the graph in size 6 at position 1,17
  annotate("text",x=1,y=14,label="A",size=15)+
  #Add "B" to the graph in size 6 at position 2,13.5
  annotate("text",x=2,y=17,label="B",size=15)+
  theme(plot.margin=margin(10,10,10,10,"pt"))

#Figure 2b - Make a dataframe called EQ_Plot using ggplot2.  Use data from Diversity summary, with the x-axis being Watershed, and the y-axis being EQ_Mean, the bars should be based on exclosures
Evar_Plot<-ggplot(Diversity_Summary,aes(x=as.factor(Fire_Regime),y=Evar_Mean,fill=exclosure))+
  ##Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and outline the bars with the color black.
  geom_bar(stat="identity", position=position_dodge(),color="black")+
  #Make error bars using the standard error from the mean.  Position them at 0.9 with a width of 0.2  
  geom_errorbar(aes(ymin=Evar_Mean-Evar_St_Error,ymax=Evar_Mean+Evar_St_Error),position=position_dodge(0.9),width=0.2)+
  #Label the x-axis "Watershed"
  xlab("Burn Regime")+
  #Label the y-axis "Evenness Quotient"
  ylab(bquote("Evenness Index (per " *~m^2*")"))+
  #Fill the bars with grey and white and label them "Inside Fence" and "Outside Fence"
  scale_fill_manual(values=c("lightcyan3","cadetblue4"), labels=c("Deer Removal","Control"))+
  #do not include a legend
  theme(legend.position="none")+
  scale_x_discrete(labels=Fire_Regime_Labels)+
  #Expand the y-axis to 1.0
  expand_limits(y=0.5)+
  #add text "b." at 0.6,1.0 in size 10
  annotate("text",x=.55,y=0.5,label="b.",size=20)+
  theme(plot.margin=margin(10,10,10,10,"pt"))

#Figure 2 - Make a figure that has one row and two columns for two graphs
pushViewport(viewport(layout=grid.layout(1,2)))
#print out the viewport plot where the Species_Richness_plot is at position 1,1
print(Species_Richness_Plot,vp=viewport(layout.pos.row=1, layout.pos.col =1))
#print out the viewport plot where the Species_Richness_plot is at position 1,2
print(Evar_Plot,vp=viewport(layout.pos.row=1, layout.pos.col =2))
#Save at 2000 x 1000  



#### Figure 4, appendix S7 - Functional Group ####

Functional_Group_Cover<-Relative_Cover%>%
  group_by(Watershed,exclosure,plot,functional_group)%>%
  summarise(FG_Relative_Cover=sum(Relative_Cover))%>%
  mutate(Fire_Regime=ifelse(Watershed=="K1B","annual","four"))

Functional_Group_Cover$Fire_Regime_Fact=as.factor(Functional_Group_Cover$Fire_Regime)
Functional_Group_Cover$Watershed_Fact=as.factor(Functional_Group_Cover$Watershed)
Functional_Group_Cover$exclosure_Fact=as.factor(Functional_Group_Cover$exclosure)

#Appendix S7

summary (Graminoid_Mixed_Model_Richness <- lmer(FG_Relative_Cover ~ exclosure_Fact*Fire_Regime_Fact + (1 | Watershed_Fact/exclosure_Fact), data = subset(Functional_Group_Cover,functional_group=="graminoid")))
anova(Graminoid_Mixed_Model_Richness)

summary (Forb_Mixed_Model_Richness <- lmer(FG_Relative_Cover ~ exclosure_Fact*Fire_Regime_Fact + (1 | Watershed_Fact/exclosure_Fact), data = subset(Functional_Group_Cover,functional_group=="forb")))
anova(Forb_Mixed_Model_Richness)

#Make a new dataframe to generate Figure 2 called Diversity_Summary using data from Diversity
FG_Cover_Summary<-Functional_Group_Cover%>%
  group_by(Fire_Regime,exclosure,functional_group,plot)%>%
  summarise(Avg_Relative_Cover=mean(FG_Relative_Cover))%>%
  ungroup()%>%
  group_by(Fire_Regime,exclosure,functional_group)%>%
  #In new columns, calculate the standard deviation, mean, and length of Species Richness and Evar
  summarize(Richness_Std=sd(Avg_Relative_Cover),Richness_Mean=mean(Avg_Relative_Cover), Richness_n=length(Avg_Relative_Cover))%>%
  #Make a new column and calculate the Standard Error for Species Richness and EQ
  mutate(Richness_St_Error=Richness_Std/sqrt(Richness_n))


#Figure 4
## Make a bar graph of Functional group species richness and evenness## --find a way to add back in the one woody species for richness

#Figure 4a - Make a dataframe called Species_Richness_Plot using gglpot2.  Use data from Diversity_Summary, making the x-axis Watershed, and y-axis Richness_Mean, the bars should be based on exclosures
FG_Graminoid_Plot<-ggplot(subset(FG_Cover_Summary,functional_group=="graminoid"),aes(x=Fire_Regime,y=Richness_Mean,fill=exclosure))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and outline the bars with the color black.
  geom_bar(stat="identity", position=position_dodge(),color="black")+
  #make error bars using the Standard error from the mean and place them at 0.9 with a width of 0.2
  #Label the x-axis "Watershed"
  xlab("Burn Regime")+
  #Label the y-axis "Species Richness"
  ylab(bquote("Relative Cover (per " *~m^2*")"))+
  #Fill the bar graphs with grey and white according and label them "Inside Fence" and "Outside Fence"
  geom_errorbar(aes(ymin=Richness_Mean-Richness_St_Error,ymax=Richness_Mean+Richness_St_Error),position=position_dodge(0.9),width=0.2)+
  scale_fill_manual(values=c("lightcyan3","cadetblue4"), labels=c("Deer Exclusion","Control"))+
  #Make the y-axis expand to 20
  expand_limits(y=1)+
  scale_x_discrete(labels=Fire_Regime_Labels)+
  #Place the legend at 0.8,0.94 and space it out by 3 lines
  theme(legend.position=c(0.75,0.94), legend.key.size = unit(2.0, 'lines'),legend.title = element_blank())+
  #Add "a." to the graph in size 10 at position 0.6,20
  annotate("text",x=0.93,y=1,label="a. Graminoids",size=15)+
  theme(plot.margin=margin(10,10,10,10,"pt"))

#Figure 4b - Make a dataframe called Species_Richness_Plot using gglpot2.  Use data from Diversity_Summary, making the x-axis Watershed, and y-axis Richness_Mean, the bars should be based on exclosures
FG_Forb_Plot<-ggplot(subset(FG_Cover_Summary,functional_group=="forb"),aes(x=Fire_Regime,y=Richness_Mean,fill=exclosure))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and outline the bars with the color black.
  geom_bar(stat="identity", position=position_dodge(),color="black")+
  #make error bars using the Standard error from the mean and place them at 0.9 with a width of 0.2
  #Label the x-axis "Watershed"
  xlab("Burn Regime")+
  #Label the y-axis "Species Richness"
  ylab(bquote("Relative Cover (per " *~m^2*")"))+
  #Fill the bar graphs with grey and white according and label them "Inside Fence" and "Outside Fence"
  scale_fill_manual(values=c("lightcyan3","cadetblue4"), labels=c("Deer Exclusion","Control"))+
  geom_errorbar(aes(ymin=Richness_Mean-Richness_St_Error,ymax=Richness_Mean+Richness_St_Error),position=position_dodge(0.9),width=0.2)+
  #Make the y-axis expand to 20
  expand_limits(y=1.00)+
  scale_x_discrete(labels=Fire_Regime_Labels)+
  #Place the legend at 0.8,0.94 and space it out by 3 lines
  theme(legend.position="none")+
  #Add "a." to the graph in size 10 at position 0.6,20
  annotate("text",x=1.36,y=1.0,label="b. Non-Leguminous Forbs",size=15)+
  theme(plot.margin=margin(10,10,10,10,"pt"))
#Figure 2b - Make a dataframe called EQ_Plot using ggplot2.  Use data from Diversity summary, with the x-axis being Watershed, and the y-axis being EQ_Mean, the bars should be based on exclosures

#Figure 4 - Make a figure that has one row and two columns for two graphs
pushViewport(viewport(layout=grid.layout(1,2)))
#print out the viewport plot where the Species_Richness_plot is at position 1,1
print(FG_Graminoid_Plot,vp=viewport(layout.pos.row=1, layout.pos.col =1))
#print out the viewport plot where the Species_Richness_plot is at position 1,2
print(FG_Forb_Plot,vp=viewport(layout.pos.row=1, layout.pos.col =2))
#Save at 2000 x 1000 




#### Figure 5 - Rank Abundance Curves ####


#Make a new data table called RAC_Mean using data from Relative_Cover - the mean is being taken across all plots for ease of comparison
RAC_Mean<-Avg_Relative_Cover%>%
  #Group by Treatment and taxa
  #mutate(Fire_Regime=ifelse(Watershed=="K1B","Annual","Four Year"))%>%
  group_by(exclosure,Fire_Regime,taxa)%>%
  #In a column named "Mean" give the mean of the "Relative_Cover" according to the grouping
  summarize(Mean=mean(Avg_Relative_Cover))%>%
  #Ungroup the data
  ungroup()%>%
  #Arrange the columns by "Treatment" and the reverse "Mean" (this will arrange the data table to be categorized by treatment and then highest to lowest mean within each treatmnet
  arrange(exclosure,Fire_Regime,-Mean)%>%
  #Group by treatment
  group_by(exclosure,Fire_Regime)%>%
  mutate(Treatment=paste(Fire_Regime,exclosure,sep = "."))%>%
  #Make a new column called "Ranks" and give the rank from low to high of the treatments according to the grouping
  mutate(Ranks=seq_along(Treatment))%>%
  #Ungroup data
  ungroup()%>%
  #Make a new column called "Treatment_Type" where data from "exclosure" is taken and rewritten as "Inside Fence" and "Outside Fence"
  mutate(Treatment_Type=ifelse(exclosure=="inside","Deer Removal", "Control"))%>%
  #Make a new column called Symbol so that if the taxa is equal to one stated below, the symbol number will be entered (15,5,2,etc.), for all else enter 16 (rare species) - this will later allow for symbols to be assigned in the figure 3 based on this column
  mutate(Symbol=ifelse(taxa=="andropogon gerardii",0, ifelse(taxa=="carex heliophila",2, ifelse(taxa=="schizachyrium scoparium",17, ifelse(taxa=="sorghastrum nutans",5, ifelse(taxa=="sporobolus asper",18,ifelse(taxa=="aster ericoides",15,ifelse(taxa=="sporobolus heterolepis",1,16))))))))

RAC_1Inside <- subset(RAC_Mean,Treatment=="Annual Burn Regime.inside")
#Made plot using data from RAC_Mean with x being "Ranks" and y being "Mean"
RAC_1Inside_Graph<-ggplot(RAC_1Inside,aes(x=Ranks,y=Mean))+
  #Make a line graph
  geom_line()+
  #when making the points in the line graph, change the symbols to be equal to the "Symbol" column and make them a size 3
  geom_point(shape=RAC_1Inside$Symbol, size=4)+
  #Label the y-axis "Relative Abundance (%)"
  ylab("Relative Abundance (%)")+
  #Change the theme so that the boarders and backgrounds are white and the x- and y-axis text size is 24
  theme(strip.background = element_rect(color="white",fill="white"),strip.text.x = element_text(size=28), strip.text.y = element_text(size=28),axis.title.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.title.y = element_text(size=30))+
  expand_limits(y=0.5)+
  labs(title="Deer Exclusion")

RAC_1Outside <- subset(RAC_Mean,Treatment=="Annual Burn Regime.outside")
#Made plot using data from RAC_Mean with x being "Ranks" and y being "Mean"
RAC_1Outside_Graph<-ggplot(RAC_1Outside,aes(x=Ranks,y=Mean))+
  #Make a line graph
  geom_line()+
  #when making the points in the line graph, change the symbols to be equal to the "Symbol" column and make them a size 3
  geom_point(shape=RAC_1Outside$Symbol, size=4)+
  #Change the theme so that the boarders and backgrounds are white and the x- and y-axis text size is 24
  theme(strip.background = element_rect(color="white",fill="white"),strip.text.x = element_text(size=28), strip.text.y = element_text(size=28),axis.title.x = element_blank(), axis.title.y=element_blank(),axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),axis.text.y = element_blank(),axis.text.x = element_blank())+
  expand_limits(y=0.5)+
  labs(title="Control")

RAC_4Inside <- subset(RAC_Mean,Treatment=="Four year Burn Regime.inside")
#Made plot using data from RAC_Mean with x being "Ranks" and y being "Mean"
RAC_4Inside_Graph<-ggplot(RAC_4Inside,aes(x=Ranks,y=Mean))+
  #Make a line graph
  geom_line()+
  #when making the points in the line graph, change the symbols to be equal to the "Symbol" column and make them a size 3
  geom_point(shape=RAC_4Inside$Symbol, size=4)+
  #Label the x-axis "Species Rank"
  #Label the y-axis "Relative Abundance (%)"
  ylab("Relative Abundance (%)")+
  xlab("Species Rank")+
  #Change the theme so that the boarders and backgrounds are white and the x- and y-axis text size is 24
  theme(strip.background = element_rect(color="white",fill="white"),strip.text.x = element_text(size=28), strip.text.y = element_text(size=28),axis.title.y = element_text(size=30),axis.title.x = element_text(size=30))+
  expand_limits(y=0.5)+
  theme(plot.margin=margin(10,10,10,10,"pt"))

RAC_4Outside <- subset(RAC_Mean,Treatment=="Four year Burn Regime.outside")
#Made plot using data from RAC_Mean with x being "Ranks" and y being "Mean"
RAC_4Outside_Graph<-ggplot(RAC_4Outside,aes(x=Ranks,y=Mean))+
  #Make a line graph
  geom_line()+
  #when making the points in the line graph, change the symbols to be equal to the "Symbol" column and make them a size 3
  geom_point(shape=RAC_4Outside$Symbol, size=4)+
  #Label the x-axis "Species Rank"
  xlab("Species Rank")+
  #Change the theme so that the boarders and backgrounds are white and the x- and y-axis text size is 24
  theme(strip.background = element_rect(color="white",fill="white"),strip.text.x = element_text(size=28), strip.text.y = element_text(size=28),axis.title.y = element_blank(), axis.ticks.y = element_blank(),axis.text.y = element_blank(),axis.title.x = element_text(size=30))+
  expand_limits(y=0.5)+
  theme(plot.margin=margin(10,10,10,10,"pt"))

#Figure 5 - Make a figure that has two row and two columns for four graphs
pushViewport(viewport(layout=grid.layout(2,2)))
#print out the viewport plot where the Species_Richness_plot is at position 1,1
print(RAC_1Inside_Graph,vp=viewport(layout.pos.row=1, layout.pos.col =1))
#print out the viewport plot where the Species_Richness_plot is at position 1,2
print(RAC_1Outside_Graph,vp=viewport(layout.pos.row=1, layout.pos.col =2))
#print out the viewport plot where the Species_Richness_plot is at position 2,1
print(RAC_4Inside_Graph,vp=viewport(layout.pos.row=2, layout.pos.col =1))
#print out the viewport plot where the Species_Richness_plot is at position 2,2
print(RAC_4Outside_Graph,vp=viewport(layout.pos.row=2, layout.pos.col =2))
#Save at 2000 x 1200 



#### Figure 1 - Bray-Curtis NMDS ####


#Update the theme so that legends will have titles (all else is the same as above)
theme_update(axis.title.x=element_text(size=40, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=40),
             axis.title.y=element_text(size=40, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=40),
             plot.title = element_text(size=40, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.text=element_text(size=30))

#Figure 1
#Make new data frame called BC_Data and run an NMDS using data from Wide_Relative_Cover, columns 4 through 49
BC_Data <- metaMDS(Wide_Relative_Cover[,5:50])
#Make a data frame called sites with 1 column and same number of rows that is in Wide_Relative_Cover
sites <- 4:nrow(Wide_Relative_Cover)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-3
BC_Meta_Data <- Wide_Relative_Cover[,1:4]
#make a plot using the dataframe BC_Data and the column "points".  Make "Watershed" a factor - make the different watersheds different colors
plot(BC_Data$points,col=as.factor(BC_Meta_Data$Fire_Regime))
#make elipses using the BC_Data.  Group by "Watershed" and use standard deviation to draw eclipses and display by sites, add labels based on Watershed type.
ordiellipse(BC_Data,groups = as.factor(BC_Meta_Data$Fire_Regime),kind = "sd",display = "sites", label = T)

#Use the vegan ellipse function to make ellipses           
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS = data.frame(MDS1 = BC_Data$points[,1], MDS2 = BC_Data$points[,2],group=BC_Meta_Data$Fire_Regime)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_NMDS_Graph <- cbind(BC_Meta_Data,BC_NMDS)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses<-ordiellipse(BC_Data, BC_Meta_Data$Fire_Regime, display = "sites",
                             kind = "se", conf = 0.95, label = T)
#Make a new empty data frame called BC_Ellipses                
BC_Ellipses <- data.frame()
#Generate ellipses points
for(g in levels(BC_NMDS$group)){
  BC_Ellipses <- rbind(BC_Ellipses, cbind(as.data.frame(with(BC_NMDS[BC_NMDS$group==g,],                                                  veganCovEllipse(BC_Ord_Ellipses[[g]]$cov,BC_Ord_Ellipses[[g]]$center,BC_Ord_Ellipses[[g]]$scale)))
                                          ,group=g))
}
#Plot the data from BC_NMDS_Graph, where x=MDS1 and y=MDS2, make an ellipse based on "group"
ggplot(data = BC_NMDS_Graph, aes(MDS1,MDS2, shape = group))+
  #make a point graph where the points are size 5.  Color them based on exlosure
  geom_point(size=5, aes(color = exclosure))+
  #Make "inside" and "outside" dark grey and back and label them "Inside Fence", "Outside Fence".  Label this legend "Treatment"
  scale_color_manual(breaks=c("inside","outside"),
                     values = c("lightcyan3","cadetblue4"),
                     labels=c("Deer Exclusion", "Control"),
                     name="Treatment")+
  #Use different shapes according to Watershed types
  scale_shape_discrete(name="Burn Regime")+
  #Use the data from BC_Ellipses to make ellipses that are size 1 with a solid line
  geom_path(data = BC_Ellipses, aes(x=NMDS1, y=NMDS2), size=1, linetype=1)+
  #make the text size of the legend titles 28
  theme(legend.title = element_text(size=30),  legend.key.size = unit(2.0, 'lines'),legend.position=c(0.14,0.82))+
  #Add annotations of K1B, 4B, and K4A inside the elipses and bold them
  annotate("text",x=-.155,y=0.17,label="Annual",size=12, fontface="bold")+
  annotate("text",x=0.21,y=-0.18,label="Four Year",size=12, fontface="bold")+
  #Label the x-axis "NMDS1" and the y-axis "NMDS2"
  xlab("NMDS1")+
  ylab("NMDS2")+
  theme(plot.margin=margin(10,10,10,10,"pt"))
#export at 1500x933


##PerMANOVA

#Make a new dataframe with the data from Wide_Relative_Cover all columns after 4
Species_Matrix <- Wide_Relative_Cover_1[,5:ncol(Wide_Relative_Cover_1)]
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix <- Wide_Relative_Cover_1[,1:4]
Environment_Matrix$Watershed_Fact=as.factor(Environment_Matrix$Watershed)
Environment_Matrix$Exclosure_Fact=as.factor(Environment_Matrix$exclosure)
Environment_Matrix$Plot_Fact=as.factor(Environment_Matrix$plot)
Environment_Matrix$Fire_Regime_Fact=as.factor(Environment_Matrix$Fire_Regime)
Environment_Matrix$Watershed_Exclosure=interaction(Environment_Matrix$Watershed_Fact:Environment_Matrix$Exclosure_Fact)
Environment_Matrix$Watershed_Exclosure_Plot=interaction(Environment_Matrix$Watershed_Exclosure:Environment_Matrix$Plot_Fact)



#Make a new dataframe with data from Relative_Cover
Relative_Cover2 <- Relative_Cover%>%
  #Keep only the columns "Watershed", "Exclosure","plot","taxa", and "Relative_Cover"
  select(Watershed,exclosure,plot,taxa,Relative_Cover)%>%
  mutate(Fire_Regime=ifelse(Watershed=="K1B","Annual","Four Year"))

Relative_Cover2$Watershed_Fact=as.factor(Relative_Cover2$Watershed)
Relative_Cover2$Exclosure_Fact=as.factor(Relative_Cover2$exclosure)
Relative_Cover2$Plot_Fact=as.factor(Relative_Cover2$plot)
Relative_Cover2$Fire_Regime_Fact=as.factor(Relative_Cover2$Fire_Regime)
Relative_Cover2$Watershed_Exclosure=interaction(Relative_Cover2$Watershed_Fact:Relative_Cover2$Exclosure_Fact)

#Make a new dataframe with data from Relative_Cover2
Wide_Relative_Cover2 <- Relative_Cover2%>%
  #Make a qide data frame using "Taxa" as the columns and fill with "Relative_Cover", if there is no data, fill cell with zero
  spread(key = taxa, value = Relative_Cover, fill = 0)
#run a perMANOVA comparing across watershed and exclosure, how does the species composition differ.  Permutation = 999 - run this 999 times and tell us what the preportion of times it was dissimilar
#Adding in the 'strata' function does not affect results - i can't figure out if I am doing in incorrectly or if they do not affect the results (seems unlikely though becuase everything is exactly the same)
PerMANOVA2 <- adonis2(formula = Species_Matrix~Fire_Regime_Fact*Exclosure_Fact+Fire_Regime_Fact/Exclosure_Fact, data=Environment_Matrix,permutations = 999, method = "bray", strata=Watershed_Fact)
#give a print out of the PermMANOVA
print(PerMANOVA2)

##PermDisp

#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix <- vegdist(Species_Matrix)
#Run a dissimilarity matrix (PermDisp) comparing watershed
Dispersion_Results_Fire_Regime <- betadisper(BC_Distance_Matrix,Wide_Relative_Cover_1$Fire_Regime)
permutest(Dispersion_Results_Fire_Regime,pairwise = T, permutations = 999)
#Run a dissimilarity matrix (PermDisp) comparing exlosure
Dispersion_Results_Treatment <- betadisper(BC_Distance_Matrix,Wide_Relative_Cover_1$exclosure)
permutest(Dispersion_Results_Treatment,pairwise = T, permutations = 999)

#### Appendix S5 ####
#-Make a new data table called "Species_Table" using data from "Extra_Species_Richness"
Species_Table <- Extra_Species_Identity %>%
  #Make a new column named "Location" change exclosure data to "Outside Fence" and "Inside Fence"
  mutate(Location=ifelse(exclosure=="outside", "Outside Fence", "Inside Fence"))%>%
  #Make a new column named "Treatment" and paste Watershed and Location together, separating them with a dash
  mutate(Treatment=paste(Fire_Regime,Location, sep = "-"))%>%
  #Keep only the columns taxa, Species_Presence and Treatment
  select(taxa,Species_Presence,Treatment)%>%
  #Delete all rows that are not unique
  unique()%>%
  #Make a wide data frame where the treatments are the column headers and taxa are rows and the values are filled with information from "Species_Presence".  If there is no information from "Species_Presence" then fill that cell with a zero
  spread(key = Treatment, value=Species_Presence, fill=0)
#Write a csv file onto the computer
write.csv(Species_Table, file = "Species_Table.csv")

#### Appendix S6- SIMPER ####

#Run a SIMPER test comparing data from the Environment_Matrix, to data from the Species_Matrix grouping by fire regime
SIMPER <- with(Environment_Matrix,simper(Species_Matrix,Fire_Regime))
#Print out a summary of the results
summary(SIMPER)

#### Appendix S8-S9 Functional Group Richness by Treatment Type ####

#Make a new data frame from "Extra_Species_Identity" to generate richness values for each research area
Functional_Group_Extra_Species_Richness<-Extra_Species_Identity%>%  
  select(-Species_Presence)%>%
  unique()%>%
  #group data frame by Watershed and exclosure
  group_by(functional_group,exclosure,Fire_Regime,Watershed)%>%
  #Make a new column named "Richness" and add the unique number of rows in the column "taxa" according to the groupings
  summarise(Richness=length(taxa))%>%
  #stop grouping by watershed and exclosure
  ungroup()%>%
  #group data by exclosure, fire regime and functional group
  group_by(exclosure,Fire_Regime,functional_group)%>%
  #add new column averaging the richness column by groupings
  summarize(Avg_Richness=mean(Richness))


#Make a new data table using data from "Functional_Group_Extra_Species_Richness"
Functional_Group_Extra_Species_Richness_Summary<-Functional_Group_Extra_Species_Richness%>%
  #Group data by the columns "Watershed" and "exclosure"
  group_by(exclosure,functional_group,Fire_Regime)%>%
  #In this data frame, summarize the data.  Make a new column named "Richness_Std" and calculate the standard deviation from the column "Richness".  Also calculate the mean and length from the column "Richness" and place them into their own columns.
  summarize(Richness_Std=sd(Avg_Richness),Richness_Mean=mean(Avg_Richness),Richness_n=length(Avg_Richness))%>%
  #Make a new column called "Richness_St_Error" and divide "Richness_Std" by the square root of "Richness_n"
  mutate(Richness_St_Error=Richness_Std/sqrt(Richness_n))%>%
  mutate(Burn_Regime_Labels=ifelse(Fire_Regime=="Annual","Annual Burn Regime","Four year Burn Regime"))


#Appendix S8
## T-test comparing research area level species richness across exclosure treatments, using burn regimes as replicates ##
#Graminoids
Functional_Group_Graminoids<-(subset(Functional_Group_Extra_Species_Richness_Summary,functional_group=="graminoid"))
t.test(Functional_Group_Graminoids$Richness_Mean~Functional_Group_Graminoids$exclosure)
t.test(Functional_Group_Graminoids$Richness_Mean~Functional_Group_Graminoids$Fire_Regime)

#Forbs
Functional_Group_forb<-(subset(Functional_Group_Extra_Species_Richness_Summary,functional_group=="forb"))
t.test(Functional_Group_forb$Richness_Mean~Functional_Group_forb$exclosure)
t.test(Functional_Group_forb$Richness_Mean~Functional_Group_forb$Fire_Regime)

#Legumes
Functional_Group_legume<-(subset(Functional_Group_Extra_Species_Richness_Summary,functional_group=="legume"))
t.test(Functional_Group_legume$Richness_Mean~Functional_Group_legume$exclosure)
t.test(Functional_Group_legume$Richness_Mean~Functional_Group_legume$Fire_Regime)

#Woody
Functional_Group_woody<-(subset(Functional_Group_Extra_Species_Richness_Summary,functional_group=="woody"))
t.test(Functional_Group_woody$Richness_Mean~Functional_Group_woody$exclosure)
t.test(Functional_Group_woody$Richness_Mean~Functional_Group_woody$Fire_Regime)

##Appendix S9 - Make a bar graph using ggplot2.  Use data from "Functional_Group_Extra_Species_Diversity_Summary".  Change the aesthetics x is equal to the data from "exlosure", and y is equal to the "Richness_Mean"
ggplot(Functional_Group_Extra_Species_Richness_Summary,aes(x=exclosure,y=Richness_Mean, fill=functional_group, position="stack"))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity",color="black")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  #Label the x-axis "Treatment"
  xlab("Deer Exclusure Treatment")+
  #Label the y-axis "Species Richness"
  ylab(bquote("Species Richness (per 900" *~m^2*")"))+
  scale_fill_manual(values=c("white","lightcyan2", "cadetblue4","darkslategrey"), labels=c("Non-Leguminous Forb","Graminoid","Legume","Woody"))+
  scale_x_discrete(labels=c("inside"="Deer Exclusion","outside"="Control"))+
  theme(legend.key = element_rect(size=4), legend.key.size = unit(1.0,"centimeters"))+
  facet_wrap(~Burn_Regime_Labels)+
  #Make the y-axis extend to 50
  expand_limits(y=50)+
  theme(strip.background = element_blank(), panel.border = element_rect(), strip.text.x = element_text(size=40), strip.text.y = element_text(size=40))+
  guides(fill=guide_legend(title="Functional Group"))+
  theme(legend.title = element_text(size = 30), legend.position = c(0.15,0.92))
#Save at the graph at 1400x1500
