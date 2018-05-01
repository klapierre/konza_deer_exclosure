####### Konza Deer Exclosure Project######

###Set all general necessities###

#Set working directory 
setwd("/Users/kathrynbloodworth/Documents/SERC/Projects/Deer_Exclosure")

#Load and install libraries
library(MASS)
#install.packages("vegan")
library(vegan)
#install.packages("ggplot2")
library(ggplot2)
library(grid)
#install.packages("devtools")
library(devtools)
#take this library from github called codyn that is still being developed and load it into R
install_github("NCEAS/codyn",ref=github_pull(83))
library(codyn)
#install.packages("tidyverse")
library(tidyverse)
#Set ggplot2 theme to black and white
theme_set(theme_bw())
#Update ggplot2 theme - make box around the x-axis title size 30, vertically justify x-axis title to 0.35, Place a margin of 15 around the x-axis title.  Make the x-axis title size 30. For y-axis title, make the box size 30, put the writing at a 90 degree angle, and vertically justify the title to 0.5.  Add a margin of 15 and make the y-axis text size 25. Make the plot title size 30 and vertically justify it to 2.  Do not add any grid lines.  Do not add a legend title, and make the legend size 20
theme_update(axis.title.x=element_text(size=30, vjust=-0.35, margin=margin(t=15)),
             axis.text.x=element_text(size=30), axis.title.y=element_text(size=30, angle=90, vjust=0.5,
                                                                          margin=margin(r=15)), axis.text.y=element_text(size=30), plot.title =
               element_text(size=30, vjust=2), panel.grid.major=element_blank(),
             panel.grid.minor=element_blank(), legend.title=element_blank(),
             legend.text=element_text(size=30))

###Read in all data and join data frames###


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


### 30 x 30 m Research Area Speices Richness ###


#Make a new data table and place the data from "Deer_Exclosure_Spp_Name" in it
Extra_Species_Identity<-Deer_Exclosure_Spp_Name%>%
  #Keep only the columns labeled "Watershed", "exclosure", and "taxa"
  select(Watershed,exclosure,taxa)%>%
  #Keep only the unique data
  unique()%>%
  #Make a new coulmn named "Species_Presence" and place a 1 in every cell so that a wide table can be made later
  mutate(Species_Presence=1)

#Make a new data frame from "Extra_Species_Identity" to generate richness values for each research area
Extra_Species_Richness<-Extra_Species_Identity%>%  
  #group data frame by Watershed and exclosure
  group_by(Watershed,exclosure)%>%
  #Make a new column named "Richness" and add the unique number of rows in the column "taxa" according to the groupings
  summarise(Richness=length(taxa))%>%
  #stop grouping by watershed and exclosure
  ungroup()%>%
  #Make a new column called "Treatment" and paste columns "Watershed", and "exclosure" together, separating them with a "."
  mutate(Treatment=paste(Watershed,exclosure,sep="."))


#T-test comparing research area level species richness across exclosure treatments, using watersheds as replicates
t.test(Extra_Species_Richness$Richness~Extra_Species_Richness$exclosure)

#Make a new data table using data from "Extra_Species_Richness" to generate Figure 4.
Extra_Species_Richness_Summary<-Extra_Species_Richness%>%
  #Group data by the columns "Watershed" and "exclosure"
  group_by(exclosure)%>%
  #In this data frame, summarize the data.  Make a new column named "Richness_Std" and calculate the standard deviation from the column "Richness".  Also calculate the mean and length from the column "Richness" and place them into their own columns.
  summarize(Richness_Std=sd(Richness),Richness_Mean=mean(Richness),Richness_n=length(Richness))%>%
  #Make a new column called "Richness_St_Error" and divide "Richness_Std" by the square root of "Richness_n"
  mutate(Richness_St_Error=Richness_Std/sqrt(Richness_n))

##Figure 3 - Make a bar graph using ggplot2.  Use data from "Extra_Species_Diversity_Summary".  Change the aesthetics x is equal to the data from "exlosure", and y is equal to the "Richness_Mean"
ggplot(Extra_Species_Richness_Summary,aes(x=exclosure,y=Richness_Mean, fill=exclosure))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.  
  geom_bar(stat="identity", position=position_dodge())+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  geom_errorbar(aes(ymin=Richness_Mean-Richness_St_Error,ymax=Richness_Mean+Richness_St_Error),position=position_dodge(0.9),width=0.2)+
  scale_fill_manual(values=c("lightskyblue2","dodgerblue4"))+
  #Label the x-axis "Treatment"
  xlab("Treatment")+
  #Label the y-axis "Species Richness"
  ylab("Species Richness")+
  scale_x_discrete(labels=c("inside"="Inside Fence","outside"="Outside Fence"))+
  #Make the y-axis extend to 50
  expand_limits(y=50)+
  theme(legend.position = "none")
#Save at the graph at 1400x1500

##Supplementary Table 1 - Make a new data table called "Species_Table" using data from "Extra_Species_Richness"
Species_Table <- Extra_Species_Identity %>%
  #Make a new column named "Location" change exclosure data to "Outside Fence" and "Inside Fence"
  mutate(Location=ifelse(exclosure=="outside", "Outside Fence", "Inside Fence"))%>%
  #Make a new column named "Treatment" and paste Watershed and Location together, separating them with a dash
  mutate(Treatment=paste(Watershed,Location, sep = "-"))%>%
  #Keep only the columns taxa, Species_Presence and Treatment
  select(taxa,Species_Presence,Treatment)%>%
  #Delete all rows that are not unique
  unique()%>%
  #Make a wide data frame where the treatments are the column headers and taxa are rows and the values are filled with information from "Species_Presence".  If there is no information from "Species_Presence" then fill that cell with a zero
  spread(key = Treatment, value=Species_Presence, fill=0)
#Write a csv file onto the computer
write.csv(Species_Table, file = "Species_Table.csv")

###Plot Level Species Richness and Evenness###


##Calculate Relative Cover##

#Calculate Total Cover and put it into a new data tabel named "Total_Cover"
Total_Cover<-Deer_Exclosure_Spp_Name%>%
  #filter out all data where plot = 999 (this will filter out extra research area level species data)
  filter(plot!=999)%>%
  #group by "Watershed" and "plot"
  group_by(Watershed,plot)%>%
  #In a column named "Total Cover" summarize the sum of "Cover" per grouping
  summarize(Total_Cover=sum(cover))

#Calculate Relative Cover - make a new file named Relative_Cover using the data from Deer_Exclosure_Spp_Name
Relative_Cover<-Deer_Exclosure_Spp_Name%>%
  #Filter out all data where plot = 999 (this will filter out extra research area level species data)
  filter(plot!=999)%>%
  #Make a new column named "Treatment" and paste "Watershed" and "exclosure" together, separating them by a period.
  mutate(Treatment=paste(Watershed,exclosure,sep="_"))%>%
  mutate(Plot_number=paste(Treatment,plot,sep = "_"))%>%
  #Add Total_Cover data into the Relative_Cover data sheet
  left_join(Total_Cover)%>%
  #In the data sheet Relative_Cover, add a new column called "Relative_Cover", in which you divide "cover" by "Total_Cover"
  mutate(Relative_Cover=cover/Total_Cover)%>%
  #delete column "cover"
  select(-cover)


##Calculate Richness##

#Make a new data frame called Species Richness using data from Relative Cover
Species_Richness<-Relative_Cover%>%
  #do not include zeros
  filter(Relative_Cover!=0)%>%
  #group data frame by Watershed and plot
  group_by(Watershed,plot)%>%
  #In a column named richness, give the number of rows in Relative_Cover
  summarize(Richness=length(Relative_Cover))%>%
  #stop grouping by watershed and plot
  ungroup()

#Make a new data frame called Wide_Relative_Cover using data from Relative_Cover
Wide_Relative_Cover<-Relative_Cover%>%
  #Keep only the columns taxa, Relative_Cover, Watershed, plot, and exclosure
  select(taxa,Relative_Cover,Watershed,plot,exclosure)%>%
  #Make a wide table using column taxa as overarching columns, fill with values from Relative_Cover column, if there is no value for one cell, insert a zero
  spread(key=taxa,value=Relative_Cover, fill=0)


##EQ - Evenness Quotient##

#Make a new data frame called Community_Metrics and run the Evenness Quotient on the Relative_Cover data
Community_Metrics<-community_structure(Relative_Cover,replicate.var = "Plot_number",abundance.var = "Relative_Cover",metric = "EQ")%>%
  #Separate out Plot_number into three separate columns
  separate(Plot_number,c("Watershed","exclosure","plot"), sep = "_")


##Run ANOVA##

#Make a new model output named Richness_ANOVA and run an ANOVA - Richness (dependent/response variable), against watershed and exclosure (independent/explanitory variables), *run individually and together to determine an interaction
summary(Richness_ANOVA<-aov(richness~Watershed*exclosure,data=Community_Metrics))
#Run a t-test comparing "Richness" and "Watershed" because ANOVA results were significant
pairwise.t.test(Community_Metrics$richness, Community_Metrics$Watershed)
summary(EQ_ANOVA<-aov(EQ~Watershed*exclosure,data=Community_Metrics))
pairwise.t.test(Community_Metrics$EQ, Community_Metrics$Watershed)

#Make a new dataframe to generate Figure 2 called Diversity_Summary using data from Diversity
Diversity_Summary<-Community_Metrics%>%
  #group by watershed and exclosure
  group_by(Watershed,exclosure)%>%
  #In new columns, calculate the standard deviation, mean, and length of Species Richness and EQ  
  summarize(Richness_Std=sd(richness),Richness_Mean=mean(richness), Richness_n=length(richness), EQ_Std=sd(EQ), EQ_Mean=mean(EQ), EQ_n=length(EQ))%>%
  #Make a new column and calculate the Standard Error for Species Richness and EQ
  mutate(Richness_St_Error=Richness_Std/sqrt(Richness_n), EQ_St_Error=EQ_Std/sqrt(EQ_n))

##Figure 2 - Make a bar graph of species richness and evenness##

#Figure 2a - Make a dataframe called Species_Richness_Plot using gglpot2.  Use data from Diversity_Summary, making the x-axis Watershed, and y-axis Richness_Mean, the bars should be based on exclosures
Species_Richness_Plot<-ggplot(Diversity_Summary,aes(x=Watershed,y=Richness_Mean,fill=exclosure))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge)
  geom_bar(stat="identity", position=position_dodge())+
  #make error bars using the Standard error from the mean and place them at 0.9 with a width of 0.2
  geom_errorbar(aes(ymin=Richness_Mean-Richness_St_Error,ymax=Richness_Mean+Richness_St_Error),position=position_dodge(0.9),width=0.2)+
  #Label the x-axis "Watershed"
  xlab("Watershed")+
  #Label the y-axis "Species Richness"
  ylab("Species Richness")+
  #Fill the bar graphs with colors and label them "Inside Fence" and "Outside Fence"
  scale_fill_manual(values=c("lightskyblue2","dodgerblue4"), labels=c("Inside Fence","Outside Fence"))+
  #Make the y-axis expand to 20
  expand_limits(y=20)+
  #Place the legend at 0.8,0.94 and space it out by 3 lines
  theme(legend.position=c(0.75,0.94), legend.key.size = unit(2.0, 'lines'))+
  #Add "a." to the graph in size 10 at position 0.6,20
  annotate("text",x=0.6,y=20,label="a.",size=15)+
  #Add "A" to the graph in size 6 at position 1,17
  annotate("text",x=1,y=17,label="A",size=10)+
  #Add "B" to the graph in size 6 at position 2,13.5
  annotate("text",x=2,y=13.5,label="B",size=10)+
  #Add "A" to the graph in size 6 at position 3,17.5
  annotate("text",x=3,y=17.5,label="A",size=10)
  
#Figure 2b - Make a dataframe called EQ_Plot using ggplot2.  Use data from Diversity summary, with the x-axis being Watershed, and the y-axis being EQ_Mean, the bars should be based on exclosures
EQ_Plot<-ggplot(Diversity_Summary,aes(x=Watershed,y=EQ_Mean,fill=exclosure))+
  ##Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and outline the bars with the color black.
  geom_bar(stat="identity", position=position_dodge())+
  #Make error bars using the standard error from the mean.  Position them at 0.9 with a width of 0.2  
  geom_errorbar(aes(ymin=EQ_Mean-EQ_St_Error,ymax=EQ_Mean+EQ_St_Error),position=position_dodge(0.9),width=0.2)+
  #Label the x-axis "Watershed"
  xlab("Watershed")+
  #Label the y-axis "Evenness Quotient"
  ylab("Evenness Quotient")+
  #Fill the bars with grey and white and label them "Inside Fence" and "Outside Fence"
  scale_fill_manual(values=c("lightskyblue2","dodgerblue4"), labels=c("Inside Fence","Outside Fence"))+
  #do not include a legend
  theme(legend.position="none")+
  #Expand the y-axis to 1.0
  expand_limits(y=0.20)+
  #add text "b." at 0.6,1.0 in size 10
  annotate("text",x=0.6,y=0.20,label="b.",size=15)+
  #Add "A" to the graph in size 6 at position 1,17
  annotate("text",x=1,y=0.17,label="A",size=10)+
  #Add "B" to the graph in size 6 at position 2,13.5
  annotate("text",x=2,y=0.158,label="B",size=10)+
  #Add "A" to the graph in size 6 at position 3,17.5
  annotate("text",x=3,y=0.163,label="AB",size=10)

#Figure 2 - Make a figure that has one row and two columns for two graphs
pushViewport(viewport(layout=grid.layout(1,2)))
#print out the viewport plot where the Species_Richness_plot is at position 1,1
print(Species_Richness_Plot,vp=viewport(layout.pos.row=1, layout.pos.col =1))
#print out the viewport plot where the Species_Richness_plot is at position 1,2
print(EQ_Plot,vp=viewport(layout.pos.row=1, layout.pos.col =2))
#Save at 1800 x 1000  


### Figure 4 - Rank Abundance Curves ###


#Make a new data table called RAC_Mean using data from Relative_Cover - the mean is being taken across all plots for ease of comparison
RAC_Mean<-Relative_Cover%>%
  #Group by Treatment and taxa
  group_by(Treatment,taxa)%>%
  #In a column named "Mean" give the mean of the "Relative_Cover" according to the grouping
  summarize(Mean=mean(Relative_Cover))%>%
  #Ungroup the data
  ungroup()%>%
  #Arrange the columns by "Treatment" and the reverse "Mean" (this will arrange the data table to be categorized by treatment and then highest to lowest mean within each treatmnet
  arrange(Treatment,-Mean)%>%
  #Group by treatment
  group_by(Treatment)%>%
  #Make a new column called "Ranks" and give the rank from low to high of the treatments according to the grouping
  mutate(Ranks=seq_along(Treatment))%>%
  #Ungroup data
  ungroup()%>%
  #Spearate out "Treatment" into two columns called "Watershed" and "exclosure"
  separate(Treatment,c("Watershed","exclosure"))%>%
  #Make a new column called "Treatment_Type" where data from "exclosure" is taken and rewritten as "Inside Fence" and "Outside Fence"
  mutate(Treatment_Type=ifelse(exclosure=="inside","Inside Fence", "Outside Fence"))%>%
  #Make a new column called Symbol so that if the taxa is equal to one stated below, the symbol number will be entered (15,5,2,etc.), for all else enter 16 (rare species) - this will later allow for symbols to be assigned in the figure 3 based on this column
  mutate(Symbol=ifelse(taxa=="andropogon gerardii",15, ifelse(taxa=="eragrostis spectabilis",5, ifelse(taxa=="schizachyrium scoparium",2, ifelse(taxa=="sorghastrum nutans",18, ifelse(taxa=="sporobolus asper",1,ifelse(taxa=="aster ericoides",17,ifelse(taxa=="amorpha canescens",0,16))))))))

#Made plot using data from RAC_Mean with x being "Ranks" and y being "Mean"
ggplot(RAC_Mean,aes(x=Ranks,y=Mean))+
  #Make a line graph
  geom_line()+
  #when making the points in the line graph, change the symbols to be equal to the "Symbol" column and make them a size 3
  geom_point(shape=RAC_Mean$Symbol, size=4)+
  #Label the x-axis "Species Rank"
  xlab("Species Rank")+
  #Label the y-axis "Relative Abundance (%)"
  ylab("Relative Abundance (%)")+
  #Change the theme so that the boarders and backgrounds are white and the x- and y-axis text size is 24
  theme(strip.background = element_rect(color="white",fill="white"),strip.text.x = element_text(size=28), strip.text.y = element_text(size=28))+
  #Makes a matrix of panels defined by row and column facetting variables (Watershed and Treatmnet_Type)
  facet_grid(Watershed~Treatment_Type)
#save at 1500 x 933


### Bray-Curtis NMDS ###


#Update the theme so that legends will have titles (all else is the same as above)
theme_update(axis.title.x=element_text(size=30, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=30),
             axis.title.y=element_text(size=30, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=30),
             plot.title = element_text(size=30, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.text=element_text(size=25))

#Figure 1
#Make new data frame called BC_Data and run an NMDS using data from Wide_Relative_Cover, columns 4 through 49
BC_Data <- metaMDS(Wide_Relative_Cover[,4:49])
#Make a data frame called sites with 1 column and same number of rows that is in Wide_Relative_Cover
sites <- 1:nrow(Wide_Relative_Cover)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-3
BC_Meta_Data <- Wide_Relative_Cover[,1:3]
#make a plot using the dataframe BC_Data and the column "points".  Make "Watershed" a factor - make the different watersheds different colors
plot(BC_Data$points,col=as.factor(BC_Meta_Data$Watershed))
#make elipses using the BC_Data.  Group by "Watershed" and use standard deviation to draw eclipses and display by sites, add labels based on Watershed type.
ordiellipse(BC_Data,groups = as.factor(BC_Meta_Data$Watershed),kind = "sd",display = "sites", label = T)

#Use the vegan ellipse function to make ellipses           
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}
#Make a data frame called BC_NMDS and at a column using the first set of "points" in BC_Data and a column using the second set of points.  Group them by watershed
BC_NMDS = data.frame(MDS1 = BC_Data$points[,1], MDS2 = BC_Data$points[,2],group=BC_Meta_Data$Watershed)
#Make data table called BC_NMDS_Graph and bind the BC_Meta_Data, and BC_NMDS data together
BC_NMDS_Graph <- cbind(BC_Meta_Data,BC_NMDS)
#Make a data table called BC_Ord_Ellipses using data from BC_Data and watershed information from BC_Meta_Data.  Display sites and find the standard error at a confidence iinterval of 0.95.  Place lables on the graph
BC_Ord_Ellipses<-ordiellipse(BC_Data, BC_Meta_Data$Watershed, display = "sites",
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
  geom_point(size=5, aes(color = exclosure)) +
  #Make "inside" and "outside" dark grey and back and label them "Inside Fence", "Outside Fence".  Label this legend "Treatment"
  scale_color_manual(breaks=c("inside","outside"),
                     values = c("lightskyblue2","dodgerblue4"),
                     labels=c("Inside Fence", "Outside Fence"),
                     name="Treatment")+
  #Use different shapes according to Watershed types
  scale_shape_discrete(name="Watershed")+
  #Use the data from BC_Ellipses to make ellipses that are size 1 with a solid line
  geom_path(data = BC_Ellipses, aes(x=NMDS1, y=NMDS2), size=1, linetype=1)+
  #make the text size of the legend titles 28
  theme(legend.title = element_text(size=28),  legend.key.size = unit(2.0, 'lines'), legend.position = c(0.89,0.81))+
  #Add annotations of K1B, 4B, and K4A inside the elipses and bold them
  annotate("text",x=-.16,y=0.27,label="K1B",size=10, fontface="bold")+
  annotate("text",x=0.04,y=-0.09,label="4B",size=10, fontface="bold")+
  annotate("text",x=0.30,y=-0.19,label="K4A",size=10, fontface="bold")+
  #Label the x-axis "NMDS1" and the y-axis "NMDS2"
xlab("NMDS1")+
  ylab("NMDS2")
#export at 1500x933


##PerMANOVA

#Make a new dataframe with the data from Wide_Relative_Cover all columns after 4
Species_Matrix <- Wide_Relative_Cover[,4:ncol(Wide_Relative_Cover)]
#Make a new dataframe with data from Wide_Relative_Cover columns 1-3
Environment_Matrix <- Wide_Relative_Cover[,1:3]

#Make a new dataframe with data from Relative_Cover
Relative_Cover2 <- Relative_Cover%>%
  #Keep only the columns "Watershed", "Exclosure","plot","taxa", and "Relative_Cover"
  select(Watershed,exclosure,plot,taxa,Relative_Cover)
#Make a new dataframe with data from Relative_Cover2
Wide_Relative_Cover2 <- Relative_Cover2%>%
  #Make a qide data frame using "Taxa" as the columns and fill with "Relative_Cover", if there is no data, fill cell with zero
  spread(key = taxa, value = Relative_Cover, fill = 0)
#run a perMANOVA comparing across watershed and exclosure, how does the species composition differ.  Permutation = 999 - run this 999 times and tell us what the preportion of times it was dissimilar
PerMANOVA <- adonis2(formula = Species_Matrix~Watershed*exclosure, data=Environment_Matrix,permutations = 999, method = "bray")
#give a print out of the PermMANOVA
print(PerMANOVA)

##PermDisp

#Make a new dataframe and calculate the dissimilarity of the Species_Matrix dataframe
BC_Distance_Matrix <- vegdist(Species_Matrix)
#Run a dissimilarity matrix (PermDisp) comparing watershed
Dispersion_Results_Watershed <- betadisper(BC_Distance_Matrix,Wide_Relative_Cover$Watershed)
permutest(Dispersion_Results_Watershed,pairwise = T, permutations = 999)
#Run a dissimilarity matrix (PermDisp) comparing exlosure
Dispersion_Results_Treatment <- betadisper(BC_Distance_Matrix,Wide_Relative_Cover$exclosure)
permutest(Dispersion_Results_Treatment,pairwise = T, permutations = 999)


### Supplemental Tables 2-4 - SIMPER ###

#Run a SIMPER test comparing data from the Environment_Matrix, to data from the Species_Matrix grouping by "Watershed"
SIMPER <- with(Environment_Matrix,simper(Species_Matrix,Watershed))
#Print out a summary of the results
summary(SIMPER)

