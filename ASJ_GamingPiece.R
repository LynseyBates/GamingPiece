### Introduction ##############

# Part 0: Return ceramic data from Caribbean sites, filter for Gaming Pieces only

# Part 1: Gaming Piece AI index
# WBG and CEW denominators - read in counts by project (from Caribbean_AI_DenominatorTest.R)
# All Caribbean units, All Caribbean STPs
# Plot 1: Index values against Site MCDs, colored by island
# Plot 2: Index values against Site MCDs, colored by island, without zero values
# What about north american discs? Use old image from IACA paper?

# Part 2: Buttons Hypothesis
# 1) Histogram: MP assemblage only? Frequency vs. diameter
# 2) Box plot of button diameter by material, carved disc mean diameter = 24.2 mm
# 3) Compare mean measurement values: diameter, thickness, and weight of carved discs, buttons in daacs, and
# button blanks in daacs
# 4) Form attributes, discs made from hollow, flat, and unidentified vessel types
# 5) Percent ware type of morne patate carved discs
# 6) Percent Decoration for MP discs (decorative genre)

# Part 3: Over Time
# 1) Read in site data MCDs
# 2) Number of discs by phase - only 23?

setwd("P:/DAACS/Conferences_Talks/ASJ2018")

#load the library
require(RPostgreSQL)
library(dplyr)
require(tidyr)

# tell DBI which driver to use
pgSQL <- dbDriver("PostgreSQL")
# establish the connection
DRCcon<-dbConnect(pgSQL, host='drc.iath.virginia.edu', port='5432',
                  dbname='daacs-production',
                  user='drcquery', password='!queryacct!')

### Part 0: Ceramic Data ##############
carib_cerm<-dbGetQuery(DRCcon,'
                         SELECT
                         "public"."tblCeramic"."Quantity",
                         "public"."tblCeramicWare"."Ware",
                         "public"."tblCeramicGenre"."CeramicGenre",
"public"."tblCeramicForm"."CeramicForm",
"public"."tblCeramic"."SherdThickness",
"public"."tblCeramic"."MaximumSherdMeasurement",
                         "public"."tblCeramic"."SherdWeight",
                         "public"."tblContext"."ProjectID",
                         "public"."tblProjectName"."ProjectName",
"public"."tblContextUnitType"."UnitType",
"public"."tblContext"."MasterContextNumber"
                         FROM
                         "public"."tblContext"
                         INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                         INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                         INNER JOIN "public"."tblCeramic" ON "public"."tblCeramic"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                         INNER JOIN "public"."tblCeramicWare" ON "public"."tblCeramic"."WareID" = "public"."tblCeramicWare"."WareID"
                         LEFT JOIN "public"."tblCeramicGenre" ON "public"."tblCeramic"."CeramicGenreID" = "public"."tblCeramicGenre"."CeramicGenreID"
                         INNER JOIN "public"."tblProject" ON "public"."tblContext"."ProjectID" = "public"."tblProject"."ProjectID"
                         INNER JOIN "public"."tblProjectName" ON "public"."tblProject"."ProjectNameID" = "public"."tblProjectName"."ProjectNameID"
                         INNER JOIN "public"."tblCeramicForm" ON "public"."tblCeramic"."CeramicFormID" = "public"."tblCeramicForm"."CeramicFormID"
                       INNER JOIN "public"."tblContextUnitType" ON "public"."tblContext"."UnitTypeID" = "public"."tblContextUnitType"."UnitTypeID"
                     WHERE
                         "public"."tblContext"."ProjectID" = \'1243\'OR 
                         "public"."tblContext"."ProjectID" = \'1216\'OR
                         "public"."tblContext"."ProjectID" = \'1236\'OR
                         "public"."tblContext"."ProjectID" = \'1205\'OR
                         "public"."tblContext"."ProjectID" = \'1206\'OR
                         "public"."tblContext"."ProjectID" = \'1208\'OR
                         "public"."tblContext"."ProjectID" = \'1209\'OR
                         "public"."tblContext"."ProjectID" = \'1219\'OR
                         "public"."tblContext"."ProjectID" = \'1217\'OR
                         "public"."tblContext"."ProjectID" = \'1245\'OR
                         "public"."tblContext"."ProjectID" = \'1213\'OR
                         "public"."tblContext"."ProjectID" = \'1214\'OR
                         "public"."tblContext"."ProjectID" = \'1227\'OR
                         "public"."tblContext"."ProjectID" = \'1218\'OR
                         "public"."tblContext"."ProjectID" = \'1212\'OR
                         "public"."tblContext"."ProjectID" = \'1211\'OR
                         "public"."tblContext"."ProjectID" = \'1210\'OR
                         "public"."tblContext"."ProjectID" = \'1232\'OR
                         "public"."tblContext"."ProjectID" = \'1235\'OR
                         "public"."tblContext"."ProjectID" = \'1215\'OR
                         "public"."tblContext"."ProjectID" = \'1201\'OR
                         "public"."tblContext"."ProjectID" = \'1202\'OR
                         "public"."tblContext"."ProjectID" = \'1203\'OR
                         "public"."tblContext"."ProjectID" = \'1204\'
                         ')                   

# Filter by Unit Type
#carib_cerm_STP <- carib_cerm %>% filter(UnitType =='STP')
#carib_cerm_unit <- carib_cerm %>% filter(UnitType =='Quadrat/Unit')

# Keep only STP and Unit
#carib_cerm <- carib_cerm %>% filter(UnitType %in% c('Quadrat/Unit', 'STP'))

# Create Project Names df
names <- distinct(carib_cerm[,c(8,9)])

# For units, set MasterContextNumber to blank if NA
#carib_cerm_unit <- carib_cerm_unit %>% mutate(MasterContextNumber=ifelse(is.na(MasterContextNumber), '', MasterContextNumber))

# For units, remove Block C from MP
#carib_cerm_unit <- carib_cerm_unit %>% filter(MasterContextNumber != 'BlockC') 

### Part 1: Gaming Piece AI #################

# Select only gaming piece forms from cerm data
#carib_game_unit <- carib_cerm_unit %>% filter(CeramicForm %in% c("Gaming Piece","Gaming Piece, preform"))
#carib_game_STP <- carib_cerm_STP %>% filter(CeramicForm %in% c("Gaming Piece","Gaming Piece, preform"))

carib_game <- carib_cerm %>% filter(CeramicForm %in% c("Gaming Piece","Gaming Piece, preform"))

# Group by ProjectID
carib_game2 <- carib_game %>% group_by(ProjectID) %>% summarise_at("Quantity",sum) 

# Sum quantity of discs by ProjectID and Name
#caribgame_unit_byproj <- carib_game_unit %>% group_by(ProjectID, ProjectName) %>% summarise_at("Quantity", sum)
#caribgame_STP_byproj <- carib_game_STP %>% group_by(ProjectID, ProjectName) %>% summarise_at("Quantity", sum)

#caribgame_STP_byproj <- carib_game_STP %>% group_by(ProjectID, ProjectName) %>% summarise_at("Quantity", sum)

### Calculate the AIs

# Read in WBG count data
carib_STP_WBG <- read.csv("CaribSites_WBGcounts_STP.csv", stringsAsFactors = F, header = T)
carib_Unit_WBG <- read.csv("CaribSites_WBGcounts_Units.csv", stringsAsFactors = F, header = T)

carib_Unit_WBG$ProjectID <- as.character(carib_Unit_WBG$ProjectID)
carib_STP_WBG$ProjectID <- as.character(carib_STP_WBG$ProjectID)

# Join Unit and STP data
carib_WBG <- full_join(carib_Unit_WBG, carib_STP_WBG, by=c("ProjectID","blueMCD","MCD"))

# Replace NA values for quantity and area with 0s
carib_WBG <- carib_WBG %>% mutate(Quantity.y=ifelse(is.na(Quantity.y), 0, Quantity.y))
carib_WBG <- carib_WBG %>% mutate(Area.y=ifelse(is.na(Area.y), 0, Area.y))
carib_WBG <- carib_WBG %>% mutate(Quantity.x=ifelse(is.na(Quantity.x), 0, Quantity.x))
carib_WBG <- carib_WBG %>% mutate(Area.x=ifelse(is.na(Area.x), 0, Area.x))

# Create combined Quantity and Area fields
carib_WBG <- carib_WBG %>% mutate(WBG=Quantity.x+Quantity.y)
carib_WBG <- carib_WBG %>% mutate(Area=Area.x+Area.y)

carib_WBG <- carib_WBG %>% select(ProjectID, MCD, blueMCD, WBG)
  
# Join Gaming Disc Counts with WBG counts by ProjectID
carib_game3 <- full_join(carib_game2, carib_WBG, by="ProjectID")
carib_game3 <- carib_game3 %>% mutate(Quantity=ifelse(is.na(Quantity), 0, Quantity))

# Create new total field
carib_game3 <- carib_game3 %>% mutate(Total=Quantity+WBG)

# Create new AI field
carib_game3 <- carib_game3 %>% mutate(AI=Quantity/(Quantity+WBG))

# Create new island field
carib_game3 <- carib_game3 %>% mutate(island=ifelse(ProjectID %in% c(1201:1206,1210:1212,1217,1219,1227,1236,1245), 'Jamaica', NA))
carib_game3 <- carib_game3 %>% mutate(island=ifelse(ProjectID %in% c(1216, 1218, 1235, 1243), 'Dominica', island))
carib_game3 <- carib_game3 %>% mutate(island=ifelse(ProjectID %in% c(1208, 1209, 1213, 1214), 'Nevis', island))
carib_game3 <- carib_game3 %>% mutate(island=ifelse(ProjectID %in% c(1215), 'St Kitts', island))
carib_game3 <- carib_game3 %>% mutate(island=ifelse(ProjectID %in% c(1232), 'Barbados', island))

carib_game3 <- carib_game3 %>% mutate(AI=ifelse(is.na(AI), 0, AI))

# Confidence Interval function
adjustedWaldCI<-function(count,total,alpha){
  nTilde <- total+4
  pTilde <- (count+2)/(total+4)
  se <- sqrt((pTilde*(1-pTilde))/(nTilde))
  upperCL <- pTilde + se * qnorm(1-(alpha/2))
  lowerCL <- pTilde + se * qnorm(alpha/2) 
  upperCL<-ifelse ( upperCL > 1, 1, upperCL)
  lowerCL <-ifelse ( lowerCL < 0, 0, lowerCL)                               
  return(data.frame(pTilde,upperCL,lowerCL))
}

# Run CI function on data, put 
CI_disc_site <- adjustedWaldCI(carib_game3$Quantity,carib_game3$Total,0.05)
carib_game3$CIUpper <- CI_disc_site$upperCL
carib_game3$CILower <- CI_disc_site$lowerCL
carib_game3$p <- CI_disc_site$pTilde

# Set CI values to 0 for AIs of 0
carib_game3 <- carib_game3 %>% mutate(CIUpper=ifelse(AI==0, 0, CIUpper))
carib_game3 <- carib_game3 %>% mutate(CILower=ifelse(AI==0, 0, CILower))
carib_game3 <- carib_game3 %>% mutate(p=ifelse(AI==0, 0, p))

# Plot the Results
p1 <- ggplot(carib_game3, aes(x=carib_game3$blueMCD, y=carib_game3$p)) +
  geom_point(aes(fill=carib_game3$island), shape=21, size=8, alpha=0.75) +
  scale_y_continuous(limits=c(0.0,0.08), breaks=seq(0, 0.08, 0.01))+
  scale_x_continuous(limits=c(1760,1820), breaks=seq(1760, 1820, 5))+
    geom_errorbar(aes(ymin=carib_game3$CILower, ymax=carib_game3$CIUpper), colour="black", width=1)+
  theme_classic()+
  labs(x="Site BLUE MCD", y="Index Value")+
  ggtitle(expression(atop("Carved Disc by WBG Index", atop(italic("95% Confidence Intervals"), ""))))+
  theme(plot.title=element_text(size=rel(2)),axis.title=element_text(size=rel(2)),
        axis.text=element_text(size=rel(2), angle=90, hjust=1), legend.text=element_text(size=rel(2)),
        legend.title=element_text(size=rel(2)))+
scale_fill_manual(name="Island", values=c("darkgoldenrod1", "red", "blue", "lightgreen", "black"))  
p1
ggsave("DiscAI_allcarib_with0s.png", p1, width=10, height=7.5, dpi=300)

# Remove values with zeros
carib_game4 <- carib_game3 %>% filter(AI != 0)

# Plot results without zeros
p2 <- ggplot(carib_game4, aes(x=carib_game4$blueMCD, y=carib_game4$p)) +
  geom_point(aes(fill=carib_game4$island), shape=21, size=8, alpha=0.8) +
  scale_y_continuous(limits=c(0.0,0.08), breaks=seq(0, 0.08, 0.01))+
  scale_x_continuous(limits=c(1760,1820), breaks=seq(1760, 1820, 5))+
  geom_errorbar(aes(ymin=carib_game4$CILower, ymax=carib_game4$CIUpper), colour="black", width=.5)+
  theme_classic()+
  labs(x="Site BLUE MCD", y="Index Value")+
  ggtitle(expression(atop("Carved Disc by WBG Index", atop(italic("95% Confidence Intervals"), ""))))+
  theme(plot.title=element_text(size=rel(2)),axis.title=element_text(size=rel(2)),
        axis.text=element_text(size=rel(2), angle=90, hjust=1), legend.text=element_text(size=rel(2)),
        legend.title=element_text(size=rel(2)))+
  scale_fill_manual(name="Island", values=c("darkgoldenrod1", "red", "blue"))  
p2
ggsave("DiscAI_allcarib.png", p2, width=10, height=7.5, dpi=300)

### Part 3: Gaming Piece Attributes #################

# 3.1 Histogram: MP assemblage only? Frequency vs. diameter

# Create histogram
p3 <- ggplot(data=carib_game, aes(carib_game$MaximumSherdMeasurement)) + 
  geom_histogram(breaks=seq(15, 55, by=4), fill="darkgreen", col="black")+
theme_classic()+
  labs(x="Diameter", y="Frequency")+
  ggtitle(expression(atop("Carved Disc Diameter")))+
    theme(plot.title=element_text(size=rel(2)),
          axis.title=element_text(size=rel(2)),
        #  axis.text.x=element_blank(), 
          legend.text=element_text(size=rel(2)),
        legend.title=element_text(size=rel(2)),
        axis.ticks.x=element_blank(),
        axis.text = element_text(size=rel(2), angle=90, hjust=1))+
  scale_y_continuous(limits=c(0,40), breaks=seq(0, 40, 5))+
  scale_x_continuous(limits=c(15,55), breaks=seq(15, 55, 5))
p3  
ggsave("Dischistogram_allcarib.png", p3, width=10, height=7.5, dpi=300)


# 3.2 Box plot of button diameter by material, carved disc mean diameter = 24.2 mm

carib_btn<-dbGetQuery(DRCcon,'
                         SELECT
                       "public"."tblButton"."ArtifactID",
                       "public"."tblButtonMaterial"."ButtonMaterial",
"public"."tblButtonType"."ButtonType",
"public"."tblButton"."Weight",
"public"."tblButton"."Diameter",
                       "public"."tblButton"."Quantity",
                       "public"."tblContext"."ProjectID",
                       "public"."tblProjectName"."ProjectName",
                       "public"."tblContextUnitType"."UnitType",
                       "public"."tblContext"."MasterContextNumber"
                       FROM
                       "public"."tblContext"
                        INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                        INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                       INNER JOIN "public"."tblButton" ON "public"."tblButton"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                       INNER JOIN "public"."tblButtonMaterial" ON "public"."tblButton"."ButtonMaterialID" = "public"."tblButtonMaterial"."ButtonMaterialID"
                       INNER JOIN "public"."tblProject" ON "public"."tblContext"."ProjectID" = "public"."tblProject"."ProjectID"
                       INNER JOIN "public"."tblProjectName" ON "public"."tblProject"."ProjectNameID" = "public"."tblProjectName"."ProjectNameID"
                       INNER JOIN "public"."tblContextUnitType" ON "public"."tblContext"."UnitTypeID" = "public"."tblContextUnitType"."UnitTypeID"
INNER JOIN "public"."tblButtonType" ON "public"."tblButton"."ButtonTypeID" = "public"."tblButtonType"."ButtonTypeID"
                   WHERE
                       "public"."tblContext"."ProjectID" = \'1243\'OR 
                       "public"."tblContext"."ProjectID" = \'1216\'OR
                       "public"."tblContext"."ProjectID" = \'1236\'OR
                       "public"."tblContext"."ProjectID" = \'1205\'OR
                       "public"."tblContext"."ProjectID" = \'1206\'OR
                       "public"."tblContext"."ProjectID" = \'1208\'OR
                       "public"."tblContext"."ProjectID" = \'1209\'OR
                       "public"."tblContext"."ProjectID" = \'1219\'OR
                       "public"."tblContext"."ProjectID" = \'1217\'OR
                       "public"."tblContext"."ProjectID" = \'1245\'OR
                       "public"."tblContext"."ProjectID" = \'1213\'OR
                       "public"."tblContext"."ProjectID" = \'1214\'OR
                       "public"."tblContext"."ProjectID" = \'1227\'OR
                       "public"."tblContext"."ProjectID" = \'1218\'OR
                       "public"."tblContext"."ProjectID" = \'1212\'OR
                       "public"."tblContext"."ProjectID" = \'1211\'OR
                       "public"."tblContext"."ProjectID" = \'1210\'OR
                       "public"."tblContext"."ProjectID" = \'1232\'OR
                       "public"."tblContext"."ProjectID" = \'1235\'OR
                       "public"."tblContext"."ProjectID" = \'1215\'OR
                       "public"."tblContext"."ProjectID" = \'1201\'OR
                       "public"."tblContext"."ProjectID" = \'1202\'OR
                       "public"."tblContext"."ProjectID" = \'1203\'OR
                       "public"."tblContext"."ProjectID" = \'1204\'
                       ')                   

# Remove records where diameter is NA
carib_btn2 <- carib_btn %>% filter(!is.na(Diameter))

# Only keep certain materials
carib_btn3 <- carib_btn2 %>% filter(! ButtonMaterial %in% c('Ceramic','Gold','Ivory','Metal, unid.',
                                                            'Silver','Missing','Glass',
                                                            'Synthetic, unid.', 'Synthetic/Modern'))
# Recode certain material types
carib_btn3 <- carib_btn3 %>% mutate(ButtonMaterial=ifelse(ButtonMaterial=='Lead Alloy', 'Pewter', ButtonMaterial))
carib_btn3 <- carib_btn3 %>% mutate(ButtonMaterial=ifelse(ButtonMaterial=='Copper Alloy', 'Cu Alloy', ButtonMaterial))

# Box plot
p4 <- ggplot(carib_btn3, aes(x=carib_btn3$ButtonMaterial, y=carib_btn3$Diameter))+
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, fill="lightgreen")+
  theme_classic()+
  labs(x="Material Type", y="Diameter")+
  ggtitle(expression(atop("Button Diameter by Material")))+
  theme(plot.title=element_text(size=rel(2)),
        axis.title=element_text(size=rel(2)),
        #  axis.text.x=element_blank(), 
        legend.text=element_text(size=rel(2)),
        legend.title=element_text(size=rel(2)),
        axis.ticks.x=element_blank(),
        axis.text = element_text(size=rel(2), angle=90, hjust=1))+
  scale_y_continuous(limits=c(0,40), breaks=seq(0, 40, 5))
p4
ggsave("BtnDiamBoxPlot_allcarib.png", p4, width=10, height=7.5, dpi=300)

# 3.3 Compare mean measurement values: diameter, thickness, and weight of carved discs, 
# buttons in daacs, and button blanks in daacs

