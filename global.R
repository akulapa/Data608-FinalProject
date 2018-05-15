library(shiny)
library(leaflet)
library(dplyr)
library(reshape2)
library(tidyr)

#Load data
CTDrug <- read.csv("https://raw.githubusercontent.com/akulapa/Data608-FinalProject/master/CT_Drug.csv", header=TRUE, stringsAsFactors=FALSE)
#CTDrug <- read.csv("C:/Pavan/CUNY/608/FinalY/Design1/CT_Drug.csv", header=TRUE, stringsAsFactors=FALSE)
CTDrug$zip <- paste0("0",CTDrug$zip)

#Factor and order variables
drugOrder <- c('Amphet','Benzodiazepine','Cocaine','EtOH','Fentanyl','Heroin','Hydrocodone','Morphine','Opioid',
               'Oxycodone','Oxymorphone','Tramad','Other')
drugOrderGG <- c('Other','Tramad','Oxymorphone','Oxycodone','Opioid','Morphine','Hydrocodone','Heroin','Fentanyl',
                 'EtOH','Cocaine','Benzodiazepine','Amphet')

ageOrder <- c("<15", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", ">74","Grand Total")

raceOrder <- c("Black", "Hispanic", "White", "Other","Grand Total")
genderOrder <- c("Female", "Male", "Grand Total")

#Convert Race and AgeGroup into factors
CTDrug$Race2 <- ifelse(CTDrug$Race == 'Other','ROther',CTDrug$Race)
CTDrug$Race <- factor(CTDrug$Race, raceOrder)
CTDrug$AgeGroup <- factor(CTDrug$AgeGroup, ageOrder)

CTDrug_YearOverYear <- CTDrug %>% 
  select(Year) %>% 
  group_by(Year) %>%
  summarize(TotalDeaths = n())

CTDrug_UniqueGender <- CTDrug %>% select(Sex) %>% unique()
CTDrug_UniqueGender$Sex <- as.character(CTDrug_UniqueGender$Sex)
CTDrug_UniqueGender <- rbind(c("All"),CTDrug_UniqueGender)
CTDrug_UniqueGender$Sex <- factor(CTDrug_UniqueGender$Sex, c("All",genderOrder))
CTDrug_UniqueGender$Sex <- sort(CTDrug_UniqueGender$Sex)

CTDrug_UniqueAge <- CTDrug %>% select(AgeGroup) %>% unique()
CTDrug_UniqueAge$AgeGroup <- as.character(CTDrug_UniqueAge$AgeGroup)
CTDrug_UniqueAge <- rbind(c("All"),CTDrug_UniqueAge)
CTDrug_UniqueAge$AgeGroup <- factor(CTDrug_UniqueAge$AgeGroup, c("All",ageOrder))
CTDrug_UniqueAge$AgeGroup <- sort(CTDrug_UniqueAge$AgeGroup)

CTDrug_UniqueRace <- CTDrug %>% select(Race) %>% unique()
CTDrug_UniqueRace$Race <- as.character(CTDrug_UniqueRace$Race)
CTDrug_UniqueRace<-rbind(c("All"),CTDrug_UniqueRace)
CTDrug_UniqueRace$Race <- factor(CTDrug_UniqueRace$Race, c("All",raceOrder))
CTDrug_UniqueRace$Race <- sort(CTDrug_UniqueRace$Race)

CTDrug_YearOverYear$RateOfChange <- round(c(1,100*diff(CTDrug_YearOverYear$TotalDeaths)/CTDrug_YearOverYear[-nrow(CTDrug_YearOverYear),]$TotalDeaths),2)

#Location data based on year and location
CTDrug_Year <- CTDrug %>% 
  select(Year, DeathLoc, DeathState, Longitude, Latitude,zip) %>% 
  mutate(Year = as.integer(Year), DeathLoc = toupper(DeathLoc), DeathState = toupper(DeathState)) %>% 
  group_by(Year, DeathLoc, DeathState, Longitude, Latitude,zip) %>%
  summarize(TotalDeaths = n()) %>% ungroup()

#Drug data based on the year and location and gender
CTDrug_Gender <- CTDrug %>% 
  select(Year, DeathLoc, DeathState, Sex) %>% 
  na.omit() %>% 
  mutate(Year = as.integer(Year), DeathLoc = toupper(DeathLoc), DeathState = toupper(DeathState)) %>% 
  group_by(Year, DeathLoc, DeathState, Sex) %>%
  summarize(DrugDeaths = n()) %>% 
  ungroup() %>% 
  right_join(CTDrug_Year, by = c("Year" = "Year", "DeathLoc" = "DeathLoc", "DeathState" = "DeathState"))

CTDrug_Gender <- CTDrug_Gender %>% spread(Sex, DrugDeaths)

#Drug data based on the year and location and race
CTDrug_Race <- CTDrug %>% 
  select(Year, DeathLoc, DeathState, Race2) %>% 
  na.omit() %>% 
  mutate(Year = as.integer(Year), DeathLoc = toupper(DeathLoc), DeathState = toupper(DeathState)) %>% 
  group_by(Year, DeathLoc, DeathState, Race2) %>%
  summarize(DrugDeaths = n()) %>% 
  ungroup() %>% 
  right_join(CTDrug_Gender, by = c("Year" = "Year", "DeathLoc" = "DeathLoc", "DeathState" = "DeathState"))

CTDrug_Race <- CTDrug_Race %>% spread(Race2, DrugDeaths)

#Drug data based on the year and location and age
CTDrug_Age <- CTDrug %>% 
  select(Year, DeathLoc, DeathState, AgeClass) %>% 
  na.omit() %>% 
  mutate(Year = as.integer(Year), DeathLoc = toupper(DeathLoc), DeathState = toupper(DeathState)) %>% 
  group_by(Year, DeathLoc, DeathState, AgeClass) %>%
  summarize(DrugDeaths = n()) %>% 
  ungroup() %>% 
  right_join(CTDrug_Race, by = c("Year" = "Year", "DeathLoc" = "DeathLoc", "DeathState" = "DeathState"))

CTDrug_Age <- CTDrug_Age %>% spread(AgeClass, DrugDeaths)

#Drug data based on the year and location and drug
CTDrug_Drug <- CTDrug %>% 
  select(Year, DeathLoc, DeathState, Heroin,Cocaine,Fentanyl,Oxycodone,Oxymorphone,EtOH,Hydrocodone,
         Benzodiazepine,Amphet,Tramad,Morphine,Opioid,Other) %>% 
  na.omit() %>% 
  mutate(Year = as.integer(Year), DeathLoc = toupper(DeathLoc), DeathState = toupper(DeathState)) %>% 
  melt(id.vars = c('Year','DeathLoc','DeathState'), measure.vars = drugOrder) %>% 
  select(Year, DeathLoc, DeathState, Drug = variable, Usuage = value) %>% 
  filter(Usuage == 'Y') %>% 
  group_by(Year, DeathLoc, DeathState, Drug) %>%
  summarize(DrugDeaths = n()) %>% 
  ungroup() %>% 
  right_join(CTDrug_Age, by = c("Year" = "Year", "DeathLoc" = "DeathLoc", "DeathState" = "DeathState"))

CTDrug_Drug <- CTDrug_Drug %>% spread(Drug, DrugDeaths)

CTDrug_Drug[is.na(CTDrug_Drug)] <- 0

CTDrug_Drug<- CTDrug_Drug %>% 
  mutate(Other = ifelse (TotalDeaths > Heroin+Cocaine+Fentanyl+Oxycodone+Oxymorphone+EtOH+Hydrocodone+Benzodiazepine+
                           Amphet+Tramad+Morphine+Opioid+Other, Other + (TotalDeaths - (Heroin+Cocaine+Fentanyl+Oxycodone+
                                                                                          Oxymorphone+EtOH+Hydrocodone+Benzodiazepine+
                                                                                          Amphet+Tramad+Morphine+Opioid+Other)),Other))

Years <- CTDrug %>% distinct(Year) %>% select(Year) %>% na.omit() %>% mutate(Year = as.integer(Year)) %>% data.frame()

#Age Group
tblAge <-melt(CTDrug, id.vars = c("AgeGroup", "Year"), measure.vars = drugOrder)
colnames(tblAge)<-c('HeatGroup', 'Year', 'Drug', 'value')
tblAge$Drug <- factor(tblAge$Drug, drugOrderGG)
tblAge$HeatGroup <- factor(tblAge$HeatGroup, ageOrder)
tblAge <- tblAge %>% 
  filter(value=='Y') %>% 
  select(HeatGroup,Year,Drug) %>% 
  group_by(HeatGroup,Year,Drug) %>%
  summarise(Usage=n()) %>% ungroup()

tblAge <- tblAge %>% group_by(HeatGroup,Year) %>% mutate(Percentage = round(Usage*100/sum(Usage),2))

tblAgeGender <- CTDrug %>% 
  select(AgeGroup, Gender = Sex, Year) %>% 
  group_by(AgeGroup, Gender, Year) %>%
  summarise(Total=n()) %>% ungroup()

tblAgeGender$Gender <- factor(tblAgeGender$Gender, genderOrder)
tblAgeGender$AgeGroup <- factor(tblAgeGender$AgeGroup, ageOrder)


tblRaceGender <- CTDrug %>% 
  select(Race, Gender = Sex, Year) %>% 
  group_by(Race,Gender, Year) %>%
  summarise(Total=n()) %>% ungroup()

tblRaceGender$Gender <- factor(tblRaceGender$Gender, genderOrder)
tblRaceGender$Race <- factor(tblRaceGender$Race, raceOrder)

tblDrugGender <-melt(CTDrug, id.vars = c("Sex", "Year"), measure.vars = drugOrder)
colnames(tblDrugGender)<-c('Gender', 'Year', 'Drug', 'value')
tblDrugGender$Drug <- factor(tblDrugGender$Drug, drugOrderGG)
tblDrugGender$Gender <- factor(tblDrugGender$Gender, genderOrder)
tblDrugGender <- tblDrugGender %>% 
  filter(value=='Y') %>% 
  select(Gender,Year,Drug) %>% 
  group_by(Gender,Year,Drug) %>%
  summarise(Total=n()) %>% ungroup()


tblAgeYear <- CTDrug %>% 
  group_by(Year) %>% 
  summarise(GTotal = n()) %>% 
  inner_join(CTDrug) %>% 
  select(AgeGroup, Year, GTotal) %>% 
  group_by(AgeGroup, Year, GTotal) %>% 
  summarise(Total = n()) %>%
  select(AgeGroup,Year,Total, GTotal) %>% 
  mutate(Percentage = paste0(round(Total/GTotal*100,2),'%'), Percentage1 = round(Total/GTotal*100,2))

tblAgeYearG <- tblAgeYear %>% ungroup() %>% select(Year, Total = GTotal) %>% unique() %>% 
  mutate(AgeGroup='Grand Total', GTotal = 0, Percentage='', Percentage1=0) %>% 
  select(AgeGroup, Year, Total, GTotal, Percentage, Percentage1) %>% 
  rbind(ungroup(tblAgeYear))

tblAgeYearG$AgeGroup <-factor(tblAgeYearG$AgeGroup, ageOrder)
tblAgeYearG <-arrange(tblAgeYearG, Year,AgeGroup)
tblAgeYearG$GTotal<-NULL
tblAgeYearG$Percentage1<-NULL
tblAgeYear$GTotal<-NULL

tblAgeRace <- CTDrug %>% 
  select(AgeGroup, Race, Year) %>% 
  group_by(AgeGroup, Race, Year) %>%
  summarise(Total=n()) %>% ungroup()

tblAgeRace$Race <- factor(tblAgeRace$Race, raceOrder)
tblAgeRace$AgeGroup <- factor(tblAgeRace$AgeGroup, ageOrder)

tblAgeDrug <-melt(CTDrug, id.vars = c("AgeGroup", "Year"), measure.vars = drugOrder)
colnames(tblAgeDrug)<-c('AgeGroup', 'Year', 'Drug', 'value')
tblAgeDrug$Drug <- factor(tblAgeDrug$Drug, drugOrderGG)
tblAgeDrug$AgeGroup <- factor(tblAgeDrug$AgeGroup, ageOrder)
tblAgeDrug <- tblAgeDrug %>% 
  filter(value=='Y') %>% 
  select(AgeGroup,Year,Drug) %>% 
  group_by(AgeGroup,Year,Drug) %>%
  summarise(Total=n()) %>% ungroup()


#Race Group
tblRace<-melt(CTDrug, id.vars = c("Race", "Year"), measure.vars = drugOrder)
colnames(tblRace)<-c('HeatGroup', 'Year', 'Drug', 'value')
tblRace$Drug <- factor(tblRace$Drug, drugOrderGG)
tblRace$HeatGroup <- factor(tblRace$HeatGroup, raceOrder)
tblRace <- tblRace %>% filter(value=='Y') %>% 
  select(HeatGroup,Year,Drug) %>% 
  group_by(HeatGroup,Year,Drug) %>% 
  summarise(Usage=n()) %>% ungroup()

tblRace <- tblRace %>% group_by(HeatGroup,Year) %>% mutate(Percentage = round(Usage*100/sum(Usage),2))

tblRaceYear <- CTDrug %>% 
  group_by(Year) %>% 
  summarise(GTotal = n()) %>% 
  inner_join(CTDrug) %>% 
  select(Race, Year, GTotal) %>% 
  group_by(Race, Year, GTotal) %>% 
  summarise(Total = n()) %>%
  select(Race,Year,Total, GTotal) %>% 
  mutate(Percentage = paste0(round(Total/GTotal*100,2),'%'), Percentage1 = round(Total/GTotal*100,2))

tblRaceYearG <- tblRaceYear %>% ungroup() %>% select(Year, Total = GTotal) %>% unique() %>% 
  mutate(Race='Grand Total', GTotal = 0, Percentage='', Percentage1=0) %>% 
  select(Race, Year, Total, GTotal, Percentage, Percentage1) %>% 
  rbind(ungroup(tblRaceYear))

tblRaceYearG$Race <-factor(tblRaceYearG$Race, raceOrder)
tblRaceYearG <-arrange(tblRaceYearG, Year,Race)
tblRaceYearG$GTotal<-NULL
tblRaceYearG$Percentage1<-NULL
tblRaceYear$GTotal<-NULL

tblRaceDrug <-melt(CTDrug, id.vars = c("Race", "Year"), measure.vars = drugOrder)
colnames(tblRaceDrug)<-c('Race', 'Year', 'Drug', 'value')
tblRaceDrug$Drug <- factor(tblRaceDrug$Drug, drugOrderGG)
tblRaceDrug$AgeGroup <- factor(tblRaceDrug$Race, raceOrder)
tblRaceDrug <- tblRaceDrug %>% 
  filter(value=='Y') %>% 
  select(Race,Year,Drug) %>% 
  group_by(Race,Year,Drug) %>%
  summarise(Total=n()) %>% ungroup()

#Gender Group
tblGender<-melt(CTDrug, id.vars = c("Sex", "Year"), measure.vars = drugOrder)
colnames(tblGender)<-c('HeatGroup', 'Year', 'Drug', 'value')
tblGender$Drug <- factor(tblGender$Drug, drugOrderGG)
tblGender$HeatGroup <- factor(tblGender$HeatGroup, genderOrder)
tblGender <- tblGender %>% filter(value=='Y') %>% 
  select(HeatGroup,Year,Drug) %>% 
  group_by(HeatGroup,Year,Drug) %>% 
  summarise(Usage=n()) %>% ungroup()

tblGender <- tblGender %>% group_by(HeatGroup,Year) %>% mutate(Percentage = round(Usage*100/sum(Usage),2))

tblSexYear <- CTDrug %>% 
  group_by(Year) %>% 
  summarise(GTotal = n()) %>% 
  inner_join(CTDrug) %>% 
  select(Sex, Year, GTotal) %>% 
  group_by(Sex, Year, GTotal) %>% 
  summarise(Total = n()) %>%
  select(Sex,Year,Total, GTotal) %>% 
  mutate(Percentage = paste0(round(Total/GTotal*100,2),'%'), Percentage1 = round(Total/GTotal*100,2))

tblSexYearG <- tblSexYear %>% ungroup() %>% select(Year, Total = GTotal) %>% unique() %>% 
  mutate(Sex='Grand Total', GTotal = 0, Percentage='', Percentage1=0) %>% 
  select(Sex, Year, Total, GTotal, Percentage, Percentage1) %>% 
  rbind(ungroup(tblSexYear))

tblSexYearG$Sex <-factor(tblSexYearG$Sex, genderOrder)
tblSexYearG <-arrange(tblSexYearG, Year, Sex)
tblSexYearG$GTotal<-NULL
tblSexYearG$Percentage1<-NULL
tblSexYear$GTotal<-NULL

tblDrugYear <- CTDrug %>% 
  dplyr::select(Year, Heroin,Cocaine,Fentanyl,Oxycodone,Oxymorphone,EtOH,Hydrocodone,
                Benzodiazepine,Amphet,Tramad,Morphine,Opioid,Other) %>% 
  melt(id.vars = c('Year'), measure.vars = drugOrder)

colnames(tblDrugYear) <- c('Year','Drug','Usage')
tblDrugYear <- tblDrugYear %>% filter(tblDrugYear$Usage == 'Y')

tblDrugYear <- tblDrugYear %>% 
  group_by(Year) %>% 
  summarise(GTotal = n()) %>% 
  inner_join(tblDrugYear) %>% 
  select(Drug, Year, GTotal) %>% 
  group_by(Drug, Year, GTotal) %>% 
  summarise(Total = n()) %>%
  select(Drug,Year,Total, GTotal) %>% 
  mutate(Percentage = paste0(round(Total/GTotal*100,2),'%'), Percentage1 = round(Total/GTotal*100,2))

tblDrugYear <- ungroup(tblDrugYear)
tblDrugYear$GTotal<-NULL
tblDrugYear$GTotal<-NULL

tblDrugYear$Drug <- factor(tblDrugYear$Drug, drugOrderGG)
tblDrugYearGG <- arrange(tblDrugYear, Year, Drug)

tblDrugYear$Drug <- factor(tblDrugYear$Drug, drugOrder)
tblDrugYear <- arrange(tblDrugYear, Year, Drug)
