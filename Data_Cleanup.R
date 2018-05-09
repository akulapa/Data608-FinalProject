library(dplyr)
library(tidyr)
library(stringr)
CT_Drug <- read.csv("https://raw.githubusercontent.com/akulapa/Data608-FinalProject/master/Accidental_Drug_Related_Deaths__2012-2017.csv", header= TRUE, stringsAsFactors = F)
CT_Drug$DeathLoc <- str_replace(CT_Drug$DeathLoc, "\r\n", "")
CT_Drug$DeathLoc <- str_replace(CT_Drug$DeathLoc, "\tab", "")
CT_Drug$DeathLoc <- str_replace(CT_Drug$DeathLoc, "\n", "")
CT_Drug <- CT_Drug %>% separate(DeathLoc, c("DeathLoc", "Latitude"), '\\(')
CT_Drug <- CT_Drug %>% separate(Latitude, c("Latitude", "Longitude"), '\\,')
CT_Drug <- CT_Drug %>% separate(DeathLoc, c("DeathLoc", "DeathState"), '\\,')

CT_Drug <- CT_Drug %>% separate(Date, c("Month", "Day", "Year"), '\\/', remove = F)

CT_Drug$Longitude <- str_replace(CT_Drug$Longitude, "\\)", "")
CT_Drug$Longitude <- str_replace(CT_Drug$Longitude, " ", "")
CT_Drug$Latitude <- str_replace(CT_Drug$Latitude, " ", "")
CT_Drug$DeathLoc <- str_replace(CT_Drug$DeathLoc, " ", "")
CT_Drug$DeathState <- str_replace(CT_Drug$DeathState, " ", "")
CT_Drug$Longitude <- as.numeric(CT_Drug$Longitude)
CT_Drug$Latitude <- as.numeric(CT_Drug$Latitude)

names(CT_Drug)[names(CT_Drug) == 'Morphine..not.heroin.'] <- 'Morphine'
names(CT_Drug)[names(CT_Drug) == 'Any.Opioid'] <- 'Opioid'

CT_Drug$Morphine = ifelse(toupper(CT_Drug$Other) %in% c('MORPHINE'),'Y',toupper(CT_Drug$Morphine))
CT_Drug$Other = ifelse(toupper(CT_Drug$Other) %in% c('MORPHINE'),'N',toupper(CT_Drug$Other))

CT_Drug <- CT_Drug %>% 
  select(Year, Sex, Race, Heroin, Cocaine, Fentanyl, Oxycodone, DeathLoc, DeathState, Longitude, Latitude,
         Oxymorphone, EtOH, Hydrocodone, Benzodiazepine,
         Amphet, Tramad, Morphine, Other, Opioid, Age) %>% 
  mutate(Heroin = ifelse(!Heroin %in% c('Y','y'),'N',toupper(Heroin)),
         Cocaine = ifelse(!Cocaine %in% c('Y','y'),'N',toupper(Cocaine)),
         Fentanyl = ifelse(!Fentanyl %in% c('Y','y'),'N',toupper(Fentanyl)),
         Oxycodone = ifelse(!Oxycodone %in% c('Y','y'),'N',toupper(Oxycodone)),
         Oxymorphone = ifelse(!Oxymorphone %in% c('Y','y'),'N',toupper(Oxymorphone)),
         EtOH = ifelse(!EtOH %in% c('Y','y'),'N',toupper(EtOH)),
         Hydrocodone = ifelse(!Hydrocodone %in% c('Y','y'),'N',toupper(Hydrocodone)),
         Benzodiazepine = ifelse(!Benzodiazepine %in% c('Y','y'),'N',toupper(Benzodiazepine)),
         Amphet = ifelse(!Amphet %in% c('Y','y'),'N',toupper(Amphet)),
         Tramad = ifelse(!Tramad %in% c('Y','y'),'N',toupper(Tramad)),
         Morphine = ifelse(!Morphine %in% c('Y','y'),'N',toupper(Morphine)),
         Other = ifelse(Other %in% c('',' '),'N','Y'),
         Opioid = ifelse(!Opioid %in% c('Y','y'),'N',toupper(Opioid))
  )

CT_Drug$AgeGroup <- cut(CT_Drug$Age, breaks = c(0, 15, 25, 35, 45, 55, 65, 75, 100), right = FALSE,
                       labels = c("<15", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", ">74"))

CT_Drug[CT_Drug == ''] <- NA
CT_Drug <- CT_Drug[complete.cases(CT_Drug),]

CT_Drug <- CT_Drug %>% separate(Race, c("Race"), ',', remove = T)
CT_Drug$Race <- ifelse(!CT_Drug$Race %in% c("White", "Hispanic", "Black"),"Other", CT_Drug$Race)

write.csv(CT_Drug, file = "D:/CUNY/608/FinalProject/Data608-FinalProject/CT_Drug.csv",row.names=FALSE)
