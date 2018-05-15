library(dplyr)
library(tidyr)
library(stringr)
library(zipcode)
data("zipcode")

zipcode$city = toupper(zipcode$city)



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

CT_Drug$AgeClass <- cut(CT_Drug$Age, breaks = c(0, 15, 25, 35, 45, 55, 65, 75, 100), right = FALSE,
                        labels = c("A", "B", "C", "D", "E", "F", "G", "H"))

CT_Drug[CT_Drug == ''] <- NA
CT_Drug <- CT_Drug[complete.cases(CT_Drug),]

CT_Drug <- CT_Drug %>% separate(Race, c("Race"), ',', remove = T)
CT_Drug$Race <- ifelse(!CT_Drug$Race %in% c("White", "Hispanic", "Black"),"Other", CT_Drug$Race)

CT_Drug$DeathLoc <- toupper(CT_Drug$DeathLoc) 
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "WESTHAVEN","WEST HAVEN", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "NEWHAVEN","NEW HAVEN", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "NEWHARTFORD","NEW HARTFORD", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "NEWBRITAIN","NEW BRITAIN", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "NORTHCANAAN","NORTH CANAAN", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "NEWLONDON","NEW LONDON", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "EASTHARTFORD","EAST HARTFORD", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "NEWHARTFORD","NEW HARTFORD", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "NORTHBRANFORD","NORTH BRANFORD", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "JEWETTCITY","JEWETT CITY", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "NORTHHAVEN","NORTH HAVEN", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "NEWMILFORD","NEW MILFORD", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "NEWHARTFORD","NEW HARTFORD", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "EASTWINDSOR","EAST WINDSOR", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "NEWFAIRFIELD","NEW FAIRFIELD", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "WINDSORLOCKS","WIND SORLOCKS", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "STAFFORDSPRINGS","STAFFORD SPRINGS", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "DEEPRIVER","DEEP RIVER", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "EASTLYME","EAST LYME", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "SGLASTONBURY","GLASTONBURY", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "EASTHAMPTON","EAST HAMPTON", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "NORTHGROSVENORDALE","NORTH GROSVENORDALE", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "COSCOB","COS COB", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "NEWCANAAN","NEW CANAAN", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "EASTHAVEN","EAST HAVEN", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "NOHAVEN","NORTH HAVEN", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "WESTSTAFFORD","WEST STAFFORD", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "ROCKYHILL","ROCKY HILL", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "EASTGRANBY","EAST GRANBY", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "WESTHARTFORD","WEST HARTFORD", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "SOUTHWINDSOR","SOUTH WINDSOR", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "EASTHADDAM","EAST HADDAM", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "OLDSAYBROOK","OLD SAYBROOK", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "WHAVEN","WEST HAVEN", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "NHAVEN","NORTH HAVEN", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "STAFFORDSPGS","STAFFORD SPRINGS", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "NORTHGRANBY","NORTH GRANBY", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "EASTWOODSTOCK","EAST WOODSTOCK", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "EASTHARTLAND","EAST HARTLAND", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "NORTHSTONINGTON","NORTH STONINGTON", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "OLDLYME","OLD LYME", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "BROADBROOK","BROAD BROOK", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "GROTONLONG POINT","GROTON", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "BEACONFALLS","BEACON FALLS", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "GALESFERRY","GALES FERRY", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "QUAKERHILL","QUAKER HILL", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "WESTSUFFIELD","WEST SUFFIELD", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "VERNON-ROCKVILLE","VERNON", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "06340","GROTON", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "NEWPRESTON","NEW PRESTON", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "CORNWALLBRIDGE","CORNWALL BRIDGE", CT_Drug$DeathLoc)
CT_Drug$DeathLoc <- ifelse(CT_Drug$DeathLoc == "STORRS","MANSFIELD", CT_Drug$DeathLoc)

CT_Loc <- zipcode %>% filter(state=="CT") %>% select(city, zip)  %>% group_by(city) %>% top_n(1)
CT_Zip <- CT_Drug %>% select(DeathLoc) %>% unique()
CT_ZipMatch <- CT_Zip %>% left_join(CT_Loc, by=c("DeathLoc"="city"))

CT_ZipMatch$zip <- ifelse(CT_ZipMatch$DeathLoc == "NORTH CANAAN","06018",CT_ZipMatch$zip)
CT_ZipMatch$zip <- ifelse(CT_ZipMatch$DeathLoc == "BAKERSVILLE","06057",CT_ZipMatch$zip)
CT_ZipMatch$zip <- ifelse(CT_ZipMatch$DeathLoc == "BARK HAMSTED","06063",CT_ZipMatch$zip)
CT_ZipMatch$zip <- ifelse(CT_ZipMatch$DeathLoc == "BERLIN","06023",CT_ZipMatch$zip)
CT_ZipMatch$zip <- ifelse(CT_ZipMatch$DeathLoc == "VERNON","06066",CT_ZipMatch$zip)
CT_ZipMatch$zip <- ifelse(CT_ZipMatch$DeathLoc == "WINCHESTER","06094",CT_ZipMatch$zip)
CT_ZipMatch$zip <- ifelse(CT_ZipMatch$DeathLoc == "WIND SORLOCKS","06096",CT_ZipMatch$zip)
CT_ZipMatch$zip <- ifelse(CT_ZipMatch$DeathLoc == "MANSFIELD","06235",CT_ZipMatch$zip)
CT_ZipMatch$zip <- ifelse(CT_ZipMatch$DeathLoc == "LISBON","06351",CT_ZipMatch$zip)
CT_ZipMatch$zip <- ifelse(CT_ZipMatch$DeathLoc == "ROCKVILLE","06066",CT_ZipMatch$zip)
CT_ZipMatch$zip <- ifelse(CT_ZipMatch$DeathLoc == "WEST STAFFORD","06076",CT_ZipMatch$zip)
CT_ZipMatch$zip <- ifelse(CT_ZipMatch$DeathLoc == "FRANKLIN","06254",CT_ZipMatch$zip)
CT_ZipMatch$zip <- ifelse(CT_ZipMatch$DeathLoc == "KILLINGLY","06239",CT_ZipMatch$zip)
CT_ZipMatch$zip <- ifelse(CT_ZipMatch$DeathLoc == "WEST HARTFORD","06107",CT_ZipMatch$zip)
CT_ZipMatch$zip <- ifelse(CT_ZipMatch$DeathLoc == "BARKHAMSTED","06063",CT_ZipMatch$zip)
CT_ZipMatch$zip <- ifelse(CT_ZipMatch$DeathLoc == "SPRAGUE","06330",CT_ZipMatch$zip)
CT_ZipMatch$zip <- ifelse(CT_ZipMatch$DeathLoc == "GRISWOLD","06351",CT_ZipMatch$zip)
CT_ZipMatch$zip <- ifelse(CT_ZipMatch$DeathLoc == "WARREN","06754",CT_ZipMatch$zip)
CT_ZipMatch$zip <- ifelse(CT_ZipMatch$DeathLoc == "NEW PRESTON","06777",CT_ZipMatch$zip)

CT_Drug <- CT_Drug %>% inner_join(CT_ZipMatch)

write.csv(CT_Drug, file = "D:/CUNY/608/FinalProject/Data608-FinalProject/CT_Drug.csv",row.names=FALSE)

