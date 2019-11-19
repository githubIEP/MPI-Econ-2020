library(dplyr)
library(tidyr)
library(iep2)
library(stringr)
library(WDI)
library(readr)
library(openxlsx)
library(tidyverse)
library(xlsx)
library(readxl)


#________________________________________________EXCHANGE RATE_______________________________________________________________________________



#"PA.NUS.FCRF"          Official exchange rate (LCU per US$, period average)

#"PX.REX.REER"         Real effective exchange rate index (2010 = 100)


# prepare exchage from USD to mex pesos
exchange <- WDI::WDI(country = "all", indicator ="PA.NUS.FCRF", start = 2000, end = 2018)%>%
  subset(iso2c=="US" | iso2c=="GB" | iso2c=="MX") %>%
  select(-iso2c) %>%  spread(country,PA.NUS.FCRF) %>% mutate(year=year+1)



#_________________________________________________________GDP SCALE_________________________________________________________________________________________________

# GDP SCALE -> NY.GDP.PCAP.PP.CD -> GDP per capita, PPP (current international $)

gdpscale <- WDI::WDI(country = "all", indicator ="NY.GDP.PCAP.PP.CD", start = 2000, end = 2018)%>%
  subset(iso2c=="US" | iso2c=="GB" | iso2c=="MX")  %>%
  select(-iso2c) %>%  spread(country,NY.GDP.PCAP.PP.CD) %>% mutate(year=year+1)


for(i in 1:nrow(gdpscale)){
  gdpscale[i,'us.scale'] <- gdpscale[i,"Mexico"]/gdpscale[i, "United States"]
  gdpscale[i,'uk.scale'] <- gdpscale[i,"Mexico"]/gdpscale[i, "United Kingdom"]
}

gdpscale <- gdpscale[,c("year", "us.scale","uk.scale")]


### exported to excel to scale unitcosts
#write.csv(gdpscale, file = "C:/Users/hbardwell/Documents/Github/econmpi18/data/gdpscale.csv")








#_________________________________________________________CONSUMER PRICE INDEX____________________________________________________________________________________


# Consumer price index (2010 = 100) -> FP.CPI.TOTL

CPI <- WDI::WDI(country = "all", indicator ="FP.CPI.TOTL", start = 2000, end = 2018)%>%
  subset(iso2c=="MX" | iso2c=="US" | iso2c=="GB") %>% 
  select(-iso2c) %>% 
  spread(country,FP.CPI.TOTL)

CPI <- CPI %>% mutate(CPI_factor_Mex = tail(CPI$Mexico, 1)/Mexico) 
CPI <- CPI %>% mutate(CPI_factor_US = tail(CPI$`United States`, 1)/`United States`)
CPI <- CPI %>% mutate(CPI_factor_US = tail(CPI$`United Kingdom`, 1)/`United Kingdom`)
CPI <-  CPI %>% mutate(ratio_US = Mexico/`United States`)
CPI <-  CPI %>% mutate(ratio_UK = Mexico/`United Kingdom`)

# change year
CPI[,1] <- CPI[,1] + 1


#exchange to real terms
CPI1 <- CPI %>% select(c("year","ratio_US","ratio_UK"))

exchange <- left_join(exchange, CPI1, by="year")
exchange <- exchange %>% mutate(real_peso_US = Mexico / ratio_US, real_peso_UK = Mexico / ratio_UK)

rm(CPI1)





#_________________________________________________________GDP DEFLATOR_____________________________________________________________________________


#GDP deflator used is the GDP deflator (base year varies by country) -> NY.GDP.DEFL.ZS

deflator <- WDI::WDI(country = "all", indicator ="NY.GDP.DEFL.ZS", start = 2000, end = 2018)%>%
  subset(iso2c=="US" | iso2c=="GB" | iso2c=="MX") %>%
  select(-iso2c) %>% 
  spread(country,NY.GDP.DEFL.ZS)

deflator <- deflator %>% mutate(deflator_factor = tail(deflator$Mexico, 1)/Mexico) 


# change year
deflator[,1] <- deflator[,1] + 1

#delete out Uk and USA
deflator <- deflator[,-3:-4]


#_______________________________________________Merge the CPI and the GDP deflator ->  CPI_DEFLATOR MERGE______________________________________________________________________


#deflator<- subset(deflator, year>2014)

CPI_deflator <- merge(CPI, deflator,
                      by = "year" , all.y=TRUE)
names(CPI_deflator)
CPI_deflator <-  CPI_deflator %>% select(year, CPI_factor_Mex,deflator_factor)


colnames(CPI_deflator)[2] <- "Mexico_CPI"
colnames(CPI_deflator)[3] <- "Mexico_deflator"



#######_____________________________________________ update unit costs_____________________________________________


#unit costs
unit_cost <-rio::import("data/unit cost.csv")
unit_cost <- unit_cost %>% left_join(exchange, by="year") %>%
  mutate(cost = cost * real_peso_US)  %>%
  left_join(CPI_deflator, by="year")%>% 
  mutate(cost = cost* Mexico_CPI) %>% 
  select(c("indicator", "type","year","cost"))



unit_cost_fear <- read_csv("data/unit cost fear.csv")
unit_cost_fear <- unit_cost_fear %>% left_join(exchange, by="year") %>%
  mutate(cost = cost * real_peso_UK) %>%
  left_join(CPI_deflator, by="year") %>% 
  mutate(cost = cost* Mexico_CPI) %>% 
  select(c("indicator", "type","year","cost"))


unit_cost <- unit_cost %>% rbind(unit_cost_fear)
rm(unit_cost_fear)


years <- data.frame(2000:2019)
names(years)
years <- rename(years, year="X2000.2019")
unit_cost$indicator  <-with(unit_cost, paste0(unit_cost$indicator," - ",unit_cost$type))
unit_cost <- unit_cost %>% select(-c(year,type))
unit_cost <- merge(years, unit_cost)



unit_cost <- merge(gdpscale,unit_cost,by="year")
unit_cost <- spread(unit_cost, indicator, cost)

##check fear is not scaled with the us then uk
for (i in 1:nrow(unit_cost)){
  unit_cost[i,c(4:13)] <- unit_cost[i,c(4:13)]* unit_cost[i,"us.scale"]
  unit_cost[i, "Fear of crime - indirect"] <- unit_cost[i, "Fear of crime - indirect"] * unit_cost[i,"uk.scale"]
}

unit_cost <- unit_cost %>% select(-c("us.scale","uk.scale"))
unit_cost <- unit_cost %>% gather(indicator, cost, -c(year)) %>% na.omit(unit_cost)




#____________________________________________________________POPULATION______________________________________________________________________________________________

pop <- read_csv("data/population.data.mexico.csv")%>% 
  subset(year>2006 & year< 2020) %>% 
  subset(select=c(code,year,indicator,value)) %>% 
  subset(indicator=="total population") %>% 
  subset(code!="Nat")

nat <- pop %>% group_by(year) %>%
  summarise(national=sum(value)) %>% 
  ungroup()

pop <- full_join(pop,nat, by="year") %>% 
  mutate(pop_weight=value/national)


statecodes <- read.csv("data/mpi.state.codes.csv") %>% 
  na.omit() %>% select(-X)

rm(nat)

unit_cost_incar <- read_excel("data/unit cost incar.xlsx", 
                              sheet = "unit.cost.incar")

unit_cost_incar$indicator  <-with(unit_cost_incar, paste0(unit_cost_incar$indicator," - ",unit_cost_incar$type))

unit_cost_incar <- unit_cost_incar %>%  select(-type) %>%
  left_join(CPI_deflator, by="year") %>% 
  mutate(cost = cost* Mexico_CPI) %>% 
  select(c("indicator","year","cost"))


unit_cost <- rbind(unit_cost, unit_cost_incar)

#_________________________________________________________Mexico_government_expenditures____________________________________________________________________________________
#_________________________                   Justice            Defense        P&S                      _______________________________________________________________________________

#2018 was calculated in the spreadsheet and then the spreadsheetis uploaded.
getwd()
Mex_gov_exp <- read_excel("Mexico governement expenditures.xlsx", 
                          sheet = "FASP")

Mex_gov_exp[,c(2:14)] <- lapply(Mex_gov_exp[,c(2:14)], function(x)as.numeric( as.character(x )))  

Mex_gov_exp <- Mex_gov_exp %>% gather(year, value,-indicator) %>%
  mutate(year=as.numeric(year)) %>% 
  full_join(CPI_deflator, by="year")


Mex_gov_exp[,"valueconstant"] <- Mex_gov_exp[,"value"]*Mex_gov_exp[,"Mexico_CPI"]

Mex_gov_exp <- Mex_gov_exp %>% 
  subset(select=-c(value,Mexico_CPI,Mexico_deflator)) %>% 
  spread(indicator,valueconstant) %>%
  mutate(year=as.numeric(as.character(year))) %>%
  subset(year>2006 & year< 2020) %>% select(-`<NA>`) 




#FASP weight calc, 

mex_names <- read_excel("config/names of states v2 with code.xlsx", 
                        sheet = "codes")

tmp <- read_csv("data/FASP raw.csv", 
                   skip = 2)
tmp <- tmp[-c(34:50),]
tmp <- tmp %>%  rename(state = X1)
tmp[16,1] <- "México"
tmp[10,1] <- "Distrito Federal"
tmp[20,1] <- "Nuevo León"
tmp[23,1] <- "Querétaro"
tmp[25,1] <- "San Luis Potosí"
tmp[32,1] <-"Yucatán"
tmp <- tmp %>%  left_join(mex_names, by="state")
tmp[,c(2:14)] <- lapply(tmp[,c(2:14)], function(x)as.numeric( as.character(x )))
tmp$state.long[is.na(tmp$state.long)] <- "national"
tmp$code[is.na(tmp$code)] <- "nat"


tmp <- tmp %>% select(-state) %>% 
  subset(select=-state.long) %>% 
  gather(year,fasp, -(code))

#develop national totals to make weights
nat <- tmp %>% 
  subset(code=="nat")

nat <- nat %>% select(-code)

tmp <- tmp %>% left_join(nat, by=c("year")) %>%
  mutate(fasp.weight=fasp.x/fasp.y) %>%
  subset(code!="nat") %>% select(-fasp.x,-fasp.y)

rm(nat)



####
govtexp <- data.frame(expand.grid(code=statecodes[,"code"], year=2007:2019))
govtexp <- full_join(govtexp, Mex_gov_exp, by="year")

govtexp <- govtexp %>% 
  merge(tmp[,c("year","code","fasp.weight")], by=c("year","code")) %>%
  merge(pop[,c("year","code","pop_weight")], by=c("year","code")) %>% 
  mutate(defense1=(Defense/2)*fasp.weight, justice1=(Justice/2)*fasp.weight,
         domsecu1=(`Public Order and Domestic Safety`/2)*fasp.weight) %>% 
  mutate(defense2=(Defense/2)*pop_weight, justice2=(Justice/2)*pop_weight,
         domsecu2=(`Public Order and Domestic Safety`/2)*pop_weight) %>% 
  mutate(defense=defense1+defense2, justice=justice1+justice2, domsecu=domsecu1+domsecu2)

govtexp <- govtexp[,c("code","year","defense","justice","domsecu")]


#round to minimise errors
govtexp[3:5] <-  round(govtexp[3:5], digits = 4)










#_____________________________________ENVE CPI ADJUSTMENT_______________________________________________________________________________

##updated every two years #update in 2020 MPI

ENVE <- read_excel("data/data - old/ENVE - Losses due to insecurity.xlsx", sheet = "all") %>% subset(year>2010)
ENVE[2:5] <- lapply(ENVE[2:5], function(x) {as.numeric(as.character(x))})
ENVE <- rename(ENVE, state=State)


ENVE <- left_join(ENVE,CPI_deflator, by="year") %>% 
  mutate(adj_protcost=`Spending on measures to protect from crime`* Mexico_CPI) %>% 
  mutate(adj_crimelosses=`Losses as a result of crime`*Mexico_CPI) %>% 
  mutate(adj_totalcost=`Costs of crime` * Mexico_CPI)

ENVE <- left_join(ENVE, mex_names, by = "state")


ENVE <- ENVE[c(12,2,8:10)]


########################################################################################

#_____________________________________ENVIPE CRIME COSTS + CPI ADJUSTMENT_______________________________________________________________________________

setwd("~/Github/MPI Econ 2020/data/ENVIPE")
filelist <- list.files(pattern = "\\.xlsx")

tmp = read_excel("V_percepcion_seguridad_2017_est (1).xlsx", 
                                sheet = "5.4", col_names = FALSE, skip = 9)

tmp <- tmp %>%  rename(state="...1", value= "...2")







ENVIPE <- read_excel("~/Github/MPI Econ 2020/data/data - old/cleaned CrimeCostSheet4-22-2014-17.xlsx", sheet = "all") %>%  
  subset(State!="Country mexico") %>% rename(year = Year)  %>% mutate(year = year + 1)

ENVIPE <- ENVIPE[,c(1,2,4,5,6)]

colnames(ENVIPE)[3] <- "protect_cost"
colnames(ENVIPE)[4] <- "losses_from_crime_cost"
colnames(ENVIPE)[5] <- "health_cost"

#subset the CPI multiplier to allow for the joining of the dataframes
ENVIPE <- full_join(ENVIPE,CPI_deflator, by="year") %>% 
  mutate(adj_protcost=protect_cost*Mexico_CPI) %>% 
  mutate(adj_crimelosses=losses_from_crime_cost*Mexico_CPI) %>% 
  mutate(adj_healthcost=health_cost* Mexico_CPI)

ENVIPE <- rename(ENVIPE, state=State)
#CPI adds in 2018, delete this out
ENVIPE <- left_join(ENVIPE, mex_names, by = "state")

ENVIPE <- ENVIPE[c(12,2:10)]


#_____________________________________ENVIPE fear_______________________________________________________________________________



all_crime <- full_join(ENVIPE, ENVE, by = c("code","year"))


all_crime <- all_crime[-c(6,12,13,16)]
all_crime <- all_crime %>% mutate(type = "direct")
all_crime <- all_crime[c(1,2,13,3:12)]


colnames(all_crime)[4] <- "protect_cost_ENVIPE"
colnames(all_crime)[6] <- "health_cost_ENVIPE"
colnames(all_crime)[5] <- "losses_from_crime_cost_ENVIPE"



colnames(all_crime)[10] <- "protect_cost_ENVE"
colnames(all_crime)[11] <- "losses_from_crime_cost_ENVE"

all_crime <- all_crime %>%mutate(protect_cost = protect_cost_ENVIPE + protect_cost_ENVE) %>% 
  mutate(losses_from_crime = health_cost_ENVIPE + losses_from_crime_cost_ENVIPE + losses_from_crime_cost_ENVE)


all_crime <- all_crime[-c(4:13)]


all_crime <- gather(all_crime,subtype,value,-c(code,year,type)) 
all_crime <- full_join(all_crime, CPI_multiplier3, by = "year")
all_crime <-  all_crime %>% mutate(value = value * CPI_factor)
all_crime <- all_crime[,c(1:5)]

########################################################################################


#_____________________________________ENVIPE fear_______________________________________________________________________________


#upload fear data for the years, covert spanish text to english and insert year column 

envpe2014 <- read.csv("data/data - old/ENVPE 2014.csv",  encoding="latin1") %>% 
  select(state, Inseguro) %>%    mutate(year = 2014)

envpe2015 <- read.csv("data/data - old/ENVPE 2015.csv", encoding="latin1") %>% 
  select(state, Inseguro) %>%       mutate(year = 2015)

envpe2016 <- read.csv("data/data - old/ENVPE2016.csv", encoding="latin1") %>% 
  select(state, Inseguro) %>%       mutate(year = 2016)

envpe2017 <- read.csv("data/data - old/ENVPE2017.csv", encoding="latin1") %>% 
  select(state, Inseguro) %>%      mutate(year = 2017)

envpe2018 <- read.csv("data/data - old/ENVPE2018.csv", encoding="latin1") %>% 
  select(state, Inseguro)  %>% mutate(year = 2018)

envpe2019 <- read.csv("data/envpe2019.csv",encoding="latin1") %>% 
  select(state, Inseguro)  %>% mutate(year = 2019)

#Add all files into one dataframe
fear <- bind_rows(envpe2014, envpe2015, envpe2016,envpe2017, envpe2018, envpe2019)

fear <- fear %>% left_join(mex_names, by="state") %>% 
  select(-state, -state.long)


rm(envpe2014, envpe2015, envpe2016, envpe2017, envpe2018, envpe2019)


fear <- rename(fear, value=Inseguro)
fear[,'subtype'] <- "fear"
fear <- fear %>% mutate(value=value/100)

fear <- merge(fear, pop, by=c("code","year"))
fear <- fear %>% mutate(value.x=value.x*value.y) %>% rename(fear=value.x)
fear <- fear[,c("code","year","fear","subtype")]

#_____________________________________Remaining projects_______________________________________________________________________________
# incarceration - minimum wage from mexico and the incarceration numbers
setwd("~/Github/MPI Econ 2020")


incarceration <- read_csv("data/incarceration MPI 2019.csv")
incarceration[15,1] <- "México"
incarceration[16,1] <- "Michoacán de Ocampo"
incarceration[19,1] <- "Nuevo León"
incarceration[22,1] <- "Querétaro"
incarceration[24,1] <- "San Luis Potosí"
incarceration[31,1] <-"Yucatán"
"check

incarceration <- left_join(incarceration, mex_names, by = "state")
incarceration <-  incarceration %>%  select(-state)


incarceration <- incarceration %>% gather(year, value,-c(code,state.long))
tmp <- unit_cost %>% subset(indicator=="minimumannualwage - indirect")

incarceration <- mutate(incarceration, year=as.numeric(year))
incarceration <- left_join(tmp,incarceration, by="year")
incarceration <- incarceration %>% subset(indicator=="minimumannualwage - indirect") %>% na.omit()


incarceration <- mutate(incarceration, value=as.numeric(value))


incarceration <- incarceration %>% mutate(incar=value*cost*0.6)

incar.cost <- incarceration[,c("code","year","incar")]
incar.cost[,'subtype'] <- "incarceration"
incar.cost  <- rename(incar.cost, value=incar)
incar.cost <- mutate(incar.cost, type = "indirect")

#make year numeric
incar.cost <-  incar.cost %>% mutate(year=as.numeric(as.character(year)))




all_crime <- read_csv("data/allcrime2018 MPI.csv", 
                                          col_types = cols(value = col_number(), 
                                                           year = col_number()))


all_crime <- all_crime %>%  select(-X1)

#_____________________________________adding costs all together_______________________________________________________________________________


#upload and take out unnecessary columns 

mpidata <- read_csv("data/MPI 2019 crime data2.csv") %>%
  subset(type=="raw")%>% subset(indicator!="firearms.crime")%>% subset(subtype!="organized.crime") %>% 
  subset(subtype!="retail.drug.sales") %>%  subset(subtype!="family.violence")
table(mpidata$subtype)

mpidata$subtype[mpidata$indicator=="homicide"] <- "homicide"

#delete unneeded variables

mpidata <- mpidata %>%subset(subtype!="total") %>%
  subset(subtype!="narcotics")

#create list for only wanted states - this will ensure that additional states or codes are deleted 

pos= c("AGU","BCN", "BCS" , "CAM", "CHH", "CHP", "COA", "COL", "DIF","DUR", "GUA",
       "GRO","HID","JAL","MEX","MIC","MOR","NAY","NLE","OAX","PUE","QUE" , "ROO",
       "SIN",  "SLP", "SON", "TAB", "TAM","TLA", "VER","YUC","ZAC")


mpidata2 <- mpidata[,c("code","year","value","subtype")]
unit_cost2 = unit_cost
unit_cost=unit_cost2
# unit_cost <- separate(unit_cost, "indicator", into = c("indicator","type"), sep = " - ")
unit_cost$indicator <- tolower(unit_cost$indicator )


unit_cost <- unit_cost %>% spread(indicator,cost) %>% subset(year>2014)
mpidata2 <- mpidata2 %>% spread(subtype,value) %>% merge(unit_cost,by="year")

# the multipliers are used to deduct govt costs

mpi.cost <- mpidata2[mpidata2$code %in% pos,]

mpidata2 <- mpi.cost
for(i in 1:nrow(mpi.cost)){
  #homicide
  mpidata2[,"homicide.direct"] <-  mpidata2[,"homicide"]*mpidata2[,"homicide - direct"]*0.69
  mpidata2[,"homicide.indirect"] <- mpidata2[,"homicide"]*mpidata2[,"homicide - indirect"]
  # Assualt
  mpidata2[,"assault.direct"] <- mpidata2[,"assault"]*mpidata2[,"assault - direct"]*0.45
  mpidata2[,"assault.indirect"] <- mpidata2[,"assault"]*mpidata2[,"assault - indirect"]
  # rape
  mpidata2[,"rape.direct"] <- mpidata2[,"sexual.assault"]*mpidata2[,"rape - direct"]*0.36
  mpidata2[,"rape.indirect"] <- mpidata2[,"sexual.assault"]*mpidata2[,"rape - indirect"]
  # extortion
  mpidata2[,"extortion.direct"] <- mpidata2[,"extortion"]*mpidata2[,"robbery - direct"]*0.15
  mpidata2[,"extortion.indirect"] <- mpidata2[,"extortion"]*mpidata2[,"robbery - indirect"]
  # robbery
  mpidata2[,"robbery.direct"] <- mpidata2[,"robbery"]*mpidata2[,"robbery - direct"]*0.15
  mpidata2[,"robbery.indirect"] <- mpidata2[,"robbery"]*mpidata2[,"robbery - indirect"]
  # kidnapping
  mpidata2[,"kidnapping.direct"] <- mpidata2[,"kidnapping.and.human.trafficking"]*mpidata2[,"assault - direct"]*0.45
  mpidata2[,"kidnapping.indirect"] <- mpidata2[,"kidnapping.and.human.trafficking"]*mpidata2[,"assault - indirect"]
}


mpi.cost <- mpidata2[,c(1:2,20:31)]

mpi.cost <- gather(mpi.cost,subtype,value,-c(code,year))
mpi.cost <- mpi.cost %>% 
  separate(subtype, c("subtype", "type"), "\\.")




#---------------------------------------------------------------------------------------



