library(data.table)
library(ggplot2)
library(gridExtra)
library(stringr)
library(dplyr)
library(tidyr)
Sys.setlocale('LC_ALL','C')

patents<-fread('20170914_Patents_MSA09-All_Class-All_2000-2015.csv')
demographics<-fread('msa10_population_centroid.csv')
MSAs<-demographics[demographics$cbsaname10 %like% "Metropolitan Statistical Area",]

#Get # of patents by year
byyear<-patents[,.(sum(num_patents)), by=.(year, msa_name09)]
byyear$end<-byyear$year+1
byyear<-byyear[order(byyear$msa_name09),]
byyear<-byyear[,c(1,4,2,3)]
write.csv(byyear, "/users/junpingshi/Downloads/Byyear.csv")

#For Category analysis of NY
NY<-patents[patents$msa_name09=="New York-Northern New Jersey-Long Island, NY-NJ-PA"]
NY<-aggregate(num_patents~year, NY, sum)
NY<-spread(NY, tech_cat,num_patents)
fwrite(NY, "NYPatents.csv")

#For Category analysis of Cahicago
Chicago<-patents[patents$msa_name09=="Chicago-Joliet-Naperville, IL-IN-WI"]
Chicago<-aggregate(num_patents~year+tech_cat, Chicago, sum)
Chicago<-aggregate(num_patents~year, Chicago, sum)
Chicago<-spread(Chicago, tech_cat,num_patents)
fwrite(Chicago, "ChicagoPatents.csv")



LA<-patents[patents$msa_name09=="San Jose-Sunnyvale-Santa Clara, CA"]
LA<-aggregate(num_patents~tech_cat, LA, sum)


Bos<-patents[patents$msa_name09=="Boston-Cambridge-Quincy, MA-NH"]
Bos<-aggregate(num_patents~tech_cat, Bos, sum)

Pitt<-patents[patents$msa_name09=="Pittsburgh, PA"]
Pitt<-aggregate(num_patents~tech_cat, Pitt, sum)

#Get Latitude
lats<-fread('Points.1571526555721.csv')
lats<-lats[,-c(1,4)]
lats<-lats[rep(seq_len(nrow(lats)), each=16),]
write.csv(lats, "/users/junpingshi/Downloads/latseq.csv")
longlat<-demographics[-1,c(2,3,4)]
patents<-patentspc[2]
write.csv(longlat, "/users/junpingshi/Downloads/Lat.csv")

#Patents per category
patentspc<-patentspc[-8,]
patentspc
unique(patents$tech_subcat)
unique(patents$msa_name09)

#Fix City names
patentsbyyear<-fread("PatentsByYear.csv")
cities<-unique(patentsbyyear$msa_name09)
patentsTotal<-fread("MSA+Patents.csv")
patentsTotal[,1]<-cities
MSAs[,2]<-MSAcities
fwrite(patentsTotal, "MSA+Patents.csv")
cities

#Shorten city names
MSAcities<-str_remove(MSAs$cbsaname10, " Metropolitan Statistical Area")
MSAs$cbsaname10<-MSAcities
MSAPatentcities<-str_remove(MSAPatent$`Area Name`, " Metropolitan Statistical Area")
MSAPatent$`Area Name`<-MSAPatentcities
setdiff(cities, MSAcities)


#Get population
MSAPatent<-fread("MSA+Patents.csv")
colnames(MSAs)<-c("Code", "Area Name", "Longitude", "Latitude", "Population", "Factor")
colnames(MSAs)
Final<-left_join(MSAPatent, MSAs, by="Area Name")
Final<-Final[,-c(5,6,7,9)]
Final$Population<-as.numeric(as.character(Final$Population))
Final<-transform(Final, PerCapita=Patent*10000/Population)
sapply(Final, typeof)
fwrite(Final, "MSA+PatentPop.csv")
left


plotCity <- function(cityname){
    city<-patents[patents$msa_name09==cityname]
    numpatents<-aggregate(num_patents~year, city, sum)
    ggplot(data=numpatents)+geom_point(aes(x=year, y=num_patents))+ggtitle(cityname)+xlab("Year")+ylab("Number of Patents")
}

cities<-unique(patents$msa_name09)
vapply(cities, plotCity)
plotCity("Wichita, KS")
cities

cumulative<-fread("Cumulative.csv")
a<-464
b<-479
cumulative[,6]<-cumsum(cumulative[a:b, 6])
cumulative2<-fread("Cumulative.csv")
cumulative[,6]<-ave(cumulative$V1, cumulative$msa_name09, FUN=cumsum)
fwrite(cumulative, "Cumulative.csv")


Fedinterest<-fread("FEDFUNDS.csv")
Fedinterest<-Fedinterest[427:783,]
ggplot(data=Fedinterest, aes(x=DATE, y=FEDFUNDS))+geom_point()+geom_line()


Granular<-fread("GCPD_granular_data.txt")
Granular1<-fread("GCPD_granular_classCOOP.txt")

IBM<-Granular[Granular$conml=="International Business Machines Corp"]
Pfizer<-Granular[Granular$conml=="Pfizer Inc"]



#Get # of petents per year per city and flip
patents<-patents[,-c(1,4,5,6,7)]
patents<-aggregate(num_patents~msa_name09+year, patents, sum)
patents<-patents[order(patents$msa_name09, patents$year),]
row.names(patents) <- 1:nrow(patents)
patents<-spread(patents, year,num_patents)
patents<-patents[-8,]
fwrite(patents, "FlourishLineGraphPatents.csv")

patents<-patents[,-c(1,4,5,7)]
patents<-aggregate(num_patents~tech_subcat+year, patents, sum)
patents<-patents[order(patents$tech_subcat, patents$year),]
row.names(patents) <- 1:nrow(patents)
patents<-spread(patents, year,num_patents)
fwrite(patents, "FlourishLineGraphCategories.csv")

one<-fread("PatentsByYear.csv")
#West coast
WC1<-plotCity("San Jose-Sunnyvale-Santa Clara, CA")
WC2<-plotCity("San Francisco-Oakland-Fremont, CA")
WC3<-plotCity("Los Angeles-Long Beach-Santa Ana, CA")
WC4<-plotCity("San Diego-Carlsbad-San Marcos, CA")
WC5<-plotCity("Seattle-Tacoma-Bellevue, WA")
WC6<-plotCity("Portland-Vancouver-Hillsboro, OR-WA")
WC7<-plotCity("Boulder, CO")
WC8<-plotCity("Oxnard-Thousand Oaks-Ventura, CA")
WC9<-plotCity("Sacramento--Arden-Arcade--Roseville, CA")
grid.arrange(WC1, WC2, WC3, WC4, WC5,WC6,WC7,WC8,WC9)

#North East
NE1<-plotCity("New York-Northern New Jersey-Long Island, NY-NJ-PA")
NE2<-plotCity("Boston-Cambridge-Quincy, MA-NH")
NE3<-plotCity("Washington-Arlington-Alexandria, DC-VA-MD-WV")
NE4<-plotCity("Rochester, NY")
NE5<-plotCity("Pittsburgh, PA")
NE6<-plotCity("Albany-Schenectady-Troy, NY")
NE7<-plotCity("Baltimore-Towson, MD")
NE8<-plotCity("Poughkeepsie-Newburgh-Middletown, NY")
NE9<-plotCity("Bridgeport-Stamford-Norwalk, CT")
grid.arrange(NE1, NE2, NE3, NE4, NE5, NE6, NE7, NE8, NE9)

#Mid west
MW1<-plotCity("Chicago-Joliet-Naperville, IL-IN-WI")
MW2<-plotCity("Minneapolis-St. Paul-Bloomington, MN-WI")
MW3<-plotCity("Detroit-Warren-Livonia, MI")
MW4<-plotCity("Cincinnati-Middletown, OH-KY-IN")
MW5<-plotCity("Cleveland-Elyria-Mentor, OH")
MW6<-plotCity("St. Louis, MO-IL")
MW7<-plotCity("Milwaukee-Waukesha-West Allis, WI")
MW8<-plotCity("Indianapolis-Carmel, IN")
MW9<-plotCity("Kansas City, MO-KS")
grid.arrange(MW1, MW2, MW3, MW4, MW5, MW6, MW7, MW8, MW9)

#South
S1<- plotCity("Dallas-Fort Worth-Arlington, TX")
S2<-plotCity("Austin-Round Rock-San Marcos, TX")
S3<-plotCity("Houston-Sugar Land-Baytown, TX")
S4<-plotCity("Atlanta-Sandy Springs-Marietta, GA")
S5<-plotCity("Phoenix-Mesa-Glendale, AZ")
S6<-plotCity("Miami-Fort Lauderdale-Pompano Beach, FL")
S7<-plotCity("Tucson, AZ")
S8<-plotCity("Tampa-St. Petersburg-Clearwater, FL")
S9<-plotCity("Palm Bay-Melbourne-Titusville, FL")
grid.arrange(S1,S2,S3,S4,S5,S6,S7,S8,S9)
 