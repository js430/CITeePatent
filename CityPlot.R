library(data.table)
library(ggplot2)
library(gridExtra)
library(stringr)
library(dplyr)
library(tidyr)



plotCity <- function(cityname){
    city<-patents[patents$msa_name09==cityname]
    numpatents<-aggregate(num_patents~year, city, sum)
    ggplot(data=numpatents)+geom_point(aes(x=year, y=num_patents))+ggtitle(cityname)+xlab("Year")+ylab("Number of Patents")
}

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
