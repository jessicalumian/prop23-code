library(tidyverse)
library(readxl)
library(leaflet)
library(acs)
library(tigris)
library(sf)
library(htmlwidgets)


############################ Getting Clinic Locations ##############################

#Use the link below to get ahold of the datasets of dialysis clinics

#https://data.ca.gov/dataset/specialty-care-clinic-complete-data-set/resource/fca7a906-a9e0-4758-a7da-be071cb3d793

Clinics_19<-read_excel("Desktop/clinic_data/2019-specialty-care-clinic-utilization-data-preliminary-june-2020.xlsx", 
                       sheet = "Page 1-5")%>%
  filter(str_detect(`LIC_CAT`,"Dialysis"))%>%
  mutate(.,"Year"=2019)

Clinics_18<-read_excel("Desktop/clinic_data/2018-specialty-care-clinic-utilization-data-november-2019.xlsx", 
                       sheet = "Page 1-5")%>%
  filter(str_detect(`LIC_CAT`,"Dialysis"))%>%
  mutate(.,"Year"=2018)

Clinics_17<-read_excel("Desktop/clinic_data/2017-specialty-care-clinic-utilization-data-october-2018.xlsx", 
                       sheet = "Section 1-5")%>%
  filter(str_detect(TYPE_LIC,"Dialysis"))%>%
  mutate(.,"Year"=2017)

Clinics_16<-read_excel("Desktop/clinic_data/2016-specialty-care-clinic-complete-data-set-november-2019-extract.xlsx", 
                       sheet = "Section 1-5")%>%
  filter(str_detect(TYPE_LIC,"Dialysis"))%>%
  mutate(.,"Year"=2016)

Clinics_15<-read_excel("Desktop/clinic_data/2015-specialty-care-clinic-complete-data-set.xlsx", 
                       sheet = "Section 1-5")%>%
  filter(str_detect(TYPE_LIC,"Dialysis"))%>%
  mutate(.,"Year"=2015)

Clinics_14<-read_excel("Desktop/clinic_data/2014-specialty-care-clinic-complete-data-set.xlsx", 
                       sheet = "Section 1-5")%>%
  filter(str_detect(TYPE_LIC,"Dialysis"))%>%
  mutate(.,"Year"=2014)

Clinics_13<-read_excel("Desktop/clinic_data/2013-specialty-care-clinic-complete-data-set.xlsx", 
                       sheet = "Sections 1-5")%>%
  filter(str_detect(TYPE_LIC,"Dialysis"))%>%
  mutate(.,"Year"=2013)

Clinics_12<-read_excel("Desktop/clinic_data/2012-specialty-care-clinic-complete-data-set.xlsx", 
                       sheet = "Section 1-5")%>%
  filter(str_detect(TYPE_LIC,"Dialysis"))%>%
  mutate(.,"Year"=2012)


##Combinig all of the tables together for faster manipulation

All_Data<-bind_rows(Clinics_12,Clinics_13,Clinics_14,Clinics_15,Clinics_16,Clinics_17,Clinics_18,Clinics_19)


##Making a function to see if a dialysis center was open one year but not the next
##drops the clinic if it has the same name as a clinic the previous year

Closures<-function(recentyear,oldyear){
  droplevels(as.factor(oldyear$FAC_NAME),
             recentyear$FAC_NAME)%>%
    as.data.frame()%>%
    drop_na()
}

Closed_18<-Closures(Clinics_19,Clinics_18)
Closed_17<-Closures(Clinics_18,Clinics_17)
Closed_16<-Closures(Clinics_17,Clinics_16)
Closed_15<-Closures(Clinics_16,Clinics_15)
Closed_14<-Closures(Clinics_15,Clinics_14)
Closed_13<-Closures(Clinics_14,Clinics_13)
Closed_12<-Closures(Clinics_13,Clinics_12)

##Designations of when the clinic closed or if it is open is added onto the All_Data
##data frame along with a color for mapping

With_Closures<-All_Data%>%mutate(.,
                                 CLOSED= case_when(
                                   All_Data$FAC_NAME%in%Closed_12$.~"Closed 2012",
                                   All_Data$FAC_NAME%in%Closed_13$.~"Closed 2013",
                                   All_Data$FAC_NAME%in%Closed_14$.~"Closed 2014",
                                   All_Data$FAC_NAME%in%Closed_15$.~"Closed 2015",
                                   All_Data$FAC_NAME%in%Closed_16$.~"Closed 2016",
                                   All_Data$FAC_NAME%in%Closed_17$.~"Closed 2017",
                                   All_Data$FAC_NAME%in%Closed_18$.~"Closed 2018",
                                   All_Data$FAC_NAME%in%Clinics_19$FAC_NAME~"Open 2019"
                                 ))
With_Closures<-With_Closures%>%mutate(.,
                                      COLOR= case_when(
                                        With_Closures$CLOSED=="Closed 2012"~"black",
                                        With_Closures$CLOSED=="Closed 2013"~"black",
                                        With_Closures$CLOSED=="Closed 2014"~"black",
                                        With_Closures$CLOSED=="Closed 2015"~"black",
                                        With_Closures$CLOSED=="Closed 2016"~"black",
                                        With_Closures$CLOSED=="Closed 2017"~"black",
                                        With_Closures$CLOSED=="Closed 2018"~"black",
                                        With_Closures$CLOSED=="Open 2019"~"blue",
                                      ))


##Clinics with the same oshpd_id are filtered out

Unique_Clinics<-With_Closures[!duplicated(With_Closures$OSHPD_ID),]
Unique_Clinics$CLOSED<-as.factor(Unique_Clinics$CLOSED)



##Clinics Breakdown
Unique_Clinics%>%group_by(CLOSED)%>%summarise(count=n())


############################## Getting Map Data ####################################

#Get the API for accessing census data

api.key<-"ccc1b3ee11c4d79eb317224b681ccafa49451b06"
api.key.install(key=api.key)

#get the codes for all of the counties. State Code for California is 6

my.geo<-geo.make(state = 6,county =c(geo.lookup(6,"*")%>%.[2:nrow(.),3]),tract = "*")

#Table Numbers found on https://censusreporter.org/topics/ click on income for this set
#end year is confusingly named. With span=5 it is 2014-2019 data

Data<-acs.fetch(geography = my.geo,table.number = "B19013",endyear = 2014,span = 5)

#Use tracts to get borders of cities and counties in CA

California<-tracts("CA",county=c(geo.lookup(6,"*")%>%.[2:nrow(.),3]))

#Extract out the income data and geocodes to link to the tracts data frame
#paste0 function was used to make sure the codes match the tracts

income_df<- data.frame( GEOID = paste0("0",Data@geography$state,
                                       str_pad(Data@geography$county,
                                               width=3,
                                               side="left",
                                               pad="0"),
                                       Data@geography$tract
),
median_income = as.numeric(Data@estimate),
row.names=NULL)

#Join the spacial data frame with the income data using GEOID

California<-California%>%
  left_join(.,income_df,by="GEOID") %>%
  filter(median_income>=0)

#################################### Map ###########################################


#color code based on income

pal<-colorNumeric(palette = "YlGnBu",domain =California$median_income)
p<-colorFactor(palette = c("black", "blue"), domain = NULL, ordered = T)


#Color for clinics

#All Clinics
my_icons<-awesomeIcons(icon = Unique_Clinics$CLOSED,
                       markerColor = Unique_Clinics$COLOR)

#popup for city income levels and clinic info

popup <- paste0("<strong/>","GEOID: ","</strong>",California$GEOID,"<br>",
                "<strong/>","Median Income Level:","</strong>","<br>",
                "$",California$median_income)

PopUp<-paste0("<strong/>","Clinic Name: ","</strong>",Unique_Clinics$FAC_NAME,"<br>",
              "<strong/>","Status: ","</strong>",Unique_Clinics$CLOSED,"<br>",
              "<strong/>","Parent Company: ","</strong>", Unique_Clinics$PARENT_NAME, "<br>",
              "<strong/>","Address: ","</strong>", Unique_Clinics$FAC_ADDRESS_ONE," ", Unique_Clinics$FAC_CITY, ", ", Unique_Clinics$FAC_ZIPCODE, "<br>",
              "<strong/>","Phone Number: ","</strong>", Unique_Clinics$FAC_PHONE)

#Map using leaflet with all clinics

Map<-California%>%
  leaflet()%>%
  addProviderTiles("CartoDB.Positron")%>%
  addPolygons(fillColor=~pal(median_income),
              color ="#b2aeae",
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup)%>%
  addLegend(pal= pal,
            values = California$median_income,
            title = "Median Income Level",
            labFormat =labelFormat(prefix = "$"))%>%
  addAwesomeMarkers(lat = as.numeric(Unique_Clinics$LATITUDE),
                    lng = as.numeric(Unique_Clinics$LONGITUDE),
                    icon =~my_icons[Unique_Clinics$CLOSED],
                    popup = PopUp)%>%
  addLegend(pal=p, 
            values = c("Open","Closed"),
            title = "Status")
Map


saveWidget(Map,file = "map.html")


