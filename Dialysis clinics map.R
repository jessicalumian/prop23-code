library(tidyverse)
library(readxl)
library(leaflet)

##Loading Data

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
                                        With_Closures$CLOSED=="Closed 2012"~"gray",
                                        With_Closures$CLOSED=="Closed 2013"~"purple",
                                        With_Closures$CLOSED=="Closed 2014"~"beige",
                                        With_Closures$CLOSED=="Closed 2015"~"blue",
                                        With_Closures$CLOSED=="Closed 2016"~"green",
                                        With_Closures$CLOSED=="Closed 2017"~"pink",
                                        With_Closures$CLOSED=="Closed 2018"~"orange",
                                        With_Closures$CLOSED=="Open 2019"~"red",
                                      ))


##Clinics with the same oshpd_id are filtered out

Unique_Clinics<-With_Closures[!duplicated(With_Closures$OSHPD_ID),]
Unique_Clinics$CLOSED<-as.factor(Unique_Clinics$CLOSED)

##Clinics are mapped

my_icons<-awesomeIcons(icon = Unique_Clinics$CLOSED,
                       markerColor = Unique_Clinics$COLOR)

Unique_Clinics%>%leaflet()%>%addTiles()%>%
  setView(lng = -122.299396,lat = 37.783181,zoom = 5)%>%
  addAwesomeMarkers(lat = as.numeric(Unique_Clinics$LATITUDE),
             lng = as.numeric(Unique_Clinics$LONGITUDE),
             icon =~my_icons[CLOSED])
