
# month by Country and Region

EKATCEEKAT               <- read_xlsx(here::here("sources","DEC_Perform.Rep_2019_YTD_EKAT.xlsm"),sheet = "EKAT CEE",range="I16:T3000", col_names = TRUE,na = "NA")

EKATCEEKAT               <- EKATCEEKAT%>% filter_all(any_vars(!is.na(.))) 
EKATCEEKAT [is.na(EKATCEEKAT)]   <- 0   # wszystkie pola bez wartości wypełniamy zerami


EKATCEEKAT               <- left_join(EKATCEEKAT,MapowanieBaltics,by="Subsidiary Country")


colnames(EKATCEEKAT)[1]     <- "Klient"
colnames(EKATCEEKAT)[7]     <- Naglowek1
colnames(EKATCEEKAT)[8]     <- Naglowek2
colnames(EKATCEEKAT)[9]     <- Naglowek3
colnames(EKATCEEKAT)[10]    <- Naglowek4
colnames(EKATCEEKAT)[11]    <- Naglowek5
colnames(EKATCEEKAT)[12]    <- Naglowek6


MapowanieEKAT[MapowanieEKAT=="RING / HELIOS / REMBRANTIN / CHROMOS / AVRORA"]  <-"RING|HEL.|REM.|CHR.|AVR."
MapowanieEKAT[MapowanieEKAT=="COCA COLA / MULTON"]                             <-"COCACOLA|MULTON"
MapowanieEKAT[MapowanieEKAT=="AGRANA / AUSTRIA JUICE"]                         <-"AGRANA|AUSTRIAJUICE"
MapowanieEKAT[MapowanieEKAT=="LESAFFRE / SAF NEWA"]                            <-"LESAFFRE|SAFNEWA"
MapowanieEKAT[MapowanieEKAT=="MBCC Group (former BASF construction)"]          <-"MBCC Group"

EKATCEEKAT                  <- left_join(EKATCEEKAT,MapowanieEKAT,by="Klient")
EKATCEEKAT$Mapowanie        <- ifelse(is.na(EKATCEEKAT$Mapowanie),EKATCEEKAT$Mapowanie <- EKATCEEKAT$SoldTo,EKATCEEKAT$Mapowanie)




EKATCEEKAT_CEE               <- read_xlsx(here::here("sources","DEC_Perform.Rep_2019_YTD_EKAT.xlsm"),sheet = "CEEKAT",range="I16:T2000", col_names = TRUE,na = "NA")
EKATCEEKAT_CEE               <- EKATCEEKAT_CEE%>% filter_all(any_vars(!is.na(.))) 
EKATCEEKAT_CEE [is.na(EKATCEEKAT_CEE)]   <- 0   # wszystkie pola bez wartości wypełniamy zerami
EKATCEEKAT_CEE               <- left_join(EKATCEEKAT_CEE,MapowanieBaltics,by="Subsidiary Country")

colnames(EKATCEEKAT_CEE)[1]     <- "Klient"
colnames(EKATCEEKAT_CEE)[7]     <- Naglowek1
colnames(EKATCEEKAT_CEE)[8]     <- Naglowek2
colnames(EKATCEEKAT_CEE)[9]     <- Naglowek3
colnames(EKATCEEKAT_CEE)[10]    <- Naglowek4
colnames(EKATCEEKAT_CEE)[11]    <- Naglowek5
colnames(EKATCEEKAT_CEE)[12]    <- Naglowek6

EKATCEEKAT_CEE[EKATCEEKAT_CEE == "RING / HELIOS / REMBRANTIN / CHROMOS / AVRORA"]<-"RING|HEL.|REM.|CHR.|AVR."
EKATCEEKAT_CEE[EKATCEEKAT_CEE == "COCA COLA / MULTON"]<-"COCACOLA|MULTON"
EKATCEEKAT_CEE[EKATCEEKAT_CEE == "AGRANA / AUSTRIA JUICE"]<-"AGRANA|AUSTRIAJUICE"
EKATCEEKAT_CEE[EKATCEEKAT_CEE == "LESAFFRE / SAF NEWA"]<-"LESAFFRE|SAFNEWA"
EKATCEEKAT[EKATCEEKAT         == "MBCC Group (former BASF construction)"]<-"MBCC Group"

EKATCEEKAT_CEE        <- left_join(EKATCEEKAT_CEE,MapowanieEKAT,by="Klient")

EKATCEEKAT            <- EKATCEEKAT %>% select(1,3,4,15,13,14,7,8,9,10,11,12)
EKATCEEKAT_CEE        <- EKATCEEKAT_CEE %>% select(1,3,4,15,13,14,7,8,9,10,11,12)
EKATCEEKAT_CUS        <- EKATCEEKAT
EKATCEEKAT_CEE_CUS    <- EKATCEEKAT_CEE


EKATCEEKAT            <- EKATCEEKAT [,-2]
EKATCEEKAT_CEE        <- EKATCEEKAT_CEE[,-2]

EKAT<-rbind(EKATCEEKAT,EKATCEEKAT_CEE)

EKAT         <- EKAT %>% filter(`Calendar month`==MiesiacAnalizy)
EKAT         <- EKAT[,-c(1:2)]

# EKAT <- rbind(EKAT,EKAT2)
EKAT                        <- as.data.frame(EKAT)
EKAT                        <- EKAT %>% group_by(Mapowanie,Region,SubRegion) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
EKAT[is.na(EKAT)]           <- NA
EKAT$QuantityDiffYY         <- EKAT$QActYear-EKAT$QprevYear
EKAT[is.na(EKAT)]           <- NA
EKAT$QuantityDiffYYPER_P    <- as.numeric(ifelse(is.infinite(((EKAT$QActYear-EKAT$QprevYear)/EKAT$QprevYear)),"",(EKAT$QActYear-EKAT$QprevYear)/EKAT$QprevYear))
EKAT$QuantityDiffYYPER      <- ifelse(is.infinite(((EKAT$QActYear-EKAT$QprevYear)/EKAT$QprevYear)),"",scales::percent(((EKAT$QActYear-EKAT$QprevYear)/EKAT$QprevYear),accuracy = 0.1))
EKAT[is.na(EKAT)]           <- NA
EKAT$GPDiffYY               <- EKAT$GPACT-EKAT$GPPREV
EKAT[is.na(EKAT)]           <- NA
EKAT$GPDiffYYPER            <- ifelse(is.infinite(((EKAT$GPACT-EKAT$GPPREV)/EKAT$GPPREV)),"",scales::percent(((EKAT$GPACT-EKAT$GPPREV)/EKAT$GPPREV),accuracy = 0.1))
#EKAT$GPDiffYYPER       <- round(((EKAT$GPACT-EKAT$GPPREV)/EKAT$GPPREV)*100,2)
EKAT[is.na(EKAT)]      <- NA
EKAT$TurnDiffYY             <- EKAT$TURNACT-EKAT$TURNPREV
EKAT[is.na(EKAT)]      <- NA
EKAT$TurnDiffYYPER          <- ifelse(is.infinite(((EKAT$TURNACT-EKAT$TURNPREV)/EKAT$TURNPREV)),"",scales::percent(((EKAT$TURNACT-EKAT$TURNPREV)/EKAT$TURNPREV),accuracy = 0.1))
EKAT[is.na(EKAT)]      <- NA
EKAT$GPMTACT                <- ifelse(is.infinite(EKAT$GPACT*1000/EKAT$QActYear),0,EKAT$GPACT*1000/EKAT$QActYear)   # x 1000 ?
EKAT[is.na(EKAT)]      <- 0
EKAT$GPMTPREV               <- ifelse(is.infinite(EKAT$GPPREV*1000/EKAT$QprevYear),0,EKAT$GPPREV*1000/EKAT$QprevYear)
EKAT[is.na(EKAT)]      <- 0
EKAT$GPMTDIFF               <- EKAT$GPMTACT-EKAT$GPMTPREV
EKAT[is.na(EKAT)]      <- NA
EKAT$GPMTDIFFPER            <- ifelse(is.infinite((EKAT$GPMTACT-EKAT$GPMTPREV)/EKAT$GPMTPREV),"",scales::percent(((EKAT$GPMTACT-EKAT$GPMTPREV)/EKAT$GPMTPREV),accuracy = 0.1))
EKAT[is.na(EKAT)]      <- NA

DT                               <- data.table(EKAT)
EKAT                        <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
EKAT[is.na(EKAT)]      <- NA
EKAT$SubRegion              <- stri_replace_all_fixed(EKAT$SubRegion, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
EKAT                        <- setorder(EKAT,Mapowanie,Region,SubRegion,-QActYear)
EKAT$QuantityDiffYYPER_P    <- NULL



MapowaNieKey <- unique(EKAT$Mapowanie)


Total <- data.frame(Region="Total CEE",SubRegion="Total CEE", Mapowanie="Total CEE",
                    QActYear          = sum(EKAT$QActYear),
                    QprevYear         = sum(EKAT$QprevYear),
                    GPACT             = sum(EKAT$GPACT),
                    GPPREV            = sum(EKAT$GPPREV),
                    TURNACT           = sum(EKAT$TURNACT),
                    TURNPREV          = sum(EKAT$TURNPREV),
                    QuantityDiffYY    = 0,
                    QuantityDiffYYPER = 0,
                    GPDiffYYPER       = 0,
                    GPDiffYY          = 0,
                    TurnDiffYY        = sum(EKAT$TurnDiffYY),
                    TurnDiffYYPER     = 0,
                    GPMTACT           = sum(EKAT$GPMTACT),
                    GPMTPREV          = sum(EKAT$GPMTPREV),
                    GPMTDIFF          = 0,
                    GPMTDIFFPER       = 0)

Total$GPMTACT            <-  Total$GPACT*1000/Total$QActYear
Total$GPMTPREV           <-  Total$GPPREV*1000/Total$QprevYear
Total$TurnDiffYY         <-  Total$TURNACT-Total$TURNPREV
Total$QuantityDiffYY     <-  Total$QActYear-Total$QprevYear
Total$GPDiffYY           <-  Total$GPACT-Total$GPPREV
Total$GPMTDIFF           <-  Total$GPMTACT-Total$GPMTPREV
Total$QuantityDiffYYPER  <-  scales::percent(Total$QuantityDiffYY/Total$QprevYear,accuracy = 0.1)
Total$GPDiffYYPER        <-  scales::percent(Total$GPDiffYY/Total$GPPREV,accuracy = 0.1)
Total$TurnDiffYYPER      <-  scales::percent(Total$TurnDiffYY/Total$TURNPREV,accuracy = 0.1)
Total$GPMTDIFFPER        <-  scales::percent(Total$GPMTDIFF/Total$GPMTPREV,accuracy = 0.1)

RegionyCEE      <- unique(EKAT$Region)
Regiony         <- unique(EKAT$SubRegion)

for(region in unique(EKAT$Mapowanie)){
  steal <- subset(EKAT,Mapowanie==region)
  frejm <- data.frame(Mapowanie=region,Region="TOTAL", SubRegion=region, 
                      QActYear          = sum(steal$QActYear),
                      QprevYear         = sum(steal$QprevYear),
                      GPACT             = sum(steal$GPACT),
                      GPPREV            = sum(steal$GPPREV),
                      TURNACT           = sum(steal$TURNACT),
                      TURNPREV          = sum(steal$TURNPREV),
                      QuantityDiffYY    = sum(steal$QuantityDiffYY),
                      QuantityDiffYYPER = 0,
                      GPDiffYYPER       = 0,
                      GPDiffYY          = sum(steal$GPDiffYY),
                      TurnDiffYY        = sum(steal$TurnDiffYY),
                      TurnDiffYYPER     = 0,
                      GPMTACT           = sum(steal$GPMTACT),
                      GPMTPREV          = sum(steal$GPMTPREV),
                      GPMTDIFF          = sum(steal$GPMTDIFF),
                      GPMTDIFFPER       = 0)
  frejm$GPMTACT            <-  frejm$GPACT*1000/frejm$QActYear
  frejm$GPMTPREV           <-  frejm$GPPREV*1000/frejm$QprevYear
  frejm$TurnDiffYY         <-  frejm$TURNACT-frejm$TURNPREV
  frejm$QuantityDiffYY     <-  frejm$QActYear-frejm$QprevYear
  frejm$GPDiffYY           <-  frejm$GPACT-frejm$GPPREV
  frejm$GPMTDIFF           <-  frejm$GPMTACT-frejm$GPMTPREV
  frejm$QuantityDiffYYPER  <-  scales::percent(frejm$QuantityDiffYY/frejm$QprevYear,accuracy = 0.1)
  frejm$GPDiffYYPER        <-  scales::percent(frejm$GPDiffYY/frejm$GPPREV,accuracy = 0.1)
  frejm$TurnDiffYYPER      <-  scales::percent(frejm$TurnDiffYY/frejm$TURNPREV,accuracy = 0.1)
  frejm$GPMTDIFFPER        <-  scales::percent(frejm$GPMTDIFF/frejm$GPMTPREV,accuracy = 0.1)
  EKAT <- rbind(EKAT, frejm)
  
}

#GRUPOWANIE NORTH SOUTH CEE

for(mapowanie in MapowaNieKey){
  for(region in RegionyCEE){
    steal <- subset(EKAT,Mapowanie==mapowanie & Region==region)
  frejm <- data.frame(Region=region,SubRegion=region, Mapowanie=mapowanie,
                      QActYear          = sum(steal$QActYear),
                      QprevYear         = sum(steal$QprevYear),
                      GPACT             = sum(steal$GPACT),
                      GPPREV            = sum(steal$GPPREV),
                      TURNACT           = sum(steal$TURNACT),
                      TURNPREV          = sum(steal$TURNPREV),
                      QuantityDiffYY    = sum(steal$QuantityDiffYY),
                      QuantityDiffYYPER = 0,
                      GPDiffYYPER       = 0,
                      GPDiffYY          = sum(steal$GPDiffYY),
                      TurnDiffYY        = sum(steal$TurnDiffYY),
                      TurnDiffYYPER     = 0,
                      GPMTACT           = sum(steal$GPMTACT),
                      GPMTPREV          = sum(steal$GPMTPREV),
                      GPMTDIFF          = sum(steal$GPMTDIFF),
                      GPMTDIFFPER       = 0)
  frejm$GPMTACT            <-  frejm$GPACT*1000/frejm$QActYear
  frejm$GPMTPREV           <-  frejm$GPPREV*1000/frejm$QprevYear
  frejm$TurnDiffYY         <-  frejm$TURNACT-frejm$TURNPREV
  frejm$QuantityDiffYY     <-  frejm$QActYear-frejm$QprevYear
  frejm$GPDiffYY           <-  frejm$GPACT-frejm$GPPREV
  frejm$GPMTDIFF           <-  frejm$GPMTACT-frejm$GPMTPREV
  frejm$QuantityDiffYYPER  <-  scales::percent(frejm$QuantityDiffYY/frejm$QprevYear,accuracy = 0.1)
  frejm$GPDiffYYPER        <-  scales::percent(frejm$GPDiffYY/frejm$GPPREV,accuracy = 0.1)
  frejm$TurnDiffYYPER      <-  scales::percent(frejm$TurnDiffYY/frejm$TURNPREV,accuracy = 0.1)
  frejm$GPMTDIFFPER        <-  scales::percent(frejm$GPMTDIFF/frejm$GPMTPREV,accuracy = 0.1)
  #browser()
  
  EKAT <- rbind(EKAT, frejm)
  
}
}

EKAT                      <- setorder(EKAT,Mapowanie,Region)
EKAT                      <- rbind(EKAT,Total)

WyboldowaneWierszeEKAT    <- c(which(EKAT$SubRegion %in% RegionyCEE))
WyboldowaneWierszeEKAT    <- c(which(str_sub(EKAT$SubRegion,7,9) == "CEE"),WyboldowaneWierszeEKAT)
WyboldowaneWierszeEKAT    <- c(which(EKAT$SubRegion %in% MapowaNieKey),WyboldowaneWierszeEKAT)
# EKAT         <- rbind(EKAT,thereofCEE) na razie tą część tabelki trzeba odpóścić, do znalezienia rozwiązania




EKAT$Mapowanie            <- NULL
EKAT$Region               <- NULL



EKAT                      <- EKAT %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
EKAT[is.na(EKAT)]         <- ""


# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

EKAT          <- EKAT %>% mutate_if(is.numeric, ~round(., 0)) 




#######   YTD


# month by Country and Region

EKAT_YTD<-rbind(EKATCEEKAT,EKATCEEKAT_CEE)

EKAT_YTD         <- EKAT_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)

EKAT_YTD         <- EKAT_YTD[,-c(1:2)]




# EKAT_YTD <- rbind(EKAT_YTD,EKAT_YTD2)
EKAT_YTD                        <- as.data.frame(EKAT_YTD)
EKAT_YTD                        <- EKAT_YTD %>% group_by(Mapowanie,Region,SubRegion) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
EKAT_YTD[is.na(EKAT_YTD)]      <- NA
EKAT_YTD$QuantityDiffYY         <- EKAT_YTD$QActYear-EKAT_YTD$QprevYear
EKAT_YTD[is.na(EKAT_YTD)]      <- NA
EKAT_YTD$QuantityDiffYYPER_P    <- as.numeric(ifelse(is.infinite(((EKAT_YTD$QActYear-EKAT_YTD$QprevYear)/EKAT_YTD$QprevYear)),"",(EKAT_YTD$QActYear-EKAT_YTD$QprevYear)/EKAT_YTD$QprevYear))
EKAT_YTD$QuantityDiffYYPER      <- ifelse(is.infinite(((EKAT_YTD$QActYear-EKAT_YTD$QprevYear)/EKAT_YTD$QprevYear)),"",scales::percent(((EKAT_YTD$QActYear-EKAT_YTD$QprevYear)/EKAT_YTD$QprevYear),accuracy = 0.1))
EKAT_YTD[is.na(EKAT_YTD)]      <- NA
EKAT_YTD$GPDiffYY               <- EKAT_YTD$GPACT-EKAT_YTD$GPPREV
EKAT_YTD[is.na(EKAT_YTD)]      <- NA
EKAT_YTD$GPDiffYYPER            <- ifelse(is.infinite(((EKAT_YTD$GPACT-EKAT_YTD$GPPREV)/EKAT_YTD$GPPREV)),"",scales::percent(((EKAT_YTD$GPACT-EKAT_YTD$GPPREV)/EKAT_YTD$GPPREV),accuracy = 0.1))
#EKAT_YTD$GPDiffYYPER       <- round(((EKAT_YTD$GPACT-EKAT_YTD$GPPREV)/EKAT_YTD$GPPREV)*100,2)
EKAT_YTD[is.na(EKAT_YTD)]      <- NA
EKAT_YTD$TurnDiffYY             <- EKAT_YTD$TURNACT-EKAT_YTD$TURNPREV
EKAT_YTD[is.na(EKAT_YTD)]      <- NA
EKAT_YTD$TurnDiffYYPER          <- ifelse(is.infinite(((EKAT_YTD$TURNACT-EKAT_YTD$TURNPREV)/EKAT_YTD$TURNPREV)),"",scales::percent(((EKAT_YTD$TURNACT-EKAT_YTD$TURNPREV)/EKAT_YTD$TURNPREV),accuracy = 0.1))
EKAT_YTD[is.na(EKAT_YTD)]      <- NA
EKAT_YTD$GPMTACT                <- ifelse(is.infinite(EKAT_YTD$GPACT*1000/EKAT_YTD$QActYear),0,EKAT_YTD$GPACT*1000/EKAT_YTD$QActYear)   # x 1000 ?
EKAT_YTD[is.na(EKAT_YTD)]      <- 0
EKAT_YTD$GPMTPREV               <- ifelse(is.infinite(EKAT_YTD$GPPREV*1000/EKAT_YTD$QprevYear),0,EKAT_YTD$GPPREV*1000/EKAT_YTD$QprevYear)
EKAT_YTD[is.na(EKAT_YTD)]      <- 0
EKAT_YTD$GPMTDIFF               <- EKAT_YTD$GPMTACT-EKAT_YTD$GPMTPREV
EKAT_YTD[is.na(EKAT_YTD)]      <- NA
EKAT_YTD$GPMTDIFFPER            <- ifelse(is.infinite((EKAT_YTD$GPMTACT-EKAT_YTD$GPMTPREV)/EKAT_YTD$GPMTPREV),"",scales::percent(((EKAT_YTD$GPMTACT-EKAT_YTD$GPMTPREV)/EKAT_YTD$GPMTPREV),accuracy = 0.1))
EKAT_YTD[is.na(EKAT_YTD)]      <- NA

DT                               <- data.table(EKAT_YTD)
EKAT_YTD                        <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
EKAT_YTD[is.na(EKAT_YTD)]      <- NA
EKAT_YTD$SubRegion              <- stri_replace_all_fixed(EKAT_YTD$SubRegion, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
EKAT_YTD                        <- setorder(EKAT_YTD,Mapowanie,Region,SubRegion,-QActYear)
EKAT_YTD$QuantityDiffYYPER_P    <- NULL



MapowaNieKey <- unique(EKAT_YTD$Mapowanie)


Total <- data.frame(Region="Total CEE",SubRegion="Total CEE", Mapowanie="Total CEE",
                    QActYear          = sum(EKAT_YTD$QActYear),
                    QprevYear         = sum(EKAT_YTD$QprevYear),
                    GPACT             = sum(EKAT_YTD$GPACT),
                    GPPREV            = sum(EKAT_YTD$GPPREV),
                    TURNACT           = sum(EKAT_YTD$TURNACT),
                    TURNPREV          = sum(EKAT_YTD$TURNPREV),
                    QuantityDiffYY    = 0,
                    QuantityDiffYYPER = 0,
                    GPDiffYYPER       = 0,
                    GPDiffYY          = 0,
                    TurnDiffYY        = sum(EKAT_YTD$TurnDiffYY),
                    TurnDiffYYPER     = 0,
                    GPMTACT           = sum(EKAT_YTD$GPMTACT),
                    GPMTPREV          = sum(EKAT_YTD$GPMTPREV),
                    GPMTDIFF          = 0,
                    GPMTDIFFPER       = 0)

Total$GPMTACT            <-  Total$GPACT*1000/Total$QActYear
Total$GPMTPREV           <-  Total$GPPREV*1000/Total$QprevYear
Total$TurnDiffYY         <-  Total$TURNACT-Total$TURNPREV
Total$QuantityDiffYY     <-  Total$QActYear-Total$QprevYear
Total$GPDiffYY           <-  Total$GPACT-Total$GPPREV
Total$GPMTDIFF           <-  Total$GPMTACT-Total$GPMTPREV
Total$QuantityDiffYYPER  <-  scales::percent(Total$QuantityDiffYY/Total$QprevYear,accuracy = 0.1)
Total$GPDiffYYPER        <-  scales::percent(Total$GPDiffYY/Total$GPPREV,accuracy = 0.1)
Total$TurnDiffYYPER      <-  scales::percent(Total$TurnDiffYY/Total$TURNPREV,accuracy = 0.1)
Total$GPMTDIFFPER        <-  scales::percent(Total$GPMTDIFF/Total$GPMTPREV,accuracy = 0.1)

RegionyCEE<-unique(EKAT_YTD$Region)
Regiony <- unique(EKAT_YTD$SubRegion)

for(region in unique(EKAT_YTD$Mapowanie)){
  steal <- subset(EKAT_YTD,Mapowanie==region)
  frejm <- data.frame(Mapowanie=region,Region="TOTAL", SubRegion=region, 
                      QActYear          = sum(steal$QActYear),
                      QprevYear         = sum(steal$QprevYear),
                      GPACT             = sum(steal$GPACT),
                      GPPREV            = sum(steal$GPPREV),
                      TURNACT           = sum(steal$TURNACT),
                      TURNPREV          = sum(steal$TURNPREV),
                      QuantityDiffYY    = sum(steal$QuantityDiffYY),
                      QuantityDiffYYPER = 0,
                      GPDiffYYPER       = 0,
                      GPDiffYY          = sum(steal$GPDiffYY),
                      TurnDiffYY        = sum(steal$TurnDiffYY),
                      TurnDiffYYPER     = 0,
                      GPMTACT           = sum(steal$GPMTACT),
                      GPMTPREV          = sum(steal$GPMTPREV),
                      GPMTDIFF          = sum(steal$GPMTDIFF),
                      GPMTDIFFPER       = 0)
  frejm$GPMTACT            <-  frejm$GPACT*1000/frejm$QActYear
  frejm$GPMTPREV           <-  frejm$GPPREV*1000/frejm$QprevYear
  frejm$TurnDiffYY         <-  frejm$TURNACT-frejm$TURNPREV
  frejm$QuantityDiffYY     <-  frejm$QActYear-frejm$QprevYear
  frejm$GPDiffYY           <-  frejm$GPACT-frejm$GPPREV
  frejm$GPMTDIFF           <-  frejm$GPMTACT-frejm$GPMTPREV
  frejm$QuantityDiffYYPER  <-  scales::percent(frejm$QuantityDiffYY/frejm$QprevYear,accuracy = 0.1)
  frejm$GPDiffYYPER        <-  scales::percent(frejm$GPDiffYY/frejm$GPPREV,accuracy = 0.1)
  frejm$TurnDiffYYPER      <-  scales::percent(frejm$TurnDiffYY/frejm$TURNPREV,accuracy = 0.1)
  frejm$GPMTDIFFPER        <-  scales::percent(frejm$GPMTDIFF/frejm$GPMTPREV,accuracy = 0.1)
  EKAT_YTD <- rbind(EKAT_YTD, frejm)
  
}

#GRUPOWANIE NORTH SOUTH CEE

for(mapowanie in MapowaNieKey){
  for(region in RegionyCEE){
    steal <- subset(EKAT_YTD,Mapowanie==mapowanie & Region==region)
    frejm <- data.frame(Region=region,SubRegion=region, Mapowanie=mapowanie,
                        QActYear          = sum(steal$QActYear),
                        QprevYear         = sum(steal$QprevYear),
                        GPACT             = sum(steal$GPACT),
                        GPPREV            = sum(steal$GPPREV),
                        TURNACT           = sum(steal$TURNACT),
                        TURNPREV          = sum(steal$TURNPREV),
                        QuantityDiffYY    = sum(steal$QuantityDiffYY),
                        QuantityDiffYYPER = 0,
                        GPDiffYYPER       = 0,
                        GPDiffYY          = sum(steal$GPDiffYY),
                        TurnDiffYY        = sum(steal$TurnDiffYY),
                        TurnDiffYYPER     = 0,
                        GPMTACT           = sum(steal$GPMTACT),
                        GPMTPREV          = sum(steal$GPMTPREV),
                        GPMTDIFF          = sum(steal$GPMTDIFF),
                        GPMTDIFFPER       = 0)
    frejm$GPMTACT            <-  frejm$GPACT*1000/frejm$QActYear
    frejm$GPMTPREV           <-  frejm$GPPREV*1000/frejm$QprevYear
    frejm$TurnDiffYY         <-  frejm$TURNACT-frejm$TURNPREV
    frejm$QuantityDiffYY     <-  frejm$QActYear-frejm$QprevYear
    frejm$GPDiffYY           <-  frejm$GPACT-frejm$GPPREV
    frejm$GPMTDIFF           <-  frejm$GPMTACT-frejm$GPMTPREV
    frejm$QuantityDiffYYPER  <-  scales::percent(frejm$QuantityDiffYY/frejm$QprevYear,accuracy = 0.1)
    frejm$GPDiffYYPER        <-  scales::percent(frejm$GPDiffYY/frejm$GPPREV,accuracy = 0.1)
    frejm$TurnDiffYYPER      <-  scales::percent(frejm$TurnDiffYY/frejm$TURNPREV,accuracy = 0.1)
    frejm$GPMTDIFFPER        <-  scales::percent(frejm$GPMTDIFF/frejm$GPMTPREV,accuracy = 0.1)
    #browser()
    
    EKAT_YTD <- rbind(EKAT_YTD, frejm)
    
  }
}

EKAT_YTD                      <- setorder(EKAT_YTD,Mapowanie,Region)
EKAT_YTD                      <- rbind(EKAT_YTD,Total)

WyboldowaneWierszeEKAT_YTD    <- c(which(EKAT_YTD$SubRegion %in% RegionyCEE))
WyboldowaneWierszeEKAT_YTD    <- c(which(str_sub(EKAT_YTD$SubRegion,7,9) == "CEE"),WyboldowaneWierszeEKAT_YTD)
WyboldowaneWierszeEKAT_YTD    <- c(which(EKAT_YTD$SubRegion %in% MapowaNieKey),WyboldowaneWierszeEKAT_YTD)
# EKAT_YTD         <- rbind(EKAT_YTD,thereofCEE) na razie tą część tabelki trzeba odpóścić, do znalezienia rozwiązania




EKAT_YTD$Mapowanie            <- NULL
EKAT_YTD$Region               <- NULL



EKAT_YTD                      <- EKAT_YTD %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
EKAT_YTD[is.na(EKAT_YTD)]    <- ""


# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

EKAT_YTD          <- EKAT_YTD %>% mutate_if(is.numeric, ~round(., 0)) 


# month EKAT CUSTOMERS
           
colnames(EKATCEEKAT_CUS)[2]<-"SoldTo"

EKATCEEKAT_CUS<-EKATCEEKAT_CUS[,-1]
colnames(EKATCEEKAT_CUS)[1]<-"Klient"

EKATCEEKAT_CEE_CUS<-EKATCEEKAT_CEE_CUS[,-2]



EKAT_CUSTOMERS         <- rbind(EKATCEEKAT_CUS,EKATCEEKAT_CEE_CUS)



EKAT_CUSTOMERS         <- EKAT_CUSTOMERS %>% filter(`Calendar month`==MiesiacAnalizy)
#EKAT_CUSTOMERS         <- EKAT_CUSTOMERS[,-2]
EKAT_CUSTOMERS  <-EKAT_CUSTOMERS  %>% select(1,3,6:11)


# EKAT_CUSTOMERS <- rbind(EKAT_CUSTOMERS,EKAT_CUSTOMERS2)
EKAT_CUSTOMERS                        <- as.data.frame(EKAT_CUSTOMERS)
EKAT_CUSTOMERS                        <- EKAT_CUSTOMERS %>% group_by(Mapowanie,Klient) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
EKAT_CUSTOMERS[is.na(EKAT_CUSTOMERS)]      <- NA
EKAT_CUSTOMERS$QuantityDiffYY         <- EKAT_CUSTOMERS$QActYear-EKAT_CUSTOMERS$QprevYear
EKAT_CUSTOMERS[is.na(EKAT_CUSTOMERS)]      <- NA
EKAT_CUSTOMERS$QuantityDiffYYPER_P    <- as.numeric(ifelse(is.infinite(((EKAT_CUSTOMERS$QActYear-EKAT_CUSTOMERS$QprevYear)/EKAT_CUSTOMERS$QprevYear)),"",(EKAT_CUSTOMERS$QActYear-EKAT_CUSTOMERS$QprevYear)/EKAT_CUSTOMERS$QprevYear))
EKAT_CUSTOMERS$QuantityDiffYYPER      <- ifelse(is.infinite(((EKAT_CUSTOMERS$QActYear-EKAT_CUSTOMERS$QprevYear)/EKAT_CUSTOMERS$QprevYear)),"",scales::percent(((EKAT_CUSTOMERS$QActYear-EKAT_CUSTOMERS$QprevYear)/EKAT_CUSTOMERS$QprevYear),accuracy = 0.1))
EKAT_CUSTOMERS[is.na(EKAT_CUSTOMERS)]      <- NA
EKAT_CUSTOMERS$GPDiffYY               <- EKAT_CUSTOMERS$GPACT-EKAT_CUSTOMERS$GPPREV
EKAT_CUSTOMERS[is.na(EKAT_CUSTOMERS)]      <- NA
EKAT_CUSTOMERS$GPDiffYYPER            <- ifelse(is.infinite(((EKAT_CUSTOMERS$GPACT-EKAT_CUSTOMERS$GPPREV)/EKAT_CUSTOMERS$GPPREV)),"",scales::percent(((EKAT_CUSTOMERS$GPACT-EKAT_CUSTOMERS$GPPREV)/EKAT_CUSTOMERS$GPPREV),accuracy = 0.1))
#EKAT_CUSTOMERS$GPDiffYYPER       <- round(((EKAT_CUSTOMERS$GPACT-EKAT_CUSTOMERS$GPPREV)/EKAT_CUSTOMERS$GPPREV)*100,2)
EKAT_CUSTOMERS[is.na(EKAT_CUSTOMERS)]      <- NA
EKAT_CUSTOMERS$TurnDiffYY             <- EKAT_CUSTOMERS$TURNACT-EKAT_CUSTOMERS$TURNPREV
EKAT_CUSTOMERS[is.na(EKAT_CUSTOMERS)]      <- NA
EKAT_CUSTOMERS$TurnDiffYYPER          <- ifelse(is.infinite(((EKAT_CUSTOMERS$TURNACT-EKAT_CUSTOMERS$TURNPREV)/EKAT_CUSTOMERS$TURNPREV)),"",scales::percent(((EKAT_CUSTOMERS$TURNACT-EKAT_CUSTOMERS$TURNPREV)/EKAT_CUSTOMERS$TURNPREV),accuracy = 0.1))
EKAT_CUSTOMERS[is.na(EKAT_CUSTOMERS)]      <- NA
EKAT_CUSTOMERS$GPMTACT                <- ifelse(is.infinite(EKAT_CUSTOMERS$GPACT*1000/EKAT_CUSTOMERS$QActYear),0,EKAT_CUSTOMERS$GPACT*1000/EKAT_CUSTOMERS$QActYear)   # x 1000 ?
EKAT_CUSTOMERS[is.na(EKAT_CUSTOMERS)]      <- 0
EKAT_CUSTOMERS$GPMTPREV               <- ifelse(is.infinite(EKAT_CUSTOMERS$GPPREV*1000/EKAT_CUSTOMERS$QprevYear),0,EKAT_CUSTOMERS$GPPREV*1000/EKAT_CUSTOMERS$QprevYear)
EKAT_CUSTOMERS[is.na(EKAT_CUSTOMERS)]      <- 0
EKAT_CUSTOMERS$GPMTDIFF               <- EKAT_CUSTOMERS$GPMTACT-EKAT_CUSTOMERS$GPMTPREV
EKAT_CUSTOMERS[is.na(EKAT_CUSTOMERS)]      <- NA
EKAT_CUSTOMERS$GPMTDIFFPER            <- ifelse(is.infinite((EKAT_CUSTOMERS$GPMTACT-EKAT_CUSTOMERS$GPMTPREV)/EKAT_CUSTOMERS$GPMTPREV),"",scales::percent(((EKAT_CUSTOMERS$GPMTACT-EKAT_CUSTOMERS$GPMTPREV)/EKAT_CUSTOMERS$GPMTPREV),accuracy = 0.1))
EKAT_CUSTOMERS[is.na(EKAT_CUSTOMERS)]      <- NA

DT                               <- data.table(EKAT_CUSTOMERS)
EKAT_CUSTOMERS                        <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
EKAT_CUSTOMERS[is.na(EKAT_CUSTOMERS)]      <- NA
EKAT_CUSTOMERS$Klient            <- stri_replace_all_fixed(EKAT_CUSTOMERS$Klient, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
EKAT_CUSTOMERS                        <- setorder(EKAT_CUSTOMERS,Mapowanie,-QuantityDiffYY)
EKAT_CUSTOMERS$QuantityDiffYYPER_P    <- NULL


MapowaNieKeyECUS <- unique(EKAT_CUSTOMERS$Mapowanie)


Total <- data.frame(Mapowanie="Total CEE",Klient="Total CEE",
                    QActYear          = sum(EKAT_CUSTOMERS$QActYear),
                    QprevYear         = sum(EKAT_CUSTOMERS$QprevYear),
                    GPACT             = sum(EKAT_CUSTOMERS$GPACT),
                    GPPREV            = sum(EKAT_CUSTOMERS$GPPREV),
                    TURNACT           = sum(EKAT_CUSTOMERS$TURNACT),
                    TURNPREV          = sum(EKAT_CUSTOMERS$TURNPREV),
                    QuantityDiffYY    = 0,
                    QuantityDiffYYPER = 0,
                    GPDiffYYPER       = 0,
                    GPDiffYY          = 0,
                    TurnDiffYY        = sum(EKAT_CUSTOMERS$TurnDiffYY),
                    TurnDiffYYPER     = 0,
                    GPMTACT           = sum(EKAT_CUSTOMERS$GPMTACT),
                    GPMTPREV          = sum(EKAT_CUSTOMERS$GPMTPREV),
                    GPMTDIFF          = 0,
                    GPMTDIFFPER       = 0)

Total$GPMTACT            <-  Total$GPACT*1000/Total$QActYear
Total$GPMTPREV           <-  Total$GPPREV*1000/Total$QprevYear
Total$TurnDiffYY         <-  Total$TURNACT-Total$TURNPREV
Total$QuantityDiffYY     <-  Total$QActYear-Total$QprevYear
Total$GPDiffYY           <-  Total$GPACT-Total$GPPREV
Total$GPMTDIFF           <-  Total$GPMTACT-Total$GPMTPREV
Total$QuantityDiffYYPER  <-  scales::percent(Total$QuantityDiffYY/Total$QprevYear,accuracy = 0.1)
Total$GPDiffYYPER        <-  scales::percent(Total$GPDiffYY/Total$GPPREV,accuracy = 0.1)
Total$TurnDiffYYPER      <-  scales::percent(Total$TurnDiffYY/Total$TURNPREV,accuracy = 0.1)
Total$GPMTDIFFPER        <-  scales::percent(Total$GPMTDIFF/Total$GPMTPREV,accuracy = 0.1)




for(keyaccount in unique(EKAT_CUSTOMERS$Mapowanie)){
  steal <- subset(EKAT_CUSTOMERS,Mapowanie==keyaccount)
  frejm <- data.frame(Mapowanie=keyaccount,Klient=keyaccount, 
                      QActYear          = sum(steal$QActYear),
                      QprevYear         = sum(steal$QprevYear),
                      GPACT             = sum(steal$GPACT),
                      GPPREV            = sum(steal$GPPREV),
                      TURNACT           = sum(steal$TURNACT),
                      TURNPREV          = sum(steal$TURNPREV),
                      QuantityDiffYY    = sum(steal$QuantityDiffYY),
                      QuantityDiffYYPER = 0,
                      GPDiffYYPER       = 0,
                      GPDiffYY          = sum(steal$GPDiffYY),
                      TurnDiffYY        = sum(steal$TurnDiffYY),
                      TurnDiffYYPER     = 0,
                      GPMTACT           = sum(steal$GPMTACT),
                      GPMTPREV          = sum(steal$GPMTPREV),
                      GPMTDIFF          = sum(steal$GPMTDIFF),
                      GPMTDIFFPER       = 0)
  frejm$GPMTACT            <-  frejm$GPACT*1000/frejm$QActYear
  frejm$GPMTPREV           <-  frejm$GPPREV*1000/frejm$QprevYear
  frejm$TurnDiffYY         <-  frejm$TURNACT-frejm$TURNPREV
  frejm$QuantityDiffYY     <-  frejm$QActYear-frejm$QprevYear
  frejm$GPDiffYY           <-  frejm$GPACT-frejm$GPPREV
  frejm$GPMTDIFF           <-  frejm$GPMTACT-frejm$GPMTPREV
  frejm$QuantityDiffYYPER  <-  scales::percent(frejm$QuantityDiffYY/frejm$QprevYear,accuracy = 0.1)
  frejm$GPDiffYYPER        <-  scales::percent(frejm$GPDiffYY/frejm$GPPREV,accuracy = 0.1)
  frejm$TurnDiffYYPER      <-  scales::percent(frejm$TurnDiffYY/frejm$TURNPREV,accuracy = 0.1)
  frejm$GPMTDIFFPER        <-  scales::percent(frejm$GPMTDIFF/frejm$GPMTPREV,accuracy = 0.1)
  EKAT_CUSTOMERS <- rbind(EKAT_CUSTOMERS, frejm)
  
}

#GRUPOWANIE NORTH SOUTH CEE




EKAT_CUSTOMERS                      <- setorder(EKAT_CUSTOMERS,Mapowanie)
EKAT_CUSTOMERS                      <- rbind(EKAT_CUSTOMERS,Total)

WyboldowaneWierszeEKAT_CUSTOMERS    <- c(which(EKAT_CUSTOMERS$Klient %in% MapowaNieKeyECUS),length(EKAT_CUSTOMERS$Mapowanie))


# EKAT_CUSTOMERS         <- rbind(EKAT_CUSTOMERS,thereofCEE) na razie tą część tabelki trzeba odpóścić, do znalezienia rozwiązania


EKAT_CUSTOMERS$Mapowanie            <- NULL


EKAT_CUSTOMERS                      <- EKAT_CUSTOMERS %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
EKAT_CUSTOMERS[is.na(EKAT_CUSTOMERS)]         <- ""

# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

EKAT_CUSTOMERS          <- EKAT_CUSTOMERS %>% mutate_if(is.numeric, ~round(., 0)) 




####YTD EKAT CUSTOM

EKAT_CUSTOMERS_YTD         <- rbind(EKATCEEKAT_CUS,EKATCEEKAT_CEE_CUS)



EKAT_CUSTOMERS_YTD         <- EKAT_CUSTOMERS_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
#EKAT_CUSTOMERS_YTD         <- EKAT_CUSTOMERS_YTD[,-2]
EKAT_CUSTOMERS_YTD  <-EKAT_CUSTOMERS_YTD  %>% select(1,3,6:11)


# EKAT_CUSTOMERS_YTD <- rbind(EKAT_CUSTOMERS_YTD,EKAT_CUSTOMERS_YTD2)
EKAT_CUSTOMERS_YTD                        <- as.data.frame(EKAT_CUSTOMERS_YTD)
EKAT_CUSTOMERS_YTD                        <- EKAT_CUSTOMERS_YTD %>% group_by(Mapowanie,Klient) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
EKAT_CUSTOMERS_YTD[is.na(EKAT_CUSTOMERS_YTD)]      <- NA
EKAT_CUSTOMERS_YTD$QuantityDiffYY         <- EKAT_CUSTOMERS_YTD$QActYear-EKAT_CUSTOMERS_YTD$QprevYear
EKAT_CUSTOMERS_YTD[is.na(EKAT_CUSTOMERS_YTD)]      <- NA
EKAT_CUSTOMERS_YTD$QuantityDiffYYPER_P    <- as.numeric(ifelse(is.infinite(((EKAT_CUSTOMERS_YTD$QActYear-EKAT_CUSTOMERS_YTD$QprevYear)/EKAT_CUSTOMERS_YTD$QprevYear)),"",(EKAT_CUSTOMERS_YTD$QActYear-EKAT_CUSTOMERS_YTD$QprevYear)/EKAT_CUSTOMERS_YTD$QprevYear))
EKAT_CUSTOMERS_YTD$QuantityDiffYYPER      <- ifelse(is.infinite(((EKAT_CUSTOMERS_YTD$QActYear-EKAT_CUSTOMERS_YTD$QprevYear)/EKAT_CUSTOMERS_YTD$QprevYear)),"",scales::percent(((EKAT_CUSTOMERS_YTD$QActYear-EKAT_CUSTOMERS_YTD$QprevYear)/EKAT_CUSTOMERS_YTD$QprevYear),accuracy = 0.1))
EKAT_CUSTOMERS_YTD[is.na(EKAT_CUSTOMERS_YTD)]      <- NA
EKAT_CUSTOMERS_YTD$GPDiffYY               <- EKAT_CUSTOMERS_YTD$GPACT-EKAT_CUSTOMERS_YTD$GPPREV
EKAT_CUSTOMERS_YTD[is.na(EKAT_CUSTOMERS_YTD)]      <- NA
EKAT_CUSTOMERS_YTD$GPDiffYYPER            <- ifelse(is.infinite(((EKAT_CUSTOMERS_YTD$GPACT-EKAT_CUSTOMERS_YTD$GPPREV)/EKAT_CUSTOMERS_YTD$GPPREV)),"",scales::percent(((EKAT_CUSTOMERS_YTD$GPACT-EKAT_CUSTOMERS_YTD$GPPREV)/EKAT_CUSTOMERS_YTD$GPPREV),accuracy = 0.1))
#EKAT_CUSTOMERS_YTD$GPDiffYYPER       <- round(((EKAT_CUSTOMERS_YTD$GPACT-EKAT_CUSTOMERS_YTD$GPPREV)/EKAT_CUSTOMERS_YTD$GPPREV)*100,2)
EKAT_CUSTOMERS_YTD[is.na(EKAT_CUSTOMERS_YTD)]      <- NA
EKAT_CUSTOMERS_YTD$TurnDiffYY             <- EKAT_CUSTOMERS_YTD$TURNACT-EKAT_CUSTOMERS_YTD$TURNPREV
EKAT_CUSTOMERS_YTD[is.na(EKAT_CUSTOMERS_YTD)]      <- NA
EKAT_CUSTOMERS_YTD$TurnDiffYYPER          <- ifelse(is.infinite(((EKAT_CUSTOMERS_YTD$TURNACT-EKAT_CUSTOMERS_YTD$TURNPREV)/EKAT_CUSTOMERS_YTD$TURNPREV)),"",scales::percent(((EKAT_CUSTOMERS_YTD$TURNACT-EKAT_CUSTOMERS_YTD$TURNPREV)/EKAT_CUSTOMERS_YTD$TURNPREV),accuracy = 0.1))
EKAT_CUSTOMERS_YTD[is.na(EKAT_CUSTOMERS_YTD)]      <- NA
EKAT_CUSTOMERS_YTD$GPMTACT                <- ifelse(is.infinite(EKAT_CUSTOMERS_YTD$GPACT*1000/EKAT_CUSTOMERS_YTD$QActYear),0,EKAT_CUSTOMERS_YTD$GPACT*1000/EKAT_CUSTOMERS_YTD$QActYear)   # x 1000 ?
EKAT_CUSTOMERS_YTD[is.na(EKAT_CUSTOMERS_YTD)]      <- 0
EKAT_CUSTOMERS_YTD$GPMTPREV               <- ifelse(is.infinite(EKAT_CUSTOMERS_YTD$GPPREV*1000/EKAT_CUSTOMERS_YTD$QprevYear),0,EKAT_CUSTOMERS_YTD$GPPREV*1000/EKAT_CUSTOMERS_YTD$QprevYear)
EKAT_CUSTOMERS_YTD[is.na(EKAT_CUSTOMERS_YTD)]      <- 0
EKAT_CUSTOMERS_YTD$GPMTDIFF               <- EKAT_CUSTOMERS_YTD$GPMTACT-EKAT_CUSTOMERS_YTD$GPMTPREV
EKAT_CUSTOMERS_YTD[is.na(EKAT_CUSTOMERS_YTD)]      <- NA
EKAT_CUSTOMERS_YTD$GPMTDIFFPER            <- ifelse(is.infinite((EKAT_CUSTOMERS_YTD$GPMTACT-EKAT_CUSTOMERS_YTD$GPMTPREV)/EKAT_CUSTOMERS_YTD$GPMTPREV),"",scales::percent(((EKAT_CUSTOMERS_YTD$GPMTACT-EKAT_CUSTOMERS_YTD$GPMTPREV)/EKAT_CUSTOMERS_YTD$GPMTPREV),accuracy = 0.1))
EKAT_CUSTOMERS_YTD[is.na(EKAT_CUSTOMERS_YTD)]      <- NA

DT                               <- data.table(EKAT_CUSTOMERS_YTD)
EKAT_CUSTOMERS_YTD                        <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
EKAT_CUSTOMERS_YTD[is.na(EKAT_CUSTOMERS_YTD)]      <- NA
EKAT_CUSTOMERS_YTD$Klient            <- stri_replace_all_fixed(EKAT_CUSTOMERS_YTD$Klient, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
EKAT_CUSTOMERS_YTD                        <- setorder(EKAT_CUSTOMERS_YTD,Mapowanie,-QuantityDiffYY)
EKAT_CUSTOMERS_YTD$QuantityDiffYYPER_P    <- NULL


MapowaNieKeyECUS <- unique(EKAT_CUSTOMERS_YTD$Mapowanie)


Total <- data.frame(Mapowanie="Total CEE",Klient="Total CEE",
                    QActYear          = sum(EKAT_CUSTOMERS_YTD$QActYear),
                    QprevYear         = sum(EKAT_CUSTOMERS_YTD$QprevYear),
                    GPACT             = sum(EKAT_CUSTOMERS_YTD$GPACT),
                    GPPREV            = sum(EKAT_CUSTOMERS_YTD$GPPREV),
                    TURNACT           = sum(EKAT_CUSTOMERS_YTD$TURNACT),
                    TURNPREV          = sum(EKAT_CUSTOMERS_YTD$TURNPREV),
                    QuantityDiffYY    = 0,
                    QuantityDiffYYPER = 0,
                    GPDiffYYPER       = 0,
                    GPDiffYY          = 0,
                    TurnDiffYY        = sum(EKAT_CUSTOMERS_YTD$TurnDiffYY),
                    TurnDiffYYPER     = 0,
                    GPMTACT           = sum(EKAT_CUSTOMERS_YTD$GPMTACT),
                    GPMTPREV          = sum(EKAT_CUSTOMERS_YTD$GPMTPREV),
                    GPMTDIFF          = 0,
                    GPMTDIFFPER       = 0)

Total$GPMTACT            <-  Total$GPACT*1000/Total$QActYear
Total$GPMTPREV           <-  Total$GPPREV*1000/Total$QprevYear
Total$TurnDiffYY         <-  Total$TURNACT-Total$TURNPREV
Total$QuantityDiffYY     <-  Total$QActYear-Total$QprevYear
Total$GPDiffYY           <-  Total$GPACT-Total$GPPREV
Total$GPMTDIFF           <-  Total$GPMTACT-Total$GPMTPREV
Total$QuantityDiffYYPER  <-  scales::percent(Total$QuantityDiffYY/Total$QprevYear,accuracy = 0.1)
Total$GPDiffYYPER        <-  scales::percent(Total$GPDiffYY/Total$GPPREV,accuracy = 0.1)
Total$TurnDiffYYPER      <-  scales::percent(Total$TurnDiffYY/Total$TURNPREV,accuracy = 0.1)
Total$GPMTDIFFPER        <-  scales::percent(Total$GPMTDIFF/Total$GPMTPREV,accuracy = 0.1)




for(keyaccount in unique(EKAT_CUSTOMERS_YTD$Mapowanie)){
  steal <- subset(EKAT_CUSTOMERS_YTD,Mapowanie==keyaccount)
  frejm <- data.frame(Mapowanie=keyaccount,Klient=keyaccount, 
                      QActYear          = sum(steal$QActYear),
                      QprevYear         = sum(steal$QprevYear),
                      GPACT             = sum(steal$GPACT),
                      GPPREV            = sum(steal$GPPREV),
                      TURNACT           = sum(steal$TURNACT),
                      TURNPREV          = sum(steal$TURNPREV),
                      QuantityDiffYY    = sum(steal$QuantityDiffYY),
                      QuantityDiffYYPER = 0,
                      GPDiffYYPER       = 0,
                      GPDiffYY          = sum(steal$GPDiffYY),
                      TurnDiffYY        = sum(steal$TurnDiffYY),
                      TurnDiffYYPER     = 0,
                      GPMTACT           = sum(steal$GPMTACT),
                      GPMTPREV          = sum(steal$GPMTPREV),
                      GPMTDIFF          = sum(steal$GPMTDIFF),
                      GPMTDIFFPER       = 0)
  frejm$GPMTACT            <-  frejm$GPACT*1000/frejm$QActYear
  frejm$GPMTPREV           <-  frejm$GPPREV*1000/frejm$QprevYear
  frejm$TurnDiffYY         <-  frejm$TURNACT-frejm$TURNPREV
  frejm$QuantityDiffYY     <-  frejm$QActYear-frejm$QprevYear
  frejm$GPDiffYY           <-  frejm$GPACT-frejm$GPPREV
  frejm$GPMTDIFF           <-  frejm$GPMTACT-frejm$GPMTPREV
  frejm$QuantityDiffYYPER  <-  scales::percent(frejm$QuantityDiffYY/frejm$QprevYear,accuracy = 0.1)
  frejm$GPDiffYYPER        <-  scales::percent(frejm$GPDiffYY/frejm$GPPREV,accuracy = 0.1)
  frejm$TurnDiffYYPER      <-  scales::percent(frejm$TurnDiffYY/frejm$TURNPREV,accuracy = 0.1)
  frejm$GPMTDIFFPER        <-  scales::percent(frejm$GPMTDIFF/frejm$GPMTPREV,accuracy = 0.1)
  EKAT_CUSTOMERS_YTD <- rbind(EKAT_CUSTOMERS_YTD, frejm)
  
}

#GRUPOWANIE NORTH SOUTH CEE

EKAT_CUSTOMERS_YTD                      <- setorder(EKAT_CUSTOMERS_YTD,Mapowanie)
EKAT_CUSTOMERS_YTD                      <- rbind(EKAT_CUSTOMERS_YTD,Total)

WyboldowaneWierszeEKAT_CUSTOMERS_YTD    <- c(which(EKAT_CUSTOMERS_YTD$Klient %in% MapowaNieKeyECUS),length(EKAT_CUSTOMERS_YTD$Mapowanie))


# EKAT_CUSTOMERS_YTD         <- rbind(EKAT_CUSTOMERS_YTD,thereofCEE) na razie tą część tabelki trzeba odpóścić, do znalezienia rozwiązania


EKAT_CUSTOMERS_YTD$Mapowanie            <- NULL


EKAT_CUSTOMERS_YTD                                    <- EKAT_CUSTOMERS_YTD %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
EKAT_CUSTOMERS_YTD[is.na(EKAT_CUSTOMERS_YTD)]         <- ""

# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

EKAT_CUSTOMERS_YTD          <- EKAT_CUSTOMERS_YTD %>% mutate_if(is.numeric, ~round(., 0)) 



