
MainSourceWhDir               <- read_xlsx(here::here("sources","DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "Cleaning",range="I16:X60000", col_names = TRUE,na = "NA")


MainSourceWhDir                    <- MainSourceWhDir%>% filter_all(any_vars(!is.na(.))) 
MainSourceWhDir  [is.na(MainSourceWhDir)]   <- 0   # wszystkie pola bez wartości wypełniamy zerami


MainSourceWhDir                  <- left_join(MainSourceWhDir,MapowanieBaltics,by="Subsidiary Country")

MAIN_WhDir_MTH<-MainSourceWhDir 


colnames(MAIN_WhDir_MTH)[11]     <- Naglowek1
colnames(MAIN_WhDir_MTH)[12]     <- Naglowek2
colnames(MAIN_WhDir_MTH)[13]     <- Naglowek3
colnames(MAIN_WhDir_MTH)[14]     <- Naglowek4
colnames(MAIN_WhDir_MTH)[15]     <- Naglowek5
colnames(MAIN_WhDir_MTH)[16]     <- Naglowek6


MAIN_WhDir_MTH                   <- MAIN_WhDir_MTH %>% filter(`Calendar month`==MiesiacAnalizy)
MAIN_WhDir_MTH                   <- MAIN_WhDir_MTH %>% select(17,18,11,12,13,14,15,16,17,7)

# MAIN_WhDir_MTH <- rbind(MAIN_WhDir_MTH,MAIN_WhDir_MTH2)
MAIN_WhDir_MTH                        <- as.data.frame(MAIN_WhDir_MTH)
MAIN_WhDir_MTH                        <- MAIN_WhDir_MTH %>% group_by(Region,SubRegion,WhsDirComm) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
MAIN_WhDir_MTH[is.na(MAIN_WhDir_MTH)]      <- NA
MAIN_WhDir_MTH$QuantityDiffYY         <- MAIN_WhDir_MTH$QActYear-MAIN_WhDir_MTH$QprevYear
MAIN_WhDir_MTH[is.na(MAIN_WhDir_MTH)]      <- NA
MAIN_WhDir_MTH$QuantityDiffYYPER_P    <- as.numeric(ifelse(is.infinite(((MAIN_WhDir_MTH$QActYear-MAIN_WhDir_MTH$QprevYear)/MAIN_WhDir_MTH$QprevYear)),"",(MAIN_WhDir_MTH$QActYear-MAIN_WhDir_MTH$QprevYear)/MAIN_WhDir_MTH$QprevYear))
MAIN_WhDir_MTH$QuantityDiffYYPER_P    <- ifelse(is.na(MAIN_WhDir_MTH$QuantityDiffYYPER_P),"0",MAIN_WhDir_MTH$QuantityDiffYYPER_P)
MAIN_WhDir_MTH$QuantityDiffYYPER      <- ifelse(is.infinite(((MAIN_WhDir_MTH$QActYear-MAIN_WhDir_MTH$QprevYear)/MAIN_WhDir_MTH$QprevYear)),"",scales::percent(((MAIN_WhDir_MTH$QActYear-MAIN_WhDir_MTH$QprevYear)/MAIN_WhDir_MTH$QprevYear),accuracy = 0.1))
MAIN_WhDir_MTH$QuantityDiffYYPER      <- ifelse(is.na(MAIN_WhDir_MTH$QuantityDiffYYPER),"0",MAIN_WhDir_MTH$QuantityDiffYYPER)


MAIN_WhDir_MTH[is.na(MAIN_WhDir_MTH)]      <- NA
MAIN_WhDir_MTH$GPDiffYY               <- MAIN_WhDir_MTH$GPACT-MAIN_WhDir_MTH$GPPREV
MAIN_WhDir_MTH[is.na(MAIN_WhDir_MTH)]      <- NA
MAIN_WhDir_MTH$GPDiffYYPER            <- ifelse(is.infinite(((MAIN_WhDir_MTH$GPACT-MAIN_WhDir_MTH$GPPREV)/MAIN_WhDir_MTH$GPPREV)),"",scales::percent(((MAIN_WhDir_MTH$GPACT-MAIN_WhDir_MTH$GPPREV)/MAIN_WhDir_MTH$GPPREV),accuracy = 0.1))
#MAIN_WhDir_MTH$GPDiffYYPER       <- round(((MAIN_WhDir_MTH$GPACT-MAIN_WhDir_MTH$GPPREV)/MAIN_WhDir_MTH$GPPREV)*100,2)
MAIN_WhDir_MTH[is.na(MAIN_WhDir_MTH)]      <- NA
MAIN_WhDir_MTH$TurnDiffYY             <- MAIN_WhDir_MTH$TURNACT-MAIN_WhDir_MTH$TURNPREV
MAIN_WhDir_MTH[is.na(MAIN_WhDir_MTH)]      <- NA
MAIN_WhDir_MTH$TurnDiffYYPER          <- ifelse(is.infinite(((MAIN_WhDir_MTH$TURNACT-MAIN_WhDir_MTH$TURNPREV)/MAIN_WhDir_MTH$TURNPREV)),"",scales::percent(((MAIN_WhDir_MTH$TURNACT-MAIN_WhDir_MTH$TURNPREV)/MAIN_WhDir_MTH$TURNPREV),accuracy = 0.1))
MAIN_WhDir_MTH$TurnDiffYYPER          <- ifelse(is.na(MAIN_WhDir_MTH$TurnDiffYYPER),"0",MAIN_WhDir_MTH$TurnDiffYYPER)

MAIN_WhDir_MTH[is.na(MAIN_WhDir_MTH)]      <- NA
MAIN_WhDir_MTH$GPMTACT                <- ifelse(is.infinite(MAIN_WhDir_MTH$GPACT*1000/MAIN_WhDir_MTH$QActYear),0,MAIN_WhDir_MTH$GPACT*1000/MAIN_WhDir_MTH$QActYear)   # x 1000 ?
MAIN_WhDir_MTH[is.na(MAIN_WhDir_MTH)]      <- 0
MAIN_WhDir_MTH$GPMTPREV               <- ifelse(is.infinite(MAIN_WhDir_MTH$GPPREV*1000/MAIN_WhDir_MTH$QprevYear),0,MAIN_WhDir_MTH$GPPREV*1000/MAIN_WhDir_MTH$QprevYear)
MAIN_WhDir_MTH[is.na(MAIN_WhDir_MTH)]      <- 0
MAIN_WhDir_MTH$GPMTDIFF               <- MAIN_WhDir_MTH$GPMTACT-MAIN_WhDir_MTH$GPMTPREV
MAIN_WhDir_MTH[is.na(MAIN_WhDir_MTH)]      <- NA
MAIN_WhDir_MTH$GPMTDIFFPER            <- ifelse(is.infinite((MAIN_WhDir_MTH$GPMTACT-MAIN_WhDir_MTH$GPMTPREV)/MAIN_WhDir_MTH$GPMTPREV),"",scales::percent(((MAIN_WhDir_MTH$GPMTACT-MAIN_WhDir_MTH$GPMTPREV)/MAIN_WhDir_MTH$GPMTPREV),accuracy = 0.1))
MAIN_WhDir_MTH$GPMTDIFFPER            <- ifelse(is.na(MAIN_WhDir_MTH$GPMTDIFFPER),"0",MAIN_WhDir_MTH$GPMTDIFFPER)
MAIN_WhDir_MTH[is.na(MAIN_WhDir_MTH)] <- NA

DT                                    <- data.table(MAIN_WhDir_MTH)
MAIN_WhDir_MTH                        <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
MAIN_WhDir_MTH[is.na(MAIN_WhDir_MTH)]      <- NA
MAIN_WhDir_MTH$SubRegion              <- stri_replace_all_fixed(MAIN_WhDir_MTH$SubRegion, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
MAIN_WhDir_MTH                        <- setorder(MAIN_WhDir_MTH,Region,SubRegion,-QActYear)
MAIN_WhDir_MTH$QuantityDiffYYPER_P    <- NULL

thereofCEE <-MAIN_WhDir_MTH %>% group_by(WhsDirComm) %>% summarize(QActYear=sum(QActYear),QprevYear=sum(QprevYear),GPACT=sum(GPACT),GPPREV=sum(GPPREV),TURNACT=sum(TURNACT),TURNPREV=sum(TURNPREV))

thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$QuantityDiffYY          <- thereofCEE$QActYear-thereofCEE$QprevYear
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$QuantityDiffYYPER_P     <- as.numeric(ifelse(is.infinite(((thereofCEE$QActYear-thereofCEE$QprevYear)/thereofCEE$QprevYear)),"",(thereofCEE$QActYear-thereofCEE$QprevYear)/thereofCEE$QprevYear))
thereofCEE$QuantityDiffYYPER       <- ifelse(is.infinite(((thereofCEE$QActYear-thereofCEE$QprevYear)/thereofCEE$QprevYear)),"",scales::percent(((thereofCEE$QActYear-thereofCEE$QprevYear)/thereofCEE$QprevYear),accuracy = 0.1))
thereofCEE$QuantityDiffYYPER       <- ifelse(is.na(thereofCEE$QuantityDiffYYPER),"0",thereofCEE$QuantityDiffYYPER )
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$GPDiffYY                <- thereofCEE$GPACT-thereofCEE$GPPREV
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$GPDiffYYPER             <- ifelse(is.infinite(((thereofCEE$GPACT-thereofCEE$GPPREV)/thereofCEE$GPPREV)),"",scales::percent(((thereofCEE$GPACT-thereofCEE$GPPREV)/thereofCEE$GPPREV),accuracy = 0.1))
thereofCEE$GPDiffYYPER             <- ifelse(is.na(thereofCEE$GPDiffYYPER ),"0",thereofCEE$GPDiffYYPER  )
#thereofCEE$GPDiffYYPER       <- round(((thereofCEE$GPACT-thereofCEE$GPPREV)/thereofCEE$GPPREV)*100,2)
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$TurnDiffYY              <- thereofCEE$TURNACT-thereofCEE$TURNPREV
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$TurnDiffYYPER           <- ifelse(is.infinite(((thereofCEE$TURNACT-thereofCEE$TURNPREV)/thereofCEE$TURNPREV)),"",scales::percent(((thereofCEE$TURNACT-thereofCEE$TURNPREV)/thereofCEE$TURNPREV),accuracy = 0.1))
thereofCEE$TurnDiffYYPER           <- ifelse(is.na(thereofCEE$TurnDiffYYPER    ),"0",thereofCEE$TurnDiffYYPER     )
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$GPMTACT                 <- ifelse(is.infinite(thereofCEE$GPACT*1000/thereofCEE$QActYear),0,thereofCEE$GPACT*1000/thereofCEE$QActYear)   # x 1000 ?
thereofCEE[is.na(thereofCEE)]      <- 0
thereofCEE$GPMTPREV                <- ifelse(is.infinite(thereofCEE$GPPREV*1000/thereofCEE$QprevYear),0,thereofCEE$GPPREV*1000/thereofCEE$QprevYear)
thereofCEE[is.na(thereofCEE)]      <- 0
thereofCEE$GPMTDIFF                <- thereofCEE$GPMTACT-thereofCEE$GPMTPREV
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$GPMTDIFFPER             <- ifelse(is.infinite((thereofCEE$GPMTACT-thereofCEE$GPMTPREV)/thereofCEE$GPMTPREV),"",scales::percent(((thereofCEE$GPMTACT-thereofCEE$GPMTPREV)/thereofCEE$GPMTPREV),accuracy = 0.1))
thereofCEE$GPMTDIFFPER             <- ifelse(is.na(thereofCEE$GPMTDIFFPER),"0",thereofCEE$GPMTDIFFPER)
thereofCEE[is.na(thereofCEE)]      <- NA


DT                                <- data.table(thereofCEE)
thereofCEE                        <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
thereofCEE[is.na(thereofCEE)]     <- NA
thereofCEE$SubRegion              <- ""
thereofCEE$SubRegion[1]           <- "thereof"
thereofCEE                        <- setorder(thereofCEE,-QActYear)
thereofCEE$QuantityDiffYYPER_P    <- NULL

thereofCEE$Region<-""


thereofCEE    <- thereofCEE %>% select(19,18,1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
thereofCEE    <- thereofCEE %>% mutate_if(is.numeric, ~round(., 0)) 



RegionyCEE <- unique(MAIN_WhDir_MTH$Region)


Total <- data.frame(Region="Total CEE",SubRegion="Total CEE", WhsDirComm="Total CEE thereof",
                    QActYear          = sum(MAIN_WhDir_MTH$QActYear),
                    QprevYear         = sum(MAIN_WhDir_MTH$QprevYear),
                    GPACT             = sum(MAIN_WhDir_MTH$GPACT),
                    GPPREV            = sum(MAIN_WhDir_MTH$GPPREV),
                    TURNACT           = sum(MAIN_WhDir_MTH$TURNACT),
                    TURNPREV          = sum(MAIN_WhDir_MTH$TURNPREV),
                    QuantityDiffYY    = 0,
                    QuantityDiffYYPER = 0,
                    GPDiffYYPER       = 0,
                    GPDiffYY          = 0,
                    TurnDiffYY        = sum(MAIN_WhDir_MTH$TurnDiffYY),
                    TurnDiffYYPER     = 0,
                    GPMTACT           = 0,
                    GPMTPREV          = 0,
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

Regiony <- unique(MAIN_WhDir_MTH$SubRegion)

for(region in unique(MAIN_WhDir_MTH$SubRegion)){
  steal <- subset(MAIN_WhDir_MTH,SubRegion==region)
  frejm <- data.frame(Region=NA,SubRegion=region, WhsDirComm=region,
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
                      GPMTACT           = 0,
                      GPMTPREV          = 0,
                      GPMTDIFF          = 0,
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
  MAIN_WhDir_MTH           <-  rbind(MAIN_WhDir_MTH, frejm)
  
}

#GRUPOWANIE NORTH SOUTH CEE



for(region in RegionyCEE){
  
  steal <- subset(MAIN_WhDir_MTH,Region==region)
  frejm <- data.frame(Region=region,SubRegion=paste("TOTAL", region), WhsDirComm=paste("TOTAL",region),
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
                      GPMTACT           = 0,
                      GPMTPREV          = 0,
                      GPMTDIFF          = 0,
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
  MAIN_WhDir_MTH <- rbind(MAIN_WhDir_MTH, frejm)
  
}



MapowaniePomocnicze            <- MapowanieBaltics
MapowaniePomocnicze$SubRegion  <- stri_replace_all_fixed(MapowaniePomocnicze$SubRegion , pattern = c("&"), replacement = c(""), vectorize_all = FALSE)

MapowaniePomocnicze            <- MapowaniePomocnicze %>% select(2,3) %>% distinct
colnames(MapowaniePomocnicze)[1] <- "RegionU"

MAIN_WhDir_MTH                      <- left_join(MAIN_WhDir_MTH,MapowaniePomocnicze,by="SubRegion")
MAIN_WhDir_MTH$Region               <- ifelse(is.na(MAIN_WhDir_MTH$Region),MAIN_WhDir_MTH$RegionU,MAIN_WhDir_MTH$Region)
MAIN_WhDir_MTH$RegionU              <- NULL


MAIN_WhDir_MTH                      <- setorder(MAIN_WhDir_MTH,Region,SubRegion)
MAIN_WhDir_MTH                      <- rbind(MAIN_WhDir_MTH,Total)

WyboldowaneWierszeMAIN_WhDir_MTH    <- c(which(MAIN_WhDir_MTH$WhsDirComm %in% Regiony),length(MAIN_WhDir_MTH$Region))
WyboldowaneWierszeMAIN_WhDir_MTH    <- c(which(str_sub(MAIN_WhDir_MTH$WhsDirComm,7,9) == "CEE"),WyboldowaneWierszeMAIN_WhDir_MTH)

MAIN_WhDir_MTH                      <- rbind(MAIN_WhDir_MTH,thereofCEE)#  na razie tą część tabelki trzeba odpóścić, do znalezienia rozwiązania





MAIN_WhDir_MTH$Region               <- NULL
MAIN_WhDir_MTH$SubRegion            <- NULL

MAIN_WhDir_MTH                      <- MAIN_WhDir_MTH %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
MAIN_WhDir_MTH[is.na(MAIN_WhDir_MTH)]    <- 0
#tutaj wybambia nam zamianę liczbowego 

# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

MAIN_WhDir_MTH          <- MAIN_WhDir_MTH %>% mutate_if(is.numeric, ~round(., 0)) 

MAIN_WhDir_MTH









# YTD -------------------------

MAIN_WhDir_YTD<-MainSourceWhDir 

colnames(MAIN_WhDir_YTD)[11]     <- Naglowek1
colnames(MAIN_WhDir_YTD)[12]     <- Naglowek2
colnames(MAIN_WhDir_YTD)[13]     <- Naglowek3
colnames(MAIN_WhDir_YTD)[14]     <- Naglowek4
colnames(MAIN_WhDir_YTD)[15]     <- Naglowek5
colnames(MAIN_WhDir_YTD)[16]     <- Naglowek6

MAIN_WhDir_YTD                  <- MAIN_WhDir_YTD%>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
MAIN_WhDir_YTD                  <- MAIN_WhDir_YTD%>% select(17,18,11,12,13,14,15,16,17,7)

# MAIN_WhDir_YTD<- rbind(MAIN_WhDir_YTD,MAIN_WhDir_YTD2)
MAIN_WhDir_YTD                        <- as.data.frame(MAIN_WhDir_YTD)
MAIN_WhDir_YTD                        <- MAIN_WhDir_YTD%>% group_by(Region,SubRegion,WhsDirComm) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
MAIN_WhDir_YTD[is.na(MAIN_WhDir_YTD)] <- NA
MAIN_WhDir_YTD$QuantityDiffYY         <- MAIN_WhDir_YTD$QActYear-MAIN_WhDir_YTD$QprevYear
MAIN_WhDir_YTD[is.na(MAIN_WhDir_YTD)] <- NA
MAIN_WhDir_YTD$QuantityDiffYYPER_P    <- as.numeric(ifelse(is.infinite(((MAIN_WhDir_YTD$QActYear-MAIN_WhDir_YTD$QprevYear)/MAIN_WhDir_YTD$QprevYear)),"",(MAIN_WhDir_YTD$QActYear-MAIN_WhDir_YTD$QprevYear)/MAIN_WhDir_YTD$QprevYear))
MAIN_WhDir_YTD$QuantityDiffYYPER_P    <- ifelse(is.na(MAIN_WhDir_YTD$QuantityDiffYYPER_P),"0",MAIN_WhDir_YTD$QuantityDiffYYPER_P)
MAIN_WhDir_YTD$QuantityDiffYYPER      <- ifelse(is.infinite(((MAIN_WhDir_YTD$QActYear-MAIN_WhDir_YTD$QprevYear)/MAIN_WhDir_YTD$QprevYear)),"",scales::percent(((MAIN_WhDir_YTD$QActYear-MAIN_WhDir_YTD$QprevYear)/MAIN_WhDir_YTD$QprevYear),accuracy = 0.1))
MAIN_WhDir_YTD$QuantityDiffYYPER      <- ifelse(is.na(MAIN_WhDir_YTD$QuantityDiffYYPER),"0",MAIN_WhDir_YTD$QuantityDiffYYPER)


MAIN_WhDir_YTD[is.na(MAIN_WhDir_YTD)]      <- NA
MAIN_WhDir_YTD$GPDiffYY               <- MAIN_WhDir_YTD$GPACT-MAIN_WhDir_YTD$GPPREV
MAIN_WhDir_YTD[is.na(MAIN_WhDir_YTD)]      <- NA
MAIN_WhDir_YTD$GPDiffYYPER            <- ifelse(is.infinite(((MAIN_WhDir_YTD$GPACT-MAIN_WhDir_YTD$GPPREV)/MAIN_WhDir_YTD$GPPREV)),"",scales::percent(((MAIN_WhDir_YTD$GPACT-MAIN_WhDir_YTD$GPPREV)/MAIN_WhDir_YTD$GPPREV),accuracy = 0.1))
#MAIN_WhDir_YTD$GPDiffYYPER       <- round(((MAIN_WhDir_YTD$GPACT-MAIN_WhDir_YTD$GPPREV)/MAIN_WhDir_YTD$GPPREV)*100,2)
MAIN_WhDir_YTD[is.na(MAIN_WhDir_YTD)]      <- NA
MAIN_WhDir_YTD$TurnDiffYY             <- MAIN_WhDir_YTD$TURNACT-MAIN_WhDir_YTD$TURNPREV
MAIN_WhDir_YTD[is.na(MAIN_WhDir_YTD)]      <- NA
MAIN_WhDir_YTD$TurnDiffYYPER          <- ifelse(is.infinite(((MAIN_WhDir_YTD$TURNACT-MAIN_WhDir_YTD$TURNPREV)/MAIN_WhDir_YTD$TURNPREV)),"",scales::percent(((MAIN_WhDir_YTD$TURNACT-MAIN_WhDir_YTD$TURNPREV)/MAIN_WhDir_YTD$TURNPREV),accuracy = 0.1))
MAIN_WhDir_YTD$TurnDiffYYPER          <- ifelse(is.na(MAIN_WhDir_YTD$TurnDiffYYPER),"0",MAIN_WhDir_YTD$TurnDiffYYPER)

MAIN_WhDir_YTD[is.na(MAIN_WhDir_YTD)]      <- NA
MAIN_WhDir_YTD$GPMTACT                <- ifelse(is.infinite(MAIN_WhDir_YTD$GPACT*1000/MAIN_WhDir_YTD$QActYear),0,MAIN_WhDir_YTD$GPACT*1000/MAIN_WhDir_YTD$QActYear)   # x 1000 ?
MAIN_WhDir_YTD[is.na(MAIN_WhDir_YTD)]      <- 0
MAIN_WhDir_YTD$GPMTPREV               <- ifelse(is.infinite(MAIN_WhDir_YTD$GPPREV*1000/MAIN_WhDir_YTD$QprevYear),0,MAIN_WhDir_YTD$GPPREV*1000/MAIN_WhDir_YTD$QprevYear)
MAIN_WhDir_YTD[is.na(MAIN_WhDir_YTD)]      <- 0
MAIN_WhDir_YTD$GPMTDIFF               <- MAIN_WhDir_YTD$GPMTACT-MAIN_WhDir_YTD$GPMTPREV
MAIN_WhDir_YTD[is.na(MAIN_WhDir_YTD)]      <- NA
MAIN_WhDir_YTD$GPMTDIFFPER            <- ifelse(is.infinite((MAIN_WhDir_YTD$GPMTACT-MAIN_WhDir_YTD$GPMTPREV)/MAIN_WhDir_YTD$GPMTPREV),"",scales::percent(((MAIN_WhDir_YTD$GPMTACT-MAIN_WhDir_YTD$GPMTPREV)/MAIN_WhDir_YTD$GPMTPREV),accuracy = 0.1))
MAIN_WhDir_YTD$GPMTDIFFPER            <- ifelse(is.na(MAIN_WhDir_YTD$GPMTDIFFPER),"0",MAIN_WhDir_YTD$GPMTDIFFPER)
MAIN_WhDir_YTD[is.na(MAIN_WhDir_YTD)] <- NA

DT                                    <- data.table(MAIN_WhDir_YTD)
MAIN_WhDir_YTD                       <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
MAIN_WhDir_YTD[is.na(MAIN_WhDir_YTD)]      <- NA
MAIN_WhDir_YTD$SubRegion              <- stri_replace_all_fixed(MAIN_WhDir_YTD$SubRegion, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
MAIN_WhDir_YTD                       <- setorder(MAIN_WhDir_YTD,Region,SubRegion,-QActYear)
MAIN_WhDir_YTD$QuantityDiffYYPER_P    <- NULL

thereofCEE <-MAIN_WhDir_YTD%>% group_by(WhsDirComm) %>% summarize(QActYear=sum(QActYear),QprevYear=sum(QprevYear),GPACT=sum(GPACT),GPPREV=sum(GPPREV),TURNACT=sum(TURNACT),TURNPREV=sum(TURNPREV))

thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$QuantityDiffYY          <- thereofCEE$QActYear-thereofCEE$QprevYear
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$QuantityDiffYYPER_P     <- as.numeric(ifelse(is.infinite(((thereofCEE$QActYear-thereofCEE$QprevYear)/thereofCEE$QprevYear)),"",(thereofCEE$QActYear-thereofCEE$QprevYear)/thereofCEE$QprevYear))
thereofCEE$QuantityDiffYYPER       <- ifelse(is.infinite(((thereofCEE$QActYear-thereofCEE$QprevYear)/thereofCEE$QprevYear)),"",scales::percent(((thereofCEE$QActYear-thereofCEE$QprevYear)/thereofCEE$QprevYear),accuracy = 0.1))
thereofCEE$QuantityDiffYYPER       <- ifelse(is.na(thereofCEE$QuantityDiffYYPER),"0",thereofCEE$QuantityDiffYYPER )
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$GPDiffYY                <- thereofCEE$GPACT-thereofCEE$GPPREV
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$GPDiffYYPER             <- ifelse(is.infinite(((thereofCEE$GPACT-thereofCEE$GPPREV)/thereofCEE$GPPREV)),"",scales::percent(((thereofCEE$GPACT-thereofCEE$GPPREV)/thereofCEE$GPPREV),accuracy = 0.1))
thereofCEE$GPDiffYYPER             <- ifelse(is.na(thereofCEE$GPDiffYYPER ),"0",thereofCEE$GPDiffYYPER  )
#thereofCEE$GPDiffYYPER       <- round(((thereofCEE$GPACT-thereofCEE$GPPREV)/thereofCEE$GPPREV)*100,2)
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$TurnDiffYY              <- thereofCEE$TURNACT-thereofCEE$TURNPREV
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$TurnDiffYYPER           <- ifelse(is.infinite(((thereofCEE$TURNACT-thereofCEE$TURNPREV)/thereofCEE$TURNPREV)),"",scales::percent(((thereofCEE$TURNACT-thereofCEE$TURNPREV)/thereofCEE$TURNPREV),accuracy = 0.1))
thereofCEE$TurnDiffYYPER           <- ifelse(is.na(thereofCEE$TurnDiffYYPER    ),"0",thereofCEE$TurnDiffYYPER     )
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$GPMTACT                 <- ifelse(is.infinite(thereofCEE$GPACT*1000/thereofCEE$QActYear),0,thereofCEE$GPACT*1000/thereofCEE$QActYear)   # x 1000 ?
thereofCEE[is.na(thereofCEE)]      <- 0
thereofCEE$GPMTPREV                <- ifelse(is.infinite(thereofCEE$GPPREV*1000/thereofCEE$QprevYear),0,thereofCEE$GPPREV*1000/thereofCEE$QprevYear)
thereofCEE[is.na(thereofCEE)]      <- 0
thereofCEE$GPMTDIFF                <- thereofCEE$GPMTACT-thereofCEE$GPMTPREV
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$GPMTDIFFPER             <- ifelse(is.infinite((thereofCEE$GPMTACT-thereofCEE$GPMTPREV)/thereofCEE$GPMTPREV),"",scales::percent(((thereofCEE$GPMTACT-thereofCEE$GPMTPREV)/thereofCEE$GPMTPREV),accuracy = 0.1))
thereofCEE$GPMTDIFFPER             <- ifelse(is.na(thereofCEE$GPMTDIFFPER),"0",thereofCEE$GPMTDIFFPER)
thereofCEE[is.na(thereofCEE)]      <- NA


DT                                <- data.table(thereofCEE)
thereofCEE                        <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
thereofCEE[is.na(thereofCEE)]     <- NA
thereofCEE$SubRegion              <- ""
thereofCEE$SubRegion[1]           <- "thereof"
thereofCEE                        <- setorder(thereofCEE,-QActYear)
thereofCEE$QuantityDiffYYPER_P    <- NULL

thereofCEE$Region<-""


thereofCEE    <- thereofCEE %>% select(19,18,1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
thereofCEE    <- thereofCEE %>% mutate_if(is.numeric, ~round(., 0)) 



RegionyCEE <- unique(MAIN_WhDir_YTD$Region)


Total <- data.frame(Region="Total CEE",SubRegion="Total CEE", WhsDirComm="Total CEE thereof",
                    QActYear          = sum(MAIN_WhDir_YTD$QActYear),
                    QprevYear         = sum(MAIN_WhDir_YTD$QprevYear),
                    GPACT             = sum(MAIN_WhDir_YTD$GPACT),
                    GPPREV            = sum(MAIN_WhDir_YTD$GPPREV),
                    TURNACT           = sum(MAIN_WhDir_YTD$TURNACT),
                    TURNPREV          = sum(MAIN_WhDir_YTD$TURNPREV),
                    QuantityDiffYY    = 0,
                    QuantityDiffYYPER = 0,
                    GPDiffYYPER       = 0,
                    GPDiffYY          = 0,
                    TurnDiffYY        = sum(MAIN_WhDir_YTD$TurnDiffYY),
                    TurnDiffYYPER     = 0,
                    GPMTACT           = 0,
                    GPMTPREV          = 0,
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

Regiony <- unique(MAIN_WhDir_YTD$SubRegion)

for(region in unique(MAIN_WhDir_YTD$SubRegion)){
  steal <- subset(MAIN_WhDir_YTD,SubRegion==region)
  frejm <- data.frame(Region=NA,SubRegion=region, WhsDirComm=region,
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
                      GPMTACT           = 0,
                      GPMTPREV          = 0,
                      GPMTDIFF          = 0,
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
  MAIN_WhDir_YTD<- rbind(MAIN_WhDir_YTD, frejm)
  
}

#GRUPOWANIE NORTH SOUTH CEE



for(region in RegionyCEE){
  
  steal <- subset(MAIN_WhDir_YTD,Region==region)
  frejm <- data.frame(Region=region,SubRegion=paste("TOTAL", region), WhsDirComm=paste("TOTAL",region),
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
                      GPMTACT           = 0,
                      GPMTPREV          = 0,
                      GPMTDIFF          = 0,
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
  MAIN_WhDir_YTD<- rbind(MAIN_WhDir_YTD, frejm)
  
}



MapowaniePomocnicze            <- MapowanieBaltics
MapowaniePomocnicze$SubRegion  <- stri_replace_all_fixed(MapowaniePomocnicze$SubRegion , pattern = c("&"), replacement = c(""), vectorize_all = FALSE)

MapowaniePomocnicze            <- MapowaniePomocnicze %>% select(2,3) %>% distinct
colnames(MapowaniePomocnicze)[1] <- "RegionU"

MAIN_WhDir_YTD                     <- left_join(MAIN_WhDir_YTD,MapowaniePomocnicze,by="SubRegion")
MAIN_WhDir_YTD$Region               <- ifelse(is.na(MAIN_WhDir_YTD$Region),MAIN_WhDir_YTD$RegionU,MAIN_WhDir_YTD$Region)
MAIN_WhDir_YTD$RegionU              <- NULL


MAIN_WhDir_YTD                     <- setorder(MAIN_WhDir_YTD,Region,SubRegion)
MAIN_WhDir_YTD                     <- rbind(MAIN_WhDir_YTD,Total)

WyboldowaneWierszeMAIN_WhDir_YTD   <- c(which(MAIN_WhDir_YTD$WhsDirComm %in% Regiony),length(MAIN_WhDir_YTD$Region))
WyboldowaneWierszeMAIN_WhDir_YTD   <- c(which(str_sub(MAIN_WhDir_YTD$WhsDirComm,7,9) == "CEE"),WyboldowaneWierszeMAIN_WhDir_YTD)

MAIN_WhDir_YTD                     <- rbind(MAIN_WhDir_YTD,thereofCEE)#  na razie tą część tabelki trzeba odpóścić, do znalezienia rozwiązania



MAIN_WhDir_YTD$Region               <- NULL
MAIN_WhDir_YTD$SubRegion            <- NULL

MAIN_WhDir_YTD                      <- MAIN_WhDir_YTD%>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
MAIN_WhDir_YTD[is.na(MAIN_WhDir_YTD)]    <- 0


# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

MAIN_WhDir_YTD         <- MAIN_WhDir_YTD%>% mutate_if(is.numeric, ~round(., 0)) 

MAIN_WhDir_YTD


















