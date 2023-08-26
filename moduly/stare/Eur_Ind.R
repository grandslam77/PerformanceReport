#MONTH

MainSource                      <- read_xlsx(here::here("sources","DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "BES_BSP",range="I16:Y60000", col_names = TRUE,na = "NA")
MainSource                      <- MainSource  %>% filter_all(any_vars(!is.na(.))) 
MainSource [is.na(MainSource)]  <- 0   # wszystkie pola bez wartości wypełniamy zerami
MainSource                      <- left_join(MainSource,MapowanieBaltics,by="Subsidiary Country")
MainSource                      <- left_join(MainSource,MapowanieBESBSP,by="European Industry")
MainSource                      <- left_join(MainSource,MapowanieESSP,by="Material Division")





MAIN_EUR_IND <- left_join(MainSource,MapowanieBranzaIndu,by="European Industry")

colnames(MAIN_EUR_IND)[12]<-Naglowek1
colnames(MAIN_EUR_IND)[13]<-Naglowek2
colnames(MAIN_EUR_IND)[14]<-Naglowek3
colnames(MAIN_EUR_IND)[15]<-Naglowek4
colnames(MAIN_EUR_IND)[16]<-Naglowek5
colnames(MAIN_EUR_IND)[17]<-Naglowek6


MAIN_EUR_IND                        <- MAIN_EUR_IND %>% filter(`Calendar month`==MiesiacAnalizy)
MAIN_EUR_IND                        <- MAIN_EUR_IND %>% select(20,22,12,13,14,15,16,17)

# MAIN_EUR_IND<- rbind(MAIN_EUR_IND,MAIN_EUR_IND2)
MAIN_EUR_IND                        <- as.data.frame(MAIN_EUR_IND)
MAIN_EUR_IND                        <- MAIN_EUR_IND%>% group_by(MapowanieBranza,MapowanieBranzaInd) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
MAIN_EUR_IND[is.na(MAIN_EUR_IND)]   <- NA
MAIN_EUR_IND$QuantityDiffYY         <- MAIN_EUR_IND$QActYear-MAIN_EUR_IND$QprevYear
MAIN_EUR_IND[is.na(MAIN_EUR_IND)]   <- NA
MAIN_EUR_IND$QuantityDiffYYPER_P    <- as.numeric(ifelse(is.infinite(((MAIN_EUR_IND$QActYear-MAIN_EUR_IND$QprevYear)/MAIN_EUR_IND$QprevYear)),"",(MAIN_EUR_IND$QActYear-MAIN_EUR_IND$QprevYear)/MAIN_EUR_IND$QprevYear))
MAIN_EUR_IND$QuantityDiffYYPER_P    <- ifelse(is.na(MAIN_EUR_IND$QuantityDiffYYPER_P),"0",MAIN_EUR_IND$QuantityDiffYYPER_P)
MAIN_EUR_IND$QuantityDiffYYPER      <- ifelse(is.infinite(((MAIN_EUR_IND$QActYear-MAIN_EUR_IND$QprevYear)/MAIN_EUR_IND$QprevYear)),"",scales::percent(((MAIN_EUR_IND$QActYear-MAIN_EUR_IND$QprevYear)/MAIN_EUR_IND$QprevYear),accuracy = 0.1))
MAIN_EUR_IND$QuantityDiffYYPER      <- ifelse(is.na(MAIN_EUR_IND$QuantityDiffYYPER),"0",MAIN_EUR_IND$QuantityDiffYYPER)

MAIN_EUR_IND[is.na(MAIN_EUR_IND)]   <- NA
MAIN_EUR_IND$GPDiffYY               <- MAIN_EUR_IND$GPACT-MAIN_EUR_IND$GPPREV
MAIN_EUR_IND[is.na(MAIN_EUR_IND)]   <- NA
MAIN_EUR_IND$GPDiffYYPER            <- ifelse(is.infinite(((MAIN_EUR_IND$GPACT-MAIN_EUR_IND$GPPREV)/MAIN_EUR_IND$GPPREV)),"",scales::percent(((MAIN_EUR_IND$GPACT-MAIN_EUR_IND$GPPREV)/MAIN_EUR_IND$GPPREV),accuracy = 0.1))
#MAIN_EUR_IND$GPDiffYYPER       <- round(((MAIN_EUR_IND$GPACT-MAIN_EUR_IND$GPPREV)/MAIN_EUR_IND$GPPREV)*100,2)
MAIN_EUR_IND[is.na(MAIN_EUR_IND)]   <- NA
MAIN_EUR_IND$TurnDiffYY             <- MAIN_EUR_IND$TURNACT-MAIN_EUR_IND$TURNPREV
MAIN_EUR_IND[is.na(MAIN_EUR_IND)]   <- NA
MAIN_EUR_IND$TurnDiffYYPER          <- ifelse(is.infinite(((MAIN_EUR_IND$TURNACT-MAIN_EUR_IND$TURNPREV)/MAIN_EUR_IND$TURNPREV)),"",scales::percent(((MAIN_EUR_IND$TURNACT-MAIN_EUR_IND$TURNPREV)/MAIN_EUR_IND$TURNPREV),accuracy = 0.1))
MAIN_EUR_IND$TurnDiffYYPER          <- ifelse(is.na(MAIN_EUR_IND$TurnDiffYYPER),"0",MAIN_EUR_IND$TurnDiffYYPER)

MAIN_EUR_IND[is.na(MAIN_EUR_IND)]   <- NA
MAIN_EUR_IND$GPMTACT                <- ifelse(is.infinite(MAIN_EUR_IND$GPACT*1000/MAIN_EUR_IND$QActYear),0,MAIN_EUR_IND$GPACT*1000/MAIN_EUR_IND$QActYear)   # x 1000 ?
MAIN_EUR_IND[is.na(MAIN_EUR_IND)]   <- 0
MAIN_EUR_IND$GPMTPREV               <- ifelse(is.infinite(MAIN_EUR_IND$GPPREV*1000/MAIN_EUR_IND$QprevYear),0,MAIN_EUR_IND$GPPREV*1000/MAIN_EUR_IND$QprevYear)
MAIN_EUR_IND[is.na(MAIN_EUR_IND)]   <- 0
MAIN_EUR_IND$GPMTDIFF               <- MAIN_EUR_IND$GPMTACT-MAIN_EUR_IND$GPMTPREV
MAIN_EUR_IND[is.na(MAIN_EUR_IND)]   <- NA
MAIN_EUR_IND$GPMTDIFFPER            <- ifelse(is.infinite((MAIN_EUR_IND$GPMTACT-MAIN_EUR_IND$GPMTPREV)/MAIN_EUR_IND$GPMTPREV),"",scales::percent(((MAIN_EUR_IND$GPMTACT-MAIN_EUR_IND$GPMTPREV)/MAIN_EUR_IND$GPMTPREV),accuracy = 0.1))
MAIN_EUR_IND$GPMTDIFFPER            <- ifelse(is.na(MAIN_EUR_IND$GPMTDIFFPER),"0",MAIN_EUR_IND$GPMTDIFFPER)
MAIN_EUR_IND[is.na(MAIN_EUR_IND)] <- NA

DT                                    <- data.table(MAIN_EUR_IND)
MAIN_EUR_IND                      <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
MAIN_EUR_IND[is.na(MAIN_EUR_IND)]      <- NA
MAIN_EUR_IND$MapowanieBranzaInd             <- stri_replace_all_fixed(MAIN_EUR_IND$MapowanieBranzaInd, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
MAIN_EUR_IND                      <- setorder(MAIN_EUR_IND,MapowanieBranza,MapowanieBranzaInd)
MAIN_EUR_IND$QuantityDiffYYPER_P    <- NULL



RegionyCEE <- unique(MAIN_EUR_IND$MapowanieBranza)


Total <- data.frame(MapowanieBranza="MapowanieBranza",MapowanieBranzaInd="Total CEE:",
                    QActYear          = sum(MAIN_EUR_IND$QActYear),
                    QprevYear         = sum(MAIN_EUR_IND$QprevYear),
                    GPACT             = sum(MAIN_EUR_IND$GPACT),
                    GPPREV            = sum(MAIN_EUR_IND$GPPREV),
                    TURNACT           = sum(MAIN_EUR_IND$TURNACT),
                    TURNPREV          = sum(MAIN_EUR_IND$TURNPREV),
                    QuantityDiffYY    = 0,
                    QuantityDiffYYPER = 0,
                    GPDiffYYPER       = 0,
                    GPDiffYY          = 0,
                    TurnDiffYY        = sum(MAIN_EUR_IND$TurnDiffYY),
                    TurnDiffYYPER     = 0,
                    GPMTACT           = sum(MAIN_EUR_IND$GPMTACT),
                    GPMTPREV          = sum(MAIN_EUR_IND$GPMTPREV),
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

Regiony <- unique(MAIN_EUR_IND$MapowanieBranzaInd)

for(region in unique(MAIN_EUR_IND$MapowanieBranza)){
  steal <- subset(MAIN_EUR_IND,MapowanieBranza==region)
  frejm <- data.frame(MapowanieBranza=region,MapowanieBranzaInd=region,
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
  MAIN_EUR_IND             <-  rbind(MAIN_EUR_IND, frejm)
  
}

#GRUPOWANIE NORTH SOUTH CEE
# 
# 
# 
# for(region in RegionyCEE){
#   
#   steal <- subset(MAIN_EUR_IND,Region==region)
#   frejm <- data.frame(Region=region,SubRegion=paste("TOTAL", region), WhsDirComm=paste("TOTAL",region),
#                       QActYear          = sum(steal$QActYear),
#                       QprevYear         = sum(steal$QprevYear),
#                       GPACT             = sum(steal$GPACT),
#                       GPPREV            = sum(steal$GPPREV),
#                       TURNACT           = sum(steal$TURNACT),
#                       TURNPREV          = sum(steal$TURNPREV),
#                       QuantityDiffYY    = sum(steal$QuantityDiffYY),
#                       QuantityDiffYYPER = 0,
#                       GPDiffYYPER       = 0,
#                       GPDiffYY          = sum(steal$GPDiffYY),
#                       TurnDiffYY        = sum(steal$TurnDiffYY),
#                       TurnDiffYYPER     = 0,
#                       GPMTACT           = sum(steal$GPMTACT),
#                       GPMTPREV          = sum(steal$GPMTPREV),
#                       GPMTDIFF          = sum(steal$GPMTDIFF),
#                       GPMTDIFFPER       = 0)
#   
#   frejm$TurnDiffYY         <-  frejm$TURNACT-frejm$TURNPREV
#   frejm$QuantityDiffYY     <-  frejm$QActYear-frejm$QprevYear
#   frejm$GPDiffYY           <-  frejm$GPACT-frejm$GPPREV
#   frejm$GPMTDIFF           <-  frejm$GPMTACT-frejm$GPMTPREV
#   frejm$QuantityDiffYYPER  <-  scales::percent(frejm$QuantityDiffYY/frejm$QprevYear,accuracy = 0.1)
#   frejm$GPDiffYYPER        <-  scales::percent(frejm$GPDiffYY/frejm$GPPREV,accuracy = 0.1)
#   frejm$TurnDiffYYPER      <-  scales::percent(frejm$TurnDiffYY/frejm$TURNPREV,accuracy = 0.1)
#   frejm$GPMTDIFFPER        <-  scales::percent(frejm$GPMTDIFF/frejm$GPMTPREV,accuracy = 0.1)
#   MAIN_EUR_IND<- rbind(MAIN_EUR_IND, frejm)
#   
# }
# 
# 
# 
# MapowaniePomocnicze            <- MapowanieBaltics
# MapowaniePomocnicze$SubRegion  <- stri_replace_all_fixed(MapowaniePomocnicze$SubRegion , pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
# 
# MapowaniePomocnicze            <- MapowaniePomocnicze %>% select(2,3) %>% distinct
# colnames(MapowaniePomocnicze)[1] <- "RegionU"
# 
# MAIN_EUR_IND                    <- left_join(MAIN_EUR_IND,MapowaniePomocnicze,by="SubRegion")
# MAIN_EUR_IND$Region               <- ifelse(is.na(MAIN_EUR_IND$Region),MAIN_EUR_IND$RegionU,MAIN_EUR_IND$Region)
# MAIN_EUR_IND$RegionU              <- NULL


MAIN_EUR_IND                    <- setorder(MAIN_EUR_IND,MapowanieBranza,QActYear)
MAIN_EUR_IND                    <- rbind(MAIN_EUR_IND,Total)

WyboldowaneWierszeMAIN_EUR_IND  <- c(which(MAIN_EUR_IND$MapowanieBranzaInd %in% RegionyCEE),length(MAIN_EUR_IND$MapowanieBranzaInd))
WyboldowaneWierszeMAIN_EUR_IND  <- c(which(str_sub(MAIN_EUR_IND$MapowanieBranzaInd,1,4) == "Tota"),WyboldowaneWierszeMAIN_EUR_IND)


MAIN_EUR_IND$MapowanieBranza               <- NULL


MAIN_EUR_IND                    <- MAIN_EUR_IND %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
MAIN_EUR_IND[is.na(MAIN_EUR_IND)]    <- ""


# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

MAIN_EUR_IND        <- MAIN_EUR_IND%>% mutate_if(is.numeric, ~round(., 0)) 
MAIN_EUR_IND$MapowanieBranzaInd             <- stri_replace_all_fixed(MAIN_EUR_IND$MapowanieBranzaInd, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
MAIN_EUR_IND


#MONTH

MAIN_EUR_IND_Y <- left_join(MainSource,MapowanieBranzaIndu,by="European Industry")





colnames(MAIN_EUR_IND_Y)[12]<-Naglowek1
colnames(MAIN_EUR_IND_Y)[13]<-Naglowek2
colnames(MAIN_EUR_IND_Y)[14]<-Naglowek3
colnames(MAIN_EUR_IND_Y)[15]<-Naglowek4
colnames(MAIN_EUR_IND_Y)[16]<-Naglowek5
colnames(MAIN_EUR_IND_Y)[17]<-Naglowek6

MAIN_EUR_IND_Y                   <- MAIN_EUR_IND_Y %>% filter(`Calendar month`==MiesiacAnalizy)
MAIN_EUR_IND_Y                   <- MAIN_EUR_IND_Y %>% select(20,22,12,13,14,15,16,17)




# MAIN_EUR_IND_Y<- rbind(MAIN_EUR_IND_Y,MAIN_EUR_IND_Y2)
MAIN_EUR_IND_Y                      <- as.data.frame(MAIN_EUR_IND_Y)
MAIN_EUR_IND_Y                      <- MAIN_EUR_IND_Y%>% group_by(MapowanieBranza,MapowanieBranzaInd) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
MAIN_EUR_IND_Y[is.na(MAIN_EUR_IND_Y)]      <- NA
MAIN_EUR_IND_Y$QuantityDiffYY         <- MAIN_EUR_IND_Y$QActYear-MAIN_EUR_IND_Y$QprevYear
MAIN_EUR_IND_Y[is.na(MAIN_EUR_IND_Y)]      <- NA
MAIN_EUR_IND_Y$QuantityDiffYYPER_P    <- as.numeric(ifelse(is.infinite(((MAIN_EUR_IND_Y$QActYear-MAIN_EUR_IND_Y$QprevYear)/MAIN_EUR_IND_Y$QprevYear)),"",(MAIN_EUR_IND_Y$QActYear-MAIN_EUR_IND_Y$QprevYear)/MAIN_EUR_IND_Y$QprevYear))
MAIN_EUR_IND_Y$QuantityDiffYYPER_P    <- ifelse(is.na(MAIN_EUR_IND_Y$QuantityDiffYYPER_P),"0",MAIN_EUR_IND_Y$QuantityDiffYYPER_P)
MAIN_EUR_IND_Y$QuantityDiffYYPER      <- ifelse(is.infinite(((MAIN_EUR_IND_Y$QActYear-MAIN_EUR_IND_Y$QprevYear)/MAIN_EUR_IND_Y$QprevYear)),"",scales::percent(((MAIN_EUR_IND_Y$QActYear-MAIN_EUR_IND_Y$QprevYear)/MAIN_EUR_IND_Y$QprevYear),accuracy = 0.1))
MAIN_EUR_IND_Y$QuantityDiffYYPER      <- ifelse(is.na(MAIN_EUR_IND_Y$QuantityDiffYYPER),"0",MAIN_EUR_IND_Y$QuantityDiffYYPER)


MAIN_EUR_IND_Y[is.na(MAIN_EUR_IND_Y)]      <- NA
MAIN_EUR_IND_Y$GPDiffYY               <- MAIN_EUR_IND_Y$GPACT-MAIN_EUR_IND_Y$GPPREV
MAIN_EUR_IND_Y[is.na(MAIN_EUR_IND_Y)]      <- NA
MAIN_EUR_IND_Y$GPDiffYYPER            <- ifelse(is.infinite(((MAIN_EUR_IND_Y$GPACT-MAIN_EUR_IND_Y$GPPREV)/MAIN_EUR_IND_Y$GPPREV)),"",scales::percent(((MAIN_EUR_IND_Y$GPACT-MAIN_EUR_IND_Y$GPPREV)/MAIN_EUR_IND_Y$GPPREV),accuracy = 0.1))
#MAIN_EUR_IND_Y$GPDiffYYPER       <- round(((MAIN_EUR_IND_Y$GPACT-MAIN_EUR_IND_Y$GPPREV)/MAIN_EUR_IND_Y$GPPREV)*100,2)
MAIN_EUR_IND_Y[is.na(MAIN_EUR_IND_Y)]      <- NA
MAIN_EUR_IND_Y$TurnDiffYY             <- MAIN_EUR_IND_Y$TURNACT-MAIN_EUR_IND_Y$TURNPREV
MAIN_EUR_IND_Y[is.na(MAIN_EUR_IND_Y)]      <- NA
MAIN_EUR_IND_Y$TurnDiffYYPER          <- ifelse(is.infinite(((MAIN_EUR_IND_Y$TURNACT-MAIN_EUR_IND_Y$TURNPREV)/MAIN_EUR_IND_Y$TURNPREV)),"",scales::percent(((MAIN_EUR_IND_Y$TURNACT-MAIN_EUR_IND_Y$TURNPREV)/MAIN_EUR_IND_Y$TURNPREV),accuracy = 0.1))
MAIN_EUR_IND_Y$TurnDiffYYPER          <- ifelse(is.na(MAIN_EUR_IND_Y$TurnDiffYYPER),"0",MAIN_EUR_IND_Y$TurnDiffYYPER)

MAIN_EUR_IND_Y[is.na(MAIN_EUR_IND_Y)]      <- NA
MAIN_EUR_IND_Y$GPMTACT                <- ifelse(is.infinite(MAIN_EUR_IND_Y$GPACT*1000/MAIN_EUR_IND_Y$QActYear),0,MAIN_EUR_IND_Y$GPACT*1000/MAIN_EUR_IND_Y$QActYear)   # x 1000 ?
MAIN_EUR_IND_Y[is.na(MAIN_EUR_IND_Y)]      <- 0
MAIN_EUR_IND_Y$GPMTPREV               <- ifelse(is.infinite(MAIN_EUR_IND_Y$GPPREV*1000/MAIN_EUR_IND_Y$QprevYear),0,MAIN_EUR_IND_Y$GPPREV*1000/MAIN_EUR_IND_Y$QprevYear)
MAIN_EUR_IND_Y[is.na(MAIN_EUR_IND_Y)]      <- 0
MAIN_EUR_IND_Y$GPMTDIFF               <- MAIN_EUR_IND_Y$GPMTACT-MAIN_EUR_IND_Y$GPMTPREV
MAIN_EUR_IND_Y[is.na(MAIN_EUR_IND_Y)]      <- NA
MAIN_EUR_IND_Y$GPMTDIFFPER            <- ifelse(is.infinite((MAIN_EUR_IND_Y$GPMTACT-MAIN_EUR_IND_Y$GPMTPREV)/MAIN_EUR_IND_Y$GPMTPREV),"",scales::percent(((MAIN_EUR_IND_Y$GPMTACT-MAIN_EUR_IND_Y$GPMTPREV)/MAIN_EUR_IND_Y$GPMTPREV),accuracy = 0.1))
MAIN_EUR_IND_Y$GPMTDIFFPER            <- ifelse(is.na(MAIN_EUR_IND_Y$GPMTDIFFPER),"0",MAIN_EUR_IND_Y$GPMTDIFFPER)
MAIN_EUR_IND_Y[is.na(MAIN_EUR_IND_Y)] <- NA

DT                                    <- data.table(MAIN_EUR_IND_Y)
MAIN_EUR_IND_Y                      <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
MAIN_EUR_IND_Y[is.na(MAIN_EUR_IND_Y)]      <- NA
MAIN_EUR_IND_Y$MapowanieBranzaInd             <- stri_replace_all_fixed(MAIN_EUR_IND_Y$MapowanieBranzaInd, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
MAIN_EUR_IND_Y                      <- setorder(MAIN_EUR_IND_Y,MapowanieBranza,MapowanieBranzaInd)
MAIN_EUR_IND_Y$QuantityDiffYYPER_P    <- NULL



RegionyCEE <- unique(MAIN_EUR_IND_Y$MapowanieBranza)


Total <- data.frame(MapowanieBranza="MapowanieBranza",MapowanieBranzaInd="Total CEE:",
                    QActYear          = sum(MAIN_EUR_IND_Y$QActYear),
                    QprevYear         = sum(MAIN_EUR_IND_Y$QprevYear),
                    GPACT             = sum(MAIN_EUR_IND_Y$GPACT),
                    GPPREV            = sum(MAIN_EUR_IND_Y$GPPREV),
                    TURNACT           = sum(MAIN_EUR_IND_Y$TURNACT),
                    TURNPREV          = sum(MAIN_EUR_IND_Y$TURNPREV),
                    QuantityDiffYY    = 0,
                    QuantityDiffYYPER = 0,
                    GPDiffYYPER       = 0,
                    GPDiffYY          = 0,
                    TurnDiffYY        = sum(MAIN_EUR_IND_Y$TurnDiffYY),
                    TurnDiffYYPER     = 0,
                    GPMTACT           = sum(MAIN_EUR_IND_Y$GPMTACT),
                    GPMTPREV          = sum(MAIN_EUR_IND_Y$GPMTPREV),
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

Regiony <- unique(MAIN_EUR_IND_Y$MapowanieBranzaInd)

for(region in unique(MAIN_EUR_IND_Y$MapowanieBranza)){
  steal <- subset(MAIN_EUR_IND_Y,MapowanieBranza==region)
  frejm <- data.frame(MapowanieBranza=region,MapowanieBranzaInd=region,
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
  MAIN_EUR_IND_Y             <-  rbind(MAIN_EUR_IND_Y, frejm)
  
}

#GRUPOWANIE NORTH SOUTH CEE
# 
# 
# 
# for(region in RegionyCEE){
#   
#   steal <- subset(MAIN_EUR_IND_Y,Region==region)
#   frejm <- data.frame(Region=region,SubRegion=paste("TOTAL", region), WhsDirComm=paste("TOTAL",region),
#                       QActYear          = sum(steal$QActYear),
#                       QprevYear         = sum(steal$QprevYear),
#                       GPACT             = sum(steal$GPACT),
#                       GPPREV            = sum(steal$GPPREV),
#                       TURNACT           = sum(steal$TURNACT),
#                       TURNPREV          = sum(steal$TURNPREV),
#                       QuantityDiffYY    = sum(steal$QuantityDiffYY),
#                       QuantityDiffYYPER = 0,
#                       GPDiffYYPER       = 0,
#                       GPDiffYY          = sum(steal$GPDiffYY),
#                       TurnDiffYY        = sum(steal$TurnDiffYY),
#                       TurnDiffYYPER     = 0,
#                       GPMTACT           = sum(steal$GPMTACT),
#                       GPMTPREV          = sum(steal$GPMTPREV),
#                       GPMTDIFF          = sum(steal$GPMTDIFF),
#                       GPMTDIFFPER       = 0)
#   
#   frejm$TurnDiffYY         <-  frejm$TURNACT-frejm$TURNPREV
#   frejm$QuantityDiffYY     <-  frejm$QActYear-frejm$QprevYear
#   frejm$GPDiffYY           <-  frejm$GPACT-frejm$GPPREV
#   frejm$GPMTDIFF           <-  frejm$GPMTACT-frejm$GPMTPREV
#   frejm$QuantityDiffYYPER  <-  scales::percent(frejm$QuantityDiffYY/frejm$QprevYear,accuracy = 0.1)
#   frejm$GPDiffYYPER        <-  scales::percent(frejm$GPDiffYY/frejm$GPPREV,accuracy = 0.1)
#   frejm$TurnDiffYYPER      <-  scales::percent(frejm$TurnDiffYY/frejm$TURNPREV,accuracy = 0.1)
#   frejm$GPMTDIFFPER        <-  scales::percent(frejm$GPMTDIFF/frejm$GPMTPREV,accuracy = 0.1)
#   MAIN_EUR_IND_Y<- rbind(MAIN_EUR_IND_Y, frejm)
#   
# }
# 
# 
# 
# MapowaniePomocnicze            <- MapowanieBaltics
# MapowaniePomocnicze$SubRegion  <- stri_replace_all_fixed(MapowaniePomocnicze$SubRegion , pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
# 
# MapowaniePomocnicze            <- MapowaniePomocnicze %>% select(2,3) %>% distinct
# colnames(MapowaniePomocnicze)[1] <- "RegionU"
# 
# MAIN_EUR_IND_Y                    <- left_join(MAIN_EUR_IND_Y,MapowaniePomocnicze,by="SubRegion")
# MAIN_EUR_IND_Y$Region               <- ifelse(is.na(MAIN_EUR_IND_Y$Region),MAIN_EUR_IND_Y$RegionU,MAIN_EUR_IND_Y$Region)
# MAIN_EUR_IND_Y$RegionU              <- NULL


MAIN_EUR_IND_Y                    <- setorder(MAIN_EUR_IND_Y,MapowanieBranza,QActYear)
MAIN_EUR_IND_Y                    <- rbind(MAIN_EUR_IND_Y,Total)

WyboldowaneWierszeMAIN_EUR_IND_Y  <- c(which(MAIN_EUR_IND_Y$MapowanieBranzaInd %in% RegionyCEE),length(MAIN_EUR_IND_Y$MapowanieBranzaInd))
WyboldowaneWierszeMAIN_EUR_IND_Y  <- c(which(str_sub(MAIN_EUR_IND_Y$MapowanieBranzaInd,1,4) == "Tota"),WyboldowaneWierszeMAIN_EUR_IND_Y)


MAIN_EUR_IND_Y$MapowanieBranza               <- NULL


MAIN_EUR_IND_Y                    <- MAIN_EUR_IND_Y %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
MAIN_EUR_IND_Y[is.na(MAIN_EUR_IND_Y)]    <- ""


# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

MAIN_EUR_IND_Y        <- MAIN_EUR_IND_Y%>% mutate_if(is.numeric, ~round(., 0)) 
MAIN_EUR_IND_Y$MapowanieBranzaInd             <- stri_replace_all_fixed(MAIN_EUR_IND_Y$MapowanieBranzaInd, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
MAIN_EUR_IND_Y
