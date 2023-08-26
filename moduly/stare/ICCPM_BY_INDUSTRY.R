

ICCPM_DATA<-MainSource
colnames(ICCPM_DATA)[5]<-"Division"
ICCPM_DATA <-left_join(ICCPM_DATA,MapowanieICCPMOthers,by="Division")


colnames(ICCPM_DATA)[12]<-Naglowek1
colnames(ICCPM_DATA)[13]<-Naglowek2
colnames(ICCPM_DATA)[14]<-Naglowek3
colnames(ICCPM_DATA)[15]<-Naglowek4
colnames(ICCPM_DATA)[16]<-Naglowek5
colnames(ICCPM_DATA)[17]<-Naglowek6

#ICCPM_DATA                    <- left_join(ICCPM_DATA,MapowanieBaltics,by="Subsidiary Country")


ICCPM_DATA                   <- ICCPM_DATA %>% filter(`Calendar month`==MiesiacAnalizy)
ICCPM_DATA                   <- ICCPM_DATA %>% select(22,5,12,13,14,15,16,17)

# ICCPM_DATA<-rbind(ICCPM_DATA,ICCPM_DATA2)
ICCPM_DATA                   <- as.data.frame(ICCPM_DATA)
ICCPM_DATA                   <- ICCPM_DATA %>% group_by(Mapowanie,Division) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
ICCPM_DATA[is.na(ICCPM_DATA)]      <- NA
ICCPM_DATA$QuantityDiffYY    <- ICCPM_DATA$QActYear-ICCPM_DATA$QprevYear
ICCPM_DATA[is.na(ICCPM_DATA)]      <- NA
ICCPM_DATA$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((ICCPM_DATA$QActYear-ICCPM_DATA$QprevYear)/ICCPM_DATA$QprevYear)),"",(ICCPM_DATA$QActYear-ICCPM_DATA$QprevYear)/ICCPM_DATA$QprevYear))
ICCPM_DATA$QuantityDiffYYPER <- ifelse(is.infinite(((ICCPM_DATA$QActYear-ICCPM_DATA$QprevYear)/ICCPM_DATA$QprevYear)),"",scales::percent(((ICCPM_DATA$QActYear-ICCPM_DATA$QprevYear)/ICCPM_DATA$QprevYear),accuracy = 0.1))
ICCPM_DATA[is.na(ICCPM_DATA)]      <- NA
ICCPM_DATA$GPDiffYY          <- ICCPM_DATA$GPACT-ICCPM_DATA$GPPREV
ICCPM_DATA[is.na(ICCPM_DATA)]      <- NA
ICCPM_DATA$GPDiffYYPER       <- ifelse(is.infinite(((ICCPM_DATA$GPACT-ICCPM_DATA$GPPREV)/ICCPM_DATA$GPPREV)),"",scales::percent(((ICCPM_DATA$GPACT-ICCPM_DATA$GPPREV)/ICCPM_DATA$GPPREV),accuracy = 0.1))
#ICCPM_DATA$GPDiffYYPER       <- round(((ICCPM_DATA$GPACT-ICCPM_DATA$GPPREV)/ICCPM_DATA$GPPREV)*100,2)
ICCPM_DATA[is.na(ICCPM_DATA)]      <- NA
ICCPM_DATA$TurnDiffYY        <- ICCPM_DATA$TURNACT-ICCPM_DATA$TURNPREV
ICCPM_DATA[is.na(ICCPM_DATA)]      <- NA
ICCPM_DATA$TurnDiffYYPER     <- ifelse(is.infinite(((ICCPM_DATA$TURNACT-ICCPM_DATA$TURNPREV)/ICCPM_DATA$TURNPREV)),"",scales::percent(((ICCPM_DATA$TURNACT-ICCPM_DATA$TURNPREV)/ICCPM_DATA$TURNPREV),accuracy = 0.1))
ICCPM_DATA[is.na(ICCPM_DATA)]      <- NA
ICCPM_DATA$GPMTACT           <- ifelse(is.infinite(ICCPM_DATA$GPACT*1000/ICCPM_DATA$QActYear),0,ICCPM_DATA$GPACT*1000/ICCPM_DATA$QActYear)   # x 1000 ?
ICCPM_DATA[is.na(ICCPM_DATA)]      <- 0
ICCPM_DATA$GPMTPREV          <- ifelse(is.infinite(ICCPM_DATA$GPPREV*1000/ICCPM_DATA$QprevYear),0,ICCPM_DATA$GPPREV*1000/ICCPM_DATA$QprevYear)
ICCPM_DATA[is.na(ICCPM_DATA)]      <- 0
ICCPM_DATA$GPMTDIFF          <- ICCPM_DATA$GPMTACT-ICCPM_DATA$GPMTPREV
ICCPM_DATA[is.na(ICCPM_DATA)]      <- NA
ICCPM_DATA$GPMTDIFFPER       <- ifelse(is.infinite((ICCPM_DATA$GPMTACT-ICCPM_DATA$GPMTPREV)/ICCPM_DATA$GPMTPREV),"",scales::percent(((ICCPM_DATA$GPMTACT-ICCPM_DATA$GPMTPREV)/ICCPM_DATA$GPMTPREV),accuracy = 0.1))
ICCPM_DATA[is.na(ICCPM_DATA)]      <- NA

DT                     <- data.table(ICCPM_DATA)
ICCPM_DATA                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
ICCPM_DATA[is.na(ICCPM_DATA)]      <- NA
ICCPM_DATA$Division      <- stri_replace_all_fixed(ICCPM_DATA$Division, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
ICCPM_DATA                      <- setorder(ICCPM_DATA,Mapowanie,-QuantityDiffYYPER_P)
ICCPM_DATA$QuantityDiffYYPER_P <-NULL



Total<-data.frame(Mapowanie="Total CEE", Division="Total CEE",
                  QActYear          = sum(ICCPM_DATA$QActYear),
                  QprevYear         = sum(ICCPM_DATA$QprevYear),
                  GPACT             = sum(ICCPM_DATA$GPACT),
                  GPPREV            = sum(ICCPM_DATA$GPPREV),
                  TURNACT           = sum(ICCPM_DATA$TURNACT),
                  TURNPREV          = sum(ICCPM_DATA$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(ICCPM_DATA$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(ICCPM_DATA$GPMTACT),
                  GPMTPREV          = sum(ICCPM_DATA$GPMTPREV),
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

Regiony<-unique(ICCPM_DATA$Mapowanie)

for(region in unique(ICCPM_DATA$Mapowanie)){
  
  steal <- subset(ICCPM_DATA,Mapowanie==region)
  frejm <- data.frame(Mapowanie=region, Division=region, 
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
  
  ICCPM_DATA <- rbind(ICCPM_DATA, frejm)

}

ICCPM_DATA                      <- rbind(ICCPM_DATA,Total)
ICCPM_DATA                      <- setorder(ICCPM_DATA,Mapowanie)
ICCPM_DATA$Mapowanie               <- NULL
ICCPM_DATA                      <- ICCPM_DATA %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
ICCPM_DATA[is.na(ICCPM_DATA)]    <- ""

WyboldowaneWierszeICCPM_DATA   <- c(which(ICCPM_DATA$Division %in% Regiony),length(ICCPM_DATA$Division))
WyboldowaneWierszeICCPM_DATA   <- c(which(str_sub(ICCPM_DATA$Division,7,9) == "CEE"),WyboldowaneWierszeICCPM_DATA)
WyboldowaneWierszeICCPM_DATA <- unique(WyboldowaneWierszeICCPM_DATA)
# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

ICCPM_DATA <- ICCPM_DATA %>% mutate_if(is.numeric, ~round(., 0))

#YTD

ICCPM_DATAY <- MainSource
colnames(ICCPM_DATAY)[5]<-"Division"
ICCPM_DATAY <-left_join(ICCPM_DATAY,MapowanieICCPMOthers,by="Division")


colnames(ICCPM_DATAY)[12]<-Naglowek1
colnames(ICCPM_DATAY)[13]<-Naglowek2
colnames(ICCPM_DATAY)[14]<-Naglowek3
colnames(ICCPM_DATAY)[15]<-Naglowek4
colnames(ICCPM_DATAY)[16]<-Naglowek5
colnames(ICCPM_DATAY)[17]<-Naglowek6

#ICCPM_DATAY                    <- left_join(ICCPM_DATAY,MapowanieBaltics,by="Subsidiary Country")


ICCPM_DATAY                   <- ICCPM_DATAY %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
ICCPM_DATAY                   <- ICCPM_DATAY %>% select(22,5,12,13,14,15,16,17)

# ICCPM_DATAY<-rbind(ICCPM_DATAY,ICCPM_DATAY2)
ICCPM_DATAY                   <- as.data.frame(ICCPM_DATAY)
ICCPM_DATAY                   <- ICCPM_DATAY %>% group_by(Mapowanie,Division) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
ICCPM_DATAY[is.na(ICCPM_DATAY)]      <- NA
ICCPM_DATAY$QuantityDiffYY    <- ICCPM_DATAY$QActYear-ICCPM_DATAY$QprevYear
ICCPM_DATAY[is.na(ICCPM_DATAY)]      <- NA
ICCPM_DATAY$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((ICCPM_DATAY$QActYear-ICCPM_DATAY$QprevYear)/ICCPM_DATAY$QprevYear)),"",(ICCPM_DATAY$QActYear-ICCPM_DATAY$QprevYear)/ICCPM_DATAY$QprevYear))
ICCPM_DATAY$QuantityDiffYYPER <- ifelse(is.infinite(((ICCPM_DATAY$QActYear-ICCPM_DATAY$QprevYear)/ICCPM_DATAY$QprevYear)),"",scales::percent(((ICCPM_DATAY$QActYear-ICCPM_DATAY$QprevYear)/ICCPM_DATAY$QprevYear),accuracy = 0.1))
ICCPM_DATAY[is.na(ICCPM_DATAY)]      <- NA
ICCPM_DATAY$GPDiffYY          <- ICCPM_DATAY$GPACT-ICCPM_DATAY$GPPREV
ICCPM_DATAY[is.na(ICCPM_DATAY)]      <- NA
ICCPM_DATAY$GPDiffYYPER       <- ifelse(is.infinite(((ICCPM_DATAY$GPACT-ICCPM_DATAY$GPPREV)/ICCPM_DATAY$GPPREV)),"",scales::percent(((ICCPM_DATAY$GPACT-ICCPM_DATAY$GPPREV)/ICCPM_DATAY$GPPREV),accuracy = 0.1))
#ICCPM_DATAY$GPDiffYYPER       <- round(((ICCPM_DATAY$GPACT-ICCPM_DATAY$GPPREV)/ICCPM_DATAY$GPPREV)*100,2)
ICCPM_DATAY[is.na(ICCPM_DATAY)]      <- NA
ICCPM_DATAY$TurnDiffYY        <- ICCPM_DATAY$TURNACT-ICCPM_DATAY$TURNPREV
ICCPM_DATAY[is.na(ICCPM_DATAY)]      <- NA
ICCPM_DATAY$TurnDiffYYPER     <- ifelse(is.infinite(((ICCPM_DATAY$TURNACT-ICCPM_DATAY$TURNPREV)/ICCPM_DATAY$TURNPREV)),"",scales::percent(((ICCPM_DATAY$TURNACT-ICCPM_DATAY$TURNPREV)/ICCPM_DATAY$TURNPREV),accuracy = 0.1))
ICCPM_DATAY[is.na(ICCPM_DATAY)]      <- NA
ICCPM_DATAY$GPMTACT           <- ifelse(is.infinite(ICCPM_DATAY$GPACT*1000/ICCPM_DATAY$QActYear),0,ICCPM_DATAY$GPACT*1000/ICCPM_DATAY$QActYear)   # x 1000 ?
ICCPM_DATAY[is.na(ICCPM_DATAY)]      <- 0
ICCPM_DATAY$GPMTPREV          <- ifelse(is.infinite(ICCPM_DATAY$GPPREV*1000/ICCPM_DATAY$QprevYear),0,ICCPM_DATAY$GPPREV*1000/ICCPM_DATAY$QprevYear)
ICCPM_DATAY[is.na(ICCPM_DATAY)]      <- 0
ICCPM_DATAY$GPMTDIFF          <- ICCPM_DATAY$GPMTACT-ICCPM_DATAY$GPMTPREV
ICCPM_DATAY[is.na(ICCPM_DATAY)]      <- NA
ICCPM_DATAY$GPMTDIFFPER       <- ifelse(is.infinite((ICCPM_DATAY$GPMTACT-ICCPM_DATAY$GPMTPREV)/ICCPM_DATAY$GPMTPREV),"",scales::percent(((ICCPM_DATAY$GPMTACT-ICCPM_DATAY$GPMTPREV)/ICCPM_DATAY$GPMTPREV),accuracy = 0.1))
ICCPM_DATAY[is.na(ICCPM_DATAY)]      <- NA

DT                     <- data.table(ICCPM_DATAY)
ICCPM_DATAY                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
ICCPM_DATAY[is.na(ICCPM_DATAY)]      <- NA
ICCPM_DATAY$Division      <- stri_replace_all_fixed(ICCPM_DATAY$Division, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
ICCPM_DATAY                      <- setorder(ICCPM_DATAY,Mapowanie,-QuantityDiffYYPER_P)
ICCPM_DATAY$QuantityDiffYYPER_P <-NULL



Total<-data.frame(Mapowanie="Total CEE", Division="Total CEE",
                  QActYear          = sum(ICCPM_DATAY$QActYear),
                  QprevYear         = sum(ICCPM_DATAY$QprevYear),
                  GPACT             = sum(ICCPM_DATAY$GPACT),
                  GPPREV            = sum(ICCPM_DATAY$GPPREV),
                  TURNACT           = sum(ICCPM_DATAY$TURNACT),
                  TURNPREV          = sum(ICCPM_DATAY$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(ICCPM_DATAY$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(ICCPM_DATAY$GPMTACT),
                  GPMTPREV          = sum(ICCPM_DATAY$GPMTPREV),
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

Regiony<-unique(ICCPM_DATAY$Mapowanie)

for(region in unique(ICCPM_DATAY$Mapowanie)){
  
  steal <- subset(ICCPM_DATAY,Mapowanie==region)
  frejm <- data.frame(Mapowanie=region, Division=region, 
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
  
  ICCPM_DATAY <- rbind(ICCPM_DATAY, frejm)
  
}

ICCPM_DATAY                      <- rbind(ICCPM_DATAY,Total)
ICCPM_DATAY                      <- setorder(ICCPM_DATAY,Mapowanie)
ICCPM_DATAY$Mapowanie               <- NULL
ICCPM_DATAY                      <- ICCPM_DATAY %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
ICCPM_DATAY[is.na(ICCPM_DATAY)]    <- ""

WyboldowaneWierszeICCPM_DATAY   <- c(which(ICCPM_DATAY$Division %in% Regiony),length(ICCPM_DATAY$Division))
WyboldowaneWierszeICCPM_DATAY   <- c(which(str_sub(ICCPM_DATAY$Division,7,9) == "CEE"),WyboldowaneWierszeICCPM_DATAY)
WyboldowaneWierszeICCPM_DATAY <- unique(WyboldowaneWierszeICCPM_DATAY)
# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

ICCPM_DATAY <- ICCPM_DATAY %>% mutate_if(is.numeric, ~round(., 0))



