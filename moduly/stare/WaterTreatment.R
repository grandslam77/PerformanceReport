
WTMain  <- read_xlsx(here::here("sources","DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "Water Treatment",range="I5:X15000", col_names = TRUE,na = "NA")


WTMain                      <- WTMain  %>% filter_all(any_vars(!is.na(.))) 
WTMain [is.na(WTMain)]      <- 0   # wszystkie pola bez wartości wypełniamy zerami

WTMain                      <- left_join(WTMain,MapowanieBaltics,by="Subsidiary Country")

colnames(MapowanieIntercompany)[1]<-"SoldToCountry"
colnames(WTMain)[9]<-"SoldToCountry"

WTMain$Clasification<-as.character("")



WTMain                <- left_join(WTMain,MapowanieIntercompany,by="SoldToCountry")
WTMain$Clasification  <- ifelse(WTMain$Intern=="External",WTMain$Clasification<-"3rd party",ifelse(WTMain$SoldToCountry %in% MapowanieIntercompany$SoldToCountry,WTMain$Clasification<-"Intercompany (inside CEE)",WTMain$Clasification<-"Intercompany (outside CEE)"))

colnames(WTMain)[2]<-"WaterClass"

WT_TOTAL<-WTMain %>% subset(WaterClass=="WATER TREATMENT BUSINESS Summary")


colnames(WT_TOTAL)[11]<-Naglowek1
colnames(WT_TOTAL)[12]<-Naglowek2
colnames(WT_TOTAL)[13]<-Naglowek3
colnames(WT_TOTAL)[14]<-Naglowek4
colnames(WT_TOTAL)[15]<-Naglowek5
colnames(WT_TOTAL)[16]<-Naglowek6



WT_TOTAL                   <- WT_TOTAL %>% filter(`Calendar month`==MiesiacAnalizy)
WT_TOTAL                   <- WT_TOTAL %>% select(17,18,11,12,13,14,15,16)

# WT_TOTAL<-rbind(WT_TOTAL,WT_TOTAL2)
WT_TOTAL                   <- as.data.frame(WT_TOTAL)
WT_TOTAL                   <- WT_TOTAL %>% group_by(Region,SubRegion) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
WT_TOTAL[is.na(WT_TOTAL)]      <- NA
WT_TOTAL$QuantityDiffYY    <- WT_TOTAL$QActYear-WT_TOTAL$QprevYear
WT_TOTAL[is.na(WT_TOTAL)]      <- NA
WT_TOTAL$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((WT_TOTAL$QActYear-WT_TOTAL$QprevYear)/WT_TOTAL$QprevYear)),"",(WT_TOTAL$QActYear-WT_TOTAL$QprevYear)/WT_TOTAL$QprevYear))
WT_TOTAL$QuantityDiffYYPER <- ifelse(is.infinite(((WT_TOTAL$QActYear-WT_TOTAL$QprevYear)/WT_TOTAL$QprevYear)),"",scales::percent(((WT_TOTAL$QActYear-WT_TOTAL$QprevYear)/WT_TOTAL$QprevYear),accuracy = 0.1))
WT_TOTAL[is.na(WT_TOTAL)]      <- NA
WT_TOTAL$GPDiffYY          <- WT_TOTAL$GPACT-WT_TOTAL$GPPREV
WT_TOTAL[is.na(WT_TOTAL)]      <- NA
WT_TOTAL$GPDiffYYPER       <- ifelse(is.infinite(((WT_TOTAL$GPACT-WT_TOTAL$GPPREV)/WT_TOTAL$GPPREV)),"",scales::percent(((WT_TOTAL$GPACT-WT_TOTAL$GPPREV)/WT_TOTAL$GPPREV),accuracy = 0.1))
#WT_TOTAL$GPDiffYYPER       <- round(((WT_TOTAL$GPACT-WT_TOTAL$GPPREV)/WT_TOTAL$GPPREV)*100,2)
WT_TOTAL[is.na(WT_TOTAL)]      <- NA
WT_TOTAL$TurnDiffYY        <- WT_TOTAL$TURNACT-WT_TOTAL$TURNPREV
WT_TOTAL[is.na(WT_TOTAL)]      <- NA
WT_TOTAL$TurnDiffYYPER     <- ifelse(is.infinite(((WT_TOTAL$TURNACT-WT_TOTAL$TURNPREV)/WT_TOTAL$TURNPREV)),"",scales::percent(((WT_TOTAL$TURNACT-WT_TOTAL$TURNPREV)/WT_TOTAL$TURNPREV),accuracy = 0.1))
WT_TOTAL[is.na(WT_TOTAL)]      <- NA
WT_TOTAL$GPMTACT           <- ifelse(is.infinite(WT_TOTAL$GPACT*1000/WT_TOTAL$QActYear),0,WT_TOTAL$GPACT*1000/WT_TOTAL$QActYear)   # x 1000 ?
WT_TOTAL[is.na(WT_TOTAL)]      <- 0
WT_TOTAL$GPMTPREV          <- ifelse(is.infinite(WT_TOTAL$GPPREV*1000/WT_TOTAL$QprevYear),0,WT_TOTAL$GPPREV*1000/WT_TOTAL$QprevYear)
WT_TOTAL[is.na(WT_TOTAL)]      <- 0
WT_TOTAL$GPMTDIFF          <- WT_TOTAL$GPMTACT-WT_TOTAL$GPMTPREV
WT_TOTAL[is.na(WT_TOTAL)]      <- NA
WT_TOTAL$GPMTDIFFPER       <- ifelse(is.infinite((WT_TOTAL$GPMTACT-WT_TOTAL$GPMTPREV)/WT_TOTAL$GPMTPREV),"",scales::percent(((WT_TOTAL$GPMTACT-WT_TOTAL$GPMTPREV)/WT_TOTAL$GPMTPREV),accuracy = 0.1))
WT_TOTAL[is.na(WT_TOTAL)]      <- NA

DT                     <- data.table(WT_TOTAL)
WT_TOTAL                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
WT_TOTAL[is.na(WT_TOTAL)]      <- NA
WT_TOTAL$SubRegion         <- stri_replace_all_fixed(WT_TOTAL$SubRegion, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
WT_TOTAL                      <- setorder(WT_TOTAL,Region,-QuantityDiffYYPER_P)
WT_TOTAL$QuantityDiffYYPER_P <-NULL



Total<-data.frame(Region="Total CEE", SubRegion="Total CEE",
                  QActYear          = sum(WT_TOTAL$QActYear),
                  QprevYear         = sum(WT_TOTAL$QprevYear),
                  GPACT             = sum(WT_TOTAL$GPACT),
                  GPPREV            = sum(WT_TOTAL$GPPREV),
                  TURNACT           = sum(WT_TOTAL$TURNACT),
                  TURNPREV          = sum(WT_TOTAL$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(WT_TOTAL$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(WT_TOTAL$GPMTACT),
                  GPMTPREV          = sum(WT_TOTAL$GPMTPREV),
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


Regiony<-unique(WT_TOTAL$Region)

for(region in unique(WT_TOTAL$Region)){
  
  steal <- subset(WT_TOTAL,Region==region)
  frejm <- data.frame(Region=region, SubRegion=region, 
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
  
  
  WT_TOTAL <- rbind(WT_TOTAL, frejm)
  
}

WT_TOTAL                      <- rbind(WT_TOTAL,Total)
WT_TOTAL                      <- setorder(WT_TOTAL,Region)
WyboldowaneWierszeWT_TOTAL <- c(which(WT_TOTAL$SubRegion %in% Regiony),length(WT_TOTAL$Region))
WT_TOTAL$Region               <- NULL
WT_TOTAL                      <- WT_TOTAL %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
WT_TOTAL[is.na(WT_TOTAL)]    <- ""



# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

WT_TOTAL          <- WT_TOTAL %>% mutate_if(is.numeric, ~round(., 0)) 

## ----------------------------  CEE Region - Total results   (month)

WyboldowaneWierszeWT_TOTAL    <- c(which(WT_TOTAL$SubRegion %in% Regiony),length(WT_TOTAL$SubRegion))


## WT_TOTAL_YTD





WT_TOTAL_YTD<-WTMain



colnames(WT_TOTAL_YTD)[11]<-Naglowek1
colnames(WT_TOTAL_YTD)[12]<-Naglowek2
colnames(WT_TOTAL_YTD)[13]<-Naglowek3
colnames(WT_TOTAL_YTD)[14]<-Naglowek4
colnames(WT_TOTAL_YTD)[15]<-Naglowek5
colnames(WT_TOTAL_YTD)[16]<-Naglowek6



WT_TOTAL_YTD                   <- WT_TOTAL_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
WT_TOTAL_YTD                   <- WT_TOTAL_YTD %>% select(17,18,11,12,13,14,15,16)

# WT_TOTAL_YTD<-rbind(WT_TOTAL_YTD,WT_TOTAL_YTD2)
WT_TOTAL_YTD                   <- as.data.frame(WT_TOTAL_YTD)
WT_TOTAL_YTD                   <- WT_TOTAL_YTD %>% group_by(Region,SubRegion) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
WT_TOTAL_YTD[is.na(WT_TOTAL_YTD)]      <- NA
WT_TOTAL_YTD$QuantityDiffYY    <- WT_TOTAL_YTD$QActYear-WT_TOTAL_YTD$QprevYear
WT_TOTAL_YTD[is.na(WT_TOTAL_YTD)]      <- NA
WT_TOTAL_YTD$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((WT_TOTAL_YTD$QActYear-WT_TOTAL_YTD$QprevYear)/WT_TOTAL_YTD$QprevYear)),"",(WT_TOTAL_YTD$QActYear-WT_TOTAL_YTD$QprevYear)/WT_TOTAL_YTD$QprevYear))
WT_TOTAL_YTD$QuantityDiffYYPER <- ifelse(is.infinite(((WT_TOTAL_YTD$QActYear-WT_TOTAL_YTD$QprevYear)/WT_TOTAL_YTD$QprevYear)),"",scales::percent(((WT_TOTAL_YTD$QActYear-WT_TOTAL_YTD$QprevYear)/WT_TOTAL_YTD$QprevYear),accuracy = 0.1))
WT_TOTAL_YTD[is.na(WT_TOTAL_YTD)]      <- NA
WT_TOTAL_YTD$GPDiffYY          <- WT_TOTAL_YTD$GPACT-WT_TOTAL_YTD$GPPREV
WT_TOTAL_YTD[is.na(WT_TOTAL_YTD)]      <- NA
WT_TOTAL_YTD$GPDiffYYPER       <- ifelse(is.infinite(((WT_TOTAL_YTD$GPACT-WT_TOTAL_YTD$GPPREV)/WT_TOTAL_YTD$GPPREV)),"",scales::percent(((WT_TOTAL_YTD$GPACT-WT_TOTAL_YTD$GPPREV)/WT_TOTAL_YTD$GPPREV),accuracy = 0.1))
#WT_TOTAL_YTD$GPDiffYYPER       <- round(((WT_TOTAL_YTD$GPACT-WT_TOTAL_YTD$GPPREV)/WT_TOTAL_YTD$GPPREV)*100,2)
WT_TOTAL_YTD[is.na(WT_TOTAL_YTD)]      <- NA
WT_TOTAL_YTD$TurnDiffYY        <- WT_TOTAL_YTD$TURNACT-WT_TOTAL_YTD$TURNPREV
WT_TOTAL_YTD[is.na(WT_TOTAL_YTD)]      <- NA
WT_TOTAL_YTD$TurnDiffYYPER     <- ifelse(is.infinite(((WT_TOTAL_YTD$TURNACT-WT_TOTAL_YTD$TURNPREV)/WT_TOTAL_YTD$TURNPREV)),"",scales::percent(((WT_TOTAL_YTD$TURNACT-WT_TOTAL_YTD$TURNPREV)/WT_TOTAL_YTD$TURNPREV),accuracy = 0.1))
WT_TOTAL_YTD[is.na(WT_TOTAL_YTD)]      <- NA
WT_TOTAL_YTD$GPMTACT           <- ifelse(is.infinite(WT_TOTAL_YTD$GPACT*1000/WT_TOTAL_YTD$QActYear),0,WT_TOTAL_YTD$GPACT*1000/WT_TOTAL_YTD$QActYear)   # x 1000 ?
WT_TOTAL_YTD[is.na(WT_TOTAL_YTD)]      <- 0
WT_TOTAL_YTD$GPMTPREV          <- ifelse(is.infinite(WT_TOTAL_YTD$GPPREV*1000/WT_TOTAL_YTD$QprevYear),0,WT_TOTAL_YTD$GPPREV*1000/WT_TOTAL_YTD$QprevYear)
WT_TOTAL_YTD[is.na(WT_TOTAL_YTD)]      <- 0
WT_TOTAL_YTD$GPMTDIFF          <- WT_TOTAL_YTD$GPMTACT-WT_TOTAL_YTD$GPMTPREV
WT_TOTAL_YTD[is.na(WT_TOTAL_YTD)]      <- NA
WT_TOTAL_YTD$GPMTDIFFPER       <- ifelse(is.infinite((WT_TOTAL_YTD$GPMTACT-WT_TOTAL_YTD$GPMTPREV)/WT_TOTAL_YTD$GPMTPREV),"",scales::percent(((WT_TOTAL_YTD$GPMTACT-WT_TOTAL_YTD$GPMTPREV)/WT_TOTAL_YTD$GPMTPREV),accuracy = 0.1))
WT_TOTAL_YTD[is.na(WT_TOTAL_YTD)]      <- NA

DT                     <- data.table(WT_TOTAL_YTD)
WT_TOTAL_YTD                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
WT_TOTAL_YTD[is.na(WT_TOTAL_YTD)]      <- NA
WT_TOTAL_YTD$SubRegion         <- stri_replace_all_fixed(WT_TOTAL_YTD$SubRegion, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
WT_TOTAL_YTD                      <- setorder(WT_TOTAL_YTD,Region,-QuantityDiffYYPER_P)
WT_TOTAL_YTD$QuantityDiffYYPER_P <-NULL



Total<-data.frame(Region="Total CEE", SubRegion="Total CEE",
                  QActYear          = sum(WT_TOTAL_YTD$QActYear),
                  QprevYear         = sum(WT_TOTAL_YTD$QprevYear),
                  GPACT             = sum(WT_TOTAL_YTD$GPACT),
                  GPPREV            = sum(WT_TOTAL_YTD$GPPREV),
                  TURNACT           = sum(WT_TOTAL_YTD$TURNACT),
                  TURNPREV          = sum(WT_TOTAL_YTD$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(WT_TOTAL_YTD$TurnDiffYY),
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


Regiony<-unique(WT_TOTAL_YTD$Region)

for(region in unique(WT_TOTAL_YTD$Region)){
  
  steal <- subset(WT_TOTAL_YTD,Region==region)
  frejm <- data.frame(Region=region, SubRegion=region, 
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
  
  
  WT_TOTAL_YTD <- rbind(WT_TOTAL_YTD, frejm)
  
}

WT_TOTAL_YTD                      <- rbind(WT_TOTAL_YTD,Total)
WT_TOTAL_YTD                      <- setorder(WT_TOTAL_YTD,Region)
WyboldowaneWierszeWT_TOTAL_YTD <- c(which(WT_TOTAL_YTD$SubRegion %in% Regiony),length(WT_TOTAL_YTD$Region))
WT_TOTAL_YTD$Region               <- NULL
WT_TOTAL_YTD                      <- WT_TOTAL_YTD %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
WT_TOTAL_YTD[is.na(WT_TOTAL_YTD)]    <- ""

# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

WT_TOTAL_YTD          <- WT_TOTAL_YTD %>% mutate_if(is.numeric, ~round(., 0)) 

## ----------------------------  CEE Region - Total results   (month)

WyboldowaneWierszeWT_TOTAL_YTD    <- c(which(WT_TOTAL_YTD$SubRegion %in% Regiony),length(WT_TOTAL_YTD$SubRegion))



#### WT: CROSSSELING





WT_CROSS<-WTMain %>% subset(WaterClass=="WATER TREATMENT - Cross Selling")

#unique(WTMain$WaterClass)


colnames(WT_CROSS)[11]<-Naglowek1
colnames(WT_CROSS)[12]<-Naglowek2
colnames(WT_CROSS)[13]<-Naglowek3
colnames(WT_CROSS)[14]<-Naglowek4
colnames(WT_CROSS)[15]<-Naglowek5
colnames(WT_CROSS)[16]<-Naglowek6



WT_CROSS                   <- WT_CROSS %>% filter(`Calendar month`==MiesiacAnalizy)
WT_CROSS                   <- WT_CROSS %>% select(17,18,11,12,13,14,15,16)

# WT_CROSS<-rbind(WT_CROSS,WT_CROSS2)
WT_CROSS                   <- as.data.frame(WT_CROSS)
WT_CROSS                   <- WT_CROSS %>% group_by(Region,SubRegion) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
WT_CROSS[is.na(WT_CROSS)]      <- NA
WT_CROSS$QuantityDiffYY    <- WT_CROSS$QActYear-WT_CROSS$QprevYear
WT_CROSS[is.na(WT_CROSS)]      <- NA
WT_CROSS$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((WT_CROSS$QActYear-WT_CROSS$QprevYear)/WT_CROSS$QprevYear)),"",(WT_CROSS$QActYear-WT_CROSS$QprevYear)/WT_CROSS$QprevYear))
WT_CROSS$QuantityDiffYYPER <- ifelse(is.infinite(((WT_CROSS$QActYear-WT_CROSS$QprevYear)/WT_CROSS$QprevYear)),"",scales::percent(((WT_CROSS$QActYear-WT_CROSS$QprevYear)/WT_CROSS$QprevYear),accuracy = 0.1))
WT_CROSS[is.na(WT_CROSS)]      <- NA
WT_CROSS$GPDiffYY          <- WT_CROSS$GPACT-WT_CROSS$GPPREV
WT_CROSS[is.na(WT_CROSS)]      <- NA
WT_CROSS$GPDiffYYPER       <- ifelse(is.infinite(((WT_CROSS$GPACT-WT_CROSS$GPPREV)/WT_CROSS$GPPREV)),"",scales::percent(((WT_CROSS$GPACT-WT_CROSS$GPPREV)/WT_CROSS$GPPREV),accuracy = 0.1))
#WT_CROSS$GPDiffYYPER       <- round(((WT_CROSS$GPACT-WT_CROSS$GPPREV)/WT_CROSS$GPPREV)*100,2)
WT_CROSS[is.na(WT_CROSS)]      <- NA
WT_CROSS$TurnDiffYY        <- WT_CROSS$TURNACT-WT_CROSS$TURNPREV
WT_CROSS[is.na(WT_CROSS)]      <- NA
WT_CROSS$TurnDiffYYPER     <- ifelse(is.infinite(((WT_CROSS$TURNACT-WT_CROSS$TURNPREV)/WT_CROSS$TURNPREV)),"",scales::percent(((WT_CROSS$TURNACT-WT_CROSS$TURNPREV)/WT_CROSS$TURNPREV),accuracy = 0.1))
WT_CROSS[is.na(WT_CROSS)]      <- NA
WT_CROSS$GPMTACT           <- ifelse(is.infinite(WT_CROSS$GPACT*1000/WT_CROSS$QActYear),0,WT_CROSS$GPACT*1000/WT_CROSS$QActYear)   # x 1000 ?
WT_CROSS[is.na(WT_CROSS)]      <- 0
WT_CROSS$GPMTPREV          <- ifelse(is.infinite(WT_CROSS$GPPREV*1000/WT_CROSS$QprevYear),0,WT_CROSS$GPPREV*1000/WT_CROSS$QprevYear)
WT_CROSS[is.na(WT_CROSS)]      <- 0
WT_CROSS$GPMTDIFF          <- WT_CROSS$GPMTACT-WT_CROSS$GPMTPREV
WT_CROSS[is.na(WT_CROSS)]      <- NA
WT_CROSS$GPMTDIFFPER       <- ifelse(is.infinite((WT_CROSS$GPMTACT-WT_CROSS$GPMTPREV)/WT_CROSS$GPMTPREV),"",scales::percent(((WT_CROSS$GPMTACT-WT_CROSS$GPMTPREV)/WT_CROSS$GPMTPREV),accuracy = 0.1))
WT_CROSS[is.na(WT_CROSS)]      <- NA

DT                     <- data.table(WT_CROSS)
WT_CROSS                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
WT_CROSS[is.na(WT_CROSS)]      <- NA
WT_CROSS$SubRegion         <- stri_replace_all_fixed(WT_CROSS$SubRegion, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
WT_CROSS                      <- setorder(WT_CROSS,Region,-QuantityDiffYYPER_P)
WT_CROSS$QuantityDiffYYPER_P <-NULL



Total<-data.frame(Region="Total CEE", SubRegion="Total CEE",
                  QActYear          = sum(WT_CROSS$QActYear),
                  QprevYear         = sum(WT_CROSS$QprevYear),
                  GPACT             = sum(WT_CROSS$GPACT),
                  GPPREV            = sum(WT_CROSS$GPPREV),
                  TURNACT           = sum(WT_CROSS$TURNACT),
                  TURNPREV          = sum(WT_CROSS$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(WT_CROSS$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(WT_CROSS$GPMTACT),
                  GPMTPREV          = sum(WT_CROSS$GPMTPREV),
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


Regiony<-unique(WT_CROSS$Region)

for(region in unique(WT_CROSS$Region)){
  
  steal <- subset(WT_CROSS,Region==region)
  frejm <- data.frame(Region=region, SubRegion=region, 
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
  
  
  WT_CROSS <- rbind(WT_CROSS, frejm)
  
}

WT_CROSS                      <- rbind(WT_CROSS,Total)
WT_CROSS                      <- setorder(WT_CROSS,Region)
WyboldowaneWierszeWT_CROSS <- c(which(WT_CROSS$SubRegion %in% Regiony),length(WT_CROSS$Region))
WT_CROSS$Region               <- NULL
WT_CROSS                      <- WT_CROSS %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
WT_CROSS[is.na(WT_CROSS)]    <- ""



# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

WT_CROSS          <- WT_CROSS %>% mutate_if(is.numeric, ~round(., 0)) 

## ----------------------------  CEE Region - Total results   (month)

WyboldowaneWierszeWT_CROSS    <- c(which(WT_CROSS$SubRegion %in% Regiony),length(WT_CROSS$SubRegion))


## WT_TOTAL_YTD





WT_CROSS_YTD<-WTMain %>% subset(WaterClass=="WATER TREATMENT - Cross Selling")



colnames(WT_CROSS_YTD)[11]<-Naglowek1
colnames(WT_CROSS_YTD)[12]<-Naglowek2
colnames(WT_CROSS_YTD)[13]<-Naglowek3
colnames(WT_CROSS_YTD)[14]<-Naglowek4
colnames(WT_CROSS_YTD)[15]<-Naglowek5
colnames(WT_CROSS_YTD)[16]<-Naglowek6



WT_CROSS_YTD                   <- WT_CROSS_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
WT_CROSS_YTD                   <- WT_CROSS_YTD %>% select(17,18,11,12,13,14,15,16)

# WT_CROSS_YTD<-rbind(WT_CROSS_YTD,WT_CROSS_YTD2)
WT_CROSS_YTD                   <- as.data.frame(WT_CROSS_YTD)
WT_CROSS_YTD                   <- WT_CROSS_YTD %>% group_by(Region,SubRegion) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
WT_CROSS_YTD[is.na(WT_CROSS_YTD)]      <- NA
WT_CROSS_YTD$QuantityDiffYY    <- WT_CROSS_YTD$QActYear-WT_CROSS_YTD$QprevYear
WT_CROSS_YTD[is.na(WT_CROSS_YTD)]      <- NA
WT_CROSS_YTD$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((WT_CROSS_YTD$QActYear-WT_CROSS_YTD$QprevYear)/WT_CROSS_YTD$QprevYear)),"",(WT_CROSS_YTD$QActYear-WT_CROSS_YTD$QprevYear)/WT_CROSS_YTD$QprevYear))
WT_CROSS_YTD$QuantityDiffYYPER <- ifelse(is.infinite(((WT_CROSS_YTD$QActYear-WT_CROSS_YTD$QprevYear)/WT_CROSS_YTD$QprevYear)),"",scales::percent(((WT_CROSS_YTD$QActYear-WT_CROSS_YTD$QprevYear)/WT_CROSS_YTD$QprevYear),accuracy = 0.1))
WT_CROSS_YTD[is.na(WT_CROSS_YTD)]      <- NA
WT_CROSS_YTD$GPDiffYY          <- WT_CROSS_YTD$GPACT-WT_CROSS_YTD$GPPREV
WT_CROSS_YTD[is.na(WT_CROSS_YTD)]      <- NA
WT_CROSS_YTD$GPDiffYYPER       <- ifelse(is.infinite(((WT_CROSS_YTD$GPACT-WT_CROSS_YTD$GPPREV)/WT_CROSS_YTD$GPPREV)),"",scales::percent(((WT_CROSS_YTD$GPACT-WT_CROSS_YTD$GPPREV)/WT_CROSS_YTD$GPPREV),accuracy = 0.1))
#WT_CROSS_YTD$GPDiffYYPER       <- round(((WT_CROSS_YTD$GPACT-WT_CROSS_YTD$GPPREV)/WT_CROSS_YTD$GPPREV)*100,2)
WT_CROSS_YTD[is.na(WT_CROSS_YTD)]      <- NA
WT_CROSS_YTD$TurnDiffYY        <- WT_CROSS_YTD$TURNACT-WT_CROSS_YTD$TURNPREV
WT_CROSS_YTD[is.na(WT_CROSS_YTD)]      <- NA
WT_CROSS_YTD$TurnDiffYYPER     <- ifelse(is.infinite(((WT_CROSS_YTD$TURNACT-WT_CROSS_YTD$TURNPREV)/WT_CROSS_YTD$TURNPREV)),"",scales::percent(((WT_CROSS_YTD$TURNACT-WT_CROSS_YTD$TURNPREV)/WT_CROSS_YTD$TURNPREV),accuracy = 0.1))
WT_CROSS_YTD[is.na(WT_CROSS_YTD)]      <- NA
WT_CROSS_YTD$GPMTACT           <- ifelse(is.infinite(WT_CROSS_YTD$GPACT*1000/WT_CROSS_YTD$QActYear),0,WT_CROSS_YTD$GPACT*1000/WT_CROSS_YTD$QActYear)   # x 1000 ?
WT_CROSS_YTD[is.na(WT_CROSS_YTD)]      <- 0
WT_CROSS_YTD$GPMTPREV          <- ifelse(is.infinite(WT_CROSS_YTD$GPPREV*1000/WT_CROSS_YTD$QprevYear),0,WT_CROSS_YTD$GPPREV*1000/WT_CROSS_YTD$QprevYear)
WT_CROSS_YTD[is.na(WT_CROSS_YTD)]      <- 0
WT_CROSS_YTD$GPMTDIFF          <- WT_CROSS_YTD$GPMTACT-WT_CROSS_YTD$GPMTPREV
WT_CROSS_YTD[is.na(WT_CROSS_YTD)]      <- NA
WT_CROSS_YTD$GPMTDIFFPER       <- ifelse(is.infinite((WT_CROSS_YTD$GPMTACT-WT_CROSS_YTD$GPMTPREV)/WT_CROSS_YTD$GPMTPREV),"",scales::percent(((WT_CROSS_YTD$GPMTACT-WT_CROSS_YTD$GPMTPREV)/WT_CROSS_YTD$GPMTPREV),accuracy = 0.1))
WT_CROSS_YTD[is.na(WT_CROSS_YTD)]      <- NA

DT                     <- data.table(WT_CROSS_YTD)
WT_CROSS_YTD                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
WT_CROSS_YTD[is.na(WT_CROSS_YTD)]      <- NA
WT_CROSS_YTD$SubRegion         <- stri_replace_all_fixed(WT_CROSS_YTD$SubRegion, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
WT_CROSS_YTD                      <- setorder(WT_CROSS_YTD,Region,-QuantityDiffYYPER_P)
WT_CROSS_YTD$QuantityDiffYYPER_P <-NULL



Total<-data.frame(Region="Total CEE", SubRegion="Total CEE",
                  QActYear          = sum(WT_CROSS_YTD$QActYear),
                  QprevYear         = sum(WT_CROSS_YTD$QprevYear),
                  GPACT             = sum(WT_CROSS_YTD$GPACT),
                  GPPREV            = sum(WT_CROSS_YTD$GPPREV),
                  TURNACT           = sum(WT_CROSS_YTD$TURNACT),
                  TURNPREV          = sum(WT_CROSS_YTD$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(WT_CROSS_YTD$TurnDiffYY),
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


Regiony<-unique(WT_CROSS_YTD$Region)

for(region in unique(WT_CROSS_YTD$Region)){
  
  steal <- subset(WT_CROSS_YTD,Region==region)
  frejm <- data.frame(Region=region, SubRegion=region, 
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
  
  
  WT_CROSS_YTD <- rbind(WT_CROSS_YTD, frejm)
  
}

WT_CROSS_YTD                      <- rbind(WT_CROSS_YTD,Total)
WT_CROSS_YTD                      <- setorder(WT_CROSS_YTD,Region)
WyboldowaneWierszeWT_CROSS_YTD <- c(which(WT_CROSS_YTD$SubRegion %in% Regiony),length(WT_CROSS_YTD$Region))
WT_CROSS_YTD$Region               <- NULL
WT_CROSS_YTD                      <- WT_CROSS_YTD %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
WT_CROSS_YTD[is.na(WT_CROSS_YTD)]    <- ""

# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

WT_CROSS_YTD          <- WT_CROSS_YTD %>% mutate_if(is.numeric, ~round(., 0)) 

## ----------------------------  CEE Region - Total results   (month)

WyboldowaneWierszeWT_CROSS_YTD    <- c(which(WT_CROSS_YTD$SubRegion %in% Regiony),length(WT_CROSS_YTD$SubRegion))





##### thirdParty (month) CROSSSELLING


WT_THIRD<-WTMain %>% subset(WaterClass=="WATER TREATMENT - Cross Selling")




colnames(WT_THIRD)[11]<-Naglowek1
colnames(WT_THIRD)[12]<-Naglowek2
colnames(WT_THIRD)[13]<-Naglowek3
colnames(WT_THIRD)[14]<-Naglowek4
colnames(WT_THIRD)[15]<-Naglowek5
colnames(WT_THIRD)[16]<-Naglowek6

WT_THIRD                   <- WT_THIRD %>% filter(`Calendar month`==MiesiacAnalizy)
WT_THIRD                   <- WT_THIRD %>% select(17,19,11,12,13,14,15,16)

# WT_THIRD<-rbind(WT_THIRD,WT_THIRD2)
WT_THIRD                   <- as.data.frame(WT_THIRD)
WT_THIRD                   <- WT_THIRD %>% group_by(Region,Clasification) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
WT_THIRD[is.na(WT_THIRD)]      <- NA
WT_THIRD$QuantityDiffYY    <- WT_THIRD$QActYear-WT_THIRD$QprevYear
WT_THIRD[is.na(WT_THIRD)]      <- NA
WT_THIRD$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((WT_THIRD$QActYear-WT_THIRD$QprevYear)/WT_THIRD$QprevYear)),"",(WT_THIRD$QActYear-WT_THIRD$QprevYear)/WT_THIRD$QprevYear))

WT_THIRD$QuantityDiffYYPER <- ifelse(is.infinite(((WT_THIRD$QActYear-WT_THIRD$QprevYear)/WT_THIRD$QprevYear)),"",ifelse(is.na(((WT_THIRD$QActYear-WT_THIRD$QprevYear)/WT_THIRD$QprevYear)),"",  scales::percent(((WT_THIRD$QActYear-WT_THIRD$QprevYear)/WT_THIRD$QprevYear),accuracy = 0.1)))
WT_THIRD[is.na(WT_THIRD)]      <- NA
WT_THIRD$GPDiffYY          <- WT_THIRD$GPACT-WT_THIRD$GPPREV
WT_THIRD[is.na(WT_THIRD)]      <- NA
WT_THIRD$GPDiffYYPER       <- ifelse(is.infinite(((WT_THIRD$GPACT-WT_THIRD$GPPREV)/WT_THIRD$GPPREV)),"",ifelse(is.na(((WT_THIRD$GPACT-WT_THIRD$GPPREV)/WT_THIRD$GPPREV)),"",scales::percent(((WT_THIRD$GPACT-WT_THIRD$GPPREV)/WT_THIRD$GPPREV),accuracy = 0.1)))
#WT_THIRD$GPDiffYYPER       <- round(((WT_THIRD$GPACT-WT_THIRD$GPPREV)/WT_THIRD$GPPREV)*100,2)
WT_THIRD[is.na(WT_THIRD)]      <- NA
WT_THIRD$TurnDiffYY        <- WT_THIRD$TURNACT-WT_THIRD$TURNPREV
WT_THIRD[is.na(WT_THIRD)]      <- NA
WT_THIRD$TurnDiffYYPER     <- ifelse(is.infinite(((WT_THIRD$TURNACT-WT_THIRD$TURNPREV)/WT_THIRD$TURNPREV)),"",ifelse(is.na(((WT_THIRD$TURNACT-WT_THIRD$TURNPREV)/WT_THIRD$TURNPREV)),"",scales::percent(((WT_THIRD$TURNACT-WT_THIRD$TURNPREV)/WT_THIRD$TURNPREV),accuracy = 0.1)))
WT_THIRD[is.na(WT_THIRD)]      <- NA
WT_THIRD$GPMTACT           <- ifelse(is.infinite(WT_THIRD$GPACT*1000/WT_THIRD$QActYear),0,WT_THIRD$GPACT*1000/WT_THIRD$QActYear)   # x 1000 ?
WT_THIRD[is.na(WT_THIRD)]      <- 0
WT_THIRD$GPMTPREV          <- ifelse(is.infinite(WT_THIRD$GPPREV*1000/WT_THIRD$QprevYear),0,WT_THIRD$GPPREV*1000/WT_THIRD$QprevYear)
WT_THIRD[is.na(WT_THIRD)]      <- 0
WT_THIRD$GPMTDIFF          <- WT_THIRD$GPMTACT-WT_THIRD$GPMTPREV
WT_THIRD[is.na(WT_THIRD)]      <- NA
WT_THIRD$GPMTDIFFPER       <- ifelse(is.infinite((WT_THIRD$GPMTACT-WT_THIRD$GPMTPREV)/WT_THIRD$GPMTPREV),"",ifelse(is.na(((WT_THIRD$GPMTACT-WT_THIRD$GPMTPREV)/WT_THIRD$GPMTPREV)),"",scales::percent(((WT_THIRD$GPMTACT-WT_THIRD$GPMTPREV)/WT_THIRD$GPMTPREV),accuracy = 0.1)))
WT_THIRD[is.na(WT_THIRD)]      <- NA

DT                     <- data.table(WT_THIRD)
WT_THIRD                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
WT_THIRD[is.na(WT_THIRD)]      <- NA
WT_THIRD$Clasification     <- stri_replace_all_fixed(WT_THIRD$Clasification, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
WT_THIRD                      <- setorder(WT_THIRD,Region,-QuantityDiffYYPER_P)
WT_THIRD$QuantityDiffYYPER_P <-NULL


Total<-data.frame(Region="Total CEE", Clasification="Total CEE",
                  QActYear          = sum(WT_THIRD$QActYear),
                  QprevYear         = sum(WT_THIRD$QprevYear),
                  GPACT             = sum(WT_THIRD$GPACT),
                  GPPREV            = sum(WT_THIRD$GPPREV),
                  TURNACT           = sum(WT_THIRD$TURNACT),
                  TURNPREV          = sum(WT_THIRD$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(WT_THIRD$TurnDiffYY),
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


Regiony<-unique(WT_THIRD$Region)

for(region in unique(WT_THIRD$Region)){
  
  steal <- subset(WT_THIRD,Region==region)
  frejm <- data.frame(Region=region, Clasification=region, 
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
  
  
  WT_THIRD <- rbind(WT_THIRD, frejm)
  
}

WT_THIRD                      <- rbind(WT_THIRD,Total)
WT_THIRD                      <- setorder(WT_THIRD,Region)
WT_THIRD$Region               <- NULL
WT_THIRD                      <- WT_THIRD %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
WT_THIRD[is.na(WT_THIRD)]    <- ""



# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

WT_THIRD          <- WT_THIRD %>% mutate_if(is.numeric, ~round(., 0)) 

## ----------------------------  CEE Region - Total results   (month)

WyboldowaneWierszeWT_THIRD    <- c(which(WT_THIRD$Clasification %in% Regiony),length(WT_THIRD$Clasification))


## WT_TOTAL_YTD





WT_THIRD_YTD<-WTMain %>% subset(WaterClass=="WATER TREATMENT - Cross Selling")




colnames(WT_THIRD_YTD)[11]<-Naglowek1
colnames(WT_THIRD_YTD)[12]<-Naglowek2
colnames(WT_THIRD_YTD)[13]<-Naglowek3
colnames(WT_THIRD_YTD)[14]<-Naglowek4
colnames(WT_THIRD_YTD)[15]<-Naglowek5
colnames(WT_THIRD_YTD)[16]<-Naglowek6



WT_THIRD_YTD                   <- WT_THIRD_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
WT_THIRD_YTD                   <- WT_THIRD_YTD %>% select(17,19,11,12,13,14,15,16)

# WT_THIRD_YTD<-rbind(WT_THIRD_YTD,WT_THIRD_YTD2)
WT_THIRD_YTD                   <- as.data.frame(WT_THIRD_YTD)
WT_THIRD_YTD                   <- WT_THIRD_YTD %>% group_by(Region,Clasification) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
WT_THIRD_YTD[is.na(WT_THIRD_YTD)]      <- NA
WT_THIRD_YTD$QuantityDiffYY    <- WT_THIRD_YTD$QActYear-WT_THIRD_YTD$QprevYear
WT_THIRD_YTD[is.na(WT_THIRD_YTD)]      <- NA
WT_THIRD_YTD$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((WT_THIRD_YTD$QActYear-WT_THIRD_YTD$QprevYear)/WT_THIRD_YTD$QprevYear)),"",(WT_THIRD_YTD$QActYear-WT_THIRD_YTD$QprevYear)/WT_THIRD_YTD$QprevYear))

WT_THIRD_YTD$QuantityDiffYYPER <- ifelse(is.infinite(((WT_THIRD_YTD$QActYear-WT_THIRD_YTD$QprevYear)/WT_THIRD_YTD$QprevYear)),"",ifelse(is.na(((WT_THIRD_YTD$QActYear-WT_THIRD_YTD$QprevYear)/WT_THIRD_YTD$QprevYear)),"",  scales::percent(((WT_THIRD_YTD$QActYear-WT_THIRD_YTD$QprevYear)/WT_THIRD_YTD$QprevYear),accuracy = 0.1)))
WT_THIRD_YTD[is.na(WT_THIRD_YTD)]      <- NA
WT_THIRD_YTD$GPDiffYY          <- WT_THIRD_YTD$GPACT-WT_THIRD_YTD$GPPREV
WT_THIRD_YTD[is.na(WT_THIRD_YTD)]      <- NA
WT_THIRD_YTD$GPDiffYYPER       <- ifelse(is.infinite(((WT_THIRD_YTD$GPACT-WT_THIRD_YTD$GPPREV)/WT_THIRD_YTD$GPPREV)),"",ifelse(is.na(((WT_THIRD_YTD$GPACT-WT_THIRD_YTD$GPPREV)/WT_THIRD_YTD$GPPREV)),"",scales::percent(((WT_THIRD_YTD$GPACT-WT_THIRD_YTD$GPPREV)/WT_THIRD_YTD$GPPREV),accuracy = 0.1)))
#WT_THIRD_YTD$GPDiffYYPER       <- round(((WT_THIRD_YTD$GPACT-WT_THIRD_YTD$GPPREV)/WT_THIRD_YTD$GPPREV)*100,2)
WT_THIRD_YTD[is.na(WT_THIRD_YTD)]      <- NA
WT_THIRD_YTD$TurnDiffYY        <- WT_THIRD_YTD$TURNACT-WT_THIRD_YTD$TURNPREV
WT_THIRD_YTD[is.na(WT_THIRD_YTD)]      <- NA
WT_THIRD_YTD$TurnDiffYYPER     <- ifelse(is.infinite(((WT_THIRD_YTD$TURNACT-WT_THIRD_YTD$TURNPREV)/WT_THIRD_YTD$TURNPREV)),"",ifelse(is.na(((WT_THIRD_YTD$TURNACT-WT_THIRD_YTD$TURNPREV)/WT_THIRD_YTD$TURNPREV)),"",scales::percent(((WT_THIRD_YTD$TURNACT-WT_THIRD_YTD$TURNPREV)/WT_THIRD_YTD$TURNPREV),accuracy = 0.1)))
WT_THIRD_YTD[is.na(WT_THIRD_YTD)]      <- NA
WT_THIRD_YTD$GPMTACT           <- ifelse(is.infinite(WT_THIRD_YTD$GPACT*1000/WT_THIRD_YTD$QActYear),0,WT_THIRD_YTD$GPACT*1000/WT_THIRD_YTD$QActYear)   # x 1000 ?
WT_THIRD_YTD[is.na(WT_THIRD_YTD)]      <- 0
WT_THIRD_YTD$GPMTPREV          <- ifelse(is.infinite(WT_THIRD_YTD$GPPREV*1000/WT_THIRD_YTD$QprevYear),0,WT_THIRD_YTD$GPPREV*1000/WT_THIRD_YTD$QprevYear)
WT_THIRD_YTD[is.na(WT_THIRD_YTD)]      <- 0
WT_THIRD_YTD$GPMTDIFF          <- WT_THIRD_YTD$GPMTACT-WT_THIRD_YTD$GPMTPREV
WT_THIRD_YTD[is.na(WT_THIRD_YTD)]      <- NA
WT_THIRD_YTD$GPMTDIFFPER       <- ifelse(is.infinite((WT_THIRD_YTD$GPMTACT-WT_THIRD_YTD$GPMTPREV)/WT_THIRD_YTD$GPMTPREV),"",ifelse(is.na(((WT_THIRD_YTD$GPMTACT-WT_THIRD_YTD$GPMTPREV)/WT_THIRD_YTD$GPMTPREV)),"",scales::percent(((WT_THIRD_YTD$GPMTACT-WT_THIRD_YTD$GPMTPREV)/WT_THIRD_YTD$GPMTPREV),accuracy = 0.1)))
WT_THIRD_YTD[is.na(WT_THIRD_YTD)]      <- NA

DT                     <- data.table(WT_THIRD_YTD)
WT_THIRD_YTD                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
WT_THIRD_YTD[is.na(WT_THIRD_YTD)]      <- NA
WT_THIRD_YTD$Clasification     <- stri_replace_all_fixed(WT_THIRD_YTD$Clasification, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
WT_THIRD_YTD                      <- setorder(WT_THIRD_YTD,Region,-QuantityDiffYYPER_P)
WT_THIRD_YTD$QuantityDiffYYPER_P <-NULL


Total<-data.frame(Region="Total CEE", Clasification="Total CEE",
                  QActYear          = sum(WT_THIRD_YTD$QActYear),
                  QprevYear         = sum(WT_THIRD_YTD$QprevYear),
                  GPACT             = sum(WT_THIRD_YTD$GPACT),
                  GPPREV            = sum(WT_THIRD_YTD$GPPREV),
                  TURNACT           = sum(WT_THIRD_YTD$TURNACT),
                  TURNPREV          = sum(WT_THIRD_YTD$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(WT_THIRD_YTD$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(WT_THIRD_YTD$GPMTACT),
                  GPMTPREV          = sum(WT_THIRD_YTD$GPMTPREV),
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


Regiony<-unique(WT_THIRD_YTD$Region)

for(region in unique(WT_THIRD_YTD$Region)){
  
  steal <- subset(WT_THIRD_YTD,Region==region)
  frejm <- data.frame(Region=region, Clasification=region, 
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
  
  
  WT_THIRD_YTD <- rbind(WT_THIRD_YTD, frejm)
  
}

WT_THIRD_YTD                      <- rbind(WT_THIRD_YTD,Total)
WT_THIRD_YTD                      <- setorder(WT_THIRD_YTD,Region)
WT_THIRD_YTD$Region               <- NULL
WT_THIRD_YTD                      <- WT_THIRD_YTD %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
WT_THIRD_YTD[is.na(WT_THIRD_YTD)]    <- ""



# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

WT_THIRD_YTD          <- WT_THIRD_YTD %>% mutate_if(is.numeric, ~round(., 0)) 

## ----------------------------  CEE Region - Total results   (month)

WyboldowaneWierszeWT_THIRD_YTD    <- c(which(WT_THIRD_YTD$Clasification %in% Regiony),length(WT_THIRD_YTD$Clasification))





### CROSS SELING BY INDUSTRY


WT_CS_IND<-WTMain %>% subset(WaterClass=="WATER TREATMENT - Cross Selling")




colnames(WT_CS_IND)[11]<-Naglowek1
colnames(WT_CS_IND)[12]<-Naglowek2
colnames(WT_CS_IND)[13]<-Naglowek3
colnames(WT_CS_IND)[14]<-Naglowek4
colnames(WT_CS_IND)[15]<-Naglowek5
colnames(WT_CS_IND)[16]<-Naglowek6

WT_CS_IND                   <- WT_CS_IND %>% filter(`Calendar month`==MiesiacAnalizy)
WT_CS_IND                   <- WT_CS_IND %>% select(4,11,12,13,14,15,16)

# WT_CS_IND<-rbind(WT_CS_IND,WT_CS_IND2)
WT_CS_IND                   <- as.data.frame(WT_CS_IND)
WT_CS_IND                   <- WT_CS_IND %>% group_by(`European Industry`) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
WT_CS_IND[is.na(WT_CS_IND)]      <- NA
WT_CS_IND$QuantityDiffYY    <- WT_CS_IND$QActYear-WT_CS_IND$QprevYear
WT_CS_IND[is.na(WT_CS_IND)]      <- NA
WT_CS_IND$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((WT_CS_IND$QActYear-WT_CS_IND$QprevYear)/WT_CS_IND$QprevYear)),"",(WT_CS_IND$QActYear-WT_CS_IND$QprevYear)/WT_CS_IND$QprevYear))

WT_CS_IND$QuantityDiffYYPER <- ifelse(is.infinite(((WT_CS_IND$QActYear-WT_CS_IND$QprevYear)/WT_CS_IND$QprevYear)),"",ifelse(is.na(((WT_CS_IND$QActYear-WT_CS_IND$QprevYear)/WT_CS_IND$QprevYear)),"",  scales::percent(((WT_CS_IND$QActYear-WT_CS_IND$QprevYear)/WT_CS_IND$QprevYear),accuracy = 0.1)))
WT_CS_IND[is.na(WT_CS_IND)]      <- NA
WT_CS_IND$GPDiffYY          <- WT_CS_IND$GPACT-WT_CS_IND$GPPREV
WT_CS_IND[is.na(WT_CS_IND)]      <- NA
WT_CS_IND$GPDiffYYPER       <- ifelse(is.infinite(((WT_CS_IND$GPACT-WT_CS_IND$GPPREV)/WT_CS_IND$GPPREV)),"",ifelse(is.na(((WT_CS_IND$GPACT-WT_CS_IND$GPPREV)/WT_CS_IND$GPPREV)),"",scales::percent(((WT_CS_IND$GPACT-WT_CS_IND$GPPREV)/WT_CS_IND$GPPREV),accuracy = 0.1)))
#WT_CS_IND$GPDiffYYPER       <- round(((WT_CS_IND$GPACT-WT_CS_IND$GPPREV)/WT_CS_IND$GPPREV)*100,2)
WT_CS_IND[is.na(WT_CS_IND)]      <- NA
WT_CS_IND$TurnDiffYY        <- WT_CS_IND$TURNACT-WT_CS_IND$TURNPREV
WT_CS_IND[is.na(WT_CS_IND)]      <- NA
WT_CS_IND$TurnDiffYYPER     <- ifelse(is.infinite(((WT_CS_IND$TURNACT-WT_CS_IND$TURNPREV)/WT_CS_IND$TURNPREV)),"",ifelse(is.na(((WT_CS_IND$TURNACT-WT_CS_IND$TURNPREV)/WT_CS_IND$TURNPREV)),"",scales::percent(((WT_CS_IND$TURNACT-WT_CS_IND$TURNPREV)/WT_CS_IND$TURNPREV),accuracy = 0.1)))
WT_CS_IND[is.na(WT_CS_IND)]      <- NA
WT_CS_IND$GPMTACT           <- ifelse(is.infinite(WT_CS_IND$GPACT*1000/WT_CS_IND$QActYear),0,WT_CS_IND$GPACT*1000/WT_CS_IND$QActYear)   # x 1000 ?
WT_CS_IND[is.na(WT_CS_IND)]      <- 0
WT_CS_IND$GPMTPREV          <- ifelse(is.infinite(WT_CS_IND$GPPREV*1000/WT_CS_IND$QprevYear),0,WT_CS_IND$GPPREV*1000/WT_CS_IND$QprevYear)
WT_CS_IND[is.na(WT_CS_IND)]      <- 0
WT_CS_IND$GPMTDIFF          <- WT_CS_IND$GPMTACT-WT_CS_IND$GPMTPREV
WT_CS_IND[is.na(WT_CS_IND)]      <- NA
WT_CS_IND$GPMTDIFFPER       <- ifelse(is.infinite((WT_CS_IND$GPMTACT-WT_CS_IND$GPMTPREV)/WT_CS_IND$GPMTPREV),"",ifelse(is.na(((WT_CS_IND$GPMTACT-WT_CS_IND$GPMTPREV)/WT_CS_IND$GPMTPREV)),"",scales::percent(((WT_CS_IND$GPMTACT-WT_CS_IND$GPMTPREV)/WT_CS_IND$GPMTPREV),accuracy = 0.1)))
WT_CS_IND[is.na(WT_CS_IND)]      <- NA

DT                     <- data.table(WT_CS_IND)
WT_CS_IND                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
WT_CS_IND[is.na(WT_CS_IND)]      <- NA
WT_CS_IND$`European Industry`     <- stri_replace_all_fixed(WT_CS_IND$`European Industry`, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
WT_CS_IND                      <- setorder(WT_CS_IND,-QuantityDiffYYPER_P)
WT_CS_IND$QuantityDiffYYPER_P <-NULL


Total<-data.frame(`European Industry`="Total CEE", Clasification="Total CEE",
                  QActYear          = sum(WT_CS_IND$QActYear),
                  QprevYear         = sum(WT_CS_IND$QprevYear),
                  GPACT             = sum(WT_CS_IND$GPACT),
                  GPPREV            = sum(WT_CS_IND$GPPREV),
                  TURNACT           = sum(WT_CS_IND$TURNACT),
                  TURNPREV          = sum(WT_CS_IND$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(WT_CS_IND$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(WT_CS_IND$GPMTACT),
                  GPMTPREV          = sum(WT_CS_IND$GPMTPREV),
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

colnames(Total)[1]     <- "European Industry"
Total$Clasification             <- NULL
WT_CS_IND                      <- rbind(WT_CS_IND,Total)


WT_CS_IND                      <- WT_CS_IND %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
WT_CS_IND[is.na(WT_CS_IND)]    <- ""



# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

WT_CS_IND          <- WT_CS_IND %>% mutate_if(is.numeric, ~round(., 0)) 

## ----------------------------  CEE Region - Total results   (month)

WyboldowaneWierszeWT_CS_IND    <- length(WT_CS_IND$`European Industry`)


## WT_CS_IND


### CROSS SELING BY INDUSTRY


WT_CS_IND_YTD<-WTMain %>% subset(WaterClass=="WATER TREATMENT - Cross Selling")




colnames(WT_CS_IND_YTD)[11]<-Naglowek1
colnames(WT_CS_IND_YTD)[12]<-Naglowek2
colnames(WT_CS_IND_YTD)[13]<-Naglowek3
colnames(WT_CS_IND_YTD)[14]<-Naglowek4
colnames(WT_CS_IND_YTD)[15]<-Naglowek5
colnames(WT_CS_IND_YTD)[16]<-Naglowek6

WT_CS_IND_YTD                   <- WT_CS_IND_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
WT_CS_IND_YTD                   <- WT_CS_IND_YTD %>% select(4,11,12,13,14,15,16)

# WT_CS_IND_YTD<-rbind(WT_CS_IND_YTD,WT_CS_IND_YTD2)
WT_CS_IND_YTD                   <- as.data.frame(WT_CS_IND_YTD)
WT_CS_IND_YTD                   <- WT_CS_IND_YTD %>% group_by(`European Industry`) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
WT_CS_IND_YTD[is.na(WT_CS_IND_YTD)]      <- NA
WT_CS_IND_YTD$QuantityDiffYY    <- WT_CS_IND_YTD$QActYear-WT_CS_IND_YTD$QprevYear
WT_CS_IND_YTD[is.na(WT_CS_IND_YTD)]      <- NA
WT_CS_IND_YTD$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((WT_CS_IND_YTD$QActYear-WT_CS_IND_YTD$QprevYear)/WT_CS_IND_YTD$QprevYear)),"",(WT_CS_IND_YTD$QActYear-WT_CS_IND_YTD$QprevYear)/WT_CS_IND_YTD$QprevYear))

WT_CS_IND_YTD$QuantityDiffYYPER <- ifelse(is.infinite(((WT_CS_IND_YTD$QActYear-WT_CS_IND_YTD$QprevYear)/WT_CS_IND_YTD$QprevYear)),"",ifelse(is.na(((WT_CS_IND_YTD$QActYear-WT_CS_IND_YTD$QprevYear)/WT_CS_IND_YTD$QprevYear)),"",  scales::percent(((WT_CS_IND_YTD$QActYear-WT_CS_IND_YTD$QprevYear)/WT_CS_IND_YTD$QprevYear),accuracy = 0.1)))
WT_CS_IND_YTD[is.na(WT_CS_IND_YTD)]      <- NA
WT_CS_IND_YTD$GPDiffYY          <- WT_CS_IND_YTD$GPACT-WT_CS_IND_YTD$GPPREV
WT_CS_IND_YTD[is.na(WT_CS_IND_YTD)]      <- NA
WT_CS_IND_YTD$GPDiffYYPER       <- ifelse(is.infinite(((WT_CS_IND_YTD$GPACT-WT_CS_IND_YTD$GPPREV)/WT_CS_IND_YTD$GPPREV)),"",ifelse(is.na(((WT_CS_IND_YTD$GPACT-WT_CS_IND_YTD$GPPREV)/WT_CS_IND_YTD$GPPREV)),"",scales::percent(((WT_CS_IND_YTD$GPACT-WT_CS_IND_YTD$GPPREV)/WT_CS_IND_YTD$GPPREV),accuracy = 0.1)))
#WT_CS_IND_YTD$GPDiffYYPER       <- round(((WT_CS_IND_YTD$GPACT-WT_CS_IND_YTD$GPPREV)/WT_CS_IND_YTD$GPPREV)*100,2)
WT_CS_IND_YTD[is.na(WT_CS_IND_YTD)]      <- NA
WT_CS_IND_YTD$TurnDiffYY        <- WT_CS_IND_YTD$TURNACT-WT_CS_IND_YTD$TURNPREV
WT_CS_IND_YTD[is.na(WT_CS_IND_YTD)]      <- NA
WT_CS_IND_YTD$TurnDiffYYPER     <- ifelse(is.infinite(((WT_CS_IND_YTD$TURNACT-WT_CS_IND_YTD$TURNPREV)/WT_CS_IND_YTD$TURNPREV)),"",ifelse(is.na(((WT_CS_IND_YTD$TURNACT-WT_CS_IND_YTD$TURNPREV)/WT_CS_IND_YTD$TURNPREV)),"",scales::percent(((WT_CS_IND_YTD$TURNACT-WT_CS_IND_YTD$TURNPREV)/WT_CS_IND_YTD$TURNPREV),accuracy = 0.1)))
WT_CS_IND_YTD[is.na(WT_CS_IND_YTD)]      <- NA
WT_CS_IND_YTD$GPMTACT           <- ifelse(is.infinite(WT_CS_IND_YTD$GPACT*1000/WT_CS_IND_YTD$QActYear),0,WT_CS_IND_YTD$GPACT*1000/WT_CS_IND_YTD$QActYear)   # x 1000 ?
WT_CS_IND_YTD[is.na(WT_CS_IND_YTD)]      <- 0
WT_CS_IND_YTD$GPMTPREV          <- ifelse(is.infinite(WT_CS_IND_YTD$GPPREV*1000/WT_CS_IND_YTD$QprevYear),0,WT_CS_IND_YTD$GPPREV*1000/WT_CS_IND_YTD$QprevYear)
WT_CS_IND_YTD[is.na(WT_CS_IND_YTD)]      <- 0
WT_CS_IND_YTD$GPMTDIFF          <- WT_CS_IND_YTD$GPMTACT-WT_CS_IND_YTD$GPMTPREV
WT_CS_IND_YTD[is.na(WT_CS_IND_YTD)]      <- NA
WT_CS_IND_YTD$GPMTDIFFPER       <- ifelse(is.infinite((WT_CS_IND_YTD$GPMTACT-WT_CS_IND_YTD$GPMTPREV)/WT_CS_IND_YTD$GPMTPREV),"",ifelse(is.na(((WT_CS_IND_YTD$GPMTACT-WT_CS_IND_YTD$GPMTPREV)/WT_CS_IND_YTD$GPMTPREV)),"",scales::percent(((WT_CS_IND_YTD$GPMTACT-WT_CS_IND_YTD$GPMTPREV)/WT_CS_IND_YTD$GPMTPREV),accuracy = 0.1)))
WT_CS_IND_YTD[is.na(WT_CS_IND_YTD)]      <- NA

DT                     <- data.table(WT_CS_IND_YTD)
WT_CS_IND_YTD                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
WT_CS_IND_YTD[is.na(WT_CS_IND_YTD)]      <- NA
WT_CS_IND_YTD$`European Industry`     <- stri_replace_all_fixed(WT_CS_IND_YTD$`European Industry`, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
WT_CS_IND_YTD                      <- setorder(WT_CS_IND_YTD,-QuantityDiffYYPER_P)
WT_CS_IND_YTD$QuantityDiffYYPER_P <-NULL


Total<-data.frame(`European Industry`="Total CEE", Clasification="Total CEE",
                  QActYear          = sum(WT_CS_IND_YTD$QActYear),
                  QprevYear         = sum(WT_CS_IND_YTD$QprevYear),
                  GPACT             = sum(WT_CS_IND_YTD$GPACT),
                  GPPREV            = sum(WT_CS_IND_YTD$GPPREV),
                  TURNACT           = sum(WT_CS_IND_YTD$TURNACT),
                  TURNPREV          = sum(WT_CS_IND_YTD$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(WT_CS_IND_YTD$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(WT_CS_IND_YTD$GPMTACT),
                  GPMTPREV          = sum(WT_CS_IND_YTD$GPMTPREV),
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

colnames(Total)[1]     <- "European Industry"
Total$Clasification                <- NULL
WT_CS_IND_YTD                      <- rbind(WT_CS_IND_YTD,Total)


WT_CS_IND_YTD                          <- WT_CS_IND_YTD %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
WT_CS_IND_YTD[is.na(WT_CS_IND_YTD)]    <- ""



# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

WT_CS_IND_YTD          <- WT_CS_IND_YTD %>% mutate_if(is.numeric, ~round(., 0)) 

## ----------------------------  CEE Region - Total results   (month)

WyboldowaneWierszeWT_CS_IND_YTD    <- length(WT_CS_IND_YTD$`European Industry`)




#### -------Water Treatment industry by country-----------


WT_INDUSTRY<-WTMain %>% subset(WaterClass=="WATER TREATMENT - Industry")



colnames(WT_INDUSTRY)[11]<-Naglowek1
colnames(WT_INDUSTRY)[12]<-Naglowek2
colnames(WT_INDUSTRY)[13]<-Naglowek3
colnames(WT_INDUSTRY)[14]<-Naglowek4
colnames(WT_INDUSTRY)[15]<-Naglowek5
colnames(WT_INDUSTRY)[16]<-Naglowek6



WT_INDUSTRY                   <- WT_INDUSTRY %>% filter(`Calendar month`==MiesiacAnalizy)
WT_INDUSTRY                   <- WT_INDUSTRY %>% select(17,18,11,12,13,14,15,16)

# WT_INDUSTRY<-rbind(WT_INDUSTRY,WT_INDUSTRY2)
WT_INDUSTRY                   <- as.data.frame(WT_INDUSTRY)
WT_INDUSTRY                   <- WT_INDUSTRY %>% group_by(Region,SubRegion) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
WT_INDUSTRY[is.na(WT_INDUSTRY)]      <- NA
WT_INDUSTRY$QuantityDiffYY    <- WT_INDUSTRY$QActYear-WT_INDUSTRY$QprevYear
WT_INDUSTRY[is.na(WT_INDUSTRY)]      <- NA
WT_INDUSTRY$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((WT_INDUSTRY$QActYear-WT_INDUSTRY$QprevYear)/WT_INDUSTRY$QprevYear)),"",(WT_INDUSTRY$QActYear-WT_INDUSTRY$QprevYear)/WT_INDUSTRY$QprevYear))
WT_INDUSTRY$QuantityDiffYYPER <- ifelse(is.infinite(((WT_INDUSTRY$QActYear-WT_INDUSTRY$QprevYear)/WT_INDUSTRY$QprevYear)),"",scales::percent(((WT_INDUSTRY$QActYear-WT_INDUSTRY$QprevYear)/WT_INDUSTRY$QprevYear),accuracy = 0.1))
WT_INDUSTRY[is.na(WT_INDUSTRY)]      <- NA
WT_INDUSTRY$GPDiffYY          <- WT_INDUSTRY$GPACT-WT_INDUSTRY$GPPREV
WT_INDUSTRY[is.na(WT_INDUSTRY)]      <- NA
WT_INDUSTRY$GPDiffYYPER       <- ifelse(is.infinite(((WT_INDUSTRY$GPACT-WT_INDUSTRY$GPPREV)/WT_INDUSTRY$GPPREV)),"",scales::percent(((WT_INDUSTRY$GPACT-WT_INDUSTRY$GPPREV)/WT_INDUSTRY$GPPREV),accuracy = 0.1))
#WT_INDUSTRY$GPDiffYYPER       <- round(((WT_INDUSTRY$GPACT-WT_INDUSTRY$GPPREV)/WT_INDUSTRY$GPPREV)*100,2)
WT_INDUSTRY[is.na(WT_INDUSTRY)]      <- NA
WT_INDUSTRY$TurnDiffYY        <- WT_INDUSTRY$TURNACT-WT_INDUSTRY$TURNPREV
WT_INDUSTRY[is.na(WT_INDUSTRY)]      <- NA
WT_INDUSTRY$TurnDiffYYPER     <- ifelse(is.infinite(((WT_INDUSTRY$TURNACT-WT_INDUSTRY$TURNPREV)/WT_INDUSTRY$TURNPREV)),"",scales::percent(((WT_INDUSTRY$TURNACT-WT_INDUSTRY$TURNPREV)/WT_INDUSTRY$TURNPREV),accuracy = 0.1))
WT_INDUSTRY[is.na(WT_INDUSTRY)]      <- NA
WT_INDUSTRY$GPMTACT           <- ifelse(is.infinite(WT_INDUSTRY$GPACT*1000/WT_INDUSTRY$QActYear),0,WT_INDUSTRY$GPACT*1000/WT_INDUSTRY$QActYear)   # x 1000 ?
WT_INDUSTRY[is.na(WT_INDUSTRY)]      <- 0
WT_INDUSTRY$GPMTPREV          <- ifelse(is.infinite(WT_INDUSTRY$GPPREV*1000/WT_INDUSTRY$QprevYear),0,WT_INDUSTRY$GPPREV*1000/WT_INDUSTRY$QprevYear)
WT_INDUSTRY[is.na(WT_INDUSTRY)]      <- 0
WT_INDUSTRY$GPMTDIFF          <- WT_INDUSTRY$GPMTACT-WT_INDUSTRY$GPMTPREV
WT_INDUSTRY[is.na(WT_INDUSTRY)]      <- NA
WT_INDUSTRY$GPMTDIFFPER       <- ifelse(is.infinite((WT_INDUSTRY$GPMTACT-WT_INDUSTRY$GPMTPREV)/WT_INDUSTRY$GPMTPREV),"",scales::percent(((WT_INDUSTRY$GPMTACT-WT_INDUSTRY$GPMTPREV)/WT_INDUSTRY$GPMTPREV),accuracy = 0.1))
WT_INDUSTRY[is.na(WT_INDUSTRY)]      <- NA

DT                     <- data.table(WT_INDUSTRY)
WT_INDUSTRY                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
WT_INDUSTRY[is.na(WT_INDUSTRY)]      <- NA
WT_INDUSTRY$SubRegion         <- stri_replace_all_fixed(WT_INDUSTRY$SubRegion, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
WT_INDUSTRY                      <- setorder(WT_INDUSTRY,Region,-QuantityDiffYYPER_P)
WT_INDUSTRY$QuantityDiffYYPER_P <-NULL



Total<-data.frame(Region="Total CEE", SubRegion="Total CEE",
                  QActYear          = sum(WT_INDUSTRY$QActYear),
                  QprevYear         = sum(WT_INDUSTRY$QprevYear),
                  GPACT             = sum(WT_INDUSTRY$GPACT),
                  GPPREV            = sum(WT_INDUSTRY$GPPREV),
                  TURNACT           = sum(WT_INDUSTRY$TURNACT),
                  TURNPREV          = sum(WT_INDUSTRY$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(WT_INDUSTRY$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(WT_INDUSTRY$GPMTACT),
                  GPMTPREV          = sum(WT_INDUSTRY$GPMTPREV),
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


Regiony<-unique(WT_INDUSTRY$Region)

for(region in unique(WT_INDUSTRY$Region)){
  
  steal <- subset(WT_INDUSTRY,Region==region)
  frejm <- data.frame(Region=region, SubRegion=region, 
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
  
  
  WT_INDUSTRY <- rbind(WT_INDUSTRY, frejm)
  
}

WT_INDUSTRY                      <- rbind(WT_INDUSTRY,Total)
WT_INDUSTRY                      <- setorder(WT_INDUSTRY,Region)
WyboldowaneWierszeWT_INDUSTRY <- c(which(WT_INDUSTRY$SubRegion %in% Regiony),length(WT_INDUSTRY$Region))
WT_INDUSTRY$Region               <- NULL
WT_INDUSTRY                      <- WT_INDUSTRY %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
WT_INDUSTRY[is.na(WT_INDUSTRY)]  <- ""



# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

WT_INDUSTRY          <- WT_INDUSTRY %>% mutate_if(is.numeric, ~round(., 0)) 

## ----------------------------  CEE Region - Total results   (month)

WyboldowaneWierszeWT_INDUSTRY    <- c(which(WT_INDUSTRY$SubRegion %in% Regiony),length(WT_INDUSTRY$SubRegion))



#### -------Water Treatment industry by country YTD-----------


WT_INDUSTRY_YTD<-WTMain %>% subset(WaterClass=="WATER TREATMENT - Industry")



colnames(WT_INDUSTRY_YTD)[11]<-Naglowek1
colnames(WT_INDUSTRY_YTD)[12]<-Naglowek2
colnames(WT_INDUSTRY_YTD)[13]<-Naglowek3
colnames(WT_INDUSTRY_YTD)[14]<-Naglowek4
colnames(WT_INDUSTRY_YTD)[15]<-Naglowek5
colnames(WT_INDUSTRY_YTD)[16]<-Naglowek6



WT_INDUSTRY_YTD                   <- WT_INDUSTRY_YTD %>% filter(`Calendar month` %in% 1: MiesiacAnalizy)
WT_INDUSTRY_YTD                   <- WT_INDUSTRY_YTD %>% select(17,18,11,12,13,14,15,16)

# WT_INDUSTRY_YTD<-rbind(WT_INDUSTRY_YTD,WT_INDUSTRY_YTD2)
WT_INDUSTRY_YTD                   <- as.data.frame(WT_INDUSTRY_YTD)
WT_INDUSTRY_YTD                   <- WT_INDUSTRY_YTD %>% group_by(Region,SubRegion) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
WT_INDUSTRY_YTD[is.na(WT_INDUSTRY_YTD)]      <- NA
WT_INDUSTRY_YTD$QuantityDiffYY    <- WT_INDUSTRY_YTD$QActYear-WT_INDUSTRY_YTD$QprevYear
WT_INDUSTRY_YTD[is.na(WT_INDUSTRY_YTD)]      <- NA
WT_INDUSTRY_YTD$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((WT_INDUSTRY_YTD$QActYear-WT_INDUSTRY_YTD$QprevYear)/WT_INDUSTRY_YTD$QprevYear)),"",(WT_INDUSTRY_YTD$QActYear-WT_INDUSTRY_YTD$QprevYear)/WT_INDUSTRY_YTD$QprevYear))
WT_INDUSTRY_YTD$QuantityDiffYYPER <- ifelse(is.infinite(((WT_INDUSTRY_YTD$QActYear-WT_INDUSTRY_YTD$QprevYear)/WT_INDUSTRY_YTD$QprevYear)),"",scales::percent(((WT_INDUSTRY_YTD$QActYear-WT_INDUSTRY_YTD$QprevYear)/WT_INDUSTRY_YTD$QprevYear),accuracy = 0.1))
WT_INDUSTRY_YTD[is.na(WT_INDUSTRY_YTD)]      <- NA
WT_INDUSTRY_YTD$GPDiffYY          <- WT_INDUSTRY_YTD$GPACT-WT_INDUSTRY_YTD$GPPREV
WT_INDUSTRY_YTD[is.na(WT_INDUSTRY_YTD)]      <- NA
WT_INDUSTRY_YTD$GPDiffYYPER       <- ifelse(is.infinite(((WT_INDUSTRY_YTD$GPACT-WT_INDUSTRY_YTD$GPPREV)/WT_INDUSTRY_YTD$GPPREV)),"",scales::percent(((WT_INDUSTRY_YTD$GPACT-WT_INDUSTRY_YTD$GPPREV)/WT_INDUSTRY_YTD$GPPREV),accuracy = 0.1))
#WT_INDUSTRY_YTD$GPDiffYYPER       <- round(((WT_INDUSTRY_YTD$GPACT-WT_INDUSTRY_YTD$GPPREV)/WT_INDUSTRY_YTD$GPPREV)*100,2)
WT_INDUSTRY_YTD[is.na(WT_INDUSTRY_YTD)]      <- NA
WT_INDUSTRY_YTD$TurnDiffYY        <- WT_INDUSTRY_YTD$TURNACT-WT_INDUSTRY_YTD$TURNPREV
WT_INDUSTRY_YTD[is.na(WT_INDUSTRY_YTD)]      <- NA
WT_INDUSTRY_YTD$TurnDiffYYPER     <- ifelse(is.infinite(((WT_INDUSTRY_YTD$TURNACT-WT_INDUSTRY_YTD$TURNPREV)/WT_INDUSTRY_YTD$TURNPREV)),"",scales::percent(((WT_INDUSTRY_YTD$TURNACT-WT_INDUSTRY_YTD$TURNPREV)/WT_INDUSTRY_YTD$TURNPREV),accuracy = 0.1))
WT_INDUSTRY_YTD[is.na(WT_INDUSTRY_YTD)]      <- NA
WT_INDUSTRY_YTD$GPMTACT           <- ifelse(is.infinite(WT_INDUSTRY_YTD$GPACT*1000/WT_INDUSTRY_YTD$QActYear),0,WT_INDUSTRY_YTD$GPACT*1000/WT_INDUSTRY_YTD$QActYear)   # x 1000 ?
WT_INDUSTRY_YTD[is.na(WT_INDUSTRY_YTD)]      <- 0
WT_INDUSTRY_YTD$GPMTPREV          <- ifelse(is.infinite(WT_INDUSTRY_YTD$GPPREV*1000/WT_INDUSTRY_YTD$QprevYear),0,WT_INDUSTRY_YTD$GPPREV*1000/WT_INDUSTRY_YTD$QprevYear)
WT_INDUSTRY_YTD[is.na(WT_INDUSTRY_YTD)]      <- 0
WT_INDUSTRY_YTD$GPMTDIFF          <- WT_INDUSTRY_YTD$GPMTACT-WT_INDUSTRY_YTD$GPMTPREV
WT_INDUSTRY_YTD[is.na(WT_INDUSTRY_YTD)]      <- NA
WT_INDUSTRY_YTD$GPMTDIFFPER       <- ifelse(is.infinite((WT_INDUSTRY_YTD$GPMTACT-WT_INDUSTRY_YTD$GPMTPREV)/WT_INDUSTRY_YTD$GPMTPREV),"",scales::percent(((WT_INDUSTRY_YTD$GPMTACT-WT_INDUSTRY_YTD$GPMTPREV)/WT_INDUSTRY_YTD$GPMTPREV),accuracy = 0.1))
WT_INDUSTRY_YTD[is.na(WT_INDUSTRY_YTD)]      <- NA

DT                     <- data.table(WT_INDUSTRY_YTD)
WT_INDUSTRY_YTD                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
WT_INDUSTRY_YTD[is.na(WT_INDUSTRY_YTD)]      <- NA
WT_INDUSTRY_YTD$SubRegion         <- stri_replace_all_fixed(WT_INDUSTRY_YTD$SubRegion, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
WT_INDUSTRY_YTD                      <- setorder(WT_INDUSTRY_YTD,Region,-QuantityDiffYYPER_P)
WT_INDUSTRY_YTD$QuantityDiffYYPER_P <-NULL



Total<-data.frame(Region="Total CEE", SubRegion="Total CEE",
                  QActYear          = sum(WT_INDUSTRY_YTD$QActYear),
                  QprevYear         = sum(WT_INDUSTRY_YTD$QprevYear),
                  GPACT             = sum(WT_INDUSTRY_YTD$GPACT),
                  GPPREV            = sum(WT_INDUSTRY_YTD$GPPREV),
                  TURNACT           = sum(WT_INDUSTRY_YTD$TURNACT),
                  TURNPREV          = sum(WT_INDUSTRY_YTD$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(WT_INDUSTRY_YTD$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(WT_INDUSTRY_YTD$GPMTACT),
                  GPMTPREV          = sum(WT_INDUSTRY_YTD$GPMTPREV),
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


Regiony<-unique(WT_INDUSTRY_YTD$Region)

for(region in unique(WT_INDUSTRY_YTD$Region)){
  
  steal <- subset(WT_INDUSTRY_YTD,Region==region)
  frejm <- data.frame(Region=region, SubRegion=region, 
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
  
  
  WT_INDUSTRY_YTD <- rbind(WT_INDUSTRY_YTD, frejm)
  
}

WT_INDUSTRY_YTD                      <- rbind(WT_INDUSTRY_YTD,Total)
WT_INDUSTRY_YTD                      <- setorder(WT_INDUSTRY_YTD,Region)
WyboldowaneWierszeWT_INDUSTRY_YTD <- c(which(WT_INDUSTRY_YTD$SubRegion %in% Regiony),length(WT_INDUSTRY_YTD$Region))
WT_INDUSTRY_YTD$Region               <- NULL
WT_INDUSTRY_YTD                      <- WT_INDUSTRY_YTD %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
WT_INDUSTRY_YTD[is.na(WT_INDUSTRY_YTD)]  <- ""



# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

WT_INDUSTRY_YTD          <- WT_INDUSTRY_YTD %>% mutate_if(is.numeric, ~round(., 0)) 

## ----------------------------  CEE Region - Total results   (month)

WyboldowaneWierszeWT_INDUSTRY_YTD    <- c(which(WT_INDUSTRY_YTD$SubRegion %in% Regiony),length(WT_INDUSTRY_YTD$SubRegion))







