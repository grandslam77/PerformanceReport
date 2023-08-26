SALES_MAIN <- MainSource



SALES_MAIN$Clasification  <- " "
SALES_MAIN$Clasification  <- ifelse(SALES_MAIN$Intern=="External",SALES_MAIN$Clasification<-"3rd party",ifelse(SALES_MAIN$`SoldTo Country` %in% MapowanieIntercompany$SoldToCountry,SALES_MAIN$Clasification<-"Intercompany (inside CEE)",SALES_MAIN$Clasification<-"Intercompany (outside CEE)"))


colnames(SALES_MAIN)[12]<-Naglowek1
colnames(SALES_MAIN)[13]<-Naglowek2
colnames(SALES_MAIN)[14]<-Naglowek3
colnames(SALES_MAIN)[15]<-Naglowek4
colnames(SALES_MAIN)[16]<-Naglowek5
colnames(SALES_MAIN)[17]<-Naglowek6

SALES_MAIN                   <- SALES_MAIN %>% filter(`Calendar month`==MiesiacAnalizy)
SALES_MAIN                   <- SALES_MAIN %>% select(19,22,12,13,14,15,16,17)



# SALES_MAIN<-rbind(SALES_MAIN,SALES_MAIN2)
SALES_MAIN                   <- as.data.frame(SALES_MAIN)
SALES_MAIN                   <- SALES_MAIN %>% group_by(SubRegion,Clasification) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
SALES_MAIN[is.na(SALES_MAIN)]      <- NA
SALES_MAIN$QuantityDiffYY    <- SALES_MAIN$QActYear-SALES_MAIN$QprevYear
SALES_MAIN[is.na(SALES_MAIN)]      <- NA
SALES_MAIN$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((SALES_MAIN$QActYear-SALES_MAIN$QprevYear)/SALES_MAIN$QprevYear)),"",(SALES_MAIN$QActYear-SALES_MAIN$QprevYear)/SALES_MAIN$QprevYear))

SALES_MAIN$QuantityDiffYYPER <- ifelse(is.infinite(((SALES_MAIN$QActYear-SALES_MAIN$QprevYear)/SALES_MAIN$QprevYear)),"",ifelse(is.na(((SALES_MAIN$QActYear-SALES_MAIN$QprevYear)/SALES_MAIN$QprevYear)),"",  scales::percent(((SALES_MAIN$QActYear-SALES_MAIN$QprevYear)/SALES_MAIN$QprevYear),accuracy = 0.1)))
SALES_MAIN[is.na(SALES_MAIN)]      <- NA
SALES_MAIN$GPDiffYY          <- SALES_MAIN$GPACT-SALES_MAIN$GPPREV
SALES_MAIN[is.na(SALES_MAIN)]      <- NA
SALES_MAIN$GPDiffYYPER       <- ifelse(is.infinite(((SALES_MAIN$GPACT-SALES_MAIN$GPPREV)/SALES_MAIN$GPPREV)),"",ifelse(is.na(((SALES_MAIN$GPACT-SALES_MAIN$GPPREV)/SALES_MAIN$GPPREV)),"",scales::percent(((SALES_MAIN$GPACT-SALES_MAIN$GPPREV)/SALES_MAIN$GPPREV),accuracy = 0.1)))
#SALES_MAIN$GPDiffYYPER       <- round(((SALES_MAIN$GPACT-SALES_MAIN$GPPREV)/SALES_MAIN$GPPREV)*100,2)
SALES_MAIN[is.na(SALES_MAIN)]      <- NA
SALES_MAIN$TurnDiffYY        <- SALES_MAIN$TURNACT-SALES_MAIN$TURNPREV
SALES_MAIN[is.na(SALES_MAIN)]      <- NA
SALES_MAIN$TurnDiffYYPER     <- ifelse(is.infinite(((SALES_MAIN$TURNACT-SALES_MAIN$TURNPREV)/SALES_MAIN$TURNPREV)),"",ifelse(is.na(((SALES_MAIN$TURNACT-SALES_MAIN$TURNPREV)/SALES_MAIN$TURNPREV)),"",scales::percent(((SALES_MAIN$TURNACT-SALES_MAIN$TURNPREV)/SALES_MAIN$TURNPREV),accuracy = 0.1)))
SALES_MAIN[is.na(SALES_MAIN)]      <- NA
SALES_MAIN$GPMTACT           <- ifelse(is.infinite(SALES_MAIN$GPACT*1000/SALES_MAIN$QActYear),0,SALES_MAIN$GPACT*1000/SALES_MAIN$QActYear)   # x 1000 ?
SALES_MAIN[is.na(SALES_MAIN)]      <- 0
SALES_MAIN$GPMTPREV          <- ifelse(is.infinite(SALES_MAIN$GPPREV*1000/SALES_MAIN$QprevYear),0,SALES_MAIN$GPPREV*1000/SALES_MAIN$QprevYear)
SALES_MAIN[is.na(SALES_MAIN)]      <- 0
SALES_MAIN$GPMTDIFF          <- SALES_MAIN$GPMTACT-SALES_MAIN$GPMTPREV
SALES_MAIN[is.na(SALES_MAIN)]      <- NA
SALES_MAIN$GPMTDIFFPER       <- ifelse(is.infinite((SALES_MAIN$GPMTACT-SALES_MAIN$GPMTPREV)/SALES_MAIN$GPMTPREV),"",ifelse(is.na(((SALES_MAIN$GPMTACT-SALES_MAIN$GPMTPREV)/SALES_MAIN$GPMTPREV)),"",scales::percent(((SALES_MAIN$GPMTACT-SALES_MAIN$GPMTPREV)/SALES_MAIN$GPMTPREV),accuracy = 0.1)))
SALES_MAIN[is.na(SALES_MAIN)]      <- NA

DT                     <- data.table(SALES_MAIN)
SALES_MAIN                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
SALES_MAIN[is.na(SALES_MAIN)]      <- NA
SALES_MAIN$Clasification     <- stri_replace_all_fixed(SALES_MAIN$Clasification, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
SALES_MAIN$SubRegion     <- stri_replace_all_fixed(SALES_MAIN$SubRegion, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
SALES_MAIN                      <- setorder(SALES_MAIN,SubRegion,-QuantityDiffYYPER_P)
SALES_MAIN$QuantityDiffYYPER_P <-NULL


Total<-data.frame(SubRegion="Total CEE", Clasification="Total CEE",
                  QActYear          = sum(SALES_MAIN$QActYear),
                  QprevYear         = sum(SALES_MAIN$QprevYear),
                  GPACT             = sum(SALES_MAIN$GPACT),
                  GPPREV            = sum(SALES_MAIN$GPPREV),
                  TURNACT           = sum(SALES_MAIN$TURNACT),
                  TURNPREV          = sum(SALES_MAIN$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(SALES_MAIN$TurnDiffYY),
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


Regiony<-unique(SALES_MAIN$SubRegion)

for(region in unique(SALES_MAIN$SubRegion)){
  
  steal <- subset(SALES_MAIN,SubRegion==region)
  frejm <- data.frame(SubRegion=region, Clasification=region, 
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
  
  
  SALES_MAIN <- rbind(SALES_MAIN, frejm)
  
}


SALES_MAIN                      <- setorder(SALES_MAIN,SubRegion)
SALES_MAIN                      <- rbind(SALES_MAIN,Total)
SALES_MAIN$SubRegion               <- NULL
SALES_MAIN                      <- SALES_MAIN %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)




# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

SALES_MAIN          <- SALES_MAIN %>% mutate_if(is.numeric, ~round(., 0)) 
SALES_MAIN[is.na(SALES_MAIN)]    <- ""


WyboldowaneWierszeSALES_MAIN    <- c(which(SALES_MAIN$Clasification %in% Regiony),length(SALES_MAIN$Clasification))


# YTD 3 PARTY---------------------------------------------------


SALES_MAIN_YTD <- MainSource


SALES_MAIN_YTD$Clasification <- ""
SALES_MAIN_YTD$Clasification  <- ifelse(SALES_MAIN_YTD$Intern=="External",SALES_MAIN_YTD$Clasification<-"3rd party",ifelse(SALES_MAIN_YTD$`SoldTo Country` %in% MapowanieIntercompany$SoldToCountry,SALES_MAIN_YTD$Clasification<-"Intercompany (inside CEE)",SALES_MAIN_YTD$Clasification<-"Intercompany (outside CEE)"))


colnames(SALES_MAIN_YTD)[12]<-Naglowek1
colnames(SALES_MAIN_YTD)[13]<-Naglowek2
colnames(SALES_MAIN_YTD)[14]<-Naglowek3
colnames(SALES_MAIN_YTD)[15]<-Naglowek4
colnames(SALES_MAIN_YTD)[16]<-Naglowek5
colnames(SALES_MAIN_YTD)[17]<-Naglowek6

SALES_MAIN_YTD                   <- SALES_MAIN_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
SALES_MAIN_YTD                   <- SALES_MAIN_YTD %>% select(19,22,12,13,14,15,16,17)



# SALES_MAIN_YTD<-rbind(SALES_MAIN_YTD,SALES_MAIN_YTD2)
SALES_MAIN_YTD                   <- as.data.frame(SALES_MAIN_YTD)
SALES_MAIN_YTD                   <- SALES_MAIN_YTD %>% group_by(SubRegion,Clasification) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
SALES_MAIN_YTD[is.na(SALES_MAIN_YTD)]      <- NA
SALES_MAIN_YTD$QuantityDiffYY    <- SALES_MAIN_YTD$QActYear-SALES_MAIN_YTD$QprevYear
SALES_MAIN_YTD[is.na(SALES_MAIN_YTD)]      <- NA
SALES_MAIN_YTD$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((SALES_MAIN_YTD$QActYear-SALES_MAIN_YTD$QprevYear)/SALES_MAIN_YTD$QprevYear)),"",(SALES_MAIN_YTD$QActYear-SALES_MAIN_YTD$QprevYear)/SALES_MAIN_YTD$QprevYear))

SALES_MAIN_YTD$QuantityDiffYYPER <- ifelse(is.infinite(((SALES_MAIN_YTD$QActYear-SALES_MAIN_YTD$QprevYear)/SALES_MAIN_YTD$QprevYear)),"",ifelse(is.na(((SALES_MAIN_YTD$QActYear-SALES_MAIN_YTD$QprevYear)/SALES_MAIN_YTD$QprevYear)),"",  scales::percent(((SALES_MAIN_YTD$QActYear-SALES_MAIN_YTD$QprevYear)/SALES_MAIN_YTD$QprevYear),accuracy = 0.1)))
SALES_MAIN_YTD[is.na(SALES_MAIN_YTD)]      <- NA
SALES_MAIN_YTD$GPDiffYY          <- SALES_MAIN_YTD$GPACT-SALES_MAIN_YTD$GPPREV
SALES_MAIN_YTD[is.na(SALES_MAIN_YTD)]      <- NA
SALES_MAIN_YTD$GPDiffYYPER       <- ifelse(is.infinite(((SALES_MAIN_YTD$GPACT-SALES_MAIN_YTD$GPPREV)/SALES_MAIN_YTD$GPPREV)),"",ifelse(is.na(((SALES_MAIN_YTD$GPACT-SALES_MAIN_YTD$GPPREV)/SALES_MAIN_YTD$GPPREV)),"",scales::percent(((SALES_MAIN_YTD$GPACT-SALES_MAIN_YTD$GPPREV)/SALES_MAIN_YTD$GPPREV),accuracy = 0.1)))
#SALES_MAIN_YTD$GPDiffYYPER       <- round(((SALES_MAIN_YTD$GPACT-SALES_MAIN_YTD$GPPREV)/SALES_MAIN_YTD$GPPREV)*100,2)
SALES_MAIN_YTD[is.na(SALES_MAIN_YTD)]      <- NA
SALES_MAIN_YTD$TurnDiffYY        <- SALES_MAIN_YTD$TURNACT-SALES_MAIN_YTD$TURNPREV
SALES_MAIN_YTD[is.na(SALES_MAIN_YTD)]      <- NA
SALES_MAIN_YTD$TurnDiffYYPER     <- ifelse(is.infinite(((SALES_MAIN_YTD$TURNACT-SALES_MAIN_YTD$TURNPREV)/SALES_MAIN_YTD$TURNPREV)),"",ifelse(is.na(((SALES_MAIN_YTD$TURNACT-SALES_MAIN_YTD$TURNPREV)/SALES_MAIN_YTD$TURNPREV)),"",scales::percent(((SALES_MAIN_YTD$TURNACT-SALES_MAIN_YTD$TURNPREV)/SALES_MAIN_YTD$TURNPREV),accuracy = 0.1)))
SALES_MAIN_YTD[is.na(SALES_MAIN_YTD)]      <- NA
SALES_MAIN_YTD$GPMTACT           <- ifelse(is.infinite(SALES_MAIN_YTD$GPACT*1000/SALES_MAIN_YTD$QActYear),0,SALES_MAIN_YTD$GPACT*1000/SALES_MAIN_YTD$QActYear)   # x 1000 ?
SALES_MAIN_YTD[is.na(SALES_MAIN_YTD)]      <- 0
SALES_MAIN_YTD$GPMTPREV          <- ifelse(is.infinite(SALES_MAIN_YTD$GPPREV*1000/SALES_MAIN_YTD$QprevYear),0,SALES_MAIN_YTD$GPPREV*1000/SALES_MAIN_YTD$QprevYear)
SALES_MAIN_YTD[is.na(SALES_MAIN_YTD)]      <- 0
SALES_MAIN_YTD$GPMTDIFF          <- SALES_MAIN_YTD$GPMTACT-SALES_MAIN_YTD$GPMTPREV
SALES_MAIN_YTD[is.na(SALES_MAIN_YTD)]      <- NA
SALES_MAIN_YTD$GPMTDIFFPER       <- ifelse(is.infinite((SALES_MAIN_YTD$GPMTACT-SALES_MAIN_YTD$GPMTPREV)/SALES_MAIN_YTD$GPMTPREV),"",ifelse(is.na(((SALES_MAIN_YTD$GPMTACT-SALES_MAIN_YTD$GPMTPREV)/SALES_MAIN_YTD$GPMTPREV)),"",scales::percent(((SALES_MAIN_YTD$GPMTACT-SALES_MAIN_YTD$GPMTPREV)/SALES_MAIN_YTD$GPMTPREV),accuracy = 0.1)))
SALES_MAIN_YTD[is.na(SALES_MAIN_YTD)]      <- NA

DT                     <- data.table(SALES_MAIN_YTD)
SALES_MAIN_YTD                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
SALES_MAIN_YTD[is.na(SALES_MAIN_YTD)]      <- NA
SALES_MAIN_YTD$Clasification     <- stri_replace_all_fixed(SALES_MAIN_YTD$Clasification, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
SALES_MAIN_YTD$SubRegion     <- stri_replace_all_fixed(SALES_MAIN_YTD$SubRegion, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
SALES_MAIN_YTD                      <- setorder(SALES_MAIN_YTD,SubRegion,-QuantityDiffYYPER_P)
SALES_MAIN_YTD$QuantityDiffYYPER_P <-NULL


Total<-data.frame(SubRegion="Total CEE", Clasification="Total CEE",
                  QActYear          = sum(SALES_MAIN_YTD$QActYear),
                  QprevYear         = sum(SALES_MAIN_YTD$QprevYear),
                  GPACT             = sum(SALES_MAIN_YTD$GPACT),
                  GPPREV            = sum(SALES_MAIN_YTD$GPPREV),
                  TURNACT           = sum(SALES_MAIN_YTD$TURNACT),
                  TURNPREV          = sum(SALES_MAIN_YTD$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(SALES_MAIN_YTD$TurnDiffYY),
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


Regiony<-unique(SALES_MAIN_YTD$SubRegion)

for(region in unique(SALES_MAIN_YTD$SubRegion)){
  
  steal <- subset(SALES_MAIN_YTD,SubRegion==region)
  frejm <- data.frame(SubRegion=region, Clasification=region, 
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
  
  
  SALES_MAIN_YTD <- rbind(SALES_MAIN_YTD, frejm)
  
}


SALES_MAIN_YTD                      <- setorder(SALES_MAIN_YTD,SubRegion)
SALES_MAIN_YTD                      <- rbind(SALES_MAIN_YTD,Total)
SALES_MAIN_YTD$SubRegion            <- NULL
SALES_MAIN_YTD                      <- SALES_MAIN_YTD %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)




# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

SALES_MAIN_YTD          <- SALES_MAIN_YTD %>% mutate_if(is.numeric, ~round(., 0)) 
SALES_MAIN_YTD[is.na(SALES_MAIN_YTD)]    <- ""


WyboldowaneWierszeSALES_MAIN_YTD    <- c(which(SALES_MAIN_YTD$Clasification %in% Regiony),length(SALES_MAIN_YTD$Clasification))







