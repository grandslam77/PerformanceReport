DIVBYSALES<- MainSource
colnames(DIVBYSALES)[5]<-"Division"
DIVBYSALES <-left_join(DIVBYSALES,MapowanieICCPMOthers,by="Division")

DIVBYSALES$Clasification <- ""
DIVBYSALES$Clasification  <- ifelse(DIVBYSALES$Intern=="External",DIVBYSALES$Clasification<-"3rd party",ifelse(DIVBYSALES$`SoldTo Country` %in% MapowanieIntercompany$SoldToCountry,DIVBYSALES$Clasification<-"Intercompany (inside CEE)",DIVBYSALES$Clasification<-"Intercompany (outside CEE)"))


colnames(DIVBYSALES)[12]<-Naglowek1
colnames(DIVBYSALES)[13]<-Naglowek2
colnames(DIVBYSALES)[14]<-Naglowek3
colnames(DIVBYSALES)[15]<-Naglowek4
colnames(DIVBYSALES)[16]<-Naglowek5
colnames(DIVBYSALES)[17]<-Naglowek6

DIVBYSALES                   <- DIVBYSALES %>% filter(`Calendar month`==MiesiacAnalizy)
DIVBYSALES                   <- DIVBYSALES %>% select(22,5,23,12,13,14,15,16,17)



# DIVBYSALES<-rbind(DIVBYSALES,DIVBYSALES2)
DIVBYSALES                   <- as.data.frame(DIVBYSALES)
DIVBYSALES                   <- DIVBYSALES %>% group_by(Mapowanie,Division,Clasification) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
DIVBYSALES[is.na(DIVBYSALES)]      <- NA
DIVBYSALES$QuantityDiffYY    <- DIVBYSALES$QActYear-DIVBYSALES$QprevYear
DIVBYSALES[is.na(DIVBYSALES)]      <- NA
DIVBYSALES$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((DIVBYSALES$QActYear-DIVBYSALES$QprevYear)/DIVBYSALES$QprevYear)),"",(DIVBYSALES$QActYear-DIVBYSALES$QprevYear)/DIVBYSALES$QprevYear))

DIVBYSALES$QuantityDiffYYPER <- ifelse(is.infinite(((DIVBYSALES$QActYear-DIVBYSALES$QprevYear)/DIVBYSALES$QprevYear)),"",ifelse(is.na(((DIVBYSALES$QActYear-DIVBYSALES$QprevYear)/DIVBYSALES$QprevYear)),"",  scales::percent(((DIVBYSALES$QActYear-DIVBYSALES$QprevYear)/DIVBYSALES$QprevYear),accuracy = 0.1)))
DIVBYSALES[is.na(DIVBYSALES)]      <- NA
DIVBYSALES$GPDiffYY          <- DIVBYSALES$GPACT-DIVBYSALES$GPPREV
DIVBYSALES[is.na(DIVBYSALES)]      <- NA
DIVBYSALES$GPDiffYYPER       <- ifelse(is.infinite(((DIVBYSALES$GPACT-DIVBYSALES$GPPREV)/DIVBYSALES$GPPREV)),"",ifelse(is.na(((DIVBYSALES$GPACT-DIVBYSALES$GPPREV)/DIVBYSALES$GPPREV)),"",scales::percent(((DIVBYSALES$GPACT-DIVBYSALES$GPPREV)/DIVBYSALES$GPPREV),accuracy = 0.1)))
#DIVBYSALES$GPDiffYYPER       <- round(((DIVBYSALES$GPACT-DIVBYSALES$GPPREV)/DIVBYSALES$GPPREV)*100,2)
DIVBYSALES[is.na(DIVBYSALES)]      <- NA
DIVBYSALES$TurnDiffYY        <- DIVBYSALES$TURNACT-DIVBYSALES$TURNPREV
DIVBYSALES[is.na(DIVBYSALES)]      <- NA
DIVBYSALES$TurnDiffYYPER     <- ifelse(is.infinite(((DIVBYSALES$TURNACT-DIVBYSALES$TURNPREV)/DIVBYSALES$TURNPREV)),"",ifelse(is.na(((DIVBYSALES$TURNACT-DIVBYSALES$TURNPREV)/DIVBYSALES$TURNPREV)),"",scales::percent(((DIVBYSALES$TURNACT-DIVBYSALES$TURNPREV)/DIVBYSALES$TURNPREV),accuracy = 0.1)))
DIVBYSALES[is.na(DIVBYSALES)]      <- NA
DIVBYSALES$GPMTACT           <- ifelse(is.infinite(DIVBYSALES$GPACT*1000/DIVBYSALES$QActYear),0,DIVBYSALES$GPACT*1000/DIVBYSALES$QActYear)   # x 1000 ?
DIVBYSALES[is.na(DIVBYSALES)]      <- 0
DIVBYSALES$GPMTPREV          <- ifelse(is.infinite(DIVBYSALES$GPPREV*1000/DIVBYSALES$QprevYear),0,DIVBYSALES$GPPREV*1000/DIVBYSALES$QprevYear)
DIVBYSALES[is.na(DIVBYSALES)]      <- 0
DIVBYSALES$GPMTDIFF          <- DIVBYSALES$GPMTACT-DIVBYSALES$GPMTPREV
DIVBYSALES[is.na(DIVBYSALES)]      <- NA
DIVBYSALES$GPMTDIFFPER       <- ifelse(is.infinite((DIVBYSALES$GPMTACT-DIVBYSALES$GPMTPREV)/DIVBYSALES$GPMTPREV),"",ifelse(is.na(((DIVBYSALES$GPMTACT-DIVBYSALES$GPMTPREV)/DIVBYSALES$GPMTPREV)),"",scales::percent(((DIVBYSALES$GPMTACT-DIVBYSALES$GPMTPREV)/DIVBYSALES$GPMTPREV),accuracy = 0.1)))
DIVBYSALES[is.na(DIVBYSALES)]      <- NA

DT                     <- data.table(DIVBYSALES)
DIVBYSALES                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
DIVBYSALES[is.na(DIVBYSALES)]      <- NA
DIVBYSALES$Clasification     <- stri_replace_all_fixed(DIVBYSALES$Clasification, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
DIVBYSALES$Division    <- stri_replace_all_fixed(DIVBYSALES$Division, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
DIVBYSALES                      <- setorder(DIVBYSALES,Clasification,-QuantityDiffYYPER_P)
DIVBYSALES$QuantityDiffYYPER_P <-NULL


Total<-data.frame(Mapowanie="Total CEE",Division="Total CEE", Clasification="Total CEE",
                  QActYear          = sum(DIVBYSALES$QActYear),
                  QprevYear         = sum(DIVBYSALES$QprevYear),
                  GPACT             = sum(DIVBYSALES$GPACT),
                  GPPREV            = sum(DIVBYSALES$GPPREV),
                  TURNACT           = sum(DIVBYSALES$TURNACT),
                  TURNPREV          = sum(DIVBYSALES$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(DIVBYSALES$TurnDiffYY),
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


Divisions<-unique(DIVBYSALES$Division)
Icmki <-unique(DIVBYSALES$Mapowanie)



for(region in unique(DIVBYSALES$Mapowanie)){
  steal <- subset(DIVBYSALES,Mapowanie==region)
  frejm <- data.frame(Mapowanie=region,Region="TOTAL", Division=region, 
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
 DIVBYSALES<- rbind(DIVBYSALES, frejm)
  
}




# GRUPOWANIE NORTH SOUTH CEE

for(mapowanie in Icmki){
  for(region in Divisions){
    steal <- subset(DIVBYSALES,Mapowanie==mapowanie & Division==region)
    frejm <- data.frame(Mapowanie=mapowanie,Division=region, Clasification=region,
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
    #browser()
    
    DIVBYSALES <- rbind(DIVBYSALES, frejm)
    
  }
}



