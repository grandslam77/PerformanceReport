
MAIN_ESSP_T<-MainSource
MAIN_ESSP_T<- left_join(MAIN_ESSP_T,MapowanieBranzaIndu,by="European Industry")

colnames(MAIN_ESSP_T)[3]<-"Cleaning"
colnames(MAIN_ESSP_T)[12]<-Naglowek1
colnames(MAIN_ESSP_T)[13]<-Naglowek2
colnames(MAIN_ESSP_T)[14]<-Naglowek3
colnames(MAIN_ESSP_T)[15]<-Naglowek4
colnames(MAIN_ESSP_T)[16]<-Naglowek5
colnames(MAIN_ESSP_T)[17]<-Naglowek6

MAIN_ESSP_T                     <- MAIN_ESSP_T %>% filter(`Calendar month`==MiesiacAnalizy)
MAIN_ESSP_T$MapowanieBranzaInd  <-ifelse(MAIN_ESSP_T$Cleaning=="Others Industries",MAIN_ESSP_T$MapowanieBranzaInd,MAIN_ESSP_T$Cleaning)

MAIN_ESSP_T                     <- MAIN_ESSP_T %>% select(22,21,12,13,14,15,16,17)




# MAIN_ESSP_T <- rbind(MAIN_ESSP_T,MAIN_ESSP_T2)
MAIN_ESSP_T                        <- as.data.frame(MAIN_ESSP_T)
MAIN_ESSP_T                        <- MAIN_ESSP_T %>% group_by(MapowanieBranzaInd,MapowanieMD) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
MAIN_ESSP_T[is.na(MAIN_ESSP_T)]      <- NA
MAIN_ESSP_T$QuantityDiffYY         <- MAIN_ESSP_T$QActYear-MAIN_ESSP_T$QprevYear
MAIN_ESSP_T[is.na(MAIN_ESSP_T)]      <- NA
MAIN_ESSP_T$QuantityDiffYYPER_P    <- as.numeric(ifelse(is.infinite(((MAIN_ESSP_T$QActYear-MAIN_ESSP_T$QprevYear)/MAIN_ESSP_T$QprevYear)),"",(MAIN_ESSP_T$QActYear-MAIN_ESSP_T$QprevYear)/MAIN_ESSP_T$QprevYear))
MAIN_ESSP_T$QuantityDiffYYPER      <- ifelse(is.infinite(((MAIN_ESSP_T$QActYear-MAIN_ESSP_T$QprevYear)/MAIN_ESSP_T$QprevYear)),"",scales::percent(((MAIN_ESSP_T$QActYear-MAIN_ESSP_T$QprevYear)/MAIN_ESSP_T$QprevYear),accuracy = 0.1))
MAIN_ESSP_T[is.na(MAIN_ESSP_T)]      <- NA
MAIN_ESSP_T$GPDiffYY               <- MAIN_ESSP_T$GPACT-MAIN_ESSP_T$GPPREV
MAIN_ESSP_T[is.na(MAIN_ESSP_T)]      <- NA
MAIN_ESSP_T$GPDiffYYPER            <- ifelse(is.infinite(((MAIN_ESSP_T$GPACT-MAIN_ESSP_T$GPPREV)/MAIN_ESSP_T$GPPREV)),"",scales::percent(((MAIN_ESSP_T$GPACT-MAIN_ESSP_T$GPPREV)/MAIN_ESSP_T$GPPREV),accuracy = 0.1))
#MAIN_ESSP_T$GPDiffYYPER       <- round(((MAIN_ESSP_T$GPACT-MAIN_ESSP_T$GPPREV)/MAIN_ESSP_T$GPPREV)*100,2)
MAIN_ESSP_T[is.na(MAIN_ESSP_T)]      <- NA
MAIN_ESSP_T$TurnDiffYY             <- MAIN_ESSP_T$TURNACT-MAIN_ESSP_T$TURNPREV
MAIN_ESSP_T[is.na(MAIN_ESSP_T)]      <- NA
MAIN_ESSP_T$TurnDiffYYPER          <- ifelse(is.infinite(((MAIN_ESSP_T$TURNACT-MAIN_ESSP_T$TURNPREV)/MAIN_ESSP_T$TURNPREV)),"",scales::percent(((MAIN_ESSP_T$TURNACT-MAIN_ESSP_T$TURNPREV)/MAIN_ESSP_T$TURNPREV),accuracy = 0.1))
MAIN_ESSP_T[is.na(MAIN_ESSP_T)]      <- NA
MAIN_ESSP_T$GPMTACT                <- ifelse(is.infinite(MAIN_ESSP_T$GPACT*1000/MAIN_ESSP_T$QActYear),0,MAIN_ESSP_T$GPACT*1000/MAIN_ESSP_T$QActYear)   # x 1000 ?

char_cols <- sapply(MAIN_ESSP_T, is.character)
ktore<-unname(which(char_cols))
MAIN_ESSP_T[, ktore][is.na(MAIN_ESSP_T[, ktore])] <- "0"



MAIN_ESSP_T$GPMTPREV               <- ifelse(is.infinite(MAIN_ESSP_T$GPPREV*1000/MAIN_ESSP_T$QprevYear),0,MAIN_ESSP_T$GPPREV*1000/MAIN_ESSP_T$QprevYear)
MAIN_ESSP_T[, ktore][is.na(MAIN_ESSP_T[, ktore])] <- "0"



MAIN_ESSP_T$GPMTDIFF               <- MAIN_ESSP_T$GPMTACT-MAIN_ESSP_T$GPMTPREV
MAIN_ESSP_T[, ktore][is.na(MAIN_ESSP_T[, ktore])] <- "0"
MAIN_ESSP_T[is.na(MAIN_ESSP_T)]      <- NA
MAIN_ESSP_T$GPMTDIFFPER            <- ifelse(is.infinite((MAIN_ESSP_T$GPMTACT-MAIN_ESSP_T$GPMTPREV)/MAIN_ESSP_T$GPMTPREV),"",scales::percent(((MAIN_ESSP_T$GPMTACT-MAIN_ESSP_T$GPMTPREV)/MAIN_ESSP_T$GPMTPREV),accuracy = 0.1))
MAIN_ESSP_T[, ktore][is.na(MAIN_ESSP_T[, ktore])] <- "0"
MAIN_ESSP_T[is.na(MAIN_ESSP_T)]      <- NA

DT                               <- data.table(MAIN_ESSP_T)
MAIN_ESSP_T                        <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
MAIN_ESSP_T[is.na(MAIN_ESSP_T)]      <- NA
MAIN_ESSP_T$MapowanieBranzaInd              <- stri_replace_all_fixed(MAIN_ESSP_T$MapowanieBranzaInd, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
MAIN_ESSP_T                        <- setorder(MAIN_ESSP_T,MapowanieBranzaInd,MapowanieMD,-QActYear)
MAIN_ESSP_T$QuantityDiffYYPER_P    <- NULL


RegionyCEE <- unique(MAIN_ESSP_T$MapowanieBranzaInd)


Total <- data.frame(MapowanieBranzaInd="Total CEE", MapowanieMD="Total CEE",
                    QActYear          = sum(MAIN_ESSP_T$QActYear),
                    QprevYear         = sum(MAIN_ESSP_T$QprevYear),
                    GPACT             = sum(MAIN_ESSP_T$GPACT),
                    GPPREV            = sum(MAIN_ESSP_T$GPPREV),
                    TURNACT           = sum(MAIN_ESSP_T$TURNACT),
                    TURNPREV          = sum(MAIN_ESSP_T$TURNPREV),
                    QuantityDiffYY    = 0,
                    QuantityDiffYYPER = 0,
                    GPDiffYYPER       = 0,
                    GPDiffYY          = 0,
                    TurnDiffYY        = sum(MAIN_ESSP_T$TurnDiffYY),
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

Regiony <- unique(MAIN_ESSP_T$MapowanieBranzaInd)

for(region in unique(MAIN_ESSP_T$MapowanieBranzaInd)){
  steal <- subset(MAIN_ESSP_T,MapowanieBranzaInd==region)
  frejm <- data.frame(MapowanieBranzaInd=region, MapowanieMD=region,
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
  MAIN_ESSP_T <- rbind(MAIN_ESSP_T, frejm)
  
}

#GRUPOWANIE NORTH SOUTH CEE

# 
# 
# for(region in RegionyCEE){
#   
#   steal <- subset(MAIN_ESSP_T,Region==region)
#   frejm <- data.frame(Region=region,SubRegion=paste("TOTAL", region), MapowanieMD=paste("TOTAL",region),
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
#   MAIN_ESSP_T <- rbind(MAIN_ESSP_T, frejm)
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
# MAIN_ESSP_T                      <- left_join(MAIN_ESSP_T,MapowaniePomocnicze,by="SubRegion")
# MAIN_ESSP_T$Region               <- ifelse(is.na(MAIN_ESSP_T$Region),MAIN_ESSP_T$RegionU,MAIN_ESSP_T$Region)
# MAIN_ESSP_T$RegionU              <- NULL
# 

MAIN_ESSP_T                      <- setorder(MAIN_ESSP_T,MapowanieBranzaInd,QActYear)
MAIN_ESSP_T                      <- rbind(MAIN_ESSP_T,Total)

WyboldowaneWierszeMAIN_ESSP_T    <- c(which(MAIN_ESSP_T$MapowanieMD %in% Regiony),length(MAIN_ESSP_T$MapowanieMD))
WyboldowaneWierszeMAIN_ESSP_T    <- c(which(str_sub(MAIN_ESSP_T$MapowanieMD,7,9) == "CEE"),WyboldowaneWierszeMAIN_ESSP_T)

# MAIN_ESSP_T         <- rbind(MAIN_ESSP_T,thereofCEE) na razie tą część tabelki trzeba odpuścić, do znalezienia rozwiązania





MAIN_ESSP_T$MapowanieBranzaInd              <- NULL


MAIN_ESSP_T                        <- MAIN_ESSP_T %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)



# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

MAIN_ESSP_T                                                                         <- MAIN_ESSP_T %>% mutate_if(is.numeric, ~round(., 0)) 
MAIN_ESSP_T[is.na(MAIN_ESSP_T)]    <- ""
MAIN_ESSP_T$MapowanieMD[MAIN_ESSP_T$MapowanieMD=="Cleaning (II) - DISTRIBUTION"]    <- "CLEANING DISTR."
MAIN_ESSP_T$MapowanieMD[MAIN_ESSP_T$MapowanieMD=="Cleaning (II) - TOLL PRODUCTION"] <- "CLEANING TOLL PR."
MAIN_ESSP_T$MapowanieMD[MAIN_ESSP_T$MapowanieMD=="Industrial Sales and Services"]   <- "Indstr. Sales.Serv."



# YTD


MAIN_ESSP_TY  <- MainSource
MAIN_ESSP_TY  <- left_join(MAIN_ESSP_TY,MapowanieBranzaIndu,by="European Industry")

colnames(MAIN_ESSP_TY)[3]  <- "Cleaning"
colnames(MAIN_ESSP_TY)[12] <- Naglowek1
colnames(MAIN_ESSP_TY)[13] <- Naglowek2
colnames(MAIN_ESSP_TY)[14] <- Naglowek3
colnames(MAIN_ESSP_TY)[15] <- Naglowek4
colnames(MAIN_ESSP_TY)[16] <- Naglowek5
colnames(MAIN_ESSP_TY)[17] <- Naglowek6

MAIN_ESSP_TY                     <- MAIN_ESSP_TY %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
MAIN_ESSP_TY$MapowanieBranzaInd  <- ifelse(MAIN_ESSP_TY$Cleaning=="Others Industries",MAIN_ESSP_TY$MapowanieBranzaInd,MAIN_ESSP_TY$Cleaning)

MAIN_ESSP_TY                     <- MAIN_ESSP_TY %>% select(22,21,12,13,14,15,16,17)


# MAIN_ESSP_TY <- rbind(MAIN_ESSP_TY,MAIN_ESSP_TY2)
MAIN_ESSP_TY                        <- as.data.frame(MAIN_ESSP_TY)
MAIN_ESSP_TY                        <- MAIN_ESSP_TY %>% group_by(MapowanieBranzaInd,MapowanieMD) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
MAIN_ESSP_TY[is.na(MAIN_ESSP_TY)]    <- NA
MAIN_ESSP_TY$QuantityDiffYY         <- MAIN_ESSP_TY$QActYear-MAIN_ESSP_TY$QprevYear
MAIN_ESSP_TY[is.na(MAIN_ESSP_TY)]    <- NA
MAIN_ESSP_TY$QuantityDiffYYPER_P    <- as.numeric(ifelse(is.infinite(((MAIN_ESSP_TY$QActYear-MAIN_ESSP_TY$QprevYear)/MAIN_ESSP_TY$QprevYear)),"",(MAIN_ESSP_TY$QActYear-MAIN_ESSP_TY$QprevYear)/MAIN_ESSP_TY$QprevYear))
MAIN_ESSP_TY$QuantityDiffYYPER      <- ifelse(is.infinite(((MAIN_ESSP_TY$QActYear-MAIN_ESSP_TY$QprevYear)/MAIN_ESSP_TY$QprevYear)),"",scales::percent(((MAIN_ESSP_TY$QActYear-MAIN_ESSP_TY$QprevYear)/MAIN_ESSP_TY$QprevYear),accuracy = 0.1))
MAIN_ESSP_TY[is.na(MAIN_ESSP_TY)]    <- NA
MAIN_ESSP_TY$GPDiffYY               <- MAIN_ESSP_TY$GPACT-MAIN_ESSP_TY$GPPREV
MAIN_ESSP_TY[is.na(MAIN_ESSP_TY)]    <- NA
MAIN_ESSP_TY$GPDiffYYPER            <- ifelse(is.infinite(((MAIN_ESSP_TY$GPACT-MAIN_ESSP_TY$GPPREV)/MAIN_ESSP_TY$GPPREV)),"",scales::percent(((MAIN_ESSP_TY$GPACT-MAIN_ESSP_TY$GPPREV)/MAIN_ESSP_TY$GPPREV),accuracy = 0.1))
#MAIN_ESSP_TY$GPDiffYYPER       <- round(((MAIN_ESSP_TY$GPACT-MAIN_ESSP_TY$GPPREV)/MAIN_ESSP_TY$GPPREV)*100,2)
MAIN_ESSP_TY[is.na(MAIN_ESSP_TY)]    <- NA
MAIN_ESSP_TY$TurnDiffYY             <- MAIN_ESSP_TY$TURNACT-MAIN_ESSP_TY$TURNPREV
MAIN_ESSP_TY[is.na(MAIN_ESSP_TY)]    <- NA
MAIN_ESSP_TY$TurnDiffYYPER          <- ifelse(is.infinite(((MAIN_ESSP_TY$TURNACT-MAIN_ESSP_TY$TURNPREV)/MAIN_ESSP_TY$TURNPREV)),"",scales::percent(((MAIN_ESSP_TY$TURNACT-MAIN_ESSP_TY$TURNPREV)/MAIN_ESSP_TY$TURNPREV),accuracy = 0.1))
MAIN_ESSP_TY[is.na(MAIN_ESSP_TY)]    <- NA
MAIN_ESSP_TY$GPMTACT                <- ifelse(is.infinite(MAIN_ESSP_TY$GPACT*1000/MAIN_ESSP_TY$QActYear),0,MAIN_ESSP_TY$GPACT*1000/MAIN_ESSP_TY$QActYear)   # x 1000 ?
char_cols <- sapply(MAIN_ESSP_TY, is.character)
ktore<-unname(which(char_cols))
MAIN_ESSP_TY[, ktore][is.na(MAIN_ESSP_TY[, ktore])] <- "0"


MAIN_ESSP_TY$GPMTPREV               <- ifelse(is.infinite(MAIN_ESSP_TY$GPPREV*1000/MAIN_ESSP_TY$QprevYear),0,MAIN_ESSP_TY$GPPREV*1000/MAIN_ESSP_TY$QprevYear)
MAIN_ESSP_TY[, ktore][is.na(MAIN_ESSP_TY[, ktore])] <- "0"

MAIN_ESSP_TY$GPMTDIFF               <- MAIN_ESSP_TY$GPMTACT-MAIN_ESSP_TY$GPMTPREV
MAIN_ESSP_TY[, ktore][is.na(MAIN_ESSP_TY[, ktore])] <- "0"

MAIN_ESSP_TY[is.na(MAIN_ESSP_TY)]    <- NA
MAIN_ESSP_TY$GPMTDIFFPER            <- ifelse(is.infinite((MAIN_ESSP_TY$GPMTACT-MAIN_ESSP_TY$GPMTPREV)/MAIN_ESSP_TY$GPMTPREV),"",scales::percent(((MAIN_ESSP_TY$GPMTACT-MAIN_ESSP_TY$GPMTPREV)/MAIN_ESSP_TY$GPMTPREV),accuracy = 0.1))
MAIN_ESSP_TY[, ktore][is.na(MAIN_ESSP_TY[, ktore])] <- "0"

MAIN_ESSP_TY[is.na(MAIN_ESSP_TY)]    <- NA

DT                                 <- data.table(MAIN_ESSP_TY)
MAIN_ESSP_TY                        <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
MAIN_ESSP_TY[is.na(MAIN_ESSP_TY)]    <- NA
MAIN_ESSP_TY$MapowanieBranzaInd     <- stri_replace_all_fixed(MAIN_ESSP_TY$MapowanieBranzaInd, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
MAIN_ESSP_TY                        <- setorder(MAIN_ESSP_TY,MapowanieBranzaInd,MapowanieMD,-QActYear)
MAIN_ESSP_TY$QuantityDiffYYPER_P    <- NULL

RegionyCEE                         <- unique(MAIN_ESSP_TY$MapowanieBranzaInd)

Total <- data.frame(MapowanieBranzaInd="Total CEE", MapowanieMD="Total CEE",
                    QActYear          = sum(MAIN_ESSP_TY$QActYear),
                    QprevYear         = sum(MAIN_ESSP_TY$QprevYear),
                    GPACT             = sum(MAIN_ESSP_TY$GPACT),
                    GPPREV            = sum(MAIN_ESSP_TY$GPPREV),
                    TURNACT           = sum(MAIN_ESSP_TY$TURNACT),
                    TURNPREV          = sum(MAIN_ESSP_TY$TURNPREV),
                    QuantityDiffYY    = 0,
                    QuantityDiffYYPER = 0,
                    GPDiffYYPER       = 0,
                    GPDiffYY          = 0,
                    TurnDiffYY        = sum(MAIN_ESSP_TY$TurnDiffYY),
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

Regiony <- unique(MAIN_ESSP_TY$MapowanieBranzaInd)

for(region in unique(MAIN_ESSP_TY$MapowanieBranzaInd)){
  steal <- subset(MAIN_ESSP_TY,MapowanieBranzaInd==region)
  frejm <- data.frame(MapowanieBranzaInd=region, MapowanieMD=region,
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
  MAIN_ESSP_TY <- rbind(MAIN_ESSP_TY, frejm)
  
}

#GRUPOWANIE NORTH SOUTH CEE

# 
# 
# for(region in RegionyCEE){
#   
#   steal <- subset(MAIN_ESSP_TY,Region==region)
#   frejm <- data.frame(Region=region,SubRegion=paste("TOTAL", region), MapowanieMD=paste("TOTAL",region),
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
#   MAIN_ESSP_TY <- rbind(MAIN_ESSP_TY, frejm)
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
# MAIN_ESSP_TY                      <- left_join(MAIN_ESSP_TY,MapowaniePomocnicze,by="SubRegion")
# MAIN_ESSP_TY$Region               <- ifelse(is.na(MAIN_ESSP_TY$Region),MAIN_ESSP_TY$RegionU,MAIN_ESSP_TY$Region)
# MAIN_ESSP_TY$RegionU              <- NULL
# 

MAIN_ESSP_TY                      <- setorder(MAIN_ESSP_TY,MapowanieBranzaInd,QActYear)
MAIN_ESSP_TY                      <- rbind(MAIN_ESSP_TY,Total)

WyboldowaneWierszeMAIN_ESSP_TY    <- c(which(MAIN_ESSP_TY$MapowanieMD %in% Regiony),length(MAIN_ESSP_TY$MapowanieMD))
WyboldowaneWierszeMAIN_ESSP_TY    <- c(which(str_sub(MAIN_ESSP_TY$MapowanieMD,7,9) == "CEE"),WyboldowaneWierszeMAIN_ESSP_TY)

# MAIN_ESSP_TY         <- rbind(MAIN_ESSP_TY,thereofCEE) na razie tą część tabelki trzeba odpuścić, do znalezienia rozwiązania



MAIN_ESSP_TY$MapowanieBranzaInd              <- NULL


MAIN_ESSP_TY                        <- MAIN_ESSP_TY %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)



# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

MAIN_ESSP_TY                                                                         <- MAIN_ESSP_TY %>% mutate_if(is.numeric, ~round(., 0)) 
MAIN_ESSP_TY[is.na(MAIN_ESSP_TY)]   <- ""
MAIN_ESSP_TY$MapowanieMD[MAIN_ESSP_TY$MapowanieMD=="Cleaning (II) - DISTRIBUTION"]    <- "CLEANING DISTR."
MAIN_ESSP_TY$MapowanieMD[MAIN_ESSP_TY$MapowanieMD=="Cleaning (II) - TOLL PRODUCTION"] <- "CLEANING TOLL PR."
MAIN_ESSP_TY$MapowanieMD[MAIN_ESSP_TY$MapowanieMD=="Industrial Sales and Services"]   <- "Indstr. Sales.Serv."




