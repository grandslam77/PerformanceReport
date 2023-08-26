
BBS_podst                     <- read_xlsx(here::here("sources","DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "BBS",range="I16:X6000", col_names = TRUE,na = "NA")

BBS_podst                     <- BBS_podst %>% filter_all(any_vars(!is.na(.))) 
BBS_podst[is.na(BBS_podst)]   <- 0   # wszystkie pola bez wartości wypełniamy zerami

BBS                           <- BBS_podst


#BBS[,11:16]             <- round(BBS[,11:16],0)


Naglowek1 <- "Quantity KG TOTAL 2021"
Naglowek2 <- "Quantity KG TOTAL 2022"
Naglowek3 <- "Margin EUR (adj) TOTAL 2021"
Naglowek4 <- "Margin EUR (adj) TOTAL 2022"
Naglowek5 <- "Turnover EUR (adj) TOTAL 2021"
Naglowek6 <- "Turnover EUR (adj) TOTAL 2022"

colnames(BBS)[11]<-Naglowek1
colnames(BBS)[12]<-Naglowek2
colnames(BBS)[13]<-Naglowek3
colnames(BBS)[14]<-Naglowek4
colnames(BBS)[15]<-Naglowek5
colnames(BBS)[16]<-Naglowek6




BBS                    <- left_join(BBS,MapowanieBaltics,by="Subsidiary Country")


BBSM_YTD                   <- BBS %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
BBSM_YTD                   <- BBSM_YTD %>% select(17,18,11,12,13,14,15,16)

# BBSM_YTD<-rbind(BBSM_YTD,BBSM_YTD2)
BBSM_YTD                   <- as.data.frame(BBSM_YTD)
BBSM_YTD                   <- BBSM_YTD %>% group_by(Region,SubRegion) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
BBSM_YTD[is.na(BBSM_YTD)]      <- NA
BBSM_YTD$QuantityDiffYY    <- BBSM_YTD$QActYear-BBSM_YTD$QprevYear
BBSM_YTD[is.na(BBSM_YTD)]      <- NA
BBSM_YTD$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((BBSM_YTD$QActYear-BBSM_YTD$QprevYear)/BBSM_YTD$QprevYear)),"",(BBSM_YTD$QActYear-BBSM_YTD$QprevYear)/BBSM_YTD$QprevYear))
BBSM_YTD$QuantityDiffYYPER <- ifelse(is.infinite(((BBSM_YTD$QActYear-BBSM_YTD$QprevYear)/BBSM_YTD$QprevYear)),"",scales::percent(((BBSM_YTD$QActYear-BBSM_YTD$QprevYear)/BBSM_YTD$QprevYear),accuracy = 0.1))
BBSM_YTD[is.na(BBSM_YTD)]      <- NA
BBSM_YTD$GPDiffYY          <- BBSM_YTD$GPACT-BBSM_YTD$GPPREV
BBSM_YTD[is.na(BBSM_YTD)]      <- NA
BBSM_YTD$GPDiffYYPER       <- ifelse(is.infinite(((BBSM_YTD$GPACT-BBSM_YTD$GPPREV)/BBSM_YTD$GPPREV)),"",scales::percent(((BBSM_YTD$GPACT-BBSM_YTD$GPPREV)/BBSM_YTD$GPPREV),accuracy = 0.1))
#BBSM_YTD$GPDiffYYPER       <- round(((BBSM_YTD$GPACT-BBSM_YTD$GPPREV)/BBSM_YTD$GPPREV)*100,2)
BBSM_YTD[is.na(BBSM_YTD)]      <- NA
BBSM_YTD$TurnDiffYY        <- BBSM_YTD$TURNACT-BBSM_YTD$TURNPREV
BBSM_YTD[is.na(BBSM_YTD)]      <- NA
BBSM_YTD$TurnDiffYYPER     <- ifelse(is.infinite(((BBSM_YTD$TURNACT-BBSM_YTD$TURNPREV)/BBSM_YTD$TURNPREV)),"",scales::percent(((BBSM_YTD$TURNACT-BBSM_YTD$TURNPREV)/BBSM_YTD$TURNPREV),accuracy = 0.1))
BBSM_YTD[is.na(BBSM_YTD)]      <- NA
BBSM_YTD$GPMTACT           <- ifelse(is.infinite(BBSM_YTD$GPACT*1000/BBSM_YTD$QActYear),0,BBSM_YTD$GPACT*1000/BBSM_YTD$QActYear)   # x 1000 ?
BBSM_YTD[is.na(BBSM_YTD)]      <- 0
BBSM_YTD$GPMTPREV          <- ifelse(is.infinite(BBSM_YTD$GPPREV*1000/BBSM_YTD$QprevYear),0,BBSM_YTD$GPPREV*1000/BBSM_YTD$QprevYear)
BBSM_YTD[is.na(BBSM_YTD)]      <- 0
BBSM_YTD$GPMTDIFF          <- BBSM_YTD$GPMTACT-BBSM_YTD$GPMTPREV
BBSM_YTD[is.na(BBSM_YTD)]      <- NA
BBSM_YTD$GPMTDIFFPER       <- ifelse(is.infinite((BBSM_YTD$GPMTACT-BBSM_YTD$GPMTPREV)/BBSM_YTD$GPMTPREV),"",scales::percent(((BBSM_YTD$GPMTACT-BBSM_YTD$GPMTPREV)/BBSM_YTD$GPMTPREV),accuracy = 0.1))
BBSM_YTD[is.na(BBSM_YTD)]      <- NA

DT                     <- data.table(BBSM_YTD)
BBSM_YTD                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
BBSM_YTD[is.na(BBSM_YTD)]      <- NA
BBSM_YTD$SubRegion         <- stri_replace_all_fixed(BBSM_YTD$SubRegion, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
BBSM_YTD                      <- setorder(BBSM_YTD,Region,-QuantityDiffYYPER_P)
BBSM_YTD$QuantityDiffYYPER_P <-NULL



Total<-data.frame(Region="Total CEE", SubRegion="Total CEE",
                  QActYear          = sum(BBSM_YTD$QActYear),
                  QprevYear         = sum(BBSM_YTD$QprevYear),
                  GPACT             = sum(BBSM_YTD$GPACT),
                  GPPREV            = sum(BBSM_YTD$GPPREV),
                  TURNACT           = sum(BBSM_YTD$TURNACT),
                  TURNPREV          = sum(BBSM_YTD$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(BBSM_YTD$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(BBSM_YTD$GPMTACT),
                  GPMTPREV          = sum(BBSM_YTD$GPMTPREV),
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

Regiony=unique(BBSM_YTD$Region)

for(region in unique(BBSM_YTD$Region)){
  
  steal <- subset(BBSM_YTD,Region==region)
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
                      GPMTACT           = sum(steal$GPMTACT),
                      GPMTPREV          = sum(steal$GPMTPREV),
                      GPMTDIFF          = sum(steal$GPMTDIFF),
                      GPMTDIFFPER       = 0)
  frejm$GPMTACT            <-  frejm$GPACT*1000/Total$QActYear
  frejm$GPMTPREV           <-  frejm$GPPREV*1000/Total$QprevYear
  frejm$TurnDiffYY         <-  frejm$TURNACT-frejm$TURNPREV
  frejm$QuantityDiffYY     <-  frejm$QActYear-frejm$QprevYear
  frejm$GPDiffYY           <-  frejm$GPACT-frejm$GPPREV
  frejm$GPMTDIFF           <-  frejm$GPMTACT-frejm$GPMTPREV
  frejm$QuantityDiffYYPER  <-  scales::percent(frejm$QuantityDiffYY/frejm$QprevYear,accuracy = 0.1)
  frejm$GPDiffYYPER        <-  scales::percent(frejm$GPDiffYY/frejm$GPPREV,accuracy = 0.1)
  frejm$TurnDiffYYPER      <-  scales::percent(frejm$TurnDiffYY/frejm$TURNPREV,accuracy = 0.1)
  frejm$GPMTDIFFPER        <-  scales::percent(frejm$GPMTDIFF/frejm$GPMTPREV,accuracy = 0.1)
  
  
  BBSM_YTD <- rbind(BBSM_YTD, frejm)
  
}

BBSM_YTD                      <- rbind(BBSM_YTD,Total)
BBSM_YTD                      <- setorder(BBSM_YTD,Region)
BBSM_YTD$Region               <- NULL
BBSM_YTD                      <- BBSM_YTD %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
BBSM_YTD[is.na(BBSM_YTD)]         <- ""

WyboldowaneWierszeBBSM_YTD <- c(which(BBSM_YTD$SubRegion %in% Regiony),length(BBSM_YTD$SubRegion))

# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

BBSM_YTD <- BBSM_YTD %>% mutate_if(is.numeric, ~round(., 0))



#### ---------------- BBS EUROPEAN INDUSTRY

BBS_EI_YTD <- BBS_podst


colnames(BBS_EI_YTD)[11]<-Naglowek1
colnames(BBS_EI_YTD)[12]<-Naglowek2
colnames(BBS_EI_YTD)[13]<-Naglowek3
colnames(BBS_EI_YTD)[14]<-Naglowek4
colnames(BBS_EI_YTD)[15]<-Naglowek5
colnames(BBS_EI_YTD)[16]<-Naglowek6

BBS_EI_YTD                  <- BBS_EI_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)

colnames(MapowanieBranzaIndu)[1]  <-"European Industry"

BBS_EI_YTD                    <- left_join(BBS_EI_YTD,MapowanieBranzaIndu,by="European Industry")

BBS_EI_YTD                   <- BBS_EI_YTD %>% select(17,11:16)
colnames(BBS_EI_YTD)[1]  <-"European Industry"
# BBSM_YTD<-rbind(BBSM_YTD,BBSM_YTD2)
BBS_EI_YTD                    <- as.data.frame(BBS_EI_YTD )
BBS_EI_YTD                    <- BBS_EI_YTD  %>% group_by(`European Industry`) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
BBS_EI_YTD [is.na(BBS_EI_YTD)]    <- NA
BBS_EI_YTD$QuantityDiffYY    <- BBS_EI_YTD$QActYear-BBS_EI_YTD$QprevYear
BBS_EI_YTD[is.na(BBS_EI_YTD)]      <- NA
BBS_EI_YTD$QuantityDiffYYPER_P <- ifelse(is.infinite(((BBS_EI_YTD$QActYear-BBS_EI_YTD$QprevYear)/BBS_EI_YTD$QprevYear)),"",(BBS_EI_YTD$QActYear-BBS_EI_YTD$QprevYear)/BBS_EI_YTD$QprevYear)
BBS_EI_YTD$QuantityDiffYYPER <- ifelse(is.infinite(((BBS_EI_YTD$QActYear-BBS_EI_YTD$QprevYear)/BBS_EI_YTD$QprevYear)),"",scales::percent(((BBS_EI_YTD$QActYear-BBS_EI_YTD$QprevYear)/BBS_EI_YTD$QprevYear),accuracy = 0.1))

BBS_EI_YTD[is.na(BBS_EI_YTD)]      <- NA
BBS_EI_YTD$GPDiffYY          <- BBS_EI_YTD$GPACT-BBS_EI_YTD$GPPREV
BBS_EI_YTD[is.na(BBS_EI_YTD)]      <- NA
BBS_EI_YTD$GPDiffYYPER       <- ifelse(is.infinite(((BBS_EI_YTD$GPACT-BBS_EI_YTD$GPPREV)/BBS_EI_YTD$GPPREV)),"",scales::percent(((BBS_EI_YTD$GPACT-BBS_EI_YTD$GPPREV)/BBS_EI_YTD$GPPREV),accuracy = 0.1))
#BBS_EI_YTD$GPDiffYYPER       <- round(((BBS_EI_YTD$GPACT-BBS_EI_YTD$GPPREV)/BBS_EI_YTD$GPPREV)*100,2)
BBS_EI_YTD[is.na(BBS_EI_YTD)]      <- NA
BBS_EI_YTD$TurnDiffYY        <- BBS_EI_YTD$TURNACT-BBS_EI_YTD$TURNPREV
BBS_EI_YTD[is.na(BBS_EI_YTD)]      <- NA
BBS_EI_YTD$TurnDiffYYPER     <- ifelse(is.infinite(((BBS_EI_YTD$TURNACT-BBS_EI_YTD$TURNPREV)/BBS_EI_YTD$TURNPREV)),"",scales::percent(((BBS_EI_YTD$TURNACT-BBS_EI_YTD$TURNPREV)/BBS_EI_YTD$TURNPREV),accuracy = 0.1))
BBS_EI_YTD[is.na(BBS_EI_YTD)]      <- NA
BBS_EI_YTD$GPMTACT           <- ifelse(is.infinite(BBS_EI_YTD$GPACT*1000/BBS_EI_YTD$QActYear),0,BBS_EI_YTD$GPACT*1000/BBS_EI_YTD$QActYear)   # x 1000 ?
BBS_EI_YTD[is.na(BBS_EI_YTD)]      <- 0
BBS_EI_YTD$GPMTPREV          <- ifelse(is.infinite(BBS_EI_YTD$GPPREV*1000/BBS_EI_YTD$QprevYear),0,BBS_EI_YTD$GPPREV*1000/BBS_EI_YTD$QprevYear)
BBS_EI_YTD[is.na(BBS_EI_YTD)]      <- 0
BBS_EI_YTD$GPMTDIFF          <- BBS_EI_YTD$GPMTACT-BBS_EI_YTD$GPMTPREV
BBS_EI_YTD[is.na(BBS_EI_YTD)]      <- NA
BBS_EI_YTD$GPMTDIFFPER       <- ifelse(is.infinite((BBS_EI_YTD$GPMTACT-BBS_EI_YTD$GPMTPREV)/BBS_EI_YTD$GPMTPREV),"",scales::percent(((BBS_EI_YTD$GPMTACT-BBS_EI_YTD$GPMTPREV)/BBS_EI_YTD$GPMTPREV),accuracy = 0.1))
BBS_EI_YTD[is.na(BBS_EI_YTD)]      <- NA

DT                     <- data.table(BBS_EI_YTD)
BBS_EI_YTD                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
BBS_EI_YTD[is.na(BBS_EI_YTD)]      <- NA
BBS_EI_YTD                     <- setorder(BBS_EI_YTD,-QuantityDiffYYPER_P)
BBS_EI_YTD$QuantityDiffYYPER_P<-NULL

colnames(BBS_EI_YTD)[1]      <- "EI"

Total<-data.frame('EI'="Total CEE",
                  QActYear          = sum(BBS_EI_YTD$QActYear),
                  QprevYear         = sum(BBS_EI_YTD$QprevYear),
                  GPACT             = sum(BBS_EI_YTD$GPACT),
                  GPPREV            = sum(BBS_EI_YTD$GPPREV),
                  TURNACT           = sum(BBS_EI_YTD$TURNACT),
                  TURNPREV          = sum(BBS_EI_YTD$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(BBS_EI_YTD$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(BBS_EI_YTD$GPMTACT),
                  GPMTPREV          = sum(BBS_EI_YTD$GPMTPREV),
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

BBS_EI_YTD                   <- rbind(BBS_EI_YTD,Total)
#BBS_EI_YTD                   <- setorder(BBS_EI_YTD,EI)
#BBSM_YTD$Region              <- NULL
BBS_EI_YTD                     <- BBS_EI_YTD %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
BBS_EI_YTD[is.na(BBS_EI_YTD)]        <- ""

BBS_EI_YTD$EI        <- stri_replace_all_fixed(BBS_EI_YTD$EI, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
BBS_EI_YTD$EI        <- stri_replace_all_fixed(BBS_EI_YTD$EI, pattern = c("Industrial Sales and Services"), replacement = c("Ind. Sales and Serv."), vectorize_all = FALSE)


# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

BBS_EI_YTD <- BBS_EI_YTD %>% mutate_if(is.numeric, ~round(., 0))

WyboldowaneWierszeBBS_EI_YTD <- c(length(BBS_EI_YTD$EI))

#Jest problem z wydzieleniem Itercompany Outside z Industrial Sales and Services.

### -------------------  BBS results - Key Businesses YTD

# Mapować kraje chociaż nie wystapiła Chorwacja&SLOVENIA
# CROATIA?   dlaczego w raporcie Asi jest a u mnie nie ma?

BBS_KEY_YTD                   <- read_xlsx(here::here("sources","DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "BBS",range="AA16:AP2600", col_names = TRUE,na = "NA")

BBS_KEY_YTD                   <- BBS_KEY_YTD%>% filter_all(any_vars(!is.na(.))) 
BBS_KEY_YTD[is.na(BBS_KEY_YTD)]    <- 0   # wszystkie pola bez wartości wypełniamy zerami


colnames(BBS_KEY_YTD)[11]<-Naglowek1
colnames(BBS_KEY_YTD)[12]<-Naglowek2
colnames(BBS_KEY_YTD)[13]<-Naglowek3
colnames(BBS_KEY_YTD)[14]<-Naglowek4
colnames(BBS_KEY_YTD)[15]<-Naglowek5
colnames(BBS_KEY_YTD)[16]<-Naglowek6
colnames(BBS_KEY_YTD)[3]<-"KeyBusiness"

BBS_KEY_YTD                 <- BBS_KEY_YTD%>% filter(`Calendar month` %in% 1:MiesiacAnalizy)      #   PONIEWAÆ YTD

#colnames(MapowanieBranzaIndu)[1]  <-"European Industry"
#BBS_EI_YTD                    <- left_join(BBS_EI_YTD,MapowanieBranzaIndu,by="European Industry")

BBS_KEY_YTD                  <- BBS_KEY_YTD%>% select(1,3,11:16)

# BBSM_YTD<-rbind(BBSM_YTD,BBSM_YTD2)
BBS_KEY_YTD                  <- as.data.frame(BBS_KEY_YTD)
BBS_KEY_YTD                   <- BBS_KEY_YTD  %>% group_by(`Subsidiary Country`,KeyBusiness) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
BBS_KEY_YTD [is.na(BBS_KEY_YTD)]    <- NA


BBS_KEY_YTD$QuantityDiffYY    <- BBS_KEY_YTD$QActYear-BBS_KEY_YTD$QprevYear
BBS_KEY_YTD[is.na(BBS_KEY_YTD)]      <- NA
BBS_KEY_YTD$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((BBS_KEY_YTD$QActYear-BBS_KEY_YTD$QprevYear)/BBS_KEY_YTD$QprevYear)),"",(BBS_KEY_YTD$QActYear-BBS_KEY_YTD$QprevYear)/BBS_KEY_YTD$QprevYear))
BBS_KEY_YTD$QuantityDiffYYPER <- ifelse(is.infinite(((BBS_KEY_YTD$QActYear-BBS_KEY_YTD$QprevYear)/BBS_KEY_YTD$QprevYear)),"",scales::percent(((BBS_KEY_YTD$QActYear-BBS_KEY_YTD$QprevYear)/BBS_KEY_YTD$QprevYear),accuracy = 0.1))
BBS_KEY_YTD[is.na(BBS_KEY_YTD)]      <- NA
BBS_KEY_YTD$GPDiffYY          <- BBS_KEY_YTD$GPACT-BBS_KEY_YTD$GPPREV
BBS_KEY_YTD[is.na(BBS_KEY_YTD)]      <- NA
BBS_KEY_YTD$GPDiffYYPER       <- ifelse(is.infinite(((BBS_KEY_YTD$GPACT-BBS_KEY_YTD$GPPREV)/BBS_KEY_YTD$GPPREV)),"",scales::percent(((BBS_KEY_YTD$GPACT-BBS_KEY_YTD$GPPREV)/BBS_KEY_YTD$GPPREV),accuracy = 0.1))
#BBS_KEY_YTD$GPDiffYYPER       <- round(((BBS_KEY_YTD$GPACT-BBS_KEY_YTD$GPPREV)/BBS_KEY_YTD$GPPREV)*100,2)
BBS_KEY_YTD[is.na(BBS_KEY_YTD)]      <- NA
BBS_KEY_YTD$TurnDiffYY        <- BBS_KEY_YTD$TURNACT-BBS_KEY_YTD$TURNPREV
BBS_KEY_YTD[is.na(BBS_KEY_YTD)]      <- NA
BBS_KEY_YTD$TurnDiffYYPER     <- ifelse(is.infinite(((BBS_KEY_YTD$TURNACT-BBS_KEY_YTD$TURNPREV)/BBS_KEY_YTD$TURNPREV)),"",scales::percent(((BBS_KEY_YTD$TURNACT-BBS_KEY_YTD$TURNPREV)/BBS_KEY_YTD$TURNPREV),accuracy = 0.1))
BBS_KEY_YTD[is.na(BBS_KEY_YTD)]      <- NA
BBS_KEY_YTD$GPMTACT           <- ifelse(is.infinite(BBS_KEY_YTD$GPACT*1000/BBS_KEY_YTD$QActYear),0,BBS_KEY_YTD$GPACT*1000/BBS_KEY_YTD$QActYear)   # x 1000 ?
BBS_KEY_YTD[is.na(BBS_KEY_YTD)]      <- 0
BBS_KEY_YTD$GPMTPREV          <- ifelse(is.infinite(BBS_KEY_YTD$GPPREV*1000/BBS_KEY_YTD$QprevYear),0,BBS_KEY_YTD$GPPREV*1000/BBS_KEY_YTD$QprevYear)
BBS_KEY_YTD[is.na(BBS_KEY_YTD)]      <- 0
BBS_KEY_YTD$GPMTDIFF          <- BBS_KEY_YTD$GPMTACT-BBS_KEY_YTD$GPMTPREV
BBS_KEY_YTD[is.na(BBS_KEY_YTD)]      <- NA
BBS_KEY_YTD$GPMTDIFFPER       <- ifelse(is.infinite((BBS_KEY_YTD$GPMTACT-BBS_KEY_YTD$GPMTPREV)/BBS_KEY_YTD$GPMTPREV),"",scales::percent(((BBS_KEY_YTD$GPMTACT-BBS_KEY_YTD$GPMTPREV)/BBS_KEY_YTD$GPMTPREV),accuracy = 0.1))
BBS_KEY_YTD[is.na(BBS_KEY_YTD)]      <- NA

DT                     <- data.table(BBS_KEY_YTD)
BBS_KEY_YTD                  <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
BBS_KEY_YTD[is.na(BBS_KEY_YTD)]      <- NA
BBS_KEY_YTD$`Subsidiary Country`<- stri_replace_all_fixed(BBS_KEY_YTD$`Subsidiary Country`, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
BBS_KEY_YTD$KeyBusiness<- stri_replace_all_fixed(BBS_KEY_YTD$KeyBusiness, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)

BBS_KEY_YTD                     <- setorder(BBS_KEY_YTD,`Subsidiary Country` ,-QuantityDiffYYPER_P)
BBS_KEY_YTD$QuantityDiffYYPER_P <-NULL

Total<-data.frame('Subsidiary Country'="Total CEE Key B.", KeyBusiness="Total CEE Key B.",
                  QActYear          = sum(BBS_KEY_YTD$QActYear),
                  QprevYear         = sum(BBS_KEY_YTD$QprevYear),
                  GPACT             = sum(BBS_KEY_YTD$GPACT),
                  GPPREV            = sum(BBS_KEY_YTD$GPPREV),
                  TURNACT           = sum(BBS_KEY_YTD$TURNACT),
                  TURNPREV          = sum(BBS_KEY_YTD$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(BBS_KEY_YTD$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(BBS_KEY_YTD$GPMTACT),
                  GPMTPREV          = sum(BBS_KEY_YTD$GPMTPREV),
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
colnames(Total)[1]<-'Subsidiary Country'

#BBS_EI                   <- setorder(BBS_EI,EI)
#BBSM_YTD$Region              <- NULL

BBS_KEY_YTD[is.na(BBS_KEY_YTD)]        <- ""

Kraje<-unique(BBS_KEY_YTD$`Subsidiary Country`)
for(region in unique(BBS_KEY_YTD$`Subsidiary Country`)){
  
  steal <- subset(BBS_KEY_YTD,`Subsidiary Country`==region)
  frejm <- data.frame('Subsidiary Country'=region, 'KeyBusiness'=region, 
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
  frejm$GPMTACT            <-  frejm$GPACT*1000/Total$QActYear
  frejm$GPMTPREV           <-  frejm$GPPREV*1000/Total$QprevYear
  frejm$TurnDiffYY         <-  frejm$TURNACT-frejm$TURNPREV
  frejm$QuantityDiffYY     <-  frejm$QActYear-frejm$QprevYear
  frejm$GPDiffYY           <-  frejm$GPACT-frejm$GPPREV
  frejm$GPMTDIFF           <-  frejm$GPMTACT-frejm$GPMTPREV
  frejm$QuantityDiffYYPER  <-  scales::percent(frejm$QuantityDiffYY/frejm$QprevYear,accuracy = 0.1)
  frejm$GPDiffYYPER        <-  scales::percent(frejm$GPDiffYY/frejm$GPPREV,accuracy = 0.1)
  frejm$TurnDiffYYPER      <-  scales::percent(frejm$TurnDiffYY/frejm$TURNPREV,accuracy = 0.1)
  frejm$GPMTDIFFPER        <-  scales::percent(frejm$GPMTDIFF/frejm$GPMTPREV,accuracy = 0.1)
  
  colnames(frejm)[1]<-"Subsidiary Country"
  BBS_KEY_YTD<- rbind(BBS_KEY_YTD, frejm)
  
}


BBS_KEY_YTD                      <- rbind(BBS_KEY_YTD,Total)
BBS_KEY_YTD                     <- setorder(BBS_KEY_YTD,`Subsidiary Country`)
BBS_KEY_YTD$`Subsidiary Country`               <- NULL

BBS_KEY_YTD                      <- BBS_KEY_YTD%>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
BBS_KEY_YTD[is.na(BBS_KEY_YTD)]       <- ""
BBS_KEY_YTD<- BBS_KEY_YTD%>% mutate_if(is.numeric, ~round(., 0))

WyboldowaneWierszeBBS_KEY_YTD<-c(which(BBS_KEY_YTD$KeyBusiness %in% Kraje),length(BBS_KEY_YTD$KeyBusiness))

