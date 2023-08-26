#---------------- FORMATOWANIE TABEL ---------------------
FormatKolumnyOddzielone        <- c("l","|r","r","r","r","|r","r","r","r","|r","r","r","r","|r","r","r","r|")
FormatKolumnyNieOddzielone     <- c("lrrrrrrrrrrrrrrrr")
ShowNotAssigned = TRUE

PrePerformance = FALSE


colnames(MapowanieBaltics)[1]     <- "Subsidiary Country"
colnames(MapowanieBranzaIndu)[1]  <- "European Industry"

dataAnalizy                    <- as.Date("2023-02-28")
MiesiacAnalizy                 <- month(dataAnalizy)
RokAnalizy                     <- toupper(year(dataAnalizy))
MiesiacAnalizyTekst            <- as.character(toupper(lubridate::month(dataAnalizy,label = TRUE, abbr = FALSE, locale =  'en_US')))
 
Naglowek1 <- "Quantity KG TOTAL 2021"
Naglowek2 <- "Quantity KG TOTAL 2022"
Naglowek3 <- "Margin EUR (adj) TOTAL 2021"
Naglowek4 <- "Margin EUR (adj) TOTAL 2022"
Naglowek5 <- "Turnover EUR (adj) TOTAL 2021"
Naglowek6 <- "Turnover EUR (adj) TOTAL 2022"

sum <- function(x, ..., na.rm = TRUE) {
  base::sum(x, ..., na.rm = na.rm)
}







MainSource                      <- read_xlsx(here::here("sources","DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "BES_BSP",range="I16:Y60000", col_names = TRUE,na = "NA")
MainSource                      <- MainSource  %>% filter_all(any_vars(!is.na(.))) 
MainSource [is.na(MainSource)]  <- 0   # wszystkie pola bez wartości wypełniamy zerami
MainSource                      <- left_join(MainSource,MapowanieBaltics,by="Subsidiary Country")
MainSource                      <- left_join(MainSource,MapowanieBESBSP,by="European Industry")
MainSource                      <- left_join(MainSource,MapowanieESSP,by="Material Division")

BBS_podst                       <- read_xlsx(here::here("sources","DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "BBS",range="I16:X6000", col_names = TRUE,na = "NA")
BBS_podst                       <- BBS_podst %>% filter_all(any_vars(!is.na(.))) 
BBS_podst[is.na(BBS_podst)]     <- 0   # wszystkie pola bez wartości wypełniamy zerami
BBS                             <- BBS_podst


#BBS[,11:16]             <- round(BBS[,11:16],0)

colnames(BBS)[11]<-Naglowek1
colnames(BBS)[12]<-Naglowek2
colnames(BBS)[13]<-Naglowek3
colnames(BBS)[14]<-Naglowek4
colnames(BBS)[15]<-Naglowek5
colnames(BBS)[16]<-Naglowek6

BBS                    <- left_join(BBS,MapowanieBaltics,by="Subsidiary Country")

BBSM                   <- BBS %>% filter(`Calendar month`==MiesiacAnalizy)
BBSM                   <- BBSM %>% select(17,18,11,12,13,14,15,16)

# BBSM<-rbind(BBSM,BBSM2)
BBSM                   <- as.data.frame(BBSM)
BBSM                   <- BBSM %>% dplyr::group_by(Region,SubRegion) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
BBSM[is.na(BBSM)]      <- NA
BBSM$QuantityDiffYY    <- BBSM$QActYear-BBSM$QprevYear
BBSM[is.na(BBSM)]      <- NA
BBSM$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((BBSM$QActYear-BBSM$QprevYear)/BBSM$QprevYear)),"",(BBSM$QActYear-BBSM$QprevYear)/BBSM$QprevYear))
BBSM$QuantityDiffYYPER <- ifelse(is.infinite(((BBSM$QActYear-BBSM$QprevYear)/BBSM$QprevYear)),"",scales::percent(((BBSM$QActYear-BBSM$QprevYear)/BBSM$QprevYear),accuracy = 0.1))
BBSM[is.na(BBSM)]      <- NA
BBSM$GPDiffYY          <- BBSM$GPACT-BBSM$GPPREV
BBSM[is.na(BBSM)]      <- NA
BBSM$GPDiffYYPER       <- ifelse(is.infinite(((BBSM$GPACT-BBSM$GPPREV)/BBSM$GPPREV)),"",scales::percent(((BBSM$GPACT-BBSM$GPPREV)/BBSM$GPPREV),accuracy = 0.1))
#BBSM$GPDiffYYPER       <- round(((BBSM$GPACT-BBSM$GPPREV)/BBSM$GPPREV)*100,2)
BBSM[is.na(BBSM)]      <- NA
BBSM$TurnDiffYY        <- BBSM$TURNACT-BBSM$TURNPREV
BBSM[is.na(BBSM)]      <- NA
BBSM$TurnDiffYYPER     <- ifelse(is.infinite(((BBSM$TURNACT-BBSM$TURNPREV)/BBSM$TURNPREV)),"",scales::percent(((BBSM$TURNACT-BBSM$TURNPREV)/BBSM$TURNPREV),accuracy = 0.1))
BBSM[is.na(BBSM)]      <- NA
BBSM$GPMTACT           <- ifelse(is.infinite(BBSM$GPACT*1000/BBSM$QActYear),0,BBSM$GPACT*1000/BBSM$QActYear)   # x 1000 ?
BBSM[is.na(BBSM)]      <- 0
BBSM$GPMTPREV          <- ifelse(is.infinite(BBSM$GPPREV*1000/BBSM$QprevYear),0,BBSM$GPPREV*1000/BBSM$QprevYear)
BBSM[is.na(BBSM)]      <- 0
BBSM$GPMTDIFF          <- BBSM$GPMTACT-BBSM$GPMTPREV
BBSM[is.na(BBSM)]      <- NA
BBSM$GPMTDIFFPER       <- ifelse(is.infinite((BBSM$GPMTACT-BBSM$GPMTPREV)/BBSM$GPMTPREV),"",scales::percent(((BBSM$GPMTACT-BBSM$GPMTPREV)/BBSM$GPMTPREV),accuracy = 0.1))
BBSM[is.na(BBSM)]      <- NA

DT                     <- data.table(BBSM)
BBSM                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
BBSM[is.na(BBSM)]      <- NA
BBSM$SubRegion         <- stri_replace_all_fixed(BBSM$SubRegion, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
BBSM                      <- setorder(BBSM,Region,-QuantityDiffYYPER_P)
BBSM$QuantityDiffYYPER_P <-NULL



Total<-data.frame(Region="Total CEE", SubRegion="Total CEE",
QActYear          = sum(BBSM$QActYear),
QprevYear         = sum(BBSM$QprevYear),
GPACT             = sum(BBSM$GPACT),
GPPREV            = sum(BBSM$GPPREV),
TURNACT           = sum(BBSM$TURNACT),
TURNPREV          = sum(BBSM$TURNPREV),
QuantityDiffYY    = 0,
QuantityDiffYYPER = 0,
GPDiffYYPER       = 0,
GPDiffYY          = 0,
TurnDiffYY        = sum(BBSM$TurnDiffYY),
TurnDiffYYPER     = 0,
GPMTACT           = sum(BBSM$GPMTACT),
GPMTPREV          = sum(BBSM$GPMTPREV),
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

Regiony=unique(BBSM$Region)
   
for(region in unique(BBSM$Region)){
  
   steal <- subset(BBSM,Region==region)
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
    
    
    BBSM <- rbind(BBSM, frejm)
  
}

BBSM                      <- rbind(BBSM,Total)
BBSM                      <- setorder(BBSM,Region)
BBSM$Region               <- NULL
BBSM                      <- BBSM %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)

WyboldowaneWierszeBBSM <- c(which(BBSM$SubRegion %in% Regiony),length(BBSM$SubRegion))

# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

BBSM <- BBSM %>% mutate_if(is.numeric, ~round(., 0))
BBSM[is.na(BBSM)]         <- ""

#### ---------------- BBS EUROPEAN INDUSTRY

BBS_EI <- BBS_podst

colnames(BBS_EI)[11]<-Naglowek1
colnames(BBS_EI)[12]<-Naglowek2
colnames(BBS_EI)[13]<-Naglowek3
colnames(BBS_EI)[14]<-Naglowek4
colnames(BBS_EI)[15]<-Naglowek5
colnames(BBS_EI)[16]<-Naglowek6

BBS_EI                     <- BBS_EI %>% filter(`Calendar month`==MiesiacAnalizy)

BBS_EI                     <- left_join(BBS_EI,MapowanieBranzaIndu,by="European Industry")
BBS_EI                     <- BBS_EI %>% select(17,11:16)
colnames(BBS_EI)[1]        <-"European Industry"
# BBSM<-rbind(BBSM,BBSM2)
BBS_EI                     <- as.data.frame(BBS_EI )
BBS_EI                     <- BBS_EI  %>% group_by(`European Industry`) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
BBS_EI [is.na(BBS_EI)]     <- NA
BBS_EI$QuantityDiffYY      <- BBS_EI$QActYear-BBS_EI$QprevYear
BBS_EI[is.na(BBS_EI)]      <- NA
BBS_EI$QuantityDiffYYPER_P <- ifelse(is.infinite(((BBS_EI$QActYear-BBS_EI$QprevYear)/BBS_EI$QprevYear)),"",(BBS_EI$QActYear-BBS_EI$QprevYear)/BBS_EI$QprevYear)
BBS_EI$QuantityDiffYYPER   <- ifelse(is.infinite(((BBS_EI$QActYear-BBS_EI$QprevYear)/BBS_EI$QprevYear)),"",scales::percent(((BBS_EI$QActYear-BBS_EI$QprevYear)/BBS_EI$QprevYear),accuracy = 0.1))

BBS_EI[is.na(BBS_EI)]      <- NA
BBS_EI$GPDiffYY            <- BBS_EI$GPACT-BBS_EI$GPPREV
BBS_EI[is.na(BBS_EI)]      <- NA
BBS_EI$GPDiffYYPER         <- ifelse(is.infinite(((BBS_EI$GPACT-BBS_EI$GPPREV)/BBS_EI$GPPREV)),"",scales::percent(((BBS_EI$GPACT-BBS_EI$GPPREV)/BBS_EI$GPPREV),accuracy = 0.1))
#BBS_EI$GPDiffYYPER       <- round(((BBS_EI$GPACT-BBS_EI$GPPREV)/BBS_EI$GPPREV)*100,2)
BBS_EI[is.na(BBS_EI)]      <- NA
BBS_EI$TurnDiffYY          <- BBS_EI$TURNACT-BBS_EI$TURNPREV
BBS_EI[is.na(BBS_EI)]      <- NA
BBS_EI$TurnDiffYYPER     <- ifelse(is.infinite(((BBS_EI$TURNACT-BBS_EI$TURNPREV)/BBS_EI$TURNPREV)),"",scales::percent(((BBS_EI$TURNACT-BBS_EI$TURNPREV)/BBS_EI$TURNPREV),accuracy = 0.1))
BBS_EI[is.na(BBS_EI)]      <- NA
BBS_EI$GPMTACT           <- ifelse(is.infinite(BBS_EI$GPACT*1000/BBS_EI$QActYear),0,BBS_EI$GPACT*1000/BBS_EI$QActYear)   # x 1000 ?
BBS_EI[is.na(BBS_EI)]      <- 0
BBS_EI$GPMTPREV          <- ifelse(is.infinite(BBS_EI$GPPREV*1000/BBS_EI$QprevYear),0,BBS_EI$GPPREV*1000/BBS_EI$QprevYear)
BBS_EI[is.na(BBS_EI)]      <- 0
BBS_EI$GPMTDIFF          <- BBS_EI$GPMTACT-BBS_EI$GPMTPREV
BBS_EI[is.na(BBS_EI)]      <- NA
BBS_EI$GPMTDIFFPER       <- ifelse(is.infinite((BBS_EI$GPMTACT-BBS_EI$GPMTPREV)/BBS_EI$GPMTPREV),"",scales::percent(((BBS_EI$GPMTACT-BBS_EI$GPMTPREV)/BBS_EI$GPMTPREV),accuracy = 0.1))
BBS_EI[is.na(BBS_EI)]      <- NA

DT                     <- data.table(BBS_EI)
BBS_EI                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
BBS_EI[is.na(BBS_EI)]      <- NA
BBS_EI                     <- setorder(BBS_EI,-QuantityDiffYYPER_P)
BBS_EI$QuantityDiffYYPER_P<-NULL

colnames(BBS_EI)[1]      <- "EI"

Total<-data.frame('EI'="Total CEE",
                  QActYear          = sum(BBS_EI$QActYear),
                  QprevYear         = sum(BBS_EI$QprevYear),
                  GPACT             = sum(BBS_EI$GPACT),
                  GPPREV            = sum(BBS_EI$GPPREV),
                  TURNACT           = sum(BBS_EI$TURNACT),
                  TURNPREV          = sum(BBS_EI$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(BBS_EI$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(BBS_EI$GPMTACT),
                  GPMTPREV          = sum(BBS_EI$GPMTPREV),
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

BBS_EI                   <- rbind(BBS_EI,Total)
#BBS_EI                   <- setorder(BBS_EI,EI)
#BBSM$Region              <- NULL
BBS_EI                     <- BBS_EI %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)


BBS_EI$EI        <- stri_replace_all_fixed(BBS_EI$EI, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
BBS_EI$EI        <- stri_replace_all_fixed(BBS_EI$EI, pattern = c("Industrial Sales and Services"), replacement = c("Ind. Sales and Serv."), vectorize_all = FALSE)


# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

BBS_EI <- BBS_EI %>% mutate_if(is.numeric, ~round(., 0))
BBS_EI[is.na(BBS_EI)]        <- ""
WyboldowaneWierszeBBS_EI <- c(length(BBS_EI$EI))

#Jest problem z wydzieleniem Itercompany Outside z Industrial Sales and Services.

### -------------------  BBS results - Key Businesses YTD

# Mapować kraje chociaż nie wystapiła Chorwacja&SLOVENIA
# CROATIA?   dlaczego w raporcie Asi jest a u mnie nie ma?

BBS_KEY                    <- read_xlsx(here::here("sources","DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "BBS",range="AA16:AP2600", col_names = TRUE,na = "NA")

BBS_KEY                    <- BBS_KEY%>% filter_all(any_vars(!is.na(.))) 
BBS_KEY[is.na(BBS_KEY)]    <- 0   # wszystkie pola bez wartości wypełniamy zerami


colnames(BBS_KEY)[11]<-Naglowek1
colnames(BBS_KEY)[12]<-Naglowek2
colnames(BBS_KEY)[13]<-Naglowek3
colnames(BBS_KEY)[14]<-Naglowek4
colnames(BBS_KEY)[15]<-Naglowek5
colnames(BBS_KEY)[16]<-Naglowek6
colnames(BBS_KEY)[3]<-"KeyBusiness"

BBS_KEY                  <- BBS_KEY %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)      #   PONIEWAÆ YTD

#colnames(MapowanieBranzaIndu)[1]  <-"European Industry"
#BBS_EI                    <- left_join(BBS_EI,MapowanieBranzaIndu,by="European Industry")

BBS_KEY                   <- BBS_KEY %>% select(1,3,11:16)

# BBSM<-rbind(BBSM,BBSM2)
BBS_KEY                   <- as.data.frame(BBS_KEY)
BBS_KEY                    <- BBS_KEY   %>% group_by(`Subsidiary Country`,KeyBusiness) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
BBS_KEY  [is.na(BBS_KEY )]    <- NA


BBS_KEY$QuantityDiffYY    <- BBS_KEY$QActYear-BBS_KEY$QprevYear
BBS_KEY[is.na(BBS_KEY)]      <- NA
BBS_KEY$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((BBS_KEY$QActYear-BBS_KEY$QprevYear)/BBS_KEY$QprevYear)),"",(BBS_KEY$QActYear-BBS_KEY$QprevYear)/BBS_KEY$QprevYear))
BBS_KEY$QuantityDiffYYPER <- ifelse(is.infinite(((BBS_KEY$QActYear-BBS_KEY$QprevYear)/BBS_KEY$QprevYear)),"",scales::percent(((BBS_KEY$QActYear-BBS_KEY$QprevYear)/BBS_KEY$QprevYear),accuracy = 0.1))
BBS_KEY[is.na(BBS_KEY)]      <- NA
BBS_KEY$GPDiffYY          <- BBS_KEY$GPACT-BBS_KEY$GPPREV
BBS_KEY[is.na(BBS_KEY)]      <- NA
BBS_KEY$GPDiffYYPER       <- ifelse(is.infinite(((BBS_KEY$GPACT-BBS_KEY$GPPREV)/BBS_KEY$GPPREV)),"",scales::percent(((BBS_KEY$GPACT-BBS_KEY$GPPREV)/BBS_KEY$GPPREV),accuracy = 0.1))
#BBS_KEY$GPDiffYYPER       <- round(((BBS_KEY$GPACT-BBS_KEY$GPPREV)/BBS_KEY$GPPREV)*100,2)
BBS_KEY[is.na(BBS_KEY)]      <- NA
BBS_KEY$TurnDiffYY        <- BBS_KEY$TURNACT-BBS_KEY$TURNPREV
BBS_KEY[is.na(BBS_KEY)]      <- NA
BBS_KEY$TurnDiffYYPER     <- ifelse(is.infinite(((BBS_KEY$TURNACT-BBS_KEY$TURNPREV)/BBS_KEY$TURNPREV)),"",scales::percent(((BBS_KEY$TURNACT-BBS_KEY$TURNPREV)/BBS_KEY$TURNPREV),accuracy = 0.1))
BBS_KEY[is.na(BBS_KEY)]      <- NA
BBS_KEY$GPMTACT           <- ifelse(is.infinite(BBS_KEY$GPACT*1000/BBS_KEY$QActYear),0,BBS_KEY$GPACT*1000/BBS_KEY$QActYear)   # x 1000 ?
BBS_KEY[is.na(BBS_KEY)]      <- 0
BBS_KEY$GPMTPREV          <- ifelse(is.infinite(BBS_KEY$GPPREV*1000/BBS_KEY$QprevYear),0,BBS_KEY$GPPREV*1000/BBS_KEY$QprevYear)
BBS_KEY[is.na(BBS_KEY)]      <- 0
BBS_KEY$GPMTDIFF          <- BBS_KEY$GPMTACT-BBS_KEY$GPMTPREV
BBS_KEY[is.na(BBS_KEY)]      <- NA
BBS_KEY$GPMTDIFFPER       <- ifelse(is.infinite((BBS_KEY$GPMTACT-BBS_KEY$GPMTPREV)/BBS_KEY$GPMTPREV),"",scales::percent(((BBS_KEY$GPMTACT-BBS_KEY$GPMTPREV)/BBS_KEY$GPMTPREV),accuracy = 0.1))
BBS_KEY[is.na(BBS_KEY)]      <- NA

DT                        <- data.table(BBS_KEY)
BBS_KEY                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
BBS_KEY[is.na(BBS_KEY)]      <- NA
BBS_KEY$`Subsidiary Country`<- stri_replace_all_fixed(BBS_KEY$`Subsidiary Country`, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
BBS_KEY$KeyBusiness<- stri_replace_all_fixed(BBS_KEY$KeyBusiness, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)

BBS_KEY                      <- setorder(BBS_KEY,`Subsidiary Country` ,-QuantityDiffYYPER_P)
BBS_KEY$QuantityDiffYYPER_P <-NULL

Total<-data.frame('Subsidiary Country'="Total CEE Key B.", KeyBusiness="Total CEE Key B.",
                  QActYear          = sum(BBS_KEY$QActYear),
                  QprevYear         = sum(BBS_KEY$QprevYear),
                  GPACT             = sum(BBS_KEY$GPACT),
                  GPPREV            = sum(BBS_KEY$GPPREV),
                  TURNACT           = sum(BBS_KEY$TURNACT),
                  TURNPREV          = sum(BBS_KEY$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(BBS_KEY$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(BBS_KEY$GPMTACT),
                  GPMTPREV          = sum(BBS_KEY$GPMTPREV),
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
#BBSM$Region              <- NULL

BBS_KEY[is.na(BBS_KEY)]        <- ""

Kraje<-unique(BBS_KEY$`Subsidiary Country`)
for(region in unique(BBS_KEY$`Subsidiary Country`)){

  steal <- subset(BBS_KEY,`Subsidiary Country`==region)
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
  
  colnames(frejm)[1]<-"Subsidiary Country"
  BBS_KEY <- rbind(BBS_KEY, frejm)
  
}


BBS_KEY                       <- rbind(BBS_KEY,Total)
BBS_KEY                       <- setorder(BBS_KEY ,`Subsidiary Country`)
BBS_KEY$`Subsidiary Country`               <- NULL

BBS_KEY                       <- BBS_KEY %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)

BBS_KEY <- BBS_KEY %>% mutate_if(is.numeric, ~round(., 0))
BBS_KEY[is.na(BBS_KEY)]       <- ""
WyboldowaneWierszeBBS_KEY<-c(which(BBS_KEY$KeyBusiness %in% Kraje),length(BBS_KEY$KeyBusiness))



## ----------------------------  CEE Region - Total results   (month)



MainSource                    <- read_xlsx(here::here("sources","DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "BES_BSP",range="I16:Y60000", col_names = TRUE,na = "NA")


MainSource                      <- MainSource  %>% filter_all(any_vars(!is.na(.))) 
MainSource [is.na(MainSource)]   <- 0   # wszystkie pola bez wartości wypełniamy zerami

MainSource<-left_join(MainSource,MapowanieBaltics,by="Subsidiary Country")
MainSource<-left_join(MainSource,MapowanieBESBSP,by="European Industry")
MainSource<-left_join(MainSource,MapowanieESSP,by="Material Division")



# MainTotal<-MainSource
# 
# 
# 
# colnames(MainTotal)[12]<-Naglowek1
# colnames(MainTotal)[13]<-Naglowek2
# colnames(MainTotal)[14]<-Naglowek3
# colnames(MainTotal)[15]<-Naglowek4
# colnames(MainTotal)[16]<-Naglowek5
# colnames(MainTotal)[17]<-Naglowek6
# 
# 
# 
# 
# 
# 
# 
# MainTotal                   <- MainTotal %>% filter(`Calendar month`==MiesiacAnalizy)
# MainTotal                   <- MainTotal %>% select(18,19,12,13,14,15,16,17)
# 
# # MainTotal<-rbind(MainTotal,MainTotal2)
# MainTotal                   <- as.data.frame(MainTotal)
# MainTotal                   <- MainTotal %>% group_by(Region,SubRegion) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
# MainTotal[is.na(MainTotal)]      <- NA
# MainTotal$QuantityDiffYY    <- MainTotal$QActYear-MainTotal$QprevYear
# MainTotal[is.na(MainTotal)]      <- NA
# MainTotal$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((MainTotal$QActYear-MainTotal$QprevYear)/MainTotal$QprevYear)),"",(MainTotal$QActYear-MainTotal$QprevYear)/MainTotal$QprevYear))
# MainTotal$QuantityDiffYYPER <- ifelse(is.infinite(((MainTotal$QActYear-MainTotal$QprevYear)/MainTotal$QprevYear)),"",scales::percent(((MainTotal$QActYear-MainTotal$QprevYear)/MainTotal$QprevYear),accuracy = 0.1))
# MainTotal[is.na(MainTotal)]      <- NA
# MainTotal$GPDiffYY          <- MainTotal$GPACT-MainTotal$GPPREV
# MainTotal[is.na(MainTotal)]      <- NA
# MainTotal$GPDiffYYPER       <- ifelse(is.infinite(((MainTotal$GPACT-MainTotal$GPPREV)/MainTotal$GPPREV)),"",scales::percent(((MainTotal$GPACT-MainTotal$GPPREV)/MainTotal$GPPREV),accuracy = 0.1))
# #MainTotal$GPDiffYYPER       <- round(((MainTotal$GPACT-MainTotal$GPPREV)/MainTotal$GPPREV)*100,2)
# MainTotal[is.na(MainTotal)]      <- NA
# MainTotal$TurnDiffYY        <- MainTotal$TURNACT-MainTotal$TURNPREV
# MainTotal[is.na(MainTotal)]      <- NA
# MainTotal$TurnDiffYYPER     <- ifelse(is.infinite(((MainTotal$TURNACT-MainTotal$TURNPREV)/MainTotal$TURNPREV)),"",scales::percent(((MainTotal$TURNACT-MainTotal$TURNPREV)/MainTotal$TURNPREV),accuracy = 0.1))
# MainTotal[is.na(MainTotal)]      <- NA
# MainTotal$GPMTACT           <- ifelse(is.infinite(MainTotal$GPACT*1000/MainTotal$QActYear),0,MainTotal$GPACT*1000/MainTotal$QActYear)   # x 1000 ?
# MainTotal[is.na(MainTotal)]      <- 0
# MainTotal$GPMTPREV          <- ifelse(is.infinite(MainTotal$GPPREV*1000/MainTotal$QprevYear),0,MainTotal$GPPREV*1000/MainTotal$QprevYear)
# MainTotal[is.na(MainTotal)]      <- 0
# MainTotal$GPMTDIFF          <- MainTotal$GPMTACT-MainTotal$GPMTPREV
# MainTotal[is.na(MainTotal)]      <- NA
# MainTotal$GPMTDIFFPER       <- ifelse(is.infinite((MainTotal$GPMTACT-MainTotal$GPMTPREV)/MainTotal$GPMTPREV),"",scales::percent(((MainTotal$GPMTACT-MainTotal$GPMTPREV)/MainTotal$GPMTPREV),accuracy = 0.1))
# MainTotal[is.na(MainTotal)]      <- NA
# 
# DT                     <- data.table(MainTotal)
# MainTotal                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
# MainTotal[is.na(MainTotal)]      <- NA
# MainTotal$SubRegion         <- stri_replace_all_fixed(MainTotal$SubRegion, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
# MainTotal                      <- setorder(MainTotal,Region,-QuantityDiffYYPER_P)
# MainTotal$QuantityDiffYYPER_P <-NULL
# 
# 
# 
# Total<-data.frame(Region="Total CEE", SubRegion="Total CEE",
#                   QActYear          = sum(MainTotal$QActYear),
#                   QprevYear         = sum(MainTotal$QprevYear),
#                   GPACT             = sum(MainTotal$GPACT),
#                   GPPREV            = sum(MainTotal$GPPREV),
#                   TURNACT           = sum(MainTotal$TURNACT),
#                   TURNPREV          = sum(MainTotal$TURNPREV),
#                   QuantityDiffYY    = 0,
#                   QuantityDiffYYPER = 0,
#                   GPDiffYYPER       = 0,
#                   GPDiffYY          = 0,
#                   TurnDiffYY        = sum(MainTotal$TurnDiffYY),
#                   TurnDiffYYPER     = 0,
#                   GPMTACT           = 0,
#                   GPMTPREV          = 0,
#                   GPMTDIFF          = 0,
#                   GPMTDIFFPER       = 0)            
# 
# 
# Total$GPMTACT            <-  Total$GPACT*1000/Total$QActYear
# Total$GPMTPREV           <-  Total$GPPREV*1000/Total$QprevYear
# Total$TurnDiffYY         <-  Total$TURNACT-Total$TURNPREV
# Total$QuantityDiffYY     <-  Total$QActYear-Total$QprevYear
# Total$GPDiffYY           <-  Total$GPACT-Total$GPPREV
# Total$GPMTDIFF           <-  Total$GPMTACT-Total$GPMTPREV
# Total$QuantityDiffYYPER  <-  scales::percent(Total$QuantityDiffYY/Total$QprevYear,accuracy = 0.1)
# Total$GPDiffYYPER        <-  scales::percent(Total$GPDiffYY/Total$GPPREV,accuracy = 0.1)
# Total$TurnDiffYYPER      <-  scales::percent(Total$TurnDiffYY/Total$TURNPREV,accuracy = 0.1)
# Total$GPMTDIFFPER        <-  scales::percent(Total$GPMTDIFF/Total$GPMTPREV,accuracy = 0.1)
# 
# 
# Regiony<-unique(MainTotal$Region)
# 
# for(region in unique(MainTotal$Region)){
#   
#   steal <- subset(MainTotal,Region==region)
#   frejm <- data.frame(Region=region, SubRegion=region, 
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
#                       GPMTACT           = 0,
#                       GPMTPREV          = 0,
#                       GPMTDIFF          = 0,
#                       GPMTDIFFPER       = 0)
#   
#   
#   
#   frejm$GPMTACT            <-  frejm$GPACT*1000/frejm$QActYear
#   frejm$GPMTPREV           <-  frejm$GPPREV*1000/frejm$QprevYear
#   frejm$TurnDiffYY         <-  frejm$TURNACT-frejm$TURNPREV
#   frejm$QuantityDiffYY     <-  frejm$QActYear-frejm$QprevYear
#   frejm$GPDiffYY           <-  frejm$GPACT-frejm$GPPREV
#   frejm$GPMTDIFF           <-  frejm$GPMTACT-frejm$GPMTPREV
#   frejm$QuantityDiffYYPER  <-  scales::percent(frejm$QuantityDiffYY/frejm$QprevYear,accuracy = 0.1)
#   frejm$GPDiffYYPER        <-  scales::percent(frejm$GPDiffYY/frejm$GPPREV,accuracy = 0.1)
#   frejm$TurnDiffYYPER      <-  scales::percent(frejm$TurnDiffYY/frejm$TURNPREV,accuracy = 0.1)
#   frejm$GPMTDIFFPER        <-  scales::percent(frejm$GPMTDIFF/frejm$GPMTPREV,accuracy = 0.1)
#   
#   
#   MainTotal <- rbind(MainTotal, frejm)
#   
# }
# 
# MainTotal                      <- rbind(MainTotal,Total)
# MainTotal                      <- setorder(MainTotal,Region)
# WyboldowaneWierszeMainTotal <- c(which(MainTotal$SubRegion %in% Regiony),length(MainTotal$Region))
# MainTotal$Region               <- NULL
# MainTotal                      <- MainTotal %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
# MainTotal[is.na(MainTotal)]    <- ""
# 
# 
# 
# # formatowanie liczby do liczby znaków   !!!! MEGAAAAA
# 
# MainTotal          <- MainTotal %>% mutate_if(is.numeric, ~round(., 0)) 
# 
# ## ----------------------------  CEE Region - Total results   (month)
# 
# WyboldowaneWierszeTOTAL    <- c(which(MainTotal$SubRegion %in% Regiony),length(MainTotal$SubRegion))



MainTotalYTD<-MainSource


colnames(MainTotalYTD)[12]<-Naglowek1
colnames(MainTotalYTD)[13]<-Naglowek2
colnames(MainTotalYTD)[14]<-Naglowek3
colnames(MainTotalYTD)[15]<-Naglowek4
colnames(MainTotalYTD)[16]<-Naglowek5
colnames(MainTotalYTD)[17]<-Naglowek6

MainTotalYTD                   <- MainTotalYTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)   
MainTotalYTD                   <- MainTotalYTD %>% select(18,19,12,13,14,15,16,17)




# MainTotalYTD<-rbind(MainTotalYTD,MainTotalYTD2)
MainTotalYTD                   <- as.data.frame(MainTotalYTD)
MainTotalYTD                   <- MainTotalYTD %>% group_by(Region,SubRegion) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
MainTotalYTD[is.na(MainTotalYTD)]      <- NA
MainTotalYTD$QuantityDiffYY    <- MainTotalYTD$QActYear-MainTotalYTD$QprevYear
MainTotalYTD[is.na(MainTotalYTD)]      <- NA
MainTotalYTD$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((MainTotalYTD$QActYear-MainTotalYTD$QprevYear)/MainTotalYTD$QprevYear)),"",(MainTotalYTD$QActYear-MainTotalYTD$QprevYear)/MainTotalYTD$QprevYear))
MainTotalYTD$QuantityDiffYYPER <- ifelse(is.infinite(((MainTotalYTD$QActYear-MainTotalYTD$QprevYear)/MainTotalYTD$QprevYear)),"",scales::percent(((MainTotalYTD$QActYear-MainTotalYTD$QprevYear)/MainTotalYTD$QprevYear),accuracy = 0.1))
MainTotalYTD[is.na(MainTotalYTD)]      <- NA
MainTotalYTD$GPDiffYY          <- MainTotalYTD$GPACT-MainTotalYTD$GPPREV
MainTotalYTD[is.na(MainTotalYTD)]      <- NA
MainTotalYTD$GPDiffYYPER       <- ifelse(is.infinite(((MainTotalYTD$GPACT-MainTotalYTD$GPPREV)/MainTotalYTD$GPPREV)),"",scales::percent(((MainTotalYTD$GPACT-MainTotalYTD$GPPREV)/MainTotalYTD$GPPREV),accuracy = 0.1))
#MainTotalYTD$GPDiffYYPER       <- round(((MainTotalYTD$GPACT-MainTotalYTD$GPPREV)/MainTotalYTD$GPPREV)*100,2)
MainTotalYTD[is.na(MainTotalYTD)]      <- NA
MainTotalYTD$TurnDiffYY        <- MainTotalYTD$TURNACT-MainTotalYTD$TURNPREV
MainTotalYTD[is.na(MainTotalYTD)]      <- NA
MainTotalYTD$TurnDiffYYPER     <- ifelse(is.infinite(((MainTotalYTD$TURNACT-MainTotalYTD$TURNPREV)/MainTotalYTD$TURNPREV)),"",scales::percent(((MainTotalYTD$TURNACT-MainTotalYTD$TURNPREV)/MainTotalYTD$TURNPREV),accuracy = 0.1))
MainTotalYTD[is.na(MainTotalYTD)]      <- NA
MainTotalYTD$GPMTACT           <- ifelse(is.infinite(MainTotalYTD$GPACT*1000/MainTotalYTD$QActYear),0,MainTotalYTD$GPACT*1000/MainTotalYTD$QActYear)   # x 1000 ?
MainTotalYTD[is.na(MainTotalYTD)]      <- 0
MainTotalYTD$GPMTPREV          <- ifelse(is.infinite(MainTotalYTD$GPPREV*1000/MainTotalYTD$QprevYear),0,MainTotalYTD$GPPREV*1000/MainTotalYTD$QprevYear)
MainTotalYTD[is.na(MainTotalYTD)]      <- 0
MainTotalYTD$GPMTDIFF          <- MainTotalYTD$GPMTACT-MainTotalYTD$GPMTPREV
MainTotalYTD[is.na(MainTotalYTD)]      <- NA
MainTotalYTD$GPMTDIFFPER       <- ifelse(is.infinite((MainTotalYTD$GPMTACT-MainTotalYTD$GPMTPREV)/MainTotalYTD$GPMTPREV),"",scales::percent(((MainTotalYTD$GPMTACT-MainTotalYTD$GPMTPREV)/MainTotalYTD$GPMTPREV),accuracy = 0.1))
MainTotalYTD[is.na(MainTotalYTD)]      <- NA

DT                                <- data.table(MainTotalYTD)
MainTotalYTD                      <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
MainTotalYTD[is.na(MainTotalYTD)] <- NA
MainTotalYTD$SubRegion            <- stri_replace_all_fixed(MainTotalYTD$SubRegion, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
MainTotalYTD                      <- setorder(MainTotalYTD,Region,-QuantityDiffYYPER_P)
MainTotalYTD$QuantityDiffYYPER_P  <- NULL


Total <- data.frame(
  Region = "Total CEE",
  SubRegion = "Total CEE",
  QActYear          = sum(MainTotalYTD$QActYear),
  QprevYear         = sum(MainTotalYTD$QprevYear),
  GPACT             = sum(MainTotalYTD$GPACT),
  GPPREV            = sum(MainTotalYTD$GPPREV),
  TURNACT           = sum(MainTotalYTD$TURNACT),
  TURNPREV          = sum(MainTotalYTD$TURNPREV),
  QuantityDiffYY    = 0,
  QuantityDiffYYPER = 0,
  GPDiffYYPER       = 0,
  GPDiffYY          = 0,
  TurnDiffYY        = sum(MainTotalYTD$TurnDiffYY),
  TurnDiffYYPER     = 0,
  GPMTACT           = sum(MainTotalYTD$GPMTACT),
  GPMTPREV          = sum(MainTotalYTD$GPMTPREV),
  GPMTDIFF          = 0,
  GPMTDIFFPER       = 0
)

Total$GPMTACT            <-  Total$GPACT*1000/Total$QActYear
Total$GPMTPREV           <-  Total$GPPREV*1000/Total$QprevYear
Total$TurnDiffYY         <-  Total$TURNACT - Total$TURNPREV
Total$QuantityDiffYY     <-  Total$QActYear - Total$QprevYear
Total$GPDiffYY           <-  Total$GPACT - Total$GPPREV
Total$GPMTDIFF           <-  Total$GPMTACT - Total$GPMTPREV
Total$QuantityDiffYYPER  <-
  scales::percent(Total$QuantityDiffYY / Total$QprevYear, accuracy = 0.1)
Total$GPDiffYYPER        <-
  scales::percent(Total$GPDiffYY / Total$GPPREV, accuracy = 0.1)
Total$TurnDiffYYPER      <-
  scales::percent(Total$TurnDiffYY / Total$TURNPREV, accuracy = 0.1)
Total$GPMTDIFFPER        <-
  scales::percent(Total$GPMTDIFF / Total$GPMTPREV, accuracy = 0.1)


Regiony <- unique(MainTotalYTD$Region)

for (region in unique(MainTotalYTD$Region)) {
  steal <- subset(MainTotalYTD, Region == region)
  frejm <- data.frame(
    Region = region,
    SubRegion = region,
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
    GPMTDIFFPER       = 0
  )
  frejm$GPMTACT            <-  frejm$GPACT*1000/frejm$QActYear
  frejm$GPMTPREV           <-  frejm$GPPREV*1000/frejm$QprevYear
  frejm$TurnDiffYY         <-  frejm$TURNACT - frejm$TURNPREV
  frejm$QuantityDiffYY     <-  frejm$QActYear - frejm$QprevYear
  frejm$GPDiffYY           <-  frejm$GPACT - frejm$GPPREV
  frejm$GPMTDIFF           <-  frejm$GPMTACT - frejm$GPMTPREV
  frejm$QuantityDiffYYPER  <-
    scales::percent(frejm$QuantityDiffYY / frejm$QprevYear, accuracy = 0.1)
  frejm$GPDiffYYPER        <-
    scales::percent(frejm$GPDiffYY / frejm$GPPREV, accuracy = 0.1)
  frejm$TurnDiffYYPER      <-
    scales::percent(frejm$TurnDiffYY / frejm$TURNPREV, accuracy = 0.1)
  frejm$GPMTDIFFPER        <-
    scales::percent(frejm$GPMTDIFF / frejm$GPMTPREV, accuracy = 0.1)
  
  
  MainTotalYTD <- rbind(MainTotalYTD, frejm)
  
}

MainTotalYTD                      <- rbind(MainTotalYTD,Total)
MainTotalYTD                      <- setorder(MainTotalYTD,Region)
WyboldowaneWierszeMainTotalYTD    <- c(which(MainTotalYTD$SubRegion %in% Regiony),length(MainTotalYTD$Region))
MainTotalYTD$Region               <- NULL
MainTotalYTD                      <- MainTotalYTD %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
MainTotalYTD[is.na(MainTotalYTD)] <- ""


# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

MainTotalYTD          <- MainTotalYTD %>% mutate_if(is.numeric, ~round(., 0)) 


WyboldowaneWierszeTOTALYTD    <- c(which(MainTotalYTD$SubRegion %in% Regiony),length(MainTotalYTD$SubRegion))

#source(here::here("moduly","zbuduj_baze.R"))

















# --------------------  CEE Region - Results by Country ES / SP Material Type    (month)


# 3 poziomy grupowania
# 1. Zaczynamy od 2 poziomów, doczepianie regionów zostawiamy na końcu.
# 2. Grupujemy Subregion, MapowanieMd


MAIN_ESSP<-MainSource

colnames(MAIN_ESSP)[12]<-Naglowek1
colnames(MAIN_ESSP)[13]<-Naglowek2
colnames(MAIN_ESSP)[14]<-Naglowek3
colnames(MAIN_ESSP)[15]<-Naglowek4
colnames(MAIN_ESSP)[16]<-Naglowek5
colnames(MAIN_ESSP)[17]<-Naglowek6

MAIN_ESSP                   <- MAIN_ESSP %>% filter(`Calendar month`==MiesiacAnalizy)
MAIN_ESSP                   <- MAIN_ESSP %>% select(18,19,21,12,13,14,15,16,17)

# MAIN_ESSP <- rbind(MAIN_ESSP,MAIN_ESSP2)
MAIN_ESSP                        <- as.data.frame(MAIN_ESSP)
MAIN_ESSP                        <- MAIN_ESSP %>% group_by(Region,SubRegion,MapowanieMD) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
MAIN_ESSP[is.na(MAIN_ESSP)]      <- NA
MAIN_ESSP$QuantityDiffYY         <- MAIN_ESSP$QActYear-MAIN_ESSP$QprevYear
MAIN_ESSP[is.na(MAIN_ESSP)]      <- NA
MAIN_ESSP$QuantityDiffYYPER_P    <- as.numeric(ifelse(is.infinite(((MAIN_ESSP$QActYear-MAIN_ESSP$QprevYear)/MAIN_ESSP$QprevYear)),"",(MAIN_ESSP$QActYear-MAIN_ESSP$QprevYear)/MAIN_ESSP$QprevYear))
MAIN_ESSP$QuantityDiffYYPER      <- ifelse(is.infinite(((MAIN_ESSP$QActYear-MAIN_ESSP$QprevYear)/MAIN_ESSP$QprevYear)),"",scales::percent(((MAIN_ESSP$QActYear-MAIN_ESSP$QprevYear)/MAIN_ESSP$QprevYear),accuracy = 0.1))
MAIN_ESSP[is.na(MAIN_ESSP)]      <- NA
MAIN_ESSP$GPDiffYY               <- MAIN_ESSP$GPACT-MAIN_ESSP$GPPREV
MAIN_ESSP[is.na(MAIN_ESSP)]      <- NA
MAIN_ESSP$GPDiffYYPER            <- ifelse(is.infinite(((MAIN_ESSP$GPACT-MAIN_ESSP$GPPREV)/MAIN_ESSP$GPPREV)),"",scales::percent(((MAIN_ESSP$GPACT-MAIN_ESSP$GPPREV)/MAIN_ESSP$GPPREV),accuracy = 0.1))
#MAIN_ESSP$GPDiffYYPER       <- round(((MAIN_ESSP$GPACT-MAIN_ESSP$GPPREV)/MAIN_ESSP$GPPREV)*100,2)
MAIN_ESSP[is.na(MAIN_ESSP)]      <- NA
MAIN_ESSP$TurnDiffYY             <- MAIN_ESSP$TURNACT-MAIN_ESSP$TURNPREV
MAIN_ESSP[is.na(MAIN_ESSP)]      <- NA
MAIN_ESSP$TurnDiffYYPER          <- ifelse(is.infinite(((MAIN_ESSP$TURNACT-MAIN_ESSP$TURNPREV)/MAIN_ESSP$TURNPREV)),"",scales::percent(((MAIN_ESSP$TURNACT-MAIN_ESSP$TURNPREV)/MAIN_ESSP$TURNPREV),accuracy = 0.1))
MAIN_ESSP[is.na(MAIN_ESSP)]      <- NA
MAIN_ESSP$GPMTACT                <- ifelse(is.infinite(MAIN_ESSP$GPACT*1000/MAIN_ESSP$QActYear),0,MAIN_ESSP$GPACT*1000/MAIN_ESSP$QActYear)   # x 1000 ?
MAIN_ESSP[is.na(MAIN_ESSP)]      <- 0
MAIN_ESSP$GPMTPREV               <- ifelse(is.infinite(MAIN_ESSP$GPPREV*1000/MAIN_ESSP$QprevYear),0,MAIN_ESSP$GPPREV*1000/MAIN_ESSP$QprevYear)
MAIN_ESSP[is.na(MAIN_ESSP)]      <- 0
MAIN_ESSP$GPMTDIFF               <- MAIN_ESSP$GPMTACT-MAIN_ESSP$GPMTPREV
MAIN_ESSP[is.na(MAIN_ESSP)]      <- NA
MAIN_ESSP$GPMTDIFFPER            <- ifelse(is.infinite((MAIN_ESSP$GPMTACT-MAIN_ESSP$GPMTPREV)/MAIN_ESSP$GPMTPREV),"",scales::percent(((MAIN_ESSP$GPMTACT-MAIN_ESSP$GPMTPREV)/MAIN_ESSP$GPMTPREV),accuracy = 0.1))
MAIN_ESSP[is.na(MAIN_ESSP)]      <- NA

DT                               <- data.table(MAIN_ESSP)
MAIN_ESSP                        <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
MAIN_ESSP[is.na(MAIN_ESSP)]      <- NA
MAIN_ESSP$SubRegion              <- stri_replace_all_fixed(MAIN_ESSP$SubRegion, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
MAIN_ESSP                        <- setorder(MAIN_ESSP,Region,SubRegion,-QActYear)
MAIN_ESSP$QuantityDiffYYPER_P    <- NULL

thereofCEE <-MAIN_ESSP %>% group_by(MapowanieMD) %>% summarize(QActYear=sum(QActYear),QprevYear=sum(QprevYear),GPACT=sum(GPACT),GPPREV=sum(GPPREV),TURNACT=sum(TURNACT),TURNPREV=sum(TURNPREV))

thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$QuantityDiffYY         <- thereofCEE$QActYear-thereofCEE$QprevYear
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$QuantityDiffYYPER_P    <- as.numeric(ifelse(is.infinite(((thereofCEE$QActYear-thereofCEE$QprevYear)/thereofCEE$QprevYear)),"",(thereofCEE$QActYear-thereofCEE$QprevYear)/thereofCEE$QprevYear))
thereofCEE$QuantityDiffYYPER      <- ifelse(is.infinite(((thereofCEE$QActYear-thereofCEE$QprevYear)/thereofCEE$QprevYear)),"",scales::percent(((thereofCEE$QActYear-thereofCEE$QprevYear)/thereofCEE$QprevYear),accuracy = 0.1))
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$GPDiffYY               <- thereofCEE$GPACT-thereofCEE$GPPREV
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$GPDiffYYPER            <- ifelse(is.infinite(((thereofCEE$GPACT-thereofCEE$GPPREV)/thereofCEE$GPPREV)),"",scales::percent(((thereofCEE$GPACT-thereofCEE$GPPREV)/thereofCEE$GPPREV),accuracy = 0.1))
#thereofCEE$GPDiffYYPER       <- round(((thereofCEE$GPACT-thereofCEE$GPPREV)/thereofCEE$GPPREV)*100,2)
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$TurnDiffYY             <- thereofCEE$TURNACT-thereofCEE$TURNPREV
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$TurnDiffYYPER          <- ifelse(is.infinite(((thereofCEE$TURNACT-thereofCEE$TURNPREV)/thereofCEE$TURNPREV)),"",scales::percent(((thereofCEE$TURNACT-thereofCEE$TURNPREV)/thereofCEE$TURNPREV),accuracy = 0.1))
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$GPMTACT                <- ifelse(is.infinite(thereofCEE$GPACT*1000/thereofCEE$QActYear),0,thereofCEE$GPACT*1000/thereofCEE$QActYear)   # x 1000 ?
thereofCEE[is.na(thereofCEE)]      <- 0
thereofCEE$GPMTPREV               <- ifelse(is.infinite(thereofCEE$GPPREV*1000/thereofCEE$QprevYear),0,thereofCEE$GPPREV*1000/thereofCEE$QprevYear)
thereofCEE[is.na(thereofCEE)]      <- 0
thereofCEE$GPMTDIFF               <- thereofCEE$GPMTACT-thereofCEE$GPMTPREV
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$GPMTDIFFPER            <- ifelse(is.infinite((thereofCEE$GPMTACT-thereofCEE$GPMTPREV)/thereofCEE$GPMTPREV),"",scales::percent(((thereofCEE$GPMTACT-thereofCEE$GPMTPREV)/thereofCEE$GPMTPREV),accuracy = 0.1))
thereofCEE[is.na(thereofCEE)]      <- NA

DT                               <- data.table(thereofCEE)
thereofCEE                        <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$SubRegion              <- ""
thereofCEE$SubRegion[1]              <- "thereof"
thereofCEE                        <- setorder(thereofCEE,-QActYear)
thereofCEE$QuantityDiffYYPER_P    <- NULL

thereofCEE$Region<-""

thereofCEE    <- thereofCEE %>% select(19,18,1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
thereofCEE    <- thereofCEE %>% mutate_if(is.numeric, ~round(., 0)) 





RegionyCEE <- unique(MAIN_ESSP$Region)


Total <- data.frame(Region="Total CEE",SubRegion="Total CEE", MapowanieMD="Total CEE thereof",
                  QActYear          = sum(MAIN_ESSP$QActYear),
                  QprevYear         = sum(MAIN_ESSP$QprevYear),
                  GPACT             = sum(MAIN_ESSP$GPACT),
                  GPPREV            = sum(MAIN_ESSP$GPPREV),
                  TURNACT           = sum(MAIN_ESSP$TURNACT),
                  TURNPREV          = sum(MAIN_ESSP$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(MAIN_ESSP$TurnDiffYY),
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

Regiony <- unique(MAIN_ESSP$SubRegion)

for(region in unique(MAIN_ESSP$SubRegion)){
  steal <- subset(MAIN_ESSP,SubRegion==region)
  frejm <- data.frame(Region=NA,SubRegion=region, MapowanieMD=region,
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
  MAIN_ESSP <- rbind(MAIN_ESSP, frejm)
  
}

#GRUPOWANIE NORTH SOUTH CEE

for(region in RegionyCEE){
  
  steal <- subset(MAIN_ESSP,Region==region)
  frejm <- data.frame(Region=region,SubRegion=paste("TOTAL", region), MapowanieMD=paste("TOTAL",region),
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
  MAIN_ESSP <- rbind(MAIN_ESSP, frejm)
  
}

MapowaniePomocnicze            <- MapowanieBaltics
MapowaniePomocnicze$SubRegion  <- stri_replace_all_fixed(MapowaniePomocnicze$SubRegion , pattern = c("&"), replacement = c(""), vectorize_all = FALSE)

MapowaniePomocnicze            <- MapowaniePomocnicze %>% select(2,3) %>% distinct
colnames(MapowaniePomocnicze)[1] <- "RegionU"

MAIN_ESSP                      <- left_join(MAIN_ESSP,MapowaniePomocnicze,by="SubRegion")
MAIN_ESSP$Region               <- ifelse(is.na(MAIN_ESSP$Region),MAIN_ESSP$RegionU,MAIN_ESSP$Region)
MAIN_ESSP$RegionU              <- NULL

MAIN_ESSP                      <- setorder(MAIN_ESSP,Region,SubRegion)
MAIN_ESSP                      <- rbind(MAIN_ESSP,Total)

WyboldowaneWierszeMAIN_ESSP    <- c(which(MAIN_ESSP$MapowanieMD %in% Regiony),length(MAIN_ESSP$Region))
WyboldowaneWierszeMAIN_ESSP    <- c(which(str_sub(MAIN_ESSP$MapowanieMD,7,9) == "CEE"),WyboldowaneWierszeMAIN_ESSP)

# MAIN_ESSP         <- rbind(MAIN_ESSP,thereofCEE) na razie tą część tabelki trzeba odpóścić, do znalezienia rozwiązania


MAIN_ESSP$Region               <- NULL
MAIN_ESSP$SubRegion            <- NULL

MAIN_ESSP                      <- MAIN_ESSP %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)



# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

MAIN_ESSP          <- MAIN_ESSP %>% mutate_if(is.numeric, ~round(., 0)) 
MAIN_ESSP[is.na(MAIN_ESSP)]    <- ""


# --------------------  CEE Region - Results by Country ES / SP Material Type    (month)


# 3 poziomy grupowania
# 1. Zaczynamy od 2 poziomów, doczepianie regionów zostawiamy na końcu.
# 2. Grupujemy Subregion, MapowanieMd


MAIN_ESSP_YTD<-MainSource

colnames(MAIN_ESSP_YTD)[12]<-Naglowek1
colnames(MAIN_ESSP_YTD)[13]<-Naglowek2
colnames(MAIN_ESSP_YTD)[14]<-Naglowek3
colnames(MAIN_ESSP_YTD)[15]<-Naglowek4
colnames(MAIN_ESSP_YTD)[16]<-Naglowek5
colnames(MAIN_ESSP_YTD)[17]<-Naglowek6

MAIN_ESSP_YTD                   <- MAIN_ESSP_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
MAIN_ESSP_YTD                   <- MAIN_ESSP_YTD %>% select(18,19,21,12,13,14,15,16,17)

# MAIN_ESSP_YTD <- rbind(MAIN_ESSP_YTD,MAIN_ESSP_YTD2)
MAIN_ESSP_YTD                        <- as.data.frame(MAIN_ESSP_YTD)
MAIN_ESSP_YTD                        <- MAIN_ESSP_YTD %>% group_by(Region,SubRegion,MapowanieMD) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
MAIN_ESSP_YTD[is.na(MAIN_ESSP_YTD)]      <- NA
MAIN_ESSP_YTD$QuantityDiffYY         <- MAIN_ESSP_YTD$QActYear-MAIN_ESSP_YTD$QprevYear
MAIN_ESSP_YTD[is.na(MAIN_ESSP_YTD)]      <- NA
MAIN_ESSP_YTD$QuantityDiffYYPER_P    <- as.numeric(ifelse(is.infinite(((MAIN_ESSP_YTD$QActYear-MAIN_ESSP_YTD$QprevYear)/MAIN_ESSP_YTD$QprevYear)),"",(MAIN_ESSP_YTD$QActYear-MAIN_ESSP_YTD$QprevYear)/MAIN_ESSP_YTD$QprevYear))
MAIN_ESSP_YTD$QuantityDiffYYPER      <- ifelse(is.infinite(((MAIN_ESSP_YTD$QActYear-MAIN_ESSP_YTD$QprevYear)/MAIN_ESSP_YTD$QprevYear)),"",scales::percent(((MAIN_ESSP_YTD$QActYear-MAIN_ESSP_YTD$QprevYear)/MAIN_ESSP_YTD$QprevYear),accuracy = 0.1))
MAIN_ESSP_YTD[is.na(MAIN_ESSP_YTD)]      <- NA
MAIN_ESSP_YTD$GPDiffYY               <- MAIN_ESSP_YTD$GPACT-MAIN_ESSP_YTD$GPPREV
MAIN_ESSP_YTD[is.na(MAIN_ESSP_YTD)]      <- NA
MAIN_ESSP_YTD$GPDiffYYPER            <- ifelse(is.infinite(((MAIN_ESSP_YTD$GPACT-MAIN_ESSP_YTD$GPPREV)/MAIN_ESSP_YTD$GPPREV)),"",scales::percent(((MAIN_ESSP_YTD$GPACT-MAIN_ESSP_YTD$GPPREV)/MAIN_ESSP_YTD$GPPREV),accuracy = 0.1))
#MAIN_ESSP_YTD$GPDiffYYPER       <- round(((MAIN_ESSP_YTD$GPACT-MAIN_ESSP_YTD$GPPREV)/MAIN_ESSP_YTD$GPPREV)*100,2)
MAIN_ESSP_YTD[is.na(MAIN_ESSP_YTD)]      <- NA
MAIN_ESSP_YTD$TurnDiffYY             <- MAIN_ESSP_YTD$TURNACT-MAIN_ESSP_YTD$TURNPREV
MAIN_ESSP_YTD[is.na(MAIN_ESSP_YTD)]      <- NA
MAIN_ESSP_YTD$TurnDiffYYPER          <- ifelse(is.infinite(((MAIN_ESSP_YTD$TURNACT-MAIN_ESSP_YTD$TURNPREV)/MAIN_ESSP_YTD$TURNPREV)),"",scales::percent(((MAIN_ESSP_YTD$TURNACT-MAIN_ESSP_YTD$TURNPREV)/MAIN_ESSP_YTD$TURNPREV),accuracy = 0.1))
MAIN_ESSP_YTD[is.na(MAIN_ESSP_YTD)]      <- NA
MAIN_ESSP_YTD$GPMTACT                <- ifelse(is.infinite(MAIN_ESSP_YTD$GPACT*1000/MAIN_ESSP_YTD$QActYear),0,MAIN_ESSP_YTD$GPACT*1000/MAIN_ESSP_YTD$QActYear)   # x 1000 ?
MAIN_ESSP_YTD[is.na(MAIN_ESSP_YTD)]      <- 0
MAIN_ESSP_YTD$GPMTPREV               <- ifelse(is.infinite(MAIN_ESSP_YTD$GPPREV*1000/MAIN_ESSP_YTD$QprevYear),0,MAIN_ESSP_YTD$GPPREV*1000/MAIN_ESSP_YTD$QprevYear)
MAIN_ESSP_YTD[is.na(MAIN_ESSP_YTD)]      <- 0
MAIN_ESSP_YTD$GPMTDIFF               <- MAIN_ESSP_YTD$GPMTACT-MAIN_ESSP_YTD$GPMTPREV
MAIN_ESSP_YTD[is.na(MAIN_ESSP_YTD)]      <- NA
MAIN_ESSP_YTD$GPMTDIFFPER            <- ifelse(is.infinite((MAIN_ESSP_YTD$GPMTACT-MAIN_ESSP_YTD$GPMTPREV)/MAIN_ESSP_YTD$GPMTPREV),"",scales::percent(((MAIN_ESSP_YTD$GPMTACT-MAIN_ESSP_YTD$GPMTPREV)/MAIN_ESSP_YTD$GPMTPREV),accuracy = 0.1))
MAIN_ESSP_YTD[is.na(MAIN_ESSP_YTD)]      <- NA

DT                               <- data.table(MAIN_ESSP_YTD)
MAIN_ESSP_YTD                        <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
MAIN_ESSP_YTD[is.na(MAIN_ESSP_YTD)]      <- NA
MAIN_ESSP_YTD$SubRegion              <- stri_replace_all_fixed(MAIN_ESSP_YTD$SubRegion, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
MAIN_ESSP_YTD                        <- setorder(MAIN_ESSP_YTD,Region,SubRegion,-QActYear)
MAIN_ESSP_YTD$QuantityDiffYYPER_P    <- NULL

thereofCEE <-MAIN_ESSP_YTD %>% group_by(MapowanieMD) %>% summarize(QActYear=sum(QActYear),QprevYear=sum(QprevYear),GPACT=sum(GPACT),GPPREV=sum(GPPREV),TURNACT=sum(TURNACT),TURNPREV=sum(TURNPREV))

thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$QuantityDiffYY         <- thereofCEE$QActYear-thereofCEE$QprevYear
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$QuantityDiffYYPER_P    <- as.numeric(ifelse(is.infinite(((thereofCEE$QActYear-thereofCEE$QprevYear)/thereofCEE$QprevYear)),"",(thereofCEE$QActYear-thereofCEE$QprevYear)/thereofCEE$QprevYear))
thereofCEE$QuantityDiffYYPER      <- ifelse(is.infinite(((thereofCEE$QActYear-thereofCEE$QprevYear)/thereofCEE$QprevYear)),"",scales::percent(((thereofCEE$QActYear-thereofCEE$QprevYear)/thereofCEE$QprevYear),accuracy = 0.1))
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$GPDiffYY               <- thereofCEE$GPACT-thereofCEE$GPPREV
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$GPDiffYYPER            <- ifelse(is.infinite(((thereofCEE$GPACT-thereofCEE$GPPREV)/thereofCEE$GPPREV)),"",scales::percent(((thereofCEE$GPACT-thereofCEE$GPPREV)/thereofCEE$GPPREV),accuracy = 0.1))
#thereofCEE$GPDiffYYPER       <- round(((thereofCEE$GPACT-thereofCEE$GPPREV)/thereofCEE$GPPREV)*100,2)
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$TurnDiffYY             <- thereofCEE$TURNACT-thereofCEE$TURNPREV
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$TurnDiffYYPER          <- ifelse(is.infinite(((thereofCEE$TURNACT-thereofCEE$TURNPREV)/thereofCEE$TURNPREV)),"",scales::percent(((thereofCEE$TURNACT-thereofCEE$TURNPREV)/thereofCEE$TURNPREV),accuracy = 0.1))
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$GPMTACT                <- ifelse(is.infinite(thereofCEE$GPACT*1000/thereofCEE$QActYear),0,thereofCEE$GPACT*1000/thereofCEE$QActYear)   # x 1000 ?
thereofCEE[is.na(thereofCEE)]      <- 0
thereofCEE$GPMTPREV               <- ifelse(is.infinite(thereofCEE$GPPREV*1000/thereofCEE$QprevYear),0,thereofCEE$GPPREV*1000/thereofCEE$QprevYear)
thereofCEE[is.na(thereofCEE)]      <- 0
thereofCEE$GPMTDIFF               <- thereofCEE$GPMTACT-thereofCEE$GPMTPREV
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$GPMTDIFFPER            <- ifelse(is.infinite((thereofCEE$GPMTACT-thereofCEE$GPMTPREV)/thereofCEE$GPMTPREV),"",scales::percent(((thereofCEE$GPMTACT-thereofCEE$GPMTPREV)/thereofCEE$GPMTPREV),accuracy = 0.1))
thereofCEE[is.na(thereofCEE)]      <- NA

DT                               <- data.table(thereofCEE)
thereofCEE                        <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
thereofCEE[is.na(thereofCEE)]      <- NA
thereofCEE$SubRegion              <- ""
thereofCEE$SubRegion[1]              <- "thereof"
thereofCEE                        <- setorder(thereofCEE,-QActYear)
thereofCEE$QuantityDiffYYPER_P    <- NULL

thereofCEE$Region<-""


thereofCEE    <- thereofCEE %>% select(19,18,1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
thereofCEE    <- thereofCEE %>% mutate_if(is.numeric, ~round(., 0)) 






RegionyCEE <- unique(MAIN_ESSP_YTD$Region)


Total <- data.frame(Region="Total CEE",SubRegion="Total CEE", MapowanieMD="Total CEE thereof",
                    QActYear          = sum(MAIN_ESSP_YTD$QActYear),
                    QprevYear         = sum(MAIN_ESSP_YTD$QprevYear),
                    GPACT             = sum(MAIN_ESSP_YTD$GPACT),
                    GPPREV            = sum(MAIN_ESSP_YTD$GPPREV),
                    TURNACT           = sum(MAIN_ESSP_YTD$TURNACT),
                    TURNPREV          = sum(MAIN_ESSP_YTD$TURNPREV),
                    QuantityDiffYY    = 0,
                    QuantityDiffYYPER = 0,
                    GPDiffYYPER       = 0,
                    GPDiffYY          = 0,
                    TurnDiffYY        = sum(MAIN_ESSP_YTD$TurnDiffYY),
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

Regiony <- unique(MAIN_ESSP_YTD$SubRegion)

for(region in unique(MAIN_ESSP_YTD$SubRegion)){
  steal <- subset(MAIN_ESSP_YTD,SubRegion==region)
  frejm <- data.frame(Region=NA,SubRegion=region, MapowanieMD=region,
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
  MAIN_ESSP_YTD <- rbind(MAIN_ESSP_YTD, frejm)
  
}

#GRUPOWANIE NORTH SOUTH CEE



for(region in RegionyCEE){
  
  steal <- subset(MAIN_ESSP_YTD,Region==region)
  frejm <- data.frame(Region=region,SubRegion=paste("TOTAL", region), MapowanieMD=paste("TOTAL",region),
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
  MAIN_ESSP_YTD            <- rbind(MAIN_ESSP_YTD, frejm)
  
}



MapowaniePomocnicze            <- MapowanieBaltics
MapowaniePomocnicze$SubRegion  <- stri_replace_all_fixed(MapowaniePomocnicze$SubRegion , pattern = c("&"), replacement = c(""), vectorize_all = FALSE)

MapowaniePomocnicze            <- MapowaniePomocnicze %>% select(2,3) %>% distinct
colnames(MapowaniePomocnicze)[1] <- "RegionU"

MAIN_ESSP_YTD                      <- left_join(MAIN_ESSP_YTD,MapowaniePomocnicze,by="SubRegion")
MAIN_ESSP_YTD$Region               <- ifelse(is.na(MAIN_ESSP_YTD$Region),MAIN_ESSP_YTD$RegionU,MAIN_ESSP_YTD$Region)
MAIN_ESSP_YTD$RegionU              <- NULL


MAIN_ESSP_YTD                      <- setorder(MAIN_ESSP_YTD,Region,SubRegion)
MAIN_ESSP_YTD                      <- rbind(MAIN_ESSP_YTD,Total)

WyboldowaneWierszeMAIN_ESSP_YTD    <- c(which(MAIN_ESSP_YTD$MapowanieMD %in% Regiony),length(MAIN_ESSP_YTD$Region))
WyboldowaneWierszeMAIN_ESSP_YTD    <- c(which(str_sub(MAIN_ESSP_YTD$MapowanieMD,7,9) == "CEE"),WyboldowaneWierszeMAIN_ESSP_YTD)

#MAIN_ESSP_YTD         <- rbind(MAIN_ESSP_YTD,thereofCEE)  na razie tą część tabelki trzeba odpóścić, do znalezienia rozwiązania


MAIN_ESSP_YTD$Region                <- NULL
MAIN_ESSP_YTD$SubRegion             <- NULL

MAIN_ESSP_YTD                       <- MAIN_ESSP_YTD %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)



# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

MAIN_ESSP_YTD                                              <- MAIN_ESSP_YTD %>% mutate_if(is.numeric, ~round(., 0)) 
MAIN_ESSP_YTD[is.na(MAIN_ESSP_YTD)]                        <- ""
source(here::here("moduly","BBS.R"))
source(here::here("moduly","WarehouseDirect.R"))
source(here::here("moduly","Eur_Ind.R"))
source(here::here("moduly","total_results_es_sp.R"))
source(here::here("moduly","ICCPM_BY_INDUSTRY.R"))
source(here::here("moduly","TOP20.R"))
source(here::here("moduly","EKATCEKAT.R"))
source(here::here("moduly","WaterTreatment.R"))
source(here::here("moduly","salesParty.R"))


render(
  input = here::here("szkielet", "performace_raport_Creator.rmd"),          # path to the template
  output_file = paste("Performance_report","_", MiesiacAnalizyTekst, year(today()),".pdf", sep=""),  # name the output
  output_dir = here::here("output")  # folder in which to put the output file
)






