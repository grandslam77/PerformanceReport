
 

 render(
 input = here::here("szkielet", "performace_raport3.rmd"),          # path to the template
 output_file = paste("Performance_report","_", MiesiacAnalizyTekst, year(today()),".pdf", sep=""),  # name the output
 output_dir = here::here("output")  # folder in which to put the output file
 )
 

dataAnalizy            <- as.Date("2023-01-31")
MiesiacAnalizy         <- month(dataAnalizy)
RokAnalizy             <- toupper(year(dataAnalizy))
MiesiacAnalizyTekst    <- as.character(toupper(lubridate::month(dataAnalizy,label = TRUE, abbr = FALSE, locale =  'en_US')))
 
sum <- function(x, ..., na.rm = TRUE) {
  base::sum(x, ..., na.rm = na.rm)
}
 

BBS_podst                     <- read_xlsx(here::here("sources","DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "BBS",range="I16:X6000", col_names = TRUE,na = "NA")


BBS_podst                     <- BBS_podst %>% filter_all(any_vars(!is.na(.))) 
BBS_podst[is.na(BBS_podst)]         <- 0   # wszystkie pola bez wartości wypełniamy zerami

BBS<- BBS_podst

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

colnames(MapowanieBaltics)[1]  <-"Subsidiary Country"

BBS                    <- left_join(BBS,MapowanieBaltics,by="Subsidiary Country")

BBSM                   <- BBS %>% filter(`Calendar month`==MiesiacAnalizy)
BBSM                   <- BBSM %>% select(17,18,11,12,13,14,15,16)

# BBSM<-rbind(BBSM,BBSM2)
BBSM                   <- as.data.frame(BBSM)
BBSM                   <- BBSM %>% group_by(Region,SubRegion) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
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


a=0
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


Total$TurnDiffYY         <-  Total$TURNACT-Total$TURNPREV
Total$QuantityDiffYY     <-  Total$QActYear-Total$QprevYear
Total$GPDiffYY           <-  Total$GPACT-Total$GPPREV
Total$GPMTDIFF           <-  Total$GPMTACT-Total$GPMTPREV
Total$QuantityDiffYYPER  <-  scales::percent(Total$QuantityDiffYY/Total$QprevYear,accuracy = 0.1)
Total$GPDiffYYPER        <-  scales::percent(Total$GPDiffYY/Total$GPPREV,accuracy = 0.1)
Total$TurnDiffYYPER      <-  scales::percent(Total$TurnDiffYY/Total$TURNPREV,accuracy = 0.1)
Total$GPMTDIFFPER        <-  scales::percent(Total$GPMTDIFF/Total$GPMTPREV,accuracy = 0.1)
   
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
BBSM[is.na(BBSM)]         <- ""



# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

BBSM <- BBSM %>% mutate_if(is.numeric, ~round(., 0))



#### ---------------- BBS EUROPEAN INDUSTRY

BBS_EI <- BBS_podst


colnames(BBS_EI)[11]<-Naglowek1
colnames(BBS_EI)[12]<-Naglowek2
colnames(BBS_EI)[13]<-Naglowek3
colnames(BBS_EI)[14]<-Naglowek4
colnames(BBS_EI)[15]<-Naglowek5
colnames(BBS_EI)[16]<-Naglowek6



BBS_EI                  <- BBS_EI %>% filter(`Calendar month`==MiesiacAnalizy)

colnames(MapowanieBranzaIndu)[1]  <-"European Industry"

BBS_EI                    <- left_join(BBS_EI,MapowanieBranzaIndu,by="European Industry")

BBS_EI                   <- BBS_EI %>% select(17,11:16)
colnames(BBS_EI)[1]  <-"European Industry"
# BBSM<-rbind(BBSM,BBSM2)
BBS_EI                    <- as.data.frame(BBS_EI )
BBS_EI                    <- BBS_EI  %>% group_by(`European Industry`) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
BBS_EI [is.na(BBS_EI)]    <- NA
BBS_EI$QuantityDiffYY    <- BBS_EI$QActYear-BBS_EI$QprevYear
BBS_EI[is.na(BBS_EI)]      <- NA
BBS_EI$QuantityDiffYYPER_P <- ifelse(is.infinite(((BBS_EI$QActYear-BBS_EI$QprevYear)/BBS_EI$QprevYear)),"",(BBS_EI$QActYear-BBS_EI$QprevYear)/BBS_EI$QprevYear)
BBS_EI$QuantityDiffYYPER <- ifelse(is.infinite(((BBS_EI$QActYear-BBS_EI$QprevYear)/BBS_EI$QprevYear)),"",scales::percent(((BBS_EI$QActYear-BBS_EI$QprevYear)/BBS_EI$QprevYear),accuracy = 0.1))

BBS_EI[is.na(BBS_EI)]      <- NA
BBS_EI$GPDiffYY          <- BBS_EI$GPACT-BBS_EI$GPPREV
BBS_EI[is.na(BBS_EI)]      <- NA
BBS_EI$GPDiffYYPER       <- ifelse(is.infinite(((BBS_EI$GPACT-BBS_EI$GPPREV)/BBS_EI$GPPREV)),"",scales::percent(((BBS_EI$GPACT-BBS_EI$GPPREV)/BBS_EI$GPPREV),accuracy = 0.1))
#BBS_EI$GPDiffYYPER       <- round(((BBS_EI$GPACT-BBS_EI$GPPREV)/BBS_EI$GPPREV)*100,2)
BBS_EI[is.na(BBS_EI)]      <- NA
BBS_EI$TurnDiffYY        <- BBS_EI$TURNACT-BBS_EI$TURNPREV
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
BBS_EI   $QuantityDiffYYPER_P<-NULL

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
BBS_EI[is.na(BBS_EI)]        <- ""

BBS_EI$EI        <- stri_replace_all_fixed(BBS_EI$EI, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
BBS_EI$EI        <- stri_replace_all_fixed(BBS_EI$EI, pattern = c("Industrial Sales and Services"), replacement = c("Ind. Sales and Serv."), vectorize_all = FALSE)


# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

BBS_EI <- BBS_EI %>% mutate_if(is.numeric, ~round(., 0))

#Jest problem z wydzieleniem Itercompany Outside z Industrial Sales and Services.













