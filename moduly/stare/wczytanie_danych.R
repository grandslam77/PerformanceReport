
 

 render(
 input = here::here("szkielet", "performace_raport3.rmd"),          # path to the template
 output_file = paste("Performance_report","_", MiesiacAnalizyTekst, year(today()),".pdf", sep=""),  # name the output
 output_dir = here::here("output")  # folder in which to put the output file
 )
 

 
 dataAnalizy            <- as.Date("2023-01-31")
 MiesiacAnalizy         <- month(dataAnalizy)
 MiesiacAnalizyTekst    <- as.character(toupper(lubridate::month(dataAnalizy,label = TRUE, abbr = FALSE, locale =  'en_US')))
 
 sum <- function(x, ..., na.rm = TRUE) {
   base::sum(x, ..., na.rm = na.rm)
 }
 

BBS                     <- read_xlsx(here::here("sources","DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "BBS",range="I16:X6000", col_names = TRUE,na = "NA")
BBS                     <- BBS %>% filter_all(any_vars(!is.na(.))) 
BBS[is.na(BBS)]         <- 0


BBS[,11:16]             <- round(BBS[,11:16],0)


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


library(stringi)


BBS2                      <- read_xlsx(here::here("sources","DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "BBS",range="AA16:AP6000", col_names = TRUE,na = "NA")
BBS2                     <-  BBS2 %>% filter_all(any_vars(!is.na(.))) 
BBS2[is.na(BBS2)]         <- 0

BBS2[,11:16]             <- round(BBS2[,11:16],0)

colnames(BBS2)[3]   <- "BBSCompany"
colnames(BBS2)[11]  <- Naglowek1
colnames(BBS2)[12]  <- Naglowek2
colnames(BBS2)[13]  <- Naglowek3
colnames(BBS2)[14]  <- Naglowek4
colnames(BBS2)[15]  <- Naglowek5
colnames(BBS2)[16]  <- Naglowek6

colnames(MapowanieBaltics)[1]  <-"Subsidiary Country"

BBS <-left_join(BBS,MapowanieBaltics,by="Subsidiary Country")

BBSM                    <- BBS %>% filter(`Calendar month`==MiesiacAnalizy)
BBSM                    <- BBSM %>% select(17,18,11,12,13,14,15,16)

BBS2 <-left_join(BBS2,MapowanieBaltics,by="Subsidiary Country")
BBSM2                   <- BBS2 %>% filter(`Calendar month`==9)
BBSM2                   <- BBSM2 %>% select(17,18,11,12,13,14,15,16)

# BBSM<-rbind(BBSM,BBSM2)
BBSM<-as.data.frame(BBSM)
BBSM <- BBSM %>% group_by(Region,SubRegion) %>% summarize(QActYear=round(sum(`Quantity KG TOTAL 2022`)),QprevYear=round(sum(`Quantity KG TOTAL 2021`)),GPACT=round(sum(`Margin EUR (adj) TOTAL 2022`)),GPPREV=round(sum(`Margin EUR (adj) TOTAL 2021`)),TURNACT=round(sum(`Turnover EUR (adj) TOTAL 2022`)),TURNPREV=round(sum(`Turnover EUR (adj) TOTAL 2021`)))
BBSM[is.na(BBSM)]      <- NA
BBSM$QuantityDiffYY    <- BBSM$QActYear-BBSM$QprevYear
BBSM[is.na(BBSM)]      <- NA
BBSM$QuantityDiffYYPER <- ifelse(is.infinite(round(((BBSM$QActYear-BBSM$QprevYear)/BBSM$QprevYear),3)),"",scales::percent(round(((BBSM$QActYear-BBSM$QprevYear)/BBSM$QprevYear),3)))
BBSM[is.na(BBSM)]      <- NA
BBSM$GPDiffYY          <- BBSM$GPACT-BBSM$GPPREV
BBSM[is.na(BBSM)]      <- NA
BBSM$GPDiffYYPER       <- ifelse(is.infinite(round(((BBSM$GPACT-BBSM$GPPREV)/BBSM$GPPREV),3)),"",scales::percent(round(((BBSM$GPACT-BBSM$GPPREV)/BBSM$GPPREV),3)))
#BBSM$GPDiffYYPER       <- round(((BBSM$GPACT-BBSM$GPPREV)/BBSM$GPPREV)*100,2)
BBSM[is.na(BBSM)]      <- NA
BBSM$TurnDiffYY        <- BBSM$TURNACT-BBSM$TURNPREV
BBSM[is.na(BBSM)]      <- NA
BBSM$TurnDiffYYPER     <- ifelse(is.infinite(round(((BBSM$TURNACT-BBSM$TURNPREV)/BBSM$TURNPREV),3)),"",scales::percent(round(((BBSM$TURNACT-BBSM$TURNPREV)/BBSM$TURNPREV),3)))
BBSM[is.na(BBSM)]      <- NA
BBSM$GPMTACT           <- ifelse(is.infinite(round(BBSM$GPACT*1000/BBSM$QActYear,0)),0,round(BBSM$GPACT*1000/BBSM$QActYear,0))   # x 1000 ?
BBSM[is.na(BBSM)]      <- 0
BBSM$GPMTPREV          <- ifelse(is.infinite(round(BBSM$GPPREV*1000/BBSM$QprevYear,0)),0,round(BBSM$GPPREV*1000/BBSM$QprevYear,0))
BBSM[is.na(BBSM)]      <- 0
BBSM$GPMTDIFF          <- round(BBSM$GPMTACT-BBSM$GPMTPREV,0)
BBSM[is.na(BBSM)]      <- NA
BBSM$GPMTDIFFPER       <- ifelse(is.infinite(round((BBSM$GPMTACT-BBSM$GPMTPREV)/BBSM$GPMTPREV,3)),"",scales::percent(round((BBSM$GPMTACT-BBSM$GPMTPREV)/BBSM$GPMTPREV,3)))
BBSM[is.na(BBSM)]      <- NA

DT                     <- data.table(BBSM)
BBSM                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
BBSM[is.na(BBSM)]      <- NA
BBSM$SubRegion         <- stri_replace_all_fixed(BBSM$SubRegion, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)

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
Total$QuantityDiffYYPER  <-  scales::percent(round(Total$QuantityDiffYY/Total$QprevYear,3))
Total$GPDiffYYPER        <-  scales::percent(round(Total$GPDiffYY/Total$GPPREV,3))
Total$TurnDiffYYPER      <-  scales::percent(round(Total$TurnDiffYY/Total$TURNPREV,3))
Total$GPMTDIFFPER        <-  scales::percent(round(Total$GPMTDIFF/Total$GPMTPREV,3))
   
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
    frejm$QuantityDiffYYPER  <-  scales::percent(round(frejm$QuantityDiffYY/frejm$QprevYear,3))
    frejm$GPDiffYYPER        <-  scales::percent(round(frejm$GPDiffYY/frejm$GPPREV,3))
    frejm$TurnDiffYYPER      <-  scales::percent(round(frejm$TurnDiffYY/frejm$TURNPREV,3))
    frejm$GPMTDIFFPER        <-  scales::percent(round(frejm$GPMTDIFF/frejm$GPMTPREV,3))
    
    
    BBSM <- rbind(BBSM, frejm)
  
}

BBSM <- rbind(BBSM,Total)

BBSM           <- setorder(BBSM,Region)
BBSM$Region    <- NULL

BBSM <- BBSM %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
BBSM[is.na(BBSM)]      <- ""

# library(stringi)
# BBSM$SubRegion <- stri_replace_all_fixed(BBSM$SubRegion, pattern = c("&"), replacement = c(""), vectorize_all = FALSE)
# BBSM$perc      <- scales::percent(tabl1$n / sum(tabl1$n))
# BBSM$perc1     <- scales::percent(BBSM$QActYear-BBSM$QprevYear/BBSM$QprevYear)
# 





