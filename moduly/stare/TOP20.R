
MAINTOP20                      <- read_xlsx(here::here("sources","DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "TOP 20",range="C6:K11000", col_names = TRUE,na = "NA")

MAINTOP20                      <- MAINTOP20  %>% filter_all(any_vars(!is.na(.))) 
MAINTOP20 [is.na(MAINTOP20)]   <- 0   # wszystkie pola bez wartości wypełniamy zerami



Naglowek1 <- "Quantity KG TOTAL 2021"
Naglowek2 <- "Quantity KG TOTAL 2022"
Naglowek3 <- "Margin EUR (adj) TOTAL 2021"
Naglowek4 <- "Margin EUR (adj) TOTAL 2022"
Naglowek5 <- "Turnover EUR (adj) TOTAL 2021"
Naglowek6 <- "Turnover EUR (adj) TOTAL 2022"

colnames(MAINTOP20)[1]<-"Rodzaj"
colnames(MAINTOP20)[2]<-"MaterialGroup"
colnames(MAINTOP20)[4]<-Naglowek1
colnames(MAINTOP20)[5]<-Naglowek2
colnames(MAINTOP20)[6]<-Naglowek3
colnames(MAINTOP20)[7]<-Naglowek4
colnames(MAINTOP20)[8]<-Naglowek5
colnames(MAINTOP20)[9]<-Naglowek6


MAINTOP20                   <- left_join(MAINTOP20,MapowanieTOP20,by="Rodzaj")
MAINTOP20                   <- MAINTOP20[!is.na(MAINTOP20$Mapowanie),]
MAINTOP20$`Calendar month`  <- as.numeric(MAINTOP20$`Calendar month`)

TOP20GP                     <- MAINTOP20 %>% filter(`Calendar month`==MiesiacAnalizy)
TOP20GP                     <- TOP20GP %>% filter(Mapowanie=="TOP GP")


# TOP20GP<-rbind(TOP20GP,TOP20GP2)


TOP20GP                   <- as.data.frame(TOP20GP)

TOP20GP[is.na(TOP20GP)]      <- NA
TOP20GP                     <- TOP20GP%>% group_by(MaterialGroup) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))

TOP20GP$QuantityDiffYY    <- TOP20GP$QActYear-TOP20GP$QprevYear
TOP20GP[is.na(TOP20GP)]      <- NA
TOP20GP$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((TOP20GP$QActYear-TOP20GP$QprevYear)/TOP20GP$QprevYear)),"",(TOP20GP$QActYear-TOP20GP$QprevYear)/TOP20GP$QprevYear))
TOP20GP$QuantityDiffYYPER <- ifelse(is.infinite(((TOP20GP$QActYear-TOP20GP$QprevYear)/TOP20GP$QprevYear)),"",scales::percent(((TOP20GP$QActYear-TOP20GP$QprevYear)/TOP20GP$QprevYear),accuracy = 0.1))
TOP20GP[is.na(TOP20GP)]      <- NA
TOP20GP$GPDiffYY          <- TOP20GP$GPACT-TOP20GP$GPPREV
TOP20GP[is.na(TOP20GP)]      <- NA
TOP20GP$GPDiffYYPER       <- ifelse(is.infinite(((TOP20GP$GPACT-TOP20GP$GPPREV)/TOP20GP$GPPREV)),"",scales::percent(((TOP20GP$GPACT-TOP20GP$GPPREV)/TOP20GP$GPPREV),accuracy = 0.1))
#TOP20GP$GPDiffYYPER       <- round(((TOP20GP$GPACT-TOP20GP$GPPREV)/TOP20GP$GPPREV)*100,2)
TOP20GP[is.na(TOP20GP)]      <- NA
TOP20GP$TurnDiffYY        <- TOP20GP$TURNACT-TOP20GP$TURNPREV
TOP20GP[is.na(TOP20GP)]      <- NA
TOP20GP$TurnDiffYYPER     <- ifelse(is.infinite(((TOP20GP$TURNACT-TOP20GP$TURNPREV)/TOP20GP$TURNPREV)),"",scales::percent(((TOP20GP$TURNACT-TOP20GP$TURNPREV)/TOP20GP$TURNPREV),accuracy = 0.1))
TOP20GP[is.na(TOP20GP)]      <- NA
TOP20GP$GPMTACT           <- ifelse(is.infinite(TOP20GP$GPACT*1000/TOP20GP$QActYear),0,TOP20GP$GPACT*1000/TOP20GP$QActYear)   # x 1000 ?
TOP20GP[is.na(TOP20GP)]      <- 0
TOP20GP$GPMTPREV          <- ifelse(is.infinite(TOP20GP$GPPREV*1000/TOP20GP$QprevYear),0,TOP20GP$GPPREV*1000/TOP20GP$QprevYear)
TOP20GP[is.na(TOP20GP)]      <- 0
TOP20GP$GPMTDIFF          <- TOP20GP$GPMTACT-TOP20GP$GPMTPREV
TOP20GP[is.na(TOP20GP)]      <- NA
TOP20GP$GPMTDIFFPER       <- ifelse(is.infinite((TOP20GP$GPMTACT-TOP20GP$GPMTPREV)/TOP20GP$GPMTPREV),"",scales::percent(((TOP20GP$GPMTACT-TOP20GP$GPMTPREV)/TOP20GP$GPMTPREV),accuracy = 0.1))
TOP20GP[is.na(TOP20GP)]      <- NA

DT                     <- data.table(TOP20GP)
TOP20GP                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
TOP20GP[is.na(TOP20GP)]      <- NA
TOP20GP$MaterialGroup       <- stri_replace_all_fixed(TOP20GP$MaterialGroup, pattern = c("&","%"), replacement = c("","PER"), vectorize_all = FALSE)
TOP20GP                      <- setorder(TOP20GP,-GPDiffYY)
TOP20GP$QuantityDiffYYPER_P <-NULL



Total<-data.frame(MaterialGroup="Total CEE",
                  QActYear          = sum(TOP20GP$QActYear),
                  QprevYear         = sum(TOP20GP$QprevYear),
                  GPACT             = sum(TOP20GP$GPACT),
                  GPPREV            = sum(TOP20GP$GPPREV),
                  TURNACT           = sum(TOP20GP$TURNACT),
                  TURNPREV          = sum(TOP20GP$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(TOP20GP$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(TOP20GP$GPMTACT),
                  GPMTPREV          = sum(TOP20GP$GPMTPREV),
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

TOP20GP                  <- TOP20GP %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
TOP20GP                  <- rbind(TOP20GP,Total)

TOP20GP[is.na(TOP20GP)]  <- ""



# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

TOP20GP                   <- TOP20GP %>% mutate_if(is.numeric, ~round(., 0))
WyboldowaneWierszeTOP20GP <- length(TOP20GP$MaterialGroup)


#TOP20GPYTDYTD

TOP20GPYTD                     <- MAINTOP20    %>% filter(`Calendar month` <= MiesiacAnalizy)
TOP20GPYTD                     <- TOP20GPYTD   %>% filter(Mapowanie=="TOP GP")


# TOP20GPYTD <- rbind(TOP20GPYTD,TOP20GPYTD2)


TOP20GPYTD                   <- as.data.frame(TOP20GPYTD)
TOP20GPYTD[is.na(TOP20GPYTD)]      <- NA
TOP20GPYTD                     <- TOP20GPYTD%>% group_by(MaterialGroup) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))

TOP20GPYTD$QuantityDiffYY    <- TOP20GPYTD$QActYear-TOP20GPYTD$QprevYear
TOP20GPYTD[is.na(TOP20GPYTD)]      <- NA
TOP20GPYTD$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((TOP20GPYTD$QActYear-TOP20GPYTD$QprevYear)/TOP20GPYTD$QprevYear)),"",(TOP20GPYTD$QActYear-TOP20GPYTD$QprevYear)/TOP20GPYTD$QprevYear))
TOP20GPYTD$QuantityDiffYYPER <- ifelse(is.infinite(((TOP20GPYTD$QActYear-TOP20GPYTD$QprevYear)/TOP20GPYTD$QprevYear)),"",scales::percent(((TOP20GPYTD$QActYear-TOP20GPYTD$QprevYear)/TOP20GPYTD$QprevYear),accuracy = 0.1))
TOP20GPYTD[is.na(TOP20GPYTD)]      <- NA
TOP20GPYTD$GPDiffYY          <- TOP20GPYTD$GPACT-TOP20GPYTD$GPPREV
TOP20GPYTD[is.na(TOP20GPYTD)]      <- NA
TOP20GPYTD$GPDiffYYPER       <- ifelse(is.infinite(((TOP20GPYTD$GPACT-TOP20GPYTD$GPPREV)/TOP20GPYTD$GPPREV)),"",scales::percent(((TOP20GPYTD$GPACT-TOP20GPYTD$GPPREV)/TOP20GPYTD$GPPREV),accuracy = 0.1))
#TOP20GPYTD$GPDiffYYPER       <- round(((TOP20GPYTD$GPACT-TOP20GPYTD$GPPREV)/TOP20GPYTD$GPPREV)*100,2)
TOP20GPYTD[is.na(TOP20GPYTD)]      <- NA
TOP20GPYTD$TurnDiffYY        <- TOP20GPYTD$TURNACT-TOP20GPYTD$TURNPREV
TOP20GPYTD[is.na(TOP20GPYTD)]      <- NA
TOP20GPYTD$TurnDiffYYPER     <- ifelse(is.infinite(((TOP20GPYTD$TURNACT-TOP20GPYTD$TURNPREV)/TOP20GPYTD$TURNPREV)),"",scales::percent(((TOP20GPYTD$TURNACT-TOP20GPYTD$TURNPREV)/TOP20GPYTD$TURNPREV),accuracy = 0.1))
TOP20GPYTD[is.na(TOP20GPYTD)]      <- NA
TOP20GPYTD$GPMTACT           <- ifelse(is.infinite(TOP20GPYTD$GPACT*1000/TOP20GPYTD$QActYear),0,TOP20GPYTD$GPACT*1000/TOP20GPYTD$QActYear)   # x 1000 ?
TOP20GPYTD[is.na(TOP20GPYTD)]      <- 0
TOP20GPYTD$GPMTPREV          <- ifelse(is.infinite(TOP20GPYTD$GPPREV*1000/TOP20GPYTD$QprevYear),0,TOP20GPYTD$GPPREV*1000/TOP20GPYTD$QprevYear)
TOP20GPYTD[is.na(TOP20GPYTD)]      <- 0
TOP20GPYTD$GPMTDIFF          <- TOP20GPYTD$GPMTACT-TOP20GPYTD$GPMTPREV
TOP20GPYTD[is.na(TOP20GPYTD)]      <- NA
TOP20GPYTD$GPMTDIFFPER       <- ifelse(is.infinite((TOP20GPYTD$GPMTACT-TOP20GPYTD$GPMTPREV)/TOP20GPYTD$GPMTPREV),"",scales::percent(((TOP20GPYTD$GPMTACT-TOP20GPYTD$GPMTPREV)/TOP20GPYTD$GPMTPREV),accuracy = 0.1))
TOP20GPYTD[is.na(TOP20GPYTD)]      <- NA

DT                     <- data.table(TOP20GPYTD)
TOP20GPYTD                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
TOP20GPYTD[is.na(TOP20GPYTD)]      <- NA
TOP20GPYTD$MaterialGroup       <- stri_replace_all_fixed(TOP20GPYTD$MaterialGroup, pattern = c("&","%"), replacement = c("","PER"), vectorize_all = FALSE)
TOP20GPYTD                      <- setorder(TOP20GPYTD,-GPDiffYY)
TOP20GPYTD$QuantityDiffYYPER_P <-NULL



Total<-data.frame(MaterialGroup="Total CEE",
                  QActYear          = sum(TOP20GPYTD$QActYear),
                  QprevYear         = sum(TOP20GPYTD$QprevYear),
                  GPACT             = sum(TOP20GPYTD$GPACT),
                  GPPREV            = sum(TOP20GPYTD$GPPREV),
                  TURNACT           = sum(TOP20GPYTD$TURNACT),
                  TURNPREV          = sum(TOP20GPYTD$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(TOP20GPYTD$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(TOP20GPYTD$GPMTACT),
                  GPMTPREV          = sum(TOP20GPYTD$GPMTPREV),
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

TOP20GPYTD                  <- TOP20GPYTD %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
TOP20GPYTD                  <- rbind(TOP20GPYTD,Total)

TOP20GPYTD[is.na(TOP20GPYTD)]  <- ""


# formatowanie liczby do liczby znaków   !!!! MEGAAAAA


TOP20GPYTD                   <- TOP20GPYTD %>% mutate_if(is.numeric, ~round(., 0))
WyboldowaneWierszeTOP20GPYTD <- length(TOP20GPYTD$MaterialGroup)


#-TOP20QUANT (month)---------------------------------------------

TOP20QUANT                     <- MAINTOP20 %>% filter(`Calendar month`==MiesiacAnalizy)
TOP20QUANT                     <- TOP20QUANT %>% filter(Mapowanie=="TOP Qty.")

TOP20QUANT                   <- as.data.frame(TOP20QUANT)

TOP20QUANT[is.na(TOP20QUANT)]      <- NA
TOP20QUANT                    <- TOP20QUANT%>% group_by(MaterialGroup) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))

TOP20QUANT$QuantityDiffYY    <- TOP20QUANT$QActYear-TOP20QUANT$QprevYear
TOP20QUANT[is.na(TOP20QUANT)]      <- NA
TOP20QUANT$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((TOP20QUANT$QActYear-TOP20QUANT$QprevYear)/TOP20QUANT$QprevYear)),"",(TOP20QUANT$QActYear-TOP20QUANT$QprevYear)/TOP20QUANT$QprevYear))
TOP20QUANT$QuantityDiffYYPER <- ifelse(is.infinite(((TOP20QUANT$QActYear-TOP20QUANT$QprevYear)/TOP20QUANT$QprevYear)),"",scales::percent(((TOP20QUANT$QActYear-TOP20QUANT$QprevYear)/TOP20QUANT$QprevYear),accuracy = 0.1))
TOP20QUANT[is.na(TOP20QUANT)]      <- NA
TOP20QUANT$GPDiffYY          <- TOP20QUANT$GPACT-TOP20QUANT$GPPREV
TOP20QUANT[is.na(TOP20QUANT)]      <- NA
TOP20QUANT$GPDiffYYPER       <- ifelse(is.infinite(((TOP20QUANT$GPACT-TOP20QUANT$GPPREV)/TOP20QUANT$GPPREV)),"",scales::percent(((TOP20QUANT$GPACT-TOP20QUANT$GPPREV)/TOP20QUANT$GPPREV),accuracy = 0.1))
#TOP20QUANT$GPDiffYYPER       <- round(((TOP20QUANT$GPACT-TOP20QUANT$GPPREV)/TOP20QUANT$GPPREV)*100,2)
TOP20QUANT[is.na(TOP20QUANT)]      <- NA
TOP20QUANT$TurnDiffYY        <- TOP20QUANT$TURNACT-TOP20QUANT$TURNPREV
TOP20QUANT[is.na(TOP20QUANT)]      <- NA
TOP20QUANT$TurnDiffYYPER     <- ifelse(is.infinite(((TOP20QUANT$TURNACT-TOP20QUANT$TURNPREV)/TOP20QUANT$TURNPREV)),"",scales::percent(((TOP20QUANT$TURNACT-TOP20QUANT$TURNPREV)/TOP20QUANT$TURNPREV),accuracy = 0.1))
TOP20QUANT[is.na(TOP20QUANT)]      <- NA
TOP20QUANT$GPMTACT           <- ifelse(is.infinite(TOP20QUANT$GPACT*1000/TOP20QUANT$QActYear),0,TOP20QUANT$GPACT*1000/TOP20QUANT$QActYear)   # x 1000 ?
TOP20QUANT[is.na(TOP20QUANT)]      <- 0
TOP20QUANT$GPMTPREV          <- ifelse(is.infinite(TOP20QUANT$GPPREV*1000/TOP20QUANT$QprevYear),0,TOP20QUANT$GPPREV*1000/TOP20QUANT$QprevYear)
TOP20QUANT[is.na(TOP20QUANT)]      <- 0
TOP20QUANT$GPMTDIFF          <- TOP20QUANT$GPMTACT-TOP20QUANT$GPMTPREV
TOP20QUANT[is.na(TOP20QUANT)]      <- NA
TOP20QUANT$GPMTDIFFPER       <- ifelse(is.infinite((TOP20QUANT$GPMTACT-TOP20QUANT$GPMTPREV)/TOP20QUANT$GPMTPREV),"",scales::percent(((TOP20QUANT$GPMTACT-TOP20QUANT$GPMTPREV)/TOP20QUANT$GPMTPREV),accuracy = 0.1))
TOP20QUANT[is.na(TOP20QUANT)]      <- NA

DT                     <- data.table(TOP20QUANT)
TOP20QUANT                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
TOP20QUANT[is.na(TOP20QUANT)]      <- NA
TOP20QUANT$MaterialGroup       <- stri_replace_all_fixed(TOP20QUANT$MaterialGroup, pattern = c("&","%"), replacement = c("","PER"), vectorize_all = FALSE)
TOP20QUANT                      <- setorder(TOP20QUANT,-QuantityDiffYY)
TOP20QUANT$QuantityDiffYYPER_P <-NULL



Total<-data.frame(MaterialGroup="Total CEE",
                  QActYear          = sum(TOP20QUANT$QActYear),
                  QprevYear         = sum(TOP20QUANT$QprevYear),
                  GPACT             = sum(TOP20QUANT$GPACT),
                  GPPREV            = sum(TOP20QUANT$GPPREV),
                  TURNACT           = sum(TOP20QUANT$TURNACT),
                  TURNPREV          = sum(TOP20QUANT$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(TOP20QUANT$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(TOP20QUANT$GPMTACT),
                  GPMTPREV          = sum(TOP20QUANT$GPMTPREV),
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

TOP20QUANT                  <- TOP20QUANT %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
TOP20QUANT                  <- rbind(TOP20QUANT,Total)

TOP20QUANT[is.na(TOP20QUANT)]  <- ""



# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

TOP20QUANT                 <- TOP20QUANT %>% mutate_if(is.numeric, ~round(., 0))
WyboldowaneWierszeTOP20QUANT <- length(TOP20QUANT$MaterialGroup)


### --------------------------- TOP 20 QUANT YTD. --------------------------------------

#-TOP20QUANT (month)---------------------------------------------

TOP20QUANT_YTD                     <- MAINTOP20 %>% filter(`Calendar month` <= MiesiacAnalizy)
TOP20QUANT_YTD                     <- TOP20QUANT_YTD %>% filter(Mapowanie=="TOP Qty.")

TOP20QUANT_YTD                   <- as.data.frame(TOP20QUANT_YTD)
TOP20QUANT_YTD[is.na(TOP20QUANT_YTD)]      <- NA
TOP20QUANT_YTD                    <- TOP20QUANT_YTD%>% group_by(MaterialGroup) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))

TOP20QUANT_YTD$QuantityDiffYY    <- TOP20QUANT_YTD$QActYear-TOP20QUANT_YTD$QprevYear
TOP20QUANT_YTD[is.na(TOP20QUANT_YTD)]      <- NA
TOP20QUANT_YTD$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((TOP20QUANT_YTD$QActYear-TOP20QUANT_YTD$QprevYear)/TOP20QUANT_YTD$QprevYear)),"",(TOP20QUANT_YTD$QActYear-TOP20QUANT_YTD$QprevYear)/TOP20QUANT_YTD$QprevYear))
TOP20QUANT_YTD$QuantityDiffYYPER <- ifelse(is.infinite(((TOP20QUANT_YTD$QActYear-TOP20QUANT_YTD$QprevYear)/TOP20QUANT_YTD$QprevYear)),"",scales::percent(((TOP20QUANT_YTD$QActYear-TOP20QUANT_YTD$QprevYear)/TOP20QUANT_YTD$QprevYear),accuracy = 0.1))
TOP20QUANT_YTD[is.na(TOP20QUANT_YTD)]      <- NA
TOP20QUANT_YTD$GPDiffYY          <- TOP20QUANT_YTD$GPACT-TOP20QUANT_YTD$GPPREV
TOP20QUANT_YTD[is.na(TOP20QUANT_YTD)]      <- NA
TOP20QUANT_YTD$GPDiffYYPER       <- ifelse(is.infinite(((TOP20QUANT_YTD$GPACT-TOP20QUANT_YTD$GPPREV)/TOP20QUANT_YTD$GPPREV)),"",scales::percent(((TOP20QUANT_YTD$GPACT-TOP20QUANT_YTD$GPPREV)/TOP20QUANT_YTD$GPPREV),accuracy = 0.1))
#TOP20QUANT_YTD$GPDiffYYPER       <- round(((TOP20QUANT_YTD$GPACT-TOP20QUANT_YTD$GPPREV)/TOP20QUANT_YTD$GPPREV)*100,2)
TOP20QUANT_YTD[is.na(TOP20QUANT_YTD)]      <- NA
TOP20QUANT_YTD$TurnDiffYY        <- TOP20QUANT_YTD$TURNACT-TOP20QUANT_YTD$TURNPREV
TOP20QUANT_YTD[is.na(TOP20QUANT_YTD)]      <- NA
TOP20QUANT_YTD$TurnDiffYYPER     <- ifelse(is.infinite(((TOP20QUANT_YTD$TURNACT-TOP20QUANT_YTD$TURNPREV)/TOP20QUANT_YTD$TURNPREV)),"",scales::percent(((TOP20QUANT_YTD$TURNACT-TOP20QUANT_YTD$TURNPREV)/TOP20QUANT_YTD$TURNPREV),accuracy = 0.1))
TOP20QUANT_YTD[is.na(TOP20QUANT_YTD)]      <- NA
TOP20QUANT_YTD$GPMTACT           <- ifelse(is.infinite(TOP20QUANT_YTD$GPACT*1000/TOP20QUANT_YTD$QActYear),0,TOP20QUANT_YTD$GPACT*1000/TOP20QUANT_YTD$QActYear)   # x 1000 ?
TOP20QUANT_YTD[is.na(TOP20QUANT_YTD)]      <- 0
TOP20QUANT_YTD$GPMTPREV          <- ifelse(is.infinite(TOP20QUANT_YTD$GPPREV*1000/TOP20QUANT_YTD$QprevYear),0,TOP20QUANT_YTD$GPPREV*1000/TOP20QUANT_YTD$QprevYear)
TOP20QUANT_YTD[is.na(TOP20QUANT_YTD)]      <- 0
TOP20QUANT_YTD$GPMTDIFF          <- TOP20QUANT_YTD$GPMTACT-TOP20QUANT_YTD$GPMTPREV
TOP20QUANT_YTD[is.na(TOP20QUANT_YTD)]      <- NA
TOP20QUANT_YTD$GPMTDIFFPER       <- ifelse(is.infinite((TOP20QUANT_YTD$GPMTACT-TOP20QUANT_YTD$GPMTPREV)/TOP20QUANT_YTD$GPMTPREV),"",scales::percent(((TOP20QUANT_YTD$GPMTACT-TOP20QUANT_YTD$GPMTPREV)/TOP20QUANT_YTD$GPMTPREV),accuracy = 0.1))
TOP20QUANT_YTD[is.na(TOP20QUANT_YTD)]      <- NA

DT                     <- data.table(TOP20QUANT_YTD)
TOP20QUANT_YTD                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
TOP20QUANT_YTD[is.na(TOP20QUANT_YTD)]      <- NA
TOP20QUANT_YTD$MaterialGroup       <- stri_replace_all_fixed(TOP20QUANT_YTD$MaterialGroup, pattern = c("&","%"), replacement = c("","PER"), vectorize_all = FALSE)
TOP20QUANT_YTD                      <- setorder(TOP20QUANT_YTD,-QuantityDiffYY)
TOP20QUANT_YTD$QuantityDiffYYPER_P <-NULL



Total<-data.frame(MaterialGroup="Total CEE",
                  QActYear          = sum(TOP20QUANT_YTD$QActYear),
                  QprevYear         = sum(TOP20QUANT_YTD$QprevYear),
                  GPACT             = sum(TOP20QUANT_YTD$GPACT),
                  GPPREV            = sum(TOP20QUANT_YTD$GPPREV),
                  TURNACT           = sum(TOP20QUANT_YTD$TURNACT),
                  TURNPREV          = sum(TOP20QUANT_YTD$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(TOP20QUANT_YTD$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(TOP20QUANT_YTD$GPMTACT),
                  GPMTPREV          = sum(TOP20QUANT_YTD$GPMTPREV),
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

TOP20QUANT_YTD                  <- TOP20QUANT_YTD %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
TOP20QUANT_YTD                  <- rbind(TOP20QUANT_YTD,Total)

TOP20QUANT_YTD[is.na(TOP20QUANT_YTD)]  <- ""



# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

TOP20QUANT_YTD                      <- TOP20QUANT_YTD %>% mutate_if(is.numeric, ~round(., 0))
WyboldowaneWierszeTOP20QUANT_YTD    <- length(TOP20QUANT_YTD$MaterialGroup)



































# ------------------- BOTTOM20GP  (month)

BOTTOM20GP                     <- MAINTOP20 %>% filter(`Calendar month`==MiesiacAnalizy)
BOTTOM20GP                     <- BOTTOM20GP %>% filter(Mapowanie=="BOTTOM GP")





# BOTTOM20GP<-rbind(BOTTOM20GP,BOTTOM20GP2)
BOTTOM20GP                   <- as.data.frame(BOTTOM20GP)

BOTTOM20GP[is.na(BOTTOM20GP)]      <- NA
BOTTOM20GP                         <- BOTTOM20GP %>% group_by(MaterialGroup) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))

BOTTOM20GP$QuantityDiffYY          <- BOTTOM20GP$QActYear-BOTTOM20GP$QprevYear
BOTTOM20GP[is.na(BOTTOM20GP)]      <- NA
BOTTOM20GP$QuantityDiffYYPER_P     <- as.numeric(ifelse(is.infinite(((BOTTOM20GP$QActYear-BOTTOM20GP$QprevYear)/BOTTOM20GP$QprevYear)),"",(BOTTOM20GP$QActYear-BOTTOM20GP$QprevYear)/BOTTOM20GP$QprevYear))
BOTTOM20GP$QuantityDiffYYPER       <- ifelse(is.infinite(((BOTTOM20GP$QActYear-BOTTOM20GP$QprevYear)/BOTTOM20GP$QprevYear)),"",scales::percent(((BOTTOM20GP$QActYear-BOTTOM20GP$QprevYear)/BOTTOM20GP$QprevYear),accuracy = 0.1))
BOTTOM20GP[is.na(BOTTOM20GP)]      <- NA
BOTTOM20GP$GPDiffYY                <- BOTTOM20GP$GPACT-BOTTOM20GP$GPPREV
BOTTOM20GP[is.na(BOTTOM20GP)]      <- NA
BOTTOM20GP$GPDiffYYPER             <- ifelse(is.infinite(((BOTTOM20GP$GPACT-BOTTOM20GP$GPPREV)/BOTTOM20GP$GPPREV)),"",scales::percent(((BOTTOM20GP$GPACT-BOTTOM20GP$GPPREV)/BOTTOM20GP$GPPREV),accuracy = 0.1))
#BOTTOM20GP$GPDiffYYPER       <- round(((BOTTOM20GP$GPACT-BOTTOM20GP$GPPREV)/BOTTOM20GP$GPPREV)*100,2)
BOTTOM20GP[is.na(BOTTOM20GP)]      <- NA
BOTTOM20GP$TurnDiffYY              <- BOTTOM20GP$TURNACT-BOTTOM20GP$TURNPREV
BOTTOM20GP[is.na(BOTTOM20GP)]      <- NA
BOTTOM20GP$TurnDiffYYPER           <- ifelse(is.infinite(((BOTTOM20GP$TURNACT-BOTTOM20GP$TURNPREV)/BOTTOM20GP$TURNPREV)),"",scales::percent(((BOTTOM20GP$TURNACT-BOTTOM20GP$TURNPREV)/BOTTOM20GP$TURNPREV),accuracy = 0.1))
BOTTOM20GP[is.na(BOTTOM20GP)]      <- NA
BOTTOM20GP$GPMTACT                 <- ifelse(is.infinite(BOTTOM20GP$GPACT*1000/BOTTOM20GP$QActYear),0,BOTTOM20GP$GPACT*1000/BOTTOM20GP$QActYear)   # x 1000 ?
BOTTOM20GP[is.na(BOTTOM20GP)]      <- 0
BOTTOM20GP$GPMTPREV                <- ifelse(is.infinite(BOTTOM20GP$GPPREV*1000/BOTTOM20GP$QprevYear),0,BOTTOM20GP$GPPREV*1000/BOTTOM20GP$QprevYear)
BOTTOM20GP[is.na(BOTTOM20GP)]      <- 0
BOTTOM20GP$GPMTDIFF                <- BOTTOM20GP$GPMTACT-BOTTOM20GP$GPMTPREV
BOTTOM20GP[is.na(BOTTOM20GP)]      <- NA
BOTTOM20GP$GPMTDIFFPER             <- ifelse(is.infinite((BOTTOM20GP$GPMTACT-BOTTOM20GP$GPMTPREV)/BOTTOM20GP$GPMTPREV),"",scales::percent(((BOTTOM20GP$GPMTACT-BOTTOM20GP$GPMTPREV)/BOTTOM20GP$GPMTPREV),accuracy = 0.1))
BOTTOM20GP[is.na(BOTTOM20GP)]      <- NA

DT                                 <- data.table(BOTTOM20GP)
BOTTOM20GP                         <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
BOTTOM20GP[is.na(BOTTOM20GP)]      <- NA
BOTTOM20GP$MaterialGroup           <- stri_replace_all_fixed(BOTTOM20GP$MaterialGroup, pattern = c("&","%"), replacement = c("","PER"), vectorize_all = FALSE)
BOTTOM20GP                         <- setorder(BOTTOM20GP,GPDiffYY)
BOTTOM20GP$QuantityDiffYYPER_P     <- NULL



Total<-data.frame(MaterialGroup="Total CEE",
                  QActYear          = sum(BOTTOM20GP$QActYear),
                  QprevYear         = sum(BOTTOM20GP$QprevYear),
                  GPACT             = sum(BOTTOM20GP$GPACT),
                  GPPREV            = sum(BOTTOM20GP$GPPREV),
                  TURNACT           = sum(BOTTOM20GP$TURNACT),
                  TURNPREV          = sum(BOTTOM20GP$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(BOTTOM20GP$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(BOTTOM20GP$GPMTACT),
                  GPMTPREV          = sum(BOTTOM20GP$GPMTPREV),
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

BOTTOM20GP                  <- BOTTOM20GP %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
BOTTOM20GP                  <- rbind(BOTTOM20GP,Total)

BOTTOM20GP[is.na(BOTTOM20GP)]  <- ""



# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

BOTTOM20GP                   <- BOTTOM20GP %>% mutate_if(is.numeric, ~round(., 0))
WyboldowaneWierszeBOTTOM20GP <- length(BOTTOM20GP$MaterialGroup)


#BOTTOM20GP_YTD

BOTTOM20GP_YTD                     <- MAINTOP20    %>% filter(`Calendar month` <= MiesiacAnalizy)
BOTTOM20GP_YTD                     <- BOTTOM20GP_YTD   %>% filter(Mapowanie=="BOTTOM GP")


# BOTTOM20GP_YTD <- rbind(BOTTOM20GP_YTD,BOTTOM20GP_YTD2)


BOTTOM20GP_YTD                   <- as.data.frame(BOTTOM20GP_YTD)
BOTTOM20GP_YTD[is.na(BOTTOM20GP_YTD)]      <- NA
BOTTOM20GP_YTD                   <- BOTTOM20GP_YTD%>% group_by(MaterialGroup) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))

BOTTOM20GP_YTD$QuantityDiffYY    <- BOTTOM20GP_YTD$QActYear-BOTTOM20GP_YTD$QprevYear
BOTTOM20GP_YTD[is.na(BOTTOM20GP_YTD)]      <- NA
BOTTOM20GP_YTD$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((BOTTOM20GP_YTD$QActYear-BOTTOM20GP_YTD$QprevYear)/BOTTOM20GP_YTD$QprevYear)),"",(BOTTOM20GP_YTD$QActYear-BOTTOM20GP_YTD$QprevYear)/BOTTOM20GP_YTD$QprevYear))
BOTTOM20GP_YTD$QuantityDiffYYPER <- ifelse(is.infinite(((BOTTOM20GP_YTD$QActYear-BOTTOM20GP_YTD$QprevYear)/BOTTOM20GP_YTD$QprevYear)),"",scales::percent(((BOTTOM20GP_YTD$QActYear-BOTTOM20GP_YTD$QprevYear)/BOTTOM20GP_YTD$QprevYear),accuracy = 0.1))
BOTTOM20GP_YTD[is.na(BOTTOM20GP_YTD)]      <- NA
BOTTOM20GP_YTD$GPDiffYY          <- BOTTOM20GP_YTD$GPACT-BOTTOM20GP_YTD$GPPREV
BOTTOM20GP_YTD[is.na(BOTTOM20GP_YTD)]      <- NA
BOTTOM20GP_YTD$GPDiffYYPER       <- ifelse(is.infinite(((BOTTOM20GP_YTD$GPACT-BOTTOM20GP_YTD$GPPREV)/BOTTOM20GP_YTD$GPPREV)),"",scales::percent(((BOTTOM20GP_YTD$GPACT-BOTTOM20GP_YTD$GPPREV)/BOTTOM20GP_YTD$GPPREV),accuracy = 0.1))
#BOTTOM20GP_YTD$GPDiffYYPER       <- round(((BOTTOM20GP_YTD$GPACT-BOTTOM20GP_YTD$GPPREV)/BOTTOM20GP_YTD$GPPREV)*100,2)
BOTTOM20GP_YTD[is.na(BOTTOM20GP_YTD)]      <- NA
BOTTOM20GP_YTD$TurnDiffYY        <- BOTTOM20GP_YTD$TURNACT-BOTTOM20GP_YTD$TURNPREV
BOTTOM20GP_YTD[is.na(BOTTOM20GP_YTD)]      <- NA
BOTTOM20GP_YTD$TurnDiffYYPER     <- ifelse(is.infinite(((BOTTOM20GP_YTD$TURNACT-BOTTOM20GP_YTD$TURNPREV)/BOTTOM20GP_YTD$TURNPREV)),"",scales::percent(((BOTTOM20GP_YTD$TURNACT-BOTTOM20GP_YTD$TURNPREV)/BOTTOM20GP_YTD$TURNPREV),accuracy = 0.1))
BOTTOM20GP_YTD[is.na(BOTTOM20GP_YTD)]      <- NA
BOTTOM20GP_YTD$GPMTACT           <- ifelse(is.infinite(BOTTOM20GP_YTD$GPACT*1000/BOTTOM20GP_YTD$QActYear),0,BOTTOM20GP_YTD$GPACT*1000/BOTTOM20GP_YTD$QActYear)   # x 1000 ?
BOTTOM20GP_YTD[is.na(BOTTOM20GP_YTD)]      <- 0
BOTTOM20GP_YTD$GPMTPREV          <- ifelse(is.infinite(BOTTOM20GP_YTD$GPPREV*1000/BOTTOM20GP_YTD$QprevYear),0,BOTTOM20GP_YTD$GPPREV*1000/BOTTOM20GP_YTD$QprevYear)
BOTTOM20GP_YTD[is.na(BOTTOM20GP_YTD)]      <- 0
BOTTOM20GP_YTD$GPMTDIFF          <- BOTTOM20GP_YTD$GPMTACT-BOTTOM20GP_YTD$GPMTPREV
BOTTOM20GP_YTD[is.na(BOTTOM20GP_YTD)]      <- NA
BOTTOM20GP_YTD$GPMTDIFFPER       <- ifelse(is.infinite((BOTTOM20GP_YTD$GPMTACT-BOTTOM20GP_YTD$GPMTPREV)/BOTTOM20GP_YTD$GPMTPREV),"",scales::percent(((BOTTOM20GP_YTD$GPMTACT-BOTTOM20GP_YTD$GPMTPREV)/BOTTOM20GP_YTD$GPMTPREV),accuracy = 0.1))
BOTTOM20GP_YTD[is.na(BOTTOM20GP_YTD)]      <- NA

DT                               <- data.table(BOTTOM20GP_YTD)
BOTTOM20GP_YTD                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
BOTTOM20GP_YTD[is.na(BOTTOM20GP_YTD)]      <- NA
BOTTOM20GP_YTD$MaterialGroup     <- stri_replace_all_fixed(BOTTOM20GP_YTD$MaterialGroup, pattern = c("&","%"), replacement = c("","PER"), vectorize_all = FALSE)
BOTTOM20GP_YTD                  <- setorder(BOTTOM20GP_YTD,GPDiffYY)
BOTTOM20GP_YTD$QuantityDiffYYPER_P <-NULL

Total <- data.frame(MaterialGroup="Total CEE",
                  QActYear          = sum(BOTTOM20GP_YTD$QActYear),
                  QprevYear         = sum(BOTTOM20GP_YTD$QprevYear),
                  GPACT             = sum(BOTTOM20GP_YTD$GPACT),
                  GPPREV            = sum(BOTTOM20GP_YTD$GPPREV),
                  TURNACT           = sum(BOTTOM20GP_YTD$TURNACT),
                  TURNPREV          = sum(BOTTOM20GP_YTD$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(BOTTOM20GP_YTD$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(BOTTOM20GP_YTD$GPMTACT),
                  GPMTPREV          = sum(BOTTOM20GP_YTD$GPMTPREV),
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

BOTTOM20GP_YTD                  <- BOTTOM20GP_YTD %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
BOTTOM20GP_YTD                  <- rbind(BOTTOM20GP_YTD,Total)

BOTTOM20GP_YTD[is.na(BOTTOM20GP_YTD)]  <- ""



# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

BOTTOM20GP_YTD                   <- BOTTOM20GP_YTD %>% mutate_if(is.numeric, ~round(., 0))
WyboldowaneWierszeBOTTOM20GP_YTD <- length(BOTTOM20GP_YTD$MaterialGroup)



#-TOP20QUANT (month)---------------------------------------------

BOTTOM20QUANT                     <- MAINTOP20 %>% filter(`Calendar month`==MiesiacAnalizy)
BOTTOM20QUANT                     <- BOTTOM20QUANT %>% filter(Mapowanie=="BOTTOM Qty.")

BOTTOM20QUANT                   <- as.data.frame(BOTTOM20QUANT)

BOTTOM20QUANT[is.na(BOTTOM20QUANT)]      <- NA
BOTTOM20QUANT                  <- BOTTOM20QUANT%>% group_by(MaterialGroup) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))

BOTTOM20QUANT$QuantityDiffYY    <- BOTTOM20QUANT$QActYear-BOTTOM20QUANT$QprevYear
BOTTOM20QUANT[is.na(BOTTOM20QUANT)]      <- NA
BOTTOM20QUANT$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((BOTTOM20QUANT$QActYear-BOTTOM20QUANT$QprevYear)/BOTTOM20QUANT$QprevYear)),"",(BOTTOM20QUANT$QActYear-BOTTOM20QUANT$QprevYear)/BOTTOM20QUANT$QprevYear))
BOTTOM20QUANT$QuantityDiffYYPER <- ifelse(is.infinite(((BOTTOM20QUANT$QActYear-BOTTOM20QUANT$QprevYear)/BOTTOM20QUANT$QprevYear)),"",scales::percent(((BOTTOM20QUANT$QActYear-BOTTOM20QUANT$QprevYear)/BOTTOM20QUANT$QprevYear),accuracy = 0.1))
BOTTOM20QUANT[is.na(BOTTOM20QUANT)]      <- NA
BOTTOM20QUANT$GPDiffYY          <- BOTTOM20QUANT$GPACT-BOTTOM20QUANT$GPPREV
BOTTOM20QUANT[is.na(BOTTOM20QUANT)]      <- NA
BOTTOM20QUANT$GPDiffYYPER       <- ifelse(is.infinite(((BOTTOM20QUANT$GPACT-BOTTOM20QUANT$GPPREV)/BOTTOM20QUANT$GPPREV)),"",scales::percent(((BOTTOM20QUANT$GPACT-BOTTOM20QUANT$GPPREV)/BOTTOM20QUANT$GPPREV),accuracy = 0.1))
#BOTTOM20QUANT$GPDiffYYPER       <- round(((BOTTOM20QUANT$GPACT-BOTTOM20QUANT$GPPREV)/BOTTOM20QUANT$GPPREV)*100,2)
BOTTOM20QUANT[is.na(BOTTOM20QUANT)]      <- NA
BOTTOM20QUANT$TurnDiffYY        <- BOTTOM20QUANT$TURNACT-BOTTOM20QUANT$TURNPREV
BOTTOM20QUANT[is.na(BOTTOM20QUANT)]      <- NA
BOTTOM20QUANT$TurnDiffYYPER     <- ifelse(is.infinite(((BOTTOM20QUANT$TURNACT-BOTTOM20QUANT$TURNPREV)/BOTTOM20QUANT$TURNPREV)),"",scales::percent(((BOTTOM20QUANT$TURNACT-BOTTOM20QUANT$TURNPREV)/BOTTOM20QUANT$TURNPREV),accuracy = 0.1))
BOTTOM20QUANT[is.na(BOTTOM20QUANT)]      <- NA
BOTTOM20QUANT$GPMTACT           <- ifelse(is.infinite(BOTTOM20QUANT$GPACT*1000/BOTTOM20QUANT$QActYear),0,BOTTOM20QUANT$GPACT*1000/BOTTOM20QUANT$QActYear)   # x 1000 ?
BOTTOM20QUANT[is.na(BOTTOM20QUANT)]      <- 0
BOTTOM20QUANT$GPMTPREV          <- ifelse(is.infinite(BOTTOM20QUANT$GPPREV*1000/BOTTOM20QUANT$QprevYear),0,BOTTOM20QUANT$GPPREV*1000/BOTTOM20QUANT$QprevYear)
BOTTOM20QUANT[is.na(BOTTOM20QUANT)]      <- 0
BOTTOM20QUANT$GPMTDIFF          <- BOTTOM20QUANT$GPMTACT-BOTTOM20QUANT$GPMTPREV
BOTTOM20QUANT[is.na(BOTTOM20QUANT)]      <- NA
BOTTOM20QUANT$GPMTDIFFPER       <- ifelse(is.infinite((BOTTOM20QUANT$GPMTACT-BOTTOM20QUANT$GPMTPREV)/BOTTOM20QUANT$GPMTPREV),"",scales::percent(((BOTTOM20QUANT$GPMTACT-BOTTOM20QUANT$GPMTPREV)/BOTTOM20QUANT$GPMTPREV),accuracy = 0.1))
BOTTOM20QUANT[is.na(BOTTOM20QUANT)]      <- NA

DT                     <- data.table(BOTTOM20QUANT)
BOTTOM20QUANT                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
BOTTOM20QUANT[is.na(BOTTOM20QUANT)]      <- NA
BOTTOM20QUANT$MaterialGroup       <- stri_replace_all_fixed(BOTTOM20QUANT$MaterialGroup, pattern = c("&","%"), replacement = c("","PER"), vectorize_all = FALSE)
BOTTOM20QUANT                      <- setorder(BOTTOM20QUANT,-QuantityDiffYY)
BOTTOM20QUANT$QuantityDiffYYPER_P <-NULL



Total<-data.frame(MaterialGroup="Total CEE",
                  QActYear          = sum(BOTTOM20QUANT$QActYear),
                  QprevYear         = sum(BOTTOM20QUANT$QprevYear),
                  GPACT             = sum(BOTTOM20QUANT$GPACT),
                  GPPREV            = sum(BOTTOM20QUANT$GPPREV),
                  TURNACT           = sum(BOTTOM20QUANT$TURNACT),
                  TURNPREV          = sum(BOTTOM20QUANT$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(BOTTOM20QUANT$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(BOTTOM20QUANT$GPMTACT),
                  GPMTPREV          = sum(BOTTOM20QUANT$GPMTPREV),
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

BOTTOM20QUANT                  <- BOTTOM20QUANT %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
BOTTOM20QUANT                  <- rbind(BOTTOM20QUANT,Total)

BOTTOM20QUANT[is.na(BOTTOM20QUANT)]  <- ""



# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

BOTTOM20QUANT                 <- BOTTOM20QUANT %>% mutate_if(is.numeric, ~round(., 0))
WyboldowaneWierszeBOTTOM20QUANT <- length(BOTTOM20QUANT$MaterialGroup)




#-BOTTOM20QUANT (YTD)---------------------------------------------

BOTTOM20QUANT_YTD                     <- MAINTOP20 %>% filter(`Calendar month`<= MiesiacAnalizy)
BOTTOM20QUANT_YTD                     <- BOTTOM20QUANT_YTD %>% filter(Mapowanie %in% "BOTTOM Qty.")

BOTTOM20QUANT_YTD                    <- as.data.frame(BOTTOM20QUANT_YTD)
BOTTOM20QUANT_YTD                    <- BOTTOM20QUANT_YTD  %>% group_by(MaterialGroup) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))

BOTTOM20QUANT_YTD[is.na(BOTTOM20QUANT_YTD)]      <- NA
BOTTOM20QUANT_YTD$QuantityDiffYY    <- BOTTOM20QUANT_YTD$QActYear-BOTTOM20QUANT_YTD$QprevYear
BOTTOM20QUANT_YTD[is.na(BOTTOM20QUANT_YTD)]      <- NA
BOTTOM20QUANT_YTD$QuantityDiffYYPER_P<-as.numeric(ifelse(is.infinite(((BOTTOM20QUANT_YTD$QActYear-BOTTOM20QUANT_YTD$QprevYear)/BOTTOM20QUANT_YTD$QprevYear)),"",(BOTTOM20QUANT_YTD$QActYear-BOTTOM20QUANT_YTD$QprevYear)/BOTTOM20QUANT_YTD$QprevYear))
BOTTOM20QUANT_YTD$QuantityDiffYYPER <- ifelse(is.infinite(((BOTTOM20QUANT_YTD$QActYear-BOTTOM20QUANT_YTD$QprevYear)/BOTTOM20QUANT_YTD$QprevYear)),"",scales::percent(((BOTTOM20QUANT_YTD$QActYear-BOTTOM20QUANT_YTD$QprevYear)/BOTTOM20QUANT_YTD$QprevYear),accuracy = 0.1))
BOTTOM20QUANT_YTD[is.na(BOTTOM20QUANT_YTD)]      <- NA
BOTTOM20QUANT_YTD$GPDiffYY          <- BOTTOM20QUANT_YTD$GPACT-BOTTOM20QUANT_YTD$GPPREV
BOTTOM20QUANT_YTD[is.na(BOTTOM20QUANT_YTD)]      <- NA
BOTTOM20QUANT_YTD$GPDiffYYPER       <- ifelse(is.infinite(((BOTTOM20QUANT_YTD$GPACT-BOTTOM20QUANT_YTD$GPPREV)/BOTTOM20QUANT_YTD$GPPREV)),"",scales::percent(((BOTTOM20QUANT_YTD$GPACT-BOTTOM20QUANT_YTD$GPPREV)/BOTTOM20QUANT_YTD$GPPREV),accuracy = 0.1))
#BOTTOM20QUANT_YTD$GPDiffYYPER       <- round(((BOTTOM20QUANT_YTD$GPACT-BOTTOM20QUANT_YTD$GPPREV)/BOTTOM20QUANT_YTD$GPPREV)*100,2)
BOTTOM20QUANT_YTD[is.na(BOTTOM20QUANT_YTD)]      <- NA
BOTTOM20QUANT_YTD$TurnDiffYY        <- BOTTOM20QUANT_YTD$TURNACT-BOTTOM20QUANT_YTD$TURNPREV
BOTTOM20QUANT_YTD[is.na(BOTTOM20QUANT_YTD)]      <- NA
BOTTOM20QUANT_YTD$TurnDiffYYPER     <- ifelse(is.infinite(((BOTTOM20QUANT_YTD$TURNACT-BOTTOM20QUANT_YTD$TURNPREV)/BOTTOM20QUANT_YTD$TURNPREV)),"",scales::percent(((BOTTOM20QUANT_YTD$TURNACT-BOTTOM20QUANT_YTD$TURNPREV)/BOTTOM20QUANT_YTD$TURNPREV),accuracy = 0.1))
BOTTOM20QUANT_YTD[is.na(BOTTOM20QUANT_YTD)]      <- NA
BOTTOM20QUANT_YTD$GPMTACT           <- ifelse(is.infinite(BOTTOM20QUANT_YTD$GPACT*1000/BOTTOM20QUANT_YTD$QActYear),0,BOTTOM20QUANT_YTD$GPACT*1000/BOTTOM20QUANT_YTD$QActYear)   # x 1000 ?
BOTTOM20QUANT_YTD[is.na(BOTTOM20QUANT_YTD)]      <- 0
BOTTOM20QUANT_YTD$GPMTPREV          <- ifelse(is.infinite(BOTTOM20QUANT_YTD$GPPREV*1000/BOTTOM20QUANT_YTD$QprevYear),0,BOTTOM20QUANT_YTD$GPPREV*1000/BOTTOM20QUANT_YTD$QprevYear)
BOTTOM20QUANT_YTD[is.na(BOTTOM20QUANT_YTD)]      <- 0
BOTTOM20QUANT_YTD$GPMTDIFF          <- BOTTOM20QUANT_YTD$GPMTACT-BOTTOM20QUANT_YTD$GPMTPREV
BOTTOM20QUANT_YTD[is.na(BOTTOM20QUANT_YTD)]      <- NA
BOTTOM20QUANT_YTD$GPMTDIFFPER       <- ifelse(is.infinite((BOTTOM20QUANT_YTD$GPMTACT-BOTTOM20QUANT_YTD$GPMTPREV)/BOTTOM20QUANT_YTD$GPMTPREV),"",scales::percent(((BOTTOM20QUANT_YTD$GPMTACT-BOTTOM20QUANT_YTD$GPMTPREV)/BOTTOM20QUANT_YTD$GPMTPREV),accuracy = 0.1))
BOTTOM20QUANT_YTD[is.na(BOTTOM20QUANT_YTD)]      <- NA

DT                     <- data.table(BOTTOM20QUANT_YTD)
BOTTOM20QUANT_YTD                   <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
BOTTOM20QUANT_YTD[is.na(BOTTOM20QUANT_YTD)]      <- NA
BOTTOM20QUANT_YTD$MaterialGroup       <- stri_replace_all_fixed(BOTTOM20QUANT_YTD$MaterialGroup, pattern = c("&","%"), replacement = c("","PER"), vectorize_all = FALSE)
BOTTOM20QUANT_YTD                      <- setorder(BOTTOM20QUANT_YTD,-QuantityDiffYY)
BOTTOM20QUANT_YTD$QuantityDiffYYPER_P <-NULL



Total<-data.frame(MaterialGroup="Total CEE",
                  QActYear          = sum(BOTTOM20QUANT_YTD$QActYear),
                  QprevYear         = sum(BOTTOM20QUANT_YTD$QprevYear),
                  GPACT             = sum(BOTTOM20QUANT_YTD$GPACT),
                  GPPREV            = sum(BOTTOM20QUANT_YTD$GPPREV),
                  TURNACT           = sum(BOTTOM20QUANT_YTD$TURNACT),
                  TURNPREV          = sum(BOTTOM20QUANT_YTD$TURNPREV),
                  QuantityDiffYY    = 0,
                  QuantityDiffYYPER = 0,
                  GPDiffYYPER       = 0,
                  GPDiffYY          = 0,
                  TurnDiffYY        = sum(BOTTOM20QUANT_YTD$TurnDiffYY),
                  TurnDiffYYPER     = 0,
                  GPMTACT           = sum(BOTTOM20QUANT_YTD$GPMTACT),
                  GPMTPREV          = sum(BOTTOM20QUANT_YTD$GPMTPREV),
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

BOTTOM20QUANT_YTD                  <- BOTTOM20QUANT_YTD %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
BOTTOM20QUANT_YTD                  <- rbind(BOTTOM20QUANT_YTD,Total)

BOTTOM20QUANT_YTD[is.na(BOTTOM20QUANT_YTD)]  <- ""



# formatowanie liczby do liczby znaków   !!!! MEGAAAAA

BOTTOM20QUANT_YTD                      <- BOTTOM20QUANT_YTD %>% mutate_if(is.numeric, ~round(., 0))
WyboldowaneWierszeBOTTOM20QUANT_YTD    <- length(BOTTOM20QUANT_YTD$MaterialGroup)



