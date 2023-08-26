# nie trzeba podawać w argumentach funkcji listy kolumn grupujących i grupowanych
# ponieważ wiadomo, że 6 ostatnich kolumn to zawsze kolumny grupowane, pozostałe więc są grupującymi.

### 1. TOTAL RESULTS 

MainTotal                   <- MainSource
MainTotal                   <- MainTotal %>% filter(`Calendar month`==MiesiacAnalizy)
#MainTotal                   <- MainTotal %>% select(18,19,12,13,14,15,16,17)
MainTotal                   <- MainTotal %>% select(19,12,13,14,15,16,17)
#T_MainTotal                 <- MainTotal

### 2. TOTAL RESULTS YTD

MainTotalYTD                <- MainSource
MainTotalYTD                <- MainTotalYTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)   
#MainTotalYTD                <- MainTotalYTD %>% select(18,19,12,13,14,15,16,17)
MainTotalYTD                <- MainTotalYTD %>% select(19,12,13,14,15,16,17)
#T_MainTotalYTD              <- MainTotalYTD 

### 3. TOTAL RESULTS (CHARTS)

### RESULTS BY COUNTRY ES/SP MATERIAL TYPE

MAIN_ESSP                   <- MainSource
MAIN_ESSP                   <- MAIN_ESSP %>% filter(`Calendar month`==MiesiacAnalizy)
#MAIN_ESSP                   <- MAIN_ESSP %>% select(18,19,21,12,13,14,15,16,17)
MAIN_ESSP                   <- MAIN_ESSP %>% select(19,21,12,13,14,15,16,17)
#T_MAIN_ESSP                 <- MAIN_ESSP 

### RESULTS BY COUNTRY ES/SP MATERIAL TYPE (YTD)

MAIN_ESSP_YTD                   <- MainSource
MAIN_ESSP_YTD                   <- MAIN_ESSP_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
#MAIN_ESSP_YTD                   <- MAIN_ESSP_YTD %>% select(18,19,21,12,13,14,15,16,17)
MAIN_ESSP_YTD                   <- MAIN_ESSP_YTD %>% select(19,21,12,13,14,15,16,17)
#T_MAIN_ESSP_YTD                 <- MAIN_ESSP_YTD 

### RESULTS BY COUNTRY WAREHOUSE / Direct

MAIN_WhDir_MTH                   <- MainSourceWhDir 
MAIN_WhDir_MTH                   <- MAIN_WhDir_MTH %>% filter(`Calendar month`==MiesiacAnalizy)
#MAIN_WhDir_MTH                   <- MAIN_WhDir_MTH %>% select(17,18,7,11,12,13,14,15,16)
MAIN_WhDir_MTH                   <- MAIN_WhDir_MTH %>% select(18,7,11,12,13,14,15,16)
#T_MAIN_WhDir_MTH                 <- MAIN_WhDir_MTH


### RESULTS BY COUNTRY WAREHOUSE / Direct (YTD)

MAIN_WhDir_YTD                  <- MainSourceWhDir 

MAIN_WhDir_YTD                   <- MAIN_WhDir_YTD  %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
#MAIN_WhDir_YTD                   <- MAIN_WhDir_YTD  %>% select(17,18,7,11,12,13,14,15,16)
MAIN_WhDir_YTD                   <- MAIN_WhDir_YTD  %>% select(18,7,11,12,13,14,15,16)
#T_MAIN_WhDir_YTD                 <- MAIN_WhDir_YTD

### RESULTS BY EUROPEAN INDUSTRY
MAIN_EUR_IND                        <- PodstawoweZrodloCleaning
MAIN_EUR_IND                        <- MAIN_EUR_IND %>% filter(`Calendar month`==MiesiacAnalizy)
MAIN_EUR_IND                        <- MAIN_EUR_IND %>% select(19,24,11,12,13,14,15,16)
#T_MAIN_EUR_IND                      <- MAIN_EUR_IND  

### RESULTS BY EUROPEAN INDUSTRY (YTD)

MAIN_EUR_IND_Y                   <- PodstawoweZrodloCleaning
MAIN_EUR_IND_Y                   <- MAIN_EUR_IND_Y %>% filter(`Calendar month` %in% 1: MiesiacAnalizy)
MAIN_EUR_IND_Y                   <- MAIN_EUR_IND_Y %>% select(19,24,11,12,13,14,15,16)

### RESULTS BY INDUSTRY AND MATERIAL TYPE

# MAIN_ESSP_T                     <- MainSource
# MAIN_ESSP_T                     <- left_join(MAIN_ESSP_T,MapowanieBranzaIndu,by="European Industry")
# colnames(MAIN_ESSP_T)[3]        <- "Cleaning"
# MAIN_ESSP_T                     <- MAIN_ESSP_T %>% filter(`Calendar month`==MiesiacAnalizy)
# MAIN_ESSP_T$MapowanieBranzaInd  <- ifelse(MAIN_ESSP_T$Cleaning=="Others Industries",MAIN_ESSP_T$MapowanieBranzaInd,MAIN_ESSP_T$Cleaning)
# MAIN_ESSP_T                     <- MAIN_ESSP_T %>% select(22,21,12,13,14,15,16,17)


MAIN_ESSP_T                     <- PodstawoweZrodloCleaning
MAIN_ESSP_T                     <- MAIN_ESSP_T %>% filter(`Calendar month`==MiesiacAnalizy)
MAIN_ESSP_T                     <- MAIN_ESSP_T %>% select(24,20,11,12,13,14,15,16)


### RESULTS BY INDUSTRY AND MATERIAL TYPE (YTD)

MAIN_ESSP_TY                     <- PodstawoweZrodloCleaning
MAIN_ESSP_TY                     <- MAIN_ESSP_TY %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
MAIN_ESSP_TY                     <- MAIN_ESSP_TY %>% select(24,20,11,12,13,14,15,16)


### TOTAL RESULTS BY DIVISIONS

ICCPM_DATA                   <- MainSource
colnames(ICCPM_DATA)[5]      <- "Division"
ICCPM_DATA                   <- left_join(ICCPM_DATA,MapowanieICCPMOthers,by="Division")
ICCPM_DATA                   <- ICCPM_DATA %>% filter(`Calendar month`==MiesiacAnalizy)
T_ICCPM_DATA                 <- ICCPM_DATA 
ICCPM_DATA                   <- ICCPM_DATA %>% select(22,5,12,13,14,15,16,17)

### TOTAL RESULTS BY DIVISIONS (YTD)

ICCPM_DATAY                   <- MainSource
colnames(ICCPM_DATAY)[5]      <- "Division"
ICCPM_DATAY                   <- left_join(ICCPM_DATAY,MapowanieICCPMOthers,by="Division")
ICCPM_DATAY                   <- ICCPM_DATAY %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
T_ICCPM_DATAY                 <- ICCPM_DATAY 
ICCPM_DATAY                   <- ICCPM_DATAY %>% select(22,5,12,13,14,15,16,17)

### TOP 20 MATERIAL GROUPS BY MARGIN

TOP20GP                     <- MAINTOP20 %>% filter(`Calendar month`==MiesiacAnalizy)
TOP20GP                     <- TOP20GP %>% filter(Mapowanie=="TOP GP")
TOP20GP                     <- TOP20GP  %>% select(2,4:9)


#800080
#4B0082

### TOP 20 MATERIAL GROUPS BY MARGIN (YTD)

TOP20GPYTD                     <- MAINTOP20    %>% filter(`Calendar month` %in% 1: MiesiacAnalizy)
TOP20GPYTD                     <- TOP20GPYTD   %>% filter(Mapowanie=="TOP GP")
TOP20GPYTD                     <- TOP20GPYTD   %>% select(2,4:9)

TOP20GPYTD_PLOT                <- aggregate(. ~ MaterialGroup, data = TOP20GPYTD , FUN = sum)
TOP20GPYTD_PLOT$ZMIANA         <- round((TOP20GPYTD_PLOT$`Margin EUR (adj) TOTAL 2021` -TOP20GPYTD_PLOT$`Margin EUR (adj) TOTAL 2022`)/TOP20GPYTD_PLOT$`Margin EUR (adj) TOTAL 2022`,3)*100
TOP20GPYTD_PLOT$ROZNICA        <- TOP20GPYTD_PLOT$`Margin EUR (adj) TOTAL 2021` - TOP20GPYTD_PLOT$`Margin EUR (adj) TOTAL 2022`
TOP20GPYTD_PLOT                <- TOP20GPYTD_PLOT %>% select(1,8,9)
TOP20GPYTD_PLOT                <- TOP20GPYTD_PLOT[order(TOP20GPYTD_PLOT$ZMIANA),] 



### TOP 20 MATERIAL GROUPS BY QUANTITY

TOP20QUANT                     <- MAINTOP20  %>% filter(`Calendar month`==MiesiacAnalizy)
TOP20QUANT                     <- TOP20QUANT %>% filter(Mapowanie=="TOP Qty.")
TOP20QUANT                     <- TOP20QUANT %>% select(2,4:9)
 
TOP20QUANT_PLOT                <- aggregate(. ~ MaterialGroup, data = TOP20QUANT , FUN = sum)
TOP20QUANT_PLOT$ZMIANA         <- round((TOP20QUANT_PLOT$`Margin EUR (adj) TOTAL 2021` -TOP20QUANT_PLOT$`Margin EUR (adj) TOTAL 2022`)/TOP20QUANT_PLOT$`Margin EUR (adj) TOTAL 2022`,3)*100
TOP20QUANT_PLOT$ROZNICA        <- TOP20QUANT_PLOT$`Margin EUR (adj) TOTAL 2021` - TOP20QUANT_PLOT$`Margin EUR (adj) TOTAL 2022`
TOP20QUANT_PLOT                <- TOP20QUANT_PLOT %>% select(1,8,9)
TOP20QUANT_PLOT                <- TOP20QUANT_PLOT[order(TOP20QUANT_PLOT$ZMIANA),] 





### TOP 20 MATERIAL GROUPS BY QUANTITY (YTD)

TOP20QUANT_YTD                <- MAINTOP20      %>% filter(`Calendar month` %in% 1: MiesiacAnalizy)
TOP20QUANT_YTD                <- TOP20QUANT_YTD %>% filter(Mapowanie=="TOP Qty.")
TOP20QUANT_YTD                <- TOP20QUANT_YTD %>% select(2,4:9)

TOP20QUANT_YTD_PLOT                <- aggregate(. ~ MaterialGroup, data = TOP20QUANT_YTD , FUN = sum)
TOP20QUANT_YTD_PLOT$ZMIANA         <- round((TOP20QUANT_YTD_PLOT$`Margin EUR (adj) TOTAL 2021` -TOP20QUANT_YTD_PLOT$`Margin EUR (adj) TOTAL 2022`)/TOP20QUANT_YTD_PLOT$`Margin EUR (adj) TOTAL 2022`,3)*100
TOP20QUANT_YTD_PLOT$ROZNICA        <- TOP20QUANT_YTD_PLOT$`Margin EUR (adj) TOTAL 2021` - TOP20QUANT_YTD_PLOT$`Margin EUR (adj) TOTAL 2022`
TOP20QUANT_YTD_PLOT                <- TOP20QUANT_YTD_PLOT %>% select(1,8,9)
TOP20QUANT_YTD_PLOT                <- TOP20QUANT_YTD_PLOT[order(TOP20QUANT_YTD_PLOT$ZMIANA),] 


### BOTTOM 20 MATERIAL GROUPS BY GP YY CHANGES

BOTTOM20GP                     <- MAINTOP20 %>% filter(`Calendar month`==MiesiacAnalizy)
BOTTOM20GP                     <- BOTTOM20GP %>% filter(Mapowanie=="BOTTOM GP")
BOTTOM20GP                     <- BOTTOM20GP %>% select(2,4:9)

BOTTOM20GP_PLOT                <- aggregate(. ~ MaterialGroup, data = BOTTOM20GP , FUN = sum)
BOTTOM20GP_PLOT$ZMIANA         <- round((BOTTOM20GP_PLOT$`Margin EUR (adj) TOTAL 2021` -BOTTOM20GP_PLOT$`Margin EUR (adj) TOTAL 2022`)/BOTTOM20GP_PLOT$`Margin EUR (adj) TOTAL 2022`,3)*100
BOTTOM20GP_PLOT$ROZNICA        <- BOTTOM20GP_PLOT$`Margin EUR (adj) TOTAL 2021` - BOTTOM20GP_PLOT$`Margin EUR (adj) TOTAL 2022`
BOTTOM20GP_PLOT                <- BOTTOM20GP_PLOT %>% select(1,8,9)
BOTTOM20GP_PLOT                <- BOTTOM20GP_PLOT[order(BOTTOM20GP_PLOT$ZMIANA),] 


### BOTTOM 20 MATERIAL GROUPS BY GP YY CHANGES (YTD)

BOTTOM20GP_YTD                 <- MAINTOP20 %>% filter(`Calendar month` %in% 1: MiesiacAnalizy)
BOTTOM20GP_YTD                 <- BOTTOM20GP_YTD %>% filter(Mapowanie=="BOTTOM GP")
BOTTOM20GP_YTD                 <- BOTTOM20GP_YTD  %>% select(2,4:9)


BOTTOM20GP_YTD_PLOT                <- aggregate(. ~ MaterialGroup, data = BOTTOM20GP , FUN = sum)
BOTTOM20GP_YTD_PLOT$ZMIANA         <- round((BOTTOM20GP_YTD_PLOT$`Margin EUR (adj) TOTAL 2021` -BOTTOM20GP_YTD_PLOT$`Margin EUR (adj) TOTAL 2022`)/BOTTOM20GP_YTD_PLOT$`Margin EUR (adj) TOTAL 2022`,3)*100
BOTTOM20GP_YTD_PLOT$ROZNICA        <- BOTTOM20GP_YTD_PLOT$`Margin EUR (adj) TOTAL 2021` - BOTTOM20GP_YTD_PLOT$`Margin EUR (adj) TOTAL 2022`
BOTTOM20GP_YTD_PLOT                <- BOTTOM20GP_YTD_PLOT %>% select(1,8,9)
BOTTOM20GP_YTD_PLOT                <- BOTTOM20GP_YTD_PLOT[order(BOTTOM20GP_YTD_PLOT$ZMIANA),] 


### BOTTOM 20 MATERIAL GROUPS BY QUANTITY YY CHANGES 

BOTTOM20QUANT                  <- MAINTOP20 %>% filter(`Calendar month`==MiesiacAnalizy)
BOTTOM20QUANT                  <- BOTTOM20QUANT %>% filter(Mapowanie=="BOTTOM Qty.")
BOTTOM20QUANT                  <- BOTTOM20QUANT %>% select(2,4:9)

BOTTOM20QUANT_PLOT                <- aggregate(. ~ MaterialGroup, data = BOTTOM20QUANT , FUN = sum)
BOTTOM20QUANT_PLOT$ZMIANA         <- round((BOTTOM20QUANT_PLOT$`Margin EUR (adj) TOTAL 2021` -BOTTOM20QUANT_PLOT$`Margin EUR (adj) TOTAL 2022`)/BOTTOM20QUANT_PLOT$`Margin EUR (adj) TOTAL 2022`,3)*100
BOTTOM20QUANT_PLOT$ROZNICA        <- BOTTOM20QUANT_PLOT$`Margin EUR (adj) TOTAL 2021` - BOTTOM20QUANT_PLOT$`Margin EUR (adj) TOTAL 2022`
BOTTOM20QUANT_PLOT                <- BOTTOM20QUANT_PLOT %>% select(1,8,9)
BOTTOM20QUANT_PLOT                <- BOTTOM20QUANT_PLOT[order(BOTTOM20QUANT_PLOT$ZMIANA),] 


### BOTTOM 20 MATERIAL GROUPS BY QUANTITY YY CHANGES (YTD)

BOTTOM20QUANT_YTD              <- MAINTOP20 %>% filter(`Calendar month` %in% 1: MiesiacAnalizy)
BOTTOM20QUANT_YTD              <- BOTTOM20QUANT_YTD  %>% filter(Mapowanie=="BOTTOM Qty.")
BOTTOM20QUANT_YTD              <- BOTTOM20QUANT_YTD %>% select(2,4:9)

BOTTOM20QUANT_YTD_PLOT                <- aggregate(. ~ MaterialGroup, data = BOTTOM20QUANT_YTD , FUN = sum)
BOTTOM20QUANT_YTD_PLOT$ZMIANA         <- round((BOTTOM20QUANT_YTD_PLOT$`Margin EUR (adj) TOTAL 2021` -BOTTOM20QUANT_YTD_PLOT$`Margin EUR (adj) TOTAL 2022`)/BOTTOM20QUANT_YTD_PLOT$`Margin EUR (adj) TOTAL 2022`,3)*100
BOTTOM20QUANT_YTD_PLOT$ROZNICA        <- BOTTOM20QUANT_YTD_PLOT$`Margin EUR (adj) TOTAL 2021` - BOTTOM20QUANT_YTD_PLOT$`Margin EUR (adj) TOTAL 2022`
BOTTOM20QUANT_YTD_PLOT                <- BOTTOM20QUANT_YTD_PLOT %>% select(1,8,9)
BOTTOM20QUANT_YTD_PLOT                <- BOTTOM20QUANT_YTD_PLOT[order(BOTTOM20QUANT_YTD_PLOT$ZMIANA),] 


### CEEKAT/EKAT RESULTS BY COUNTRY

EKAT             <- rbind(EKATCEEKAT,EKATCEEKAT_CEE)
EKAT             <- EKAT %>% filter(`Calendar month`==MiesiacAnalizy)
EKAT             <- EKAT[,-c(1:2)]

### CEEKAT/EKAT RESULTS BY COUNTRY (YTD)

EKAT_YTD         <- rbind(EKATCEEKAT,EKATCEEKAT_CEE)
EKAT_YTD         <- EKAT_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
EKAT_YTD         <- EKAT_YTD[,-c(1:2)]

### CEEKAT/EKAT RESULTS BY CUSTOMER

EKAT_CUSTOMERS   <- EKAT_CUS
EKAT_CUSTOMERS   <- EKAT_CUSTOMERS  %>% filter(`Calendar month`==MiesiacAnalizy)
EKAT_CUSTOMERS   <- EKAT_CUSTOMERS[,-c(2,4,5)]
EKAT_CUSTOMERS   <- EKAT_CUSTOMERS  %>% select(2,1,3,4,5,6,7,8)

### CEEKAT/EKAT RESULTS BY CUSTOMER (YTD)

EKAT_CUSTOMERS_YTD         <- EKAT_CUS
EKAT_CUSTOMERS_YTD         <- EKAT_CUSTOMERS_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
EKAT_CUSTOMERS_YTD         <- EKAT_CUSTOMERS_YTD[,-c(2,4,5)]
EKAT_CUSTOMERS_YTD         <- EKAT_CUSTOMERS_YTD   %>% select(2,1,3:8)

### BBS RESULTS
#### BY COUNTRY

BBSM                       <- BBS %>% filter(`Calendar month`==MiesiacAnalizy)
BBSM                       <- BBSM %>% select(18,11:16)
### BBS RESULTS (YTD)
#### BY COUNTRY

BBSM_YTD                   <- BBS %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
BBSM_YTD                   <- BBSM_YTD %>% select(18,11:16)

#### BY EUROPEAN INDUSTRY

BBS_EI                     <- BBS
BBS_EI                     <- BBS_EI %>% filter(`Calendar month`==MiesiacAnalizy)
BBS_EI                     <- BBS_EI %>% select(22,11,12,13,14,15,16)
ced<-BBS_EI %>% filter(EuropeanIndustry=="Intercompany (inside CEE)")

sum(ced$`* 1.000 EUR...13`)

BBS_EI_YTD                         <- BBS
BBS_EI_YTD                         <- BBS_EI_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
BBS_EI_YTD                         <- left_join(BBS_EI_YTD,MapowanieBranzaIndu,by="European Industry")
BBS_EI_YTD                         <- BBS_EI_YTD %>% select(22,11:16)

#### BY EUROPEAN INDUSTRY BY COUNTRY

BBS_KEY                    <- read_xlsx(here::here("sources",zestawDanych,"DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "BBS",range="AA16:AP2600", col_names = TRUE,na = "NA")
BBS_KEY                    <- BBS_KEY%>% filter_all(any_vars(!is.na(.))) 
BBS_KEY[is.na(BBS_KEY)]    <- 0   # wszystkie pola bez wartości wypełniamy zerami
colnames(BBS_KEY)[3]       <- "KeyBusiness"
BBS_KEY                    <- BBS_KEY %>% filter(`Calendar month`==MiesiacAnalizy)      #   PONIEWAÆ YTD
BBS_KEY <- left_join(BBS_KEY,MapowanieBaltics,by = "Subsidiary Country")
BBS_KEY                    <- BBS_KEY %>% select(18,3,11:16)



#### BY EUROPEAN INDUSTRY (YTD)


#### BY EUROPEAN INDUSTRY BY COUNTRY (YTD)

BBS_KEY_YTD                        <- read_xlsx(here::here("sources",zestawDanych,"DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "BBS",range="AA16:AP2600", col_names = TRUE,na = "NA")
BBS_KEY_YTD                        <- BBS_KEY_YTD %>% filter_all(any_vars(!is.na(.))) 
BBS_KEY_YTD[is.na(BBS_KEY_YTD)]    <- 0   # wszystkie pola bez wartości wypełniamy zerami
colnames(BBS_KEY_YTD)[3]           <- "KeyBusiness"
BBS_KEY_YTD                        <- BBS_KEY_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)      #   PONIEWAÆ YTD
BBS_KEY_YTD <- left_join(BBS_KEY_YTD,MapowanieBaltics,by = "Subsidiary Country")
BBS_KEY_YTD                        <- BBS_KEY_YTD %>% select(18,3,11:16)


#### WATER TREATMENT: WATER TREATMENT INDUSTRY + CROSS SELLING

WT_TOTAL                           <- WTMain %>% subset(WaterClass=="WATER TREATMENT BUSINESS Summary")
WT_TOTAL                           <- WT_TOTAL %>% filter(`Calendar month`==MiesiacAnalizy)
WT_TOTAL                           <- WT_TOTAL %>% select(18,11,12,13,14,15,16)
#T_WT_TOTAL                         <- WT_TOTAL

#### WATER TREATMENT: WATER TREATMENT INDUSTRY + CROSS SELLING (YTD)

WT_TOTAL_YTD                       <- WTMain %>% subset(WaterClass=="WATER TREATMENT BUSINESS Summary")
WT_TOTAL_YTD                       <- WT_TOTAL_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
WT_TOTAL_YTD                       <- WT_TOTAL_YTD %>% select(18,11,12,13,14,15,16)


### WATER TREATMENT: CROSS SELLING BY COUNTRY

WT_CROSS                           <- WTMain %>% subset(WaterClass=="WATER TREATMENT - Cross Selling")
WT_CROSS                           <- WT_CROSS %>% filter(`Calendar month`==MiesiacAnalizy)
WT_CROSS                           <- WT_CROSS %>% select(18,11,12,13,14,15,16)


### WATER TREATMENT: CROSS SELLING BY COUNTRY (YTD)

WT_CROSS_YTD                   <- WTMain %>% subset(WaterClass=="WATER TREATMENT - Cross Selling")
WT_CROSS_YTD                   <- WT_CROSS_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
WT_CROSS_YTD                   <- WT_CROSS_YTD %>% select(18,11,12,13,14,15,16)


### WATER TREATMENT: CROSS SELLING BY SALES PART/CHANNEL

WT_THIRD                   <- WTMain %>% subset(WaterClass=="WATER TREATMENT - Cross Selling")
WT_THIRD                   <- WT_THIRD %>% filter(`Calendar month`==MiesiacAnalizy)
WT_THIRD                   <- WT_THIRD %>% select(18,19,11,12,13,14,15,16)


### WATER TREATMENT: CROSS SELLING BY SALES PART/CHANNEL (YTD)

WT_THIRD_YTD                   <- WTMain %>% subset(WaterClass=="WATER TREATMENT - Cross Selling")
WT_THIRD_YTD                   <- WT_THIRD_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
WT_THIRD_YTD                   <- WT_THIRD_YTD %>% select(18,19,11,12,13,14,15,16)
   

###  WATER TREATMENT: CROSS SELLING BY INDUSTRY

WT_CS_IND                   <- WTMain %>% subset(WaterClass=="WATER TREATMENT - Cross Selling")
WT_CS_IND                   <- WT_CS_IND %>% filter(`Calendar month`==MiesiacAnalizy)
WT_CS_IND                   <- WT_CS_IND %>% select(4,11,12,13,14,15,16)


### WATER TREATMENT: CROSS SELLING BY INDUSTRY (YTD)

WT_CS_IND_YTD                   <- WTMain %>% subset(WaterClass=="WATER TREATMENT - Cross Selling")
WT_CS_IND_YTD                   <- WT_CS_IND_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
WT_CS_IND_YTD                   <- WT_CS_IND_YTD %>% select(4,11,12,13,14,15,16)


### WATER TREATMENT INDUSTRY BY COUNTRY

WT_INDUSTRY                   <- WTMain %>% subset(WaterClass=="WATER TREATMENT - Industry")
WT_INDUSTRY                   <- WT_INDUSTRY %>% filter(`Calendar month`==MiesiacAnalizy)
WT_INDUSTRY                   <- WT_INDUSTRY %>% select(18,11,12,13,14,15,16)
 

### WATER TREATMENT INDUSTRY BY COUNTRY (YTD)

WT_INDUSTRY_YTD                   <- WTMain %>% subset(WaterClass=="WATER TREATMENT - Industry")
WT_INDUSTRY_YTD                   <- WT_INDUSTRY_YTD %>% filter(`Calendar month` %in% 1: MiesiacAnalizy)
WT_INDUSTRY_YTD                   <- WT_INDUSTRY_YTD %>% select(18,11,12,13,14,15,16)


### TOTAL RESULTS COUNTRY BY SALES PARTY

SALES_MAIN                       <- MainSource
SALES_MAIN$Clasification         <- " "
SALES_MAIN$Clasification         <- ifelse(SALES_MAIN$Intern=="External",SALES_MAIN$Clasification<-"3rd party",ifelse(SALES_MAIN$`SoldTo Country` %in% MapowanieIntercompany$SoldToCountry,SALES_MAIN$Clasification<-"Intercompany (inside CEE)",SALES_MAIN$Clasification<-"Intercompany (outside CEE)"))
SALES_MAIN                       <- SALES_MAIN %>% filter(`Calendar month`==MiesiacAnalizy)
SALES_MAIN                       <- SALES_MAIN %>% select(19,22,12,13,14,15,16,17)


### TOTAL RESULTS COUNTRY BY SALES PARTY (YTD)

SALES_MAIN_YTD                   <- MainSource
SALES_MAIN_YTD$Clasification     <- ""
SALES_MAIN_YTD$Clasification     <- ifelse(SALES_MAIN_YTD$Intern=="External",SALES_MAIN_YTD$Clasification<-"3rd party",ifelse(SALES_MAIN_YTD$`SoldTo Country` %in% MapowanieIntercompany$SoldToCountry,SALES_MAIN_YTD$Clasification<-"Intercompany (inside CEE)",SALES_MAIN_YTD$Clasification<-"Intercompany (outside CEE)"))
SALES_MAIN_YTD                   <- SALES_MAIN_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
SALES_MAIN_YTD                   <- SALES_MAIN_YTD %>% select(19,22,12,13,14,15,16,17)



### TOTAL RESULTS DIVISION BY SALES PARTY 

TotalDivisionSalesParty               <- MainSource
colnames(TotalDivisionSalesParty)[5]  <- "Division"
TotalDivisionSalesParty               <- left_join(TotalDivisionSalesParty,MapowanieICCPMOthers,by="Division")
TotalDivisionSalesParty$Clasification <- ""
TotalDivisionSalesParty$Clasification <- ifelse(TotalDivisionSalesParty$Intern=="External",TotalDivisionSalesParty$Clasification<-"3rd party",ifelse(TotalDivisionSalesParty$`SoldTo Country` %in% MapowanieIntercompany$SoldToCountry,TotalDivisionSalesParty$Clasification<-"Intercompany (inside CEE)",TotalDivisionSalesParty$Clasification<-"Intercompany (outside CEE)"))
TotalDivisionSalesParty               <- TotalDivisionSalesParty %>% filter(`Calendar month`==MiesiacAnalizy)
TotalDivisionSalesParty               <- TotalDivisionSalesParty %>% select(22,5,23,12,13,14,15,16,17)



### TOTAL RESULTS DIVISION BY SALES PARTY (YTD)

TotalDivisionSalesParty_YTD               <- MainSource
colnames(TotalDivisionSalesParty_YTD)[5]  <- "Division"
TotalDivisionSalesParty_YTD               <- left_join(TotalDivisionSalesParty_YTD,MapowanieICCPMOthers,by="Division")
TotalDivisionSalesParty_YTD$Clasification <- ""
TotalDivisionSalesParty_YTD$Clasification <- ifelse(TotalDivisionSalesParty_YTD$Intern=="External",TotalDivisionSalesParty_YTD$Clasification<-"3rd party",ifelse(TotalDivisionSalesParty_YTD$`SoldTo Country` %in% MapowanieIntercompany$SoldToCountry,TotalDivisionSalesParty_YTD$Clasification<-"Intercompany (inside CEE)",TotalDivisionSalesParty_YTD$Clasification<-"Intercompany (outside CEE)"))
TotalDivisionSalesParty_YTD               <- TotalDivisionSalesParty_YTD %>% filter(`Calendar month` %in% 1: MiesiacAnalizy)
TotalDivisionSalesParty_YTD               <- TotalDivisionSalesParty_YTD %>% select(22,5,23,12,13,14,15,16,17)



###CEE Region results - Divisions by Country 

DivisionsByCountry                    <- MainSource
colnames(DivisionsByCountry )[5]      <- "Division"
DivisionsByCountry                    <- left_join(DivisionsByCountry ,MapowanieICCPMOthers,by="Division")
DivisionsByCountry                    <- DivisionsByCountry  %>% filter(`Calendar month`==MiesiacAnalizy)
DivisionsByCountry                    <- DivisionsByCountry  %>% select(22,5,19,12,13,14,15,16,17)


###CEE Region results - Divisions by Country (YTD)

DivisionsByCountry_YTD                <- MainSource
colnames(DivisionsByCountry_YTD)[5]   <- "Division"
DivisionsByCountry_YTD                <- left_join(DivisionsByCountry_YTD  ,MapowanieICCPMOthers,by="Division")
DivisionsByCountry_YTD                <- DivisionsByCountry_YTD  %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
DivisionsByCountry_YTD                <- DivisionsByCountry_YTD  %>% select(22,5,19,12,13,14,15,16,17)


### CEE Region results - Industry by Country 

IndustryByCountry                <- PodstawoweZrodloCleaning
# IndustryByCountry                <- left_join(IndustryByCountry,MapowanieBranzaIndu,by="European Industry")
IndustryByCountry                <- IndustryByCountry  %>% filter(`Calendar month` ==MiesiacAnalizy)
IndustryByCountry                <- IndustryByCountry  %>% select(24,18,11,12,13,14,15,16)


### CEE Region results - Industry by Country (YTD)

IndustryByCountry_YTD                <- PodstawoweZrodloCleaning
#IndustryByCountry_YTD                <- left_join(IndustryByCountry_YTD,MapowanieBranzaIndu,by="European Industry")
IndustryByCountry_YTD                <- IndustryByCountry_YTD  %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
IndustryByCountry_YTD                <- IndustryByCountry_YTD   %>% select(24,18,11,12,13,14,15,16)



### CEE Region results - Countries by Division

CountriesByDivision               <- MainSource
colnames(CountriesByDivision)[5]  <- "Division"
CountriesByDivision               <- left_join(CountriesByDivision,MapowanieICCPMOthers,by="Division")
CountriesByDivision               <- CountriesByDivision  %>% filter(`Calendar month` == MiesiacAnalizy)
CountriesByDivision               <- CountriesByDivision  %>% select(22,19,5,12,13,14,15,16,17)

### CEE Region results - Countries by Division (YTD)

CountriesByDivision_YTD                <- MainSource
colnames(CountriesByDivision_YTD)[5]       <- "Division"
CountriesByDivision_YTD                <- left_join(CountriesByDivision_YTD ,MapowanieICCPMOthers,by="Division")
CountriesByDivision_YTD                <- CountriesByDivision_YTD   %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
CountriesByDivision_YTD                <- CountriesByDivision_YTD   %>% select(22,19,5,12,13,14,15,16,17)


### CEE Region results - Countries by Industries


CountriesByIndustries               <- PodstawoweZrodloCleaning
CountriesByIndustries               <- CountriesByIndustries  %>% filter(`Calendar month` == MiesiacAnalizy)
CountriesByIndustries               <- CountriesByIndustries  %>% select(18,24,11,12,13,14,15,16)



### CEE Region results - Countries by Industries (YTD)

CountriesByIndustries_YTD           <- PodstawoweZrodloCleaning
#CountriesByIndustries_YTD           <- left_join(CountriesByIndustries_YTD,MapowanieBranzaIndu,by="European Industry")
CountriesByIndustries_YTD           <- CountriesByIndustries_YTD    %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
CountriesByIndustries_YTD           <- CountriesByIndustries_YTD   %>% select(18,24,11,12,13,14,15,16)


### BESBSP Total results by Product Division & material Type

BESBSP_PD_MT                        <- MainSource
#BESBSP_PD_MT                       <- left_join(BESBSP_PD_MT  ,MapowanieBranzaIndu,by="European Industry")
BESBSP_PD_MT                        <- BESBSP_PD_MT    %>% filter(`Calendar month` == MiesiacAnalizy)
BESBSP_PD_MT                        <- BESBSP_PD_MT    %>% select(7,21,12,13,14,15,16,17)

### BESBSP Total results by Product Division & material Type (YTD)

BESBSP_PD_MT_YTD                    <- MainSource
#BESBSP_PD_MT_YTD                   <- left_join(BESBSP_PD_MT_YTD,MapowanieBranzaIndu,by="European Industry")
BESBSP_PD_MT_YTD                    <- BESBSP_PD_MT_YTD    %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
BESBSP_PD_MT_YTD                    <- BESBSP_PD_MT_YTD   %>% select(7,21,12,13,14,15,16,17)


### BESBSP Country results by Division

BESBSP_CRD                   <- MainSource
#BESBSP_CRD                <- left_join(BESBSP_CRD  ,MapowanieBranzaIndu,by="European Industry")
BESBSP_CRD                 <- BESBSP_CRD    %>% filter(`Calendar month` == MiesiacAnalizy)
BESBSP_CRD                 <- BESBSP_CRD    %>% select(19,7,12,13,14,15,16,17)

### BESBSP Country results by Division (YTD)

BESBSP_CRD_YTD             <- MainSource
#BESBSP_CRD_YTD          <- left_join(BESBSP_CRD_YTD,MapowanieBranzaIndu,by="European Industry")
BESBSP_CRD_YTD           <- BESBSP_CRD_YTD    %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
BESBSP_CRD_YTD           <- BESBSP_CRD_YTD   %>% select(19,7,12,13,14,15,16,17)



### BESBSP Subsidiaries results by Product Division

BESBSP_SR                 <- MainSource
#BESBSP_SR                <- left_join(BESBSP_SR  ,MapowanieBranzaIndu,by="European Industry")
BESBSP_SR                 <- BESBSP_SR    %>% filter(`Calendar month` == MiesiacAnalizy)
BESBSP_SR                 <- BESBSP_SR    %>% select(19,2,7,12,13,14,15,16,17)

### BESBSP Subsidiaries results by Product Division (YTD)

BESBSP_SR_YTD           <- MainSource
#BESBSP_SR_YTD          <- left_join(BESBSP_SR_YTD,MapowanieBranzaIndu,by="European Industry")
BESBSP_SR_YTD           <- BESBSP_SR_YTD    %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
BESBSP_SR_YTD           <- BESBSP_SR_YTD   %>% select(19,2,7,12,13,14,15,16,17)


### BESBSP IC-CPM groups results by PRODUCT DIVISION & MATERIAL TYPE

BESBSP_ICCPM                    <-  MainSource
colnames(BESBSP_ICCPM)[5]       <- "Division"
BESBSP_ICCPM                    <- left_join(BESBSP_ICCPM,MapowanieICCPMOthers,by="Division")
BESBSP_ICCPM                    <- BESBSP_ICCPM   %>% filter(`Calendar month` == MiesiacAnalizy)
BESBSP_ICCPM                    <- BESBSP_ICCPM  %>% filter(Mapowanie=="IC-CPM") %>%  select(5,7,21,12,13,14,15,16,17)

### BESBSP IC-CPM groups results by PRODUCT DIVISION & MATERIAL TYPE (YTD)

BESBSP_ICCPM_YTD                    <-  MainSource
colnames(BESBSP_ICCPM_YTD)[5]       <- "Division"
BESBSP_ICCPM_YTD                    <- left_join(BESBSP_ICCPM_YTD,MapowanieICCPMOthers,by="Division")
BESBSP_ICCPM_YTD                    <- BESBSP_ICCPM_YTD   %>% filter(`Calendar month` %in%  1:MiesiacAnalizy)
BESBSP_ICCPM_YTD                    <- BESBSP_ICCPM_YTD  %>% filter(Mapowanie=="IC-CPM") %>%  select(5,7,21,12,13,14,15,16,17)

### BESBSP IC-CPM groups results by PRODUCT DIVISION & MATERIAL TYPE

BESBSP_PDICCPM                     <-  MainSource
colnames(BESBSP_PDICCPM )[5]       <- "Division"
BESBSP_PDICCPM                     <- left_join(BESBSP_PDICCPM ,MapowanieICCPMOthers,by="Division")
BESBSP_PDICCPM                     <- BESBSP_PDICCPM    %>% filter(`Calendar month` == MiesiacAnalizy)
BESBSP_PDICCPM                     <- BESBSP_PDICCPM   %>% filter(Mapowanie=="IC-CPM") %>%  select(7,5,21,12,13,14,15,16,17)

### BESBSP IC-CPM groups results by PRODUCT DIVISION & MATERIAL TYPE


BESBSP_PDICCPM_YTD                     <-  MainSource
colnames(BESBSP_PDICCPM_YTD )[5]       <- "Division"
BESBSP_PDICCPM_YTD                     <- left_join(BESBSP_PDICCPM_YTD ,MapowanieICCPMOthers,by="Division")
BESBSP_PDICCPM_YTD                     <- BESBSP_PDICCPM_YTD    %>% filter(`Calendar month` %in%  1:MiesiacAnalizy)
BESBSP_PDICCPM_YTD                     <- BESBSP_PDICCPM_YTD   %>% filter(Mapowanie=="IC-CPM") %>%  select(7,5,21,12,13,14,15,16,17)

### BESBSP INDUSTRY RESULTS BY PRODUCT DIVISION

BESBSP_IRPD                            <- MainSource
BESBSP_IRPD                            <- left_join(BESBSP_IRPD,MapowanieBranzaIndu,by="European Industry")
colnames(BESBSP_IRPD)[3]               <- "Cleaning"
BESBSP_IRPD                            <- BESBSP_IRPD %>% filter(`Calendar month`==MiesiacAnalizy)
BESBSP_IRPD$MapowanieBranzaInd         <- ifelse(BESBSP_IRPD$Cleaning=="Others Industries",BESBSP_IRPD$MapowanieBranzaInd,BESBSP_IRPD$Cleaning)
BESBSP_IRPD                            <- BESBSP_IRPD %>% select(20,22,7,21,12,13,14,15,16,17)

### BESBSP INDUSTRY RESULTS BY PRODUCT DIVISION (YTD)

BESBSP_IRPD_YTD                        <- MainSource
BESBSP_IRPD_YTD                        <- left_join(BESBSP_IRPD_YTD,MapowanieBranzaIndu,by="European Industry")
colnames(BESBSP_IRPD_YTD)[3]           <- "Cleaning"
BESBSP_IRPD_YTD                        <- BESBSP_IRPD_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
BESBSP_IRPD_YTD$MapowanieBranzaInd     <- ifelse(BESBSP_IRPD_YTD$Cleaning=="Others Industries",BESBSP_IRPD_YTD$MapowanieBranzaInd,BESBSP_IRPD_YTD$Cleaning)
BESBSP_IRPD_YTD                        <- BESBSP_IRPD_YTD %>% select(20,22,7,21,12,13,14,15,16,17)


### BESBSP INDUSTRY RESULTS BY PRODUCT DIVISION

BESBSP_DI                            <- MainSource
BESBSP_DI                            <- left_join(BESBSP_DI,MapowanieBranzaIndu,by="European Industry")
colnames(BESBSP_DI)[3]               <- "Cleaning"
BESBSP_DI                            <- BESBSP_DI %>% filter(`Calendar month`==MiesiacAnalizy)
BESBSP_DI$MapowanieBranzaInd         <- ifelse(BESBSP_DI$Cleaning=="Others Industries",BESBSP_DI$MapowanieBranzaInd,BESBSP_DI$Cleaning)
BESBSP_DI                            <- BESBSP_DI %>% select(7,21,20,22,12,13,14,15,16,17)

### BESBSP INDUSTRY RESULTS BY PRODUCT DIVISION (YTD)

BESBSP_DI_YTD                        <- MainSource
BESBSP_DI_YTD                        <- left_join(BESBSP_DI_YTD,MapowanieBranzaIndu,by="European Industry")
colnames(BESBSP_DI_YTD)[3]           <- "Cleaning"
BESBSP_DI_YTD                        <- BESBSP_DI_YTD %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
BESBSP_DI_YTD$MapowanieBranzaInd     <- ifelse(BESBSP_DI_YTD$Cleaning=="Others Industries",BESBSP_DI_YTD$MapowanieBranzaInd,BESBSP_DI_YTD$Cleaning)
BESBSP_DI_YTD                        <- BESBSP_DI_YTD %>% select(7,21,20,22,12,13,14,15,16,17)


### BESBSP INDUSTRY RESULTS BY PRODUCT DIVISION (YTD)

BESBSP_DI_YTD2                        <- MainSource
BESBSP_DI_YTD2                        <- left_join(BESBSP_DI_YTD2,MapowanieBranzaIndu,by="European Industry")
colnames(BESBSP_DI_YTD2)[3]           <- "Cleaning"
BESBSP_DI_YTD2                        <- BESBSP_DI_YTD2 %>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
BESBSP_DI_YTD2$MapowanieBranzaInd     <- ifelse(BESBSP_DI_YTD2$Cleaning=="Others Industries",BESBSP_DI_YTD2$MapowanieBranzaInd,BESBSP_DI_YTD2$Cleaning)
BESBSP_DI_YTD2                        <- BESBSP_DI_YTD2 %>% select(7,21,20,22,12,13,14,15,16,17)




# Tabele pomocnicze


### FAS PHOSPHATES

PHOSPHATESFAS                          <- MainSourceWhDir 
PHOSPHATESFAS                          <- left_join(PHOSPHATESFAS,MapowanieFAS,by="Central Product Mgr.")
PHOSPHATESFAS                          <- PHOSPHATESFAS %>% filter(`Calendar month`==MiesiacAnalizy)
PHOSPHATESFAS                          <- PHOSPHATESFAS %>% filter(!is.na(Mapowanie)) %>% select(19,11,12,13,14,15,16)

### FAS PHOSPHATES

PHOSPHATESFAS_YTD                      <- MainSourceWhDir 
PHOSPHATESFAS_YTD                      <- left_join(PHOSPHATESFAS_YTD,MapowanieFAS,by="Central Product Mgr.")
PHOSPHATESFAS_YTD                      <- PHOSPHATESFAS_YTD%>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
PHOSPHATESFAS_YTD                      <- PHOSPHATESFAS_YTD %>% filter(!is.na(Mapowanie)) %>% select(19,11,12,13,14,15,16)


PHOSPHATESFAS2                         <- MainSourceWhDir 
PHOSPHATESFAS2                         <- left_join(PHOSPHATESFAS2,MapowanieFAS,by="Central Product Mgr.")
PHOSPHATESFAS2                         <- PHOSPHATESFAS2 %>% filter(`Calendar month`==MiesiacAnalizy)
PHOSPHATESFAS2                         <- PHOSPHATESFAS2 %>% filter(!is.na(Mapowanie)) %>% select(19,18,11,12,13,14,15,16)

### FAS PHOSPHATES

PHOSPHATESFAS_YTD2                          <- MainSourceWhDir 
PHOSPHATESFAS_YTD2                          <- left_join(PHOSPHATESFAS_YTD2,MapowanieFAS,by="Central Product Mgr.")
PHOSPHATESFAS_YTD2                          <- PHOSPHATESFAS_YTD2%>% filter(`Calendar month` %in% 1:MiesiacAnalizy)
PHOSPHATESFAS_YTD2                          <- PHOSPHATESFAS_YTD2 %>% filter(!is.na(Mapowanie)) %>% select(19,18,11,12,13,14,15,16)

PHOSPHATESFAS2a                             <-PHOSPHATESFAS2 %>% select(2,1,3,4,5,6,7,8)
PHOSPHATESFAS_YTD2a                         <-PHOSPHATESFAS_YTD2 %>% select(2,1,3,4,5,6,7,8)





# ?
# ?
# ?
# ?
# ?


# # ustalamy literę, od której mają zaczynać się nazwy zmiennych
# letters_starting_with <- "T_"
# 
# # pobieramy nazwy zmiennych
# names_list <- ls() 
# 
# # filtrujemy tylko te nazwy, które zaczynają się od litery
# selected_names <- grep(paste0("^", letters_starting_with), names_list, value = TRUE) 
# 
# # tworzymy listę zmiennych
# lista_Tabel <- lapply(selected_names, get)
# lista_Tabel<- setNames(lista_Tabel, var_list)
# # wyświetlamy listę zmiennych
# lista_Tabel
# 
# 
# var_list <- ls(pattern = "^T_")
# 
# # umieszczenie nazw zmiennych w liście
# var_list <- setNames(var_list, var_list)
# var_list <- list(var_list)



