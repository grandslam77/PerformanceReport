#---------------- FORMATOWANIE TABEL ---------------------

FormatKolumnyOddzielone           <- c("l|","r","r","r","r|","r","r","r","r|","r","r","r","r|","r","r","r","r|")  # parametr do .rmd wskazujący jak ma wyglądać justowanie i jak mają przebiegać pionowe linie oddzielające
FormatKolumnyNieOddzielone        <- c("lrrrrrrrrrrrrrrrr")
TloTotalTotal                     <- "#E3E3E3"
KolorUjemne                       <- "#A411B8"

ZestawTabelDoSortowaniaPoziom1    <- c()
pAleta                            <- c("#FFEAE4","#FFF6F8","#F9FFE2","#FFE6EC","#F3E9FF","#FFF1FA","#EFF3FF","#FEEAFF","#F6FFE5","#F6FFFF")
pAleta2                           <- c("#E6D3CD","#E3DBDD","#E0E6CB","#E3CDD2","#DBD2E6","#E3D6DF","#D0D3DE","#E2D0E3","#DBE3CC","#D6DEDE")


ShowNotAssigned = TRUE

colnames(MapowanieBaltics)[1]     <- "Subsidiary Country"
colnames(MapowanieBranzaIndu)[1]  <- "European Industry"

#dataAnalizy                       <- as.Date("2023-02-28")
bazaPerformance                   <- readRDS(here::here("sources","bazaPerformance.Rds"))



MiesiacAnalizy                    <- month(dataAnalizy)
RokAnalizy                        <- toupper(year(dataAnalizy))
MiesiacAnalizyTekst               <- as.character(toupper(lubridate::month(dataAnalizy,label = TRUE, abbr = FALSE, locale =  'en_US')))



if(MiesiacAnalizy<3){
  szerokoscWykresuTotalResult     <- 4 # w calach
}else if (MiesiacAnalizy>2& MiesiacAnalizy<5){
  szerokoscWykresuTotalResult     <- 5
}else if (MiesiacAnalizy>4& MiesiacAnalizy<8){
  szerokoscWykresuTotalResult     <- 8
  

}else{szerokoscWykresuTotalResult     <- 10}

Naglowek1                         <- "Quantity KG TOTAL 2021"
Naglowek2                         <- "Quantity KG TOTAL 2022"
Naglowek3                         <- "Margin EUR (adj) TOTAL 2021"
Naglowek4                         <- "Margin EUR (adj) TOTAL 2022"
Naglowek5                         <- "Turnover EUR (adj) TOTAL 2021"
Naglowek6                         <- "Turnover EUR (adj) TOTAL 2022"

sum <- function(x, ..., na.rm = TRUE) {
  base::sum(x, ..., na.rm = na.rm)
}

MainSource                      <- read_xlsx(here::here("sources",zestawDanych,"DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "BES_BSP",range="I16:Y60000", col_names = TRUE,na = "NA")
MainSource                      <- MainSource %>% filter_all(any_vars(!is.na(.))) 
MainSource[is.na(MainSource)]   <- 0   # wszystkie pola bez wartości wypełniamy zerami
MainSource                      <- left_join(MainSource,MapowanieBaltics,by="Subsidiary Country")
MainSource                      <- left_join(MainSource,MapowanieBESBSP,by="European Industry")
MainSource                      <- left_join(MainSource,MapowanieESSP,by="Material Division")
MainSource$`Product Division`[MainSource$`Product Division`=="#"] <- "N|A Div"



MainSourceWhDir                             <- read_xlsx(here::here("sources",zestawDanych,"DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "Cleaning",range="I16:X60000", col_names = TRUE,na = "NA")
MainSourceWhDir                             <- MainSourceWhDir %>% filter_all(any_vars(!is.na(.))) 
MainSourceWhDir  [is.na(MainSourceWhDir)]   <- 0   # wszystkie pola bez wartości wypełniamy zerami
MainSourceWhDir                             <- left_join(MainSourceWhDir,MapowanieBaltics,by="Subsidiary Country")
MainSourceWhDir$WhsDirComm[MainSourceWhDir$WhsDirComm=="Not assigned"] <- "Warehouse"



MAINTOP20                             <- read_xlsx(here::here("sources",zestawDanych,"DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "TOP 20",range="C6:K11000", col_names = TRUE,na = "NA")
MAINTOP20                             <- MAINTOP20  %>% filter_all(any_vars(!is.na(.))) 
MAINTOP20 [is.na(MAINTOP20)]          <- 0   # wszystkie pola bez wartości wypełniamy zerami

colnames(MAINTOP20)[1]                <- "Rodzaj"
colnames(MAINTOP20)[2]                <- "MaterialGroup"

MAINTOP20                             <- left_join(MAINTOP20,MapowanieTOP20,by="Rodzaj")
MAINTOP20                             <- MAINTOP20[!is.na(MAINTOP20$Mapowanie),]
MAINTOP20$`Calendar month`            <- as.numeric(MAINTOP20$`Calendar month`)
MAINTOP20$MaterialGroup               <- gsub("%", "PER", MAINTOP20$MaterialGroup)
MAINTOP20                             <- MAINTOP20 %>% select(1,2,3,5,4,7,6,9,8,10)

Naglowek1                             <- "Quantity KG TOTAL 2021"
Naglowek2                             <- "Quantity KG TOTAL 2022"
Naglowek3                             <- "Margin EUR (adj) TOTAL 2021"
Naglowek4                             <- "Margin EUR (adj) TOTAL 2022"
Naglowek5                             <- "Turnover EUR (adj) TOTAL 2021"
Naglowek6                             <- "Turnover EUR (adj) TOTAL 2022"

colnames(MAINTOP20)[4]                <- Naglowek2
colnames(MAINTOP20)[5]                <- Naglowek1
colnames(MAINTOP20)[6]                <- Naglowek4
colnames(MAINTOP20)[7]                <- Naglowek3
colnames(MAINTOP20)[8]                <- Naglowek6
colnames(MAINTOP20)[9]                <- Naglowek5

# EKAT

EKATCEEKAT                            <- read_xlsx(here::here("sources",zestawDanych,"DEC_Perform.Rep_2019_YTD_EKAT.xlsm"),sheet = "EKAT CEE",range="I16:T3000", col_names = TRUE,na = "NA")
EKATCEEKAT                            <- EKATCEEKAT%>% filter_all(any_vars(!is.na(.))) 
EKATCEEKAT [is.na(EKATCEEKAT)]        <- 0   # wszystkie pola bez wartości wypełniamy zerami


EKATCEEKAT               <- left_join(EKATCEEKAT,MapowanieBaltics,by="Subsidiary Country")

colnames(EKATCEEKAT)[1]     <- "Klient"
colnames(EKATCEEKAT)[7]     <- Naglowek1
colnames(EKATCEEKAT)[8]     <- Naglowek2
colnames(EKATCEEKAT)[9]     <- Naglowek3
colnames(EKATCEEKAT)[10]    <- Naglowek4
colnames(EKATCEEKAT)[11]    <- Naglowek5
colnames(EKATCEEKAT)[12]    <- Naglowek6

MapowanieEKAT[MapowanieEKAT=="RING / HELIOS / REMBRANTIN / CHROMOS / AVRORA"]  <-"RING|HEL.|REM.|CHR.|AVR."
MapowanieEKAT[MapowanieEKAT=="COCA COLA / MULTON"]                             <-"COCACOLA|MULTON"
MapowanieEKAT[MapowanieEKAT=="AGRANA / AUSTRIA JUICE"]                         <-"AGRANA|AUSTRIAJUICE"
MapowanieEKAT[MapowanieEKAT=="LESAFFRE / SAF NEWA"]                            <-"LESAFFRE|SAFNEWA"
MapowanieEKAT[MapowanieEKAT=="MBCC Group (former BASF construction)"]          <-"MBCC Group"

EKATCEEKAT                  <- left_join(EKATCEEKAT,MapowanieEKAT,by="Klient")
EKATCEEKAT$Mapowanie        <- ifelse(is.na(EKATCEEKAT$Mapowanie),EKATCEEKAT$Mapowanie <- EKATCEEKAT$SoldTo,EKATCEEKAT$Mapowanie)

EKATCEEKAT_CEE                           <- read_xlsx(here::here("sources",zestawDanych,"DEC_Perform.Rep_2019_YTD_EKAT.xlsm"),sheet = "CEEKAT",range="I16:T2000", col_names = TRUE,na = "NA")

EKATCEEKAT_CEE                           <- EKATCEEKAT_CEE%>% filter_all(any_vars(!is.na(.))) 
EKATCEEKAT_CEE [is.na(EKATCEEKAT_CEE)]   <- 0   # wszystkie pola bez wartości wypełniamy zerami
EKATCEEKAT_CEE                           <- left_join(EKATCEEKAT_CEE,MapowanieBaltics,by="Subsidiary Country")

colnames(EKATCEEKAT_CEE)[1]     <- "Klient"
colnames(EKATCEEKAT_CEE)[7]     <- Naglowek1
colnames(EKATCEEKAT_CEE)[8]     <- Naglowek2
colnames(EKATCEEKAT_CEE)[9]     <- Naglowek3
colnames(EKATCEEKAT_CEE)[10]    <- Naglowek4
colnames(EKATCEEKAT_CEE)[11]    <- Naglowek5
colnames(EKATCEEKAT_CEE)[12]    <- Naglowek6

EKATCEEKAT_CEE[EKATCEEKAT_CEE=="RING / HELIOS / REMBRANTIN / CHROMOS / AVRORA"]<-"RING|HEL.|REM.|CHR.|AVR."
EKATCEEKAT_CEE[EKATCEEKAT_CEE=="COCA COLA / MULTON"]<-"COCACOLA|MULTON"
EKATCEEKAT_CEE[EKATCEEKAT_CEE=="AGRANA / AUSTRIA JUICE"]<-"AGRANA|AUSTRIAJUICE"
EKATCEEKAT_CEE[EKATCEEKAT_CEE=="LESAFFRE / SAF NEWA"]<-"LESAFFRE|SAFNEWA"
EKATCEEKAT[EKATCEEKAT=="MBCC Group (former BASF construction)"]<-"MBCC Group"

EKATCEEKAT_CEE        <- left_join(EKATCEEKAT_CEE,MapowanieEKAT,by="Klient")

EKATCEEKAT            <- EKATCEEKAT     %>% select(1,3,4,15,13,14,7,8,9,10,11,12)
EKATCEEKAT_CUS        <- EKATCEEKAT
EKATCEEKAT_CEE        <- EKATCEEKAT_CEE %>% select(1,3,4,15,13,14,7,8,9,10,11,12)

EKATCEEKAT_CEE_CUS    <- EKATCEEKAT_CEE
EKATCEEKAT_CUS        <- EKATCEEKAT_CUS %>% select(2,1,3:12)

EKATCEEKAT            <- EKATCEEKAT [,-2]
EKATCEEKAT_CEE        <- EKATCEEKAT_CEE[,-2]

EKATCEEKAT_CUS        <- EKATCEEKAT_CUS [,-2]
EKATCEEKAT_CEE_CUS    <- EKATCEEKAT_CEE_CUS[,-2]

colnames(EKATCEEKAT_CUS)[1] <- "Klient"
  
EKAT_CUS              <- rbind(EKATCEEKAT_CUS,EKATCEEKAT_CEE_CUS)


EKAT                  <- rbind(EKATCEEKAT,EKATCEEKAT_CEE)

#BBS

BBS_podst                       <- read_xlsx(here::here("sources",zestawDanych,"DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "BBS",range="I16:X6000", col_names = TRUE,na = "NA")
BBS_podst                       <- BBS_podst %>% filter_all(any_vars(!is.na(.))) 
BBS_podst[is.na(BBS_podst)]     <- 0   # wszystkie pola bez wartości wypełniamy zerami
BBS                             <- BBS_podst
BBS                             <- left_join(BBS,MapowanieBaltics,by="Subsidiary Country")
BBS$Clasification               <- as.character("")
colnames(MapowanieIntercompany)[1] <- "SoldToCountry"
colnames(BBS)[9]                <- "SoldToCountry"
BBS                             <- left_join(BBS,MapowanieIntercompany,by="SoldToCountry")
BBS                             <- left_join(BBS,MapowanieBranzaIndu,by="European Industry")


BBS$Clasification               <- ifelse(BBS$Intern=="External",BBS$Clasification<-"3rd party",ifelse(BBS$SoldToCountry %in% unique(MapowanieIntercompany$SoldToCountry),BBS$Clasification<-BBS$Mapowanie,"Intercompany (outside CEE)"))
BBS$EuropeanIndustry            <- ifelse((BBS$Intern=="Internal" & BBS$`European Industry`=="Others"),BBS$EuropeanIndustry<-BBS$Clasification,BBS$EuropeanIndustry<-BBS$MapowanieBranzaInd)

# WATER TREATMENT

WTMain                             <- read_xlsx(here::here("sources",zestawDanych,"DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "Water Treatment",range="I5:X15000", col_names = TRUE,na = "NA")
WTMain                             <- WTMain  %>% filter_all(any_vars(!is.na(.))) 
WTMain [is.na(WTMain)]             <- 0   # wszystkie pola bez wartości wypełniamy zerami
WTMain                             <- left_join(WTMain,MapowanieBaltics,by="Subsidiary Country")

colnames(WTMain)[9]                <- "SoldToCountry"
WTMain$Clasification               <- as.character("")
WTMain                             <- left_join(WTMain,MapowanieIntercompany,by="SoldToCountry")
WTMain$Clasification               <- ifelse(WTMain$Intern=="External",WTMain$Clasification<-"3rd party",ifelse(WTMain$SoldToCountry %in% MapowanieIntercompany$SoldToCountry,WTMain$Clasification<-"Intercompany (inside CEE)",WTMain$Clasification<-"Intercompany (outside CEE)"))
colnames(WTMain)[2]                <- "WaterClass"

PodstawoweZrodloCleaning                                    <- read_xlsx(here::here("sources",zestawDanych,"DEC_NEW_Perform.Rep_2019_YTD_BBS_SR_WT_CL_TOP20.xlsm"),sheet = "Cleaning",range="I16:X60000", col_names = TRUE,na = "NA")
PodstawoweZrodloCleaning                                    <- PodstawoweZrodloCleaning %>% filter_all(any_vars(!is.na(.))) 
PodstawoweZrodloCleaning[is.na(PodstawoweZrodloCleaning)]   <- 0   # wszystkie pola bez wartości wypełniamy zerami
PodstawoweZrodloCleaning                                    <- left_join(PodstawoweZrodloCleaning,MapowanieBaltics,by="Subsidiary Country")
PodstawoweZrodloCleaning                                    <- left_join(PodstawoweZrodloCleaning,MapowanieBESBSP,by="European Industry")
PodstawoweZrodloCleaning                                    <- left_join(PodstawoweZrodloCleaning,MapowanieESSP,by="Material Division")
PodstawoweZrodloCleaning                                    <- left_join(PodstawoweZrodloCleaning,MapowanieBranzaIndu,by="European Industry")
PodstawoweZrodloCleaning$Clasification                      <- ifelse(PodstawoweZrodloCleaning$Intern=="External",PodstawoweZrodloCleaning$Clasification<-"3rd party",ifelse(PodstawoweZrodloCleaning$`SoldTo Country` %in% MapowanieIntercompany$SoldToCountry,PodstawoweZrodloCleaning$Clasification<-"Intercompany (inside CEE)",PodstawoweZrodloCleaning$Clasification<-"Intercompany (outside CEE)"))

PodstawoweZrodloCleaning$EuropeanIndustry2                  <- PodstawoweZrodloCleaning$MapowanieBranzaInd
PodstawoweZrodloCleaning$EuropeanIndustry2                  <- ifelse(PodstawoweZrodloCleaning$`European Industry`=="Cleaning (I&I)",PodstawoweZrodloCleaning$...2,PodstawoweZrodloCleaning$EuropeanIndustry2)

PodstawoweZrodloCleaning$WhsDirComm[PodstawoweZrodloCleaning$WhsDirComm=="#"] <-"Warehouse"
PodstawoweZrodloCleaning$InduInsideTECH                     <- ifelse(PodstawoweZrodloCleaning$`European Industry`=="Others" & PodstawoweZrodloCleaning$Intern=="Internal",PodstawoweZrodloCleaning$Clasification,PodstawoweZrodloCleaning$EuropeanIndustry2)

ModyfikowanyMainSource                                      <- MainSource
ModyfikowanyMainSource                                      <- left_join(ModyfikowanyMainSource ,MapowanieBranzaIndu,by="European Industry")
ModyfikowanyMainSource$EuropeanIndustry2                    <- ModyfikowanyMainSource$MapowanieBranzaInd
ModyfikowanyMainSource$EuropeanIndustry2                    <- ifelse(ModyfikowanyMainSource$`European Industry`=="Cleaning (I&I)",ModyfikowanyMainSource$...3,ModyfikowanyMainSource$EuropeanIndustry2)
ModyfikowanyMainSource$Clasification                        <- ifelse(ModyfikowanyMainSource$Intern=="External",ModyfikowanyMainSource$Clasification<-"3rd party",ifelse(ModyfikowanyMainSource$`SoldTo Country` %in% MapowanieIntercompany$SoldToCountry,ModyfikowanyMainSource$Clasification<-"Intercompany (inside CEE)",ModyfikowanyMainSource$Clasification<-"Intercompany (outside CEE)"))
ModyfikowanyMainSource$InduInsideTECH                       <- ifelse(ModyfikowanyMainSource$`European Industry`=="Others" & ModyfikowanyMainSource$Intern=="Internal",ModyfikowanyMainSource$Clasification,ModyfikowanyMainSource$EuropeanIndustry2)

TabelaGreenKlienci                                          <- read_xlsx(here::here("sources",zestawDanych,"klienci_branże_2019.xlsm"),sheet = "Table",range="I13:X30000", col_names = TRUE,na = "NA")
TabelaGreenKlienci                                          <- TabelaGreenKlienci %>% filter_all(any_vars(!is.na(.)))
TabelaGreenKlienci                                          <- TabelaGreenKlienci[-c(1:3),]
TabelaGreenKlienci                                          <- TabelaGreenKlienci   %>% mutate(across(.cols=5:16, .fns=as.numeric))   # zamiana kilku tekstowych kolumn na kolumny numeryczne,

tekstWazny<-"\\noindent Number of working days JUL23/JUL22: 21d/21d.  \\textbf{Loss on volume} due to close activity in Russia in July: \\textbf{-214 MT} (for GP was noted growth by +47TEUR due to fact of prev.year results with minus margin = extraordinary currency rates due to war crisis business shutdown processes in progress last year) In July 2023 we maintained a positive trend in volumes as a region we’ve closed month with YY growth on volume by +6 713 MT +9\\% YY; unfortunately, in GP we maintained YY loss trend, GP loss noted around whole CEE. In July we’ve noted \\textbf{loss on GP by -6,4\\%} -1,2 mio EUR, loss was noted in both divisions but most of it – generated by BSP: \\textbf{BES -484 TEUR (-5\\%), BSP -1,2 mio EUR (-13\\%)}. Results with not assigned division in July was 457 TEUR on GP and this brings +414 TEUR YY growth for such N.A. transactions. \\textbf{YTD for CEE – closed with loss on GP by -14,7 mio EUR/-10\\%}. YTD results by divisions: BES loss on GP by -1,7 mio EUR (-2\\%), BSP with loss -15 mio EUR (-20\\%).As mentioned, reverse to GP, on volumes we’ve noted YY growth in JULY overall CEE with growth by +6 713 MT/+8,7\\%. Growth noted for both Divisions: BES Division by +5 153 MT (+10\\%), BSP with loss by +464 MT (+2\\%). N.A. division +1 097 MT YY. \\textcolor{Ujemne}{\\textbf{niestandardowym kolorze i jest wyboldowany}}"

