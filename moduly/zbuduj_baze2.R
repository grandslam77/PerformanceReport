
bazaPerformance      <- readRDS(here::here("sources","bazaPerformance.Rds"))
# bazaPerformance[,]   <- NA
# bazaPerformance                     <- bazaPerformance %>% filter_all(any_vars(!is.na(.)))
# bazaPerformance<-bazaPerformance[,1:5]


# bazaPerformance$DataZaczytania <- as.Date(bazaPerformance$DataZaczytania)
# bazaPerformance$NazwaWskaznika <- as.character(bazaPerformance$NazwaWskaznika)
# bazaPerformance$NumerMiesiaca  <- as.integer(bazaPerformance$NumerMiesiaca)
# bazaPerformance$DataAnalizy    <- as.Date(bazaPerformance$DataAnalizy)
# bazaPerformance$Wartosc        <- as.double(bazaPerformance$Wartosc)


QuantDF          <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "Quant", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, Wartosc=Quant)
QuantPRVDF       <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "QuantPRV", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, Wartosc=Quant_PRV)
GrossProfDF      <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "GrossProf", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, Wartosc=GrossProf)
GrossProf_PRVDF  <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "GrossProf_PRV", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, Wartosc=GrossProf_PRV)
TurnovDF         <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "Turnov", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, Wartosc=Turnov)
Turnov_PRVDF     <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "Turnov_PRV", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, Wartosc=Turnov_PRV)
GPMTDF           <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "GPMT", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, Wartosc=GPMT)
GPMT_PRVDF       <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "GPMT_PRV", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, Wartosc=GPMT_PRV)


QuantDF_YTD          <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "Quant_YTD", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, Wartosc=Quant_YTD)
QuantPRVDF_YTD       <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "Quant_YTD_PRV", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, Wartosc=Quant_PRV_YTD)
GrossProfDF_YTD      <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "GrossProf_YTD", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, Wartosc=GrossProf_YTD)
GrossProf_PRVDF_YTD  <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "GrossProf_PRV_YTD", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, Wartosc=GrossProf_PRV_YTD)
TurnovDF_YTD         <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "Turnov_YTD", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, Wartosc=Turnov_YTD)
Turnov_PRVDF_YTD     <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "Turnov_PRV_YTD", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, Wartosc=Turnov_PRV_YTD)
GPMTDF_YTD           <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "GPMT_YTD", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, Wartosc=GPMT_YTD)
GPMT_PRVDF_YTD       <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "GPMT_PRV_YTD", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, Wartosc=GPMT_PRV_YTD)


colnames(bazaPerformance)[5]<-"Wartosc" 
colnames(QuantDF)[5]<-"Wartosc"
colnames(QuantPRVDF)[5]<-"Wartosc"
colnames(GrossProfDF)[5]<-"Wartosc"
colnames(GrossProf_PRVDF)[5]<-"Wartosc"
colnames(TurnovDF)[5]<-"Wartosc" 
colnames(Turnov_PRVDF)[5]<-"Wartosc" 
colnames(GPMTDF)[5]<-"Wartosc" 
colnames(GPMT_PRVDF)[5]<-"Wartosc" 
colnames(QuantDF_YTD)[5]<-"Wartosc"
colnames(QuantPRVDF_YTD)[5]<-"Wartosc" 
colnames(GrossProfDF_YTD)[5]<-"Wartosc" 
colnames(GrossProf_PRVDF_YTD)[5]<-"Wartosc" 
colnames(TurnovDF_YTD)[5]<-"Wartosc" 
colnames(Turnov_PRVDF_YTD)[5]<-"Wartosc" 
colnames(GPMTDF_YTD)[5]<-"Wartosc" 
colnames(GPMT_PRVDF_YTD)[5]<-"Wartosc"


do_bazyPerformance <- rbind(
                        QuantDF, QuantPRVDF, GrossProfDF, GrossProf_PRVDF, TurnovDF, Turnov_PRVDF, GPMTDF, GPMT_PRVDF, QuantDF_YTD,
                        QuantPRVDF_YTD, GrossProfDF_YTD, GrossProf_PRVDF_YTD, TurnovDF_YTD, Turnov_PRVDF_YTD, GPMTDF_YTD, GPMT_PRVDF_YTD)

do_bazyPerformance$Wartosc[do_bazyPerformance$Wartosc==""]<-0
do_bazyPerformance$Wartosc<-as.numeric(do_bazyPerformance$Wartosc)

bazaPerformance    <- rbind(bazaPerformance,do_bazyPerformance)
bazaPerformance    <- bazaPerformance %>% group_by(NazwaWskaznika,NumerMiesiaca) %>% filter(DataZaczytania==max(DataZaczytania))

saveRDS(bazaPerformance,here::here("sources","bazaPerformance.Rds"))

