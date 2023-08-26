
setwd(here::here("sources"))

MapowanieIntercompany              <- read_excel("pivot_source_2019.xlsx", range="A1:B16",sheet = "pomocniczy", col_names = FALSE)
MapowanieIntercompany              <- MapowanieIntercompany %>% filter_all(any_vars(!is.na(.)))

colnames(MapowanieIntercompany)[1] <- "SoldToCountry"
colnames(MapowanieIntercompany)[2] <- "Mapowanie"

MapowanieBaltics                   <- read_excel("pivot_source_2019.xlsx", range="E1:G17",sheet = "pomocniczy", col_names = FALSE)
MapowanieBaltics                   <- MapowanieBaltics  %>% filter_all(any_vars(!is.na(.)))

colnames(MapowanieBaltics)[1]      <- "Subsidiary Country"
colnames(MapowanieBaltics)[2]      <- "Region"
colnames(MapowanieBaltics)[3]      <- "SubRegion"


MapowanieBESBSP                    <- read_excel("pivot_source_2019.xlsx", range="B18:C39",sheet = "pomocniczy", col_names = FALSE)
MapowanieBESBSP                    <- MapowanieBESBSP  %>% filter_all(any_vars(!is.na(.)))

colnames(MapowanieBESBSP)[1]       <- "European Industry"
colnames(MapowanieBESBSP)[2]       <- "MapowanieBranza"

MapowanieBranzaIndu                <- read_excel("pivot_source_2019.xlsx", range="I1:J20",sheet = "pomocniczy", col_names = FALSE)
MapowanieBranzaIndu                <- MapowanieBranzaIndu %>% filter_all(any_vars(!is.na(.)))

colnames(MapowanieBranzaIndu)[1]  <- "European Industry"
colnames(MapowanieBranzaIndu)[2]   <- "MapowanieBranzaInd"


MapowanieEndDivide                 <- read_excel("pivot_source_2019.xlsx", range="H22:J50",sheet = "pomocniczy", col_names = FALSE)
MapowanieEndDivide                 <- MapowanieEndDivide %>% filter_all(any_vars(!is.na(.)))

colnames(MapowanieEndDivide)[1]    <- "EndDivide"
colnames(MapowanieEndDivide)[2]    <- "Branza"
colnames(MapowanieEndDivide)[3]    <- "BranzaDiv"


MapowanieEKAT                      <- read_excel("pivot_source_2019.xlsx", range="M1:N60",sheet = "pomocniczy", col_names = FALSE)
MapowanieEKAT                      <- MapowanieEKAT  %>% filter_all(any_vars(!is.na(.)))

colnames(MapowanieEKAT)[1]         <- "Klient"
colnames(MapowanieEKAT)[2]         <- "Mapowanie"


MapowanieFAS                       <- read_excel("pivot_source_2019.xlsx", range="Q1:R10",sheet = "pomocniczy", col_names = FALSE)
MapowanieFAS                       <- MapowanieFAS  %>% filter_all(any_vars(!is.na(.)))

colnames(MapowanieFAS)[1]          <- "Central Product Mgr."
colnames(MapowanieFAS)[2]          <- "Mapowanie"


MapowanieSerbia                    <- read_excel("pivot_source_2019.xlsx", range="R36:S42",sheet = "pomocniczy", col_names = FALSE)
MapowanieSerbia                    <- MapowanieSerbia  %>% filter_all(any_vars(!is.na(.)))

colnames(MapowanieSerbia)[1]       <- "Kraj"
colnames(MapowanieSerbia)[2]       <- "Wartosc"


MapowanieBBS                       <- read_excel("pivot_source_2019.xlsx", range="T1:U33",sheet = "pomocniczy", col_names = FALSE)
MapowanieBBS                       <- MapowanieBBS  %>% filter_all(any_vars(!is.na(.)))

colnames(MapowanieBBS)[1]          <- "Klient"
colnames(MapowanieBBS)[2]          <- "Grupa"


MapowanieESSP                      <- read_excel("pivot_source_2019.xlsx", range="R43:S48",sheet = "pomocniczy", col_names = FALSE)
MapowanieESSP                      <- MapowanieESSP  %>% filter_all(any_vars(!is.na(.)))

colnames(MapowanieESSP)[1]         <- "Material Division"
colnames(MapowanieESSP)[2]         <- "MapowanieMD"


MapowanieTOP20                     <- read_excel("pivot_source_2019.xlsx", range="X23:Y102",sheet = "pomocniczy", col_names = FALSE)
MapowanieTOP20                     <- MapowanieTOP20  %>% filter_all(any_vars(!is.na(.)))

colnames(MapowanieTOP20)[1]        <- "Rodzaj"
colnames(MapowanieTOP20)[2]        <- "Mapowanie"


MapowanieN                         <- read_excel("pivot_source_2019.xlsx", range="AA1:AB30",sheet = "pomocniczy", col_names = FALSE)
MapowanieN                         <- MapowanieN  %>% filter_all(any_vars(!is.na(.)))

colnames(MapowanieN)[1]            <- "Rodzaj"
colnames(MapowanieN)[2]            <- "Mapowanie"

JednaKolumna                       <- read_excel("pivot_source_2019.xlsx", range="AD1:AD30",sheet = "pomocniczy", col_names = FALSE)
JednaKolumna                       <- JednaKolumna  %>% filter_all(any_vars(!is.na(.)))

colnames(JednaKolumna)[1]          <- "Kolumna"

MapowanieICCPMOthers               <- read_excel("pivot_source_2019.xlsx", range="AB33:AC56",sheet = "pomocniczy", col_names = TRUE)
MapowanieICCPMOthers               <- MapowanieICCPMOthers    %>% filter_all(any_vars(!is.na(.)))


setwd(here::here("sources","mapowania"))
save(JednaKolumna,
       MapowanieBaltics,
       MapowanieBBS,
       MapowanieBESBSP,
       MapowanieBranzaIndu,
       MapowanieEKAT,
       MapowanieEndDivide,
       MapowanieESSP,
       MapowanieFAS,
       MapowanieIntercompany,
       MapowanieSerbia,
       MapowanieTOP20,
       MapowanieN,file="mapowania.RData")

setwd(here::here("moduly"))


