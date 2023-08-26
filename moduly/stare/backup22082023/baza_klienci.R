

# 
# TabelaGreenKlienciPast                                          <- read_xlsx(here::here("sources",zestawDanych,"klienci_branże_2019.xlsm"),sheet = "Arkusz1",range="A1:D30000", col_names = TRUE,na = "NA")
# TabelaGreenKlienciPast                                          <- TabelaGreenKlienciPast %>% filter_all(any_vars(!is.na(.)))
# colnames(TabelaGreenKlienciPast)[2]                             <- "Ids"
# colnames(TabelaGreenKlienciPast)[3]                             <- "PreviousAssignment"
# TabelaGreenKlienciPast$WczytanieDanych                          <- Sys.Date()
# TabelaGreenKlienciPast$MiesiacDanych                            <- floor_date(dataAnalizy, unit = "month") - days(1)

baza_kliencii <- readRDS(here::here("sources","bazaKlienciZmiana.Rds"))


TabelaGreenNoweAll                   <- read_xlsx(here::here("sources",zestawDanych,"klienci_branże_2019.xlsm"),sheet = "Table",range="I15:X20000", col_names = TRUE,na = "NA")
TabelaGreenNoweAll                   <- TabelaGreenNoweAll %>% filter_all(any_vars(!is.na(.)))

colnames(TabelaGreenNowe)[1]      <- "SoldTo"
colnames(TabelaGreenNowe)[2]      <- "Ids"
colnames(TabelaGreenNowe)[3]      <- "PreviousAssignment"
colnames(TabelaGreenNowe)[4]      <- "country"
TabelaGreenNowe                   <- TabelaGreenNowe[-1,]

TabelaGreenNowe$WczytanieDanych   <- Sys.Date()
TabelaGreenNowe$MiesiacDanych     <- floor_date(today(), unit = "month") - days(1)
TabelaGreenKlienciPast            <- rbind(TabelaGreenKlienciPast,TabelaGreenNowe)
DataNowychDanych                  <- unique(TabelaGreenNowe$MiesiacDanych)
baza_kliencii                     <- baza_kliencii %>% filter(baza_kliencii$MiesiacDanych!=DataNowychDanych)

NajstarszyOkreszBazaKlienci       <- max(baza_kliencii$MiesiacDanych)
OkresPorownywany                  <- unique(TabelaGreenNowe$MiesiacDanych)

StaraDoPorownania                 <- baza_kliencii %>% filter(baza_kliencii$MiesiacDanych==NajstarszyOkreszBazaKlienci)

print(paste("Porównywane okresy", OkresPorownywany,"do",NajstarszyOkreszBazaKlienci))

NowaTabela <- left_join(TabelaGreenNowe,StaraDoPorownania,by=c("Ids","country")) %>% mutate(NewAss=ifelse(PreviousAssignment.x!=PreviousAssignment.y & !is.na(PreviousAssignment.y),PreviousAssignment.x,"")) %>% filter(NewAss!="") %>% distinct()
Filtr<-unique(NowaTabela$Ids)



# Należy zapamiętać nową tabelę bazową będącą składnikiem table:baza_klienci oraz TabelaKlienciNowa.
# Do zapisania "baza_kliencii" pod nazwą bazy danych.

# Do porównania wychwycić tylko zmiany przypisania.

# 
# TabelaGreenHomeMonth          <- read_xlsx(here::here("sources",zestawDanych,"klienci_branże_2019.xlsm"),sheet = "Table",range="D43:D43", col_names = FALSE,na = "NA")
# TabelaGreenHomeMonth          <- "..2"
# 
# TabelaGreenHomeMonth          <- ifelse(str_sub(TabelaGreenHomeMonth,nchar(TabelaGreenHomeMonth)-1,nchar(TabelaGreenHomeMonth)-1)==".",str_sub(TabelaGreenHomeMonth,nchar(TabelaGreenHomeMonth),
#                                  nchar(TabelaGreenHomeMonth)),str_sub(TabelaGreenHomeMonth,nchar(TabelaGreenHomeMonth)-1,nchar(TabelaGreenHomeMonth)))






TabelaGreenNoweAll               <- read_xlsx(here::here("sources",zestawDanych,"klienci_branże_2019.xlsm"),sheet = "Table",range="I15:X20000", col_names = TRUE,na = "NA")
TabelaGreenNoweAll               <- TabelaGreenNoweAll %>% filter_all(any_vars(!is.na(.)))
colnames(TabelaGreenNoweAll)[1]      <- "SoldTo"
colnames(TabelaGreenNoweAll)[2]      <- "Ids"
colnames(TabelaGreenNoweAll)[3]      <- "PreviousAssignment"
colnames(TabelaGreenNoweAll)[4]      <- "country"


TabelaGreenNoweAll               <- TabelaGreenNoweAll[-1,]
TabelaGreenNoweAll               <- TabelaGreenNoweAll[,-c(7,8,11,12)]
TabelaGreenNoweAll               <- TabelaGreenNoweAll %>% mutate_at(c(5:10), as.numeric)  # zamiana wileu kolumn w numeryczne
TabelaGreenNoweAll               <- TabelaGreenNoweAll %>% mutate(across(where(is.numeric),replace_na, 0))
TabelaGreenNoweAll$MarginDiff    <- TabelaGreenNoweAll$`Margin EUR (adj)...5`-TabelaGreenNoweAll$`Margin EUR (adj)...6`
TabelaGreenNoweAll$MarginDiffPER <- round((TabelaGreenNoweAll$`Margin EUR (adj)...5`-TabelaGreenNoweAll$`Margin EUR (adj)...6`)/TabelaGreenNoweAll$`Margin EUR (adj)...6`*100,1)
TabelaGreenNoweAll$QuantDiff     <- TabelaGreenNoweAll$`Quantity MT...9`-TabelaGreenNoweAll$`Quantity MT...10`
TabelaGreenNoweAll$QuantDiffPER  <- round((TabelaGreenNoweAll$`Quantity MT...9`-TabelaGreenNoweAll$`Quantity MT...10`)/TabelaGreenNoweAll$`Quantity MT...10`*100,1)
TabelaGreenNoweAll$TurnDiff      <- TabelaGreenNoweAll$`Turnover EUR (adj)...13`-TabelaGreenNoweAll$`Turnover EUR (adj)...14`
TabelaGreenNoweAll$TurnDiffPER   <- round((TabelaGreenNoweAll$`Turnover EUR (adj)...13`-TabelaGreenNoweAll$`Turnover EUR (adj)...14`)/TabelaGreenNoweAll$`Turnover EUR (adj)...14`*100,1)
TabelaGreenNoweAll$GPMTACT       <- ifelse(is.infinite(TabelaGreenNoweAll$`Margin EUR (adj)...5`*1000/TabelaGreenNoweAll$`Quantity MT...9`),0,TabelaGreenNoweAll$`Margin EUR (adj)...5`*1000/TabelaGreenNoweAll$`Quantity MT...9`)   # x 1000 ?
TabelaGreenNoweAll$GPMTPREV      <- ifelse(is.infinite(TabelaGreenNoweAll$`Margin EUR (adj)...6`*1000/TabelaGreenNoweAll$`Quantity MT...10`),0,TabelaGreenNoweAll$`Margin EUR (adj)...6`*1000/TabelaGreenNoweAll$`Quantity MT...10`)
TabelaGreenNoweAll$GPMTDIFF      <- TabelaGreenNoweAll$GPMTACT-TabelaGreenNoweAll$GPMTPREV
TabelaGreenNoweAll$GPMTDIFFPER   <- ifelse(is.infinite((TabelaGreenNoweAll$GPMTACT-TabelaGreenNoweAll$GPMTPREV)/TabelaGreenNoweAll$GPMTPREV),"",scales::percent(((TabelaGreenNoweAll$GPMTACT-TabelaGreenNoweAll$GPMTPREV)/TabelaGreenNoweAll$GPMTPREV),accuracy = 0.1))
TabelaGreenNoweAll               <- TabelaGreenNoweAll %>% mutate(across(where(is.numeric),replace_na, 0)) %>% mutate(across(where(is.character),replace_na, "0"))
TabelaGreenNoweAll               <- TabelaGreenNoweAll %>% mutate_if(is.numeric, ~round(., 0))

TabelaGreenNoweAll <-    TabelaGreenNoweAll[TabelaGreenNoweAll$Ids %in% Filtr,]


saveRDS(TabelaGreenKlienciPast,here::here("sources","bazaKlienciZmiana.Rds"))


