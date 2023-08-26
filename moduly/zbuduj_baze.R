
bazaPerformance <- readRDS(here::here("sources","bazaPerformance.Rds"))

AktualnyTotalCEEQuantity                 <- data.frame(DataZaczytania = Sys.time(), 
                                                       NazwaWskaznika = "TOTALCEEQtyCurr", 
                                                       NumerMiesiaca = MiesiacAnalizy, 
                                                       DataAnalizy = dataAnalizy, 
                                                       WartoscWskaznika=MainTotal[MainTotal$SubRegion=="Total CEE",2])
AktualnyTotalCEEGP                       <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "TOTALCEEGPCurr", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, WartoscWskaznika=MainTotal[MainTotal$SubRegion=="Total CEE",6])
AktualnyTotalCEETurnover                 <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "TOTALCEETurnCurr", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, WartoscWskaznika=MainTotal[MainTotal$SubRegion=="Total CEE",10])

AktualnyTotalCEEQuantityPast             <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "TOTALCEEQtyPast", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, WartoscWskaznika=MainTotal[MainTotal$SubRegion=="Total CEE",3])
AktualnyTotalCEEGPPast                   <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "TOTALCEEGPPast", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, WartoscWskaznika=MainTotal[MainTotal$SubRegion=="Total CEE",7])
AktualnyTotalCEETurnoverPast             <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "TOTALCEETurnPast", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, WartoscWskaznika=MainTotal[MainTotal$SubRegion=="Total CEE",11])


AktualnyTotalCEEQuantityYTD              <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "TOTALCEEQtyCurrYTD", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, WartoscWskaznika=MainTotalYTD[MainTotalYTD$SubRegion=="Total CEE",2])
AktualnyTotalCEEGPYTD                    <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "TOTALCEEGPCurrYTD", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, WartoscWskaznika=MainTotalYTD[MainTotalYTD$SubRegion=="Total CEE",6])
AktualnyTotalCEETurnoverYTD              <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "TOTALCEETurnCurrYTD", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, WartoscWskaznika=MainTotalYTD[MainTotalYTD$SubRegion=="Total CEE",10])

AktualnyTotalCEEQuantityPastYTD          <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "TOTALCEEQtyPastYTD", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, WartoscWskaznika=MainTotalYTD[MainTotalYTD$SubRegion=="Total CEE",3])
AktualnyTotalCEEGPPastYTD                <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "TOTALCEEGPPastYTD", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, WartoscWskaznika=MainTotalYTD[MainTotalYTD$SubRegion=="Total CEE",7])
AktualnyTotalCEETurnoverPastYTD          <- data.frame(DataZaczytania = Sys.time(), NazwaWskaznika = "TOTALCEETurnPastYTD", NumerMiesiaca = MiesiacAnalizy, DataAnalizy = dataAnalizy, WartoscWskaznika=MainTotalYTD[MainTotalYTD$SubRegion=="Total CEE",11])






colnames(AktualnyTotalCEEQuantity)[5]                                   <- "Wartosc"
colnames(AktualnyTotalCEEGP)[5]                                         <- "Wartosc"
colnames(AktualnyTotalCEETurnover)[5]                                   <- "Wartosc"
colnames(AktualnyTotalCEEQuantityPast)[5]                               <- "Wartosc"
colnames(AktualnyTotalCEEGPPast)[5]                                     <- "Wartosc"
colnames(AktualnyTotalCEETurnoverPast)[5]                               <- "Wartosc"
colnames(AktualnyTotalCEEQuantityYTD)[5]                                <- "Wartosc"
colnames(AktualnyTotalCEEGPYTD)[5] <- "Wartosc"
colnames(AktualnyTotalCEETurnoverYTD)[5] <- "Wartosc"
colnames(AktualnyTotalCEEQuantityPastYTD)[5] <- "Wartosc"
colnames(AktualnyTotalCEEGPPastYTD)[5] <- "Wartosc"
colnames(AktualnyTotalCEETurnoverPastYTD)[5] <- "Wartosc"





do_bazyPerformance <-
  rbind(
    AktualnyTotalCEEQuantity,
    AktualnyTotalCEEGP,
    AktualnyTotalCEETurnover,
    AktualnyTotalCEEQuantityPast,
    AktualnyTotalCEEGPPast,
    AktualnyTotalCEETurnoverPast,
    AktualnyTotalCEEQuantityYTD,
    AktualnyTotalCEEGPYTD,
    AktualnyTotalCEETurnoverYTD,
    AktualnyTotalCEEQuantityPastYTD,
    AktualnyTotalCEEGPPastYTD,
    AktualnyTotalCEETurnoverPastYTD
    ,
    AktualnyTOTALCEENORTHQuantity,
    AktualnyTOTALCEENORTHGP,
    AktualnyTOTALCEENORTHTurnover,
    AktualnyTOTALCEENORTHQuantityPast,
    AktualnyTOTALCEENORTHGPPast,
    AktualnyTOTALCEENORTHTurnoverPast,
    AktualnyTOTALCEENORTHQuantityYTD,
    AktualnyTOTALCEENORTHGPYTD,
    AktualnyTOTALCEENORTHTurnoverYTD,
    AktualnyTOTALCEENORTHQuantityPastYTD,
    AktualnyTOTALCEENORTHGPPastYTD,
    AktualnyTOTALCEENORTHTurnoverPastYTD,
    AktualnyTOTALCEESOUTHQuantity,
    AktualnyTOTALCEESOUTHGP,
    AktualnyTOTALCEESOUTHTurnover,
    AktualnyTOTALCEESOUTHQuantityPast,
    AktualnyTOTALCEESOUTHGPPast,
    AktualnyTOTALCEESOUTHTurnoverPast,
    AktualnyTOTALCEESOUTHQuantityYTD,
    AktualnyTOTALCEESOUTHGPYTD,
    AktualnyTOTALCEESOUTHTurnoverYTD,
    AktualnyTOTALCEESOUTHQuantityPastYTD,
    AktualnyTOTALCEESOUTHGPPastYTD ,
    AktualnyTOTALCEESOUTHTurnoverPastYTD
  )


bazaPerformance    <- rbind(bazaPerformance,do_bazyPerformance)
bazaPerformance    <- bazaPerformance %>% group_by(NazwaWskaznika,NumerMiesiaca) %>% filter(DataZaczytania==max(DataZaczytania))

saveRDS(bazaPerformance,here::here("sources","bazaPerformance.Rds"))

