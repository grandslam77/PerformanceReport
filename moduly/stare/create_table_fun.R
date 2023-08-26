# Napisz funkcję w języku R przetwarzającą tabelę zgodnie z przedstawionym dalej algorytmem. Funkacja ma sumować dane z wybranych kolumn tabeli według zadancych poziomów grupowania. Argumentami funkcji są: tabela wejściowa, numery kolumn wybranych do grupowania, numery kolumn z wartościami do sumowania.

# 1.  Podstaw tabelę zawierającą dane i podaj numery kolumn które mają zostać wyselekcjonowane.
# 2.  Ograniczenie danych do miesiąca lub YTD w tabeli
# 3.  Podajemy numery kolumn do wyselekcjonowania, 
# 4.  Po wyselekcjonowaniu kolumn z tabeli dokonaj grupowania w według wszystkich kolumn gupujących w kolejności w jakie zostały wyselekcjonowane wcześniej.
# 5.  Oblicz sumę dla kazdej z ostatnich 4 kolumn jako wartość total.
# 6.  Oblicz Sumy pośrednie dla pierwszego poziomu grupowania
# 7.  Oblicz sumy pośrednie dla drugiego poziomu grupowania,
# 8.  Oblicz sumy pośrednie dla trzeciego poziomu grupowania jeśli występuje.
# 9   Oblicz sumy pośrednie dla czwartrgo poziomu grupowania jeśli występuje.
# 10. Dołącz wartość total i sumy pośrednie do tabelki i posortuj według poziomów grupowania, tak aby wartości pośrednie były pod odpowiadającymi im poziomami grupowania a wartość total na samym dole tabelki.


CreateTable <-function(tabela_wejsciowa,nazwa_tabeli=""){
  options(dplyr.summarise.inform = FALSE)
  
  #browser()
  
  ilosc_poziomow_grupowania                                  <- ncol(tabela_wejsciowa)-6
  
  colnames(tabela_wejsciowa )[ncol(tabela_wejsciowa)-5]      <- "Quantity KG TOTAL 2021"
  colnames(tabela_wejsciowa )[ncol(tabela_wejsciowa)-4]      <- "Quantity KG TOTAL 2022"
  colnames(tabela_wejsciowa )[ncol(tabela_wejsciowa)-3]      <- "Margin EUR (adj) TOTAL 2021"
  colnames(tabela_wejsciowa )[ncol(tabela_wejsciowa)-2]      <- "Margin EUR (adj) TOTAL 2022"
  colnames(tabela_wejsciowa )[ncol(tabela_wejsciowa)-1]      <- "Turnover EUR (adj) TOTAL 2021"
  colnames(tabela_wejsciowa )[ncol(tabela_wejsciowa)]        <- "Turnover EUR (adj) TOTAL 2022"
 
  nazwy_kolumn_grupujacych                                   <- colnames(tabela_wejsciowa)[1:(ncol(tabela_wejsciowa)-6)]

 
  tabela_wejsciowa                               <- tabela_wejsciowa %>% group_by_at(vars(nazwy_kolumn_grupujacych)) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))
  #tabela_wejsciowa                               <- tabela_wejsciowa  %>% mutate(across(where(is.numeric), round))
  
  tabela_wejsciowa <- tabela_wejsciowa[rowSums(tabela_wejsciowa[, c("QActYear", "QprevYear","GPACT", "GPPREV","TURNACT", "TURNPREV")]) != 0, ]
 
   #tabela_wejsciowa[,(ncol(tabela_wejsciowa)-5):ncol(tabela_wejsciowa)] <- floor(tabela_wejsciowa[,ncol(tabela_wejsciowa)-5:ncol(tabela_wejsciowa)]) 
  tabela_wejsciowa[is.na(tabela_wejsciowa)]      <- NA
  tabela_wejsciowa$QuantityDiffYY                <- tabela_wejsciowa$QActYear-tabela_wejsciowa$QprevYear
  tabela_wejsciowa[is.na(tabela_wejsciowa)]      <- NA
  tabela_wejsciowa$QuantityDiffYYPER_P           <- as.numeric(ifelse(is.infinite(((tabela_wejsciowa$QActYear-tabela_wejsciowa$QprevYear)/tabela_wejsciowa$QprevYear)),"",(tabela_wejsciowa$QActYear-tabela_wejsciowa$QprevYear)/tabela_wejsciowa$QprevYear))
  tabela_wejsciowa$QuantityDiffYYPER             <- ifelse(is.infinite(((tabela_wejsciowa$QActYear-tabela_wejsciowa$QprevYear)/tabela_wejsciowa$QprevYear)),"",scales::percent(((tabela_wejsciowa$QActYear-tabela_wejsciowa$QprevYear)/tabela_wejsciowa$QprevYear),accuracy = 0.1))
  tabela_wejsciowa[is.na(tabela_wejsciowa)]      <- NA
  tabela_wejsciowa$GPDiffYY                      <- tabela_wejsciowa$GPACT-tabela_wejsciowa$GPPREV
  tabela_wejsciowa[is.na(tabela_wejsciowa)]      <- NA
  tabela_wejsciowa$GPDiffYYPER                   <- ifelse(is.infinite(((tabela_wejsciowa$GPACT-tabela_wejsciowa$GPPREV)/tabela_wejsciowa$GPPREV)),"",scales::percent(((tabela_wejsciowa$GPACT-tabela_wejsciowa$GPPREV)/tabela_wejsciowa$GPPREV),accuracy = 0.1))
  #tabela_wejsciowa$GPDiffYYPER       <- round(((tabela_wejsciowa$GPACT-tabela_wejsciowa$GPPREV)/tabela_wejsciowa$GPPREV)*100,2)
  tabela_wejsciowa[is.na(tabela_wejsciowa)]      <- NA
  tabela_wejsciowa$TurnDiffYY                    <- tabela_wejsciowa$TURNACT-tabela_wejsciowa$TURNPREV
  tabela_wejsciowa[is.na(tabela_wejsciowa)]      <- NA
  tabela_wejsciowa$TurnDiffYYPER                 <- ifelse(is.infinite(((tabela_wejsciowa$TURNACT-tabela_wejsciowa$TURNPREV)/tabela_wejsciowa$TURNPREV)),"",scales::percent(((tabela_wejsciowa$TURNACT-tabela_wejsciowa$TURNPREV)/tabela_wejsciowa$TURNPREV),accuracy = 0.1))
  
  tabela_wejsciowa[is.na(tabela_wejsciowa)]      <- NA
  tabela_wejsciowa$GPMTACT                       <- ifelse(is.infinite(tabela_wejsciowa$GPACT*1000/tabela_wejsciowa$QActYear),0,tabela_wejsciowa$GPACT*1000/tabela_wejsciowa$QActYear)   # x 1000 ?
  
  tabela_wejsciowa$GPMTPREV                      <- ifelse(is.infinite(tabela_wejsciowa$GPPREV*1000/tabela_wejsciowa$QprevYear),0,tabela_wejsciowa$GPPREV*1000/tabela_wejsciowa$QprevYear)

  tabela_wejsciowa                               <- tabela_wejsciowa %>% mutate(across(where(is.numeric),replace_na, 0))
  tabela_wejsciowa                               <- tabela_wejsciowa %>% mutate(across(where(is.character),replace_na, "0"))
  
  tabela_wejsciowa$GPMTDIFF                      <- tabela_wejsciowa$GPMTACT-tabela_wejsciowa$GPMTPREV
  
  tabela_wejsciowa$GPMTDIFFPER                   <- ifelse(is.infinite((tabela_wejsciowa$GPMTACT-tabela_wejsciowa$GPMTPREV)/tabela_wejsciowa$GPMTPREV),"",scales::percent(((tabela_wejsciowa$GPMTACT-tabela_wejsciowa$GPMTPREV)/tabela_wejsciowa$GPMTPREV),accuracy = 0.1))

  
  DT                                             <- data.table(tabela_wejsciowa)
  tabela_wejsciowa                               <- do.call(data.table,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
  tabela_wejsciowa[is.na(tabela_wejsciowa)]      <- NA
 

  tabela_wejsciowa                               <- tabela_wejsciowa %>% mutate_if(is.character, ~stri_replace_all_fixed(., pattern = c("&","/"), replacement = c("","|"), vectorize_all = FALSE)) 
  Kolejnosc_Sortowania                           <- c(colnames(tabela_wejsciowa[,1:ilosc_poziomow_grupowania]))

  tabela_wejsciowa                               <- setorderv(tabela_wejsciowa,Kolejnosc_Sortowania)
 
  
  tabela_wejsciowa$QuantityDiffYYPER_P           <- NULL

  
  
  # tabelę z total musimy stworzyć najpierw bez kolumn grupujących ponieważ nigdy nie wiadomo ile ich będzie.
  
  Total <- data.frame(
                      QActYear          = sum(tabela_wejsciowa$QActYear),
                      QprevYear         = sum(tabela_wejsciowa$QprevYear),
                      GPACT             = sum(tabela_wejsciowa$GPACT),
                      GPPREV            = sum(tabela_wejsciowa$GPPREV),
                      TURNACT           = sum(tabela_wejsciowa$TURNACT),
                      TURNPREV          = sum(tabela_wejsciowa$TURNPREV),
                      QuantityDiffYY    = 0,
                      QuantityDiffYYPER = 0,
                      GPDiffYYPER       = 0,
                      GPDiffYY          = 0,
                      TurnDiffYY        = sum(tabela_wejsciowa$TurnDiffYY),
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
  Total$QuantityDiffYYPER  <-  ifelse(is.infinite(((tabela_wejsciowa$QActYear-tabela_wejsciowa$QprevYear)/tabela_wejsciowa$QprevYear)),"",scales::percent(Total$QuantityDiffYY/Total$QprevYear,accuracy = 0.1))
  Total$GPDiffYYPER        <-  ifelse(is.infinite(((tabela_wejsciowa$GPACT-tabela_wejsciowa$GPPREV)/tabela_wejsciowa$GPPREV)),"",scales::percent(Total$GPDiffYY/Total$GPPREV,accuracy = 0.1))
  Total$TurnDiffYYPER      <-  ifelse(is.infinite(((tabela_wejsciowa$TURNACT-tabela_wejsciowa$TURNPREV)/tabela_wejsciowa$TURNPREV)),"",scales::percent(Total$TurnDiffYY/Total$TURNPREV,accuracy = 0.1))
  Total$GPMTDIFFPER        <-  ifelse(is.infinite((tabela_wejsciowa$GPMTACT-tabela_wejsciowa$GPMTPREV)/tabela_wejsciowa$GPMTPREV),"",scales::percent(Total$GPMTDIFF/Total$GPMTPREV,accuracy = 0.1))
  
  
  
  # --------------------------- 1 poziom grupowania -------------------------------------------------------
  
   if (ilosc_poziomow_grupowania==1){   
 
      #   if(startsWith(Nazwa,"T_TOP")){  # w zależnosci od tego jaka tabela wchodzi mozna dodać dodatkowe czynności
  #     tabela_wejsciowa                      <- setorderv(tabela_wejsciowa,colnames(tabela_wejsciowa)[3])
  #   }
  #   
  #   
    
    
    tabela_wejsciowa                      <- setorderv(tabela_wejsciowa,colnames(tabela_wejsciowa)[1])
    Total[colnames(tabela_wejsciowa)[1]]  <-"TOTAL CEE"
    Total$A <-""
    Total    <-Total %>% select(17,1:16)
    colnames(Total)[1]<-colnames(tabela_wejsciowa)[1]
    tabela_wejsciowa                      <- rbind(tabela_wejsciowa,Total)
    
    Boldowanie                            <- dim(tabela_wejsciowa)[1]
    #tabela_wejsciowa         <- rbind(tabela_wejsciowa,thereofCEE) na razie tą część tabelki trzeba odpóścić, do znalezienia rozwiązania
    
    tabela_wejsciowa                                  <- tabela_wejsciowa %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
    tabela_wejsciowa[is.na(tabela_wejsciowa)]         <- ""
    tabela_wejsciowa                                  <- tabela_wejsciowa%>% mutate_if(is.numeric, ~round(., 0)) 
  }
  
  # --------------------- 2 poziomy grupowania ------------------------------------------------------------------------------------------
  
  
  if (ilosc_poziomow_grupowania==2){
    
    PoziomGrupowania1 <- unlist(as.vector(as.data.frame(unique(tabela_wejsciowa[,1]))))
   
    for(poziomgrupowania1 in PoziomGrupowania1){
      steal <- tabela_wejsciowa %>% filter(tabela_wejsciowa[,1]==poziomgrupowania1)
      frejm <- data.frame(PoziomGrupowania1=poziomgrupowania1, Cokolwiek=poziomgrupowania1,
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
      
      colnames(frejm)[1]       <- colnames(tabela_wejsciowa)[1]
      colnames(frejm)[2]       <- colnames(tabela_wejsciowa)[2]
      tabela_wejsciowa         <- rbind(tabela_wejsciowa, frejm)
    
      }
  
    #browser()
      tabela_wejsciowa                      <- setorderv(tabela_wejsciowa,colnames(tabela_wejsciowa)[1])
      Total[colnames(tabela_wejsciowa)[1]]  <-"TOTAL CEE"
      Total[colnames(tabela_wejsciowa)[2]]  <-"TOTAL CEE"
      Total$A <-""
      Total$B <-""
      Total    <-Total %>% select(17:18,1:16)
      colnames(Total)[1]<-colnames(tabela_wejsciowa)[1]
      colnames(Total)[2]<-colnames(tabela_wejsciowa)[2]
      tabela_wejsciowa                      <- rbind(tabela_wejsciowa,Total)
    
      Boldowanie                            <- c(which(unlist(tabela_wejsciowa[,2]) %in% PoziomGrupowania1))
    #tabela_wejsciowa         <- rbind(tabela_wejsciowa,thereofCEE) na razie tą część tabelki trzeba odpóścić, do znalezienia rozwiązania
    
      tabela_wejsciowa[,1]           <- NULL
      tabela_wejsciowa               <- tabela_wejsciowa %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
      tabela_wejsciowa               <- tabela_wejsciowa %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))
      tabela_wejsciowa               <- tabela_wejsciowa %>% mutate_if(is.character, ~replace(., is.na(.), 0))
      tabela_wejsciowa               <- tabela_wejsciowa%>% mutate_if(is.numeric, ~round(., 0)) 
  }
  

  # -------------------------------------- 3 poziomy grupowania --------------------------------------------

  
  if (ilosc_poziomow_grupowania==3){

    PoziomGrupowania1 <- unlist(as.vector(as.data.frame(unique(tabela_wejsciowa[,1]))))
    PoziomGrupowania2 <- unlist(as.vector(as.data.frame(unique(tabela_wejsciowa[,2]))))
    
    
    
    for(poziomgrupowania1 in PoziomGrupowania1){
      steal <- tabela_wejsciowa %>% filter(tabela_wejsciowa[,1]==poziomgrupowania1)
        
          frejm <- data.frame(PoziomGrupowania1=poziomgrupowania1, PoziomGrupowania2="ŻŻŻŻŻŻ", Cokolwiek=poziomgrupowania1,
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
      
      colnames(frejm)[1]       <- colnames(tabela_wejsciowa)[1]
      colnames(frejm)[2]       <- colnames(tabela_wejsciowa)[2]
      colnames(frejm)[3]       <- colnames(tabela_wejsciowa)[3]
      tabela_wejsciowa         <- rbind(tabela_wejsciowa, frejm)
        
    }
    
   
    
    for(poziomgrupowania1 in PoziomGrupowania1){
      for(poziomgrupowania2 in PoziomGrupowania2){
        steal <- tabela_wejsciowa %>% filter(tabela_wejsciowa[,1]==poziomgrupowania1 & tabela_wejsciowa[,2]==poziomgrupowania2) 
        if(nrow(steal)>0){
          frejm <- data.frame(PoziomGrupowania1=poziomgrupowania1,PoziomGrupowania2=poziomgrupowania2, Cokolwiek=poziomgrupowania2,
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
    
        
      colnames(frejm)[1]       <- colnames(tabela_wejsciowa)[1]
      colnames(frejm)[2]       <- colnames(tabela_wejsciowa)[2]
      colnames(frejm)[3]       <- colnames(tabela_wejsciowa)[3]
      tabela_wejsciowa         <- rbind(tabela_wejsciowa, frejm)
        }
      }
    }
    
    # sortowanie po kolumnach, które są prawidłowo przypisane do danego poziomu grupowania

    tabela_wejsciowa                      <- setorderv(tabela_wejsciowa,c(colnames(tabela_wejsciowa)[1],colnames(tabela_wejsciowa)[2]))
    
    Total[colnames(tabela_wejsciowa)[1]]  <- "TOTAL CEE"
    Total[colnames(tabela_wejsciowa)[2]]  <- "TOTAL CEE"
    Total[colnames(tabela_wejsciowa)[3]]  <- "TOTAL CEE"
    Total$A                               <- " "
    Total$B                               <- " "
    Total$C                               <- " "
 
    Total                                 <- Total %>% select(17:19,1:16)
    colnames(Total)[1]                    <- colnames(tabela_wejsciowa)[1]
    colnames(Total)[2]                    <- colnames(tabela_wejsciowa)[2]
    colnames(Total)[3]                    <- colnames(tabela_wejsciowa)[3]
    tabela_wejsciowa                      <- rbind(tabela_wejsciowa,Total)
    Boldowanie                            <- c(which(unlist(tabela_wejsciowa[,3]) %in% PoziomGrupowania2),which(unlist(tabela_wejsciowa[,3]) %in% PoziomGrupowania1))
    #tabela_wejsciowa         <- rbind(tabela_wejsciowa,thereofCEE) na razie tą część tabelki trzeba odpóścić, do znalezienia rozwiązania
    tabela_wejsciowa[,1:2]           <- NULL

    
    tabela_wejsciowa                                  <- tabela_wejsciowa %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
    tabela_wejsciowa  <- tabela_wejsciowa %>% mutate(across(where(is.numeric),replace_na, 0))
    tabela_wejsciowa  <- tabela_wejsciowa %>% mutate(across(where(is.character),replace_na, "0"))
    tabela_wejsciowa                                  <- tabela_wejsciowa%>% mutate_if(is.numeric, ~round(., 0)) 
  }
  
#### 4 poziomy grupowania ------------------------------------------------------------------------------------------

  if (ilosc_poziomow_grupowania==4){
  
  PoziomGrupowania1 <- unlist(as.vector(as.data.frame(unique(tabela_wejsciowa[,1]))))
  PoziomGrupowania2 <- unlist(as.vector(as.data.frame(unique(tabela_wejsciowa[,2]))))
  PoziomGrupowania3 <- unlist(as.vector(as.data.frame(unique(tabela_wejsciowa[,3]))))
  #browser()
  
  for(poziomgrupowania1 in PoziomGrupowania1){
    steal <- tabela_wejsciowa %>% filter(tabela_wejsciowa[,1]==poziomgrupowania1)
    frejm <- data.frame(PoziomGrupowania1=poziomgrupowania1, PoziomGrupowania2="ŻŻŻŻŻ", PoziomGrupowania3="ŻŻŻŻŻŻŻŻŻ", Cokolwiek=poziomgrupowania1,
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
    
    colnames(frejm)[1]       <- colnames(tabela_wejsciowa)[1]
    colnames(frejm)[2]       <- colnames(tabela_wejsciowa)[2]
    colnames(frejm)[3]       <- colnames(tabela_wejsciowa)[3]
    colnames(frejm)[4]       <- colnames(tabela_wejsciowa)[4]
    tabela_wejsciowa         <- rbind(tabela_wejsciowa, frejm)
    
  }
  
  
  for(poziomgrupowania1 in PoziomGrupowania1){
    for(poziomgrupowania2 in PoziomGrupowania2){
      steal <- tabela_wejsciowa %>% filter(tabela_wejsciowa[,1]==poziomgrupowania1 & tabela_wejsciowa[,2]==poziomgrupowania2) 
      if(nrow(steal)>0){
      frejm <- data.frame(PoziomGrupowania1=poziomgrupowania1,PoziomGrupowania2=poziomgrupowania2, PoziomGrupowania3="ŻŻŻŻŻŻŻŻŻ",Cokolwiek=poziomgrupowania2,
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
      frejm$GPMTACT     <-  frejm$GPACT*1000/frejm$QActYear
      frejm$GPMTPREV    <-  frejm$GPPREV*1000/frejm$QprevYear
      frejm$TurnDiffYY         <-  frejm$TURNACT-frejm$TURNPREV
      frejm$QuantityDiffYY     <-  frejm$QActYear-frejm$QprevYear
      frejm$GPDiffYY           <-  frejm$GPACT-frejm$GPPREV
      frejm$GPMTDIFF           <-  frejm$GPMTACT-frejm$GPMTPREV
      frejm$QuantityDiffYYPER  <-  scales::percent(frejm$QuantityDiffYY/frejm$QprevYear,accuracy = 0.1)
      frejm$GPDiffYYPER        <-  scales::percent(frejm$GPDiffYY/frejm$GPPREV,accuracy = 0.1)
      frejm$TurnDiffYYPER      <-  scales::percent(frejm$TurnDiffYY/frejm$TURNPREV,accuracy = 0.1)
      frejm$GPMTDIFFPER        <-  scales::percent(frejm$GPMTDIFF/frejm$GPMTPREV,accuracy = 0.1)
      
      
      colnames(frejm)[1]       <- colnames(tabela_wejsciowa)[1]
      colnames(frejm)[2]       <- colnames(tabela_wejsciowa)[2]
      colnames(frejm)[3]       <- colnames(tabela_wejsciowa)[3]
      colnames(frejm)[4]       <- colnames(tabela_wejsciowa)[4]
      tabela_wejsciowa         <- rbind(tabela_wejsciowa, frejm)
      }
    }
  }
  
  
  for(poziomgrupowania1 in PoziomGrupowania1){
    for(poziomgrupowania2 in PoziomGrupowania2){
      for(poziomgrupowania3 in PoziomGrupowania3){

      steal <- tabela_wejsciowa %>% filter(tabela_wejsciowa[,1]==poziomgrupowania1 & tabela_wejsciowa[,2]==poziomgrupowania2& tabela_wejsciowa[,3]==poziomgrupowania3) 
      if(nrow(steal)>0){
        frejm <- data.frame(PoziomGrupowania1=poziomgrupowania1,PoziomGrupowania2=poziomgrupowania2, PoziomGrupowania3=poziomgrupowania3,Cokolwiek=poziomgrupowania3,
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
      
      
      colnames(frejm)[1]       <- colnames(tabela_wejsciowa)[1]
      colnames(frejm)[2]       <- colnames(tabela_wejsciowa)[2]
      colnames(frejm)[3]       <- colnames(tabela_wejsciowa)[3]
      colnames(frejm)[4]       <- colnames(tabela_wejsciowa)[4]
      tabela_wejsciowa         <- rbind(tabela_wejsciowa, frejm)
      }
    }
  }
  } 


  # sortowanie po kolumnach, które są prawidłowo przypisane do danego poziomu grupowania

  tabela_wejsciowa                      <- setorderv(tabela_wejsciowa,c(colnames(tabela_wejsciowa)[1],colnames(tabela_wejsciowa)[2],colnames(tabela_wejsciowa)[3]))

  
  Total[colnames(tabela_wejsciowa)[1]]  <- "TOTAL CEE"
  Total[colnames(tabela_wejsciowa)[2]]  <- "TOTAL CEE"
  Total[colnames(tabela_wejsciowa)[3]]  <- "TOTAL CEE"
  Total[colnames(tabela_wejsciowa)[4]]  <- "TOTAL CEE"
  Total$A                               <- " "
  Total$B                               <- " "
  Total$C                               <- " "
  Total$C                               <- " "
  
  Total                                 <- Total %>% select(17:20,1:16)
  colnames(Total)[1]                    <- colnames(tabela_wejsciowa)[1]
  colnames(Total)[2]                    <- colnames(tabela_wejsciowa)[2]
  colnames(Total)[3]                    <- colnames(tabela_wejsciowa)[3]
  colnames(Total)[4]                    <- colnames(tabela_wejsciowa)[4]
  tabela_wejsciowa                      <- rbind(tabela_wejsciowa,Total)
  
  Boldowanie                                    <- c(which(unlist(tabela_wejsciowa[,4]) %in% PoziomGrupowania1),which(unlist(tabela_wejsciowa[,4]) %in% PoziomGrupowania2),which(unlist(tabela_wejsciowa[,4]) %in% PoziomGrupowania3))
  #tabela_wejsciowa         <- rbind(tabela_wejsciowa,thereofCEE) na razie tą część tabelki trzeba odpóścić, do znalezienia rozwiązania
  tabela_wejsciowa[,1:3]                        <- NULL
  tabela_wejsciowa                              <- tabela_wejsciowa %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
  tabela_wejsciowa                              <- tabela_wejsciowa %>% mutate(across(where(is.numeric),replace_na, 0))
  tabela_wejsciowa                              <- tabela_wejsciowa %>% mutate(across(where(is.character),replace_na, "0"))
  tabela_wejsciowa                              <- tabela_wejsciowa%>% mutate_if(is.numeric, ~round(., 0)) 
   
}
  
  tabela_wejsciowa[tabela_wejsciowa=="Industrial Sales and Services"]      <-"Industr. Sales and Serv."
  tabela_wejsciowa[tabela_wejsciowa=="BES Essentials Industries"] <-"BES Essentials Ind."
  tabela_wejsciowa[tabela_wejsciowa=="Cleaning (II) - DISTRIBUTION"] <-"Cleaning DISTR."
  tabela_wejsciowa[tabela_wejsciowa=="Cleaning (II) - TOLL PRODUCTION"] <- "Cleaning TOLL PROD."
  tabela_wejsciowa[tabela_wejsciowa=="Coatings and construction"] <-"Coatings and constr."
  tabela_wejsciowa[tabela_wejsciowa=="Cleaning and cosmetics"] <-"Cleaning and cosm."
  tabela_wejsciowa[tabela_wejsciowa=="Packaging and accessories"] <-"Packaging and acc."
  tabela_wejsciowa[tabela_wejsciowa=="Sodium hydroxide 50Toll production - Diversey"] <-"Sodium hydroxide 50Toll prod. - Diversey"
  tabela_wejsciowa[tabela_wejsciowa=="Intercompany (inside CEE)"] <-"Intercompany (in CEE)"
  tabela_wejsciowa[tabela_wejsciowa=="Intercompany (outside CEE)"] <-"Intercompany (out CEE)"
  tabela_wejsciowa[tabela_wejsciowa=="Brenntag Ljubljana d.o.o."] <-"Brenntag Ljubljana"    
  tabela_wejsciowa[tabela_wejsciowa=="BCD Polymers sp. z.o.o."] <-"BCD Polymers"     
  tabela_wejsciowa[tabela_wejsciowa=="Eurochem Service Poland"] <-"Eurochem Service"     
  #tabela_wejsciowa[tabela_wejsciowa==""] <-     
  
 
  tabela_wejsciowa[,2:ncol(tabela_wejsciowa)] <- mutate_if(tabela_wejsciowa[,2:ncol(tabela_wejsciowa)], is.character, function(x) ifelse(nchar(x) > 7, "", x)) 
  # wyczyść pole tekstowe jeśli ma więcej niż 7 znaków
  
  return(list(tabela_wejsciowa,Boldowanie)) 
}


# 
# 
# CreateTable(CountriesByDivision)
# 
# 
# 
# 
# tabeliza <- CreateTable(BESBSP_IRPD)
# 
# asd<-tabeliza[[1]]
# 












