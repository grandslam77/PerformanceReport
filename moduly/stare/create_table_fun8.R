

CreateTable <-function(tabela_wejsciowa,nazwa_tabeli="", pomocnicza){
  options(dplyr.summarise.inform = FALSE)
  #tabela_wejsciowa$MaterialGroup                <- gsub("%", "PER", tabela_wejsciowa$MaterialGroup)
  BoldowaniePomocnicze<-NA

  
  ilosc_poziomow_grupowania                                  <- ncol(tabela_wejsciowa)-6
  
  colnames(tabela_wejsciowa )[ncol(tabela_wejsciowa)-5]      <- "Quantity KG TOTAL 2021"
  colnames(tabela_wejsciowa )[ncol(tabela_wejsciowa)-4]      <- "Quantity KG TOTAL 2022"
  colnames(tabela_wejsciowa )[ncol(tabela_wejsciowa)-3]      <- "Margin EUR (adj) TOTAL 2021"
  colnames(tabela_wejsciowa )[ncol(tabela_wejsciowa)-2]      <- "Margin EUR (adj) TOTAL 2022"
  colnames(tabela_wejsciowa )[ncol(tabela_wejsciowa)-1]      <- "Turnover EUR (adj) TOTAL 2021"
  colnames(tabela_wejsciowa )[ncol(tabela_wejsciowa)]        <- "Turnover EUR (adj) TOTAL 2022"
 #
  nazwy_kolumn_grupujacych                                   <- colnames(tabela_wejsciowa)[1:(ncol(tabela_wejsciowa)-6)]
  #zapasowa                                                   <- ifelse(nazwa_tabeli %in% c("MAIN_ESSP_TY","MAIN_ESSP_T"),zapasowa<-pomocnicza,zapasowa<-tabela_wejsciowa)
  #browser()
  zapasowa <- if (nazwa_tabeli %in% c("ICCPM_DATA","ICCPM_DATAY","DivisionsByCountry","DivisionsByCountry_YTD","CountriesByDivision_YTD","CountriesByDivision")) pomocnicza else tabela_wejsciowa
  #df <- df_name %in% c("MAIN_ESSP_TY", "MAIN_ESSP_T") ? pomocnicza : zapasowa
  
  colnames(zapasowa )[ncol(zapasowa)-5]      <- "Quantity KG TOTAL 2021"
  colnames(zapasowa )[ncol(zapasowa)-4]      <- "Quantity KG TOTAL 2022"
  colnames(zapasowa )[ncol(zapasowa)-3]      <- "Margin EUR (adj) TOTAL 2021"
  colnames(zapasowa )[ncol(zapasowa)-2]      <- "Margin EUR (adj) TOTAL 2022"
  colnames(zapasowa )[ncol(zapasowa)-1]      <- "Turnover EUR (adj) TOTAL 2021"
  colnames(zapasowa )[ncol(zapasowa)]        <- "Turnover EUR (adj) TOTAL 2022"
  


  
  tabela_wejsciowa                                    <- tabela_wejsciowa %>% group_by_at(vars(nazwy_kolumn_grupujacych)) %>% summarize(QActYear=sum(`Quantity KG TOTAL 2022`),QprevYear=sum(`Quantity KG TOTAL 2021`),GPACT=sum(`Margin EUR (adj) TOTAL 2022`),GPPREV=sum(`Margin EUR (adj) TOTAL 2021`),TURNACT=sum(`Turnover EUR (adj) TOTAL 2022`),TURNPREV=sum(`Turnover EUR (adj) TOTAL 2021`))#nazwy   # tutaj może w przyszłości najpierw select(all_off(nazwy_kolumn_grupujacych))
  #tabela_wejsciowa                              <- tabela_wejsciowa  %>% mutate(across(where(is.numeric), round))
  
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
  Kolejnosc_Sortowania                           <- c("QuantityDiffYYPER_P")

  tabela_wejsciowa                               <- setorderv(tabela_wejsciowa,Kolejnosc_Sortowania,-1)

  
# zasady sortowania tabel 
  
  
  if(nazwa_tabeli %in% c("TOP20GP","TOP20GPYTD")){
      tabela_wejsciowa                        <- setorderv(tabela_wejsciowa,c("GPDiffYY"),-1) 
    }else if(nazwa_tabeli %in% c("TOP20QUANT","TOP20QUANTYTD")){
      tabela_wejsciowa                        <- setorderv(tabela_wejsciowa,c("QuantityDiffYY"),-1)
    }else if(nazwa_tabeli %in% c("BOTTOM20GP","BOTTOM20GP_YTD")){
      tabela_wejsciowa                        <- setorderv(tabela_wejsciowa,c("GPDiffYY"),1)
    }else if(nazwa_tabeli %in% c("BOTTOM20QUANT","BOTTOM20QUANT_YTD")){  
      tabela_wejsciowa                        <- setorderv(tabela_wejsciowa,c("QuantityDiffYY"),1)
    }else if(nazwa_tabeli %in% c("MainTotalYTD","MainTotal","ICCPM_DATA","ICCPM_DATAY")){
      tabela_wejsciowa                        <- setorderv(tabela_wejsciowa,c("QuantityDiffYYPER_P"),-1)
  }
  
  
  
  
  if(nazwa_tabeli %in% c("MainTotalYTD","MainTotal","ICCPM_DATA","ICCPM_DATAY","BBSM","BBSM_YTD","BBS_EI,","BBS_EI_YTD","BBS_KEY_YTD")){
    tabela_wejsciowa                        <- setorderv(tabela_wejsciowa,c("QuantityDiffYYPER_P"),-1)
  }
  
  if(nazwa_tabeli %in% c("WT_THIRD","WT_THIRD_YTD","SALES_MAIN","SALES_MAIN_YTD", "TotalDivisionSalesParty", "TotalDivisionSalesParty_YTD","pomocnicza3party")){
    tabela_wejsciowa                        <- setorderv(tabela_wejsciowa,c("Clasification"),1)
  }
  
  if(nazwa_tabeli %in% c("PHOSPHATESFAS2a","PHOSPHATESFAS_YTD2a","pomocniczaFAS")){
    
    tabela_wejsciowa                        <- setorderv(tabela_wejsciowa,c("Mapowanie"),1)
  }
  
  if(nazwa_tabeli %in% c("BESBSP_CRD","BESBSP_CRD_YTD","BESBSP_SR","BESBSP_SR_YTD")){
    tabela_wejsciowa                        <- setorderv(tabela_wejsciowa,c("Product Division"),1)
  }
  
  
  

  
  tabela_wejsciowa$QuantityDiffYYPER_P           <- NULL


  
  

  
  if(nazwa_tabeli %in% c("MAIN_ESSP","MAIN_ESSP_YTD","BESBSP_PD_MT","BESBSP_PD_MT_YTD","MAIN_ESSP_T","MAIN_ESSP_TY","BESBSP_IRPD","BESBSP_IRPD_YTD","BESBSP_PDICCPM","BESBSP_PDICCPM_YTD","BESBSP_ICCPM","BESBSP_ICCPM_YTD")){
    tabela_wejsciowa$do_sortowania          <- ""
    tabela_wejsciowa$do_sortowania          <- ifelse(tabela_wejsciowa$MapowanieMD=="SP","B",ifelse(tabela_wejsciowa$MapowanieMD=="ES","A","C"))
    tabela_wejsciowa                        <- setorderv(tabela_wejsciowa,c("do_sortowania"),1)
    tabela_wejsciowa$do_sortowania          <- NULL
  }
  
  if(nazwa_tabeli %in% c("MAIN_WhDir_MTH","MAIN_WhDir_YTD")){
    tabela_wejsciowa$do_sortowania          <- ""
    tabela_wejsciowa$do_sortowania          <- ifelse(tabela_wejsciowa$WhsDirComm=="Direct","A",ifelse(tabela_wejsciowa$WhsDirComm=="Warehouse","B","C"))
    tabela_wejsciowa                        <- setorderv(tabela_wejsciowa,c(Kolejnosc_Sortowania[1:length(Kolejnosc_Sortowania)-1],"do_sortowania"))
    tabela_wejsciowa$do_sortowania          <- NULL
  }
  

  
  
  
   
  
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
  Total$QuantityDiffYYPER  <-  ifelse(is.infinite(Total$QuantityDiffYY/Total$QprevYear),"",scales::percent(Total$QuantityDiffYY/Total$QprevYear,accuracy = 0.1))
  Total$GPDiffYYPER        <-  ifelse(is.infinite(Total$GPDiffYY/Total$GPPREV),"",scales::percent(Total$GPDiffYY/Total$GPPREV,accuracy = 0.1))
  Total$TurnDiffYYPER      <-  ifelse(is.infinite(Total$TurnDiffYY/Total$TURNPREV),"",scales::percent(Total$TurnDiffYY/Total$TURNPREV,accuracy = 0.1))
  Total$GPMTDIFFPER        <-  ifelse(is.infinite(Total$GPMTDIFF/Total$GPMTPREV),"",scales::percent(Total$GPMTDIFF/Total$GPMTPREV,accuracy = 0.1))
  
  

  
  
  
  
  
  # --------------------------- 1 poziom grupowania -------------------------------------------------------
  
   if (ilosc_poziomow_grupowania==1){   
 
      #   if(startsWith(Nazwa,"T_TOP")){  # w zależnosci od tego jaka tabela wchodzi mozna dodać dodatkowe czynności
  #     tabela_wejsciowa                      <- setorderv(tabela_wejsciowa,colnames(tabela_wejsciowa)[3])
  #   }
  #   
  #   
    
    
    #tabela_wejsciowa                      <- setorderv(tabela_wejsciowa,colnames(tabela_wejsciowa)[1])
    #tabela_wejsciowa                      <- setorderv(tabela_wejsciowa,c("QuantityDiffYYPER"))
    #Total[colnames(tabela_wejsciowa)[1]]  <- "TOTAL CEE"
    Total[colnames(tabela_wejsciowa)[1]]  <- ifelse(startsWith(nazwa_tabeli,"TOP"),"TOTAL TOP 20",ifelse(startsWith(nazwa_tabeli, "BOTTOM"),"TOTAL BOTTOM 20","TOTAL CEE"))
      
 
     
    
    Total$A <-""
    Total    <-Total %>% select(17,1:16)
    colnames(Total)[1]<-colnames(tabela_wejsciowa)[1]

    
    tabela_wejsciowa                      <- rbind(tabela_wejsciowa,Total)
    
    Boldowanie                            <- nrow(tabela_wejsciowa)
    Podkreslanie                          <- NA
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
      frejm$QuantityDiffYYPER  <-  ifelse(is.infinite(frejm$QuantityDiffYY/frejm$QprevYear),"",scales::percent(frejm$QuantityDiffYY/frejm$QprevYear,accuracy = 0.1))
      frejm$GPDiffYYPER        <-  ifelse(is.infinite(frejm$GPDiffYY/frejm$GPPREV),"",scales::percent(frejm$GPDiffYY/frejm$GPPREV,accuracy = 0.1))
      frejm$TurnDiffYYPER      <-  ifelse(is.infinite(frejm$TurnDiffYY/frejm$TURNPREV),"",scales::percent(frejm$TurnDiffYY/frejm$TURNPREV,accuracy = 0.1))
      frejm$GPMTDIFFPER        <-  ifelse(is.infinite(frejm$GPMTDIFF/frejm$GPMTPREV),"",scales::percent(frejm$GPMTDIFF/frejm$GPMTPREV,accuracy = 0.1))
      
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
      Podkreslanie                            <- c(which(unlist(tabela_wejsciowa[,2]) %in% PoziomGrupowania1))
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
      frejm$QuantityDiffYYPER  <-  ifelse(is.infinite(frejm$QuantityDiffYY/frejm$QprevYear),"",scales::percent(frejm$QuantityDiffYY/frejm$QprevYear,accuracy = 0.1))
      frejm$GPDiffYYPER        <-  ifelse(is.infinite(frejm$GPDiffYY/frejm$GPPREV),"",scales::percent(frejm$GPDiffYY/frejm$GPPREV,accuracy = 0.1))
      frejm$TurnDiffYYPER      <-  ifelse(is.infinite(frejm$TurnDiffYY/frejm$TURNPREV),"",scales::percent(frejm$TurnDiffYY/frejm$TURNPREV,accuracy = 0.1))
      frejm$GPMTDIFFPER        <-  ifelse(is.infinite(frejm$GPMTDIFF/frejm$GPMTPREV),"",scales::percent(frejm$GPMTDIFF/frejm$GPMTPREV,accuracy = 0.1))
      
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
        frejm$QuantityDiffYYPER  <-  ifelse(is.infinite(frejm$QuantityDiffYY/frejm$QprevYear),"",scales::percent(frejm$QuantityDiffYY/frejm$QprevYear,accuracy = 0.1))
        frejm$GPDiffYYPER        <-  ifelse(is.infinite(frejm$GPDiffYY/frejm$GPPREV),"",scales::percent(frejm$GPDiffYY/frejm$GPPREV,accuracy = 0.1))
        frejm$TurnDiffYYPER      <-  ifelse(is.infinite(frejm$TurnDiffYY/frejm$TURNPREV),"",scales::percent(frejm$TurnDiffYY/frejm$TURNPREV,accuracy = 0.1))
        frejm$GPMTDIFFPER        <-  ifelse(is.infinite(frejm$GPMTDIFF/frejm$GPMTPREV),"",scales::percent(frejm$GPMTDIFF/frejm$GPMTPREV,accuracy = 0.1))
    
        
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
    Podkreslanie                          <- c(which(unlist(tabela_wejsciowa[,3]) %in% PoziomGrupowania1))
    
    
    
    #tabela_wejsciowa         <- rbind(tabela_wejsciowa,thereofCEE) na razie tą część tabelki trzeba odpóścić, do znalezienia rozwiązania
    tabela_wejsciowa[,1:2]           <- NULL

    
    tabela_wejsciowa                                  <- tabela_wejsciowa %>% select(1,2,3,8,9,4,5,10,11,6,7,12,13,14,15,16,17)
    tabela_wejsciowa  <- tabela_wejsciowa %>% mutate(across(where(is.numeric),replace_na, 0))
    tabela_wejsciowa  <- tabela_wejsciowa %>% mutate(across(where(is.character),replace_na, "0"))
    tabela_wejsciowa                                  <- tabela_wejsciowa%>% mutate_if(is.numeric, ~round(., 0)) 
  }
  
#### 4 poziomy grupowania ------------------------------------------------------------------------------------------

  if (ilosc_poziomow_grupowania==4){
  
  PoziomGrupowania1 <- sort(unlist(as.vector(as.data.frame(unique(tabela_wejsciowa[,1])))))
  PoziomGrupowania2 <- unlist(as.vector(as.data.frame(unique(tabela_wejsciowa[,2]))))
  PoziomGrupowania3 <- unlist(as.vector(as.data.frame(unique(tabela_wejsciowa[,3]))))
  #browser()
  
  # for(poziomgrupowania1 in PoziomGrupowania1){
  #   steal                    <- tabela_wejsciowa %>% filter(tabela_wejsciowa[,1]==poziomgrupowania1)
  #   BoldowaniePomocnicze     <- c(BoldowaniePomocnicze,nrow(steal))
  #   # frejm <- data.frame(PoziomGrupowania1=poziomgrupowania1, PoziomGrupowania2="ŻŻŻŻŻ", PoziomGrupowania3="ŻŻŻŻŻŻŻŻŻ", Cokolwiek=poziomgrupowania1,
  #   #                     QActYear          = sum(steal$QActYear),
  #   #                     QprevYear         = sum(steal$QprevYear),
  #   #                     GPACT             = sum(steal$GPACT),
  #   #                     GPPREV            = sum(steal$GPPREV),
  #   #                     TURNACT           = sum(steal$TURNACT),
  #   #                     TURNPREV          = sum(steal$TURNPREV),
  #   #                     QuantityDiffYY    = sum(steal$QuantityDiffYY),
  #   #                     QuantityDiffYYPER = 0,
  #   #                     GPDiffYYPER       = 0,
  #   #                     GPDiffYY          = sum(steal$GPDiffYY),
  #   #                     TurnDiffYY        = sum(steal$TurnDiffYY),
  #   #                     TurnDiffYYPER     = 0,
  #   #                     GPMTACT           = 0,
  #   #                     GPMTPREV          = 0,
  #   #                     GPMTDIFF          = 0,
  #   #                     GPMTDIFFPER       = 0)
  #   # 
  #   # frejm$GPMTACT            <-  frejm$GPACT*1000/frejm$QActYear
  #   # frejm$GPMTPREV           <-  frejm$GPPREV*1000/frejm$QprevYear
  #   # frejm$TurnDiffYY         <-  frejm$TURNACT-frejm$TURNPREV
  #   # frejm$QuantityDiffYY     <-  frejm$QActYear-frejm$QprevYear
  #   # frejm$GPDiffYY           <-  frejm$GPACT-frejm$GPPREV
  #   # frejm$GPMTDIFF           <-  frejm$GPMTACT-frejm$GPMTPREV
  #   # frejm$QuantityDiffYYPER  <-  ifelse(is.infinite(frejm$QuantityDiffYY/frejm$QprevYear),"",scales::percent(frejm$QuantityDiffYY/frejm$QprevYear,accuracy = 0.1))
  #   # frejm$GPDiffYYPER        <-  ifelse(is.infinite(frejm$GPDiffYY/frejm$GPPREV),"",scales::percent(frejm$GPDiffYY/frejm$GPPREV,accuracy = 0.1))
  #   # frejm$TurnDiffYYPER      <-  ifelse(is.infinite(frejm$TurnDiffYY/frejm$TURNPREV),"",scales::percent(frejm$TurnDiffYY/frejm$TURNPREV,accuracy = 0.1))
  #   # frejm$GPMTDIFFPER        <-  ifelse(is.infinite(frejm$GPMTDIFF/frejm$GPMTPREV),"",scales::percent(frejm$GPMTDIFF/frejm$GPMTPREV,accuracy = 0.1))
  #   # 
  #   # colnames(frejm)[1]       <- colnames(tabela_wejsciowa)[1]
  #   # colnames(frejm)[2]       <- colnames(tabela_wejsciowa)[2]
  #   # colnames(frejm)[3]       <- colnames(tabela_wejsciowa)[3]
  #   # colnames(frejm)[4]       <- colnames(tabela_wejsciowa)[4]
  #   
  #   
  # }
  # 
  
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
      frejm$QuantityDiffYYPER  <-  ifelse(is.infinite(frejm$QuantityDiffYY/frejm$QprevYear),"",scales::percent(frejm$QuantityDiffYY/frejm$QprevYear,accuracy = 0.1))
      frejm$GPDiffYYPER        <-  ifelse(is.infinite(frejm$GPDiffYY/frejm$GPPREV),"",scales::percent(frejm$GPDiffYY/frejm$GPPREV,accuracy = 0.1))
      frejm$TurnDiffYYPER      <-  ifelse(is.infinite(frejm$TurnDiffYY/frejm$TURNPREV),"",scales::percent(frejm$TurnDiffYY/frejm$TURNPREV,accuracy = 0.1))
      frejm$GPMTDIFFPER        <-  ifelse(is.infinite(frejm$GPMTDIFF/frejm$GPMTPREV),"",scales::percent(frejm$GPMTDIFF/frejm$GPMTPREV,accuracy = 0.1))
      
      
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
      frejm$QuantityDiffYYPER  <-  ifelse(is.infinite(frejm$QuantityDiffYY/frejm$QprevYear),"",scales::percent(frejm$QuantityDiffYY/frejm$QprevYear,accuracy = 0.1))
      frejm$GPDiffYYPER        <-  ifelse(is.infinite(frejm$GPDiffYY/frejm$GPPREV),"",scales::percent(frejm$GPDiffYY/frejm$GPPREV,accuracy = 0.1))
      frejm$TurnDiffYYPER      <-  ifelse(is.infinite(frejm$TurnDiffYY/frejm$TURNPREV),"",scales::percent(frejm$TurnDiffYY/frejm$TURNPREV,accuracy = 0.1))
      frejm$GPMTDIFFPER        <-  ifelse(is.infinite(frejm$GPMTDIFF/frejm$GPMTPREV),"",scales::percent(frejm$GPMTDIFF/frejm$GPMTPREV,accuracy = 0.1))
      
      
      colnames(frejm)[1]       <- colnames(tabela_wejsciowa)[1]
      colnames(frejm)[2]       <- colnames(tabela_wejsciowa)[2]
      colnames(frejm)[3]       <- colnames(tabela_wejsciowa)[3]
      colnames(frejm)[4]       <- colnames(tabela_wejsciowa)[4]
      tabela_wejsciowa         <- rbind(tabela_wejsciowa, frejm)
      }
    }
  }
  } 

  for(poziomgrupowania1 in PoziomGrupowania1){
    steal                    <- tabela_wejsciowa %>% filter(tabela_wejsciowa[,1]==poziomgrupowania1)
    BoldowaniePomocnicze     <- c(BoldowaniePomocnicze,paste(poziomgrupowania1,nrow(steal),sep=""))
 
      # frejm <- data.frame(PoziomGrupowania1=poziomgrupowania1, PoziomGrupowania2="ŻŻŻŻŻ", PoziomGrupowania3="ŻŻŻŻŻŻŻŻŻ", Cokolwiek=poziomgrupowania1,
    #                     QActYear          = sum(steal$QActYear),
    #                     QprevYear         = sum(steal$QprevYear),
    #                     GPACT             = sum(steal$GPACT),
    #                     GPPREV            = sum(steal$GPPREV),
    #                     TURNACT           = sum(steal$TURNACT),
    #                     TURNPREV          = sum(steal$TURNPREV),
    #                     QuantityDiffYY    = sum(steal$QuantityDiffYY),
    #                     QuantityDiffYYPER = 0,
    #                     GPDiffYYPER       = 0,
    #                     GPDiffYY          = sum(steal$GPDiffYY),
    #                     TurnDiffYY        = sum(steal$TurnDiffYY),
    #                     TurnDiffYYPER     = 0,
    #                     GPMTACT           = 0,
    #                     GPMTPREV          = 0,
    #                     GPMTDIFF          = 0,
    #                     GPMTDIFFPER       = 0)
    # 
    # frejm$GPMTACT            <-  frejm$GPACT*1000/frejm$QActYear
    # frejm$GPMTPREV           <-  frejm$GPPREV*1000/frejm$QprevYear
    # frejm$TurnDiffYY         <-  frejm$TURNACT-frejm$TURNPREV
    # frejm$QuantityDiffYY     <-  frejm$QActYear-frejm$QprevYear
    # frejm$GPDiffYY           <-  frejm$GPACT-frejm$GPPREV
    # frejm$GPMTDIFF           <-  frejm$GPMTACT-frejm$GPMTPREV
    # frejm$QuantityDiffYYPER  <-  ifelse(is.infinite(frejm$QuantityDiffYY/frejm$QprevYear),"",scales::percent(frejm$QuantityDiffYY/frejm$QprevYear,accuracy = 0.1))
    # frejm$GPDiffYYPER        <-  ifelse(is.infinite(frejm$GPDiffYY/frejm$GPPREV),"",scales::percent(frejm$GPDiffYY/frejm$GPPREV,accuracy = 0.1))
    # frejm$TurnDiffYYPER      <-  ifelse(is.infinite(frejm$TurnDiffYY/frejm$TURNPREV),"",scales::percent(frejm$TurnDiffYY/frejm$TURNPREV,accuracy = 0.1))
    # frejm$GPMTDIFFPER        <-  ifelse(is.infinite(frejm$GPMTDIFF/frejm$GPMTPREV),"",scales::percent(frejm$GPMTDIFF/frejm$GPMTPREV,accuracy = 0.1))
    # 
    # colnames(frejm)[1]       <- colnames(tabela_wejsciowa)[1]
    # colnames(frejm)[2]       <- colnames(tabela_wejsciowa)[2]
    # colnames(frejm)[3]       <- colnames(tabela_wejsciowa)[3]
    # colnames(frejm)[4]       <- colnames(tabela_wejsciowa)[4]
    
    
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
  Podkreslanie                                  <- c(which(unlist(tabela_wejsciowa[,4]) %in% PoziomGrupowania2))
  
 
  tabela_wejsciowa[,1:3]                        <- NULL

  tabela_wejsciowa                              <- tabela_wejsciowa %>% select(1,2,3,8,9,4,5,10,11,6,7,12:17)
  tabela_wejsciowa                              <- tabela_wejsciowa %>% mutate(across(where(is.numeric),replace_na, 0))
  tabela_wejsciowa                              <- tabela_wejsciowa %>% mutate(across(where(is.character),replace_na, "0"))
  tabela_wejsciowa                              <- tabela_wejsciowa%>% mutate_if(is.numeric, ~round(., 0)) 
  BoldowaniePomocnicze                          <- BoldowaniePomocnicze[2:length(BoldowaniePomocnicze)]
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
  
  
  if(nazwa_tabeli %in% c("MAIN_ESSP","MAIN_ESSP_YTD","MAIN_WhDir_MTH","MAIN_WhDir_YTD","SALES_MAIN_YTD","SALES_MAIN")){
   #browser()
    zapasowa                                     <- zapasowa[,-1]
    podtabela                                    <- CreateTable(zapasowa)
    doczepka                                     <- podtabela[[1]]
    tabela_wejsciowa                             <- tabela_wejsciowa[-nrow(tabela_wejsciowa),]
    doczepka                                     <- setorderv(doczepka,c(colnames(doczepka)[2]),-1)
    tabela_wejsciowa                             <- rbind(tabela_wejsciowa,doczepka)
    
    if(!nazwa_tabeli %in% c("MAIN_WhDir_MTH","MAIN_WhDir_YTD")){
    tabela_wejsciowa[nrow(tabela_wejsciowa),1]   <- paste("Total",tabela_wejsciowa[nrow(tabela_wejsciowa),1])
    tabela_wejsciowa[nrow(tabela_wejsciowa)-1,1] <- paste("Total",tabela_wejsciowa[nrow(tabela_wejsciowa)-1,1])
    tabela_wejsciowa[nrow(tabela_wejsciowa)-2,1] <- paste("Total",tabela_wejsciowa[nrow(tabela_wejsciowa)-2,1])
    }else{
      z                                          <- tabela_wejsciowa[nrow(tabela_wejsciowa),]
      tabela_wejsciowa[nrow(tabela_wejsciowa)]   <- tabela_wejsciowa[nrow(tabela_wejsciowa)-1]
      tabela_wejsciowa[nrow(tabela_wejsciowa)-1] <- z
      rm(z)
    }
    # 
    # 
    
    }
    
  if(nazwa_tabeli %in% c("ICCPM_DATA","ICCPM_DATAY")){
    
    podtabela                                    <- CreateTable(pomocnicza)
    doczepka                                     <- podtabela[[1]]
    #tabela_wejsciowa                             <- tabela_wejsciowa[-nrow(tabela_wejsciowa),]
    colnames(doczepka)[1]<-colnames(tabela_wejsciowa)[1]
    doczepka                                     <- doczepka[-nrow(doczepka),]
    doczepka                                     <- setorderv(doczepka,c(colnames(doczepka)[2]),-1)
    tabela_wejsciowa                             <- rbind(tabela_wejsciowa,doczepka)
    tabela_wejsciowa[nrow(tabela_wejsciowa),1]   <- paste("thereof",tabela_wejsciowa[nrow(tabela_wejsciowa),1])
    tabela_wejsciowa[nrow(tabela_wejsciowa)-1,1] <- paste("thereof",tabela_wejsciowa[nrow(tabela_wejsciowa)-1,1])
  
  }
  
  if(nazwa_tabeli %in% c("TotalDivisionSalesParty","TotalDivisionSalesParty_YTD")){
    zapasowa                                     <- zapasowa[,-2]
    podtabela                                    <- CreateTable(zapasowa,nazwa_tabeli = "pomocnicza3party")
    doczepka                                     <- podtabela[[1]]
    doczepka                                     <- doczepka[-nrow(doczepka),]
    BoldowaniePomocnicze                         <- podtabela[[2]]
    BoldowaniePomocnicze                         <- BoldowaniePomocnicze + nrow(tabela_wejsciowa)
    #doczepka                                     <- setorderv(doczepka,c(colnames(doczepka)[2]),-1)
    tabela_wejsciowa                             <- rbind(tabela_wejsciowa,doczepka)
    tabela_wejsciowa[nrow(tabela_wejsciowa),1]   <- paste("Total",tabela_wejsciowa[nrow(tabela_wejsciowa),1])
    tabela_wejsciowa[nrow(tabela_wejsciowa)-1,1] <- paste("Total",tabela_wejsciowa[nrow(tabela_wejsciowa)-1,1])
    ifelse(nrow(doczepka)>2,paste("Total",tabela_wejsciowa[nrow(tabela_wejsciowa)-2,1]),tabela_wejsciowa)
    
    
  }
  
  
  if(nazwa_tabeli %in% c("DivisionsByCountry","DivisionsByCountry_YTD")){
 
    #zapasowa                                     <- zapasowa[,-2]
    podtabela                                    <- CreateTable(pomocnicza)
    doczepka                                     <- podtabela[[1]]
    BoldowaniePomocnicze                         <- podtabela[[2]]
    BoldowaniePomocnicze                         <- BoldowaniePomocnicze + nrow(tabela_wejsciowa)
    #tabela_wejsciowa                             <- tabela_wejsciowa[-nrow(tabela_wejsciowa),]
    #doczepka                                     <- setorderv(doczepka,c(colnames(doczepka)[2]),-1)
    
    colnames(doczepka)[1] <-colnames(tabela_wejsciowa)[1]
    tabela_wejsciowa                             <- rbind(tabela_wejsciowa,doczepka)
    tabela_wejsciowa                             <- tabela_wejsciowa[-nrow(tabela_wejsciowa),]
    #tabela_wejsciowa[nrow(tabela_wejsciowa),1]   <- paste("Total",tabela_wejsciowa[nrow(tabela_wejsciowa),1])
    #tabela_wejsciowa[nrow(tabela_wejsciowa)-1,1] <- paste("Total",tabela_wejsciowa[nrow(tabela_wejsciowa)-1,1])
    #ifelse(nrow(doczepka)>2,paste("Total",tabela_wejsciowa[nrow(tabela_wejsciowa)-2,1]),tabela_wejsciowa)
    
  }
  
  if(nazwa_tabeli %in% c("CountriesByDivision_YTD","CountriesByDivision")){
    
    #zapasowa                                     <- zapasowa[,-2]
    podtabela                                    <- CreateTable(pomocnicza,nazwa_tabeli = "pomocniczaFAS")
    doczepka                                     <- podtabela[[1]]
    BoldowaniePomocnicze                         <- podtabela[[2]]
    BoldowaniePomocnicze                         <- BoldowaniePomocnicze + nrow(tabela_wejsciowa)
    #tabela_wejsciowa                             <- tabela_wejsciowa[-nrow(tabela_wejsciowa),]
    #doczepka                                     <- setorderv(doczepka,c(colnames(doczepka)[2]),-1)
    
    colnames(doczepka)[1] <-colnames(tabela_wejsciowa)[1]
    tabela_wejsciowa                             <- rbind(tabela_wejsciowa,doczepka)
    tabela_wejsciowa                             <- tabela_wejsciowa[-nrow(tabela_wejsciowa),]
    #tabela_wejsciowa[nrow(tabela_wejsciowa),1]   <- paste("Total",tabela_wejsciowa[nrow(tabela_wejsciowa),1])
    #tabela_wejsciowa[nrow(tabela_wejsciowa)-1,1] <- paste("Total",tabela_wejsciowa[nrow(tabela_wejsciowa)-1,1])
    #ifelse(nrow(doczepka)>2,paste("Total",tabela_wejsciowa[nrow(tabela_wejsciowa)-2,1]),tabela_wejsciowa)
    
  }
  

  
  
   return(list(tabela_wejsciowa,Boldowanie,Podkreslanie,BoldowaniePomocnicze))  
  
  }
  
# 
# tab  <- CreateTable(BESBSP_DI_YTD2,nazwa_tabeli = "BESBSP_DI_YTD2")
# 
# asd  <- tab[[1]]












