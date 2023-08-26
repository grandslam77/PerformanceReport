

#    ::::::::::. .,:::::: :::::::..  .-:::::'   ...    :::::::..   .        :    :::.   :::.    :::.  .,-::::: .,::::::      :::::::..  .,::::::::::::::::.    ...    :::::::.. ::::::::::::
#   ` ;;;```.;;;;;;;'''' ;;;;``;;;; ;;;'''' .;;;;;;;. ;;;;``;;;;  ;;,.    ;;;   ;;`;;  `;;;;,  `;;;,;;;'````' ;;;;''''      ;;;;``;;;; ;;;;'''' `;;;```.;;;.;;;;;;;. ;;;;``;;;;;;;;;;;;''''
#    `]]nnn]]'  [[cccc   [[[,/[[[' [[[,,==,[[     \[[,[[[,/[[['  [[[[, ,[[[[, ,[[ '[[,  [[[[[. '[[[[[         [[cccc        [[[,/[[['  [[cccc   `]]nnn]]',[[     \[[,[[[,/[[['     [[     
#    $$$""     $$""""   $$$$$$c   `$$$"``$$$,     $$$$$$$$$c    $$$$$$$$"$$$c$$$cc$$$c $$$ "Y$c$$$$$         $$""""        $$$$$$c    $$""""    $$$""   $$$,     $$$$$$$$$c       $$     
#    888o      888oo,__ 888b "88bo,888   "888,_ _,88P888b "88bo,888 Y88" 888o888   888,888    Y88`88bo,__,o, 888oo,__      888b "88bo,888oo,__  888o    "888,_ _,88P888b "88bo,   88,    
#   YMMMb     """"YUMMMMMMM   "W" "MM,    "YMMMMMP" MMMM   "W" MMM  M'  "MMMYMM   ""` MMM     YM  "YUMMMMMP"""""YUMMM     MMMM   "W" """"YUMMM YMMMb     "YMMMMMP" MMMM   "W"    MMM    


library(here)
library(timechange)
library(lubridate)
library(tinytex)
dataRaportu <- "2023-02-28"
comments  <- "NO"

    if(is.na(dataRaportu)){
        dataAnalizy  <<- as.Date(rollback(today()))
        zestawDanych <<- as.character(rollback(today()))
    }else{
        dataAnalizy  <<- as.Date(dataRaportu)
        zestawDanych <<- as.character(dataRaportu) 
    }

  # return(list(dataAnalizy,zestawDanych)) 
  
  # -- LIBRARIES LOADING  -----------------------------------------------------------------------------------------------------------------------------
  source(here::here("moduly","biblioteki.R"))
  
  # -- MAPPING LOADING --------------------------------------------------------------------------------------------------------------------------------
  source(here::here("moduly","mapowania.R"))
  
  # -- PERFORMANCE REPORT PARAMETERS ------------------------------------------------------------------------------------------------------------------
  source(here::here("moduly","parametry_do_raportu.R"))
  
  # -- RAW TABLES ARANGEMENT --------------------------------------------------------------------------------------------------------------------------
  source(here::here("moduly","przygotowanie_tabel.R"))                # przygotowanie do funkcji CreateTable, wybór zmiennych grupujących i grupowanych 
  
  # -- TABLE BUILDING FUNCTION LOADING ----------------------------------------------------------------------------------------------------------------
  source(here::here("moduly","create_table_fun.R"))                                # tworzenie listy tabel do raportu z numerami wyboldowanych wierszy
  
  # -- TABLE BUILDING FUNCTION LOADING ----------------------------------------------------------------------------------------------------------------
  source(here::here("moduly","create_charts_funvc_kol.R"))                                                                         # tworzenie wykresów
  
  # -- .PDF FILE RENDERING ----------------------------------------------------------------------------------------------------------------------------
  source(here::here("moduly","renderowanie_raportu.R")) 
  
  
  

