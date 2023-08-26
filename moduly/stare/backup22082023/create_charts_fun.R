

TopBottom <- function(TabelaWejsciowaT,Przedrostek){
  #browser()
  tabelaWejsciowa1_PLOT                <- aggregate(. ~ MaterialGroup, data = TabelaWejsciowaT , FUN = sum)
  tabelaWejsciowa1_PLOT$ZMIANA         <- round((tabelaWejsciowa1_PLOT$`Margin EUR (adj) TOTAL 2021`-tabelaWejsciowa1_PLOT$`Margin EUR (adj) TOTAL 2022` )/tabelaWejsciowa1_PLOT$`Margin EUR (adj) TOTAL 2022`,2)*100
  tabelaWejsciowa1_PLOT$ROZNICA        <- tabelaWejsciowa1_PLOT$`Margin EUR (adj) TOTAL 2021` - tabelaWejsciowa1_PLOT$`Margin EUR (adj) TOTAL 2022`
  tabelaWejsciowa1_PLOT                <- tabelaWejsciowa1_PLOT %>% select(1,4,5,8,9)
  if(Przedrostek=="TOP20"){
    tabelaWejsciowa1_PLOT              <- tabelaWejsciowa1_PLOT[order(-tabelaWejsciowa1_PLOT$ZMIANA),] 
  }else{
    tabelaWejsciowa1_PLOT              <- tabelaWejsciowa1_PLOT[order(tabelaWejsciowa1_PLOT$ZMIANA),] 
  }
    
  #tabelaWejsciowa1_PLOT               <- tabelaWejsciowa1_PLOT[order(-tabelaWejsciowa1_PLOT$ZMIANA),] 
  tabelaWejsciowa1_PLOTREST            <- tabelaWejsciowa1_PLOT[11:20,] 
  tabelaWejsciowa1_PLOT                <- tabelaWejsciowa1_PLOT[1:10,c(1,4)] 
  NTop20                               <- Przedrostek
  DODATEK                              <- data.frame(MaterialGroup=paste("REST OF",NTop20),ZMIANA=round((sum(tabelaWejsciowa1_PLOTREST$`Margin EUR (adj) TOTAL 2021`)-sum(tabelaWejsciowa1_PLOTREST$`Margin EUR (adj) TOTAL 2022`))/sum(tabelaWejsciowa1_PLOTREST$`Margin EUR (adj) TOTAL 2022`),2)*100)
  tabelaWejsciowa1_PLOT                <- rbind(tabelaWejsciowa1_PLOT,DODATEK)
  tabelaWejsciowa1_PLOT$MaterialGroup  <- factor(tabelaWejsciowa1_PLOT$MaterialGroup, levels = tabelaWejsciowa1_PLOT$MaterialGroup) #aby wykres pokazywał się od największej wartości do najmniejszej
  
  tabelaWejsciowa2_PLOT                <- aggregate(. ~ MaterialGroup, data = TabelaWejsciowaT , FUN = sum)
  tabelaWejsciowa2_PLOT$ZMIANA         <- round((tabelaWejsciowa2_PLOT$`Quantity KG TOTAL 2021`-tabelaWejsciowa2_PLOT$`Quantity KG TOTAL 2022`)/tabelaWejsciowa2_PLOT$`Quantity KG TOTAL 2022`,2)*100
  tabelaWejsciowa2_PLOT$ROZNICA        <- tabelaWejsciowa2_PLOT$`Quantity KG TOTAL 2021` - tabelaWejsciowa2_PLOT$`Quantity KG TOTAL 2022`
  tabelaWejsciowa2_PLOT                <- tabelaWejsciowa2_PLOT %>% select(1,2,3,8,9)
  
  if(Przedrostek=="TOP20"){
    tabelaWejsciowa2_PLOT              <- tabelaWejsciowa2_PLOT[order(-tabelaWejsciowa2_PLOT$ZMIANA),] 
  }else{
    tabelaWejsciowa2_PLOT              <- tabelaWejsciowa2_PLOT[order(tabelaWejsciowa2_PLOT$ZMIANA),] 
  }
    
  
  #tabelaWejsciowa2_PLOT                <- tabelaWejsciowa2_PLOT[order(-tabelaWejsciowa2_PLOT$ZMIANA),] 
  tabelaWejsciowa2_PLOTREST            <- tabelaWejsciowa2_PLOT[11:20,] 
  tabelaWejsciowa2_PLOT                <- tabelaWejsciowa2_PLOT[1:10,c(1,4)] 
  DODATEK                              <- data.frame(MaterialGroup=paste("REST OF",NTop20),ZMIANA=round((sum(tabelaWejsciowa2_PLOTREST$`Quantity KG TOTAL 2021`)-sum(tabelaWejsciowa2_PLOTREST$`Quantity KG TOTAL 2022`))/sum(tabelaWejsciowa2_PLOTREST$`Quantity KG TOTAL 2022`),2)*100)
  tabelaWejsciowa2_PLOT                <- rbind(tabelaWejsciowa2_PLOT,DODATEK)
  tabelaWejsciowa2_PLOT$MaterialGroup  <- factor(tabelaWejsciowa2_PLOT$MaterialGroup, levels = tabelaWejsciowa2_PLOT$MaterialGroup) #aby wykres pokazywał się od największej wartości do najmniejszej
  
  Margin_plot<-ggplot(tabelaWejsciowa1_PLOT, aes(x = MaterialGroup, y = ZMIANA,fill="#A03FFF" )) +
    #geom_col(fill = "transparent", color = "#800080") +
    geom_col() +
    scale_fill_manual(values = "#A03FFF") +
    labs(title = "Monthly Gross Proffit Difference (%Y/Y)", x = "", y = "") +
    theme(legend.position = "none") +
    geom_text(aes(y = 0, label = str_wrap(paste(ZMIANA,"%",sep=""), width = 10)),vjust = ifelse(tabelaWejsciowa1_PLOT$ZMIANA>0,1.5,-0.5), size=2.2,colour=ifelse(tabelaWejsciowa1_PLOT$ZMIANA>0,"black","black")) + #,fontface="bold"
    theme(plot.title = element_text(hjust = 0.5, vjust = -0.5,size = 6.5,face="bold"), plot.margin = unit(c(0, 0.5, 0, 0), "cm")) +
    theme(axis.line.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    theme(axis.line.x = element_blank(), axis.text.x = element_text(size = 5), axis.ticks.x = element_blank()) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    #theme(panel.grid = element_blank())+
    theme(panel.background = element_blank())
  #theme(panel.grid.major.x=element_line(linetype = "dashed",colour = "light gray"))
  
  Quantity_plot<-ggplot(tabelaWejsciowa2_PLOT, aes(x = MaterialGroup, y = ZMIANA,fill="#C815E0" )) +
    geom_col() +
    scale_fill_manual(values = "#C815E0") +
    labs(title = "Monthly Quantity Difference (%Y/Y)", x = "", y = "") +
    theme(legend.position = "none")+
    geom_text(aes(y = 0, label = str_wrap(paste(ZMIANA,"%",sep=""), width = 10)),vjust = ifelse(tabelaWejsciowa2_PLOT$ZMIANA>0,1.5,-0.5), size=2.2,colour=ifelse(tabelaWejsciowa2_PLOT$ZMIANA>0,"black","black"))+   #,fontface="bold"
    theme(plot.title = element_text(hjust = 0.5, vjust = -0.5,size = 6.5,face="bold"))+
    theme(axis.line.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())+
    theme(axis.line.x = element_blank(), axis.text.x = element_text(size = 5), axis.ticks.x = element_blank())+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
    # theme(panel.grid = element_blank())+
    theme(panel.background = element_blank())
    # theme(panel.grid.major.x=element_line(linetype = "dashed",colour = "light gray"))
  
  Wykres <-Quantity_plot + Margin_plot 
  return(Wykres)
  
}


library(dplyr)
library(patchwork)
library(png)
Sys.setlocale(category = "LC_TIME", locale="en_GB.UTF-8")


bejzsize           <- 9
geompojtsize       <- 2
gruboscLini        <- 1

etykietysize       <- 3
ledzendkejsize     <- 0.08
mardzin            <- 0
ledzendposx        <- -0.01
ledzendposy        <- -0.01
dol                <- 0.5
gora               <- 0.5

#ggsave
wysokoscObrazka    <- 40
szerokoscObrazka   <- 20+5*MiesiacAnalizy
piksele            <- 100
unity              <-"mm"

RozszerzGoreOsi    <- 0.01
RozszerzDolOsi     <- 0.5
EtykietaOsiX       <- ""



EtykietaIlosci <- "QUANTITY IN kMT"
EtykietaMarz   <- "AMOUNT (in mio EUR)"
EtykietaKwot   <- "EUR"


bazaQuant              <- bazaPerformance[bazaPerformance$NazwaWskaznika %in% c("Quant","QuantPRV") ,]
bazaQuant              <- bazaQuant[bazaQuant$NumerMiesiaca<=MiesiacAnalizy,]
bazaQuant              <- bazaQuant[,c(2,4,5)]
bazaQuant              <- bazaQuant[order(bazaQuant$DataAnalizy),]
bazaQuant$DataAnalizy2 <- bazaQuant$DataAnalizy - days(day(bazaQuant$DataAnalizy))


# bazaQuantYTD  <- bazaPerformance[bazaPerformance$NazwaWskaznika %in% c("Quant_YTD","Quant_YTD_PRV"),]
# bazaQuantYTD  <- bazaQuantYTD[bazaQuantYTD$NumerMiesiaca<=MiesiacAnalizy,]
# bazaQuantYTD  <- bazaQuantYTD[,c(2,4,5)]

bazaMargin              <- bazaPerformance[bazaPerformance$NazwaWskaznika %in% c("GrossProf","GrossProf_PRV"),]
bazaMargin              <- bazaMargin[bazaMargin$NumerMiesiaca<=MiesiacAnalizy,]
bazaMargin              <- bazaMargin[,c(2,4,5)]
bazaMargin$DataAnalizy2 <- bazaMargin$DataAnalizy - days(day(bazaMargin$DataAnalizy))
# 
# bazaMarginYTD <- bazaPerformance[bazaPerformance$NazwaWskaznika %in% c("GrossProf_YTD","GrossProf_PRV_YTD"),]
# bazaMarginYTD <- bazaMarginYTD[bazaMarginYTD$NumerMiesiaca<=MiesiacAnalizy,]
# bazaMarginYTD <- bazaMarginYTD[,c(2,4,5)]

bazaTurn              <- bazaPerformance[bazaPerformance$NazwaWskaznika %in% c("Turnov","Turnov_PRV"),]
bazaTurn              <- bazaTurn[bazaTurn$NumerMiesiaca<=MiesiacAnalizy,]
bazaTurn              <- bazaTurn[,c(2,4,5)]
bazaTurn$DataAnalizy2 <- bazaTurn$DataAnalizy - days(day(bazaTurn$DataAnalizy))

# bazaTurnYTD   <- bazaPerformance[bazaPerformance$NazwaWskaznika %in% c("Turnov_YTD","Turnov_PRV_YTD"),]
# bazaTurnYTD   <- bazaTurnYTD[bazaTurnYTD$NumerMiesiaca<=MiesiacAnalizy,]
# bazaTurnYTD   <- bazaTurnYTD[,c(2,4,5)]
# 

###################################################################################
##### WYKRES MARŻE  
###################################################################################

bazaQuant$NazwaWskaznika[bazaQuant$NazwaWskaznika=="Quant"]<-as.character(year(dataAnalizy))
bazaQuant$NazwaWskaznika[bazaQuant$NazwaWskaznika=="QuantPRV"]<-as.character(year(dataAnalizy)-1)

WykresQuant <- 
  ggplot(data=bazaQuant,
  aes(x=as.POSIXct(DataAnalizy2), y=Wartosc/1000, colour=NazwaWskaznika)) +
  geom_line(linewidth=gruboscLini)+
  geom_point(size=geompojtsize)+
  scale_y_continuous(limits = c(min(bazaQuant$Wartosc/1000), max(bazaQuant$Wartosc/1000)+500/1000), expand = c(RozszerzGoreOsi, RozszerzDolOsi)) +
  
  scale_x_datetime(limits = c(as.POSIXct(ymd(min(bazaQuant$DataAnalizy2)) %m-% period("1 week")),as.POSIXct(ymd(max(bazaQuant$DataAnalizy2)) %m+% period("1 week"))),date_labels = "%b",expand = c(0.05, 0.05),date_breaks = "1 month")+
  labs(title="CEE QUANTITY",
       x = EtykietaOsiX, 
       y = EtykietaIlosci,
       colour="")+
  scale_color_manual(values=c('Black','#911DA8'))+
  theme_minimal(base_size = bejzsize)+## output not shown, it's equivalent to the below graph (with a tiny difference in the legend title)
  geom_text(aes(label=round(Wartosc/1000,1)),hjust=0.9, vjust=-1.2,size=etykietysize,color="black")+
  theme(axis.title.x = element_text(margin = margin(t = mardzin)))+
  theme(axis.title.y = element_text(margin = margin(r = mardzin)))+
  theme(legend.key.size = unit(ledzendkejsize, "cm"))+
  theme(legend.position = c(ledzendposx, ledzendposy), plot.title = element_text(face="bold"), axis.title = element_text(face="bold"), plot.margin = unit(c(gora, dol, 0.00, 0.00), "cm"),
  panel.grid.major = element_blank())#,  # Usuwa główne linie siatki
  #panel.grid.minor = element_blank())  # Usuwa doda

ggsave(
  filename = here::here("szkielet","WykresQuant.png"),
  plot = WykresQuant,
  width = 1.75,
  height = 1.0,
  dpi = 720)

# WYKRES Margin     WykresMargin
########################################################################################################

bazaMargin$NazwaWskaznika[bazaMargin$NazwaWskaznika=="GrossProf"]<-as.character(year(dataAnalizy))
bazaMargin$NazwaWskaznika[bazaMargin$NazwaWskaznika=="GrossProf_PRV"]<-as.character(year(dataAnalizy)-1)

WykresMargin <- 
  ggplot(data=bazaMargin,
         aes(x=as.POSIXct(DataAnalizy2), y=Wartosc/1000, colour=NazwaWskaznika)) +
  geom_line(linewidth=gruboscLini)+
  geom_point(size=geompojtsize)+
  scale_y_continuous(limits = c(min(bazaMargin$Wartosc/1000), max(bazaMargin$Wartosc/1000)+500/1000), expand = c(RozszerzGoreOsi, RozszerzDolOsi)) +
  scale_color_manual(values=c('Black','#911DA8'))+
  scale_x_datetime(limits = c(as.POSIXct(ymd(min(bazaMargin$DataAnalizy2)) %m-% period("1 week")),as.POSIXct(ymd(max(bazaMargin$DataAnalizy2)) %m+% period("1 week"))),date_labels = "%b",expand = c(0.01, 0.01),date_breaks = "1 month")+
  labs(title="CEE MARGIN",
       x = EtykietaOsiX, 
       y = EtykietaMarz,
       colour="")+
  
  theme_minimal(base_size = bejzsize)+## output not shown, it's equivalent to the below graph (with a tiny difference in the legend title)
  geom_text(aes(label=round(Wartosc/1000,1)),hjust=0.9, vjust=-1.2,size=etykietysize,color="black")+
  theme(axis.title.x = element_text(margin = margin(t = mardzin)))+
  theme(axis.title.y = element_text(margin = margin(r = mardzin)))+
  theme(legend.key.size = unit(ledzendkejsize, "cm"))+
  theme(legend.position = c(ledzendposx, ledzendposy), plot.title = element_text(face="bold"), axis.title = element_text(face="bold"),plot.margin = unit(c(gora, dol, 0, 0), "cm"),
  panel.grid.major = element_blank())


# WYKRES Turnover     WykresTurnov
########################################################################################################


bazaTurn$NazwaWskaznika[bazaTurn$NazwaWskaznika=="Turnov"]        <- as.character(year(dataAnalizy))
bazaTurn$NazwaWskaznika[bazaTurn$NazwaWskaznika=="Turnov_PRV"]    <- as.character(year(dataAnalizy)-1)

WykresTurnov <- 
  ggplot(data=bazaTurn,
         aes(x=as.POSIXct(DataAnalizy2), y=Wartosc/1000, colour=NazwaWskaznika)) +
  geom_line(linewidth=gruboscLini)+
  geom_point(size=geompojtsize)+
  scale_y_continuous(limits = c(min(bazaTurn$Wartosc/1000), max(bazaTurn$Wartosc/1000)+500/1000), expand = c(RozszerzGoreOsi, RozszerzDolOsi)) +
  
  scale_x_datetime(limits = c(as.POSIXct(ymd(min(bazaTurn$DataAnalizy2)%m-% period("7 days"))),as.POSIXct(ymd(max(bazaTurn$DataAnalizy2)%m+% period("1 week")))),date_labels = "%b",date_breaks = "1 month")+
  scale_color_manual(values=c('Black','#911DA8'))+
  labs(title="CEE TURNOVER",
       x = EtykietaOsiX, 
       y = EtykietaMarz,
       colour="")+
  
  theme_minimal(base_size = bejzsize)+## output not shown, it's equivalent to the below graph (with a tiny difference in the legend title)
  geom_text(aes(label=round(Wartosc/1000,1)),hjust=0.9, vjust=-1.2,size=etykietysize,color="black")+
  theme(axis.title.x = element_text(margin = margin(t = mardzin)))+
  theme(axis.title.y = element_text(margin = margin(r = mardzin)))+
  theme(legend.key.size = unit(ledzendkejsize, "cm"))+
  theme(legend.position = c(ledzendposx, ledzendposy), plot.title = element_text(face="bold"),axis.title = element_text(face="bold"),plot.margin = unit(c(gora, 1, 0, 0), "cm"),
  panel.grid.major = element_blank())

WykresZZ<-(WykresQuant/WykresMargin/WykresTurnov)# +theme(plot.margin = unit(c(-1, -1, 0, 0), "cm"))



# 
# wykresKolumnowyQuant <- ggplot() +
#   geom_col(data = dane, aes(x = Kategoria, y = Wartosc), fill = "#911DA8") +
#   geom_text(data = dane, aes(x = Kategoria, y = ifelse(Wartosc > 0, Wartosc + 2, Wartosc - 2)),
#             label = abs(dane$Wartosc), vjust = ifelse(dane$Wartosc > 0, -0.5, 1.5)) +
#   labs(title = "Wykres kolumnowy z etykietami", 
#        x = "Kategoria", y = "Wartosc") +
#   theme_minimal()
# 
# wykresKolumnowyGP <- ggplot() +
#   geom_col(data = dane, aes(x = Kategoria, y = Wartosc), fill = "#911DA8") +
#   geom_text(data = dane, aes(x = Kategoria, y = ifelse(Wartosc > 0, Wartosc + 2, Wartosc - 2)),
#             label = abs(dane$Wartosc), vjust = ifelse(dane$Wartosc > 0, -0.5, 1.5)) +
#   labs(title = "Wykres kolumnowy z etykietami", 
#        x = "Kategoria", y = "Wartosc") +
#   theme_minimal()
# 
# wykresKolumnowyTurnover <- ggplot() +
#   geom_col(data = dane, aes(x = Kategoria, y = Wartosc), fill = "#911DA8") +
#   geom_text(data = dane, aes(x = Kategoria, y = ifelse(Wartosc > 0, Wartosc + 2, Wartosc - 2)),
#             label = abs(dane$Wartosc), vjust = ifelse(dane$Wartosc > 0, -0.5, 1.5)) +
#   labs(title = "Wykres kolumnowy z etykietami", 
#        x = "Kategoria", y = "Wartosc") +
#   theme_minimal()
# 


ggsave(
  filename = here::here("szkielet","WykresQuant.png"),
  device = "png",
  plot     =  WykresQuant,
  width    =  szerokoscObrazka,
  height   =  wysokoscObrazka,
  units    =  unity,
  dpi      =  piksele)

ggsave(
  filename = here::here("szkielet","WykresMargin.png"),
  device   = "png",
  plot     =  WykresMargin,
  width    =  szerokoscObrazka,
  height   =  wysokoscObrazka,
  units    =  unity,
  dpi      =  piksele)

ggsave(
  filename = here::here("szkielet","WykresTurnov.png"),
  device   = "png",
  plot     =  WykresTurnov,
  width    =  szerokoscObrazka,
  height   =  wysokoscObrazka,
  units    =  unity,
  dpi      =  piksele)

ggsave(
  filename = here::here("szkielet","WykresRazem.png"),
  plot     =  WykresZZ,
  width    =  szerokoscObrazka,
  height   =  wysokoscObrazka*3,
  units    =  unity,
  dpi      =  piksele)

# 
# WykresQuant <- 
#   ggplot(data=bazaQuant,
#          aes(x=as.POSIXct(DataAnalizy), y=Wartosc/1000, colour=NazwaWskaznika)) +
#   geom_line(linewidth=gruboscLini)+
#   geom_point(size=geompojtsize)+
#   scale_y_continuous(limits = c(min(bazaQuant$Wartosc/1000), max(bazaQuant$Wartosc/1000)+500/1000), expand = c(RozszerzGoreOsi, RozszerzDolOsi)) +
#   
#   scale_x_datetime(
#     date_labels = "%b", 
#     expand = c(0.05, 0.05),
#     date_breaks = "1 month",
#     date_minor_breaks = "1 week",  
#     limits = c(as.POSIXct(ymd(min(bazaQuant$DataAnalizy)) - period("1 week")), 
#                as.POSIXct(ymd(max(bazaQuant$DataAnalizy)) + period("1 week"))))+
#   labs(title="CEE QUANTITY",
#        x = EtykietaOsiX, 
#        y = EtykietaIlosci,
#        colour="")+
#   scale_color_manual(values=c('Black','#911DA8'))+
#   theme_minimal(base_size = bejzsize)+## output not shown, it's equivalent to the below graph (with a tiny difference in the legend title)
#   geom_text(aes(label=round(Wartosc/1000,1)),hjust=0.9, vjust=-1.2,size=etykietysize,color="black")+
#   theme(axis.title.x = element_text(margin = margin(t = mardzin)))+
#   theme(axis.title.y = element_text(margin = margin(r = mardzin)))+
#   theme(legend.key.size = unit(ledzendkejsize, "cm"))+
#   theme(legend.position = c(ledzendposx, ledzendposy), plot.title = element_text(face="bold"), axis.title = element_text(face="bold"), plot.margin = unit(c(gora, dol, 0.00, 0.00), "cm"))
# 
























