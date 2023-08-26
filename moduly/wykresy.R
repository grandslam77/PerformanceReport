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



EtykietaIlosci <- "QUANTITY IN MT"
EtykietaMarz   <- "AMOUNT (in mio EUR)"
EtykietaKwot   <- "EUR"


bazaQuant     <- bazaPerformance[bazaPerformance$NazwaWskaznika %in% c("Quant","QuantPRV") ,]
bazaQuant     <- bazaQuant[bazaQuant$NumerMiesiaca<=MiesiacAnalizy,]
bazaQuant     <- bazaQuant[,c(2,4,5)]
# 
# bazaQuantYTD  <- bazaPerformance[bazaPerformance$NazwaWskaznika %in% c("Quant_YTD","Quant_YTD_PRV"),]
# bazaQuantYTD  <- bazaQuantYTD[bazaQuantYTD$NumerMiesiaca<=MiesiacAnalizy,]
# bazaQuantYTD  <- bazaQuantYTD[,c(2,4,5)]

bazaMargin    <- bazaPerformance[bazaPerformance$NazwaWskaznika %in% c("GrossProf","GrossProf_PRV"),]
bazaMargin    <- bazaMargin[bazaMargin$NumerMiesiaca<=MiesiacAnalizy,]
bazaMargin    <- bazaMargin[,c(2,4,5)]
# 
# bazaMarginYTD <- bazaPerformance[bazaPerformance$NazwaWskaznika %in% c("GrossProf_YTD","GrossProf_PRV_YTD"),]
# bazaMarginYTD <- bazaMarginYTD[bazaMarginYTD$NumerMiesiaca<=MiesiacAnalizy,]
# bazaMarginYTD <- bazaMarginYTD[,c(2,4,5)]

bazaTurn      <- bazaPerformance[bazaPerformance$NazwaWskaznika %in% c("Turnov","Turnov_PRV"),]
bazaTurn      <- bazaTurn[bazaTurn$NumerMiesiaca<=MiesiacAnalizy,]
bazaTurn      <- bazaTurn[,c(2,4,5)]
  
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
  aes(x=as.POSIXct(DataAnalizy), y=Wartosc/1000, colour=NazwaWskaznika)) +
  geom_line(linewidth=gruboscLini)+
  geom_point(size=geompojtsize)+
  scale_y_continuous(limits = c(min(bazaQuant$Wartosc/1000), max(bazaQuant$Wartosc/1000)+500/1000), expand = c(RozszerzGoreOsi, RozszerzDolOsi)) +
  
  scale_x_datetime(limits = c(as.POSIXct(ymd(min(bazaQuant$DataAnalizy)) %m-% period("1 week")),as.POSIXct(ymd(max(bazaQuant$DataAnalizy)) %m+% period("1 week"))),date_labels = "%b",expand = c(0.05, 0.05),date_breaks = "1 month")+
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
  theme(legend.position = c(ledzendposx, ledzendposy), plot.title = element_text(face="bold"), axis.title = element_text(face="bold"), plot.margin = unit(c(gora, dol, 0.00, 0.00), "cm"))

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
         aes(x=as.POSIXct(DataAnalizy), y=Wartosc/1000, colour=NazwaWskaznika)) +
  geom_line(linewidth=gruboscLini)+
  geom_point(size=geompojtsize)+
  scale_y_continuous(limits = c(min(bazaMargin$Wartosc/1000), max(bazaMargin$Wartosc/1000)+500/1000), expand = c(RozszerzGoreOsi, RozszerzDolOsi)) +
  scale_color_manual(values=c('Black','#911DA8'))+
  scale_x_datetime(limits = c(as.POSIXct(ymd(min(bazaMargin$DataAnalizy)) %m-% period("1 week")),as.POSIXct(ymd(max(bazaMargin$DataAnalizy)) %m+% period("1 week"))),date_labels = "%b",expand = c(0.01, 0.01),date_breaks = "1 month")+
  labs(title="CEE MARGIN",
       x = EtykietaOsiX, 
       y = EtykietaMarz,
       colour="")+
  
  theme_minimal(base_size = bejzsize)+## output not shown, it's equivalent to the below graph (with a tiny difference in the legend title)
  geom_text(aes(label=round(Wartosc/1000,1)),hjust=0.9, vjust=-1.2,size=etykietysize,color="black")+
  theme(axis.title.x = element_text(margin = margin(t = mardzin)))+
  theme(axis.title.y = element_text(margin = margin(r = mardzin)))+
  theme(legend.key.size = unit(ledzendkejsize, "cm"))+
  theme(legend.position = c(ledzendposx, ledzendposy), plot.title = element_text(face="bold"), axis.title = element_text(face="bold"),plot.margin = unit(c(gora, dol, 0, 0), "cm"))



# WYKRES Turnover     WykresTurnov
########################################################################################################


bazaTurn$NazwaWskaznika[bazaTurn$NazwaWskaznika=="Turnov"]<-as.character(year(dataAnalizy))
bazaTurn$NazwaWskaznika[bazaTurn$NazwaWskaznika=="Turnov_PRV"]<-as.character(year(dataAnalizy)-1)

WykresTurnov <- 
  ggplot(data=bazaTurn,
         aes(x=as.POSIXct(DataAnalizy), y=Wartosc/1000, colour=NazwaWskaznika)) +
  geom_line(linewidth=gruboscLini)+
  geom_point(size=geompojtsize)+
  scale_y_continuous(limits = c(min(bazaTurn$Wartosc/1000), max(bazaTurn$Wartosc/1000)+500/1000), expand = c(RozszerzGoreOsi, RozszerzDolOsi)) +
  
  scale_x_datetime(limits = c(as.POSIXct(ymd(min(bazaTurn$DataAnalizy)%m-% period("7 days"))),as.POSIXct(ymd(max(bazaTurn$DataAnalizy)%m+% period("1 week")))),date_labels = "%b",date_breaks = "1 month")+
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
  theme(legend.position = c(ledzendposx, ledzendposy), plot.title = element_text(face="bold"),axis.title = element_text(face="bold"),plot.margin = unit(c(gora, 1, 0, 0), "cm"))


WykresZZ<-(WykresQuant/WykresMargin/WykresTurnov)# +theme(plot.margin = unit(c(-1, -1, 0, 0), "cm"))





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


