# Deklaracja sciezki do bilbiotek
options(stringsAsFactors = FALSE)

options("scipen"=100, "digits"=4)


##########################################################################
#### upewnienie siê ¿e nie ma ¿adnych pakietów za³adowanych ####
gc(reset = TRUE)
#od³¹czeni wszytkich pakietów - stowrzebnuie funkcji
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

detachAllPackages() #wywo³anie funkcji 


##########################################################################
#### za³adowanie pakietów ####

library(dplyr)
library(ggplot2)
library(readxl) #do czytania excela
library(tidyr) #do geather
#install.packages("xlsx") #do czytania excela
library(xlsx) #do czytania excela
#library(rgdal) # do wczytania shapefile do granic osiedli

#library(reshape2)
#library(gridExtra)
#library(grid)
#library(lubridate) # do numeru tygodnia
library(stringr) # do funkcji pad

#library(ggmap) #do zbierania wspó³rzednych
#library(sp)  #do zbierania wspó³rzednych
library(extrafont) #do czcionek
library(scales) #do osi Y jako %
##########################################################################
#### wczytanie danych ####
#Dane stare 
load("D:/KK/OneDrive/Wroclaw w Liczbach/Gotowe projekty/stra¿ miejska/Dane RData/1 Dane nt mandatów.RData")
Dane_Stare <- Mandaty

path <- "D:\\KK\\OneDrive\\Wroclaw w Liczbach\\Gotowe projekty\\stra¿ miejska"
setwd(path)
#dane nt Mandatów nowych
Data <- data.frame()
miesiace<- c(paste0(rep(2017, 12), c(paste0(0, 1:9), 10:12)),
             paste0(rep(2018, 5),  c(paste0(0, 1:5))))

for (i in miesiace){
  tmp <- read_excel(paste0(path, "\\Dane po wniosku\\", i, ".xlsx"), sheet = 1, col_names = TRUE, na = "", skip = 2) %>%
    filter(!is.na(Lp.))
  tmp <- tmp[, c("Typ zdarzenia", "Iloœæ")]  %>%
    mutate(miesiac = i,
           data = paste0(i, "01"),
           data = as.Date(data, format = "%Y%m%d")) 
  
  Data <- rbind(Data, tmp)
  print(i)
}

names(Data) <- c("Typ", "Ilosc", "Miesiac", "data")

##########################################################################
#### porównanie danych do otrzymanych wczeœniej ####
Ilosc_2017_nowe <- Data %>%
  filter(data < '2018-01-01') 
Ilosc_2017_nowe <-  sum(Ilosc_2017_nowe$Ilosc, na.rm = T)

Ilosc_2017_stare <- Dane_Stare %>%
  filter(Rok == 2017) %>% nrow()

Ilosc_2017_nowe / 365
Ilosc_2017_stare / 365






##########################################################################
#### poprawienie kategorii ####

Typy <- Data %>%
  group_by(Typ) %>%
  summarise(ilosci = sum(Ilosc, na.rm = NA),
            prc = ilosci / sum(Data$Ilosc, na.rm = T)) %>%
  arrange(desc(ilosci))
  

Dane_kat <- Data %>%
  mutate(Typ = case_when(.$Typ == "NIEPRAWID£OWE PARKOWANIE" ~ "Nieprawid³owe parkowanie",
                         .$Typ == "PORZ¥DKOWA" ~ "Inne",
                         .$Typ == "SPALANIE" ~ "Ochrona œrodowiska",
                         .$Typ == "HANDEL" ~ "Inne",
                         .$Typ == "BEZDOMNY" ~ "Inne",
                         .$Typ == "BLOKADA" ~ "Nieprawid³owe parkowanie",
                         .$Typ == "NISZCZENIE ZIELENI" ~ "Ochrona œrodowiska",
                         .$Typ == "WRAK POJAZDU" ~ "Ochrona œrodowiska",
                         .$Typ == "NIETRZEWA OSOBA" ~ "Inne",
                         .$Typ == "LE¯¥CA OSOBA" ~ "Inne",
                         .$Typ == "BEZPIECZEÑSTWO W KOMUNIKACJI" ~ "Inne",
                         .$Typ == "SPO¯YWANIE ALKOHOLU" ~ "Inne",
                         .$Typ == "PIES" ~ "Inne",
                         .$Typ == "ZAK£ÓCANIE £ADU I PORZ¥DKU" ~ "Inne",
                         .$Typ == "PLAKATY (REKLAMY)" ~ "Inne",
                         .$Typ == "PIEC" ~ "Ochrona œrodowiska",
                         .$Typ == "WYPALANIE, OGNISKO" ~ "Inne",
                         .$Typ == "ZAK£ÓCANIE CISZY NOCNEJ" ~ "Inne",
                         .$Typ == "Zablokowany wyjazd" ~ "Nieprawid³owe parkowanie",
                         .$Typ == "WYCINKA DRZEW" ~ "Inne",
                         .$Typ == "ZAGRO¯ENIE ZDROWIA, ¯YCIA" ~ "Inne",
                         .$Typ == "OSOBA LE¯¥CA" ~ "Inne",
                         .$Typ == "¯EBRZ¥CY" ~ "Inne",
                         .$Typ == "¯EBRANIE" ~ "Inne",
                         .$Typ == "DZIKIE ZWIERZÊTA" ~ "Inne",
                         .$Typ == "WYKROCZENIA PIESZYCH" ~ "Inne",
                         .$Typ == "LAWETY" ~ "Nieprawid³owe parkowanie",
                         .$Typ == "NISZCZENIE MIENIA" ~ "Inne",
                         .$Typ == "ZDARZENIE DROGOWE" ~ "Inne",
                         .$Typ == "ALARM" ~ "Inne",
                         .$Typ == "asysta" ~ "Inne",
                         .$Typ == "Barszcz Sosnowskiego" ~ "Inne",
                         .$Typ == "PO¯AR" ~ "Inne",
                         .$Typ == "ZWIERZÊTA HODOWLANE" ~ "Inne",
                         .$Typ == "KRADZIE¯" ~ "Inne",
                         .$Typ == "KONTROLA OCHRONA SRODOWISKA" ~ "Ochrona œrodowiska",
                         .$Typ == "OGNISKO" ~ "Inne",
                         .$Typ == "KOMUNIKAT" ~ "Inne",
                         .$Typ == "POBICIE" ~ "Inne",
                         .$Typ == "BÓJKA" ~ "Inne",
                         .$Typ == "NARKOMAN" ~ "Inne",
                         .$Typ == "NIELEGALNY PO£ÓW RYB" ~ "Inne",
                         .$Typ == "£OWIENIE RYB" ~ "Inne",
                         .$Typ == "SK£ADOWANIE ZANIECZYSZCZEÑ" ~ "Ochrona œrodowiska",
                         .$Typ == "KWESTUJ¥CY" ~ "Inne",
                         .$Typ == "NIEWSKAZANIE KIEURJ¥CEGO" ~ "Inne",
                         .$Typ == "SKARGA" ~ "Inne",
                         .$Typ == "MATERIA£Y NIEBEZPIECZNE" ~ "Inne",
                         .$Typ == "UTRUDNIENIE RUCHU MPK" ~ "Nieprawid³owe parkowanie",
                         .$Typ == "ZAWIADOMIENIE KOMUNIKACYJNE" ~ "Inne",
                         .$Typ == "NAPAD" ~ "Inne",
                         .$Typ == "NIEWYBUCH (NIEWYPA£)" ~ "Inne",
                         .$Typ == "OWADY" ~ "Inne",
                         .$Typ == "ZGON" ~ "Inne",
                         .$Typ == "NISZCZENIE URZ¥DZEÑPRZECIWPOWODZIOWYCH" ~ "Inne",
                         .$Typ == "WSPÓ£PRACA Z KONTROLERAMI MPK" ~ "Inne",
                         .$Typ == "PALENIE TYTONIU W MIEJSCACH ZABRONIONYCH" ~ "Inne",
                         T ~ "co to? coœ innego niz zaplanowa³em")) %>%
  filter(!is.na(Ilosc))





Typy_kat <- Dane_kat %>%
  group_by(Typ) %>%
  summarise(ilosci = sum(Ilosc, na.rm = NA),
            prc = ilosci / sum(Dane_kat$Ilosc, na.rm = T)) %>%
  arrange(desc(ilosci))


##########################################################################
#### stworzenie wykresów ####

Ilosc_mandaty_typ <- Dane_kat %>%
  mutate(Typ = as.character(Typ),
         Typ =  case_when(Typ == "Nieprawid³owe parkowanie" ~ "Nieprawid³owe parkowanie" ,
                          Typ == "Ochrona œrodowiska" ~ "Ochrona œrodowiska" ,
                          T ~ "Inne"),
         Typ = factor(Typ, levels = c(
           "Inne",
           "Ochrona œrodowiska", 
           "Nieprawid³owe parkowanie"
         )))  %>%
  group_by(Typ, Miesiac, data) %>%
  summarise(Ilosc_interwencji = sum(Ilosc, na.rm = T)) %>% 
  ungroup()

Breakes <- as.Date(c("2017-01-01", "2017-06-01", "2018-01-01"))

# wielkoœæ obrazka
a <- 9


##########################################################################
#### Ustalenie spójnego stylu dla wykresów ####
Theme <-  theme(legend.position="bottom",
                legend.key.width = unit(1,"cm"),
                legend.title = element_blank(),
                axis.text    = element_text(family = "Ubuntu", size = 12, color = "#22211d"),
                axis.title   = element_text(family = "Ubuntu", size = 14, color = "#22211d"),
                text = element_text(family = "Ubuntu", color = "#22211d"),
                panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
                plot.background  = element_rect(fill = "#f5f5f2",  color = NA), 
                panel.background = element_rect(fill = "#f5f5f2",  color = NA), 
                legend.background = element_rect(fill = "#f5f5f2", color = NA),
                legend.text       = element_text(family = "Ubuntu", size = 13, hjust = 0, color = "#22211d"),
                plot.title    = element_text(family = "Ubuntu", size = 21,  hjust = 0.5,  color = "#4e4d47"),
                plot.subtitle = element_text(family = "Ubuntu", size = 13,  hjust = 0.01,  face = "italic", color = "#4e4d47"),
                plot.caption  = element_text(family = "Ubuntu", size = 11,  hjust = 0.99, color = "#4e4d47"),
                panel.border = element_blank()
)  

##########################################################################
#### Wykres 1 - ilosæ zg³oszeñ w czasie ####
cols <- c("Nieprawid³owe parkowanie" = "#08519C", 
          "Inne" = "#800000", 
          "Ochrona œrodowiska" = "#ffa400")

w1 <- ggplot(Ilosc_mandaty_typ, aes(x = data, y = Ilosc_interwencji,  col = Typ, group = interaction(Typ))) +
  geom_line( ) +
  #coord_cartesian(xlim = c(as.Date("2017-07-01"), as.Date("2018-02-01"))) + 
  #geom_smooth(method = "lm", se = F, linetype = 2, size = 0.5, alpha = 0.1, show_guide = FALSE) + 
  geom_point(aes(shape = Typ), size = 2) +
  scale_colour_manual(values = cols) +  
  scale_shape_manual(values = c(16, 18,  15)) + 
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y", 
               date_minor_breaks = "1 month") + 
  labs(title = "Interwencje Stra¿y Miejskiej we Wroc³awiu",
       subtitle = "Suma interwencji w miesiacu",
       x = "",
       y = "",
       caption = "Autor: WroData | ród³o: Stra¿ Miejska Wroc³aw udostêpnione dnia 8 lipca 2018 roku dla WroData
       Stra¿ Miejska Wroc³aw nie jest nie ponosi odpowiedzialnoœci za przetworzenie, dalsze udostêpnianie i wykorzystanie przes³anych danych" ) 



w1 <- w1 + Theme 
# + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(w1)


png(filename = paste0(path, "\\Wykresy nowe\\", Sys.Date()," w1" , ".png", sep=""),
    bg="white", width = a * 1.161803, height = a, units = 'in', res = 150)
      plot(w1)
dev.off()

save(w1, file = paste0(path, "\\Wykresy nowe\\", Sys.Date()," w1" , ".RData", sep=""))

##########################################################################
#### Wykres 5 - udzia³ kategorii w czasie ####
Ilosc_mandaty_typ <- within(Ilosc_mandaty_typ, 
                   Typ <- factor(Typ, 
                                 levels = c(
                                   "Nieprawid³owe parkowanie",
                                   "Ochrona œrodowiska",
                                   "Inne"
                                 )))

levels(Ilosc_mandaty_typ$Typ)

opisy <- Ilosc_mandaty_typ %>%
  group_by(data) %>%
  mutate(t   = sum(Ilosc_interwencji),
         prc = Ilosc_interwencji / t,
         prc_lag  = lag(prc),
         prc_lag2 = lag(prc_lag)) %>%
  ungroup() %>%
  mutate(y = case_when(.$Typ == "Nieprawid³owe parkowanie" ~ prc_lag + prc_lag2 + prc / 2,
                       .$Typ == "Ochrona œrodowiska" ~  prc_lag +  prc / 2,
                       .$Typ == "Inne" ~ prc / 2,
                       T ~ 0),
         label = paste0(round(prc * 100, 0), "%")) 




w5 <- ggplot(Ilosc_mandaty_typ, aes(x = data, y = Ilosc_interwencji, fill = Typ )) +
  geom_bar(position = "fill",stat = "identity") +
  scale_fill_manual(values = cols) +  
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y", 
               date_minor_breaks = "1 month") +
  scale_y_continuous(labels = percent, expand = c(0, 0)) +
  # geom_rect(data = opisy, aes(xmin = data - 15, xmax = data + 15, 
  #                                 ymin = y - 0.02, ymax = y + 0.02), fill = "black") +
  geom_text(data = opisy, aes(x = data, y = y, label = label), 
            vjust = 0.5, hjust = 0.5, alpha = 1, 
            angle = 90, show.legend = F, col = "white") + 
#   geom_label_repel(aes(label = paste0(percent(Ilosc_interwencji / sum(Ilosc_interwencji)))),
#                    position = position_stack(vjust = 0.5),
#                    alpha = 0.8, 
#                    label.padding=.1,
#                    seed = 1234, 
#                    family  = "Ubuntu", size = 3.5, color = "#22211d") +
  labs(title = "Interwencje Stra¿y Miejskiej we Wroc³awiu",
       subtitle = "Udzia³ procentowy kategorii podjêtych interwencji",
       x = "",
       y = "",
       caption = "Autor: WroData | ród³o: Stra¿ Miejska Wroc³aw udostêpnione dnia 8 lipca 2018 roku dla WroData
       Stra¿ Miejska Wroc³aw nie jest nie ponosi odpowiedzialnoœci za przetworzenie, dalsze udostêpnianie i wykorzystanie przes³anych danych" ) 


w5 <- w5 + Theme 
# + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(w5)

png(filename = paste(path, "\\Wykresy nowe\\", Sys.Date()," w5" , ".png", sep=""),
    bg="white", width = a * 1.161803, height = a, units = 'in', res = 150)
plot(w5)
dev.off()

save(w5, file = paste(path, "\\Wykresy nowe\\", Sys.Date()," w5" , ".RData", sep=""))


