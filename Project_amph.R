#Biometria e Statistica Modulo 2 
#Progetto di Luisa Costantini, Eleonora Pancaldi e Riccardo Biagini 

#Amphibians Data Set: è una raccolta di informazioni sulla dimensione della popolazione di anfibi
#da inventari naturalistici fatti appositamente per la redazione di VIA (Valutazione Impatto Ambientale)
#per due progetti stradali pianificati in Polonia (strada A e strada B) con 189 siti di occorenza. 
#abbiamo scelto questo dataset in quanto ci interesserebbe capire la correlazione tra ecc ecc 

library(ggplot2)
library(GGally)
library(tidyverse)
library(readxl)

#1 DATASET----
#In questo dataset abbiamo 190 righe (osservazioni) e 23 colonne (variabili)
#Inoltre abbiamo sia componenti biotiche (vegetazione e specie di anfibi) che abiotiche(numero di serbatoi, riserve idriche ecc...)

#Import the working directory
setwd("C:/Progetto anfibi")

Dataset_amph <- read_xlsx("C:/data/amphibians.xlsx")


#Look at their structure
view(Dataset_amph)

dim(Dataset_amph)

head(Dataset_amph)

tail(Dataset_amph)

str(Dataset_amph)

names(Dataset_amph)


glimpse(Dataset_amph)


#2 DATA CLEANING----
#Abbiamo elimianto le colonne non utili al nostro tipo di studio, in seguito abbiamo rinominato
#delle colonne. 
#Vedendo poi che la riga "ID" era uguale ai nomi delle varie colonne abbiamo deciso di eliminarla.
New_Dataset_amph <- Dataset_amph[,c(-6, -7, -8, -9, -11, -12)]
View(New_Dataset_amph)

New_Dataset_amph_cleaned <- New_Dataset_amph %>% 
  select(Cate= 'Motorway' ,
         Categorical...5 = 'Tipo_di_serbatoio_idrico')


colnames(New_Dataset_amph) <- c("ID",
                                "motorway",
                                "estensione_serb_mq",
                                "mumero_serb/habitat",
                                "tipo_serbatoio",
                                "utilizzo_riserve_idriche",
                                "dist_serb_strade",
                                "dist_serb_edifici",
                                "stato_serb",
                                "tipolo_riva",
                                "green_frogs",
                                "brown_frogs",
                                "common_toad",
                                "fire-bellied_toad",
                                "tree_frog",
                                "common_newt",
                                "great_crested_newt")

New_Dataset_amph <- New_Dataset_amph[-1,]

head(New_Dataset_amph)

view(New_Dataset_amph)

#3 DATA ANALYSIS----
head(New_Dataset_amph)
tail(New_Dataset_amph)

#Ora vedremo qual è la specie con occorenza maggiore e quella con occorenza minore
table(New_Dataset_amph$`green_frogs`)
table(New_Dataset_amph$`brown_frogs`)
table(New_Dataset_amph$`common_toad`)
table(New_Dataset_amph$`fire-bellied_toad`)
table(New_Dataset_amph$`tree_frog`)
table(New_Dataset_amph$`common_newt`)
table(New_Dataset_amph$`great_crested_newt`)
#La più frequente è "Brown Frog" e la meno frequente "Great crested newt" 

# Creazione del database con tutte le osservazioni in cui Brown frogs sono presenti
bfd <- New_Dataset_amph %>% filter(brown_frogs == '1')
view(bfd)

# Creazione del database con tutte le osservazioni in cui Great crested newt sono presenti
gcnd <- New_Dataset_amph %>% filter(great_crested_newt == '1')
view(gcnd)


#Durante lo studio verranno esaminati gli stessi parametri sia per la specie
#"Brown frogs" che per quella "Great crested newt". Per praticità verranno
#prima svolte tutte le analisi per la prima specie, e solo successivamente per la seconda.


#statistiche di base di bfd
bfd %>%
  summarise(mean = mean(estensione_serb_mq),
            sd = sd(estensione_serb_mq),
            min = min(estensione_serb_mq),
            max = max(estensione_serb_mq))


gcnd %>%
  summarise(mean = mean(estensione_serb_mq),
            sd = sd(estensione_serb_mq),
            min = min(estensione_serb_mq),
            max = max(estensione_serb_mq))


            
