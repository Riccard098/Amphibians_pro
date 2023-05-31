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

Dataset_amph <- read_xlsx("data/amphibians.xlsx")


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


colnames(New_Dataset_amph) <- c("ID",
                                "Motorway",
                                "Superficie_serbatoio_idrico_(m2)",
                                "Numeri_di_serbatoi/habitat",
                                "Tipo_di_Serbatoio",
                                "Utilizzo_riserve_idriche",
                                "Distanza_serbatoio-strade",
                                "Distanza_serbatoio-edifici",
                                "Stato_serbatoio",
                                "Tipologia_riva",
                                "Green_frogs",
                                "Brown_frogs",
                                "Common_toad",
                                "Fire-bellied_toad",
                                "Tree_frog",
                                "Common_newt",
                                "Great_crested_newt")

New_Dataset_amph <- New_Dataset_amph[-1,]

head(New_Dataset_amph)

view(New_Dataset_amph)

#Abbiamo convertito le variabili da dati qualitativi a quantitativi, in questo caso avevamo bisogno dei range in metri
#delle strade 
New_Dataset_amph["Distanza_serbatoio-strade"][New_Dataset_amph["Distanza_serbatoio-strade"] == 0] <- "<50 m"
New_Dataset_amph["Distanza_serbatoio-strade"][New_Dataset_amph["Distanza_serbatoio-strade"] == 1] <- "50-100 m"
New_Dataset_amph["Distanza_serbatoio-strade"][New_Dataset_amph["Distanza_serbatoio-strade"] == 2] <- "100-200 m"
New_Dataset_amph["Distanza_serbatoio-strade"][New_Dataset_amph["Distanza_serbatoio-strade"] == 5] <- "200-500 m"
New_Dataset_amph["Distanza_serbatoio-strade"][New_Dataset_amph["Distanza_serbatoio-strade"] == 9] <- "500-1000 m"
New_Dataset_amph["Distanza_serbatoio-strade"][New_Dataset_amph["Distanza_serbatoio-strade"] == 10] <- ">1000 m"

glimpse(New_Dataset_amph)
view(New_Dataset_amph)


#La colonna della superficie del serbatoio idrico ora non è più un character ma è stata modificata in numeric  
New_Dataset_amph <- New_Dataset_amph %>%
  mutate(
    `Superficie_serbatoio_idrico_(m2)` = as.numeric(`Superficie_serbatoio_idrico_(m2)`)
  )
glimpse(New_Dataset_amph)



#3 DATA ANALYSIS----
head(New_Dataset_amph)
tail(New_Dataset_amph)

#Ora vedremo qual è la specie con occorenza maggiore e quella con occorenza minore
table(New_Dataset_amph$`Green frogs`)
table(New_Dataset_amph$`Brown frogs`)
table(New_Dataset_amph$`Common toad`)
table(New_Dataset_amph$`Fire-bellied toad`)
table(New_Dataset_amph$`Tree frog`)
table(New_Dataset_amph$`Common newt`)
table(New_Dataset_amph$`Great crested newt`)
#La più frequente è "Brown Frog" e la meno frequente "Great crested newt" 


