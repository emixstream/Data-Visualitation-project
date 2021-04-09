# Prima infografica

set.seed(1112344789)
library(ggplot2)
library(LabRS)
library(magrittr)
library(tidyverse)
library(varhandle)
library(ggpubr)
library(corrplot)

setwd("C:/Users/39331/Desktop")
IC_PROP <- function(p) {
  estremo_sup <- p + 1.96 * sqrt((p * (1 - p)) / 26)
  estremo_inf <- p - 1.96 * sqrt((p * (1 - p)) / 26)
  return(list(estremo_sup, estremo_inf))
}

risultati <- read.csv2("TERZA VIZ.csv")

n_utile <- risultati$Utile
n_intuitiva <- risultati$Intuitiva
n_chiara <- risultati$Chiara
n_informativa <- risultati$Informativa
n_bella <- risultati$Bella
n_valutazione_complessiva <- risultati$Valore_Complessivo

utile <- cbind(n_utile, rep("utile", 26))
intuitiva <- cbind(n_intuitiva, rep("intuitiva", 26))
chiara <- cbind(n_chiara, rep("chiara", 26))
informativa <- cbind(n_informativa, rep("informativa", 26))
bella <- cbind(n_bella, rep("bella", 26))
valutazione_complessiva <- cbind(n_valutazione_complessiva, 
                                 rep("valutazione_complessiva", 26))


dati <- data.frame(rbind(bella, informativa, chiara, intuitiva, utile,
                         valutazione_complessiva))
colnames(dati) <- c("Valore", "Variabile")


# Funzione per raggruppare i valori
ragrupp <- function(x) {
  if (x >= 1 & x <= 3) {
    return("1-3")
  }
  if (x >= 4 & x <= 6) {
    return("4-6")
  }
}

j = 1
condizione <- c()
for(i in dati$Valore) {
  if(i <= 3) {
    condizione[j] <- "1-3"
  }
  else {
    condizione[j] <- "4-6"
  }
  j = j + 1
}

dati <- cbind(dati, condizione)
head(dati)

#### Calcolo delle proporzioni

t <- round(table(dati$condizione, dati$Variabile) / 26, 2)
variabile <- c("Bella", "Bella", "Informativa", "Informativa", 
               "Chiara", "Chiara", "Intuitiva", "Intuitiva",
               "Utile", "Utile", "Valutazione_complessiva", 
               "Valutazione_complessiva")
condizione <- c("1-3", "4-6", "1-3", "4-6", "1-3", "4-6", "1-3", "4-6",
                "1-3", "4-6", "1-3", "4-6")
prop <- c(t[1:2], t[3:4], t[5:6], t[7:8], t[9:10], t[11:12])

dati_prop <- data.frame(cbind(variabile, condizione, 
                              as.numeric(prop)))
colnames(dati_prop) <- c("variabile", "condizione", "prop")
dati_prop$prop <- unfactor(dati_prop$prop) #da factor a numeric

### Data frame per l'intervallo di confidenza

upper <- c(0.39, 0.39, 0.44, 0.44, 0.53, 0.53, 0.34, 0.34, 0.39, 0.39, 0.34, 0.34)
lower <- c(0.07, 0.07, 0.10, 0.10, 0.17, 0.17, 0.04, 0.04, 0.07, 0.07, 0.04, 0.04)
dati_err <- data.frame(variabile, upper, lower)


## Stacked barolot

ggplot(dati_prop, aes(x = variabile, y =prop, fill = condizione)) +
  geom_bar(position = position_stack(reverse = TRUE), 
           stat = "identity") + coord_flip() + 
  geom_errorbar(data = dati_err, aes(ymin = lower, ymax = upper), 
                width = 0.15) +
  xlab(label = "voci") + ylab(label = "percentuale di voti") +
  scale_fill_manual(values = c("blue", "red")) +
  guides(fill=guide_legend(title = " ")) + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.25),
                     sec.axis = sec_axis(~ 1 - .,)) +
  geom_hline(yintercept = 0.5, linetype = "dotted", size = 1) +
  theme(axis.text.x.bottom = element_text(color = "blue", size = 12)) +
  theme(axis.text.x.top = element_text(color = "red", size = 12)) +
  theme(axis.text.y = element_text(face = "bold", size = 10))



#### Correlogramma

dati_b <- data.frame(cbind(n_utile, n_intuitiva, n_chiara,
                           n_informativa, n_bella, 
                           n_valutazione_complessiva))
colnames(dati_b) <- c("Utile", "Intuitiva", "Chiara", 
                      "Informativa", "Bella", "Valutazione_complessiva")
corrplot(cor(dati_b), method = "ellipse", type = "lower")




                     