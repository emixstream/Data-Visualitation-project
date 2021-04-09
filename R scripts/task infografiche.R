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
  estremo_sup <- p + 1.96 * sqrt((p * (1 - p)) / 52)
  estremo_inf <- p - 1.96 * sqrt((p * (1 - p)) / 52)
  return(list(estremo_sup, estremo_inf))
}

risultati <- read.csv2("C:\\Users\\emanu\\OneDrive\\Desktop\\R scripts\\Task infografiche.csv")



### Data frame per l'intervallo di confidenza
Infografica <- c(1, 1, 2, 2, 3, 3, 4, 4)
upper <- c(0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06)
lower <- c(0.02, 0.02, 0, 0, 0, 0,  0, 0)
dati_err <- data.frame(Infografica, upper, lower, risultati$ï..Prop, 
                       risultati$Successo)

names(dati_err) <- c("Infografica", "upper", "lower", "Prop", "Successo")

## Stacked barolot
prop = c(0.80, 0.20, 0.80, 0.20, 0.80, 0.20, 0.80, 0.20)

ggplot(risultati, aes(x = Infografica, y = prop, fill = Successo)) +
  geom_bar(position = position_stack(reverse = TRUE), 
           stat = "identity") + coord_flip() + 
  geom_errorbar(data = dati_err, aes(ymin = lower, ymax = upper), width = 0.15) +
  xlab(label = "Infografica") + ylab(label = "percentuale successo") +
  scale_fill_manual(values = c("blue", "red")) +
  guides(fill=guide_legend(title = " ")) + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.25),
                     sec.axis = sec_axis(~ 1 - .,)) +
  geom_hline(yintercept = 0.10, linetype = "dotted", size = 1) +
  theme(axis.text.x.bottom = element_text(color = "blue", size = 12)) +
  theme(axis.text.x.top = element_text(color = "red", size = 12)) +
  theme(axis.text.y = element_text(face = "bold", size = 10)) + 
  labs(title = "Analisi successo/insuccesso del completamento dei tasks")








                     