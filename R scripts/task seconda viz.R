# Prima infografica


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

dati <- read.csv("task seconda viz.csv", sep = ";")
colnames(dati) <- c("Task", "Tempo", "Successo")

ggplot(dati, aes(x = Task, y = Tempo)) + 
  geom_violin(trim = FALSE, fill = "darkgray", color="darkred") +
  geom_jitter(shape=16, position=position_jitter(0.1),aes(color = Successo), 
              size = 3) + ylab("secondi") +
  theme_minimal() + geom_boxplot(width=0.01, outlier.colour = "#00BFC4")



