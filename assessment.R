library(tidyverse)
library(readxl)
library(patchwork)
library(stringr)
library(janitor)
#https://gotellilab.github.io/GotelliLabMeetingHacks/NickGotelli/ggplotPatchwork.html
#https://stackoverflow.com/questions/22945651/remove-space-between-plotted-data-and-the-axes#https://stackoverflow.com/questions/32345923/how-to-control-ordering-of-stacked-bar-chart-using-identity-on-ggplot2
#https://stackoverflow.com/questions/12075037/ggplot-legends-change-labels-order-and-title

getwd()
df <- read_excel("GeneraliDaSIto/Partecipazione.xlsx")

#PARTECIPAZIONE----
##PER GENERE----
df1 <- df[,c(1, 3, 4)]

colnames(df1) <- c("stato","maschi","femmine")

df1 <- pivot_longer(df1, -stato, names_to = 'sex') %>% 
  uncount(value)

df1 <- df1 %>% mutate(across(where(is_character),as_factor))

p1 <- df1 %>%
  group_by(stato, sex) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = stato, y = freq , fill = sex)) +
  geom_bar(position = "fill", stat = "identity", color = 'black', width = 0.7) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  scale_fill_manual(labels = c("Maschi", "Femmine"),
                    values = c("#5ab4ac", "#d8b365")) +
  geom_text(aes(label = paste0(round(freq, 3)*100,"%","\n(n=",n,")")), 
                position = position_stack(vjust = 0.5), size = 2.8) +
  ggtitle("Partecipazione al test (per genere, %)") +
  theme_bw() +
  theme(
        #aspect.ratio = 1.1,
        plot.title = element_text(size = 11),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_text(size=7.5),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        legend.title.align = 0.5,
        legend.title=element_blank()) +
  scale_x_discrete(labels = c("Abilitati" = "Dipendenti\nabilitati",
                              "Registrati" = "Dipendenti\nregistrati"), expand = c(0, 0)) +
  guides(fill = guide_legend(reverse = T))

###plot p1----
p1 + coord_flip()

##PER ETA----
df2 <- df[,c(1, 5, 6, 7, 8, 9)]

colnames(df2) <- c("stato","18-30","31-40", "41-50", "51-60", "61-99")

df2 <- pivot_longer(df2, -stato, names_to = 'eta') %>% 
  uncount(value)

df2 <- df2 %>% mutate(across(where(is_character),as_factor))

df2$eta <- factor(df2$eta, levels = c("61-99", "51-60", "41-50", "31-40", "18-30"))

p2 <- df2 %>%
  group_by(stato, eta) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = stato, y = freq , fill = eta)) +
  geom_bar(position = "fill", stat = "identity", color = 'black', width = 0.7) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  geom_text(aes(label = paste0(round(freq, 3)*100,"%","\n(n=",n,")")), 
                position = position_stack(vjust = 0.5), size = 2.8) +
  ggtitle("Partecipazione al test (per età, %)") +
  theme_bw() +
  theme(
        #aspect.ratio = 1.1,
        plot.title = element_text(size = 11),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_text(size=7.5),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        legend.title.align = 0.5,
        legend.title=element_blank()) +
  scale_x_discrete(labels = c("Abilitati" = "Dipendenti\nabilitati",
                              "Registrati" = "Dipendenti\nregistrati"), expand = c(0, 0)) +
  #scale_fill_brewer(palette = "Set3")
  scale_fill_manual(values = c("#5ab4ac", "#acd9d5", "#bfbaba", "#ebd9b2", "#d8b365")) +
  guides(fill = guide_legend(reverse = T))

###plot p2----
p2 + coord_flip()


##PER LIVELLO DI ISTRUZIONE----
df3 <- df[2, c(1, 10:14)]

colnames(df3) <- c("stato","Primaria","Secondaria di I grado", "Secondaria di II grado", "Universitaria", "Post-universitaria")

df3 <- pivot_longer(df3, -stato, names_to = 'istruzione') %>% 
  uncount(value)

df3 <- df3 %>% mutate(across(where(is_character),as_factor))

df3$istruzione <- factor(df3$istruzione, levels = c("Post-universitaria", "Universitaria", "Secondaria di II grado", "Secondaria di I grado"))

p3 <- df3 %>%
  group_by(stato, istruzione) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = stato, y = freq , fill = istruzione)) +
  geom_bar(position = "fill", stat = "identity", color = 'black', width = 0.5) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  geom_text(aes(label = paste0(round(freq, 3)*100,"%","\n(n=",n,")")), 
                position = position_stack(vjust = 0.5), size = 2.8) +
  ggtitle("Partecipazione dei dipendenti registrati al test (per livello di istruzione, %)") +
  theme_bw() +
  theme(
        #aspect.ratio = 1.1,
        plot.title = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        legend.title.align = 0.5,
        legend.title=element_blank()) +
  # scale_x_discrete(labels = c("Abilitati" = "Dipendenti \nabilitati",
  #                             "Registrati" = "Dipendenti \nregistrati")) +
  #scale_fill_brewer(palette = "Set3")
  scale_fill_manual(values = c("#5ab4ac", "#acd9d5", "#ebd9b2", "#d8b365")) +
  scale_x_discrete(expand = c(0, 0)) +
  guides(fill = guide_legend(reverse = T))

###plot p3----
p3 + coord_flip()


##PERCENTUALE IDONEI AL TEST----
df4 <- df[2, c(1, 15:16)]

colnames(df4) <- c("stato","Test completato","Test non completato")

df4 <- pivot_longer(df4, -stato, names_to = 'test') %>% 
  uncount(value)

df4 <- df4 %>% mutate(across(where(is_character),as_factor))

p4 <- df4 %>%
  group_by(stato, test) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = stato, y = freq , fill = test)) +
  geom_bar(position = "fill", stat = "identity", color = 'black', width = 0.5) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  geom_text(aes(label = paste0(round(freq, 3)*100,"%","\n(n=",n,")")), 
                position = position_stack(vjust = 0.5), size = 2.8) +
  ggtitle("Dipendenti registrati risultati idonei al test (%)") +
  theme_bw() +
  theme(
        #aspect.ratio = 1.1,
        plot.title = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        legend.title.align = 0.5,
        legend.title=element_blank()) +
  # scale_x_discrete(labels = c("Abilitati" = "Dipendenti \nabilitati",
  #                             "Registrati" = "Dipendenti \nregistrati")) +
  scale_fill_manual(labels = c("Test\ncompletato", "Test\nnon completato"),
                    values = c("#5ab4ac", "#d8b365")) + 
  scale_x_discrete(expand = c(0, 0)) +
  guides(fill = guide_legend(reverse = T))

###plot p4----
p4 + coord_flip()

###plot patchwork----
  p1 + coord_flip() + 
  p2 + coord_flip() + 
  p3 + coord_flip() + 
  p4 + coord_flip() + 
  plot_layout(ncol=1)

  
#RISULTATI ASSESSMENT----
  
dt <- read_excel("GeneraliDaSIto/Risultati assessment.xlsx")

dt <- dt[, c(3:7)]

colnames(dt) <- c("Competenza","Nessun livello","Livello base", "Livello intermedio", "Livello avanzato")

dt <- pivot_longer(dt, -Competenza, names_to = 'livello') %>% 
  uncount(value)

dt <- dt %>% mutate(across(where(is_character),as_factor))

dt$livello <- factor(dt$livello, levels = c("Livello avanzato", "Livello intermedio", "Livello base", "Nessun livello"))

p5 <- dt %>%
  group_by(Competenza, livello) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = Competenza, y = freq , fill = livello)) +
  geom_bar(position = "fill", stat = "identity", color = 'black', width = 0.9) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) + #expand=c(0,0),
  geom_text(aes(label = paste0(round(freq, 2)*100,"%","\n(n=",n,")")), 
                position = position_stack(vjust = 0.5), size = 2.7) +
  ggtitle("Livello di padronanza raggiunto dai 488 dipendenti risultati idonei per ciascuna delle 11 competenze.") +
  theme_bw() +
  theme(
        #aspect.ratio = 1.1,
        plot.title = element_text(size = 11),
        axis.text.y=element_text(size=7.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        legend.title.align = 0.5,
        legend.title=element_blank()) +
  # scale_x_discrete(labels = c("Abilitati" = "Dipendenti \nabilitati",
  #                             "Registrati" = "Dipendenti \nregistrati")) +
  scale_fill_manual(labels = c("Livello\navanzato", "Livello\nintermedio", "Livello\nbase", "Nessun\nlivello"),
                    values = c("#5ab4ac", "#acd9d5", "#ebd9b2", "#d8b365")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30), expand = c(0, 0)) +
  guides(fill = guide_legend(reverse = T))

###plot p5----
p5 + coord_flip()


#PUNTEGGIO DELL'AMMINISTRAZIONE (VEDI CODICI PERFORMANCE) ----
dat <- read_excel("GeneraliDaSIto/Punteggio dell'amministrazione.xlsx")

dat <- dat[, c(2:4)]

colnames(dat) <- c("Area","Punteggio amministrazione","Punteggio medio")

dat$Area <- factor(dat$Area)
dat$`Punteggio amministrazione` <- round(dat$`Punteggio amministrazione`,1)
dat$`Punteggio medio` <- round(dat$`Punteggio medio`,1)

dat1 <- dat %>% 
  pivot_longer(-Area, names_to = 'punteggio') %>%
  mutate(punteggio = factor(punteggio))

p6 <- ggplot(dat1, aes(x = value, y = Area, color = punteggio)) + 
  theme_bw() +
  ggtitle("Punteggio medio raggiunto dall'amministrazione per area di competenza") +
  theme(plot.title = element_text(size = 11),
        axis.text.y = element_text(size = 7.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom",
        legend.title.align = 0.5,
        legend.title=element_blank()
        ) +
  geom_segment(aes(yend = Area, xend = 0), colour = "grey50", lwd = 0.6) +
  #geom_segment(aes(yend = Area, x = value, xend = value), colour = "grey50", lwd = 0.6) +
  geom_point(size = ifelse(dat1$punteggio == "Punteggio amministrazione", 17, 10)) +
  geom_text(aes(x = value, label = value),
            color = "black",
            fontface = "bold",
            size = ifelse(dat1$punteggio == "Punteggio amministrazione", 4, 3)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20), expand = c(0, 0.5)) +
  scale_color_manual(labels = c("Punteggio medio\namministrazione", "Punteggio medio\nnazionale"),
                     values = c("#5ab4ac", "#d8b365")) +
#  scale_shape_manual(values = c(1,3)) +
    coord_cartesian(xlim = c(10, 30)) +
    guides(colour = guide_legend(override.aes = list(size = 8)))

#plot p6----
p6  
  
# TO DO --> TEST E CORSI TOTALI ESEGUITI----
  
dt <- read_excel("GeneraliDaSIto/Risultati assessment.xlsx")


dt <- dt[, c(3:7)]

colnames(dt) <- c("Competenza","Nessun livello","Livello base", "Livello intermedio", "Livello avanzato")

# df1 <- as.data.frame(df1)
# class(df1)
# df1

dt <- pivot_longer(dt, -Competenza, names_to = 'livello') %>% 
  uncount(value)

dt <- dt %>% mutate(across(where(is_character),as_factor))

dt$livello <- factor(dt$livello, levels = c("Livello avanzato", "Livello intermedio", "Livello base", "Nessun livello"))

p5 <- dt %>%
  group_by(Competenza, livello) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = Competenza, y = freq , fill = livello)) +
  geom_bar(position = "fill", stat = "identity", color = 'black', width = 0.9) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) + #expand=c(0,0),
  geom_text(aes(label = paste0(round(freq, 2)*100,"%","\n(n=",n,")")), 
                position = position_stack(vjust = 0.5), size = 2.7) +
  ggtitle("Livello di padronanza raggiunto dai 488 dipendenti risultati idonei per ciascuna delle 11 competenze.") +
  theme_bw() +
  theme(
        #aspect.ratio = 1.1,
        plot.title = element_text(size = 11),
        axis.text.y=element_text(size=7.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        legend.title.align = 0.5,
        legend.title=element_blank()) +
  # scale_x_discrete(labels = c("Abilitati" = "Dipendenti \nabilitati",
  #                             "Registrati" = "Dipendenti \nregistrati")) +
  scale_fill_manual(labels = c("Livello\navanzato", "Livello\nintermedio", "Livello\nbase", "Nessun\nlivello"),
                    values = c("#5ab4ac", "#acd9d5", "#ebd9b2", "#d8b365")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30), expand = c(0, 0)) +
  guides(fill = guide_legend(reverse = T))

p5 + coord_flip()


  
  
  

# NO --> PUNTEGGIO DELL'AMMINISTRAZIONE (VEDI CODICI PERFORMANCE) ----
# dat <- read_excel("GeneraliDaSIto/Punteggio dell'amministrazione.xlsx")
# 
# dat <- dat[, c(2:4)]
# 
# colnames(dat) <- c("Area","Punteggio amministrazione","Punteggio medio")
# 
# dat$Area <- factor(dat$Area)
# dat$`Punteggio amministrazione` <- round(dat$`Punteggio amministrazione`,1)
# dat$`Punteggio medio` <- round(dat$`Punteggio medio`,1)
# 
# dat %>% 
#   ggplot(aes(x = `Punteggio amministrazione`  , y = Area)) + 
#   theme_bw() +
#   geom_segment(aes(yend = Area, xend = 0), colour = "grey50", lwd = 0.6) +
#   geom_segment(aes(yend = Area, x = `Punteggio amministrazione`, xend = `Punteggio medio`), colour = "grey50", lwd = 0.6) +
#   
#   geom_point(aes(x = `Punteggio amministrazione`, colour = 'Punteggio amministrazione'), col = "green", size = 14, pch = 21, bg = "green") +
#   geom_text(aes(x = `Punteggio amministrazione`, label = `Punteggio amministrazione`), color = "black", size = 4) +
#   
#   geom_point(aes(x = `Punteggio medio`, colour = 'Punteggio medio'), col = "#d8b365", size = 10, pch = 21, bg = "#d8b365") +
#   ggtitle("Punteggio medio raggiunto dall'amministrazione per area di competenza") +
#   geom_text(aes(x = `Punteggio medio`, label = `Punteggio medio`), color = "black", size = 2.5) +
#   theme(
#         plot.title = element_text(size = 11),
#         axis.text.y = element_text(size = 7.5),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.ticks.y = element_blank()) +
#   scale_y_discrete(labels = function(x) str_wrap(x, width = 25)) +
#   scale_x_discrete(expand = c(0, 0.5)) +
#   #coord_cartesian(xlim = c(10, 30))+
#   coord_fixed(ratio = 1.5)







# ## NO --> ADESIONE AL TEST----
# df0 <- df[,c(1, 3, 4)]
# # df0 <- as.data.frame(t(df0)) %>% 
# #        row_to_names(row_number = 1)
# 
# colnames(df0) <- c("stato","maschi","femmine")
# 
# df0 <- as.data.frame(as.matrix(t(df0))) %>% 
#        row_to_names(row_number = 1)
# 
# df0 <- tibble::rownames_to_column(df0, var = "sesso")
# 
# df0$sesso <- factor(df0$sesso)
# df0$Abilitati <- as.numeric(df0$Abilitati)
# df0$Registrati <- as.numeric(df0$Registrati)
# 
# 
# df0 <- pivot_longer(df0, -sesso, names_to = 'stato') %>% 
#   uncount(value)
# 
# df0 <- df0 %>% mutate(across(where(is_character),as_factor))
# 
# p0 <- df0 %>%
#   group_by(sesso, stato) %>% 
#   summarise(n = n()) %>%
#   mutate(freq = n / sum(n)) %>%
#   ggplot(aes(x = sesso, y = freq , fill = stato)) +
#   geom_bar(position = "fill", stat = "identity", color = 'black', width = 0.7) +
#   scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
#   scale_fill_manual(labels = c("Dipendenti\nabilitati", "Dipendenti\nregistrati"),
#                     values = c("#5ab4ac", "#d8b365")) +
#   geom_text(aes(label = paste0(round(freq, 3)*100,"%","\n(n=",n,")")), 
#                 position = position_stack(vjust = 0.5), size = 2.8) +
#   ggtitle("Partecipazione al test (per genere, %)") +
#   theme_bw() +
#   theme(
#         #aspect.ratio = 1.1,
#         plot.title = element_text(size = 11),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.text.y=element_text(size=7.5),
#         axis.ticks.x = element_blank(),
#         axis.text.x = element_blank(),
#         legend.position = "bottom",
#         legend.title.align = 0.5,
#         legend.title=element_blank()) +
#   scale_x_discrete(labels = c("maschi" = "Maschi",
#                               "femmine" = "Femmine"), expand = c(0, 0)) +
#   guides(fill = guide_legend(reverse = T))
# 
# 
# p0 + coord_flip()
 









