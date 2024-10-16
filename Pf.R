########################################
#    Recursos utilizados na análise    #
########################################

install.packages("quarto")
library(tidyverse)
tinytex::install_tinytex()
tinytex::tinytex_root()

estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091", 
  "#041835", "#666666" )

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}


################
#  Analise 1   #
################


#Filtrando primeiramente as mulheres do banco de dados e separando elas por contagem de país e 
#país por medalha e separando os 5 primeiros

female_olympic <- filter(Olimpiadas_2000_2016, Gender == "F")
female_medalhist <- filter(female_olympic, Medal != "NA")
medalhist_country <- female_medalhist %>% count(Team, Medal, sort = TRUE)
medalhist_country_solo <- female_medalhist %>% count(Team, sort = TRUE)
top5_medalhist_solo <- medalhist_country_solo[1:5,]

#Trocando o nome das variaveis para ficar em Português

top5_medalhist_solo[1:5,1] = c("Estados Unidos", "Russia", "Alemanha", "China", "Australia")
top5_medalhist_gen[1:15,1] = c("Estados Unidos", "Estados Unidos", "Estados Unidos", "China", "Russia", "Australia", "Alemanha", "Alemanha", "Russia", "Russia", "Australia", "China", "Alemanha", "Australia", "China")
top5_medalhist_gen[1:15,2] = c("Ouro", "Prata", "Bronze", "Ouro", "Prata", "Prata", "Bronze", "Ouro", "Ouro", "Bronze", "Ouro", "Prata", "Prata", "Bronze", "Bronze")

#Criando o gráfico univariado

 ggplot(top5_medalhist_solo) +
  aes(x = fct_reorder(Team, n, .desc=T), y = n, label = n) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) +
  labs(x = "Time", y = "Atletas Femininas") +
  theme_estat()
ggsave("colunas-uni-freq.pdf", width = 158, height = 93, units = "mm"
)

#Ajeitando os dados para multivariado

female_medalhistEUA <- filter(female_medalhist, Team == "United States")
female_medalhistRUSSIA <- filter(female_medalhist, Team == "Russia")
female_medalhistGERMANY <- filter(female_medalhist, Team == "Germany")
female_medalhistCHINA <- filter(female_medalhist, Team == "China")
female_medalhistAUSTRALIA <- filter(female_medalhist, Team == "Australia")
female_medalhistMULTI <- rbind(female_medalhistAUSTRALIA, female_medalhistCHINA, female_medalhistEUA, female_medalhistGERMANY, female_medalhistRUSSIA)
top5_medalhist_gen <- female_medalhistMULTI %>% count(Team, Medal, sort = TRUE)
top5_medalhist_gen$Team <- factor(top5_medalhist_gen$Team, levels = c('Estados Unidos', 'Russia', 'Alemanha', 'China', 'Australia'))
top5_medalhist_gen$Medal <- factor(top5_medalhist_gen$Medal, levels = c('Ouro', 'Prata', 'Bronze'))

#Criando o gráfico multivariado

ggplot(top5_medalhist_gen) +
  aes(
    x = fct_reorder(Team, n, .desc = T), y = n,
    fill = Medal, label = n
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding =
                                        0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Times", y = "Quantidade de medalhas") +
  theme_estat()
ggsave("colunas-bi-freq.pdf", width = 158, height = 93, units = "mm")
