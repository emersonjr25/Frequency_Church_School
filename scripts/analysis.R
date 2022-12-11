#### PACKAGES ####
library(ggplot2)
library(dplyr)

#### READ FILE ####
data_raw_groups <- read.csv("data/raw/Dado Bruto EBD_2.csv", sep = ";", check.names = F)
data_raw_students <- read.csv("data/raw/Lista_Estudantes.csv", sep = ";")
data_processed <- read.csv("data/processed/data_processed.csv", sep = ";")

#### treating files #####

# new dataframe #
data_processed <- cbind(data_processed, data_raw_groups[-c(1)])
data_processed <- data_processed[-length(data_processed$Meses), ]

for (i in 3:length(data_processed)){
  data_processed[[i]] <- round(data_processed[[i]] / data_processed[[2]])
}

data_processed$Meses <- as.character(data_processed$Meses)
data_processed$Meses <- factor(data_processed$Meses, levels=unique(data_processed$Meses))

data_processed <- data_processed %>% 
  rename("JOVENS" = "JOVENS E AD",
         "PRIMARIOS" = "PRIM\xc1RIOS",
         "NOVOS_CONVERTIDOS" = "NOVOS COM")

new_data_processed <- data_processed[-c(1, 2)]

for(i in 1:length(data_processed$Meses)){
  data_processed$total[i]  <- sum(as.numeric(new_data_processed[i ,]))
}

## certificate ##
EBD_METADE <- round((23 + 18) / 2)

data_raw_students$certificate <- data_raw_students$Frequencia_Total >= EBD_METADE

name_certificate <- data_raw_students$Pessoas[data_raw_students$certificate]

## students ##

data_students_ordened <- data_raw_students[order(data_raw_students$Frequencia_Total, decreasing = TRUE), ]

dataframe_students <- data_students_ordened[data_students_ordened$Posicao == "Estudante", ]

result_students <- dataframe_students$Frequencia_Total[1:5]

names(result_students) <- dataframe_students$Pessoas[1:5]

result_students
## teacher ##

dataframe_teacher <- data_students_ordened[data_students_ordened$Posicao == "Professor", ]

result_teacher <- dataframe_teacher$Frequencia_Total[1:5]

names(result_teacher) <- dataframe_teacher$Pessoas[1:5]

names(result_teacher) <- c("RUTE", "ELAINE", "EMERSON", "LIDIA", "QUEREN")

result_teacher

data_general_ebd <- c(total = sum(data_raw_students$Frequencia_Total >= 1), 
                      certificado = length(name_certificate),
                      EBD_TOTAL = 41,
                      result_teacher[1],
                      result_students[1])

data_general_ebd

outros <- c(Estefane_Est_Primarios = dataframe_students[dataframe_students$Classe == "Prim\xe1rio", ][1, 2],
            Daielen_Est_Jovens = dataframe_students[dataframe_students$Classe == "Jovens", ][1, 2],
            Luan_Est_Jovens = dataframe_students[dataframe_students$Classe == "Jovens", ][2, 2],
            Jorge_Prof_Senhores = dataframe_teacher[dataframe_teacher$Classe == "Senhores", ][1, 2],
            Jose_Prof_Novos = dataframe_teacher[dataframe_teacher$Classe == "Novos Convertidos", ][1, 2])
outros

write.table(name_certificate, file = paste('output/','certificate.txt'), col.names = FALSE)

#### plots ####
Senhores <- ggplot(data_processed, aes(x = Meses, y = SENHORES, group = 1)) +
  theme_classic() +
  geom_line(color="steelblue", size = 2) +
  geom_point() + ylim(0, max(data_processed[["SENHORES"]])) +
  ggtitle("Média de Frequência de Senhores em cada mês") + 
  theme(plot.title = element_text(hjust = 0.5))
  
Senhoras <- ggplot(data_processed, aes(x = Meses, y = SENHORAS, group = 1)) +
  theme_classic() +
  geom_line(color="steelblue", size = 2) +
  geom_point() + ylim(0, max(data_processed[["SENHORAS"]])) +
  ggtitle("Média de Frequência de Senhoras em cada mês") + 
  theme(plot.title = element_text(hjust = 0.5))

Jovens <- ggplot(data_processed, aes(x = Meses, y = JOVENS, group = 1)) +
  theme_classic() +
  geom_line(color="steelblue", size = 2) +
  geom_point() + ylim(0, max(data_processed[["JOVENS"]])) +
  ggtitle("Média de Frequência de Jovens em cada mês") + 
  theme(plot.title = element_text(hjust = 0.5))

Primarios <- ggplot(data_processed, aes(x = Meses, y = PRIMARIOS, group = 1)) +
  theme_classic() +
  geom_line(color="steelblue", size = 2) +
  geom_point() + ylim(0, max(data_processed[["PRIMARIOS"]])) +
  ggtitle("Média de Frequência de Primarios em cada mês") + 
  theme(plot.title = element_text(hjust = 0.5))


Juniores <- ggplot(data_processed, aes(x = Meses, y = JUNIORES, group = 1)) +
  theme_classic() +
  geom_line(color="steelblue", size = 2) +
  geom_point() + ylim(0, max(data_processed[["JUNIORES"]])) +
  ggtitle("Média de Frequência de Juniores em cada mês") + 
  theme(plot.title = element_text(hjust = 0.5))

Novos_convertidos <- ggplot(data_processed, aes(x = Meses, y = NOVOS_CONVERTIDOS, group = 1)) +
  theme_classic() +
  geom_line(color="steelblue", size = 2) +
  geom_point() + ylim(0, max(data_processed[["NOVOS_CONVERTIDOS"]])) +
  ggtitle("Média de Frequência de Novos Convertidos em cada mês") + 
  theme(plot.title = element_text(hjust = 0.5))

total <- ggplot(data_processed, aes(x = Meses, y = total, group = 1)) +
  theme_classic() +
  geom_line(color="steelblue", size = 2) +
  geom_point() + ylim(0, max(data_processed[["total"]])) +
  ggtitle("Média de Frequência total em cada mês") + 
  theme(plot.title = element_text(hjust = 0.5))


##### SAVING PLOTS ####
tiff(filename = paste('output/','Senhores.tiff'), height = 363, width = 612)
Senhores
dev.off()

tiff(filename = paste('output/','Senhoras.tiff'), height = 363, width = 612)
Senhoras
dev.off()

tiff(filename = paste('output/','Jovens.tiff'), height = 363, width = 612)
Jovens
dev.off()

tiff(filename = paste('output/','Primarios.tiff'), height = 363, width = 612)
Primarios
dev.off()

tiff(filename = paste('output/','Juniores.tiff'), height = 363, width = 612)
Juniores
dev.off()

tiff(filename = paste('output/','Novos_convertidos.tiff'), height = 363, width = 612)
Novos_convertidos
dev.off()

tiff(filename = paste('output/','Novos_convertidos.tiff'), height = 363, width = 612)
Novos_convertidos
dev.off()

tiff(filename = paste('output/','total.tiff'), height = 363, width = 612)
total
dev.off()

#### ANONYMOUS NAMES ####
letter_total <- rep(letters, 4)
for (i in 1:length(data_raw_students$Pessoas)){
  data_raw_students$Pessoas[i] <- paste(letter_total[i], i)
}
write.csv2(data_raw_students, file = paste('output/','data_raw_students.csv'), row.names = FALSE)
          