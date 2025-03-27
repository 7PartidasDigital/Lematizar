####################################################
# Script para replicar el análisis de tópicos con  #
# stm que se discute en el el artículo "Lematizar  #
# o no lematizar, esa es la cuestión: las Siete    #
# Partidas desde el Topic Modeling" de J. M.       #
# Fradejas Rueda publicado en "Claro espejo donde  #
# la ilusión se mira. Homenaje a Pilar Celma Valero#
# y Javier Blasco Pacual. Valladolid, Universidad  #
# Valladolid, 2025.                                #
# Desarrolado en el proyecto 7PartidasDigital.     #
# AEI PID2020-112621GB-I00/10.13039/501100011033   #
# https://7partidas.hypotheses.org/                #
####################################################

# Cargar las librería necesarias
library(tidyverse)
library(tidytext)
library(stm)

# Esta tabla se ha generado a partir del análisis 
# automártico (PoSTagging) llevado a  cabo con el
# analizador desarrollado para el Old Spanish
# Textual Archive (OSTA)
# https://oldspanishtextualarchive.org/
# a partir de la transcripción electrónica del texto
# de la editio princeps de las Siete Partidas (Sevilla,
# 1491)

partidas_tabla <- read_tsv("IOC-POS-tabla.txt")

partidas_sparse <- partidas_tabla %>%
  drop_na() %>%
  #filter(upos == "NOUN" | upos == "ADJ" | upos == "VERB") %>% # Esta línea se activa (borrar el #) solo selecciona SUST., ADJ., VERB.
  filter(str_detect(lema, "[^0-9]+")) %>% # Borra los lemas cuyo valor sea número. Para explorar por palabras se anula línea con # antes de filter
  filter(upos != "PROPN") %>% # Borra los nombres propios Si se quiere desactivar, hay que poner un # antes de filter
  count(ley, lema) %>% # Cambiar ley | titulo y lema | palabra según se desee por leyes o títulos y por lema o palabras
  cast_sparse(ley, lema, n) # Cambiar ley | titulo y lema | palabra según se desee por leyes o títulos y por lemas o palabras
dim(partidas_sparse)

set.seed(1234)
topic_model <- stm(partidas_sparse, K = 16, verbose = FALSE) # el valor de K es el que deterimina nº tópicos

# Muestra en la consola las n primera palabras de cada tópico
labelTopics(topic_model, n = 10)

partidas_gamma <- tidy(
  topic_model, 
  matrix = "gamma",
  document_names = rownames(partidas_sparse)
) 

# Imprime los gráficos de bigotes con
partidas_gamma %>% 
  left_join(
    partidas_tabla %>% 
      select(partida, document = ley) %>% # Aquí hay que usar titulo | ley depende de lo que se haya seleccionado antes
      mutate(partida = fct_inorder(partida))
  ) %>%
  mutate(topic = factor(topic)) %>% 
  ggplot(aes(gamma, topic, fill = topic)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  facet_wrap(vars(partida), ncol = 4) +
  labs(x = expression(gamma))

