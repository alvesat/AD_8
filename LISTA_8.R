#######[AD-UFPE-2019] LISTA 8 ###########################################################
####### ANTONIO FERNANDES ###############################################################

###### EXERCICIO 2 ######################################################################

# 5.5.1.1

  # PACOTES

if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(rvest) == F) install.packages('rvest'); require(rvest)
if(require(httr) == F) install.packages('httr'); require(httr)
if(require(xml2) == F) install.packages('xml2'); require(xml2)

  # LINK DO SITE

link <- "https://pt.wikipedia.org/wiki/Lista_de_munic%C3%ADpios_do_Brasil_por_IDH"

  # DADOS

conteudo <- readLines(link, encoding = 'UTF-8') # Definindo como UTF-8 devido a acentuacao

head(conteudo)

  # IDENTIFICANDO PERNAMBUCO

grep("Pernambuco", conteudo)  # A LINHA DO VETOR PERNAMBUCO É 784
grep("Distrito Federal", conteudo) # IDENTIFICANDO A LINHA COM O NOME DA PRIMEIRA UF
conteudo[181 - 81] # ATE O VETOR DE NUMERO 109 VERIFICA-SE UFs

indice <- 109 # COMECANDO COM O VETOR 109
nomes_uf <- NULL 
i <- 1

while(indice < 1081){
  if(i == 1){
    nomes_uf[i] <- conteudo[indice]
  } else {
    nomes_uf[i] <- conteudo[indice + 9]
  }
  indice <- indice + 9
  i <- i + 1
}

nomes_uf

nomes_uf <- gsub("[[:print:]]+\">", "", nomes_uf) 
nomes_uf <- gsub("</a>", "", nomes_uf) 
nomes_uf <- gsub("</b>", "", nomes_uf) 
nomes_uf <- gsub("<b>", "", nomes_uf)

nomes_uf

# 5.5.2.1

link <- "https://www.camara.leg.br/internet/deputado/DepNovos_Lista.asp?Legislatura=54&Partido=QQ&SX=QQ&Todos=None&UF=QQ&condic=QQ&forma=lista&nom"

bd <- link %>%
httr::GET() %>%
xml2::read_html() %>%
rvest::html_nodes('ul') %>%
rvest::html_table(header = TRUE)

class(bd)


