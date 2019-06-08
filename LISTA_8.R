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

# 5.5.2.1 (https://github.com/leobarone/Webscraping_R_XML_Legislativo_2016_1/blob/master/Atividade_2.Rmd)

  # Atribuindo o link a um objeto

url <- "https://www.camara.leg.br/internet/deputado/DepNovos_Lista.asp?Legislatura=54&Partido=QQ&SX=QQ&Todos=None&UF=QQ&condic=QQ&forma=lista&nom"

  # Capturando o código HTML

pagina <- readLines(url)

pagina <- htmlParse(pagina)

pagina <- xmlRoot(pagina)


nodes_link <- getNodeSet(pagina, '//*[@id="content"]')

nome_parlamentar <- xpathSApply(pagina, '//*[@id="content"]/ul/li/a', xmlValue)

site <- xpathSApply(pagina, '//*[@id="content"]/ul/li/a', xmlGetAttr, name = "href")

dados <- data.frame(nome_parlamentar, site)

save(dados, file = "dados_parlamentar.RDta")


# 5.7.0.1

if(require(httr) == F) install.packages('httr'); require(httr)
if(require(XML) == F) install.packages('XML'); require(XML)                                                         
if(require(xml2) == F) install.packages('xml2'); require(xml2)

link <- paste0("http://www.camara.leg.br/SitCamaraWS/Deputados.asmx/ObterDeputados")

response <- GET(link)

data <- xmlParse(response, encoding = "UTF-8")
ls <- xmlToList(data)

x <- ls[lapply(ls, '[[',"uf")=='PE']

names(x$deputado)

ideCadastro <- NULL
condicao <- NULL 
matricula <- NULL 
idParlamentar <- NULL
nome <- NULL 
nomeParlamentar <- NULL 
urlFoto <- NULL 
sexo <- NULL 
uf <- NULL
partido <- NULL 
email <- NULL


for(i in 1:length(x)){
  ideCadastro[i] <- x[[i]]$ideCadastro
  condicao[i] <- x[[i]]$condicao
  matricula[i] <- x[[i]]$matricula
  idParlamentar[i] <- x[[i]]$idParlamentar
  nome[i] <- x[[i]]$nome
  nomeParlamentar[i] <- x[[i]]$nomeParlamentar
  urlFoto[i] <- x[[i]]$urlFoto
  sexo[i] <- x[[i]]$sexo
  uf[i] <- x[[i]]$uf
  partido[i] <- x[[i]]$partido
  email[i] <- x[[i]]$email
}


bd <- data.frame(ideCadastro, condicao, matricula, idParlamentar, 
                 nome, nomeParlamentar, urlFoto, sexo, uf, partido, email)

head(bd)

# QUESTAO 3

    ## Uma alternativa é por meio da API da CD

if(require(RCurl) == F) install.packages('RCurl'); require(RCurl)
if(require(httr) == F) install.packages('httr'); require(httr)
if(require(XML) == F) install.packages('XML'); require(XML)                                                         
if(require(xml2) == F) install.packages('xml2'); require(xml2)
if(require(jsonlite) == F) install.packages('jsonlite'); require(jsonlite)

link <- "https://dadosabertos.camara.leg.br/api/v2/deputados?idLegislatura=41&idLegislatura=42&idLegislatura=43&idLegislatura=44&idLegislatura=45&idLegislatura=46&idLegislatura=47&idLegislatura=48&idLegislatura=49&idLegislatura=50&idLegislatura=51&idLegislatura=52&idLegislatura=53&idLegislatura=54&idLegislatura=55&ordem=ASC&ordenarPor=nome"

call1 <- paste(link, sep="")

dep <- GET(call1)

dep_text <- content(dep, "text")

dep_text_json <- fromJSON(dep_text, flatten = TRUE)

dep_df <- as.data.frame(dep_text_json)

      # Segunda alternativa

if(require(httr) == F) install.packages('httr'); require(httr)
if(require(XML) == F) install.packages('XML'); require(XML)                                                         
if(require(xml2) == F) install.packages('xml2'); require(xml2)

jump <- seq(41, 55, by = 1)

site <- paste0("https://www.camara.leg.br/internet/deputado/DepNovos_Lista.asp?Legislatura=",jump,"&Partido=QQ&SX=QQ&Todos=None&UF=QQ&condic=QQ&forma=lista&nome=&ordem=nome&origem=")

nome <- lapply(site, function(i) {
  pagina <- readLines(i)
  
  pagina <- htmlParse(pagina)
  
  pagina <- xmlRoot(pagina)
  
  
  nodes_link <- getNodeSet(pagina, '//*[@id="content"]')
  
  nome_parlamentar <- xpathSApply(pagina, '//*[@id="content"]/ul/li/a', xmlValue)
})  

site <- lapply(site, function(i) {
  pagina <- readLines(i)
  
  pagina <- htmlParse(pagina)
  
  pagina <- xmlRoot(pagina)
  
  
  nodes_link <- getNodeSet(pagina, '//*[@id="content"]')
  
  site <- xpathSApply(pagina, '//*[@id="content"]/ul/li/a', xmlGetAttr, name = "href")
})  

