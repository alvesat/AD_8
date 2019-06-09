#######[AD-UFPE-2019] LISTA 8 ###########################################################
####### ANTONIO FERNANDES ###############################################################

###### EXERCICIO 2 ######################################################################

# 5.5.1.1

  # Pacotes

if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(rvest) == F) install.packages('rvest'); require(rvest)
if(require(httr) == F) install.packages('httr'); require(httr)
if(require(xml2) == F) install.packages('xml2'); require(xml2)

  # link do site

link <- "https://pt.wikipedia.org/wiki/Lista_de_munic%C3%ADpios_do_Brasil_por_IDH"

  # dados

conteudo <- readLines(link, encoding = 'UTF-8') # Definindo como UTF-8 devido a acentuacao

head(conteudo)

  # identificando pernambuco

grep("Pernambuco", conteudo)  # A LINHA DO VETOR PERNAMBUCO É 784
grep("Distrito Federal", conteudo) # IDENTIFICANDO A LINHA COM O NOME DA PRIMEIRA UF
conteudo[181 - 81] # ATE O VETOR DE NUMERO 109 VERIFICA-SE UFs

indice <- 109 # COMECANDO COM O VETOR 109
nomes_uf <- NULL 
i <- 1

  # coletando a informacao da uf

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

  # removendo caracteres especiais de uf

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

  # nodes da pagina

nodes_link <- getNodeSet(pagina, '//*[@id="content"]')

  # node do nome do parlamentar

nome_parlamentar <- xpathSApply(pagina, '//*[@id="content"]/ul/li/a', xmlValue)

  # node do site

site <- xpathSApply(pagina, '//*[@id="content"]/ul/li/a', xmlGetAttr, name = "href")

  # convertendo para dataframe

dados <- data.frame(nome_parlamentar, site)

  # salvando os dados

save(dados, file = "dados_parlamentar.RDta")


# 5.7.0.1

  # carregando pacotes necessarios

if(require(httr) == F) install.packages('httr'); require(httr)
if(require(XML) == F) install.packages('XML'); require(XML)                                                         
if(require(xml2) == F) install.packages('xml2'); require(xml2)

  # paste link do site

link <- paste0("http://www.camara.leg.br/SitCamaraWS/Deputados.asmx/ObterDeputados")

  # obtendo os dados

response <- GET(link)

  # convertendo para xml

data <- xmlParse(response, encoding = "UTF-8")
ls <- xmlToList(data)

  # lapply para selecionar apenas a uf 'pe'

x <- ls[lapply(ls, '[[',"uf")=='PE']

  # identificando os nomes presentes na lista

names(x$deputado)

  # criando um vetor para cada parte da lista

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

  # atribuindo as informações da lista para cada vetor criado


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

  # transformando em data.frame

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

# pacotes necessarios

if(require(httr) == F) install.packages('httr'); require(httr)
if(require(XML) == F) install.packages('XML'); require(XML)                                                         
if(require(xml2) == F) install.packages('xml2'); require(xml2)
if(require(plyr) == F) install.packages('plyr'); require(plyr)

# criando vetor com as legislaturas para o loop

jump <- seq(41, 55, by = 1)

# site 

site <- paste0("https://www.camara.leg.br/internet/deputado/DepNovos_Lista.asp?Legislatura=",jump,"&Partido=QQ&SX=QQ&Todos=None&UF=QQ&condic=QQ&forma=lista&nome=&ordem=nome&origem=")


# usando lapply para coletar o nome dos parlamentares de todas as legislaturas

nome <- lapply(site, function(i) {
  pagina <- readLines(i)
  
  pagina <- htmlParse(pagina)
  
  pagina <- xmlRoot(pagina)
  
  
  nodes_link <- getNodeSet(pagina, '//*[@id="content"]')
  
  nome_parlamentar <- xpathSApply(pagina, '//*[@id="content"]/ul/li/a', xmlValue)
})  

# usando lapply para coletar o site dos parlamentares de todas as legislaturas


site <- lapply(site, function(i) {
  pagina <- readLines(i)
  
  pagina <- htmlParse(pagina)
  
  pagina <- xmlRoot(pagina)
  
  
  nodes_link <- getNodeSet(pagina, '//*[@id="content"]')
  
  site <- xpathSApply(pagina, '//*[@id="content"]/ul/li/a', xmlGetAttr, name = "href")
})  

# atribuindo o numero da respectiva legislatura para cada elemento da lista em nome 

names(nome) <- jump
nome <- ldply(nome, data.frame)

# unlist site 

site <- unlist(site)
site <- as.data.frame(site)

# juntando os dois dataframes e arrumando os nomes das variaveis
 
 deputados <- cbind(nome, site)
 
 deputados$leg <- deputados$.id
 deputados$nome <- deputados$X..i..
    
 deputados$.id <- NULL
 deputados$X..i.. <- NULL
 
 deputados$nome <- gsub('/', '', deputados$nome)

 
  