#######[AD-UFPE-2019] LISTA 8 ###########################################################
####### ANTONIO FERNANDES ###############################################################

###### EXERCICIO I ######################################################################

# REPLICACAO DOS GRAFICOS DO ARTIGO "USING GRAPHS INSTEAD OF TABLES IN POLITICAL SCIENCE#

# ESTATISTICA DESCRITIVA ################################################################

 

# FIGURA 1 ##############################################################################

###############TABLES AND GRAPHS IN POLITICAL SCIENCE JOURNALS ##########################

#NECESSARIO O BANCO DE DADOS ############################################################



# FIGURA 2 ##############################################################################

######################PERCENTAGE OF YEARS WITH LEFT GOVERNMENTS, 1945-1998 ##############

# VALORES DE CADA VARIAVEL

prop.left <- 342
prop.right <- 120
maj.left <- 86
maj.right <- 256

# CRIANDO MATRIZ PARA FAZER O GRAFICO

year.matrix <- 100* cbind(c(maj.left/(maj.left+maj.right), maj.right/(maj.left+maj.right)),
                          c(prop.left/(prop.left+prop.right), prop.right/(prop.left+prop.right)))

# ADICIONANDO ROTULO

y.label <- c("Majoritarian", "Proportional")

# PDF (o grafico e salvo em pdf, facilitando o uso p/ o Latex)

pdf("grafico.2.pdf", width = 5.5, height = 2)

# AJUSTANDO O LAYOUT DO PLOT (para garantir a proporcionalidade)

layout(cbind(1,2), widths = c(5,4))

# CRIANDO GRAFICO 
par(mar=c(3,4.5,3,1)) # margens do grafico

year.bar <- barplot(year.matrix, axes = F, horiz = T,
                    beside = F, main = "", col = c(gray(.8),
                                                   gray(.9), gray(.8), gray(.9)))


axis(1, at = c(0,25,50,75,100), labels = c(0, 25, 50, 75, "100%"), # intervalo por percentil 
     cex.axis = .8, mgp = c(2,.5,0))#
axis(2, at = year.bar, labels = y.label, tick = F, las = 1,
     mgp = c(2,.5,0), cex.axis = .8) #
mtext("Percentage of Years\nWith Left Governments,\n1945-1998", 3, line = .5, # titulo do grafico
      cex = .8, font = 2)

text(40, year.bar[2], "342", cex = .8)  # rotulos do grafico
text(88, year.bar[2], "120", cex = .8)
text(10, year.bar[1], "86", cex = .8)
text(60, year.bar[1], "256", cex = .8)

# SALVANDO O GRAFICO 

dev.off()

######################PERCENTAGE OF YEARS WITH LEFT GOVERNMENTS, 1945-1998 ##############

# VALORES DE CADA VARIAVEL

prop.left.gov <- 8
prop.right.gov <- 1
maj.left.gov <- 0
maj.right.gov <- 8

# CRIANDO MATRIZ PARA FAZER O GRAFICO

country.matrix <- 100* cbind(c(maj.left.gov/(maj.left.gov+maj.right.gov), maj.right.gov/(maj.left.gov+maj.right.gov)),
                             c(prop.left.gov/(prop.left.gov+prop.right.gov), prop.right.gov/(prop.left.gov+prop.right.gov)))

# ADICIONANDO ROTULO

y.label <- c("Majoritarian", "Proportional")

# PDF (o grafico e salvo em pdf, facilitando o uso p/ o Latex)

pdf("grafico.3.pdf", width = 5.5, height = 2)

# CRIANDO GRAFICO 

par(mar=c(3,1,3,1))  # margens do grafico

country.bar <- barplot(country.matrix, axes = F, horiz = T, # # intervalo por percentil 
                       beside = F, main = "", col = c(gray(.8),
                                                      gray(.9), gray(.8), gray(.9)))
axis(1, at = c(0,25,50,75,100), labels = c(0, 25, 50, 75, "100%"), cex.axis = .8,  mgp = c(2,.5,0))#draw x axis
axis(2, at = country.bar, labels = c("",""), tick = F, las = 1, 
     line = -.2,  mgp = c(2,.5,0), cex.axis = .8)
mtext("Percentage of Countries With Center-Left Governments at Least 50% of Time, 1945-1998", 3, line = .5, 
      cex = .8, font = 2) 

text(50, year.bar[2], "8", cex = .8) # rotulos do grafico
text(95, year.bar[2], "1", cex = .8)
text(50, year.bar[1], "0/8", cex = .8)

# SALVANDO O GRAFICO 

dev.off()

####### FIGURA 3 ########################################################################

## USING A SINGLE DOT PLOT TO PRESENT SUMMARY STATISTICS ################################

# CRIANDO VETOR

var.names <- c("Size\n(1260)", "Political Talk\n(1253)", "Political Agreement\n(1154)",
               "Political Knowledge\n(1220)")
mean.vec <- c(3.13, 1.82, .43, 1.22)
sd.vec <- c(1.49, .61, .41, .42)
min.vec <- c(1, 0, 0, 0)
max.vec <- c(5,3,1,2)

# ORDENANDO VALORES (em ordem decrescente (media, desvio padrao, min e max))

mean.vec.order <- sort(mean.vec, decreasing = TRUE) 
sd.vec.order <- sd.vec[order(mean.vec, decreasing=TRUE)] 
min.vec.order <- min.vec[order(mean.vec, decreasing = TRUE)]
max.vec.order <- max.vec[order(mean.vec, decreasing = TRUE)]
var.names.order <- var.names[order(mean.vec, decreasing = TRUE)]

# ORDENANDO VALORES EIXO Y

y.axis <- c(4:1)

# SALVANDO EM PDF

pdf("graph.4.pdf", height = 2.5, width =4.5)

# GRAFICO

par(mfrow=c(1,1), mar=c(3,8,1,2))# margens

plot(mean.vec.order, y.axis, type = "p", pch = 19, xlab="", ylab ="", 
     axes = F, xlim = c(0, 5.1),
     ylim = c(min(y.axis - .1), max(y.axis + .1)), main = "", cex = .6, yaxs = "i", xaxs = "i")
box(bty = "l") 
segments((mean.vec.order - sd.vec.order), y.axis, (mean.vec.order + sd.vec.order), y.axis)
segments(min.vec.order, y.axis, max.vec.order, y.axis, lty = 2)
axis(1, at = seq(0,5,by=1), labels =  seq(0,5,by=1), tick = T, 
     cex.axis = .7, mgp = c(2,.5,0))
axis(2, at = y.axis, label = var.names.order, las = 1, tick = T, cex.axis = .7, 
     mgp = c(2,3.2,0), hadj =.5)


# SALVANDO PDF

dev.off()

####### FIGURA 4 ########################################################################

##USING A DOT PLOT AND VIOLIN PLOTS TO PRESENT SUMMARY STATISTICS #######################

# VETORES DE CADA COLUNA E NOMES

var.names <- c("Issue\nConvergence\n(982)", "Competitiveness\n(CQ Ranking)\n(65)", 
               "Total Spending/\nCapita \n(65)", "Difference Spending/\nCapita\n(65)",
               "State VotingAge\nPop. (millions-ln) \n(65)", 
               "Percent\nNegative Ads\n(65)", "2000 Year\n(65)", "2002 Year\n(65)",
               "Consensual\nIssue\n(43)", "Issue\nOwned\n(43)", "Issue\nSalience\n(43)") 
mean.vec <- c(24.85, 1.54, 3.47, 1.12, 1.2, 21.38, .38, .32, .28, .49, 2.86)
sd.vec <- c(34.73, 1.2, 2.71, 1.32, .85, 16.84, .49, .47, .45, .51, 6.38)
min.vec <- c(0, 0, .28, .03, -.65, 0, 0, 0, 0,0, 0)
max.vec <- c(99.98, 3, 13.39, 9.26, 3.13, 54.96, 1, 1, 1, 1, 35.63)

# COLOCANDO O GRAFICO EM PDF

pdf("graph.5.pdf", height = 6, width =8)

# AJUSTANDO TAMANHO DOS TRES GRAFICOS

layout(rbind(c(1,2), c(3,3)),widths=c(3,3.1,2.6), respect = F)

#  MANTENDO APENAS AS VARIAVEIS BINARIAS

keep <- 7:10

# ESTATISTICAS DESCRITIVAS DAS VARIAVEIS BINARIAS

mean.vec.binary <-  mean.vec[keep] 
sd.vec.binary<- sd.vec[keep]
min.vec.binary <- min.vec[keep]
max.vec.binary <- max.vec[keep]
var.names.binary<-var.names[keep]

# COLOCAR OS VALORES EM ORDEM DESCENDENTE

mean.vec.binary.order <- sort(mean.vec.binary, decreasing = TRUE) 
sd.vec.binary.order <- sd.vec.binary[order(mean.vec.binary, decreasing = TRUE)] 
min.vec.binary.order <- min.vec.binary[order(mean.vec.binary, decreasing = TRUE)]
max.vec.binary.order <- max.vec.binary[order(mean.vec.binary, decreasing = TRUE)]
var.names.binary.order <- var.names.binary[order(mean.vec.binary, decreasing = TRUE)]

# EIXO Y TAMBEM EM ORDEM DESCENDENTE

y.axis.binary <- c(length(mean.vec.binary):1)

# AJUSTAR MAGENS DOS GRAFICOS

par(mar=c(3,7,3,1)) 

# GRAFICO

plot(mean.vec.binary.order, y.axis.binary, type = "p", pch = 19, xlab="", ylab ="", 
     axes = F, xlim = c(min(min.vec.binary.order), max(max.vec.binary.order)), 
     ylim = c(min(y.axis.binary - .1), max(y.axis.binary + .1)), 
     main = "Binary Variables", xaxs="r")  
box() 
axis(1, at = seq(0,1,by=.25), labels =  seq(0,1,by=.25), tick = T, 
     cex.axis = .8, mgp = c(2,.5,0))
axis(2, at = y.axis.binary, label = var.names.binary.order, las = 1, tick = T,
     cex.axis =1, mgp = c(2,3,0), hadj=.5) 

# REPETIR O MESMO CAMINHO COM OS DADOS DE PORCENTAGEM

# MANTENDO APENAS AS VARIAVEIS DE PORCENTAGEM

keep <- c(1,6,11) 

# ESTATISTICAS DESCRITIVAS DAS VARIAVEIS DE PORCENTAGEM

mean.vec.percents <-  mean.vec[keep]
sd.vec.percents<- sd.vec[keep]
min.vec.percents <- min.vec[keep]
max.vec.percents <- max.vec[keep]
var.names.percents<-var.names[keep]

# COLOCAR OS VALORES EM ORDEM DESCENDENTE

mean.vec.percents.order <- sort(mean.vec.percents, decreasing = TRUE)
sd.vec.percents.order <- sd.vec.percents[order(mean.vec.percents, decreasing = TRUE)] 
min.vec.percents.order <- min.vec.percents[order(mean.vec.percents, decreasing = TRUE)]
max.vec.percents.order <- max.vec.percents[order(mean.vec.percents, decreasing = TRUE)]
var.names.percents.order <- var.names.percents[order(mean.vec.percents, decreasing = TRUE)]

# EIXO Y TAMBEM EM ORDEM DESCENDENTE

y.axis.percents <- c(length(mean.vec.percents):1)

# AJUSTAR MAGENS DOS GRAFICOS

par(mar=c(3,8,3,1)) 

# GRAFICO


plot(mean.vec.percents.order, y.axis.percents, type = "p", pch = 19, xlab="", ylab ="", 
     axes = F, xlim = c(-12, 100),
     ylim = c(min(y.axis.percents - .1), max(y.axis.percents + .1)),
     main = "Percentage Variables", xaxs="i") 
box()
segments((mean.vec.percents.order - sd.vec.percents.order), y.axis.percents, (mean.vec.percents.order + sd.vec.percents.order), y.axis.percents)
segments(min.vec.percents.order, y.axis.percents, max.vec.percents.order, y.axis.percents, lty = 2)
axis(1, at = seq(0,100,by=25), labels =  c(0,25,50,75,"100%"), tick = T, 
     cex.axis = .8, mgp = c(2,.5,0))
axis(2, at = y.axis.percents, label = var.names.percents.order, las = 1, tick = T,  
     cex.axis =1, mgp = c(2,3.2,0), hadj=.5) 

# REPETIR O MESMO CAMINHO COM OS DADOS MENSURADOS EM MILH??O

# MANTENDO APENAS AS VARIAVEIS MILHAO

keep <- c(2:5)

# ESTATISTICAS DESCRITIVAS DAS VARIAVEIS EM MILHAO

mean.vec.millions <-  mean.vec[keep]
sd.vec.millions<- sd.vec[keep]
min.vec.millions <- min.vec[keep]
max.vec.millions <- max.vec[keep]
var.names.millions<-var.names[keep]

# COLOCAR OS VALORES EM ORDEM DESCENDENTE

mean.vec.millions.order <- sort(mean.vec.millions, decreasing = TRUE) 
sd.vec.millions.order <- sd.vec.millions[order(mean.vec.millions, decreasing = TRUE)] 
min.vec.millions.order <- min.vec.millions[order(mean.vec.millions, decreasing = TRUE)]
max.vec.millions.order <- max.vec.millions[order(mean.vec.millions, decreasing = TRUE)]
var.names.millions.order <- var.names.millions[order(mean.vec.millions, decreasing = TRUE)]

# EIXO Y TAMBEM EM ORDEM DESCENDENTE

y.axis.millions <- c(length(mean.vec.millions):1)

# AJUSTAR MAGENS DOS GRAFICOS

par(mar=c(3,18,3,12))

# GRAFICO


plot(mean.vec.millions.order, y.axis.millions, type = "p", pch = 19, xlab="", ylab ="", 
     axes = F, xlim = c(-1, 14),
     ylim = c(min(y.axis.millions - .1), max(y.axis.millions + .1)),
     main = "Variables Measured in Millions", xaxs="i")
box()
segments((mean.vec.millions.order - sd.vec.millions.order), y.axis.millions, 
         (mean.vec.millions.order + sd.vec.millions.order), y.axis.millions)
segments(min.vec.millions.order, y.axis.millions, max.vec.millions.order, y.axis.millions, lty = 2)
axis(1, at = seq(0,14,by=2), labels =  seq(0,14,by=2), tick = T, las  = 1,
     line =0, cex.axis = .8, mgp = c(2,.5,0))
axis(2, at = y.axis.millions, label = var.names.millions, las = 1, tick = T, 
     cex.axis =1, mgp = c(2,5,0), hadj=.5) 


# SALVANDO O PDF COM OS GRAFICOS

dev.off()


####### FIGURA 5 ########################################################################

##USING AN ADVANCED DOT PLOTS TO PRESENT PROPORTIONS ####################################

##NECESSARIO O BANCO DE DADOS ###########################################################


####### FIGURA 6 ########################################################################

## PRESENTING A SINGLE REGRESSION MODEL USING A DOT PLOT WITH ERROR BARS ################

# VETOR COM OS COEFICIENTES, ERROS PADR??O E NOMES DAS VARIAVEIS 

coef.vec <- c( 1.31, .93, 1.46, .07, .96, .2, .22, -.21, -.32, -.27,.23, 0, -.03, .13, 
               .15, .31, -.10, .41)
se.vec <- c( .33, .32, .32, .37, .37, .13, .12, .12, .12, .07, .07, .01, .21,
             .14, .29, .25, .27, .93)
var.names <- c("Argentina", "Chile", "Colombia", "Mexico", "Venezuela", 
               "Retrospective egocentric\neconomic perceptions", "Prospective egocentric\neconomic perceptions",
               "Retrospective sociotropic\neconomic perceptions", "Prospective sociotropic\neconomic perceptions",
               "Ideological Distance\nfrom president", "Ideology", "Age", "Female", "Education",
               "Academic sector", "Business sector", "Government sector", "Constant")

# EIXO Y EM ORDEM DESCENDENTE

y.axis <- c(length(coef.vec):1)

# ABRINDO O PDF

pdf("graph.6.pdf", height = 7, width = 6)

# AJUSTANDO AS MARGENS DO GRAFICO

par(mar=c(2, 9, 2, 1))

# GRAFICO 

plot(coef.vec, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, cex = .6,
     xlim = c(-2.5,2.5), xaxs = "i", main = "") 

# INTERVALOS
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd =  1.5)
segments(coef.vec-qnorm(.95)*se.vec, y.axis -.1, coef.vec-qnorm(.95)*se.vec, y.axis +.1, lwd = 1.1)
segments(coef.vec+qnorm(.95)*se.vec, y.axis -.1, coef.vec+qnorm(.95)*se.vec, y.axis +.1, lwd = 1.1)

# EIXOS DO GRAFICO
axis(1, at = seq(-2.5,2.5,by=.5), labels =  seq(-2.5,2.5,by=.5), tick = T,
     cex.axis = .8, mgp = c(2,.5,0))
axis(3, at = seq(-2.5,2.5,by=.5), labels =  seq(-2.5,2.5,by=.5), tick = T, las  = 1,
     line =0, cex.axis = .8, mgp = c(2,.7,0))                                            
axis(2, at = y.axis, label = var.names, las = 1, tick = T, 
     cex.axis = .8) 
abline(v=0, lty = 2) 
box(bty = "o") 

# ADICIONANDO TEXTOS DO R2, R2 AJUSTADO E N
text(1.2, 5, expression(R^{2} == .15), adj = 0, cex = .7)
text(1.2, 4, expression(paste("Adjusted ", R^{2} == .12, "")), adj = 0, cex = .7)
text(1.2, 3, "n = 500", adj = 0, cex = .7)

# CAIXA EM VOLTA DO TEXTO
segments(1.1, 2.7, 1.1,5.3)
segments(1.1, 2.7, 5,2.7)
segments(1.1, 5.3, 5.2,5.3)

# SALVAR PDF

dev.off()


####### FIGURA 7 ########################################################################

## USING PARALLEL DOT PLOTS WITH ERROR BARS TO PRESENT TWO REGRESSION MODELS ############

# nesse caso, sao dois modelos de regressao** ###########################################

# COEFICIENTES, ERROS PADRAO E NOMES DAS VAR DOS DOIS MODELOS #########################


coef.vec.1<- c(0.18, -0.19,-0.39,-0.09, NA, 0.04,-0.86, 0.39,-3.76, -1.61,
               -0.34, -1.17, -1.15,-1.52, -1.66, -1.34,-2.89,-1.88,-1.08, 0.20)
se.vec.1 <-  c(0.22, 0.22, 0.18,.29, NA, 0.08,0.26,0.29,0.36,.19,0.19, 0.22,
               0.22,0.25,0.28,0.32,0.48, 0.43,0.41, 0.20)
coef.vec.2 <-  c(0.27,-0.19, NA, NA, 0.5, 0.04,-0.98,-.36,-3.66, -1.59,
                 -0.45, -1.24, -1.04, -1.83, -1.82, -1.21, -2.77, -1.34, -0.94, 0.13)
se.vec.2 <- c(0.22,0.24, NA, NA, 0.4, 0.09 , .31 , .30 , .37 , .21 , .21 , .24 , .24,
              .29 , .32 , .33 , .49 , .46 , .49 , .26)
var.names <- c("Zombie" , "SMD Only", "PR Only", "Costa Rican in PR", 
               "Vote share margin", "Urban-Rural Index","No factional\nmembership",
               "Legal professional", "1st Term", "2nd Term", "4th Term",
               "5th Term","6th Term","7th Term","8th Term","9th Term","10th Term",
               "11th Term","12th Term", "Constant")

# INDICADOR DO EIXO Y PARA ORDEM DESCENDENTE DAS VAR.

y.axis <- length(var.names):1

# OBJETO PARA AJUSTAR OS PONTOS E LINHAS DOS 2 MODELOS

adjust <- .2 

# ABRINDO PDF

pdf("graph.7.pdf", height = 8, width = 7) 

# LAYOUT DO GRAFICO 
layout(matrix(c(2,1),1,2), 
       widths = c(2, 5))

# MARGENS DO GRAFICO

par(mar=c(2,5,2,1))

# GRAFICO MOD 1

plot(coef.vec.1, y.axis+adjust, type = "p", axes = F, xlab = "", ylab = "", pch = 19, cex = .8, 
     xlim = c(min((coef.vec.1-qnorm(.975)*se.vec.1 -.1), (coef.vec.2-qnorm(.975)*se.vec.2 -.1), na.rm = T), 
              max((coef.vec.1+qnorm(.975)*se.vec.1 -.1), (coef.vec.2+qnorm(.975)*se.vec.2 -.1), na.rm = T)),  #
     ylim = c(min(y.axis), max(y.axis)), main = "")

# EIXOS MOD 1

axis(1,pretty(coef.vec.1, 3))
axis(2, at = y.axis, label = var.names, las = 1, tick = T)
axis(3,pretty(coef.vec.1, 3))
abline(h = y.axis, lty = 2, lwd = .5, col = "light grey")
box()

# LINHAS COM OS I.C E LINHA VERTICAL DO TESTE DE HIPOTESE MOD 1

segments(coef.vec.1-qnorm(.975)*se.vec.1, y.axis+adjust, coef.vec.1+qnorm(.975)*se.vec.1, y.axis+adjust, lwd =  1.3)
segments(coef.vec.1-qnorm(.95)*se.vec.1, y.axis+adjust -.035, coef.vec.1-qnorm(.95)*se.vec.1, y.axis+adjust +.035, lwd = 1.1)
segments(coef.vec.1+qnorm(.95)*se.vec.1, y.axis+adjust -.035, coef.vec.1+qnorm(.95)*se.vec.1, y.axis+adjust +.035, lwd = 1.1)  
abline(v=0, lty = 2, lwd = 1.5) 

# ADICIONAR LINHAS CONECTANDO OS IC DE 95% E PONTOS DO 2 MODELO

segments(coef.vec.2-qnorm(.975)*se.vec.2, y.axis-adjust, coef.vec.2+qnorm(.975)*se.vec.2, y.axis-adjust, lwd =  1.3)
segments(coef.vec.2-qnorm(.95)*se.vec.2, y.axis-adjust -.035, coef.vec.2-qnorm(.95)*se.vec.2, y.axis-adjust +.035, lwd = 1.1)
segments(coef.vec.2+qnorm(.95)*se.vec.2, y.axis-adjust -.035, coef.vec.2+qnorm(.95)*se.vec.2, y.axis-adjust +.035, lwd = 1.1)  
points(coef.vec.2, y.axis-adjust, pch = 21, cex = .8, bg = "white" )

# CATEGORIAS DAS VARIAVEIS DO 2 MODELO

# MARGENS E PLOT
par(mar=c(2,2,2,0))
plot(seq(0,1,length=length(var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .55
segments(left.side,20.2,left.side,16.7) 
segments(left.side,20.2,left.side+.15,20.2) 
segments(left.side,16.7,left.side+.15,16.7)
text(.4, 18.5, "MP Type", srt = 90, font = 3)



segments(left.side,15.6,left.side,12.3)
segments(left.side,15.6,left.side+.15,15.6)
segments(left.side,12.3,left.side+.15,12.3)
text(.3, 14, "Misc\nControls", srt = 90, font = 3)

segments(left.side,12.15,left.side,1.8)
segments(left.side,12.15,left.side+.15,12.15)   
segments(left.side,1.8,left.side+.15,1.8)
text(.4, 7, "Seniority", srt = 90, font = 3)

# SALVAR PDF

dev.off()


####### FIGURA 8 ########################################################################

## USING 'SMALL MULTIPLE' PLOTS TO PRESENT REGRESSION RESULTS FROM SEVERAL MODELS #######

# CRIANDO MATRIZ COM OS DADOS

# COEFICIENTES

coef.matrix <- matrix(c(-.039, NA, .048, -.133, .071, -.795, 1.47, 
                        -.036, NA, .036, -.142, .07, -.834, 1.70, 
                        -.051, NA, .017, .05, .011, -.532, .775, 
                        -.037, -.02, .047, -.131,.072, -.783, 1.45,
                        -.034, -.018, -.035, -.139, .071, -.822, 1.68,
                        -.05, -.023, .016,-.049, .013, -.521, .819),nr=7)
# R2 DOS MODELOS

R2<-  c(0.910,  0.910,  0.940,  0.910,  0.910,  0.940)

# ERRO PADRAO

se.matrix <- matrix(c(.003, NA, .011, .013, .028, .056, .152, .003, NA, .012, .014, .029, .059, .171, .003, NA,
                      .01, .013, .024, .044, .124, .003, .005, .011, .013, .028, .055, .152, .003, .005, .021, .014,
                      .029, .059, .17, .003,.006, .01, .013, .024, .044, .127),nr=7)

# NOMES DAS VARIAVEIS

varnames<- c("% of county\nregistration", "Law change", "Log population", "Log median\nfamily income",
             "% population with\nh.s. education" ,"% population\nAfrican American" ,"Constant")

# REMOVENDO O INTERCEPTO DO COEF E DO ERRO PADRAO

coef.matrix<-coef.matrix[-(7),]
se.matrix<-se.matrix[-7,]

# LISTA COM OS COEFICIENTES E IC

Y1 <- vector(length=0,mode="list")

Y1$estimate <- coef.matrix[,4:6]

# IC 95%

Y1$lo <- coef.matrix[,4:6]-qnorm(0.975)*se.matrix[,4:6]
Y1$hi <- coef.matrix[,4:6]+qnorm(0.975)*se.matrix[,4:6]

# IC 90%

Y1$lo1 <- coef.matrix[,4:6]-qnorm(0.95)*se.matrix[,4:6]
Y1$hi1 <- coef.matrix[,4:6]+qnorm(0.95)*se.matrix[,4:6]

# NOMES DAS LINHAS

rownames(Y1$estimate) <- varnames[-7] ##no intercept

### LISTA COM AS INFORMACOES DO 2 MODELO ###

Y2 <- vector(length=0,mode="list")

Y2$estimate <- coef.matrix[,1:3]

# IC 95%
Y2$lo <- coef.matrix[,1:3]-qnorm(.975)*se.matrix[,1:3]
Y2$hi <- coef.matrix[,1:3]+qnorm(.975)*se.matrix[,1:3]

# IC 90%

Y2$lo1 <- coef.matrix[,1:3]-qnorm(.95)*se.matrix[,1:3]
Y2$hi1 <- coef.matrix[,1:3]+qnorm(.95)*se.matrix[,1:3]

# NOMES DAS LINHAS

rownames(Y2$estimate) <- varnames[-7]

# ABRINDO O PDF

pdf(file="graph.9.pdf",width=3,height=9)

# ABRINDO PACOTE PARA A FUNCAO PLOT.REG

source("plotReg.R")

# GRAFICO

tmp <- plot.reg(Y1,Y2,
                label.x=c("Full Sample","Excluding counties w. partial registration",
                          "Full sample w.state year dummies"),
                refline=c(0,0,0,0,0,0),
                hlast=.12,
                print=FALSE,
                lwd.fact=.9,
                length.arrow=unit(0.3,"mm"),
                widths=c(.35,.52,.15),
                rot.label.y=0,
                just.label.y="right",
                pos.label.y=0.95,
                pch.size=0.35)

# ROTACAO DOS NOMES DO EIXO X

tmp <- editGrob(tmp,gPath("xaxis","labels"),rot=45,just="right")

# VISUALIZAR O GRAFICO E SALVAR PDF

grid.draw(tmp) 
graphics.off()