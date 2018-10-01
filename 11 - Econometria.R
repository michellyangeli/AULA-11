> IPCA.df<-read_excel("c:/Econometria/IPCA.xls")                       #Carrega o arquivo
> plot(IPCA.df$IPCA, type = "l")                                       #Cria um gráfico do arquivo
> MM <- data.frame(na.omit(ma(IPCA.df$IPCA,order = 12, centre = T)))   #Cria  uma serie de médias móveis tradicionais omitindo oa NAS de lag(N)=12    
> 
  > a <- (127-nrow(MM))+1                                                #Define um parâmetro a para ponderar as perdas de dados para ponderação da média
  > IPCA.DF <- as.data.frame(IPCA.df$IPCA[a:127])                        #Define IPCA.DF como uma vetor do mesmo tamanho que o vetor das médias móveis MM
  > Tabela1 <- cbind(IPCA.DF,MM)                                         #Cria a Tabela de Dados Tabela1
  > colnames(Tabela1) <- c("IPCA","Media Movel")
  > View(Tabela1)
  > 
    > Grafico <- ts(Tabela1, start = 2008, frequency = 12)                 #Cria a Serie Temporal "Grafico" mensal iniciando em 2008
    > 
      > plot(Grafico, plot.type= "single", col=c("Black","Blue"))
    > z <- lm(IPCA.df$IPCA~IPCA.df$Ano.Mes)                                #Regride os dados em relação ao tempo e verifica a tendência
    Error in model.frame.default(formula = IPCA.df$IPCA ~ IPCA.df$Ano.Mes,  : 
                                   tipo inv�lido (NULL) para vari�vel 'IPCA.df$Ano.Mes'
                                 In addition: Warning message:
                                   Unknown or uninitialised column: 'Ano.Mes'. 
                                 > abline(z, col="Green")                                               #Coloca a linha de regressão de tendência no gráfico
                                 Error in abline(z, col = "Green") : object 'z' not found
                                 > summary(z)
                                 Error in summary(z) : object 'z' not found
                                 > 
                                   > tabela2 <- as.data.frame(Tabela1$IPCA/Tabela1$`Média Móvel`)
                                 > plot(tabela2)
                                 Error in plot.window(xlim, ylim, log, ...) : 
                                   valores finitos s�o necess�rios para 'xlim'
                                 In addition: Warning messages:
                                   1: In min(x) : no non-missing arguments to min; returning Inf
                                 2: In max(x) : no non-missing arguments to max; returning -Inf
                                 > View(IPCA.df)
                                 > z <- lm(IPCA.df$IPCA~IPCA.df$Ano.M�s)                                #Regride os dados em relação ao tempo e verifica a tendência
                                 > abline(z, col="Green")                                               #Coloca a linha de regressão de tendência no gráfico
                                 > summary(z)
                                 
                                 Call:
                                   lm(formula = IPCA.df$IPCA ~ IPCA.df$Ano.M�s)
                                 
                                 Residuals:
                                   Min       1Q   Median       3Q      Max 
                                 -0.82458 -0.22349 -0.08363  0.20146  1.19230 
                                 
                                 Coefficients:
                                   Estimate Std. Error t value Pr(>|t|)
                                 (Intercept)      7.388e-01  4.464e-01   1.655    0.100
                                 IPCA.df$Ano.M�s -2.180e-10  3.262e-10  -0.668    0.505
                                 
                                 Residual standard error: 0.3545 on 125 degrees of freedom
                                 Multiple R-squared:  0.00356,	Adjusted R-squared:  -0.004412 
                                 F-statistic: 0.4466 on 1 and 125 DF,  p-value: 0.5052
                                 
                                 > 
                                   > tabela2 <- as.data.frame(Tabela1$IPCA/Tabela1$`Média Móvel`)
                                 > plot(tabela2)
                                 Error in plot.window(xlim, ylim, log, ...) : 
                                   valores finitos s�o necess�rios para 'xlim'
                                 In addition: Warning messages:
                                   1: In min(x) : no non-missing arguments to min; returning Inf
                                 2: In max(x) : no non-missing arguments to max; returning -Inf
                                 > tabela2 <- as.data.frame(Tabela1$IPCA/Tabela1$`Media Movel`)
                                 > plot(tabela2)
                                 > Inflacao <- ts(IPCA.df$IPCA, start = 2008, frequency = 12)
                                 > decomposicao <- decompose(Inflacao)
                                 > plot(decompose(Inflacao))
                                 > Tendencia <- decomposicao$trend
                                 > Sazonalidade <- decomposicao$seasonal
                                 > Ciclo <- decomposicao$random
                                 > Tab_Dados1 <- data.frame(IPCA.df$IPCA, Ciclo)
                                 > View(Tab_Dados1)
                                 > 
                                   > plot(Sazonalidade, type="l")
                                 > Serie_Tempo1 <- ts(Tab_Dados1, start = 2008, frequency = 12)
                                 > plot(Serie_Tempo1, plot.type = "single", col= c("Blue", "Red"))
                                 > Tab_Dados2 <- data.frame(IPCA.df$IPCA, Tendencia)
                                 > Serie_Tempo2 <- ts(Tab_Dados2, start = 2008, frequency = 12)
                                 > plot(Serie_Tempo2, plot.type = "single", col= c("Blue", "Red"))
                                 > 