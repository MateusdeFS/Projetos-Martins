## Primeira tentativa de previs�o do faturamento dos clientes iMAIS, usando
## S�ries Temporais

dados = "Cli2843.csv"
attach(dados)
names(dados)
require(rJava)
require(xlsx)  # Pacote para exportar em xlsx.

vendas =  ts(VENDAS, start = c(2019,2), frequency = 365)
vendas
ts.plot(vendas,xlab="Dia/Hora",ylab="Faturamento",main="Série Histórica do Faturamento") # O gráfico não tem tendência

# Teste de Tend�ncia

require(randtests)
cox.stuart.test(vendas)   # p-value = 0.0219, logo a série não possui tendência.

# Teste de Sazonalidade

require(GeneCycle)
fisher.g.test(vendas) # p-valor minúsculo; série possui sazonalidade.

# Pelos lag tem sazonalidadde estocástica, ajustar SARIMA

# SARIMA (p,d,q)(P,D,Q)

# SARIMA Sazonalidade 24

M1=arima(vendas,order=c(1,0,2), seasonal=list(order=c(2,0,2), period = 19))
M1 # Parâmetros Significativos

require(nortest)
lillie.test(M1$residuals) # Enquanto o p-valor estiver muito pequeno, tentar com outros par�mentros no comando arima.

# Histograma (os erros devem ter distribui��o normal)
hist(M1$residuals,xlab="Resíduos (2,0,1)(1,1,0)",ylab="Frequência",main="Distribuição dos Resíduos")

# Testes de depend�ncia da s�rie (p-valor deve ser alto)
Box.test(M1$residuals, type=c("Box-Pierce"))
Box.test(M1$residuals, type=c("Ljung-Box"))

ts.plot(M1$residuals,ylab="Resíduos (2,1,0)(1,1,0)",xlab="Tempo",main="Homecedasticidade Resíduos")

require(forecast)

previsao <- forecast(M1,20, level=c(90))
previsao
plot(previsao, lwd=2, col="black",
     xlab = "Dia", ylab=NA)

# Comando para gravar um arquivo .csv com as previs�es e seus limites (mudar o caminho inserido no comando)
results <- write.xlsx(previsao, file = "D:/Users/mateusd/Downloads/previsao_mateus.xlsx")
#################################################################


file <- paste(tempdir(), "/previsao.xlsx", sep="")
res <- write.xlsx(previsao, file) 
