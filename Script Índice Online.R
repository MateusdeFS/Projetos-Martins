## Importa dados mensais de faturamento do TEF a partir do SAS Guide e
## monta um modelo de Regress�o Linear M�ltipla para prever os pr�ximos meses

## Para prosseguir com a an�lise, utilizo o m�todo 'backward' para escolher o melhor modelo,
## isto �, rodo um modelo com todas as vari�veis poss�veis e tiro uma de cada vez, sempre
## aquela com o menor p-valor (menos significativa), at� encontrar o modelo com o
## maior R� de Pearson ajustado

library(plyr)
library(MASS)
library(lattice)
library(pryr) 	#objetct_size()
library(lmtest)
library(readxl)

#
#	Modelo Martins Amplo
#

dados <- read_excel("Dados.xlsx")
attach(dados)
names(dados)

JAN <- as.factor(JAN)
FEV <- as.factor(FEV)
MAR <- as.factor(MAR)
ABR <- as.factor(ABR)
MAI <- as.factor(MAI)
JUN <- as.factor(JUN)
JUL <- as.factor(JUL)
AGO <- as.factor(AGO)
SET <- as.factor(SET)
OUT <- as.factor(OUT)
NOV <- as.factor(NOV)
DEZ <- as.factor(DEZ)
COVID <- as.factor(COVID)

MODELO_MS_INDICE <- lm(formula = VALOR_TOTAL ~ JAN + ABR + MAI + JUN + JUL + AGO + SET + OUT + NOV
					+ SUM_TER:VALOR_TOTAL1 + SUM_QUI:VALOR_TOTAL1 + SUM_DOM:VALOR_TOTAL1
					+ SUM_DIAS_UTEIS, data=dados)
summary(MODELO_MS_INDICE)

#
#	Modelo Smart Amplo
#
library(readxl)
dadossmart <- read_excel("Z:/Mateus/Previs�es Xtrategie/Projetos do �ndice Preservados/Dados Smart.xlsx", 
                          col_types = c("text", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric"))
View(dadossmart)
attach(dadossmart)
names(dadossmart)

JAN <- as.factor(JAN)
FEV <- as.factor(FEV)
MAR <- as.factor(MAR)
ABR <- as.factor(ABR)
MAI <- as.factor(MAI)
JUN <- as.factor(JUN)
JUL <- as.factor(JUL)
AGO <- as.factor(AGO)
SET <- as.factor(SET)
OUT <- as.factor(OUT)
NOV <- as.factor(NOV)
DEZ <- as.factor(DEZ)
COVID <- as.factor(COVID)

MODELO_SMART_INDICE <- lm(formula = VALOR_TOTAL ~ JAN + FEV + MAR + ABR + MAI + JUN + JUL + AGO + SET + OUT + NOV
          + SUM_TER:VALOR_TOTAL1 + SUM_SAB:VALOR_TOTAL1 + QDE_SEMANA
					+ COVID + VALOR_TOTAL2, data=dadossmart)
summary(MODELO_SMART_INDICE)
