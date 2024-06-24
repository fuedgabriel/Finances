library(zoo)
library(quantmod)
library(tseries)
library(PerformanceAnalytics)
library(tidyverse)

############################################################
# DEMO 1
############################################################
# Estatística amostral

# Itaú Unibanco Holding SA
# Vale SA
# Bradesco
# Ambev
# B3
# Petrobrás

# baixando dados do Yahoo...
START = "2014-09-01"
END = "2019-09-30"
codigos = c("ITUB4", "VALE3", "BBDC4", "ABEV3", "B3SA3", "PETR4")

getStockData <- function(codigo) {
  aux = get.hist.quote(
    instrument=paste0(codigo, ".SA"), 
    start=START, end=END, 
    quote="AdjClose",
    provider="yahoo", 
    compression = "d",
    retclass="zoo", 
    quiet=TRUE)
  colnames(aux) <- codigo
  return(aux)
}

close.df <- 
  do.call(merge, lapply(codigos, getStockData)) %>% 
  data.frame() 

head(close.df, 5)
tail(close.df, 5)

# evolução dos preços 
plot(close.df$ITUB4, main = 'ITUB4', ylab = 'Fechamento Ajustado', type='l', lwd = 2)
plot(close.df$VALE3, main = 'VALE3', ylab = 'Fechamento Ajustado', type='l', lwd = 2)
plot(close.df$BBDC4, main = 'BBDC4', ylab = 'Fechamento Ajustado', type='l', lwd = 2)
plot(close.df$B3SA3, main = 'B3SA3', ylab = 'Fechamento Ajustado', type='l', lwd = 2)
plot(close.df$PETR4, main = 'PETR4', ylab = 'Fechamento Ajustado', type='l', lwd = 2)
plot(close.df$ABEV3, main = 'ABEV3', ylab = 'Fechamento Ajustado', type='l', lwd = 2)

# ou, tudo junto...
# gráfico de retornos normalizados
# Calcula a normalização pela primeira linha de cada coluna
normalized_df <- sweep(as.matrix(close.df), 2, as.matrix(close.df[1, ]), "/")
# Plota as séries temporais normalizadas
ts.plot(normalized_df, col = 1:length(codigos), lwd = 2)
# legendas
graphics::legend(x = 0, y = 5, legend = codigos, 
    col=1:length(codigos), lty = 1, lwd = 3)


# Calculando os retornos mensais cc
ret.df = apply(X = log(close.df), MARGIN = 2, FUN = diff)
ret.df %>% head()

# ret.df é do tipo matrix. Convertendo para um data.frame:
ret.df = 
  ret.df %>% 
  data.frame() %>% 
  rownames_to_column('data') %>% 
  mutate(data = as.Date(data))
head(ret.df)

ret.xts = xts(x = ret.df[, -1], order.by = ret.df$data)
ret.xts %>% xts::first()
ret.df %>% xts::first()
#' as duas estruturas df e xts são espécies de matrizes
#' às vezes será mais conveniente usar uma, às vezes a outra, mas lembre-se,
#' ret.df tem uma coluna data enquanto que, em ret.xts, as datas são os
#' índices das linhas

#' Será que o modelo é bom? Isto é, os retornos variam ao redor de uma média
#' e seguem uma distribuição normal
#' 
# plota os retornos de ITUB4
plot(ret.df$ITUB4, type = 'l', ylab = 'retornos cc', main = 'ITUB4', lwd = 2)
# ou, num formato mais profissional
quantmod::chartSeries(ret.xts$ITUB4, name = 'ITUB4')

# os dados parecem variar em torno de uma média
abline(h = mean(ret.df$ITUB4), col = 'red')

# hipótese: os retornos cc seguem uma distribuição normal
# visualmente...
hist(ret.df$ITUB4, breaks = 30, col = 'slateblue')
#' um teste um pouco mais técnico: os pontos gerados pela qqnorm 
#' deveriam estar sobre a reta definida por qqline
# Usando dados simulados:
dados = rnorm(1e4)
# plota o gráfico Q-Q
qqnorm(dados)
qqline(dados, col='red')
# os retornos de ITUB seguem uma distribuição normal?
qqnorm(ret.df$ITUB4)
qqline(ret.df$ITUB4)
# caudas gordas...

# estatística amostral: extraindo os parâmetros do modelo
# a matriz de covariancias
sigma.mat = cov(ret.xts)
sigma.mat

# o valor esperado dos retornos
(mu.vec = apply(ret.xts, 2, mean))

# a volatilidade dos ativos 
(sd.vec = apply(ret.xts, 2, sd))

# também poderia ser obtida de sigma.mat
sqrt(diag(sigma.mat))

# matriz de correlações
cor.mat = cor(ret.xts)
cor.mat

# visualizando as correlações
plot(x = ret.df$ITUB4, y = ret.df$BBDC4, pch=16)
plot(x = ret.df$ABEV3, y = ret.df$VALE3, pch=16)

# ou
pairs(ret.df[, -1], pch = 16)

# ou ainda...
library(corrplot)
corrplot(cor.mat)
corrplot.mixed(cor.mat)
# visualização mais rápida dos pares...
corrplot.mixed(cor.mat, tl.pos = 'lt', diag = 'u')

# informações em abundancia....
PerformanceAnalytics::chart.Correlation(ret.xts)

# um gráfico interessante...
plot(sd.vec, mu.vec, pch=16, 
     # xlim = c(0, max(sd.vec)), ylim = c(0.004, max(mu.vec)),
     xlim = c(0, max(sd.vec)), 
     xlab = expression(sigma), ylab = expression(mu), 
     main = expression(paste(mu, ' X ', sigma)))
text(sd.vec, mu.vec, labels = codigos, pos = 4, cex = 0.7)

# qual ativo é melhor?
# essas retas ajudam?
# As retas de Sharpe
# rf = 0.8133/100   # taxa Selic média no período (mensal)
rf = 0.000387883632514185 # diária
rf
segments(0, rf, sd.vec, mu.vec, col = 1:6)


############################################################
# DEMO 2a
############################################################
# Simulação de Monte Carlo
# Simulando retornos mensais para ABEV3 usando o modelo CER
# desconsiderando por ora as covariancias com os outros ativos...
ret.sim = rnorm(nrow(ret.df), mean = mu.vec['ABEV3'], sd = sd.vec['ABEV3'])

# comparação visual das duas séries...
layout(1:2)
plot(ret.sim, type = 'l', lwd = 2, col = 'blue', main = 'ABEV3 Simulação')
plot(ret.df$ABEV3, type = "l", lwd = 2, col = 'blue', main = 'ABEV3')
layout(1)
# ou...
ts.plot(cbind(ret.df$ABEV3, ts(ret.sim)), col=c('black', 'red'), lwd=2)

# ainda mais útil na simulação de Monte Carlo, olhar para a frente!
# Vamos supor que queremos uma previsão para os preços dois anos à frente...
# ret.sim = rnorm(24, mean = mu.vec['ABEV3'], sd = sd.vec['ABEV3'])
ret.sim = rnorm(2*252, mean = mu.vec['ABEV3'], sd = sd.vec['ABEV3'])

# a simulação da evolução dos preços
# P0 é o preço de ABEV ao final do período de observação
(P0 = close.df[nrow(close.df), 'ABEV3'])
#' vamos calcular a evolução dos preços a partir de P0
#' lembre-se, os retornos em ret.sim são continuamente compostos

#' Calcula os preços futuros de forma explícita
n <- length(ret.sim)  # Número de períodos
precos_futuros <- numeric(n + 1)  # Inicializa um vetor para os preços futuros
precos_futuros[1] <- P0  # O primeiro preço futuro é igual ao valor final de ITUB4

for (i in 1:n) {
  preco_atual <- precos_futuros[i]  # Preço atual
  retorno_atual <- ret.sim[i]       # Retorno atual
  preco_futuro <- preco_atual * exp(retorno_atual)  # Cálculo do próximo preço
  precos_futuros[i+1] <- preco_futuro               # Armazena o próximo preço no vetor
}

# ou, em uma única instrução ???? 
(precos_futuros = P0*exp(cumsum(c(0, ret.sim))))
# O código calcula os preços futuros de um ativo financeiro. Ele multiplica o
# preço inicial (P0) pela exponencial da soma acumulada dos retornos aleatórios.
# Isso simula o processo de acumulação de preços ao longo do tempo, assumindo
# que os retornos são continuamente compostos. O vetor c(0, ret.sim) é usado
# para garantir que o cálculo comece com o preço inicial. O resultado é
# atribuído à variável precos_futuros.

# (datas_futuras = seq(as.Date(END), by = "month", length.out = length(precos_futuros)))
(datas_futuras = seq(as.Date(END), by = "day", length.out = length(precos_futuros)))
plot(datas_futuras, precos_futuros, type = 'l', ylim = c(0, 40))
abline(h = P0, lty = 2, col='red')


#' o princípio da lista do COE...
#' um COE de um único ativo
conta1 = 0      # encerramento com o pagamento de 1 cupom
conta2 = 0      # encerramento com o pagamento de 2 cupons
conta3 = 0      # encerramento com o pagamento de 3 cupons
conta4 = 0      # encerramento com o pagamento de 4 cupons
conta0 = 0      # encerramento sem pagamento de cupons
# laço de Monte Carlo
(P0 = close.df[nrow(close.df), 'ABEV3'])
for (i in 1:1e5) {
  media = mu.vec['ABEV3']
  dp = sd.vec['ABEV3']
  ret.sim = rnorm(60, mean = media, sd = dp)
  pf = P0*exp(cumsum(ret.sim))
  if (pf[6] > P0) {
    conta1 = conta1 + 1
  } else if (pf[12] > P0) {
    conta2 = conta2 + 1
  } else if (pf[18] > P0) {
    conta3 = conta3 + 1
  } else if (pf[24] > P0) {
    conta4 = conta4 + 1
  } else {
    conta0 = conta0 + 1
  }
}
conta1/1e5
conta2/1e5
conta3/1e5
conta4/1e5
conta0/1e5

############################################################
# DEMO 2b
############################################################
# Simulação de Monte Carlo multivariável
# Simulando retornos mensais para ABEV3 e PETR4 usando o modelo CER
# mas dessa vez levando em conta as covariancias

library(mvtnorm)
ret.sim = rmvnorm(n = 60, mean = mu.vec, sigma = sigma.mat)
ret.sim = data.frame(ret.sim)

# comparando as séries originais com as geradas pela rmvnorm()
# ABEV3
layout(1:2)
plot(ret.sim$ABEV3, type = 'l', lwd = 2, col = 'blue', main = 'ABEV3 Simulação')
plot(ret.df$ABEV3, type = "l", lwd = 2, col = 'blue', main = 'ABEV3')
layout(1)
# ou...
ts.plot(cbind(ret.df$ABEV3, ret.sim$ABEV3), col=c('black', 'red'), lwd=2)

# PETR4
layout(1:2)
plot(ret.sim$PETR4, type = 'l', lwd = 2, col = 'blue', main = 'PETR4 Simulação')
plot(ret.df[, 'PETR4'], type = "l", lwd = 2, col = 'blue', main = 'PETR4')
layout(1)
# ou...
ts.plot(cbind(ret.df$PETR4, ret.sim$PETR4), col=c('black', 'red'), lwd=2)


############################################################
# DEMO 4
############################################################
# A evolução do log preço (slide 20)
# preço de ABEV3 no início do período
(P0 = close.df[1, 'ABEV3'])
# parâmetros amostrais
(mu.ABEV = mu.vec['ABEV3'])
(sd.ABEV = sd.vec['ABEV3'])
#' pelo modelo do retorno esperado constante, tirando da série de retornos
#' o retorno esperado, o que sobra são as notícias
(eps.ABEV = ret.df$ABEV3 - mu.ABEV)

# o crescimento esperado do log preço
# Definindo os pontos no tempo (0 a 60)
tempo <- 1:60

# O crescimento esperado
crescimento_esperado <- log(P0) + tempo * mu.ABEV

# Criando um gráfico de linha
plot(tempo, crescimento_esperado, type = 'l', ylab = '')

# Adicionando texto ao gráfico
texto_x <- 10  # Posição x do texto
texto_y <- 2.5  # Posição y do texto
texto_label <- expression(paste(ln, ' ', P[0])+t*mu)  # Rótulo com notação matemática
text(texto_x, texto_y, labels = texto_label, cex = 1.5)

# acrescentando a acumulação de notícias
# Calculando a série temporal
serie_temporal <- log(P0) + tempo * mu.ABEV + cumsum(eps.ABEV)

# Criando um gráfico de linha
ts.plot(cbind(serie_temporal, crescimento_esperado), col=c('black', 'red'), lwd=2)

# Adicionando texto ao gráfico
# http://vis.supstat.com/2013/04/mathematical-annotation-in-r/
texto_x <- 15
texto_y <- 2.8
texto_label <- expression(ln(P[t]) == ln(P[0]) + t * mu + sum(epsilon[s], s == 1, t))
text(texto_x, texto_y, labels = texto_label, cex = 1.5)

# transformando em preços de fechamento (slide 21)
# Calculando o crescimento esperado das cotações de fechamento
fechamento_esperado <- P0 * exp(tempo * mu.ABEV)

# Criando um gráfico de linha
plot(tempo, fechamento_esperado, type = 'l', ylab = '')

# Adicionando texto ao gráfico
texto_x <- 10
texto_y <- 13
texto_label <- expression(P[t] == paste(P[0], e^paste(t, mu)))  # Rótulo com notação matemática
text(texto_x, texto_y, labels = texto_label, cex = 1.5)

# Cálculo dos valores da série incluindo notícias
fechamento <- P0 * exp(tempo * mu.ABEV) * exp(cumsum(eps.ABEV))

# Criando um gráfico de linha
plot(tempo, fechamento, type = 'l', ylab = '')

# Adicionando texto ao gráfico
texto_x <- 10
texto_y <- 16
texto_label <- expression(P[t] == paste(P[0], e^paste(t, mu), e^sum(epsilon[s], s == 1, t)))  # Rótulo com notação matemática
text(texto_x, texto_y, labels = texto_label, cex = 1.5)

# para validação...
plot(close.df$ABEV3, type = 'l')

# estudo de cenários futuros
# Monte Carlo a partir do modelo do random walk

# Cria um gráfico vazio
plot(tempo, type = 'n', ylab = 'Preço', ylim = c(5, 40))
# Número de simulações
num_simulacoes <- 5
# Loop para realizar 5 simulações
(P0 = close.df[nrow(close.df), 'ABEV3'])
for (simulacao in 1:num_simulacoes) {
  # Gera valores aleatórios para as notícias de acordo com uma distribuição normal multivariada
  eps_sim <- rmvnorm(n = 60,  sigma = sigma.mat)
  # Nomeia as colunas da matriz de valores aleatórios
  colnames(eps_sim) <- colnames(sigma.mat)
  # Seleciona apenas a coluna 'ABEV3'
  eps_sim <- eps_sim[, 'ABEV3']
  # Calcula os preços futuros usando o modelo do Random Walk (slide 21)
  precos <- P0 * exp(tempo * mu.ABEV) * exp(cumsum(eps_sim))
  # Plota a simulação no gráfico
  lines(tempo, precos, col = simulacao)
}

# aumentando o número de simulações
# Cria um gráfico vazio
plot(tempo, type = 'n', ylab = 'Preço', ylim = c(5, 40))
# Número de simulações
num_simulacoes <- 500
# Loop para realizar 5 simulações
for (simulacao in 1:num_simulacoes) {
  # Gera valores aleatórios para as notícias de acordo com uma distribuição normal multivariada
  eps_sim <- rmvnorm(n = 60, sigma = sigma.mat)
  # Nomeia as colunas da matriz de valores aleatórios
  colnames(eps_sim) <- colnames(sigma.mat)
  # Seleciona apenas a coluna 'ABEV3'
  eps_sim <- eps_sim[, 'ABEV3']
  # Calcula os preços futuros usando o modelo do Random Walk (slide 21)
  precos <- P0 * exp(tempo * mu.ABEV) * exp(cumsum(eps_sim))
  # Plota a simulação no gráfico
  lines(tempo, precos, col = simulacao)
}

# Definindo o tempo
t <- seq(1, 60, by = 1)
t = 1:60
# Calculando o crescimento esperado de AMBEV
crescimento_esperado <- P0 * exp(t * mu.ABEV)
# Plotando o crescimento esperado (linha preta)
lines(t, crescimento_esperado, type = 'l', lwd = 3, ylim = c(min(crescimento_esperado), max(crescimento_esperado)))
# Calculando os intervalos de confiança
upper_bound <- P0 * exp(t * mu.ABEV + 2 * sqrt(t) * sd.ABEV)
lower_bound <- P0 * exp(t * mu.ABEV - 2 * sqrt(t) * sd.ABEV)
# Plotando o intervalo de confiança (linhas vermelhas)
lines(t, upper_bound, col = 'red', lwd = 3)
lines(t, lower_bound, col = 'red', lwd = 3)
# Adicionando uma legenda
legend('topright', legend = c('Crescimento Esperado', 'Intervalo de Confiança'),
       col = c('black', 'red'), lwd = c(3, 3))

############################################################
# DEMO 5
############################################################
# Simulação da incerteza na média usando Monte Carlo
num_simulacoes <- 10000
# um vetor para armazenar as médias
media_amostral <- numeric(num_simulacoes)
#' Realiza 10.000 simulações da média amostral
#' os valores mean = 0.008 e sd = 0.03 são arbitrários
for (i in 1:num_simulacoes) {
  amostra <- rnorm(n = 60, mean = 0.008, sd = 0.03)
  media_amostral[i] <- mean(amostra)
}
# Plota um histograma das médias amostrais
hist(media_amostral, col = 'lightblue', breaks = 30, main = 'Distribuição das Médias Amostrais')

# Calcula o valor esperado da média amostral
media_esperada <- mean(media_amostral)
cat('Valor Esperado da Média Amostral:', media_esperada, '\n')

# Calcula o erro padrão (standard error) da média amostral
erro_padrao <- sd(media_amostral)
cat('Erro Padrão da Média Amostral:', erro_padrao, '\n')

#' não confunda o standard error com o standard deviation
#' o standard error eu posso reduzir se eu usar amostras grandes, 
#' já O standard deviation é uma característica do fenômeno

# É possível calcular o erro padrão da média amostral analiticamente
erro_padrao_analitico <- 0.03 / sqrt(60)
cat('Erro Padrão da Média Amostral (Analítico):', erro_padrao_analitico, '\n')


############################################################
# DEMO 6
############################################################
# Estimando os erros padrão (SE)

# Calculando o número de observações a partir dos dados
nobs <- nrow(ret.df)
cat('Número de Observações (nobs):', nobs, '\n')

# Calculando o valor esperado dos retornos
(muhat.vec <- apply(ret.xts, 2, mean))

# Calculando o desvio padrão dos retornos
(sdhat.vec <- apply(ret.xts, 2, sd))

# Calculando a variancia dos retornos
(sigma2hat.vec <- apply(ret.xts, 2, var))

# Calculando as correlações entre os retornos
corhat.mat <- cor(ret.xts)
# Extraindo os valores das correlações da matriz (triangular inferior)
(rhohat.vec <- corhat.mat[lower.tri(corhat.mat)])
# Criando pares de ativos para as correlações
(pares <- t(combn(codigos, 2)))
cat('Pares de Ativos para Correlações:', pares, '\n')
# Criando um dataframe com os pares de ativos e suas correlações
rhohat.df <- data.frame(pares, rhohat.vec)
colnames(rhohat.df) <- c('Ativo1', 'Ativo2', 'Correlação')
cat('Correlações entre Pares de Ativos:', '\n')
(rhohat.df)

# Cálculo dos erros padrão para a média dos retornos (slide 36)
(se.muhat <- sdhat.vec / sqrt(nobs))

# Cálculo dos erros padrão para a volatilidade dos retornos
(se.sdhat <- sdhat.vec / sqrt(2 * nobs))

# Cálculo dos erros padrão para a variancia dos retornos
(se.sigma2hat <- sigma2hat.vec / sqrt(nobs/2))

cat('Razão entre SE da volatilidade e volatilidade:', se.sdhat / sdhat.vec)
# por que eles são todos iguais?

# um gráfico interessante...
# Função para desenhar uma elipse
draw_ellipse <- function(a, b, xc, yc, ...) {
  # 'a' é o comprimento do eixo paralelo ao eixo x
  # 'b' é o comprimento do eixo paralelo ao eixo y
  # 'xc' e 'yc' são as coordenadas do centro da elipse
  # '...' são quaisquer argumentos que podem ser passados para a função 'points'
  
  # Cria um vetor 't' com ângulos de 0 a 2*pi para desenhar a elipse
  t <- seq(0, 2*pi, by=pi/400)
  
  # Calcula as coordenadas dos pontos da elipse
  xt <- xc + a * cos(t)
  yt <- yc + b * sin(t)
  
  # Desenha a elipse no gráfico
  points(xt, yt, ...)
}

# Cria um gráfico de dispersão dos retornos
plot(sdhat.vec, muhat.vec, pch = 16,
     ylab = expression(hat(mu)),
     xlab = expression(hat(sigma)),
     xlim = c(0, 0.16),
     ylim = c(-0.03, 0.05))

# Adiciona rótulos aos pontos no gráfico
text(sdhat.vec, muhat.vec, labels = codigos, pos = 4)

# Desenha elipses de intervalo de confiança
for (i in 1:length(codigos)) {
  draw_ellipse(a = 2 * se.sdhat[i], b = 2 * se.muhat[i], sdhat.vec[i], muhat.vec[i], col = i)
}

# Cálculo das Retas de Sharpe

# Definir a taxa de referência (rf) em formato decimal
rf <- 0.8133 / 100   # A taxa Selic média no período

# Agora, vamos desenhar as Retas de Sharpe no gráfico
# Cada linha representa um ativo diferente (1 a 6)
# As Retas de Sharpe ajudam a avaliar o desempenho dos ativos em relação à taxa de referência

# Abaixo, estamos usando um loop para calcular e desenhar as linhas de Sharpe
# A variável 'a' é a taxa de referência e 'b' é a inclinação da linha de Sharpe
# 'col' define a cor da linha e 'lty' define o estilo da linha (neste caso, linha tracejada)
for (i in 1:6) {
  # Calculando a inclinação 'b' da Reta de Sharpe para cada ativo
  b <- (muhat.vec[i] - rf) / sdhat.vec[i]
  
  # Agora, vamos desenhar a Reta de Sharpe no gráfico
  abline(a = rf, b = b, col = i, lty = 3)
}

############################################################
# DEMO 7
############################################################
# Intervalos de confiança de 95% para a média
mu.lower = muhat.vec - 2 * se.muhat
mu.upper = muhat.vec + 2 * se.muhat
mu.width = mu.upper - mu.lower
cbind(mu.lower, mu.upper, mu.width)

# intervalos de confiança de 95% para a volatilidade
sd.lower = sdhat.vec - 2 * se.sdhat
sd.upper = sdhat.vec + 2 * se.sdhat
sd.width = sd.upper - sd.lower
cbind(sd.lower, sd.upper, sd.width)

# intervalos de confiança de 95% para a correlação
rho.lower = rhohat.vec - 2 * se.rhohat
rho.upper = rhohat.vec + 2 * se.rhohat
rho.width = rho.upper - rho.lower
cbind(rho.lower, rho.upper, rho.width)

# intervalos de confiança largos -> estimativa imprecisa

############################################################
# DEMO 8
############################################################
# Simulação de Monte Carlo para a determinação dos erros padrão

# Carregando bibliotecas necessárias
library(mvtnorm)

# Definindo o número de observações e simulações
(n_observacoes = nrow(ret.df))
n_simulacoes = 100000  # Vamos realizar 100.000 simulações

#' Vamos calcular as Estimativas de Erro Padrão (SE) 
#' para a média, variância e desvio padrão 
#' por meio de simulações e compará-las com os valores teóricos.

# Criando matrizes para armazenar os resultados das simulações
media_simulada = matrix(data = 0, nrow = n_simulacoes, ncol = length(codigos))
variancia_simulada = matrix(data = 0, nrow = n_simulacoes, ncol = length(codigos))
desvio_padrao_simulado = matrix(data = 0, nrow = n_simulacoes, ncol = length(codigos))

# Realizando a Simulação de Monte Carlo
for (simulacao in 1:n_simulacoes) {
  retornos_simulados = rmvnorm(n = n_observacoes, mean = muhat.vec, sigma = sigma.mat)
  media_simulada[simulacao, ] = apply(retornos_simulados, 2, mean)
  variancia_simulada[simulacao, ] = apply(retornos_simulados, 2, var)
  desvio_padrao_simulado[simulacao, ] = apply(retornos_simulados, 2, sd)
}

# Calculando o Erro Padrão da Média e comparando com o valor teórico
erro_padrao_media_simulada = apply(media_simulada, 2, sd)
resultados_media = rbind(
  'Erro Padrão MC' = erro_padrao_media_simulada, 
  'Erro Padrão Teórico' = se.muhat)
(resultados_media)

# Calculando o Erro Padrão da Variância e comparando com o valor teórico
erro_padrao_variancia_simulada = apply(variancia_simulada, 2, sd)
resultados_variancia = rbind(
  'Erro Padrão MC' = erro_padrao_variancia_simulada, 
  'Erro Padrão Teórico' = se.sigma2hat)
(resultados_variancia)

# Calculando o Erro Padrão do Desvio Padrão e comparando com o valor teórico
erro_padrao_desvio_padrao_simulado = apply(desvio_padrao_simulado, 2, sd)
resultados_desvio_padrao = rbind(
  'Erro Padrão MC' = erro_padrao_desvio_padrao_simulado, 
  'Erro Padrão Teórico' = se.sdhat)
(resultados_desvio_padrao)

# Plotando Histogramas para ABEV3
layout(mat = matrix(1:4, 2, 2, byrow = TRUE))
hist(media_simulada[, 1], breaks = 30, col = 'slateblue', main = 'Média Simulada')
hist(variancia_simulada[, 1], breaks = 30, col = 'slateblue', main = 'Variância Simulada')
hist(desvio_padrao_simulado[, 1], breaks = 30, col = 'slateblue', main = 'Desvio Padrão Simulado')
layout(1)


# para estudar em casa...
# Cálculo do Erro Padrão da Correlação usando Simulação de Monte Carlo

# Carregando bibliotecas necessárias
library(mvtnorm)

# Definindo o número de simulações
n_simulacoes = 10000  # Vamos realizar 10.000 simulações

# Vamos calcular o Erro Padrão da Correlação por meio de simulações e compará-lo com o valor teórico.

# Criando uma matriz para armazenar os resultados das simulações de correlação
correlacao_simulada = matrix(0, n_simulacoes, choose(length(codigos), 2))

# Função para combinar os nomes dos ativos em pares
combina = function(pares) {
  paste(pares[1, ], pares[2, ], sep = '/')
}

# Nomeando as colunas da matriz com os nomes dos pares de ativos
colnames(correlacao_simulada) = combina(combn(codigos, 2))

# Realizando as Simulações de Monte Carlo
for (simulacao in 1:n_simulacoes)  {
  retornos_simulados = rmvnorm(n = n_observacoes, mean = muhat.vec, sigma = sigma.mat)
  correlacao_auxiliar = cor(retornos_simulados)
  correlacao_simulada[simulacao, ] = correlacao_auxiliar[lower.tri(correlacao_auxiliar)]
}

# Tabulando os Resultados
# - Correlações amostrais
# - Média das correlações simuladas por MC
# - Erro Padrão amostral
# - Erro Padrão teórico
resultados_correlacao = rbind(
  rhohat.vec,  # Correlações amostrais
  apply(correlacao_simulada, 2, mean),       # Média das correlações por MC
  apply(correlacao_simulada, 2, sd),         # Erro Padrão amostral
  (1 - rhohat.vec^2) / sqrt(n_observacoes)   # Erro Padrão teórico
)
rownames(resultados_correlacao) = c('Correlações amostrais', 'Correlações MC', 'Erro Padrão MC', 'Erro Padrão Teórico')
(resultados_correlacao)

# Plotando Histogramas das Correlações Simuladas
for (i in 1:choose(length(codigos), 2)) {
  hist(correlacao_simulada[, i], col = 'slateblue1', main = colnames(correlacao_simulada)[i])
}


############################################################
# DEMO 9
############################################################
# Estimando o Valor em Risco (VaR) no modelo CER

# Definindo o nível de confiança desejado (5%)
nivel_confianca = 0.05

# Calculando o quantil a 5% para cada ativo supondo uma distribuição normal
quantil_05 = qnorm(p = nivel_confianca, mean = muhat.vec, sd = sdhat.vec)

# Definindo o valor inicial do portfólio
W0 = 100000

# Calculando o VaR a 5% 
(VaR_05 = (exp(quantil_05) - 1) * W0)

#' Alternativamente, podemos calcular o VaR usando quantis, 
#' o que é melhor quando a distribuição não é normal
quantil_05 = apply(ret.xts, 2, quantile, prob = nivel_confianca)

# Calculando o VaR a 5% 
(VaR_05 = (exp(quantil_05) - 1) * W0)


############################################################
# DEMO 10
############################################################
# Calculando o Standard Error (SE) no VaR (Value at Risk) usando Monte Carlo

# Definindo o número de simulações e observações
numero_simulacoes = 100000
(numero_observacoes = nrow(ret.df))

#' Calculando o VaR a 5% a partir de uma única amostra
#' supondo uma distribuição normal dos retornos
quantil_05 = qnorm(p = 0.05, mean = muhat.vec, sd = sdhat.vec)

# Definindo o valor inicial do portfólio
W0 = 100000

# Calculando o VaR a 5%
(VaR_05 = (exp(quantil_05) - 1) * W0)

# Determinando agora o SE do VaR
# Alocando espaço para armazenar os resultados das simulações Monte Carlo
VaR.mat.MC = matrix(data = 0, nrow = numero_simulacoes, ncol = length(codigos))
colnames(VaR.mat.MC) = codigos

# Loop de Monte Carlo para simulações
for (i in 1:numero_simulacoes)  {
  # Gerando retornos simulados
  ret.sim = rmvnorm(n = numero_observacoes, mean = muhat.vec, sigma = sigma.mat)
  
  # Calculando a média e desvio padrão dos retornos simulados
  mu.vec.MC = apply(ret.sim, 2, mean)
  sd.vec.MC = apply(ret.sim, 2, sd)
  
  #' Calculando o quantil a 5% com base nos retornos simulados
  #' Distribuição normal
  quantil_05.vec = qnorm(p = 0.05, mean = mu.vec.MC, sd = sd.vec.MC)
  
  # Calculando o VaR a 5% para cada ativo no portfólio
  VaR.mat.MC[i, ] = (exp(quantil_05.vec) - 1) * W0
}

SE.VaR.05 = apply(VaR.mat.MC, 2, sd)

# Tabulando os resultados
resultados = rbind(
  'VaR.05 amostral' = VaR_05,                        # VaR amostral
  'VaR.05 médio MC' = apply(VaR.mat.MC, 2, mean),    # VaR médio, Monte Carlo
  'SE VaR.05 MC' =  SE.VaR.05                        # Standard Error do VaR, Monte Carlo
)
resultados

# Calculando intervalos de confiança a 95%
intervalos_confianca = rbind(
  'Limite inferior' = VaR_05 - 2 * SE.VaR.05,   
  'Limite superior' = VaR_05 + 2 * SE.VaR.05
)
intervalos_confianca

############################################################
# DEMO 11 - Drawdowns
############################################################
# Crie um vetor com os preços da série temporal
precos <- close.df$ITUB4

# Inicialize variáveis para armazenar o máximo drawdown em termos percentuais
max_drawdown <- 0
valor_maximo <- -Inf

# Itere pelos preços
for (i in 1:length(precos)) {
  preco = precos[i]
  # Se o preço atual for maior que o valor máximo anterior, atualize o valor máximo
  if (preco > valor_maximo) {
    valor_maximo <- preco
    inicio_drawdown = i
  } else {
    # Caso contrário, calcule o drawdown percentual e verifique se é o máximo
    drawdown_percentual <- ((valor_maximo - preco) / valor_maximo) * 100
    if (drawdown_percentual > max_drawdown) {
      max_drawdown <- drawdown_percentual
      from = inicio_drawdown
      to = i
    }
  }
}
max_drawdown

# gráfico
plot(precos, type = 'l', main = 'ITUB4')
segments(from, precos[from], to, precos[from], col="red")
segments(from, precos[to], to, precos[to], col="red")
mid <- (from + to)/2
arrows(mid, precos[from], mid, precos[to], col="red", length = 0.16)

# gráfico underwater
x = xts(exp(ret.df$ITUB4)-1, order.by = ret.df$data)
PerformanceAnalytics::charts.PerformanceSummary(x)
PerformanceAnalytics::chart.Drawdown(x)

PainIndex(ret.xts)

#############################################
# trechos de código REMOVIDOS
#############################################

# macete... extraindo os valores das correlações da matriz de correlacoes
cor.vec = cor.mat[lower.tri(cor.mat)]
cor.vec

# como rapidamente dar nomes às dimensões?
pares = t(combn(codigos, 2))
pares
cor.df = data.frame(pares, cor.vec)
colnames(cor.df) = c('at1', 'at2', 'corr')
cor.df

# da linha 145
# hipótese: o valor esperado do retorno e a volatilidade são constantes
# itub4
plot(ret.df$data, ret.df$ITUB4, 
     type = "l", lwd=2,
     main = 'ITUB4',
     ylab = expression(mu[ITUB4]),
     xlab = 'ano')
# petrobrás
plot(ret.df$data, ret.df$PETR4, 
     type = "l", lwd=2,
     main = 'PETR4',
     ylab = expression(mu[PETR4]),
     xlab = 'ano')


############################################################
# DEMO 1a
############################################################
# Gerador de números aleatórios com distribuição normal
aux = qnorm(runif(1e6))
hist(aux, col = 'lightblue', breaks = 100, freq = F)
# em Excel
# NORM.INV é o equivalente ao qnorm()
# NORM.DIST é o equivalente ao pnorm()
# =NORM.INV(RAND();0;1)

# Como funciona a rmvnorm? ############################
# decompondo Sigma nas matrizes E e Lambda
aux = eigen(sigma.mat)
E = aux$vectors
Lambda = diag(aux$values)
# verificando
sigma.mat
E %*% Lambda %*% t(E)

# gerando a matriz V
m = 1e7
n = length(codigos)
V = matrix(rnorm(m*n), m, n)

# hipótese a matriz V tem vetor de médias igual a 0
# e matriz de correlação igual a identidade
apply(V, 2, mean)
cor(V)
# ou, mais elegante...
aux = cor(V)
ifelse(abs(aux) < 1e-3, 0, aux)
# ou...
aux[abs(aux) < 1e-3] = 0
aux

# como transformar a matriz V em uma matriz de séries que tem as propriedades estatísticas da série histórica?
X = t(E %*% sqrt(Lambda) %*% t(V))
head(X)
# calculando as propriedades estatísticas da matriz X
sigma.mat
cov(X)
abs(sigma.mat - cov(X)) > 1e-3

# no entanto,
mu.vec
apply(X, 2, mean)

# para corrigir as médias...
# qual é o comportamento quando eu somo uma matriz e um vetor?
matrix(0, 3, 3) + c(1, 2, 3)
# cada elemento do vetor é somado a todos os elementos na linha correspondente.
# Logo...
X = t(t(X) + mu.vec)

# verificando mais uma vez...
mu.vec
apply(X, 2, mean)

sigma.mat
cov(X)

cor(ret.df)
cor(X)

# mas, é mais fácil fazer com o rmvnorm...
#######################


############################################################
# DEMO 3
############################################################
# ver o arquivo covmat.mw e o slide 23
eps.mat = t(t(ret.xts)-mu.vec)
t(eps.mat)%*%eps.mat/(nrow(eps.mat)-1)
sigma.mat

# construíndo a matriz de covariâncias no Excel
# https://www.youtube.com/watch?v=EAf-3vO2A0I
# write.csv2(x = ret.df, "d:/temp/temp.csv")
xlsx::write.xlsx(
  x = ret.df, 
  file = "d:/temp/temp.xlsx", 
  row.names = F)

# abrir o arquivo com o Excel

# copiando a matriz de covariancias para o clipboard...
write.table(
  x=sigma.mat, 
  file = 'clipboard-16384', 
  sep = '\t', 
  dec = ',',
  row.names = F, 
  col.names = F)


# SE da variância (slide 35)
sigma2hat.vec = diag(sigma.mat)
se.sigma2hat = sigma2hat.vec/sqrt(nobs/2)
rbind(sigma2hat.vec, se.sigma2hat)

# SE da correlação (slide 34)
rhohat.vec = cor(ret.xts)[lower.tri(cor(ret.xts))]
se.rhohat = (1 - rhohat.vec^2)/sqrt(nobs)
rbind(rhohat.vec, se.rhohat/rhohat.vec*100)


# um esquenta...
dentro = NULL
for (i in 1:1e5) {
  amostra = rnorm(n = 60, mean = 1e-2, sd = 5e-2)
  mu = mean(amostra)
  se = sd(amostra)/sqrt(60)
  dentro[i] = (mu-1.959964*se)<1e-2 & (mu+1.959964*se)>1e-2
}
sum(dentro)/1e5



