library(zoo)
library(quantmod)
library(tseries)
library(PerformanceAnalytics)
library(tidyverse)

############################################################
# DEMO 1
############################################################

# Itau Unibanco Holding SA
# Vale SA
# Bradesco
# Ambev
# B3
# Petrobr?s

START = "2014-09-01"
END = "2018-05-30"
codigos = c("PETR4", "VALE3", "ITUB4", "BBAS3")

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



#plot(close.df$ABEV3, main = 'ABEV3', ylab = 'Fechamento Ajustado', type='l', lwd = 2)


# Calculando os retornos mensais cc
ret.df = apply(X = log(close.df), MARGIN = 2, FUN = diff)
#ret.df %>% head()

# ret.df do tipo matrix. Convertendo para um data.frame:
ret.df = 
  ret.df %>% 
  data.frame() %>% 
  rownames_to_column('data') %>% 
  mutate(data = as.Date(data))



ret.xts = xts(x = ret.df[, -1], order.by = ret.df$data)


# o valor esperado dos retornos
(mu.vec = apply(ret.xts, 2, mean))

# a volatilidade dos ativos 
(sd.vec = apply(ret.xts, 2, sd))



simula_coe <- function(close_df, mu_vec, sd_vec, ativo, n_sim = 1e5, periodos = 756) {
  # Inicializar contadores
  conta1 <- 0
  conta2 <- 0
  conta3 <- 0
  conta4 <- 0
  conta5 <- 0
  conta6 <- 0
  conta0 <- 0
  
  # Obter o preço inicial do ativo
  P0 <- close_df[nrow(close_df), ativo]
  
  # Laço de Monte Carlo
  for (i in 1:n_sim) {
    media <- mu_vec[ativo]
    dp <- sd_vec[ativo]
    ret.sim <- rnorm(periodos, mean = media, sd = dp)
    pf <- P0 * exp(cumsum(ret.sim))
    
    if (pf[6 * 21] > P0) {
      conta1 <- conta1 + 1
    } else if (pf[12 * 21] > P0) {
      conta2 <- conta2 + 1
    } else if (pf[18 * 21] > P0) {
      conta3 <- conta3 + 1
    } else if (pf[24 * 21] > P0) {
      conta4 <- conta4 + 1
    } else if (pf[30 * 21] > P0) {
      conta5 <- conta5 + 1
    } else if (pf[36 * 21] > P0) {
      conta6 <- conta6 + 1
    } else {
      conta0 <- conta0 + 1
    }
  }
  
  # Retornar os resultados como proporções
  return(list(
    conta1 = conta1 / n_sim,
    conta2 = conta2 / n_sim,
    conta3 = conta3 / n_sim,
    conta4 = conta4 / n_sim,
    conta5 = conta5 / n_sim,
    conta6 = conta6 / n_sim,
    conta0 = conta0 / n_sim
  ))
}



sim.PETR <- simula_coe(close_df = close.df, mu_vec = mu.vec, sd_vec = sd.vec, ativo = 'PETR4')
sim.VALE <- simula_coe(close_df = close.df, mu_vec = mu.vec, sd_vec = sd.vec, ativo = 'VALE3')
sim.ITUB <- simula_coe(close_df = close.df, mu_vec = mu.vec, sd_vec = sd.vec, ativo = 'ITUB4')
sim.BBAS <- simula_coe(close_df = close.df, mu_vec = mu.vec, sd_vec = sd.vec, ativo = 'BBAS3')

prob_conj_6  <-  sim.PETR$conta1 * sim.VALE$conta1 * sim.ITUB$conta1 * sim.BBAS$conta1
prob_conj_12 <- sim.PETR$conta2 * sim.VALE$conta2 * sim.ITUB$conta2 * sim.BBAS$conta2
prob_conj_18 <- sim.PETR$conta3 * sim.VALE$conta3 * sim.ITUB$conta3 * sim.BBAS$conta3
prob_conj_24 <- sim.PETR$conta4 * sim.VALE$conta4 * sim.ITUB$conta4 * sim.BBAS$conta4
prob_conj_30 <- sim.PETR$conta5 * sim.VALE$conta5 * sim.ITUB$conta5 * sim.BBAS$conta5
prob_conj_36 <- sim.PETR$conta6 * sim.VALE$conta6 * sim.ITUB$conta6 * sim.BBAS$conta6
prob_conj_loser <- sim.PETR$conta0 * sim.VALE$conta0 * sim.ITUB$conta0 * sim.BBAS$conta0

print(paste("A probabilidade conjunta em 6 meses: ", prob_conj_6 * 100, "%"))
print(paste("A probabilidade conjunta em 12 meses: ", prob_conj_12 * 100, "%"))
print(paste("A probabilidade conjunta em 18 meses: ", prob_conj_18 * 100, "%"))
print(paste("A probabilidade conjunta em 24 meses: ", prob_conj_24 * 100, "%"))
print(paste("A probabilidade conjunta em 30 meses: ", prob_conj_30 * 100, "%"))
print(paste("A probabilidade conjunta em 36 meses: ", prob_conj_36 * 100, "%"))
print(paste("A probabilidade conjunta de perder: ", prob_conj_loser * 100, "%"))




