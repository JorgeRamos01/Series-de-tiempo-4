remove(list = ls())

# paqueterIas
library(vars)
library(portes)
library(tseries)

set.seed(1234)

Monte.Carlo<-function(T=100, C=100, K=2, p=2, R=500, alpha=0.05){
  # simulamos algunos coeficientes
  Phi <- runif(K*K*p, -0.5, 0.5)
  
  # coeficientes VAR(2)
  Phi  <- array(Phi, dim = c(K, K, p))
  
  est <- c("maxroot", "AIC", "SC", "HQ", "serial", "arch",
           paste("Series",1:p, sep = ""))
  resultados <- matrix(0, R, length(est))
  colnames(resultados) <- est
  
  for(i in 1 : R){ # print(i)
    # simulaciOn
    Y <- varima.sim(model=(list(ar = Phi)), n = T + C, k = K,
                   innov.dist="t",dft=5)
    
    # quitamos las primeras series
    Y <- Y[-(1:C),]
    
    # rezago
    k <- VARselect(Y, type = "none")
    resultados[i,c("AIC", "SC", "HQ")] <- k$selection[c("AIC(n)", "SC(n)",
                                                        "HQ(n)")]
    
    # estimaciOn
    var1 <- VAR(Y, p = p, type = "none")
    
    # estacionariedad
    resultados[i, "maxroot"] <- max(roots(var1))
    
    # pruebas de residuales
    resultados[i,"serial"] <- serial.test(var1)$serial$p.value
    resultados[i,"arch"] <- arch.test(var1)$arch$p.value
    
    # causalidad
    for(j in 1 : p){
      resultados[i,paste("Series",j, sep = "")] <- c(causality(var1,
                                                               cause = paste("Series.",j,
                                                                             sep = ""))$Granger$p.value)
    }
    
  }
  
  # observamos los resultados
  print(sum(resultados[,"maxroot"] < 1)/R)
  print(colSums(resultados[, c("AIC", "SC", "HQ")] == p)/R)
  print(colSums(resultados[,c("serial", "arch")] > alpha)/R)
  print(colSums(resultados[,paste("Series",1:p, sep = "")] < alpha)/R)
}
