
# GERADOR DE VALORES ALEATÓRIOS DA G0I

# Gera uma amostra de tamanho n da distribuição g0i
rG0I = function(n, alpha, gama, L){
  
  # ----- SANITIZAÇÃO DE ALFA -----
  
  # Sanetização do parâmetro alfa
  if (alpha >= -1)
    stop("O parâmetros alpha da g0i deve ser menor que -1.")
  
  # ----- SANETIZAÇÃO DO GAMA -----
  
  # Sanetização do parâmetro gama
  if (gamma <= 0)
    stop("O parâmetro gamma da g0i deve ser positivo.")
  
  # ----- SANETIZAÇÃO DO PARÂMETRO L -----
  
  # Verificando se L é finito
  if (!is.finite(L)) {
    stop("L deve ser um número finito.")
  }
  
  # Verificando se L é positivo
  if (L <= 0) {
    stop("L deve ser maior que zero.")
  }
  
  # Verificando se L é inteiro
  if (L != floor(L)) {
    stop("L deve ser um número inteiro.")
  }
  
  # ----- SANETIZAÇÃO DO TAMNHO AMOSTRAL -----
  
  # Verificando se n é inteiro
  if (n != floor(n)) {
    stop("O tamnho amostral n deve ser um número inteiro.")
  }
  
  # Verificando se n é positivo
  if (n <= 0) {
    stop("O tamanho amaostral n deve ser um valor positivo.")
  }
  
  # Verificando se n não é NA, NaN, Inf etc
  if (!is.finite(n)) {
    stop("O tamanho amostral n deve ser um número finito")
  }
  
  #Gerando a g0i pela definição como a razão de duas gamas
  res = rgamma(n,L,L) / rgamma(n,alpha,gama);
  
  return(res)
  
}
#---------------------------------------------------------------------------------------------------