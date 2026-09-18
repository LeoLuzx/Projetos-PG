
# GRADIENTE DA LOG-VEROSSIMILHANÇA DA g0i

#Define o gradiente da log-verossimilhança
calcular_grad_loglik_g0i = function(Xobs, alpha, gamma, L){
  
  # ----- SANETIZAÇÃO DA AMOSTRA -----
  
  # Se os dados não forem numéricos, o R vai coagir o restante num tipo de dado diferente
  if (!is.numeric(Xobs))
    stop("Xobs na log-lik da g0i deve ser um vetor numérico.")
  
  # Se os dados forem um vetor vazio
  if (length(Xobs) == 0)
    stop("Xobs na log-lik da g0i deve ser um vetor não vazio.")
  
  # Se os dados tiverem valores NA, NaN ou Inf.
  if (any(!is.finite(Xobs)))
    stop("Xobs na log-lik da g0i não pode conter NA, NaN ou Inf.")
  
  # Se os dados não forem positivos
  if (any(Xobs <= 0))
    stop("Xobs na log-lik da g0i deve conter apenas valores positivos.")
  
  # ----- SANETIZAÇÃO DE ALFA -----
  
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
  
  # ----- CONSTRUÇÃO DO GRADIENTE DA LOG-VERO DA g0i -----
  
  #Tamanho da amostra
  n = length(Xobs)
  
  # Derivada da log-verossimilhança em relação a alfa
  dalpha <- - n * log(gama)  - n * digamma(L - alpha) + n * digamma(-alpha) + sum(log(gama + L * Xobs))
    
  #Derivada da log-verossimilhança em relação a gama
  dgamma <- - n * alpha / gama + (alpha - L) * sum(1 / (gama + L * Xobs) )
    
  # ----- RETORNO COMO UM VETOR -----
  
  return(c(dalpha, dgamma))
  
}
