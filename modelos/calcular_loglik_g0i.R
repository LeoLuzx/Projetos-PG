
# FUNÇÃO PARA CALCULAR A LOG-VEROSSIMILHANÇA DA DISTRIBUIÇÃO g0i

# Sem valores padrão para os parâmetros para que o usuários seja obrigado e não esquecer

log_likelihood_g0i <- function(Xobs, alpha, gamma, L){
  
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
  
  # ----- CONSTRUÇÃO DA LOG-VERO DA g0i -----
  
  # Tamanho da amostra
  n <- length(Xobs)
  
  #Parcela constante da log-verossimilhança
  const <- n * L * log(L) + n * log(gamma(L - alpha)) - alpha * n * log(gama) - n * log(gamma(-alpha)) - n * log(gamma(L))
  
  #Parcela variável da log-verossimilhança
  var <- (L - 1) * sum(log(Xobs)) - (L - alpha) * sum(log(gama + L * Xobs))
  
  # ----- RETORNO COMO VALOR ÚNICO -----
  
  #Resultado (somas das duas parcelas)
  res <- (const + var)
  
  #Retorno do resultado
  return(res)
  
}
