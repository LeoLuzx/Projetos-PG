
# FUNÇÃO PARA CALCULAR A LOG-VEROSSIMILHANÇA DA DISTRIBUIÇÃO g0i

# Sem valores padrão para os parâmetros para que o usuários seja obrigado e não esquecer

log_likelihood_g0i <- function(Xobs, alfa, gama, L){
  
  # ----- VALIDAÇÃO DA AMOSTRA -----
  
  # Ser numérico já garante que não é vazio, pois is.numeric(c()) retorna FALSE. No entanto, essa
  # parte é mantida por questões de clareza semântica e comunicação do problema caso ocorra.
  if (length(Xobs) == 0)
    stop("Xobs na log-verossimilhança da g0i deve ser um vetor não vazio.")
  
  # Ser numérico não garante que seja um vetor, pois matrizes e outras estruturas de valores reais 
  # também podem ser do tipo numérico. Por isso, verificamos aqui a dimensão do objeto para garantir
  # que ele seja um vetor que deve ter dimensão 'NULL'.
  if (!is.null(dim(Xobs)))
    stop("Xobs na log-verossimilhança da g0i deve ser um vetor.")
  
  # Se os dados não forem numéricos, o R vai coagir o restante num tipo de dado diferente que está 
  # na parte mais acima na hierarquia dos dados e isso vai acusar que o vetor não é numérico já que
  # 'complex' e 'character' são tipos de dados de hierarquia mais alta. Os ipos de dados 'double' e 
  # 'integer' são numéricos e podem passar por esse nível pois serão coagidos a 'double'. 
  if (!is.numeric(Xobs))
    stop("Xobs na log-verossimilhança da g0i deve ter apenas dados numéricos.")
  
  # Se os dados tiverem valores NA, NaN ou Inf
  if (any(!is.finite(Xobs)))
    stop("Xobs na log-verossimilhança da g0i não pode conter NA's, NaN's ou Inf's.")
  
  # Se os dados não forem todos positivos
  if (any(Xobs < 0))
    stop("Xobs na log-verossimilhança da g0i não deve conter valores negativos.")
  
  # Se os dados não forem todos positivos
  if (any(Xobs == 0))
    stop("Xobs na log-verossimilhança da g0i não deve conter zeros.")
  
  # ----- VALIDAÇÃO DO PARÂMETRO ALFA -----
  
  # Deve ser um valor numérico (double ou integer)
  if (!is.numeric(alfa))
    stop("O parâmetro alfa na log-verossimilhança da g0i deve ser um valor numérico.")
  
  # Deve ser um valor único, não podendo aceitar um vetor (nem mesmo vazio)
  if (length(alfa) != 1)
    stop("O parâmetro alfa na log-verossimilhança da g0i deve ser um valor único.")
  
  # Deve ser um número finito. Não pode conter NA's, NaN's nem Inf's
  if (!is.finite(alfa))
    stop("O parâmetro alfa na log-verossimilhança da g0i não pode ser NA, NaN ou Inf.")
  
  # Deve ser menor do que -1 (pela teoria)
  if (alfa >= -1)
    stop("O parâmetros alfa na log-verossimilhança da g0i deve ser menor do que -1.")
  
  # ----- VALIDAÇÃO DO PARÂMETRO GAMA -----
  
  # Deve ser um valor numérico (double ou integer)
  if (!is.numeric(gama))
    stop("O parâmetro gama na log-verossimilhança da g0i deve ser um valor numérico.")
  
  # Deve ser um valor único, não podendo aceitar um vetor (nem mesmo vazio)
  if (length(gama) != 1)
    stop("O parâmetro gama na log-verossimilhança da g0i deve ser um valor único.")
  
  # Deve ser um número finito. Não pode conter NA's, NaN's nem Inf's
  if (!is.finite(gama))
    stop("O parâmetro gama na log-verossimilhança da g0i não pode ser NA, NaN ou Inf.")
  
  # Deve ser maior do que 0 (pela teoria)
  if (gama <= 0)
    stop("O parâmetros gama na log-verossimilhança da g0i deve ser maior do que 0.")
  
  # ----- VALIDAÇÃO DO PARÂMETRO L -----
  
  # Deve ser um valor numérico (double ou integer)
  if (!is.numeric(L))
    stop("O parâmetro L na log-verossimilhança da g0i deve ser um valor numérico.")
  
  # Deve ser um valor único, não podendo aceitar um vetor (nem mesmo vazio)
  if (length(L) != 1)
    stop("O parâmetro L na log-verossimilhança da g0i deve ser um valor único.")
  
  # Deve ser um número finito. Não pode conter NA's, NaN's nem Inf's
  if (!is.finite(L))
    stop("O parãmetro L na log-verossimilhança da g0i não pode ser NA, NaN ou Inf.")
  
  # Verificando se L é inteiro (quer passado como 'double', quer passado como 'integer')
  if (L != floor(L))
    stop("O parãmetro L na log-verossimilhança da g0i deve ser um número inteiro.")
  
  # Deve ser maior do que 0 (pela teoria)
  if (L <= 0)
    stop("O parâmetro L na log-verossimilhança da g0i deve ser maior do que 0.")
  
  # ----- CONSTRUÇÃO DA LOG-VERO DA g0i -----
  
  # Tamanho da amostra
  n <- length(Xobs)
  
  #Parcela constante da log-verossimilhança
  const <- n * L * log(L) + n * log(gamma(L - alfa)) - alfa * n * log(gama) - n * log(gamma(-alfa)) - n * log(gamma(L))
  
  #Parcela variável da log-verossimilhança
  var <- (L - 1) * sum(log(Xobs)) - (L - alfa) * sum(log(gama + L * Xobs))
  
  # ----- RETORNO COMO VALOR ÚNICO -----
  
  #Resultado (somas das duas parcelas)
  res <- (const + var)
  
  #Retorno do resultado
  return(res)
  
}
