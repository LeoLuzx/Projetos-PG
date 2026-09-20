
# GRADIENTE DA LOG-VEROSSIMILHANÇA DA GAMA

# Na parametrização Gamma(forma, taxa), lembrar que taxa = 1/scale
# Sem valores padrão para os parâmetros para que o usuários seja obrigado e não esquecer

calcular_grad_loglik_gama <- function(Xobs, shape, scale){
  
  # ----- SANETIZAÇÃO DA AMOSTRA -----
  
  # Se os dados não forem numéricos, o R vai coagir o restante num tipo de dado diferente
  if (!is.numeric(Xobs))
    stop("Xobs na log-lik da gama deve ser um vetor numérico.")
  
  # Se os dados forem um vetor vazio
  if (length(Xobs) == 0)
    stop("Xobs na log-lik da gama deve ser um vetor não vazio.")
  
  # Se os dados tiverem valores NA, NaN ou Inf.
  if (any(!is.finite(Xobs)))
    stop("Xobs na log-lik da gama não pode conter NA, NaN ou Inf.")
  
  # Se os dados não forem positivos
  if (any(Xobs <= 0))
    stop("Xobs na log-lik da gama deve conter apenas valores positivos.")
  
  # ----- SANETIZAÇÃO DA FORMA -----
  
  # Sanetização do parâmetro de forma
  if (shape <= 0)
    stop("O parãmetros de forma 'shape' da gama deve ser positivo.")
  
  # ----- SANETIZAÇÃO DA ESCALA -----
  
  # Sanetização do parâmetro de escala
  if (scale <= 0)
    stop("O parâmetro de escala 'scale' da gama deve ser positivo.")
  
  # ----- CONSTRUÇÃO DO GRADIENTE DA LOG-VERO DA GAMA -----
  
  # Tamanho da amostra
  n <- length(Xobs)
  
  # Derivada em shape
  dshape <- sum(log(Xobs)) - n * log(scale) - n * digamma(shape)
  
  # Derivada em scale
  dscale <- sum(Xobs) / (scale^2) - n * shape / scale
  
  # ----- RETORNO COMO UM VETOR -----
  
  return(c(dshape, dscale))
}
