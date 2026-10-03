# GRADIENTE DA LOG-VEROSSIMILHANÇA DA GAMA

# Na parametrização Gamma(forma, taxa), lembrar que taxa = 1/scale
grad_loglik_gama <- function(Xobs, shape, scale){
  
  # OBS: A única condição de entrada não verificável nesta estrutura ocorre quando há algum valor 
  # do tipo lógico dentro do vetor 'Xobs', pois nesse caso, esse valor lógico será coagido para 
  # 'double' fazendo 'TRUE' virar 1 e 'FALSE' virar 0. No caso de 'FALSE', ao ser convertido para o 
  # valor 0, a restrição de valores não nulos evitará a sua aceitação, mas, no caso de 'TRUE' sendo 
  # convertido para 1, esse dado errado passará pela verificação. Portanto, nesse único caso, é o 
  # usuário quem deve zelar para que não haja valores lógicos intrusos no vetor de amostras. Dado o 
  # uso desta função, o evento de existirem valores do tipo lógico dentro do vetor de amostras é 
  # improvável, no entanto, devemos tomar nota.
  
  # ----- VALIDAÇÃO DA AMOSTRA -----
  
  validar_vetor(Xobs)
  
  # ----- VALIDAÇÃO DA FORMA -----
  
  validar_valor(shape)
  
  # Deve ser maior que zero (pela teoria)
  if (shape <= 0)
    stop("O parâmetro shape no gradiente da log-verossimilhança da distribuição gama deve ser maior que zero.")
  
  # ----- VALIDAÇÃO DA ESCALA -----
  
  validar_valor(scale)
  
  # Deve ser maior que zero (pela teoria)
  if (scale <= 0)
    stop("O parâmetro scale no gradiente da log-verossimilhança da distribuição gama deve ser maior que zero.")
  
  # ----- CONSTRUÇÃO DO GRADIENTE DA LOG-VEROSSIMILHANÇA DA DISTRIBUIÇÃO GAMA -----
  
  # Tamanho da amostra
  n <- length(Xobs)
  
  # Derivada em shape
  dshape <- sum(log(Xobs)) - n * log(scale) - n * digamma(shape)
  
  # Derivada em scale
  dscale <- sum(Xobs) / (scale^2) - n * shape / scale
  
  # ----- RETORNO COMO UM VETOR -----
  
  return(c(dshape, dscale))
}
