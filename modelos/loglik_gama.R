# FUNÇÃO PARA CALCULAR A LOG-VEROSSIMILHANÇA DA DISTRIBUIÇÃO GAMA

# Na parametrização Gamma(forma, taxa), lembrar que taxa = 1/scale
loglik_gama <- function(Xobs, shape, scale) {
  
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
    stop("O parâmetro shape na log-verossimilhança da distribuição gama deve ser maior que zero.")
  
  # ----- VALIDAÇÃO DA ESCALA -----
  
  validar_valor(scale)
  
  # Deve ser maior que zero (pela teoria)
  if (scale <= 0)
    stop("O parâmetro scale na log-verossimilhança da distribuição gama deve ser maior que zero.")
  
  # ----- CONSTRUÇÃO DA LOG-VEROSSIMILHNAÇA DA DISTRIBUIÇÃO GAMA -----
  
  # Tamanho da amostra
  n <- length(Xobs)
  
  # Cálculo da log-verossimilhança
  logvero <- - n * shape * log(scale) - n * lgamma(shape) + (shape - 1) * sum(log(Xobs)) - sum(Xobs) / scale
  
  # ----- RETORNO COMO VALOR ÚNICO -----
  
  return(logvero)
}
