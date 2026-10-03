# LOG-VEROSSIMILHANÇA DA DISTRIBUIÇÃO g0i

# Define a log-verossimilhança da distribuição g0i
loglik_g0i <- function(Xobs, alfa, gama, L){
  
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
  
  # ----- VALIDAÇÃO DO PARÂMETRO ALFA -----
  
  validar_valor(alfa)
  
  # Deve ser menor do que -1 (pela teoria)
  if (alfa >= -1)
    stop("O parâmetro alfa na log-verossimilhança da distribuição g0i deve ser menor do que -1.")
  
  # ----- VALIDAÇÃO DO PARÂMETRO GAMA -----
  
  validar_valor(gama)
  
  # Deve ser maior do que zero (pela teoria)
  if (gama <= 0)
    stop("O parâmetro gama na log-verossimilhança da distribuição g0i deve ser maior do que zero.")
  
  # ----- VALIDAÇÃO DO PARÂMETRO L -----
  
  validar_valor(L)
  
  # Verificando se L é inteiro (quer passado como 'double', quer passado como 'integer')
  if (L != floor(L))
    stop("O parâmetro L na log-verossimilhança da distribuição g0i deve ser um número inteiro.")
  
  # Deve ser maior do que 0 (pela teoria)
  if (L <= 0)
    stop("O parâmetro L na log-verossimilhança da distribuição g0i deve ser maior do que zero.")
  
  # ----- CONSTRUÇÃO DA LOG-VEROSSIMILHANÇA DA DISTRIBUIÇÃO g0i -----
  
  # Tamanho da amostra
  n = length(Xobs)
  
  # Esta quantidade, que é comum às duas componentes do gradiente, deve ser verificada porque pode 
  # ter comportamento explosivo quando valores muito grandes surgem, por exemplo, quando os seus 
  # elementos são  estimados em ciclos bootstrap gerando overflow ou underflow. Os avisos aqui 
  # permitem comunicar se por acaso isso acontecer.
  aux <- gama + L * Xobs
  
  if (any(!is.finite(aux)))
    stop("O cálculo de (gama + L * Xobs) na log-verossimilhança da distribuição g0i produziu valores infinitos.")
  
  # Parcela constante da log-verossimilhança
  const <- n * L * log(L) + n * log(gamma(L - alfa)) - alfa * n * log(gama) - n * log(gamma(-alfa)) - n * log(gamma(L))
  
  # Parcela variável da log-verossimilhança
  var <- (L - 1) * sum(log(Xobs)) - (L - alfa) * sum(log(aux))
  
  # ----- RETORNO COMO VALOR ÚNICO -----
  
  # Resultado (somas das duas parcelas)
  res <- (const + var)
  
  # Retorno do resultado
  return(res)
}
