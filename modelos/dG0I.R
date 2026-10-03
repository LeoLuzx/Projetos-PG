# DENSIDADE DA DISTRIBUIÇÃO g0i

#Calcula o valor da densidade da distribuição g0i
dG0I <- function(x, alfa, gama, L){
  
  # ----- VALIDAÇÃO DO VALOR X -----
  
  validar_valor(x)
  
  # Deve ser maior do que zero
  if (x <= 0)
    stop("O valor de x para o cálculo da densidade da distribuiçaõ g0i deve ser maior do que zero")
  
  # ----- VALIDAÇÃO DO PARÂMETRO ALFA -----
  
  validar_valor(alfa)
  
  # Deve ser menor do que -1 (pela teoria)
  if (alfa >= -1)
    stop("O parâmetro alfa para o cálculo da densidade da distribuição g0i deve ser menor do que -1.")
  
  # ----- VALIDAÇÃO DO PARÂMETRO GAMA -----
  
  validar_valor(gama)
  
  # Deve ser maior do que zero (pela teoria)
  if (gama <= 0)
    stop("O parâmetro gama para o cálculo da densidade da distribuição g0i deve ser maior do que zero.")
  
  # ----- VALIDAÇÃO DO PARÂMETRO L -----
  
  validar_valor(L)
  
  # Verificando se L é inteiro (quer passado como 'double', quer passado como 'integer')
  if (L != floor(L))
    stop("O parâmetro L para o cálculo da densidade da distribuição g0i deve ser um número inteiro.")
  
  # Deve ser maior do que 0 (pela teoria)
  if (L <= 0)
    stop("O parâmetro L para o cálculo da densidade da distribuição g0i deve ser maior do que zero.")
  
  # ----- CÁLCULO DA DENSIDADE -----
  
  # Esta quantidade deve ser verificada porque pode  ter comportamento explosivo quando valores 
  # muito grandes surgem, por exemplo, quando os seus elementos são  estimados em ciclos bootstrap 
  # gerando overflow ou underflow. Os avisos aqui permitem comunicar se por acaso isso acontecer.
  aux <- gama + L * x
  
  if (!is.finite(aux))
    stop("O cálculo de (gama + L * x) na densidade da distribuição g0i produziu valores infinitos.")
  
  # Numerador da g0i
  numerador <- L^L * gamma(L - alfa) * x^(L - 1)
  
  # Denominador da g0i
  denominador <- (gama ^ alfa) * gamma(-alfa) * gamma(L) * (aux)^(L - alfa)
  
  # Densidade final (razão dos dois anteriores)
  densidade <- numerador / denominador
  
  return(densidade)
  
}
