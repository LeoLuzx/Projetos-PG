
# FUNÇÃO PARA PLOTAR A IMAGEM

# Plota a imagem SAR em um dado canal
# - 'imag' é a imagem como array
# - 'canal' é o canal como valor inteiro [1 a 6]
plotar_imagem <- function(imag, canal){
  
  # Captura dos dados do canal
  dados <- Re(imag[,,canal])
  
  # Limpa a área de plotagem
  novografico()
  
  # Título
  titulo <- paste("Imagem gerada a partir do canal", canal)
  
  # Correção de contraste (para visualização)
  dados_corrigidos <- dados^0.01
  
  # Transformação para manter orientação consistente
  z <- t(dados_corrigidos)[, nrow(dados_corrigidos):1]
  
  # EXEMPLO DE APLICAÇÃO:
  
  # Matriz de entrada:
  #       [,1] [,2] [,3]
  # [1,]    1    4    7
  # [2,]    2    5    8
  # [3,]    3    6    9
  
  # t(dados_corrigidos):
  #       [,1] [,2] [,3]
  # [1,]    1    2    3
  # [2,]    4    5    6
  # [3,]    7    8    9
  
  # t(dados_corrigidos)[, 3:1]
  #       [,1] [,2] [,3]
  # [1,]    3    6    9
  # [2,]    2    5    8
  # [3,]    1    4    7
  
  # Plot com eixos corretos
  image(
    x = 1:ncol(dados_corrigidos),
    y = 1:nrow(dados_corrigidos),
    z = z,
    # Escala de cinza de 0.01 a 1
    col = gray(1:100/100),
    main = "",
    xlab = "", ylab = "",
    # Usa uma grade de pixels reais
    useRaster = TRUE,
    axes = FALSE
  )
}
