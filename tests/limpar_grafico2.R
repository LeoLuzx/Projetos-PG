# GERAR NOVA ÁREA GRÁFICA

# Esta é uma versão experimental e bem mais simples da da função 'limpar_grafico()' pois a antiga 
# possivelmente pode gerar aberturas sucessivas de novas telas externas nesta nova reorganização do 
# projeto. Até agora, está OK, mas ainda necessita de uns testes antes de substituir a antiga.

# Gerar nova área gráfica para que não haja erro na plotagem
limpar_grafico2 <- function() {
  
  # Fecha todos os dispositivos gráficos abertos, eliminando as áreas gráficas e evitando 
  # sobreposição de plotagens
  graphics.off()
  
}