####################################################################################################

# ARQUIVO PRINCIPAL DO PROCESSAMENTO

####################################################################################################

# NÃO É NECESSÁRIO CARREGAR NENHUM ARQUIVO

# O arquivo .Rdata já detém todos os objetos necessários ao processamento
# O arquivo .Rprofile já carrega todo o necessário quando da abertura do RStudio

# Da pasta 'proxys' é carregado o arquivo 'dependencias.R' (via .Rprofile) que:
# - limpa o prompt e emite a mesnagem 'Console limpo e tudo carregado!'
# - limpa o 'Global Environment' (não apaga objetos criados, apenas os antigos renomeados)
# - carrega objetos (imagens e outros que forem de uso recorrente)
# - carrega os pacotes necessários juntamente com suas dependências

# Ainda da pasta 'proxys', o arquivo 'dependencias.R' carrega o arquivo 'funcoes.R' que:
# - carrega as funções utilitárias da pasta 'utils'
# - carrega as funções dos modelos da pasta 'modelos'

# A seperação dos arquivos de funções desta forma visa:
# - que cada função esteja em um arquivo próprio para facilitar a manutenção
# - facilitar a busca temática por pasta: utils/modelos