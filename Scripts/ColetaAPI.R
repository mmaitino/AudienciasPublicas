library(httr)
library(jsonlite)
library(tidyverse)

# Fazendo as queries com um wrapper -----------
# Primeiro, vamos testar se é viável fazer tudo com um dos wrappers já existentes pra API da Câmara.
# No caso, vamos usar o pacote camaradosdeputadosapi, de Pedro D Rocha. Os demais em geral só tem info de
# deputados.

# devtools::install_github("pedrodrocha/camaradeputadosapi")
library(camaradeputadosapi)

eventos <- camaradeputadosapi::eventos(codTipoEvento = "120", 
                                       dataInicio = "2003-07-01", dataFim = "2003-12-31")

# A API da Câmara tem algumas restrições: 
# "Por padrão, todos os serviços de listagens retornam 15 itens, e o limite por requisição é de 100 itens"
# No wrapper temos um resultado restrito a 15 rows, independentemente do número real de resultados da query. 
# Isso é um problema para nós. Nos pedidos diretos isso também acontece, mas podemos acessar a 2a página etc
# pelos links dos resultados.
# Não vamos poder trabalhar com o wrapper.

# Olhando as urls das páginas completas, notei depois que é possível incluir uma query aumentando
# o nº máximo de itens retornados. Com isso, poderia usar o wrapper 
# (notei isso depois de escrever as funçoes diretas com a API. Vide abaixo)
camaradeputadosapi::eventos(codTipoEvento = "120", 
                            dataInicio = "2003-07-01", dataFim = "2003-12-31", itens = 50)
# mas achei os resultados meio estranhos - parecem não ser sempre consistentes
# (com itens 105, ampliei o período pro ano todo e retorna menos eventos do que que no período de meio ano)


# Comunicando direto com API --------------
path_API <- "https://dadosabertos.camara.leg.br/api/v2"
path_tipoeventos <- "/referencias/eventos/codTipoEvento"

# Caso mais simples: sem parâmetros na query
request_result <- httr::GET(paste0(path_API, path_tipoeventos)) #retorna um raw Unicode do json
request_result #status é importante - 200 = sucesso
data <- rawToChar(request_result$content) #httr tem a função content() que faz o rawToChar
data <- jsonlite::fromJSON(data)
tipos_evento <- data$dados

# Especificando parametros pra query
request_result <- httr::GET(paste0(path_API, "/eventos"),
                            query = list(codTipoEvento = "120",
                                         dataInicio = "2003-07-01",
                                         dataFim = "2003-07-31")) 
data <- rawToChar(request_result$content)
data <- jsonlite::fromJSON(data)
eventos <- data$dados

link_df <- as_tibble(data$links)
link_df # podemos ver se temos mais resultados do que os mostrados e os links para obtê-los

# Precisamos criar uma função que 

# a) cheque se há mais de uma página nos resultados
  # poderia checar diretamente (e.g., se first != last. self nunca é igual a first)
  # mas quando há 2+ páginas, passa a existir um link com rel='next'
# b) se há mais, abra a próxima página e colete os resultados (sequencialmente até que self seja == last)

# Funções -------------
request_camara <- function(path_detail, arg_list = NULL){
  url <- paste0("https://dadosabertos.camara.leg.br/api/v2", path_detail)
  
  # deveria adicionar alguns mecanismos de 'segurança' pros requests: trycatch, retornar erros, etc
  if(is.null(arg_list)){
    response <- httr::GET(url,httr::accept("application/json"))
  } else {
    response <- httr::GET(url,httr::accept("application/json"),
                          query = arg_list)
  }
  response
}

get_content <- function(APIresponse){
  # as páginas seguintes dos resultados retornam um html, não o formato de resposta da API
  getjsonfromhtml <- function(htmlurl){
    html_content <- rvest::html_text(xml2::read_html(htmlurl))
    json_content <- jsonlite::fromJSON(html_content)
    json_content
  }
  
  
  json_content <- jsonlite::fromJSON(rawToChar(APIresponse$content))
  results <- json_content$dados
  link_df <- json_content$links
  
  if("next" %in% link_df$rel){# caso haja mais de uma página nos resultados
    last_url <- link_df[link_df$rel == 'last', "href"]
    next_url <- link_df[link_df$rel == 'next', "href"]
    
    while (next_url != last_url){# prosseguir enquanto nao for a ultima página
      json_next <- getjsonfromhtml(next_url)
      new_results <- json_next$dados
      new_links <- json_next$links
      
      results <- bind_rows(results, new_results) # acrescenta os dados
      next_url <- new_links[new_links$rel == 'next', "href"] # atualiza o next
    }
    
    json_final <- getjsonfromhtml(last_url)
    lastpg_results <- json_final$dados
    results <- bind_rows(results, lastpg_results)
  }
  results
}

# Com essa função, podemos coletar, via API, todos os eventos de interesse. Se a estrutura se mantiver mais
# ou menos similar nos dados, teríamos os depoentes na descrição e extrairíamos esses dados mais adiante

# A pipeline seria simples:
# argumentos <- list(codTipoEvento = "120", dataInicio = "2003-07-01", dataFim = "2003-07-31")
# df <- request_camara("/eventos", argumentos) %>% get_content()


# Agora precisamos 
# a) identificar os tipos de evento relevantes e
# b) rodar pedidos em intervalos de tempo que não excedam limites de request
# Com isso, podemos coletar todos os eventos da Câmara e começar a explorar o quão completos sao os dados

irrelevantes <- c(110,112,115,118,122,150,175,177,
                  180,185,190,191,196,197,200,201,
                  202,203,204,205,207,210)
codigos <- tipos_evento %>% filter(!cod %in% irrelevantes) %>% pull(cod)
codigos <- paste(codigos, collapse = ",")

argumentos <- list(codTipoEvento = codigos,
                   dataInicio = "2017-08-01", dataFim = "2017-12-31")
tst <- request_camara("/eventos", argumentos) %>% get_content()


df_full <- tibble()
#1991-1998 (começam a aparecer dados de audiências em 1999. antes até tem sessoes na api mas nao audiencia)
for(year in 2023:2024){
  months <- c("01","02","03","04","05","06","07","08","09","10","11","12")
  for(i in seq_along(months)){
    print(paste(year, months[i]))
    start_date = paste(year,months[i],"01", sep = "-")
    end_date = lubridate::ceiling_date(lubridate::ymd(start_date), "month") - lubridate::days(1)
    
    argumentos <- list(codTipoEvento = codigos,
                       dataInicio = start_date, dataFim = end_date)
    df <- request_camara("/eventos", argumentos) %>% get_content()
    df_full <- bind_rows(df_full, df)
    Sys.sleep(5)
  }
  Sys.sleep(10)
}


df_full <- distinct(df_full)

# Export df
df_full %>% unnest(cols = c(orgaos, localCamara), names_sep = "_") %>% # unnest nas colunas dataframe
  # bind_rows(restyear_unnested) %>% arrange(dataHoraInicio) %>%
  write.csv2("eventos_9924.csv")


abrildf <- tibble()


# # na pg 8 em 2014-04 o fromJSON deu erro, tive que fazer manualmente
# for (i in 9:10){
#   url_base <- "https://dadosabertos.camara.leg.br/api/v2/eventos?codTipoEvento=120%2C125%2C130%2C140%2C152%2C155%2C157%2C160%2C170%2C194%2C195%2C198%2C199%2C206%2C208%2C209%2C211%2C212&dataInicio=2014-04-01&dataFim=2014-04-30&pagina=NPAG&itens=15"
#   url_sub <- gsub("NPAG", i, url_base)
#   json_new <- getjsonfromhtml(url_sub)
#   json_data <- json_new$dados
#   abrildf <- bind_rows(abrildf, json_data)
# }
# 
# 
# missingpage_unnested <- tst$dados %>% as_tibble() %>% unnest(evento_) %>% unnest() %>% unnest %>% unnest()
# missingpage_unnested <- missingpage_unnested %>% mutate_all(na_if,"") %>%
#   rename(localCamara_nome = nome, localCamara_predio = predio, 
#          localCamara_sala = sala, localCamara_andar = andar,
#          orgaos_id = id1, orgaos_uri = uri1, orgaos_sigla = sigla, 
#          orgaos_nome = nome1, orgaos_apelido = apelido, orgaos_codTipoOrgao = codTipoOrgao,
#          orgaos_tipoOrgao = tipoOrgao, orgaos_nomePublicacao = nomePublicacao, 
#          orgaos_nomeResumido = nomeResumido)
# 
# missingpage_unnested <- missingpage_unnested %>% 
#   mutate(id = as.integer(id), orgaos_id = as.integer(id),
#          orgaos_codTipoOrgao = as.integer(orgaos_codTipoOrgao),
#          localCamara_predio = as.logical(localCamara_predio),
#          localCamara_sala = as.logical(localCamara_sala),
#          localCamara_andar = as.logical(localCamara_andar),
#          urlRegistro = as.logical(urlRegistro)
#          )
# 
# 
# abrildf_unnested <- abrildf %>% unnest(cols = c(orgaos, localCamara), names_sep = "_")
# 
# abrildf_unnested <- bind_rows(abrildf_unnested, missingpage_unnested) %>% arrange(dataHoraInicio)
# 
# 
# 
# restyear_unnested <- restyear %>% unnest(cols = c(orgaos, localCamara), names_sep = "_")
# restyear_unnested <- bind_rows(restyear_unnested, abrildf_unnested)
# write.csv2(restyear_unnested, "eventos_14abril+.csv")



# Depois valeria a pena fazer uma 'auditoria' do scraping, sorteando algumas datas/periodos
# e vendo se está tudo lá

# talvez o melhor a fazer, ao invés de ficar fazendo mil requests diferentes na API,
# fosse fazer um só e o loop ser só a mudança de página. Aí sei exatamente onde retomar o loop etc