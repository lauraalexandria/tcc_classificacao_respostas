library(tidyverse)
library(knitr)
library(kableExtra)
library(tidytext)
library(readxl)
library(tidymodels)
library(textrecipes) # steps para variáveis textuais;
library(discrim) # outras possibilidades de modelos, como análises discriminantes
library(themis) # possui step com downsample para classes desbalanceadas;
library(workflowsets)
library(stringr)
library(tidytext)
library(quanteda)
library(quanteda.textmodels)

### O que eu já fiz:
# 1. Retirar stopwords
# 2. Retirar Níveis com poucas observações;
# 3. Caçar o workflowsets
# 4. Mudar para os outros tipo de Bag of Words;

### O que eu tenho que correr para fazer:
# 1. Comparar os resultados limitando pelo número de palavras mais frequentes
# 2. Passar o trem para shiny porque vai ser mais fácil de montar;
# 3. Refazer tudo com os pacotes base (chorando...) - ESPERA
# 4. Adicionar tabela do lado com os modelo$.workflow[[1]]$fit$fit
# 5. Acho que as vip poderiam contribuir em algo tambiem.
# 6. Sentar analisar e tirar observações sobre as análises;
# 7. Milagre se eu comentasse algum código;

# TENS CORAGEM DE RODAR COM OS TUNE??

numbers2words <- function(x){
  ## Function by John Fox found here: 
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
  helper <- function(x){
    
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
    else trim(paste(tens[digits[2]],
                    Recall(as.numeric(digits[1]))))
    else if (nDigits == 3)
      if (as.numeric(str_flatten(digits[c(2,1)])) <= 9)
        trim(paste(centos[digits[3]],
                   ones[digits[1]]))
    else if (as.numeric(str_flatten(digits[c(2,1)])) <= 19) 
      trim(paste(centos[digits[3]],
                 teens[digits[1]]))
    else trim(paste(centos[digits[3]],
                    tens[digits[2]],
                    Recall(as.numeric(digits[1]))))
    else if (nDigits == 4) trim(paste(ones[digits[4]], "mil", 
                                      Recall(makeNumber(digits[3:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "muito grande!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],"," ,
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    #Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    #Clear any trailing " and"
    text=gsub(" and$","",text)
    #Clear any trailing comma
    gsub("\ *,$","",text)
  }  
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))     
  #Disable scientific notation
  opts <- options(scipen=100) 
  on.exit(options(opts)) 
  ones <- c("", "um", "dois", "três", "quatro", "cinco", "seis", "sete",
            "oito", "nove") 
  names(ones) <- 0:9 
  teens <- c("dez", "onze", "doze", "treze", "quatorze", "quinze",
             "dezesseis", "dezessete", "dezoito", "dezenove")
  names(teens) <- 0:9 
  tens <- c("", "", "vinte", "trinta", "quarenta", "cinquenta", "sessenta", "setenta", "oitenta", "noventa") 
  names(tens) <- 0:9 
  centos <- c("cento","duzentos", "trezentos", "quatrocentos", "quinhentos", "seiscentos", "setecentos", "oitocentos", "novecentos")
  names(centos) <- 1:9
  x <- round(x)
  suffixes <- c("mil", "milhão", "bilhão", "trilhão")     
  if (length(x) > 1) return(trim(sapply(x, helper)))
  helper(x)
}

transforma_num <- function(palavra){
  
  if(str_detect(palavra, "[a-zéèàá]")){return(palavra)}      
  else{return(numbers2words(as.numeric(palavra)))}
  
}


setwd("análises/Zé Gotinha")

respostas <- read_xlsx("dados/Respostas_Zé_Gotinha 13.08.2021.xlsx") %>%
  select(-9) %>% 
  mutate(`Nível da rubrica` = case_when(
    `Nível da rubrica` == "4" ~ "3",
    TRUE ~ `Nível da rubrica`
  )) %>% filter(id != "SMED") %>% 
  mutate(Resposta = tolower(Resposta) %>% # Todas as letras em minúsculo
           str_replace_all("[.,!?:;/'*]", " ")) %>%  # Remove pontuação AINDA TENHO QUE REMOVER ASPAS
  separate_rows(Resposta, sep = " ") %>% 
  drop_na(Resposta) %>% filter(str_detect(Resposta, "[:alnum:]")) %>% 
  mutate(Resposta = map(Resposta, transforma_num) %>% unlist()) %>% 
  group_by(estudante_id, pergunta_id, Pergunta, `Nível da rubrica`) %>% 
  summarise(Resposta = str_flatten(Resposta, collapse = " "))

perguntas_fora <- respostas %>% 
  group_by(pergunta_id, Pergunta,`Nível da rubrica`) %>% 
  count() %>% filter(n <= 4) %>% 
  mutate(perg_nivel = paste(pergunta_id, `Nível da rubrica`)) %>% 
  pull(perg_nivel)

respostas <- respostas %>% 
  mutate(perg_nivel = paste(pergunta_id, `Nível da rubrica`)) %>%
  filter(!perg_nivel %in% perguntas_fora) %>% 
  select(-perg_nivel) 

set.seed(1234)

classificacao <- function(dados, modelo, tipo_bag, pergunta){
  
  dados <- dados %>% filter(pergunta_id == pergunta)
  
  if (tipo_bag == "onehot") {
    
    dados_split <- initial_split(dados, strata = `Nível da rubrica`, prop = 8/10)
    dados_train <- training(dados_split)
    dados_test <- testing(dados_split)
    dados_folds <- vfold_cv(dados_train)
    
    receita <- recipe(`Nível da rubrica` ~ Resposta, data = dados_train) %>%
      step_tokenize(Resposta) %>%
      # step_tokenfilter(Resposta, max = 10) %>% 
      step_stopwords(Resposta,language = "pt") %>% 
      step_sequence_onehot(Resposta) # ou step_tf(), step_sequence_onehot()
    # step_downsample(`Nível da rubrica`)
    
  }
  
  if (tipo_bag == "tf") {
    
    dados_split <- initial_split(dados, strata = `Nível da rubrica`, prop = 8/10)
    dados_train <- training(dados_split)
    dados_test <- testing(dados_split)
    dados_folds <- vfold_cv(dados_train)
    
    receita <- recipe(`Nível da rubrica` ~ Resposta, data = dados_train) %>%
      step_tokenize(Resposta) %>%
      # step_tokenfilter(Resposta, max = 10) %>% 
      step_stopwords(Resposta,language = "pt") %>% 
      step_tf(Resposta)
    
  }
  
  if (tipo_bag == "tfidf") {
    
    dados_split <- initial_split(dados, strata = `Nível da rubrica`, prop = 8/10)
    dados_train <- training(dados_split)
    dados_test <- testing(dados_split)
    dados_folds <- vfold_cv(dados_train)
    
    receita <- recipe(`Nível da rubrica` ~ Resposta, data = dados_train) %>%
      step_tokenize(Resposta) %>%
      # step_tokenfilter(Resposta, max = 10) %>% 
      step_stopwords(Resposta, language = "pt") %>% 
      step_tfidf(Resposta) # ou step_tf(), step_sequence_onehot()
    # step_downsample(`Nível da rubrica`)
    
  }
  
  if (tipo_bag == "lsa") {
    
    palavras <- recipe( ~ Resposta, data = dados) %>%
      step_tokenize(Resposta) %>%
      step_stopwords(Resposta, language = "pt") %>% 
      show_tokens(Resposta) %>% unlist() %>% unique()
    
    bag <- dados %>% ungroup() %>% unnest_tokens(word, Resposta) %>%
      distinct(estudante_id, word, .keep_all = T) %>%
      mutate(bag = 1) %>%
      pivot_wider(id_cols = estudante_id, names_from = word, values_from = bag, values_fill = 0) %>%
      select(- estudante_id) %>% as.matrix()
    palavras <- colnames(bag)

    matriz <- scale(bag)

    b <- min(dim(matriz)[1]/dim(matriz)[2], dim(matriz)[2]/dim(matriz)[1])
    w <- 0.56*b^3 - 0.95*b^2 + 1.82*b + 1.43
    ymed <- svd(matriz)$d %>% median()
    k <- round(w*ymed)
    bag <- svd(bag)$u[, 1:k] %*% diag(svd(bag)$d[1:k]) %*% t(svd(bag)$v[,1:k])
    colnames(bag) <- palavras
    
    bag <- tokens(dados$Resposta, what = "word", 
                  remove_numbers = TRUE, remove_punct = TRUE,
                  remove_symbols = TRUE, remove_hyphens = TRUE)
    bag <- tokens_select(bag, pattern = stopwords("pt"), selection = "remove")
    bag <- dfm(bag)
    bag <- dfm_tfidf(bag, scheme_tf = "prop")
    # bag <- as.matrix(bag)
    
    projecao <- textmodel_lsa(bag, nd = k) 
    projecao <- as.data.frame(projecao$docs)
    
    dados <- cbind(dados %>% ungroup() %>% select(estudante_id, `Nível da rubrica`),
                   projecao)
    
    dados_split <- initial_split(dados, strata = `Nível da rubrica`, prop = 8/10)
    dados_train <- training(dados_split)
    dados_test <- testing(dados_split)
    dados_folds <- vfold_cv(dados_train)
    
    receita <- recipe(`Nível da rubrica` ~ ., data = dados_train) %>% 
      step_rm(estudante_id)
    
    }
  
  if (tipo_bag != "lsa") {
  
    palavras <- recipe( ~ Resposta, data = dados_train) %>%
      step_tokenize(Resposta) %>%
      step_stopwords(Resposta, language = "pt") %>% 
      show_tokens(Resposta) %>% unlist() %>% unique()
    
    k <- NA
  
  }
  
  
  if(modelo == "naive_bayes"){ 
    
    func_mod <- naive_Bayes()
    engine_mod <- "naivebayes"
    grid <- NA
    
    receita <- receita %>% 
      step_zv(all_predictors())
    
  }
  
  if(modelo == "svm"){ 
    
    func_mod <- svm_linear(cost = tune())
    engine_mod <- "kernlab"
    grid <- grid_regular(cost())
    
    receita <- receita %>% 
      step_zv(all_predictors())
    
  }
  
  if(modelo == "random_forest"){ 
    
    func_mod <- rand_forest(mode = "classification",
                            mtry = tune(), trees = tune(), min_n = tune())
    engine_mod <- "ranger"
    grid <- expand.grid(mtry = c(5, 7, 10), 
                        trees = c(500, 1000, 2000),
                        min_n =  c(10, 20, 30))
    
  }
  
  modelo <- func_mod %>%  
    set_mode("classification") %>%
    set_engine(engine_mod, importance = "impurity")
  
  workflow <- workflow() %>%    
    add_recipe(receita) %>%
    add_model(modelo)
  
  resamples <-  tune_grid(
    workflow,
    dados_folds,
    grid = grid,
    control = control_resamples(verbose = T, save_pred = TRUE))
  
  final_workflow <- finalize_workflow(workflow, select_best(resamples))
  
  modelo <- last_fit(final_workflow, dados_split)
  
  list(modelo = modelo, dados_test = dados_test, palavras = palavras, k = k)
  
}


for (tipo_modelo in c("naive_bayes", "svm", "random_forest")){
  
  for (tipo_bag in c("onehot", "tf", "tfidf", "lsa")){
    
    for (p in c(1:3, 5:21)){
      
      modelo <- classificacao(respostas, tipo_modelo, tipo_bag, p)
      saveRDS(modelo, paste0("modelos/para duração/", tipo_modelo, "_", tipo_bag, "_pergunta_", p, ".rds"))
      
    }
  }
}







