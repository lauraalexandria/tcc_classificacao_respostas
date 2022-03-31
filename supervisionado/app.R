#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(tidymodels)
library(tune)
library(workflowsets)
library(vip)
library(knitr)
library(kableExtra)
library(readxl)

respostas <- read_xlsx("dados/Respostas_Zé_Gotinha 13.08.2021.xlsx") %>%
    select(-9) %>% 
    mutate(`Nível da rubrica` = case_when(
        `Nível da rubrica` == "4" ~ "3",
        TRUE ~ `Nível da rubrica`
    )) %>% filter(id != "SMED")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Classificadores - Zé Gotinha"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel( 
            width = 2,
            awesomeRadio(
                inputId = "pergunta",
                label = "Escolha uma Pergunta", 
                choices = paste("Pergunta", c(1:3, 5:21)),
                selected = "Pergunta 1"
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            fluidRow(
                
                column(width = 5,
                       radioGroupButtons(
                           inputId = "tipo_mod",
                           label = "Escolha um tipo de label:",
                           choices = c("Naive Bayes" = "naive_bayes", 
                                       "SVM" = "svm", "Random Forest" = "random_forest"),
                           checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                
                column(width = 4, uiOutput("n_palavras")),
                
                column(width = 3, uiOutput("n_k"))
            
            ),
            
           fluidRow(
               column(width = 9, 
                      uiOutput("classes_inicias"),
                      plotOutput("matriz")),
               column(width = 3, uiOutput("tabela_resultados"))),
           plotOutput("vip"), 
           plotOutput("comparacao_final")
           
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$n_palavras <- renderUI({
        
        p <- str_sub(input$pergunta, start = 10)
        palavras <- readRDS(paste0("modelos/palavras_pergunta_",p,".rds"))$palavras
        
        h3(paste( "Nº de palavras", palavras %>% length()))
        
    })
    
    output$n_k <- renderUI({
        
        p <- str_sub(input$pergunta, start = 10)
        k <- readRDS(paste0("modelos/naive_bayes_lsa_pergunta_",p,".rds"))$k
        
        h3(paste( "K =", k))
        
    })
    
    output$classes_inicias <- renderUI({
        
        p <- str_sub(input$pergunta, start = 10)
        tipo_mod <- input$tipo_mod
        
        maior_acuracia <- 0
        
        for (bag in c("_onehot", "_tf", "_tfidf", "_lsa")){
            
            modelo_teste <- readRDS(paste0("modelos/", tipo_mod , bag,"_pergunta_",p,".rds"))$modelo
            acuracia <- collect_metrics(modelo_teste)[1,3] %>% as.numeric()
            
            if (acuracia > maior_acuracia){
                
                maior_acuracia <- acuracia
                modelo <- modelo_teste
                tipo_bag <- bag
                
            }
            
        }
        
        n_teste <- collect_predictions(modelo) %>% 
            group_by(`Nível da rubrica`) %>% count() %>% rename(Teste = n)
        predicoes <- collect_predictions(modelo) %>% 
            group_by(.pred_class) %>% count()
        
        respostas %>% filter(pergunta_id == p) %>% 
            group_by(Pergunta,`Nível da rubrica`) %>% 
            count() %>% 
            left_join(n_teste) %>%
            left_join(predicoes, by = c("Nível da rubrica" = ".pred_class")) %>%
            rename(Total = n.x, Predito = n.y) %>% 
            kable() %>% 
            kable_styling(bootstrap_options = c("striped", "hover")) %>%
            collapse_rows(columns = 1, valign = "top") %>% 
            row_spec(0, background = "#778bd9", color = "white") %>% 
            HTML()
        
    })
    
    
    output$matriz <- renderPlot({
        
        p <- str_sub(input$pergunta, start = 10)
        tipo_mod <- input$tipo_mod
        
        maior_acuracia <- 0
        
        for (bag in c(#"_onehot", "_tf", "_tfidf", 
                      "_lsa")){
            
            modelo_teste <- readRDS(paste0("modelos/", tipo_mod , bag,"_pergunta_",p,".rds"))$modelo
            acuracia <- collect_metrics(modelo_teste)[1,3] %>% as.numeric()
            
            if (acuracia > maior_acuracia){
                
                maior_acuracia <- acuracia
                modelo <- modelo_teste
                tipo_bag <- bag
                
            }
        
        }
        
        per_acerto <-  conf_mat_resampled(modelo) %>% 
            mutate(class = ifelse(Truth == Prediction, "Acerto", "Erro")) %>% 
            group_by(class) %>% 
            summarise(Freq = sum(Freq)) %>% ungroup() %>% 
            mutate(Prop = (Freq/sum(Freq))*100) %>% 
            pull(Prop) %>% .[1]  %>% round(2)
        
        conf_mat_resampled(modelo) %>% group_by(Truth) %>% 
            mutate(Freq = round(Freq/sum(Freq),2)) %>% 
            ggplot(aes(x = Prediction, y = Truth, fill = Freq)) +
            geom_tile(color = "white") + 
            geom_text(aes(label = Freq)) +
            labs(title = str_to_title(str_replace_all(paste(tipo_mod, " - ",tipo_bag, "/ Porcentagem de Acerto = ", per_acerto, "%"), "_", " "))) +
            theme_minimal() +
            scale_fill_gradient2(low = "#00AFBB")
        
    })
    
    output$tabela_resultados <- renderUI({
        
        p <- str_sub(input$pergunta, start = 10)
        
        naive1 <- collect_metrics(readRDS(paste0("modelos/naive_bayes_onehot_pergunta_",p,".rds"))$modelo)
        naive2 <- collect_metrics(readRDS(paste0("modelos/naive_bayes_tf_pergunta_",p,".rds"))$modelo)
        naive3 <- collect_metrics(readRDS(paste0("modelos/naive_bayes_tfidf_pergunta_",p,".rds"))$modelo)
        naive4 <- collect_metrics(readRDS(paste0("modelos/naive_bayes_lsa_pergunta_",p,".rds"))$modelo)
        naive <- rbind(naive1, naive2, naive3, naive4) %>% mutate(Modelo = "Naive Bayes", Bag = rep(c("One Hot", "TF", "TFIDF", "LSA"), each = 2))
        
        svm1 <- collect_metrics(readRDS(paste0("modelos/svm_onehot_pergunta_",p,".rds"))$modelo)
        svm2 <- collect_metrics(readRDS(paste0("modelos/svm_tf_pergunta_",p,".rds"))$modelo)
        svm3 <- collect_metrics(readRDS(paste0("modelos/svm_tfidf_pergunta_",p,".rds"))$modelo)
        svm4 <- collect_metrics(readRDS(paste0("modelos/svm_lsa_pergunta_",p,".rds"))$modelo)
        svm <- rbind(svm1, svm2, svm3, svm4) %>% mutate(Modelo = "SVM", Bag = rep(c("One Hot", "TF", "TFIDF", "LSA"), each = 2))
        
        rf1 <- collect_metrics(readRDS(paste0("modelos/random_forest_onehot_pergunta_",p,".rds"))$modelo)
        rf2 <- collect_metrics(readRDS(paste0("modelos/random_forest_tf_pergunta_",p,".rds"))$modelo)
        rf3 <- collect_metrics(readRDS(paste0("modelos/random_forest_tfidf_pergunta_",p,".rds"))$modelo)
        rf4 <- collect_metrics(readRDS(paste0("modelos/random_forest_lsa_pergunta_",p,".rds"))$modelo)
        rf <- rbind(rf1, rf2, rf3, rf4) %>% mutate(Modelo = "Random Forest", Bag = rep(c("One Hot", "TF", "TFIDF", "LSA"), each = 2))
        
        if(input$tipo_mod == "naive_bayes"){dados <- naive}
        if(input$tipo_mod == "svm"){dados <- svm}
        if(input$tipo_mod == "random_forest"){dados <- rf}
        
        dados %>% 
            select(Modelo, Bag, `Métrica` = .metric, Estimativa = .estimate) %>% 
            mutate(Estimativa = round(Estimativa, 2)) %>% 
            kbl() %>% kable_styling(bootstrap_options = c("striped", "hover")) %>%
            row_spec(0, background = "#778bd9", color = "white") %>%
            collapse_rows(columns = 1:2, valign = "top") %>% 
            HTML()
            
    })
    
    output$vip <- renderPlot({

        p <- str_sub(input$pergunta, start = 10)
        modelo <- readRDS(paste0("modelos/random_forest_tfidf_pergunta_",p,".rds"))$modelo
        
        modelo %>%
          pluck(".workflow", 1) %>%
          pull_workflow_fit()  %>%
          vip(num_features = 20) + theme_minimal()
    })
    
    
    output$comparacao_final <- renderPlot({
        
        nivel_dificuldade <- readRDS("dados/nivel_dificuldade.rds") %>% 
            mutate(pergunta_id = paste("Pergunta", pergunta_id),
                   `Nível de dificuldade` = factor(`Nível de dificuldade`, 
                                                   levels = c("Fácil", "Médio", "Difícil"))) %>% 
            rename(Pergunta = pergunta_id)
        
        tipo_mod <- input$tipo_mod
        
        if (tipo_mod == "random_forest"){tipo_bag <- "_tfidf"}
        else {tipo_bag <- "_tf"}
        
        dados <- data.frame(Pergunta = character(), Acerto = double())
        
        for (p in c(1:3,5:21)){
            
            modelo <- readRDS(paste0("modelos/", tipo_mod , tipo_bag,"_pergunta_",p,".rds"))$modelo
            
            perc_acerto <- conf_mat_resampled(modelo) %>% 
                mutate(class = ifelse(Truth == Prediction, "Acerto", "Erro")) %>% 
                group_by(class) %>% 
                summarise(Freq = sum(Freq)) %>% ungroup() %>% 
                mutate(Prop = (Freq/sum(Freq))*100) %>% 
                pull(Prop) %>% .[1]  %>% round(2)
            
            dados <- dados %>% add_row(Pergunta = paste("Pergunta", p), 
                                       Acerto = perc_acerto)
            
        }
        
        
        dados %>% 
            left_join(nivel_dificuldade) %>% 
            mutate(Pergunta = fct_reorder(Pergunta, Acerto)) %>% 
            arrange(Pergunta) %>% 
            ggplot(aes(y = Acerto, x = Pergunta, fill = `Nível de dificuldade`)) + geom_col() + coord_flip() +
            labs(x = "", y = "Proporção de Acerto") + theme_minimal()
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
