# app.R

# Carrega as bibliotecas necessárias
library(shiny)
library(flexdashboard) # Mantido para 'gauge' se for renderizado via uiOutput, ou para recriar o visual.
library(orcamentoBR)
library(dplyr)
library(kableExtra)
library(scales)
library(formattable) # Para barras de cores nas tabelas de eficiência
library(DT) # Para tabelas interativas

# Define a interface do usuário (UI)
ui <- fluidPage(
  # Título do aplicativo
  titlePanel("Relatório Orçamentário - SIOP - IFPE"),
  
  # Sidebar com controles de entrada (exemplo: ano)
  sidebarLayout(
    sidebarPanel(
      h4("Este aplicativo exibe dados orçamentários do IFPE em 2025."),
      hr(), 
      helpText("Os dados são obtidos atravéis da API SIOP."),
      hr(),    
      # Adiciona um seletor de arquivo para carregar o dataframe existente (agora CSV)
      #fileInput("file_upload", "Carregar arquivo de dados (.csv)",
      #          accept = c(".csv", "text/csv")), # Aceita arquivos CSV
      p(paste("Data de referência do relatório:", format(Sys.Date(), "%d/%m/%Y"))), # Atualizado para refletir a data de geração do relatório

    ),
      
  
    # Painel principal com as abas de conteúdo
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Visão Geral",
                 h3("Resumo Orçamentário"),
                 hr(),
                 # Renderiza a tabela de visão geral
                 DTOutput("visaoGeralTable"),
                 hr(),
                 
                
                 
                 fluidRow(
                   column(4, h4("Empenhado/Dotação"), gaugeOutput("gaugeED")),
                   column(4, h4("Pago/Dotação"), gaugeOutput("gaugePD")),
                   column(4, h4("Pago/Empenhado"), gaugeOutput("gaugePE"))
                 )
        ),
        tabPanel("Por Função",
                 h3("Dados Orçamentários por Função"),
                 hr(),
                 DTOutput("funcaoTable"),
                 h3("Eficiência por Função"),
                 hr(),
                 uiOutput("funcaoEfcTable") # Usar uiOutput para formattable que gera HTML
        ),
        tabPanel("Por Ação Orçamentária",
                 h3("Dados Orçamentários por Ação"),
                 hr(),
                 DTOutput("acaoTable"),
                 h3("Eficiência por Ação"),
                 hr(),
                 uiOutput("acaoEfcTable") # Usar uiOutput para formattable que gera HTML
        ),
        tabPanel("Nota",
                 h3("Observações"),
                 hr(),
                 p("Este documento é gerado a partir da biblioteca ",
                   tags$b("orcamentoBR"), " disponibilizada pelo SIOP através do R."),
                 p("Link da documentação:",
                   a("https://cran.r-project.org/web/packages/orcamentoBR/index.html",
                     href="https://cran.r-project.org/web/packages/orcamentoBR/index.html", target="_blank")),
                 textOutput("dataGeracao")
        )
      )
    )
  )
)

# Define a lógica do servidor
server <- function(input, output, session) {
  
  # Define o ano corrente para extração de dados
  # Poderia ser input$ano_selecionado se houvesse um seletor de ano na UI
  anoCorrente <- 2025 # Mantido como referência, mas a extração é primariamente via arquivo
  data_hoje <- Sys.Date()
  
  # Reativo para carregar e processar os dados do SIOP
  df_siop_reactive <- reactive({
    # Exibe uma notificação enquanto aguarda o arquivo ou o carregamento
    
    
    df_loaded <- NULL
    
    # Extração de dados da API .
    if (is.null(df_loaded)) {
      showNotification("carregando. Extraindo dados da API SIOP (isso pode levar alguns minutos)...",
                       type = "message", duration = NULL, id = "api_loading_msg")
      Sys.sleep(5) # <--- VOCÊ PODERIA ADICIONAR UM DELAY AQUI TAMBÉM SE REATIVAR A API
      df_siop_IFPE <- despesaDetalhada(
        exercicio = anoCorrente,
        Esfera = TRUE, Orgao = TRUE, UO = "26418", Funcao = TRUE, Subfuncao = TRUE,
        Programa = TRUE, Acao = TRUE, PlanoOrcamentario = TRUE, Subtitulo = TRUE,
        CategoriaEconomica = TRUE, GND = TRUE, ModalidadeAplicacao = TRUE,
        ElementoDespesa = TRUE, Fonte = TRUE, IdUso = TRUE, ResultadoPrimario = TRUE,
        valorPLOA = TRUE, valorLOA = TRUE, valorLOAmaisCredito = TRUE,
        valorEmpenhado = TRUE, valorLiquidado = TRUE, valorPago = TRUE,
        incluiDescricoes = TRUE
      )
      df_siop_IFPE$Dt_extracao <- data_hoje
      df_siop_IFPE <- data.frame(df_siop_IFPE)
      removeNotification(id = "api_loading_msg")
      showNotification("Dados extraídos da API com sucesso!", type = "message") # <--- ALTERADO AQUI
      return(df_siop_IFPE)
    }
    # Em caso de erro da API Tenta carregar o dataframe a partir do arquivo enviado
    if (is.na(df_siop_IFPE)) {
      tryCatch({
        showNotification("Aguardando o upload do arquivo de dados ou carregando via API (se ativado)...",
                         type = "message", duration = NULL, id = "loading_msg")
        fileInput("file_upload", "Carregar arquivo de dados (.csv)",
                            accept = c(".csv", "text/csv")) # Aceita arquivos CSV
        # Lendo o arquivo CSV, garantindo que strings não sejam convertidas em fatores
        df_loaded <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE) # <--- ALTERAÇÃO AQUI
        # Adiciona um atraso de 3 segundos para simular um carregamento mais longo
        Sys.sleep(3) # <--- DELAY ADICIONADO AQUI
        if (!inherits(df_loaded, "data.frame")) {
          stop("O arquivo .csv carregado não contém um dataframe diretamente. Certifique-se de que o arquivo .csv contem um dataframe.")
        }
        removeNotification(id = "loading_msg")
        showNotification("Dados carregados com sucesso do arquivo CSV!", type = "message") # <--- ALTERADO AQUI
        return(df_loaded) # Retorna o dataframe carregado
      }, error = function(e) {
        removeNotification(id = "loading_msg")
        showNotification(paste("Erro ao carregar o arquivo CSV:", e$message,
                               "\nCertifique-se de que o formato do CSV (separador, decimal) está correto."),
                         type = "error", duration = 10)
        return(NULL) # Retorna NULL em caso de erro
      })
    }
    
    # Se nenhum arquivo for carregado ou houver erro, e você quiser manter a opção de extração via API:
   
    
    # Se nada for carregado (nem arquivo, nem API), retorne NULL e exiba uma mensagem para o usuário.
    if (is.null(df_loaded)) {
      removeNotification(id = "loading_msg")
      showNotification("Por favor, carregue um arquivo .csv contendo o dataframe 'df_siop_IFPE'.", type = "warning", duration = 5)
      return(NULL)
    }
    
    return(df_loaded)
  })
  
  # Reativo para a visão geral
  visaoGeral_reactive <- reactive({
    df_siop_IFPE <- req(df_siop_reactive()) # Requer que o dataframe esteja disponível
    visaoGeral <- df_siop_IFPE %>%
      summarise(LOA = sum(loa),
                LOAMaisCredito = sum(loa_mais_credito),
                Empenho = sum(empenhado),
                Liquidado = sum(liquidado),
                Pago = sum(pago))
    visaoGeral
  })
  
  # Renderiza a tabela de visão geral
  output$visaoGeralTable <- renderDT({
    visaoGeral <- visaoGeral_reactive()
    visaoGeral_formatted <- visaoGeral %>%
      mutate(across(c(LOA, LOAMaisCredito, Empenho, Liquidado, Pago),
                    ~ format(., nsmall = 2, big.mark = ".", decimal.mark = ",")))
    datatable(
      visaoGeral_formatted,
      caption = "Valores em R$",
      options = list(dom = 't', # Apenas a tabela, sem barra de busca ou paginação
                     paging = FALSE,
                     searching = FALSE,
                     columnDefs = list(list(className = 'dt-center', targets = '_all'))
      ),
      rownames = FALSE
    )
  })
  
  # Reativos para os cálculos dos gauges
  visaoGeral_ed <- reactive({
    visaoGeral <- visaoGeral_reactive()
    if (visaoGeral$LOAMaisCredito > 0) {
      100 * (visaoGeral$Empenho / visaoGeral$LOAMaisCredito)
    } else {
      0
    }
  })
  
  visaoGeral_pd <- reactive({
    visaoGeral <- visaoGeral_reactive()
    if (visaoGeral$LOAMaisCredito > 0) {
      100 * (visaoGeral$Pago / visaoGeral$LOAMaisCredito)
    } else {
      0
    }
  })
  
  visaoGeral_pe <- reactive({
    visaoGeral <- visaoGeral_reactive()
    if (visaoGeral$Empenho > 0) {
      100 * (visaoGeral$Pago / visaoGeral$Empenho)
    } else {
      0
    }
  })
  
  

  # Renderiza os gauges (usando renderUI para incorporar o HTML gerado por flexdashboard::gauge)
  output$gaugeED <- renderGauge({
    # Envolve a chamada a gauge com HTML() para garantir a compatibilidade com Shiny
    flexdashboard::gauge(visaoGeral_ed(), min = 0, max = 100, symbol = '%', label = "Empenhado/Dotação")
  })
  
  output$gaugePD <- renderGauge({
    # Envolve a chamada a gauge com HTML() para garantir a compatibilidade com Shiny
    flexdashboard::gauge(visaoGeral_pd(), min = 0, max = 100, symbol = '%', label = "Pago/Dotação")
  })
  
  output$gaugePE <- renderGauge({
    # Envolve a chamada a gauge com HTML() para garantir a compatibilidade com Shiny
    flexdashboard::gauge(visaoGeral_pe(), min = 0, max = 100, symbol = '%', label = "Pago/Empenhado")
  })
  
  # Reativo para a tabela por Função
  tb_Funcao_reactive <- reactive({
    df_siop_IFPE <- req(df_siop_reactive()) # Requer que o dataframe esteja disponível
    tb_Funcao <- df_siop_IFPE %>%
      group_by(Funcao_desc) %>%
      summarise(LOA = sum(loa),
                LOAMaisCredito = sum(loa_mais_credito),
                Empenho = sum(empenhado),
                Liquidado = sum(liquidado),
                Pago = sum(pago)) %>%
      ungroup() %>%
      mutate(
        efc_Empenho = round(100 * Empenho / LOAMaisCredito, 2),
        efc_Pago = round(100 * Pago / LOAMaisCredito, 2),
        efc_PagoEmpenho = round(100 * Pago / Empenho, 2)
      )
    tb_Funcao
  })
  
  # Renderiza a tabela por Função
  output$funcaoTable <- renderDT({
    tb_Funcao <- tb_Funcao_reactive()
    tb_Funcao_display <- tb_Funcao %>%
      select(Funcao_desc, LOA, LOAMaisCredito, Empenho, Liquidado, Pago) %>%
      mutate(across(c(LOA, LOAMaisCredito, Empenho, Liquidado, Pago),
                    ~ format(., nsmall = 2, big.mark = ".", decimal.mark = ",")))
    datatable(
      tb_Funcao_display,
      caption = "Valores em R$",
      options = list(pageLength = 10, # Número de linhas por página
                     lengthMenu = c(5, 10, 20, 50), # Opções de linhas por página
                     searching = TRUE, # Barra de busca
                     columnDefs = list(list(className = 'dt-left', targets = 0), # Alinha a descrição da função à esquerda
                                       list(className = 'dt-right', targets = c(1,2,3,4,5))) # Alinha os valores à direita
      ),
      rownames = FALSE
    )
  })
  
  # Renderiza a tabela de eficiência por Função (usando formattable e kable)
  output$funcaoEfcTable <- renderUI({
    tb_Funcao <- tb_Funcao_reactive()
    tb_Funcao_efc_html <- tb_Funcao %>%
      mutate(
        Funcao = Funcao_desc,
        Empenho_efc = color_bar("lightgreen")(efc_Empenho),
        Pago_efc = color_bar("lightgreen")(efc_Pago),
        PagoEmpenho_efc = color_bar("lightgreen")(efc_PagoEmpenho)
      ) %>%
      select(Funcao, Empenho_efc, Pago_efc, PagoEmpenho_efc) %>%
      kable("html", escape = F) %>%
      kable_styling("hover", full_width = F)
    HTML(as.character(tb_Funcao_efc_html))
  })
  
  # Reativo para a tabela por Ação
  tb_Acao_reactive <- reactive({
    df_siop_IFPE <- req(df_siop_reactive()) # Requer que o dataframe esteja disponível
    tb_Acao <- df_siop_IFPE %>%
      group_by(Acao_desc) %>%
      summarise(LOA = sum(loa),
                LOAMaisCredito = sum(loa_mais_credito),
                Empenho = sum(empenhado),
                Liquidado = sum(liquidado),
                Pago = sum(pago)) %>%
      ungroup() %>%
      mutate(
        efc_Empenho = round(100 * Empenho / LOAMaisCredito, 2),
        efc_Pago = round(100 * Pago / LOAMaisCredito, 2),
        efc_PagoEmpenho = round(100 * Pago / Empenho, 2)
      )
    tb_Acao
  })
  
  # Renderiza a tabela por Ação
  output$acaoTable <- renderDT({
    tb_Acao <- tb_Acao_reactive()
    tb_Acao_display <- tb_Acao %>%
      select(Acao_desc, LOA, LOAMaisCredito, Empenho, Liquidado, Pago) %>%
      mutate(across(c(LOA, LOAMaisCredito, Empenho, Liquidado, Pago),
                    ~ format(., nsmall = 2, big.mark = ".", decimal.mark = ",")))
    datatable(
      tb_Acao_display,
      caption = "Valores em R$",
      options = list(pageLength = 10,
                     lengthMenu = c(5, 10, 20, 50),
                     searching = TRUE,
                     columnDefs = list(list(className = 'dt-left', targets = 0),
                                       list(className = 'dt-right', targets = c(1,2,3,4,5)))
      ),
      rownames = FALSE
    )
  })
  
  # Renderiza a tabela de eficiência por Ação
  output$acaoEfcTable <- renderUI({
    tb_Acao <- tb_Acao_reactive()
    tb_Acao_efc_html <- tb_Acao %>%
      mutate(
        Acao = Acao_desc,
        Empenho_efc = color_bar("lightgreen")(efc_Empenho),
        Pago_efc = color_bar("lightgreen")(efc_Pago),
        PagoEmpenho_efc = color_bar("lightgreen")(efc_PagoEmpenho)
      ) %>%
      select(Acao, Empenho_efc, Pago_efc, PagoEmpenho_efc) %>%
      kable("html", escape = F) %>%
      kable_styling("hover", full_width = F)
    HTML(as.character(tb_Acao_efc_html))
  })
  
  # Exibe a data de geração no painel "Nota"
  output$dataGeracao <- renderText({
    paste("Relatório gerado em", format(data_hoje, "%d/%m/%Y"))
  })
  
  # Salvar o dataframe (opcional, pois no ambiente Shiny não é o local ideal para persistência de arquivos locais)
  # Este código foi removido do ambiente Shiny para evitar problemas de permissão e localização de arquivos
  # em ambientes de deploy, mas pode ser reintroduzido com cuidado se necessário (ex: download button)
  # observe({
  #   df_siop_IFPE_to_save <- df_siop_reactive()
  #   file_path_csv <- "BaseSIOP-IFPE.csv"
  #   write.csv(df_siop_IFPE_to_save, file = file_path_csv, row.names = TRUE)
  # })
}

# Cria e executa o aplicativo Shiny
shinyApp(ui = ui, server = server)

