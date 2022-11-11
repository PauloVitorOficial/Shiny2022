library(shinydashboard)
library(shiny)
library(ggplot2)
library(tm)
library(wordcloud)

ui=dashboardPage(
    skin = "red", # cor
    dashboardHeader(   # cabeçalho
        title = img(
            src="https://raw.githubusercontent.com/PauloVitorOficial/Shiny2022/main/LOGO_moodle.png",
            width= "200px",
            heigth = "100px"
        ),
        titleWidth ="300"
    ),
    
    sidebar = dashboardSidebar(  # barra lateral
        sidebarMenu(
            menuItem("Análise descritiva", tabName = "widgets", icon = icon("stats", lib = "glyphicon")),
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"),badgeLabel = "novo", badgeColor = "green"),
            menuItem("Metadados", icon = icon("file-code-o"),
                     href = "http://dados.ufop.br/dataset/0de458ea-0c19-4b01-a712-cd7d37d4ad87/resource/0e29b3e6-c911-4263-98ad-bf003bb86223/download/dicionario_de_dados_bolsistas_ic.pdf"),
            menuItem("Abrir página local dos dados", icon = icon("file-code-o"),
                     href = "http://dados.ufop.br/dataset/bolsistas-de-iniciacao-cientifica")
        )
    ),
    dashboardBody(    #corpo
        tabItems(
            tabItem(tabName = "dashboard",
                    h2("Iniciação Científica UFOP 2017-2021"),
                    
                    box(width = 2,
                        
                        selectInput("variavel", "Variável:", 
                                    choices=c( "ano"  ,"fomento","tipo_bolsa"))
                    ),
                    
                    
                    
                    box(
                        width = 5,
                        title = "Iniciações científicas",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        plotOutput("grafico1")
                    ),
                    
                    
                    box(width = 5,
                        title = "Apenas as iniciações científicas com bolsa",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        plotOutput("grafico2")
                    )
                    
                    
            ),
            
            tabItem(tabName = "widgets",
                    h2("Iniciação Científica UFOP 2017-2021"),
                    
                    box(
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        plotOutput("grafico3")
                    ),
                    box(
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        plotOutput("grafico4")
                    ),
                    box(
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        plotOutput("grafico5")
                    ),
                    box(
                        title = "Palavras mais frequentes para a variável Linha de Pesquisa",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        plotOutput("grafico6")
                    )
            )
        ) 
    )
)


server <- function(input,output){ 
    dados=read.csv("https://github.com/PauloVitorOficial/Shiny2022/raw/main/ictratada.csv")
    dados$curso_bolsista=factor(dados$curso_bolsista)
    dados$programa=factor(dados$programa)
    dados$ano=factor(dados$ano)
    dados$setor=factor(dados$setor)
    dados$fomento=factor(dados$fomento)
    dados$tipo_bolsa=factor(dados$tipo_bolsa)
    
    dados2=dados[dados$tipo_bolsa=="BOLSISTA",]
    
    output$grafico1 <- renderPlot(
            plot(dados[input$variavel],
             main=input$variavel,
             ylab="Número de bolsas"
             )
        )
        
    output$grafico2 <- renderPlot(
            plot(dados2[input$variavel],
                 main=input$variavel,
                 ylab="Número de bolsas"
            )
        )
    
    output$grafico3 <- renderPlot({
        ggplot(dados,aes(ano))+
            geom_bar(fill="steelblue")+
            theme_minimal()+
            ggtitle("Comparação do número de bolsas \n ao longo dos anos") +
            xlab("Ano") + ylab("Frequência")
    })
    
    output$grafico4 <- renderPlot({
        ggplot(dados,aes(fomento))+
            geom_bar(fill="steelblue")+
            theme_minimal()+
            ggtitle("Comparação do número de bolsas \n por fomento") +
            xlab("Fomento") + ylab("Frequência")
    })
    
    output$grafico5 <- renderPlot({
        ggplot(dados,aes(tipo_bolsa))+
            geom_bar(fill="steelblue")+
            theme_minimal()+
            ggtitle("Comparação do número de bolsas \n pelo tipo da bolsa") +
            xlab("Fomento") + ylab("Frequência")+
            scale_x_discrete(limits=c("BOLSISTA", "VOLUNTÁRIO"))
    })
    
        text <- (dados$linha_pesquisa)
        myCorpus = Corpus(VectorSource(text))
        myCorpus = tm_map(myCorpus, content_transformer(tolower))
        myCorpus = tm_map(myCorpus, removePunctuation)
        myCorpus = tm_map(myCorpus, removeNumbers)
        myCorpus = tm_map(myCorpus, removeWords,
                          c(stopwords("pt"),"linha","pesquisa","estudo","estudos","técnicas","desenvolvimento","ser"))
        myDTM = TermDocumentMatrix(myCorpus,
                                   control = list(minWordLength = 1))
        m = as.matrix(myDTM)
        x=sort(rowSums(m), decreasing = TRUE)
    
    output$grafico6 <- renderPlot({
        wordcloud(names(x), x,
                  min.freq = 200,
                  colors=brewer.pal(8, "Dark2"),main = "x")
    })
}

shinyApp(ui = ui, server = server)









    
   


    