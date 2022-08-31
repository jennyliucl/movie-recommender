library(shiny)
library(shinydashboard)
library(colourpicker)
library(tidyverse)
library(DT)
library(here)
library(slickR) # image slideshow
options(shiny.usecairo = FALSE)

# source: https://stackoverflow.com/questions/48210709/show-content-for-menuitem-when-menusubitems-exist-in-shiny-dashboard/62834634#62834634
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

# Define UI for application
ui <- dashboardPage(
  
  skin = "yellow",

  dashboardHeader(
    title = span(tagList(icon("imdb"), "Movie Recommender")),
    titleWidth = 350),
  
  # Sidebar content
  dashboardSidebar(
    width = 350,
    
    sidebarMenu(
      id = "sidebarid",

      menuItem("操作說明", tabName = "readme", icon = icon("book")),

      convertMenuItem(
        menuItem("推薦系統", tabName = "recommender", icon = icon("clapperboard"),
                 # color
                 menuSubItem(icon = NULL,
                             fluidRow(
                               column(7,colourInput("color", "顏色", palette = "limited", showColour = "background", allowedCols = c())), # will update
                               column(1,
                                      br(),
                                      br(),
                                      actionButton("refresh", "其他色彩", icon("rotate-left", lib="font-awesome")))
                             )
                 ),
                 
                 # age
                 menuSubItem(icon = NULL,
                             selectInput("age", "年齡", 
                                         choices = list("5歲以下" = 0, "6歲至11歲" = 6,
                                                        "12歲至17歲" = 12, "18歲以上" = 18), selected = 18)
                 ),
                 
                 # by genre or keyword
                 menuSubItem(icon = NULL,
                             radioButtons("bygork", "篩選依據",
                                          choices = list("電影類型" = 1, "電影關鍵字" = 2), selected = 1)
                 ),
                 
                 # genre
                 menuSubItem(icon = NULL,
                             conditionalPanel("input.bygork==1",
                                              selectInput("genre", "電影類型", 
                                                          choices = list("歡樂" = "a", "熱血" = "b", "劇情" = "c",
                                                                         "西部與戰爭" = "d", "傳記與歷史" = "e", "黑暗" = "f", 
                                                                         "音樂" = "g", "奇幻" = "h", "動畫" = "i"), selected = "a")
                             ),
                             
                 ),
                 
                 # keyword
                 menuSubItem(icon = NULL,
                             conditionalPanel("input.bygork==2",
                                              fluidRow(
                                                column(7,selectInput("keyword", "電影關鍵字", choices = "")), # will update
                                                column(1,
                                                       br(),
                                                       br(),
                                                       actionButton("refresh_keyword", "其他字詞", icon("rotate-left", lib="font-awesome")))
                                              )
                             )
                 ),
                 
                 # run
                 menuSubItem(icon = NULL,
                             fluidRow(
                               column(7),
                               column(1,actionButton("go", "開始推薦", icon("paper-plane"), style="color: #fff; background-color: #F39C12; border-color: #EA9111"))
                             )
                 )
        ),"recommender"), # end recommender
      
      menuItem("資料探索", tabName = "eda-home", icon = icon("chart-column"),
        menuSubItem("變數說明",tabName = "eda"),
        menuSubItem("EDA-年份",tabName = "year"),
        menuSubItem("EDA-分級",tabName = "certificate"),
        menuSubItem("EDA-種類",tabName = "genre"),
        menuSubItem("EDA-時長",tabName = "runtime"),
        menuSubItem("EDA-毛利",tabName = "gross"),
        menuSubItem("EDA-關鍵字",tabName = "keyword"),
        menuSubItem("EDA-海報色彩",tabName = "colour"),
        menuSubItem("EDA-評分用戶數",tabName = "votes"),
        menuSubItem("EDA-Meta Score",tabName = "meta"),
        menuSubItem("EDA-IMDb Rating",tabName = "imdb")
      ),
      
      menuItem("IMDb搜尋", icon = icon("magnifying-glass"), href = "https://www.imdb.com"),
      menuItem("開發人員", tabName = "developer", icon = icon("people-group"))
      
    ) # end sidebarMenu
  ), # end dashboardSidebar
  
  # Body content
  dashboardBody(
    
    # source: https://stackoverflow.com/questions/45706670/shiny-dashboadpage-lock-dashboardheader-on-top
    tags$script(HTML("$('body').addClass('fixed');")),

    tabItems(

      # 操作說明
      tabItem(tabName = "readme",
              h2("歡迎！"),
              br(),
              p("想看電影卻又不知從何下手？", align = "center"),
              p("沒心情翻閱網路上的電影推薦文章？", align = "center"),
              p("那請一定要來試試我們的",strong("電影推薦系統！"), align = "center"),
              br(),
              br(),
              p("用輕鬆的心情選擇喜歡的顏色", align = "center"),
              p("確認年齡後", align = "center"),
              p("挑選一個感興趣的電影類別或關鍵字", align = "center"),
              p(strong("顏色 ➡︎ 年齡 ➡ 電影︎類別 / 關鍵字"), align = "center"),
              p("簡單三步驟", align = "center"),
              p("就從",strong("IMDb評分前1000名"),"的電影中", align = "center"),
              p("推薦合適的五部高分電影給您", align = "center"),
              br(),
              br(),
              p("快來感受", align = "center"),
              h4(strong("用喜好色彩找到海報顏色相近的電影"), align = "center", style = "color: #F39C12"),
              p("這截然不同的推薦體驗吧", align = "center"), #now
      ),
      
      # 推薦系統
      tabItem(tabName = "recommender",
              h2("Top 1000 Movies by IMDb Rating"),
              h4("推薦電影清單"),
              br(),
              DTOutput("filter_table")
      ),
      
      # 資料探索
      tabItem(tabName = "eda",
              h2("Variables"),
              h4("Dataset", align = "left"),
              p("Download from",a("kaggle", href = "https://www.kaggle.com/datasets/harshitshankhdhar/imdb-dataset-of-top-1000-movies-and-tv-shows")),
              h4("Codebook", align = "left"),
              img(src="codebook.png", height = 550, width = 500),
              h4("Correlation Matrix", align = "left"),
              img(src="correlation.png", height = 300, width = 500)
      ),
      
      tabItem(tabName = "year",
              h2("Year"),
              h4("Released Year", align = "left"),
              img(src="year.png", height = 400, width = 500),
              br(),
              img(src="year_minmax.png", height = 50, width = 100),
              br(),
              img(src="year2.png", height = 50, width = 500),
              h4("Movie Year Interval", align = "left"),
              img(src="yearinterval.png", height = 350, width = 500),
              br(),
              img(src="year1.png", height = 30, width = 400),
              h4("Year vs Runtime", align = "left"),
              img(src="time_vs_year.png", height = 400, width = 500),
              br(),
              img(src="time_vs_year1.png", height = 30, width = 170),
              h4("Year vs Genre", align = "left"),
              img(src="year_genre.png", height = 350, width = 500),
              br(),
              img(src="year_genre1.png", height = 35, width = 500),
              h4("Year vs Votes", align = "left"),
              img(src="year_vs_votes.png", height = 400, width = 500)
      ),
      tabItem(tabName = "certificate",
              h2("Certificate"),
              h4("Certificate", align = "left"),
              img(src="certificate.png", height = 450, width = 500),
              h4("Certificate vs Genre", align = "left"),
              img(src="certificate1.png", height = 300, width = 500),
              img(src="certificate2.png", height = 300, width = 500),
              img(src="certificate3.png", height = 300, width = 500),
              img(src="certificate4.png", height = 300, width = 500),
              br(),
              img(src="certificate5.png", height = 75, width = 500),
              h4("Certificate vs Runtime", align = "left"),
              img(src="certificate_time.png", height = 400, width = 500),
              br(),
              img(src="certificate_time1.png", height = 60, width = 500),
              h4("Certificate vs IMDB Rating", align = "left"),
              img(src="certificate_imdb.png", height = 500, width = 500),
              img(src="certificate_imdb1.png", height = 60, width = 500),
              h4("Certificate vs Gross", align = "left"),
              img(src="certificate_gross.png", height = 350, width = 500),
              br(),
              img(src="certificate_gross1.png", height = 75, width = 500),
              h4("Colour vs Certificate", align = "left"),
              img(src="color_vs_certificate.png", height = 400, width = 500),
              br(),
              img(src="color_vs_certificate1.png", height = 35, width = 400),
              h4("Certificate vs IMDB Rating vs Meta score", align = "left"),
              img(src="certificate_score.png", height = 300, width = 500),
              br(),
              img(src="certificate_score1.png", height = 50, width = 450)
      ),
      tabItem(tabName = "runtime",
              h2("Runtime"),
              h4("Runtime", align = "left"),
              img(src="time.png", height = 400, width = 500),
              br(),
              img(src="time1.png", height = 50, width = 300),
              br(),
              img(src="time2.png", height = 50, width = 350),
              h4("Year vs Runtime", align = "left"),
              img(src="time_vs_year.png", height = 400, width = 500),
              br(),
              img(src="time_vs_year1.png", height = 30, width = 170),
              h4("Certificate vs Runtime", align = "left"),
              img(src="certificate_time.png", height = 400, width = 500),
              br(),
              img(src="certificate_time1.png", height = 60, width = 500)
      ),
      tabItem(tabName = "genre",
              h2("Genre"),
              h4("Genre Table", align = "left"),
              img(src="genre.png", height = 350, width = 300),
              br(),
              img(src="genre2.png", height = 30, width = 450),
              h4("Genre", align = "left"),
              img(src="genre1.png", height = 350, width = 500),
              br(),
              img(src="genre3.png", height = 30, width = 400),
              h4("Year vs Genre", align = "left"),
              img(src="year_genre.png", height = 350, width = 500),
              br(),
              img(src="year_genre1.png", height = 35, width = 500),
              h4("Certificate vs Genre", align = "left"),
              img(src="certificate1.png", height = 300, width = 500),
              img(src="certificate2.png", height = 300, width = 500),
              img(src="certificate3.png", height = 300, width = 500),
              img(src="certificate4.png", height = 300, width = 500),
              br(),
              img(src="certificate5.png", height = 75, width = 500),
              h4("IMDB Rating vs Genre", align = "left"),
              img(src="imdb_vs_genre.png", height = 400, width = 500),
              br(),
              img(src="imdb_genre1.png", height = 70, width = 500),
              h4("IMDB Rating vs Genre", align = "left"),
              img(src="imdb_genre_box_violin.png", height = 400, width = 500),
              br(),
              img(src="imdb_genre_box_violin1.png", height = 70, width = 500),
              h4("Genre vs Votes", align = "left"),
              img(src="genre_votes.png", height = 400, width = 500),
              br(),
              img(src="genre_votes1.png", height = 65, width = 500),
              h4("Genre vs RGB", align = "left"),
              img(src="rgb_vs_genre.png", height = 300, width = 500),
              br(),
              img(src="rgb_vs_genre1.png", height = 90, width = 500),
              h4("Genre vs RGB", align = "left"),
              img(src="rgb_vs_genre3.png", height = 300, width = 500),
              br(),
              img(src="rgb_vs_genre4.png", height = 90, width = 500),
              h4("Colour vs Genre", align = "left"),
              img(src="color_vs_genre3.png", height = 300, width = 550),
              br(),
              img(src="color_vs_genre4.png", height = 320, width = 300),
              h4("Colour vs Genre", align = "left"),
              img(src="color_vs_genre.png", height = 400, width = 500),
              img(src="color_vs_genre1.png", height = 400, width = 500),
              img(src="color_vs_genre5.png", height = 400, width = 500),
              br(),
              img(src="color_vs_genre2.png", height = 50, width = 500)
      ),
      tabItem(tabName = "imdb",
              h2("IMDB_Rating"),
              h4("IMDB_Rating", align = "left"),
              img(src="imdb.png", height = 450, width = 500),
              br(),
              img(src="imdb_mean1.png", height = 50, width = 400),
              br(),
              img(src="imdb_mean3.png", height = 65, width = 350),
              h4("Certificate vs IMDB Rating", align = "left"),
              img(src="certificate_imdb.png", height = 500, width = 500),
              br(),
              img(src="certificate_imdb1.png", height = 60, width = 500),
              h4("IMDB Rating vs Genre", align = "left"),
              img(src="imdb_vs_genre.png", height = 350, width = 500),
              br(),
              img(src="imdb_genre1.png", height = 70, width = 500),
              h4("IMDB Rating vs Genre", align = "left"),
              img(src="imdb_genre_box_violin.png", height = 350, width = 500),
              br(),
              img(src="imdb_genre_box_violin1.png", height = 70, width = 500),
              h4("IMDB Rating vs Meta Score", align = "left"),
              img(src="imdb_vs_meta.png", height = 400, width = 500),
              br(),
              img(src="imdb_vs_meta1.png", height = 50, width = 500),
              h4("IMDB Rating vs Gross", align = "left"),
              img(src="imdb_vs_gross.png", height = 400, width = 500),
              h4("Certificate vs IMDB Rating vs Meta score", align = "left"),
              img(src="certificate_score.png", height = 300, width = 500),
              br(),
              img(src="certificate_score1.png", height = 50, width = 450)
      ),
      tabItem(tabName = "meta",
              h2("Meta score"),
              h4("Meta score", align = "left"),
              img(src="meta_score.png", height = 400, width = 500),
              h4("IMDB Rating vs Meta score", align = "left"),
              img(src="imdb_vs_meta.png", height = 400, width = 500),
              br(),
              img(src="imdb_vs_meta1.png", height = 50, width = 500),
              h4("Meta score vs Gross", align = "left"),
              img(src="meta_vs_gross.png", height = 400, width = 500),
              h4("Certificate vs IMDB Rating vs Meta score", align = "left"),
              img(src="certificate_score.png", height = 300, width = 500),
              br(),
              img(src="certificate_score1.png", height = 50, width = 450)
      ),
      tabItem(tabName = "votes",
              h2("No of votes"),
              h4("Number of votes", align = "left"),
              img(src="vote.png", height = 400, width = 500),
              br(),
              br(),
              img(src="votes1.png", height = 50, width = 400),
              br(),
              img(src="votes2.png", height = 50, width = 400),
              h4("Year vs Votes", align = "left"),
              img(src="year_vs_votes.png", height = 400, width = 500),
              h4("Genre vs Votes", align = "left"),
              img(src="genre_votes.png", height = 400, width = 500),
              br(),
              img(src="genre_votes1.png", height = 65, width = 500),
              h4("Votes vs Gross", align = "left"),
              img(src="votes_vs_gross.png", height = 400, width = 500),
              br(),
              img(src="votes_vs_gross1.png", height = 50, width = 500)
      ),
      tabItem(tabName = "gross",
              h2("Gross"),
              h4("Gross", align = "left"),
              img(src="gross.png", height = 400, width = 500),
              h4("Certificate vs Gross", align = "left"),
              img(src="certificate_gross.png", height = 350, width = 500),
              br(),
              img(src="certificate_gross1.png", height = 75, width = 500),
              h4("IMDB Rating vs Gross", align = "left"),
              img(src="imdb_vs_gross.png", height = 400, width = 500),
              h4("Meta Score vs Gross", align = "left"),
              img(src="meta_vs_gross.png", height = 400, width = 500),
              h4("Votes vs Gross", align = "left"),
              img(src="votes_vs_gross.png", height = 400, width = 500),
              br(),
              img(src="votes_vs_gross1.png", height = 50, width = 500)
      ),
      tabItem(tabName = "colour",
              h2("Poster Colour"),
              img(src="poster.png", height = 350, width = 500),
              br(),
              img(src="poster1.png", height = 50, width = 500),
              h4("RGB", align = "left"),
              img(src="poster_rgb.png", height = 350, width = 500),
              br(),
              img(src="poster_rgb1.png", height = 30, width = 350),
              h4("3000 colours", align = "left"),
              img(src="3000.png", height = 300, width = 500),
              br(),
              img(src="3000_1.png", height = 30, width = 400),
              h4("Colours count", align = "left"),
              img(src="color2.png", height = 300, width = 500),
              br(),
              img(src="color3.png", height = 50, width = 500),
              h4("Genre vs RGB", align = "left"),
              img(src="rgb_vs_genre.png", height = 300, width = 500),
              br(),
              img(src="rgb_vs_genre1.png", height = 90, width = 500),
              h4("Genre vs RGB", align = "left"),
              img(src="rgb_vs_genre3.png", height = 300, width = 500),
              br(),
              img(src="rgb_vs_genre4.png", height = 90, width = 500),
              h4("Colour vs Genre", align = "left"),
              img(src="color_vs_genre3.png", height = 300, width = 550),
              img(src="color_vs_genre4.png", height = 320, width = 300),
              h4("Colour", align = "left"),
              img(src="color.png", height = 350, width = 500),
              br(),
              img(src="color1.png", height = 35, width = 500),
              h4("Colour vs Certificate", align = "left"),
              img(src="color_vs_certificate.png", height = 400, width = 500),
              br(),
              img(src="color_vs_certificate1.png", height = 35, width = 400),
              h4("Colour vs Genre", align = "left"),
              img(src="color_vs_genre.png", height = 400, width = 500),
              img(src="color_vs_genre1.png", height = 400, width = 500),
              img(src="color_vs_genre5.png", height = 400, width = 500),
              br(),
              img(src="color_vs_genre2.png", height = 50, width = 500),
              br(),
              h4("All Posters's Three Dominant Colors", align = "left"),
              slickROutput("slideshow", width="648px")
      ),
      tabItem(tabName = "keyword",
              h2("Keyword"),
              h4("tf-idf", align = "left"),
              img(src="keywords.png", height = 350, width = 500),
              h4("One-gram Word Cloud", align = "left"),
              img(src="keywords1.png", height = 400, width = 400)
      ),
      
      # 開發人員
      tabItem(tabName = "developer",
              h2("2022政大暑期電腦課程工作坊專題製作"),
              br(),
              h4("小組名單"),
              br(),
              tableOutput("name"),
              br(),
              h4("特別感謝"),
              br(),
              tableOutput("thanks"),
              br(),
              "註：依姓名筆畫排序"
      )
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage


# Define server
server <- function(input, output, session) {
  
  # read dataset
  df = read.csv("IMDBdataset-utf8.csv")
  key = read.csv("key.csv")
  
  # sample 8 color
  colorname = c(df$colorname1,df$colorname2,df$colorname3)
  hex = c(df$hex1,df$hex2,df$hex3)
  allcolor = data.frame(cbind(colorname,hex))
  red = allcolor %>% filter(colorname=="red") %>% select(hex) %>% unlist %>% as.vector
  ora = allcolor %>% filter(colorname=="orange") %>% select(hex) %>% unlist %>% as.vector
  yel = allcolor %>% filter(colorname=="yellow") %>% select(hex) %>% unlist %>% as.vector
  gre = allcolor %>% filter(colorname=="green") %>% select(hex) %>% unlist %>% as.vector
  blu = allcolor %>% filter(colorname=="blue") %>% select(hex) %>% unlist %>% as.vector
  pur = allcolor %>% filter(colorname=="purple") %>% select(hex) %>% unlist %>% as.vector
  bla = allcolor %>% filter(colorname=="black") %>% select(hex) %>% unlist %>% as.vector
  whi = allcolor %>% filter(colorname=="white") %>% select(hex) %>% unlist %>% as.vector
  
  addcolor <- observe({
    updateColourInput(session, inputId = "color", label = "顏色", palette = "limited", showColour = "background", allowedCols = c(sample(red,1),sample(ora,1),sample(yel,1),sample(gre,1),
                                                                                                                                sample(blu,1),sample(pur,1),sample(bla,1),sample(whi,1)))
  })
  
  # refresh color
  observe({ 
    if (input$refresh >0){
      changecolor <- observe({
        updateColourInput(session, inputId = "color", label = "顏色", palette = "limited", showColour = "background", allowedCols = c(sample(red,1),sample(ora,1),sample(yel,1),sample(gre,1),
                                                                                                                                    sample(blu,1),sample(pur,1),sample(bla,1),sample(whi,1)))
      })
    }
  })
  
  # sample 9 keyword
  observe({
    if (input$color %in% df[,61] | input$color %in% df[,62] | input$color %in% df[,63]){
      # check which color
      if (input$color %in% df[,61]){
        j = 61
      } else if (input$color %in% df[,62]){
        j = 62
      } else if (input$color %in% df[,63]){
        j = 63
      }
      # get black, white, red, ...
      color_key = df[which(df[,j]==input$color)[1],(j-3)]
      # get the column of keyword
      keywords = key %>% select(!!as.symbol(color_key)) %>% filter(!!as.symbol(color_key) != "") %>% unlist %>% as.vector
      # update input
      addkeyword <- updateSelectInput(session, "keyword", "電影關鍵字", choices = c(sample(keywords,9)))
      # refresh keyword
      if (input$refresh_keyword >0){
        addkeyword <- observe({
          updateSelectInput(session, "keyword", "電影關鍵字", choices = c(sample(keywords,9)))
        })
      }
    }
  })

  # ciede2000 function from ColorNameR package
  ciede2000 <- function(lab_color1, lab_color2, k_L=1, k_C=1, k_H=1) {
    L1 <- lab_color1[1]
    a1 <- lab_color1[2]
    b1 <- lab_color1[3]
    L2 <- lab_color2[1]
    a2 <- lab_color2[2]
    b2 <- lab_color2[3]
    
    C1 <- base::sqrt(a1^2 + b1^2)
    C2 <- base::sqrt(a2^2 + b2^2)
    C_bar <- (C1 + C2) / 2
    G <- (1 - base::sqrt(C_bar^7 / (C_bar^7 + 25^7))) / 2
    a1p <- (1 + G) * a1
    a2p <- (1 + G) * a2
    C1p <- base::sqrt(a1p^2 + b1^2)
    C2p <- base::sqrt(a2p^2 + b2^2)
    h1p <- base::atan2(b1, a1p) %% (2*base::pi)
    h2p <- base::atan2(b2, a2p) %% (2*base::pi)
    
    delta_Lp <- L2 - L1
    delta_Cp <- C2p - C1p
    delta_hp <- base::ifelse(C1p * C2p != 0,
                             base::ifelse(base::abs(h2p - h1p) <= base::pi,
                                          h2p - h1p,
                                          h2p - h1p + base::sign(h1p - h2p) * 2 * base::pi),
                             0)
    delta_Hp <- 2 * base::sqrt(C1p * C2p) * base::sin(delta_hp / 2)
    
    Lp_bar <- (L1 + L2) / 2
    Cp_bar <- (C1p + C2p) / 2
    hp_bar <- base::ifelse(C1p * C2p != 0,
                           base::ifelse(base::abs(h2p - h1p) <= base::pi,
                                        (h1p + h2p) / 2,
                                        -(h1p + h2p + base::sign(2 * base::pi - h1p - h2p) * 2 * base::pi) / 2),
                           h1p + h2p)
    
    deg2rad <- base::pi / 180
    rad2deg <- 1 / deg2rad
    Tp <- (1 - 0.17 * base::cos(hp_bar - 30 * deg2rad)
           + 0.24 * base::cos(2 * hp_bar)
           + 0.32 * base::cos(3 * hp_bar + 6 * deg2rad)
           - 0.20 * base::cos(4 * hp_bar - 63 * deg2rad))
    delta_theta <- 30 * deg2rad * base::exp(- ((hp_bar * rad2deg - 275) / 25)^2)
    R_C <- 2 * base::sqrt(Cp_bar^7 / (Cp_bar^7 + 25^7))
    S_L <- 1 + ((0.015 * (Lp_bar - 50)^2) / base::sqrt(20 + (Lp_bar - 50)^2))
    S_C <- 1 + 0.045 * Cp_bar
    S_H <- 1 + 0.015 * Cp_bar * Tp
    R_T <- - base::sin(2 * delta_theta) * R_C
    
    term1 <- delta_Lp / (k_L * S_L)
    term2 <- delta_Cp / (k_C * S_C)
    term3 <- delta_Hp / (k_H * S_H)
    term4 <- R_T * term2 * term3
    
    base::sqrt(term1^2 + term2^2 + term3^2 + term4)
  }
  
  # color difference and filter
  observe({
    if (input$go >= 1){
      
      # check which color
      if (input$color %in% df[,61]){
        j = 61
      } else if (input$color %in% df[,62]){
        j = 62
      } else if (input$color %in% df[,63]){
        j = 63
      }
      
      # calculate color difference
      diff_1 = rep(NA,1000)
      diff_2 = rep(NA,1000)
      diff_3 = rep(NA,1000)
      min_diff = rep(NA,1000)
      for (i in 1:1000){
        diff_1[i] = ciede2000(which(df[,j]==input$color)[1] %>% df[., 23:25] %>% unlist %>% as.vector,
                              df[i, 23:25] %>% unlist %>% as.vector)
        diff_2[i] = ciede2000(which(df[,j]==input$color)[1] %>% df[., 23:25] %>% unlist %>% as.vector,
                              df[i, 26:28] %>% unlist %>% as.vector)
        diff_3[i] = ciede2000(which(df[,j]==input$color)[1] %>% df[., 23:25] %>% unlist %>% as.vector,
                              df[i, 29:31] %>% unlist %>% as.vector)
        min_diff[i] = min(diff_1[i],diff_2[i],diff_3[i])
      }
      
      # filter
      filter_df <- df %>% 
        mutate(color_1_diff = diff_1,
               color_2_diff = diff_2,
               color_3_diff = diff_3,
               min_color_diff = min_diff) %>%
        filter(certificate_age<=as.numeric(input$age)) %>%
        arrange(min_color_diff)
      
      if (input$bygork==1){
        filter_df <- filter_df %>% 
          filter(!!as.symbol(input$genre)==1)
      } else if (input$bygork==2){
        filter_df <- filter_df %>% 
          filter(!!as.symbol(input$keyword)==1)
      }

      # show output table
      output$filter_table = DT::renderDataTable({
        datatable(
          filter_df[1:5,] %>%
            arrange(desc(IMDB_Rating)) %>%
            mutate(poster = map_chr(file_name, ~ as.character(img(src = .x, height = "98px", width = "67px")))) %>%
            select("海報"=poster, "電影名稱"=series_title, "年份"=released_year, "分級"=certificate, "種類"=genre,"簡介"=overview, "IMDB Rating"=IMDB_Rating),
          escape = FALSE, # source: https://stackoverflow.com/questions/72458575/r-shiny-download-multiple-local-images-in-zip-file
          options = list(autoWidth = TRUE, 
                         columnDefs = list(list(width = "10%", className = "dt-center", targets = c(1,3,4,7)),
                                           list(width = "15%", targets = c(2)),
                                           list(width = "15%", targets = c(5)),
                                           list(width = "30%", targets = c(6))
                                           ),
                         scrollX = TRUE)
        )
      }
    )}
  })
  
  # image slideshow
  dominant_color_list = paste("dominant_color/", df$dominant_color_file, sep = "")
  output$slideshow <- renderSlickR({
    imgs <- dominant_color_list
    slickR(imgs)
  })
  
  # name list and thanks
  output$name = renderTable(
    data.frame(`姓名` = c("温展德（組長）","林柏辰","陳玟儒","黃宥芯","黃詩涵","劉貞莉"),
               `分工` = c("單變數分析、上台報告","處理電影分級、建置推薦系統","處理電影類型、建置推薦系統","單變數分析、建置推薦系統","處理摘要斷詞、上台報告","處理海報色彩、建置推薦系統"))
  )
  output$thanks =  renderTable(
    data.frame(`國立政治大學統計學系` = c("","","","",""),
               `余清祥教授` = c("呂靖翎學長","張君瑋學長","陳慧霜學姊","廖靖芸學姊","簡廷儒學長"))
  )

}


# Run the application 
shinyApp(ui = ui, server = server)
