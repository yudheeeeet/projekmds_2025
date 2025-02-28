library(shiny)
library(RMySQL)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)

# Konfigurasi koneksi database
dbConfig <- list(
  host = "127.0.0.1",
  port = 3309,          # Port MySQL di DBngin
  user = "root",
  password = "",
  dbname = "mds_db" 
)

# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Football Analytics Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Pemain", tabName = "players", icon = icon("user")),
      menuItem("Klub", tabName = "clubs", icon = icon("shield")),
      menuItem("Transfer", tabName = "transfers", icon = icon("exchange")),
      menuItem("Pertandingan", tabName = "matches", icon = icon("futbol"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box {border-radius: 5px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);}
        .bg-blue {background-color: #3c8dbc !important; color: white !important;}
        .bg-green {background-color: #00a65a !important; color: white !important;}
        .bg-yellow {background-color: #f39c12 !important; color: white !important;}
        .bg-red {background-color: #dd4b39 !important; color: white !important;}
        .value-box {transition: transform 0.3s; cursor: pointer;}
        .value-box:hover {transform: translateY(-5px);}
        table.dataTable thead th {background-color: #3c8dbc; color: white;}
        .nav-tabs-custom {box-shadow: 0 2px 10px rgba(0,0,0,0.1);}
      "))
    ),
    tabItems(
      # Dashboard tab
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("totalPlayersBox", width = 3),
                valueBoxOutput("totalClubsBox", width = 3),
                valueBoxOutput("totalTransfersBox", width = 3),
                valueBoxOutput("totalGamesBox", width = 3)
              ),
              fluidRow(
                box(
                  title = "Top Pemain by Gol", status = "primary", solidHeader = TRUE,
                  plotlyOutput("topScorersPlot", height = 300)
                ),
                box(
                  title = "Distribusi Pertandingan per Musim", status = "success", solidHeader = TRUE,
                  plotlyOutput("matchesPerSeasonPlot", height = 300)
                )
              ),
              fluidRow(
                box(
                  title = "Statistik Klub Terkini", width = 12, status = "info",
                  DTOutput("clubStatsTable")
                )
              )
      ),
      
      # Pemain tab
      tabItem(tabName = "players",
              fluidRow(
                column(3,
                       box(
                         title = "Filter", width = NULL, status = "primary", solidHeader = TRUE,
                         selectInput("playerNationality", "Pilih Negara:", choices = c("Semua", "England", "Spain", "France", "Germany", "Italy", "Brazil", "Argentina")),
                         numericInput("minGoals", "Minimum Gol:", value = 5)
                       )
                ),
                column(9,
                       tabBox(
                         title = "Data Pemain", width = NULL,
                         tabPanel("Tabel", DTOutput("playerTable")),
                         tabPanel("Grafik Gol", plotlyOutput("playerGoalsPlot")),
                       )
                )
              )
      ),
      
      # Klub tab
      tabItem(tabName = "clubs",
              fluidRow(
                column(3,
                       box(
                         title = "Filter", width = NULL, status = "success", solidHeader = TRUE,
                         selectInput("clubCompetition", "Kompetisi:", choices = c("Semua", "Premier League", "La Liga", "Bundesliga", "Serie A", "Ligue 1")),
                         sliderInput("squadSizeRange", "Ukuran Squad:", min = 15, max = 50, value = c(20, 35))
                       )
                ),
                column(9,
                       tabBox(
                         title = "Data Klub", width = NULL,
                         tabPanel("Tabel", DTOutput("clubTable")),
                         tabPanel("Rata-rata Usia", plotlyOutput("clubAgePlot")),
                         tabPanel("Ukuran Squad", plotlyOutput("squadSizePlot"))
                       )
                )
              )
      ),
      
      # Transfer tab
      tabItem(tabName = "transfers",
              fluidRow(
                column(3,
                       box(
                         title = "Filter", width = NULL, status = "warning", solidHeader = TRUE,
                         dateRangeInput("transferDateRange", "Rentang Tanggal:", 
                                        start = "2020-01-01", end = Sys.Date()),
                         selectInput("transferType", "Jenis Transfer:", 
                                     choices = c("Semua", "Permanent", "Loan"))
                       )
                ),
                column(9,
                       tabBox(
                         title = "Data Transfer", width = NULL,
                         tabPanel("Tabel", DTOutput("transferTable")),
                         tabPanel("Tren Waktu", plotlyOutput("transferTimePlot")),
                         tabPanel("Transfer per Klub", plotlyOutput("transferClubPlot"))
                       )
                )
              )
      ),
      
      # Pertandingan tab
      tabItem(tabName = "matches",
              fluidRow(
                column(3,
                       box(
                         title = "Filter", width = NULL, status = "danger", solidHeader = TRUE,
                         selectInput("matchSeason", "Musim:", 
                                     choices = c("Semua", "2023/2024", "2022/2023", "2021/2022", "2020/2021")),
                         pickerInput("selectClubs", "Pilih Klub:", 
                                     choices = c("All Clubs" = "", "Manchester United", "Liverpool", "Arsenal", "Barcelona", "Real Madrid"),
                                     multiple = TRUE,
                                     options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3"))
                       )
                ),
                column(9,
                       tabBox(
                         title = "Data Pertandingan", width = NULL,
                         tabPanel("Tabel", DTOutput("matchTable")),
                         tabPanel("Skor per Musim", plotlyOutput("scoreDistributionPlot")),
                         tabPanel("Performa Tim", plotlyOutput("teamPerformancePlot"))
                       )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Fungsi koneksi database
  getData <- function(query) {
    con <- dbConnect(MySQL(),
                     host = dbConfig$host,
                     port = dbConfig$port,
                     user = dbConfig$user,
                     password = dbConfig$password,
                     dbname = dbConfig$dbname)
    on.exit(dbDisconnect(con))
    
    dbGetQuery(con, query)
  }
  
  # Data-data dasar untuk dashboard
  totalPlayers <- reactive({
    getData("SELECT COUNT(DISTINCT player_id) AS total FROM players;")$total
  })
  
  totalClubs <- reactive({
    getData("SELECT COUNT(DISTINCT club_id) AS total FROM clubs;")$total
  })
  
  totalTransfers <- reactive({
    getData("SELECT COUNT(*) AS total FROM transfers;")$total
  })
  
  totalGames <- reactive({
    getData("SELECT COUNT(*) AS total FROM games;")$total
  })
  
  # Value boxes
  output$totalPlayersBox <- renderValueBox({
    valueBox(
      totalPlayers(), "Total Pemain", icon = icon("users"),
      color = "blue"
    )
  })
  
  output$totalClubsBox <- renderValueBox({
    valueBox(
      totalClubs(), "Total Klub", icon = icon("shield"),
      color = "green"
    )
  })
  
  output$totalTransfersBox <- renderValueBox({
    valueBox(
      totalTransfers(), "Total Transfer", icon = icon("exchange"),
      color = "yellow"
    )
  })
  
  output$totalGamesBox <- renderValueBox({
    valueBox(
      totalGames(), "Total Pertandingan", icon = icon("futbol"),
      color = "red"
    )
  })
  
  # Top Scorers Plot
  output$topScorersPlot <- renderPlotly({
    data <- getData("
      SELECT 
        ANY_VALUE(p.name) AS name,
        SUM(a.goals) AS total_goals
      FROM players p
      JOIN appearances a ON p.player_id = a.player_id
      GROUP BY p.player_id
      ORDER BY total_goals DESC
      LIMIT 200;
    ")
    
    plot_ly(data, x = ~name, y = ~total_goals, type = 'bar', 
            marker = list(color = '#3c8dbc')) %>%
      layout(title = "Top 10 Pencetak Gol",
             xaxis = list(title = ""),
             yaxis = list(title = "Total Gol"))
  })
  
  # Matches Per Season Plot
  output$matchesPerSeasonPlot <- renderPlotly({
    data <- getData("
      SELECT 
        season,
        COUNT(*) as total_matches
      FROM games
      GROUP BY season
      ORDER BY season;
    ")
    
    plot_ly(data, x = ~season, y = ~total_matches, type = 'bar',
            marker = list(color = '#00a65a')) %>%
      layout(title = "Jumlah Pertandingan per Musim",
             xaxis = list(title = "Musim"),
             yaxis = list(title = "Jumlah Pertandingan"))
  })
  
  # Club Stats Table
  output$clubStatsTable <- renderDT({
    data <- getData("
      SELECT 
        c.name AS club,
        ANY_VALUE(comp.name) AS league,
        c.squad_size,
        c.average_age,
        COUNT(p.player_id) AS total_players
      FROM clubs c
      JOIN competitions comp ON c.domestic_competition_id = comp.competition_id
      LEFT JOIN players p ON c.club_id = p.current_club_id
      GROUP BY c.club_id, c.name, c.squad_size, c.average_age
      ORDER BY total_players DESC
      LIMIT 200;
    ")
    
    datatable(data, 
              options = list(pageLength = 5, 
                             dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf'),
                             scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle('league', 
                  backgroundColor = styleEqual(
                    c('Premier League', 'La Liga', 'Bundesliga', 'Serie A', 'Ligue 1'), 
                    c('#3771C8', '#FF9300', '#D3010C', '#008000', '#002F6C')
                  ),
                  color = 'white',
                  fontWeight = 'bold')
  })
  
  # Pemain Tab
  playerData <- reactive({
    query <- "
      SELECT 
        p.player_id,
        p.name,
        p.country_of_birth,
        ANY_VALUE(c.name) AS current_club,
        SUM(a.goals) AS total_goals,
        SUM(a.assists) AS total_assists,
        SUM(a.minutes_played) AS total_minutes
      FROM players p
      LEFT JOIN clubs c ON p.current_club_id = c.club_id
      LEFT JOIN appearances a ON p.player_id = a.player_id
      GROUP BY p.player_id, p.name, p.country_of_birth
      HAVING 1=1
    "
    
    if (input$playerNationality != "Semua") {
      query <- paste0(query, " AND p.country_of_birth = '", input$playerNationality, "'")
    }
    
    query <- paste0(query, " AND total_goals >= ", input$minGoals)
    query <- paste0(query, " ORDER BY total_goals DESC LIMIT 500;")
    
    getData(query)
  })
  
  output$playerTable <- renderDT({
    datatable(playerData(),
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$playerGoalsPlot <- renderPlotly({
    data <- playerData()[1:15,]
    
    plot_ly(data, x = ~name, y = ~total_goals, type = 'bar', 
            marker = list(color = '#3c8dbc'),
            hoverinfo = "text",
            text = ~paste(name, "<br>", 
                          "Gol:", total_goals, "<br>",
                          "Assist:", total_assists, "<br>",
                          "Klub:", current_club)) %>%
      layout(title = "Top 15 Pencetak Gol",
             xaxis = list(title = ""),
             yaxis = list(title = "Total Gol"))
  })
  
  # Klub Tab
  clubData <- reactive({
    query <- "
    SELECT 
      c.club_id,
      c.name,
      ANY_VALUE(comp.name) AS league,
      c.squad_size,
      c.average_age,
      COUNT(DISTINCT p.player_id) AS total_players,
      COUNT(DISTINCT g.game_id) AS total_games
    FROM clubs c
    JOIN competitions comp ON c.domestic_competition_id = comp.competition_id
    LEFT JOIN players p ON c.club_id = p.current_club_id
    LEFT JOIN games g ON c.club_id = g.home_club_id OR c.club_id = g.away_club_id
    WHERE 1=1
  "
    
    if (input$clubCompetition != "Semua") {
      query <- paste0(query, " AND comp.name = '", input$clubCompetition, "'")
    }
    
    # Add squad_size filter to WHERE clause
    query <- paste0(query, " AND c.squad_size BETWEEN ", input$squadSizeRange[1], " AND ", input$squadSizeRange[2])
    
    # Add GROUP BY after WHERE clause
    query <- paste0(query, " GROUP BY c.club_id, c.name, c.squad_size, c.average_age")
    query <- paste0(query, " ORDER BY total_players DESC LIMIT 200;")
    
    getData(query)
  })
  
  output$clubTable <- renderDT({
    datatable(clubData(),
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle('league', 
                  backgroundColor = styleEqual(
                    c('Premier League', 'La Liga', 'Bundesliga', 'Serie A', 'Ligue 1'), 
                    c('#3771C8', '#FF9300', '#D3010C', '#008000', '#002F6C')
                  ),
                  color = 'white',
                  fontWeight = 'bold')
  })
  
  output$clubAgePlot <- renderPlotly({
    data <- clubData()[1:20,]
    
    plot_ly(data, x = ~name, y = ~average_age, type = 'bar', 
            marker = list(color = '#00a65a'),
            hoverinfo = "text",
            text = ~paste(name, "<br>", 
                          "Liga:", league, "<br>",
                          "Rata-rata usia:", average_age, "<br>",
                          "Ukuran squad:", squad_size)) %>%
      layout(title = "Rata-rata Usia Klub",
             xaxis = list(title = "", tickangle = 45),
             yaxis = list(title = "Usia Rata-rata"))
  })
  
  output$squadSizePlot <- renderPlotly({
    data <- clubData()[1:20,]
    
    plot_ly(data, x = ~name, y = ~squad_size, type = 'bar', 
            marker = list(color = '#f39c12'),
            hoverinfo = "text",
            text = ~paste(name, "<br>", 
                          "Liga:", league, "<br>",
                          "Ukuran squad:", squad_size, "<br>",
                          "Rata-rata usia:", average_age)) %>%
      layout(title = "Ukuran Squad Klub",
             xaxis = list(title = "", tickangle = 45),
             yaxis = list(title = "Ukuran Squad"))
  })
  
  # Transfer Tab
  transferData <- reactive({
    query <- "
      SELECT 
        t.player_id,
        t.player_name,
        t.transfer_date,
        ANY_VALUE(f.name) AS from_club,
        ANY_VALUE(t2.name) AS to_club,
        t.transfer_fee
      FROM transfers t
      JOIN clubs f ON t.from_club_id = f.club_id
      JOIN clubs t2 ON t.to_club_id = t2.club_id
      WHERE 1=1
    "
    
    if (!is.null(input$transferDateRange)) {
      query <- paste0(query, " AND t.transfer_date BETWEEN '", input$transferDateRange[1], "' AND '", input$transferDateRange[2], "'")
    }
    
    query <- paste0(query, " GROUP BY t.player_id, t.player_name, t.transfer_date, t.transfer_fee")
    query <- paste0(query, " ORDER BY t.transfer_date DESC LIMIT 100;")
    
    getData(query)
  })
  
  output$transferTable <- renderDT({
    datatable(transferData(),
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle('transfer_fee',
                  backgroundColor = styleInterval(
                    c(1000000, 10000000), 
                    c('#f39c12', '#00a65a', '#dd4b39')
                  ),
                  color = 'white',
                  fontWeight = 'bold')
  })
  
  output$transferTimePlot <- renderPlotly({
    data <- transferData()
    data$transfer_month <- format(as.Date(data$transfer_date), "%Y-%m")
    
    monthly_counts <- data %>%
      group_by(transfer_month) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(transfer_month)
    
    plot_ly(monthly_counts, x = ~transfer_month, y = ~count, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#dd4b39', width = 3),
            marker = list(color = '#dd4b39', size = 8)) %>%
      layout(title = "Tren Transfer per Bulan",
             xaxis = list(title = "Bulan"),
             yaxis = list(title = "Jumlah Transfer"))
  })
  
  output$transferClubPlot <- renderPlotly({
    data <- transferData()
    
    to_club_counts <- data %>%
      group_by(to_club) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count)) %>%
      head(10)
    
    plot_ly(to_club_counts, x = ~to_club, y = ~count, type = 'bar',
            marker = list(color = '#3c8dbc')) %>%
      layout(title = "Top 10 Klub Tujuan Transfer",
             xaxis = list(title = "", tickangle = 45),
             yaxis = list(title = "Jumlah Transfer"))
  })
  
  # Pertandingan Tab
  matchData <- reactive({
    query <- "
      SELECT 
        g.game_id,
        g.date,
        home.name AS home_team,
        away.name AS away_team,
        g.home_club_goals,
        g.away_club_goals,
        g.season
      FROM games g
      JOIN clubs home ON g.home_club_id = home.club_id
      JOIN clubs away ON g.away_club_id = away.club_id
      WHERE 1=1
    "
    
    if (input$matchSeason != "Semua") {
      query <- paste0(query, " AND g.season = '", input$matchSeason, "'")
    }
    
    if (!is.null(input$selectClubs) && length(input$selectClubs) > 0) {
      clubs_list <- paste0("'", paste(input$selectClubs, collapse = "','"), "'")
      query <- paste0(query, " AND (home.name IN (", clubs_list, ") OR away.name IN (", clubs_list, "))")
    }
    
    query <- paste0(query, " ORDER BY g.date DESC LIMIT 100;")
    
    getData(query)
  })
  
  output$matchTable <- renderDT({
    data <- matchData()
    data$result <- paste0(data$home_club_goals, " - ", data$away_club_goals)
    
    datatable(data %>% select(date, home_team, result, away_team, season),
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle('result',
                  backgroundColor = styleEqual(
                    levels = unique(data$result),
                    values = sapply(unique(data$result), function(res) {
                      parts <- as.numeric(unlist(strsplit(res, " - ")))
                      if(parts[1] > parts[2]) return('#dff0d8')
                      else if(parts[1] < parts[2]) return('#f2dede')
                      else return('#fcf8e3')
                    })
                  )
      )
  })
  
  output$scoreDistributionPlot <- renderPlotly({
    data <- matchData()
    data$total_goals <- data$home_club_goals + data$away_club_goals
    
    plot_ly(data, x = ~total_goals, type = 'histogram',
            marker = list(color = '#dd4b39', line = list(color = 'white', width = 1))) %>%
      layout(title = "Distribusi Total Gol per Pertandingan",
             xaxis = list(title = "Total Gol"),
             yaxis = list(title = "Jumlah Pertandingan"))
  })
  
  output$teamPerformancePlot <- renderPlotly({
    data <- matchData()
    
    # Calculate home team performances
    home_perf <- data %>%
      group_by(team = home_team) %>%
      summarise(
        matches = n(),
        wins = sum(home_club_goals > away_club_goals),
        draws = sum(home_club_goals == away_club_goals),
        losses = sum(home_club_goals < away_club_goals),
        goals_for = sum(home_club_goals),
        goals_against = sum(away_club_goals),
        .groups = 'drop'
      )
    
    # Calculate away team performances
    away_perf <- data %>%
      group_by(team = away_team) %>%
      summarise(
        matches = n(),
        wins = sum(away_club_goals > home_club_goals),
        draws = sum(away_club_goals == home_club_goals),
        losses = sum(away_club_goals < home_club_goals),
        goals_for = sum(away_club_goals),
        goals_against = sum(home_club_goals),
        .groups = 'drop'
      )
    
    # Combine home and away performances
    combined_perf <- bind_rows(home_perf, away_perf) %>%
      group_by(team) %>%
      summarise(
        matches = sum(matches),
        wins = sum(wins),
        draws = sum(draws),
        losses = sum(losses),
        goals_for = sum(goals_for),
        goals_against = sum(goals_against),
        .groups = 'drop'
      ) %>%
      mutate(
        win_rate = wins / matches * 100,
        goal_diff = goals_for - goals_against
      ) %>%
      arrange(desc(win_rate)) %>%
      head(10)
    
    plot_ly(combined_perf, x = ~team, y = ~win_rate, type = 'bar',
            marker = list(color = '#3c8dbc'),
            hoverinfo = "text",
            text = ~paste(team, "<br>",
                          "Win Rate:", round(win_rate, 1), "%<br>",
                          "Matches:", matches, "<br>",
                          "W/D/L:", wins, "/", draws, "/", losses, "<br>",
                          "Goal Diff:", goal_diff)) %>%
      layout(title = "Top 10 Tim berdasarkan Win Rate",
             xaxis = list(title = "", tickangle = 45),
             yaxis = list(title = "Win Rate (%)"))
  })
}

# Run aplikasi
shinyApp(ui = ui, server = server)