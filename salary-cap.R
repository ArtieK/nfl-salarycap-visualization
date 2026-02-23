# ── Libraries ─────────────────────────────────────────────────────────────────

library(shiny)
library(rvest)
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(tidyr)
library(nflreadr)

# ── Data Pipeline ─────────────────────────────────────────────────────────────

# Scrape positional cap spending from OverTheCap (all years on one page)
page   <- read_html("https://overthecap.com/positional-spending")
tables <- html_table(page)
years  <- 2013:2029

pos_data <- Map(function(tbl, yr) {
  tbl %>% mutate(Year = yr)
}, tables, years) %>%
  bind_rows()

# Clean dollar columns
pos_data <- pos_data %>%
  mutate(across(c(QB, RB, WR, TE, OL, Offense, IDL, EDGE, LB, S, CB, Defense),
                ~parse_number(as.character(.))))

# Keep only completed seasons
pos_data <- pos_data %>%
  filter(Year <= 2024)

# Load regular season win/loss records from nflreadr
schedules <- load_schedules(seasons = 2013:2024) %>%
  filter(game_type == "REG") %>%
  select(season, home_team, away_team, home_score, away_score) %>%
  mutate(
    home_win = home_score > away_score,
    away_win = away_score > home_score
  )

home_records <- schedules %>%
  group_by(season, team = home_team) %>%
  summarise(wins = sum(home_win, na.rm = TRUE), games = n())

away_records <- schedules %>%
  group_by(season, team = away_team) %>%
  summarise(wins = sum(away_win, na.rm = TRUE), games = n())

records <- bind_rows(home_records, away_records) %>%
  group_by(season, team) %>%
  summarise(wins = sum(wins), games = sum(games)) %>%
  mutate(win_pct = wins / games)

# Build lookup table: OTC nickname -> nflreadr team abbreviation
teams_lookup <- nflreadr::load_teams() %>%
  select(team_abbr, team_name) %>%
  mutate(nickname = sub(".*\\s", "", team_name))

# Normalize relocated team abbreviations
records <- records %>%
  mutate(team = case_when(
    team == "OAK" ~ "LV",
    team == "STL" ~ "LA",
    team == "SD"  ~ "LAC",
    TRUE ~ team
  ))

# Verified salary cap totals by season year
cap_by_year <- tibble(
  Year = 2013:2024,
  cap  = c(123000000,  # 2013
           133000000,  # 2014
           143280000,  # 2015
           155270000,  # 2016
           167000000,  # 2017
           177200000,  # 2018
           188200000,  # 2019
           198200000,  # 2020
           182500000,  # 2021 - dropped due to COVID
           208200000,  # 2022
           224800000,  # 2023
           255400000)  # 2024
)

# Verified Super Bowl results by NFL season year
# Note: Year = season year, game played February of following year
sb_winners <- tibble(
  Year      = 2013:2024,
  sb_winner = c("BAL","SEA","NE", "DEN","NE","PHI","NE", "KC","TB","LA","KC","KC"),
  sb_loser  = c("SF","DEN","SEA","CAR","ATL","NE","LA","SF","KC","CIN","PHI","SF")
)

# Human-readable position labels for UI display
pos_labels <- c(
  "QB"   = "Quarterback",
  "RB"   = "Running Back",
  "WR"   = "Wide Receiver",
  "TE"   = "Tight End",
  "OL"   = "Offensive Line",
  "IDL"  = "Interior D-Line",
  "EDGE" = "Edge Rusher",
  "LB"   = "Linebacker",
  "S"    = "Safety",
  "CB"   = "Cornerback"
)

# Join all data sources and compute cap % per position
# cap % normalizes raw spending by that season's salary cap so years are comparable
combined <- pos_data %>%
  left_join(teams_lookup, by = c("Team" = "nickname")) %>%
  left_join(records, by = c("team_abbr" = "team", "Year" = "season")) %>%
  left_join(cap_by_year, by = "Year") %>%
  left_join(sb_winners, by = "Year") %>%
  mutate(
    sb_outcome = case_when(
      team_abbr == sb_winner ~ "Won Super Bowl",
      team_abbr == sb_loser  ~ "Lost Super Bowl",
      TRUE ~ "Other"
    ),
    across(c(QB, RB, WR, TE, OL, IDL, EDGE, LB, S, CB),
           ~. / cap * 100,
           .names = "{.col}_pct")
  )

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  
  titlePanel("NFL Cap Allocation & Winning"),
  
  sidebarLayout(
    sidebarPanel(
      
      # ── App Context ───────────────────────────────────────────────────────────
      p("Every NFL offseason, the same debate erupts: is paying a quarterback
        $50M a year the path to a Super Bowl, or a trap? This app lets you
        explore how all 32 teams have allocated their salary cap across positions
        from 2013–2024 — and whether it actually correlates with winning."),
      p("Since the salary cap grew from $123M to $255M over this period,
        spending is shown as a % of that year's cap so comparisons across
        seasons are effective"),
      
      hr(),
      
      # ── CONTROL 1: Position Group Selector ───────────────────────────────────
      # Affects: both plots
      selectInput(
        inputId  = "position",
        label    = "Position Group",
        choices  = setNames(names(pos_labels), pos_labels),
        selected = "QB"
      ),
      helpText("Not sure what a position means? IDL = Interior Defensive Linemen
               (nose tackles, defensive tackles), EDGE = pass rushers off the
               edge. OL = the offensive line blocking for your QB."),
      
      hr(),
      
      # ── CONTROL 2: Year Range Slider ─────────────────────────────────────────
      # Affects: scatter plot (filters data), trend plot (highlights range)
      sliderInput(
        inputId = "year_range",
        label   = "Season Range",
        min     = 2013,
        max     = 2024,
        value   = c(2013, 2024),
        step    = 1,
        sep     = ""
      ),
      
      hr(),
      
      # ── CONTROL 3: Super Bowl Highlight Toggle ────────────────────────────────
      # Affects: scatter plot only
      checkboxInput(
        inputId = "highlight_sb",
        label   = "Highlight Super Bowl teams",
        value   = TRUE
      ),
      
      hr(),
      
      # ── CONTROL 4: Team Highlight Selector ───────────────────────────────────
      # Highlights a team's dots in blue and labels each season with the year
      # Does NOT filter out other teams
      # Affects: scatter plot only
      selectInput(
        inputId  = "highlight_team",
        label    = "Highlight a Team",
        choices  = c("None", sort(unique(combined$team_abbr))),
        selected = "None"
      )
      
    ),
    
    mainPanel(
      
      # ── PLOT 1: Cap % vs Win Percentage Scatter ───────────────────────────────
      # Each dot = one team-season. Trend line shows overall direction.
      plotOutput("scatter_plot", height = "420px"),
      
      p("Each dot represents one team in one season. The dashed trend line shows
        the overall relationship between spending at that position and winning.
        The real story is often in the outliers — use the team highlighter to
        trace a specific franchise over time."),
      
      hr(),
      
      # ── PLOT 2: League-Wide Spending Trend ────────────────────────────────────
      # Grey = full 2013-2024 range, gold = selected year range
      plotOutput("trend_plot", height = "260px")
      
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  # ── REACTIVE 1: Year-Filtered Data ───────────────────────────────────────────
  # Filters combined to selected year range
  # Used by: both plots
  filtered_data <- reactive({
    combined %>%
      filter(Year >= input$year_range[1],
             Year <= input$year_range[2])
  })
  
  # ── REACTIVE 2: Position Column Name ─────────────────────────────────────────
  # Derives "_pct" column name from position selector e.g. "QB" -> "QB_pct"
  # Used by: both plots
  pos_col <- reactive({
    paste0(input$position, "_pct")
  })
  
  # ── REACTIVE 3: Point Display Categories ─────────────────────────────────────
  # Assigns each row a display category for color in the scatter plot
  # Priority: Highlighted Team > Won SB > Lost SB > Other
  # Used by: scatter plot only
  point_data <- reactive({
    filtered_data() %>%
      mutate(
        display_cat = case_when(
          team_abbr == input$highlight_team                         ~ "Highlighted Team",
          input$highlight_sb & sb_outcome == "Won Super Bowl"  ~ "Won Super Bowl",
          input$highlight_sb & sb_outcome == "Lost Super Bowl" ~ "Lost Super Bowl",
          TRUE                                                  ~ "Other"
        ),
        cap_pct = .data[[pos_col()]]
      )
  })
  
  # ── PLOT 1: Scatter Plot ──────────────────────────────────────────────────────
  output$scatter_plot <- renderPlot({
    
    df       <- point_data()
    pos_name <- pos_labels[input$position]
    
    ggplot(df, aes(x = cap_pct, y = win_pct, color = display_cat)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE,
                  color = "black", linetype = "dashed", linewidth = 0.8) +
      {
        if (input$highlight_team != "None")
          geom_text(data = df %>% filter(display_cat == "Highlighted Team"),
                    aes(label = Year), size = 3, vjust = -1, fontface = "bold")
        else
          list()
      } +
      scale_color_manual(
        values = c(
          "Won Super Bowl"   = "#f0b400",
          "Lost Super Bowl"  = "#e05252",
          "Highlighted Team" = "#58a6ff",
          "Other"            = "#cccccc"
        ),
        name = NULL
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                         limits = c(0, 1)) +
      scale_x_continuous(labels = function(x) paste0(round(x, 1), "%")) +
      labs(
        x     = paste0(pos_name, " Cap Spending (% of Salary Cap)"),
        y     = "Win Percentage",
        title = paste0(pos_name, " Spending vs Winning  |  ",
                       input$year_range[1], "–", input$year_range[2])
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title      = element_text(face = "bold"),
        legend.position = "bottom"
      )
  })
  
  # ── PLOT 2: Trend Plot ────────────────────────────────────────────────────────
  # Grey background = full 2013-2024 range for context
  # Gold overlay = selected year range from slider
  output$trend_plot <- renderPlot({
    
    trend_bg <- combined %>%
      group_by(Year) %>%
      summarise(avg_pct = mean(.data[[pos_col()]], na.rm = TRUE))
    
    trend_highlight <- filtered_data() %>%
      group_by(Year) %>%
      summarise(avg_pct = mean(.data[[pos_col()]], na.rm = TRUE))
    
    ggplot() +
      geom_line(data = trend_bg, aes(x = Year, y = avg_pct),
                color = "#cccccc", linewidth = 1) +
      geom_line(data = trend_highlight, aes(x = Year, y = avg_pct),
                color = "#f0b400", linewidth = 1.5) +
      geom_point(data = trend_highlight, aes(x = Year, y = avg_pct),
                 color = "#f0b400", size = 2.5) +
      scale_x_continuous(breaks = 2013:2024) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      labs(
        title = paste("League-Wide", pos_labels[input$position], "Spending Over Time"),
        x     = "Season",
        y     = "Avg Cap %"
      ) +
      theme_minimal()
  })
  
}

# ── Launch ────────────────────────────────────────────────────────────────────

shinyApp(ui, server)