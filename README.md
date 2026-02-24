# NFL Cap Allocation & Winning

An interactive R Shiny app exploring how NFL teams allocate their salary cap across position groups — and whether that allocation correlates with regular season success or Super Bowl appearances.

https://artie.shinyapps.io/salary-cap/

## Overview

Every offseason the same debate erupts: is paying a quarterback $50M a year the path to a Super Bowl, or a trap? This app lets you explore that question across all 32 teams from 2013–2024, with data on 10 position groups and 12 seasons of results.

Spending is shown as a **percentage of that season's salary cap** rather than raw dollars, making comparisons across years meaningful as the cap grew from $123M to $255M over the window.

## Features

- **Scatter plot** — positional cap % vs win percentage for every team-season, with Super Bowl winners/losers highlighted
- **Trend plot** — league-wide average cap % over time, with a selected year range highlighted in context
- **Four interactive controls** — position group selector, season range slider, Super Bowl highlight toggle, and team highlighter

## Data Sources

- **Positional spending** — scraped from [OverTheCap.com](https://overthecap.com/positional-spending)
- **Win/loss records** — `nflreadr::load_schedules()`
- **Salary cap totals & Super Bowl results** — hardcoded and verified by season year

## Running the App

```r
# Install dependencies if needed
install.packages(c("shiny", "rvest", "dplyr", "readr", "ggplot2", "scales", "tidyr"))
install.packages("nflreadr")

# Run the app
shiny::runApp("nfl_cap_shiny.R")
```

## Tech Stack

R · Shiny · ggplot2 · rvest · nflreadr · dplyr
