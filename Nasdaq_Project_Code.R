## Import the dataset

library(readr)
London_Judas_Swing_Reversal_profile_Analytics_Data <- read_csv("C:/Users/Meryem/Desktop/K.O Trading Analytics Data/London Judas Swing - Reversal profile Analytics Data.csv")
View(London_Judas_Swing_Reversal_profile_Analytics_Data)

## Let's make a copy to work on 

Judas_reversal <- London_Judas_Swing_Reversal_profile_Analytics_Data

## Let's install the packages that we're going to use 

install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("scales")
install.packages("gt")
install.packages("gtExtras")

## Let's load those packages

library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(scales)  
library(hms)
library(gt)
library(gtExtras)


## Let's clean our data 
## First Let's remove some empty rows

Judas_reversal <- Judas_reversal[-c(181:364),] 
View(Judas_reversal)

##  We have some variables/columns that we won't be using for this project

Judas_reversal$status <- NULL  
Judas_reversal$tags <- NULL  
Judas_reversal$exchangeRate <- NULL  
Judas_reversal$amountClosed <- NULL
Judas_reversal$uPnL <- NULL
Cleaned_Judas_reversal <- Judas_reversal

View(Cleaned_Judas_reversal)

## Let's change the initial balance variable to non scientific data

Cleaned_Judas_reversal$initialBalance <- format(Cleaned_Judas_reversal$initialBalance, scientific = FALSE)

options(scipen = 999)

## Now let's put the correct pair name in

Cleaned_Judas_reversal$pair[Cleaned_Judas_reversal$pair == "USATECHIDXUSD"] <- "NAS100"

## Let's specify the names of the days of the week

Cleaned_Judas_reversal$day <- recode(Cleaned_Judas_reversal$day,
  '1' = "Monday",
  '2' = "Tuesday",
  '3' = "Wednesday",
  '4' = "Thursday",
  '5' = "Friday")

str(Cleaned_Judas_reversal)

## Let's fix the "Startdate" and "Enddate"
## We'll start by changing the variable from Char to Date and Time then separating them to different columns
Cleaned_Judas_reversal$dateStart <- 
  as.POSIXct(Cleaned_Judas_reversal$dateStart, format = "%m/%d/%Y %H:%M")
Cleaned_Judas_reversal$TradedateS <- as.Date(Cleaned_Judas_reversal$dateStart)
Cleaned_Judas_reversal$TradetimeS <- format(Cleaned_Judas_reversal$dateStart, "%H:%M")

## Now we do the same for Enddate

Cleaned_Judas_reversal$dateEnd <- 
  as.POSIXct(Cleaned_Judas_reversal$dateEnd, format = "%m/%d/%Y %H:%M")
Cleaned_Judas_reversal$TradedateE <- as.Date(Cleaned_Judas_reversal$dateEnd)
Cleaned_Judas_reversal$TradetimeE <- format(Cleaned_Judas_reversal$dateEnd, "%H:%M")

## Make Initial Balance numeric

Cleaned_Judas_reversal$initialBalance <- as.numeric(Cleaned_Judas_reversal$initialBalance)

## Now let's organize our data 

Org_Cleaned_JDR <- Cleaned_Judas_reversal %>%  select(
  id,pair,TradedateS,TradetimeS,TradedateE,TradetimeE,day,side,amount,
  entryPrice,initalSL,maxTP,avgClosePrice,idealTP,avgRiskReward,
  maxRiskReward,initialBalance,rPnL,currentRealizedBalance
)

Org_Cleaned_JDR <- Org_Cleaned_JDR %>% rename(
  Id = id,
  Pair = pair,
  Day = day,
  Side = side,
  SL = initalSL,
  TP = maxTP,
  MaxTP = idealTP,
  AvgRR = avgRiskReward,
  MaxRR = maxRiskReward,
  Initial_balance = initialBalance,
  Realized_balance = currentRealizedBalance
)

View(Org_Cleaned_JDR)

JDR <- Org_Cleaned_JDR

View(JDR)

## Now let's analyse our data 
## How does the equity curve look like for this strategy? (line graph)


min_balance <- min(JDR$Realized_balance, na.rm = TRUE)
max_balance <- max(JDR$Realized_balance, na.rm = TRUE)

Equity_Curve <- JDR %>%
  ggplot(aes(x = TradedateS, y = Realized_balance))+
  geom_line(size = 1.35, color = "red")+
  geom_hline(yintercept = min_balance,
             linetype = "dashed", color = "darkorchid4", size = 1)+  ## min balance line 
  geom_hline(yintercept = max_balance,
             linetype = "dashed", color = "darkorchid4", size = 1)+  ## max balance line
  annotate("text", x = max(JDR$TradedateS), y = max_balance,
           label = paste0("Max Balance: ", dollar(max_balance)),
           vjust = 1.3, hjust = 7, color = "darkorchid4", size = 4)+  ## max balance annotation
  
  annotate("text", x = max(JDR$TradedateS), y = min_balance,
           label = paste0("Min Balance: ", dollar(min_balance)),
           vjust = -.85, hjust = 1, color = "darkorchid4", size = 4)+  ## min balance annotation
  labs(title = "Equity Curve",
       x = "Month",
       y = "Balance")+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  scale_y_continuous(
    breaks = seq(0, max_balance, by = 10000), 
    labels = dollar_format(prefix = "$", big.mark = ","))+
  theme_minimal()+
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "lightgrey"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(angle = 90),
    plot.title = element_text(hjust = 0.5))




## What are some performance metrics for this strategy? (avgRR, Winrate, Avg Trade duration)? (Dashboard)

## RR plot With avg line

RM_RR <- JDR %>% mutate(
  Pos_RR = ifelse(JDR$MaxRR == 0, NA, MaxRR))

avg_rr <- mean(RM_RR$Pos_RR, na.rm = TRUE)

RM_RR %>% 
  ggplot(aes(x = TradedateS, y = Pos_RR))+
  geom_point(color = "red", size = 2.5, shape = 16, na.rm = TRUE)+
  geom_hline(yintercept = avg_rr, linetype = "dashed", color = "darkorchid4", size = 0.5)+
  annotate("text", x = min(RM_RR$TradedateS, na.rm = TRUE), y = avg_rr, 
           label = paste0("AVG: ", round(avg_rr, 2)),
           hjust = -7, vjust = -.5, size = 3.5, color = "darkorchid4")+
  labs(
    title = "Average Risk to Reward",
    x = "Date",
    y = "RR")+
  theme_minimal()+
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "lightgrey"),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5))


## What is the average trade duration

Trade_duration <- JDR %>% 
  mutate(
    Trade_duration_min = 
      as.numeric(as_hms(paste0(TradetimeE, ":00")) - as_hms(paste0(TradetimeS, ":00")),
                 units = "mins"))

Trade_duration %>%
  ggplot(aes(x = TradedateS, y = Trade_duration_min))+
  geom_point(color = "red", size = 2)+
  theme_minimal()+
  geom_hline(yintercept = mean(Trade_duration$Trade_duration_min),
             linetype = "dashed", color = "darkorchid4", size = 1)+
  annotate("text",
           x = max(Trade_duration$TradedateS),
           y = mean(Trade_duration$Trade_duration_min),
           label = paste0("Avg: ",
                          round(mean(Trade_duration$Trade_duration_min), 1), " min"),
           hjust = 1.9, vjust = -1.1, color = "darkorchid4", size = 4)+
  labs(
    title = "Trade Duration with Avg line",
    x = "Date",
    y = "Duration in minutes")+
  theme(plot.title = element_text(hjust = .5))


## Which side of the market are we more likely to predict? Buyside or Sellside? 

Trade_counts <-JDR %>%
  count(Side) %>%
  mutate(
    percent_side = n / sum(n) * 100,
    label = paste0(Side, ": ", n, " (", round(percent_side, 1), "%)"))
View(Trade_counts)

Buy_Sell_Trades <- ggplot(Trade_counts, aes(x = "", y = n, fill = Side))+
  geom_col(width = 1)+
  coord_polar(theta = "y")+
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
  labs(title = "Buy/Sell Trades")+
  scale_fill_manual(values = c("buy" = "chartreuse4", "sell" = "red"))+
  theme_minimal()+
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),  ## Hide x-axis title
    axis.title.y = element_blank(),  ## Hide y-axis title
    axis.text = element_blank(),     ## Hide axis labels
    panel.grid = element_blank(),    ## Remove grid lines
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank())



## Let's calculate the Wins Loss BE by Side of the market

winloss <- JDR %>% 
  mutate(
    Win_Loss = ifelse (rPnL > 0, "Win",
                       ifelse(rPnL == 0, "BE", "Loss")))


win_loss_summary <- winloss %>%
  count(Side, Win_Loss) %>%
  group_by(Side) %>%
  mutate(
    percent = n / sum(n) * 100, 
    label = paste0(Win_Loss, ": ", n, " (", round(percent, 1), "%)"))

Buy_Sell_stats <- ggplot(win_loss_summary, aes(x = Side, y = n, fill = Win_Loss))+
  geom_bar(stat = "identity", position = "stack", width = .7) +  # Use stacked bars
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white")+  ## Put labels inside bars
  labs(title = "Win/Loss/BE Proportions by Side of Market")+
  scale_fill_manual(values = c("BE" = "darkgray", "Win" = "chartreuse4", "Loss" = "red"))+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),  ## Hide x-axis title
    axis.title.y = element_blank(),  ## Hide y-axis title
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.text.y = element_blank(),  ## Hide y-axis text
    panel.grid = element_blank(),  ## Remove grid lines
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    legend.position = "none")


## Let's see the win rate of each Side 

win_rate_summary <- JDR %>%
  mutate(
    Win_Loss = ifelse(rPnL > 0, "Win", ifelse(rPnL == 0, "BE", "Loss"))
  ) %>%
  count(Side, Win_Loss) %>%
  group_by(Side) %>%
  mutate(
    total_trades = sum(n),
    wins = sum(n[Win_Loss == "Win"]),
    win_rate = wins / total_trades * 100
  ) %>%
  filter(Win_Loss == "Win") %>%
  select(Side, win_rate)

win_rate_summary <- JDR %>%
  mutate(Win_Loss = ifelse(rPnL > 0, "Win", ifelse(rPnL == 0, "BE", "Loss"))) %>%
  count(Side, Win_Loss) %>%
  group_by(Side) %>%
  mutate(total = sum(n)) %>%
  filter(Win_Loss == "Win") %>%
  mutate(win_rate = round(n / total * 100, 1)) %>%
  select(Side, win_rate)

View(win_rate_summary)

## Table visualization 

win_rate_summary %>%
    ungroup() %>%
    select(Side, win_rate) %>%
    gt() %>%
  gt_theme_538() %>% 
    fmt_number(
      columns = win_rate,
      decimals = 1,
      suffixing = FALSE) %>%
    cols_label(win_rate = "Win Rate (%)") %>%
    tab_header(title = "Win Rate by Trade Side") %>%
    gt_highlight_rows(
      rows = 1,
      target_col = 1,
      bold_target_only = TRUE,
      fill = "darkgreen",
      font_color = "black",
      alpha = 0.4) %>%
    gt_highlight_rows(
      rows = 2,
      target_col = 1,
      bold_target_only = TRUE,
      fill = "red",
      font_color = "black",
      alpha = 0.5)

## Bar chart
  
ggplot(win_rate_summary, aes(x = Side, y = win_rate, fill = Side))+
  geom_bar(stat = "identity", width = .3, show.legend = FALSE)+
  geom_text(aes(label = paste0(round(win_rate, 1), "%")), vjust = -0.5, color = "black")+
  labs(title = "Win Rate by Side of Market",
       y = "Win Rate (%)",
       x = "Side of Market")+
  scale_fill_manual(values = c("buy" = "chartreuse4", "sell" = "red"))+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white",color = NA))



## Which day of the week is riskier or less profitable?

day_win_loss <- JDR %>%
  mutate(result = ifelse(rPnL > 0, "Win", "Loss")) %>%
  group_by(Day, result) %>%
  summarise(total_rPnL = sum(rPnL, na.rm = TRUE), .groups = "drop") %>%
  mutate(day = factor(Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))) %>%  
  arrange(day)

View(day_win_loss)

ggplot(day_win_loss, aes(x = day, y = total_rPnL, fill = result))+
  geom_col(position = "stack", width = 0.3) +
  geom_text(
    aes(label = dollar(total_rPnL, prefix = "$")),
    position = position_stack(vjust = 1.05),
    vjust = -1.75,
    hjust = 2,
    size = 3.5,
    color = "black")+
  coord_flip()+
  scale_y_continuous(labels = label_dollar())+
  scale_fill_manual(values = c("Win" = "chartreuse4", "Loss" = "red"))+
  labs(
    title = "Total Win/loss by Day",
    x = "Day",
    y = "rPnL",
    fill = "Trade Result")+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = .5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())


## Monte Carlo simulation

MT_sim <- JDR$rPnL
MT_sim <- na.omit(MT_sim)

set.seed(123)  # For reproducibility

## Define parameters
n_simulations <- 10
n_trades <- 3000

## Run simulations
simulations <- replicate(n_simulations , {
  sample_returns <- sample(MT_sim, n_trades, replace = TRUE)
  cumsum(sample_returns)  # Cumulative balance (equity curve)
})

## Convert matrix to data frame
sim_df <- as.data.frame(simulations)

## Add trade number column
sim_df <- sim_df %>%
  mutate(Trade = 1:n_trades) %>%
  pivot_longer(-Trade, names_to = "Simulation", values_to = "Balance")


final_stats <- sim_df %>%
  group_by(Simulation) %>%
  summarise(Final_Balance = last(Balance)) %>%
  summarise(
    avg_final = mean(Final_Balance),
    min_final = min(Final_Balance),
    max_final = max(Final_Balance))

avg_final <- final_stats$avg_final
min_final <- final_stats$min_final
max_final <- final_stats$max_final


ggplot(sim_df, aes(x = Trade, y = Balance, color = Simulation))+
  geom_line(size = 1)+
  geom_hline(yintercept = c(avg_final, max_final, min_final),
             linetype = c("dotted", "dashed", "dashed"),
             color = c("darkorchid", "darkorchid4", "darkorchid4"),
             size = 0.4)+
  annotate("text", x = max(sim_df$Trade), y = avg_final,
           label = paste0("Avg: ", dollar(avg_final)),
           hjust = 8.5, vjust = -0.5, color = "darkorchid", size = 3.5)+
  annotate("text", x = max(sim_df$Trade), y = max_final,
           label = paste0("Max: ", dollar(max_final)),
           hjust = 7.2, vjust = -0.5, color = "darkorchid4", size = 3.5)+
  annotate("text", x = max(sim_df$Trade), y = min_final,
           label = paste0("Min: ", dollar(min_final)),
           hjust = 8.5, vjust = 1.5, color = "darkorchid4", size = 3.5)+
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ","))+
  labs(title = "10 Monte Carlo Simulations of 3,000 Trades",
       x = "Trades",
       y = "Cumulative PnL")+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")


## Let's make a BE threshold matrix
## Define RR and Win Rate vectors
rr_values <- 1:5
winrate_values <- seq(0, 1, by = 0.1)

## Create grid and logic
grid <- expand.grid(WinRate = winrate_values, RR = rr_values) %>%
  mutate(
    BE = 1 / (1 + RR),
    Status = case_when(
      WinRate > BE ~ "Profitable",
      WinRate == BE ~ "Breakeven",
      WinRate < BE ~ "Not Profitable"),
    WinRate = paste0(round(WinRate * 100), "%"),
    RR = paste0(RR))

## Pivot to put WinRate as columns and RR as rows
table_data_flipped <- grid %>%
  select(RR, WinRate, Status) %>%
  pivot_wider(names_from = WinRate, values_from = Status)

## Build gt table
gt_table <- table_data_flipped %>%
  gt(rowname_col = "RR") %>%
  tab_header(
    title = "Profitability and BE Threshold Matrix") %>%
  data_color(
    columns = everything(),
    colors = function(x){
      dplyr::case_when(
        x == "Profitable" ~ "chartreuse4",
        x == "Breakeven" ~ "yellow",
        x == "Not Profitable" ~ "red",
        TRUE ~ "white")})

## Display table

gt_table

## Show the overall winrate of the model

Hi <- JDR

win_rate <- sum(Hi$rPnL > 0, na.rm = TRUE) / nrow(Hi) * 100

## Create a table to display the win rate
win_rate_table <- data.frame(
  Metric = "Win Rate",
  Value = sprintf("%.2f%%", win_rate))


win_rate_table %>%
  gt() %>%
  tab_header(
    title = "Overall Win Rate"
  ) %>%
  cols_label(
    Metric = "",
    Value = "Win Rate (%)")
win_rate_table %>%
  gt() %>%
  tab_header(
    title = "Overall Win Rate"
  ) %>%
  cols_label(
    Metric = "",
    Value = "Win Rate (%)"
  ) %>%
  tab_options(
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none"
  )

## Breakeven threshold table

data.frame(RR = 5, Breakeven = sprintf("%.1f%%", 100 / (1 + 5))) %>%
  gt() %>%
  tab_header(title = "Breakeven Threshold at 5RR")
