library(tidyr)
library(plotly)
library(lubridate)
library(gsheet)
library(dplyr)

###################
# SETUP           #
###################
# Test: format(D_blinks$Timestamp, "%Y-%m-%d %H:%M:%OS6")
# options("digits.secs"=6)
options("digits.secs"=6)
vistemplate <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("pan2d","select2d","hoverCompareCartesian", "toggleSpikelines","zoom2d","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan", showlegend=FALSE, xaxis=list(mirror=T, ticks='outside', showline=T), yaxis=list(mirror=T, ticks='outside', showline=T))

timetemplate <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan", xaxis = list(tickformat="%H:%M:%S.%L ms"), yaxis = list(range=c(0,1.1)))

load("data_all.rda")

#######################
# CALCULATE AGGERGATES#
#######################

# Clean D_all so we only have events.
D_all <- D_all %>% filter(!Event %in% c('Sample','NoData'))


# Label input windows above 10 as '', below 10 as ''
D_all <- D_all %>%
  mutate(ExperimentalMode = "InstructedToAttempt") #ifelse(InputWindowOrder < 10, "InstructedToAttempt","InstructedToRest"))




# Count
trial_summary <- D_all %>% ungroup() %>% group_by(PID,Condition,Repetition,ExperimentalMode) %>%
  filter(Event == "GameDecision") %>%
  summarize(rejInput = sum(TrialResult == "RejInput"),
            accInput = sum(TrialResult == "AccInput"),
            totalTrials = rejInput+accInput)

trial_summary <- trial_summary %>% ungroup() %>%
  mutate(rate = accInput/totalTrials,
         session = 1,
         session = cumsum(session))

p_overall <- trial_summary %>% group_by(PID, Condition, ExperimentalMode) %>%
  summarize(avg_rate = mean(rate),
            sum_acc = sum(accInput)) %>% View()


vistemplate %>%
  add_trace(data = trial_summary %>% filter(ExperimentalMode == "InstructedToAttempt", Condition == "MI"), x=~session, y=~rate,
            type='scatter',mode='lines+markers',color=~PID) %>%
  add_trace(data = trial_summary %>% filter(ExperimentalMode == "InstructedToRest", Condition == "MI"), x=~session, y=~rate,
            type='scatter',mode='lines+markers',color=I('Black'))
  
