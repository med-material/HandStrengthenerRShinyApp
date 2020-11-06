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

###################
# LOAD AND ENRICH #
###################
load_data <- function(participant, cond,rep) {
  dir = paste('H',participant,'_',cond,'/',sep='')
  datasets = list.files(dir, pattern=paste("_",rep,".csv",sep=''))
  datasets <- lapply(paste(dir,datasets, sep=''), read.csv, na.strings="NULL")
  
  game_exists = length(datasets) > 0
  meta_exists = length(datasets) > 1
  sample_exists = length(datasets) > 2
  
  dataset_combined = data.frame()
  if(game_exists) {
    names(datasets)[1] = "Game"
    
    # Setup Game dataset
    datasets$Game <- datasets$Game %>%
      mutate(BCIConfidence = as.numeric(BCIConfidence),
             EventType = 'Event')
    
    dataset_combined = bind_rows(dataset_combined, datasets$Game)
  }
  if (sample_exists) {
    names(datasets)[3] = "Sample"
    
    # Setup Sample dataset
    datasets$Sample <- datasets$Sample %>%
      mutate(BCIConfidence = as.numeric(BCIConfidence),
             EventType = 'BCI')
    
    dataset_combined = bind_rows(dataset_combined, datasets$Sample)
  }
  if (meta_exists){
    names(datasets)[2] = "Meta"
    datasets$Meta <- datasets$Meta[1,]
    
    # Setup Meta dataset
    datasets$Meta <- datasets$Meta %>%
      mutate(Timestamp = NULL,
             SessionID = NULL,
             Email = NULL,
             Framecount = NULL)
    
    dataset_combined = bind_cols(dataset_combined, datasets$Meta)
  }
  dataset_combined <- dataset_combined %>% 
    mutate(Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
    arrange(Timestamp)
    #distinct() # used due to a bug in gamemanger prior to v20.10.2020
  
  return(dataset_combined)
}


participants <- 4
conditions <- c('MI') #'HA'
repetitions <- 1:4

D_all <- data.frame()

for (p in participants) {
  for (r in repetitions) {
    for (c in conditions) {
      tmp <- data.frame()
      tmp <- load_data(p,c,r)
      tmp$Condition = c
      tmp$PID = p
      tmp$Repetition = r
      D_all <- bind_rows(D_all,tmp)
    }
  }
}

D_all <- D_all %>%
  mutate(PID = as.factor(PID),
         Repetition = as.factor(Repetition),
         Condition = as.factor(Condition))

save(D_all, file='data_all.rda', compress=FALSE)
######################
# Visualize Timeline #
######################

# Step: Check framerate.
vistemplate %>% add_trace(data=D_all, name="Timestamp", x=~Framecount, y=~Timestamp, type='scatter', mode='markers',
                          hoverinfo='text',text=~Event)


# Step: Filter out all the rows without data in them.
#D_all <- D_all %>% filter(!Event %in% c('NoData'))
#D_all %>% filter(Event != 'Sample') %>% View()

# Add label positions
D_all <- D_all %>% ungroup() %>%
  mutate(labelpos = 0.2,
         labelpos = ifelse(Event == "Sample", 0.1, labelpos),
         labelpos = ifelse(Event == "InputWindowChange", 0.15, labelpos),
         labelpos = ifelse(Event == "MotorImagery", 0.21, labelpos),
         labelpos = ifelse(Event == "Rest", 0.21, labelpos),
         labelpos = ifelse(Event == "GameDecision", 0.3, labelpos),
         labelpos = ifelse(Event %in% c("GameRunning","GameStopped"), 0.4, labelpos))

# Add end Timestamps for Input Windows
D_all <- D_all %>% ungroup() %>% filter(Event == "InputWindowChange") %>% group_by(InputWindowOrder) %>%
  mutate(InputWindowEnd = lead(Timestamp)) %>%
  filter(InputWindow == "Open") %>% select(InputWindow, InputWindowOrder, InputWindowEnd) %>%
  right_join(D_all) %>% arrange(Timestamp)

# Add end Timestamps for Motor Imagery events.
#D_all <- D_all %>% ungroup() %>% mutate(BCIConfidence = ifelse(is.na(BCIConfidence), -1, BCIConfidence))

timetemplate %>% 
  add_segments(name=~Event, data=D_all %>% filter(!Event %in% c('Sample', 'NoData')), type='scattergl',
               x =~Timestamp, y=~labelpos-0.02, xend =~Timestamp, yend =0, size=I(1), color=I("Gray")) %>%
  add_trace(name=~Event, data=D_all %>% filter(!Event %in% c('Sample', 'NoData')),
            x =~Timestamp, y =~labelpos, color =~Event, type='scattergl',mode='text', text=~Event, textfont = list(size = 10)) %>%
  add_segments(name="InputWindow", data=D_all %>% filter(InputWindow == 'Open'), type='scattergl',
               x = ~Timestamp, y = 0.02, xend= ~InputWindowEnd, yend = 0.02, size=I(6), color =I('SeaGreen')) %>%
  add_segments(name="BCIThreshold", data=D_all, type='scattergl',
               x = ~min(Timestamp), y =0.73, xend=~max(Timestamp), yend =0.73, size=I(2), visible = "legendonly", color =I('LightCoral')) %>%
  #add_segments(name="MoleHit", data=D_e_molehitduration,
  #             x = ~Timestamp, y = 0, xend= ~MoleDestinyTimestamp, yend = 0, size=I(6), color =I('DarkGreen')) %>%
  #add_segments(name="MoleMiss", data=D_e_molemissduration,
  #             x = ~Timestamp, y = 0.01, xend= ~MoleDestinyTimestamp, yend = 0.01, size=I(6), color = I("DarkSeaGreen")) %>%
  # Head Rotation: 0.1 = 36 degrees (0.5 = 180 degrees)
  add_trace(name="BCIConfidence", data=D_all %>% filter(Event == 'Sample'), x =~Timestamp, y =~jitter(BCIConfidence,amount=.01), type='scattergl',
            mode='markers+lines', visible = "legendonly", hoverinfo='text',text=~BCIConfidence) %>%
  add_trace(name="Samples", data=D_all %>% filter(Event %in% c('Sample','NoData')), x =~Timestamp, y =0.5, type='scattergl',
            mode='markers+lines', visible = "legendonly") %>%
  #add_trace(name="HeadRotationY", data=D_sample, x =~Timestamp, y =~HeadCameraRotEulerYNorm, 
  #          type='scatter',mode='markers+lines', visible = "legendonly", hoverinfo='text',text=~HeadCameraRotEulerYWrap) %>%
  #add_trace(name="HeadRotationZ", data=D_sample, x =~Timestamp, y =~HeadCameraRotEulerZNorm,
  #          type='scatter',mode='markers+lines', visible = "legendonly", hoverinfo='text',text=~HeadCameraRotEulerZWrap) %>%
  # World Head Position: 0.1 = 1 meter
  #add_trace(name="HeadPositionX", data=D_sample, x =~Timestamp, y =~HeadCameraPosWorldXNorm, 
  #          type='scatter',mode='markers+lines', visible = "legendonly", hoverinfo='text',text=~HeadCameraPosWorldX) %>%
  #add_trace(name="HeadPositionY", data=D_sample, x =~Timestamp, y =~HeadCameraPosWorldYNorm, 
  #          type='scatter',mode='markers+lines', visible = "legendonly", hoverinfo='text',text=~HeadCameraPosWorldY) %>%
  #add_trace(name="HeadPositionZ", data=D_sample, x =~Timestamp, y =~HeadCameraPosWorldZNorm, 
  #          type='scatter',mode='markers+lines', visible = "legendonly", hoverinfo='text',text=~HeadCameraPosWorldZ) %>%  
  # World Gaze Position (not normalized yet)
  #add_trace(name="WorldGazeX", data=D_sample, x =~Timestamp, y =~WorldGazeHitPositionX, 
  #          type='scatter',mode='markers+lines', visible = "legendonly", hoverinfo='text',text=~WorldGazeHitPositionX) %>%
  #add_trace(name="WorldGazeY", data=D_sample, x =~Timestamp, y =~WorldGazeHitPositionY, 
  #          type='scatter',mode='markers+lines', visible = "legendonly", hoverinfo='text',text=~WorldGazeHitPositionY) %>%
  #add_trace(name="WorldGazeZ", data=D_sample, x =~Timestamp, y =~WorldGazeHitPositionZ, 
  #          type='scatter',mode='markers+lines', visible = "legendonly", hoverinfo='text',text=~WorldGazeHitPositionZ) %>%  
  layout(
    yaxis=list(dtick = 0.1,title=" ")
  )
