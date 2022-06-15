# Select Condition and Participant for playback.
v_Condition = "Test Condition 2"
v_Participant = 0


# MoleTemplate contains template data, used to generate the plotly animation data frame.
MoleTemplate <- D %>% group_by(Participant, Condition) %>% filter(!is.na(MoleId)) %>%
  rename(x = MolePositionWorldX, y = MolePositionWorldY, size = MoleSize, id = MoleId) %>%
  select(Participant, Condition, x,y, id) %>%
  distinct(id, .keep_all=T) %>% mutate(size = 0)

# MoleTemplate contains template data, used to generate the plotly animation data frame.
MoleData <- D %>% filter(Event %in% c("Mole Hit", "Mole Expired","Mole Spawned")) %>%
  rename(x = MolePositionWorldX, y = MolePositionWorldY, size = MoleSize, id = MoleId) %>%
  select(Timestamp, Participant, Condition, Event, x,y, id) %>%
  mutate(size = ifelse(Event == "Mole Spawned", 1, 0),
         second = as.POSIXct(paste(Timestamp), format = "%Y-%m-%d %H:%M:%S")) %>% select(-Timestamp)

# MoleData contains all the actual mole activity during the condition.
MoleData = MoleData %>% filter(Participant == v_Participant, Condition == v_Condition) %>%
  group_by(second) %>%
  group_modify(~left_join(MoleTemplate %>% filter(Participant == v_Participant, Condition == v_Condition),.x)) %>%
  mutate(size = ifelse(is.na(size), 0, size))

# First we generate all seconds in the animation.
Time = D %>% filter(Participant == v_Participant, Condition == v_Condition) %>%
  summarize(time_start = Timestamp[Event == "Game Started"],
            total_time = difftime(Timestamp[Event %in% c("Game Stopped", "Game Finished")], 
                                  Timestamp[Event == "Game Started"], units="secs"),
            total_time = as.integer(total_time),
            second = 0)
TimeAnimate= Time %>% uncount(total_time, .remove=F, .id="second") %>% bind_rows(Time) %>% arrange(second)

TimeAnimate = TimeAnimate %>%
  mutate(second = format(second, format = "%Y-%m-%d %H:%M:%S"),
         time_start = as.POSIXct(paste(time_start), format = "%Y-%m-%d %H:%M:%S"),
         second = seconds(second) + time_start) %>%
  select(second, time_start) %>%
  distinct(second, .keep_all=T)

# Then we generate the MoleTemplate for each second listed in MoleAnimate.
MoleAnimate = TimeAnimate %>%  group_by(second) %>%
  group_modify(~left_join(MoleTemplate %>% filter(Participant == v_Participant, Condition == v_Condition),.x))


DataTemplate = MoleTemplate %>% %>% mutate(name = "Active Mole") %>%
  bind_rows(LaserTemplate %>% rename(x = RightControllerLaserPosWorldX, y = RightControllerLaserPosWorldY) %>% mutate(name = "Laser", id="laser"))

# Then we generate the MoleTemplate for each second listed in MoleAnimate.
DataAnimate = TimeAnimate %>%  group_by(second) %>%
  group_modify(~left_join(DataTemplate %>% filter(Participant == v_Participant, Condition == v_Condition),.x))

DataAnimate %>% left_join(MoleData, by=c("second", id = "MoleId")) %>% view()


# Finally we join the actual Mole activity - full_join overwrites the MoleSize so it becomes 1 the correct places.
MoleAnimate = MoleAnimate %>% left_join(MoleData, by=c("second","id")) %>% group_by(id) %>%
  fill(Event, .direction="down") %>% 
  mutate(x = coalesce(x.x, x.y),
         y = coalesce(y.x, y.y),
         size = ifelse(Event == "Mole Spawned", 1, 0),
         size = ifelse(is.na(size), 0, size),
         second = as.numeric(difftime(second,time_start))) %>%
  select(-ends_with(".x"), -ends_with(".y")) %>% ungroup()

LaserTemplate <- D %>% filter(Participant == v_Participant, Condition == v_Condition) %>% 
  slice(1) %>% select(Participant, Condition) %>%
  mutate(RightControllerLaserPosWorldX = NA,
         RightControllerLaserPosWorldY = NA)

LaserAnimate = TimeAnimate %>% group_by(second) %>%
  group_modify(~left_join(LaserTemplate %>% filter(Participant == v_Participant, Condition == v_Condition),.x))

LaserData <- D %>% filter(PlayPeriod == "Game", Participant == v_Participant, Condition == v_Condition) %>%
  select(Timestamp, Participant, Condition, Event, RightControllerLaserPosWorldX,RightControllerLaserPosWorldY) %>%
  mutate(second = as.POSIXct(paste(Timestamp), format = "%Y-%m-%d %H:%M:%S")) %>% select(-Timestamp) %>%
  group_by(second) %>% 
  summarize(RightControllerLaserPosWorldX = mean(RightControllerLaserPosWorldX, na.rm=T),
            RightControllerLaserPosWorldY = mean(RightControllerLaserPosWorldY, na.rm=T))

LaserAnimate = MoleAnimate %>% ungroup() %>% left_join(LaserData, by="second") %>%
  mutate(RightControllerLaserPosWorldX = coalesce(RightControllerLaserPosWorldX.x, RightControllerLaserPosWorldX.y),
         RightControllerLaserPosWorldY = coalesce(RightControllerLaserPosWorldY.x, RightControllerLaserPosWorldY.y),
         second = as.numeric(difftime(second,time_start))) %>%
  select(-ends_with(".x"), -ends_with(".y"))


# Plot the animation with static Spawn Points and animated active moles.
fig %>%
  add_trace(name="Spawn Points", data=D %>% filter(PlayPeriod == "Game") %>% filter(Participant == v_Participant, Condition == v_Condition) %>% ungroup() %>% distinct(MoleId,.keep_all=T),
            x=~MolePositionWorldX, y=~MolePositionWorldY, type='scatter',
            mode='markers',symbol=I('o'),marker=list(size=32), 
            hoverinfo='text',text=~paste(MoleId,MolePositionWorldX,MolePositionWorldY)) %>%
  add_trace(name="Active Mole", data=MoleAnimate,
            x=~x, y=~y, type='scatter',mode='markers',frame=~second, marker=list(size=~size*32),
            hoverinfo='text',text=~paste(id,x,y)) %>%
  layout(xaxis = list(range=c(min(D$MolePositionWorldX,na.rm=T)-1, max(D$MolePositionWorldX,na.rm=T)+1)),
         yaxis = list(range=c(min(D$MolePositionWorldY,na.rm=T)-1, max(D$MolePositionWorldY,na.rm=T)+1))
  )

# Todo: Add pointer
add_trace(name="Laser Pointer R", data=LaserAnimate,
          x=~RightControllerLaserPosWorldX, y=~RightControllerLaserPosWorldY,
          type='scatter',mode='markers',frame=~second) %>%
  
  