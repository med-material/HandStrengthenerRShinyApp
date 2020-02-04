#### MANUAL DATA
# This script manipulates our global dataframe 'D' and adds extra data manually through scripting.

newsettingsPID = c(602,603,604) # PIDs which did have prior training
oldsettingsPID = c(101, 600, 108, 206, 210, 402, 404, 601, 302, 303) #PIDs which had no prior training

# Add participant ages
D$Age = NA
D$Age[D$PID == 101] <- 25
D$Age[D$PID == 602] <- 23
D$Age[D$PID == 604] <- 24
D$Age[D$PID == 603] <- 22
D$Age[D$PID == 210] <- 26
D$Age[D$PID == 601] <- 30
D$Age[D$PID == 404] <- 23
D$Age[D$PID == 108] <- 30
D$Age[D$PID == 402] <- 30
D$Age[D$PID == 206] <- 23
D$Age[D$PID == 600] <- 25
D$Age[D$PID == 302] <- 22
D$Age[D$PID == 303] <- 26

# Add participant genders
D$Gender = NA
D$Gender[D$PID == 101] <- 'M'
D$Gender[D$PID == 602] <- 'F'
D$Gender[D$PID == 604] <- 'M'
D$Gender[D$PID == 603] <- 'M'
D$Gender[D$PID == 210] <- 'M'
D$Gender[D$PID == 601] <- 'M'
D$Gender[D$PID == 404] <- 'F'
D$Gender[D$PID == 108] <- 'M'
D$Gender[D$PID == 402] <- 'M'
D$Gender[D$PID == 206] <- 'M'
D$Gender[D$PID == 600] <- 'M'
D$Gender[D$PID == 302] <- 'M'
D$Gender[D$PID == 303] <- 'M'

#Add participant motoric impairments
D$MotoricImpairment = FALSE
D$MotoricImpairment[D$PID == 602] <- TRUE # TODO: Check with Yohann if this is correct.
D$Gender[D$PID == 600] <- NA

#Add procedure version
D$ProcedureVersion = NA # What version of the procedure the participant received ("v1", "v2")
D = D %>% mutate(ProcedureVersion = ifelse(PID %in% newsettingsPID, "v1", "v0.9"))

# Factorize a number of variables.
D$PID <- factor(D$PID, ordered = TRUE) #ordered = TRUE, so plotly stops plotting it as numeric value.

# TODO, could be useful
#df$BaguetteSize <- factor(df$BaguetteSize)
#levels(df$BaguetteSize) <- c("Small", "Medium", "Large")
#df$DateAdded <- as.POSIXct(df$DateAdded, format = "%Y-%m-%d %H:%M:%S")
