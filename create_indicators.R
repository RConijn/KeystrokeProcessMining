require(tidyverse)
require(stringi)

# load log data
log <- read_csv("data_out/all_logs_extended.csv", 
                locale = locale(encoding = "Latin1"))


##############################################################################
#create indicators

log_ind <- log %>%
  group_by(ParticipantID) %>%
  mutate(
    location = ifelse(type == "focus", output, NA),
    output2 = stri_trans_general(output, "Latin-ASCII")
    ) %>%
  fill(location) %>%
  mutate(
    reading_assignment_start = ifelse(location == "Task assignment" & 
                                  lag(location) != "Task assignment", 1, 0),
    reading_assignment_count = ifelse(location == "Task assignment",
                                      cumsum(reading_assignment_start), 0),
    reading_source_start = ifelse(location == "Source texts" & 
                                        lag(location) != "Source texts", 1, 0),
    reading_source_count = ifelse(location == "Source texts",
                                      cumsum(reading_source_start), 0),
    positionchange = lag(positionFull) - positionFull,
    action = case_when(
    #Initial reading assignment 
    reading_assignment_count == 1 ~ "Initial reading assignment",  
    #Rereading assignment
    reading_assignment_count > 1 ~ "Rereading assignment",  
    #Initial reading sources
    reading_source_count == 1 ~ "Initial reading sources",    
    #Rereading sources
    reading_source_count > 1 ~ "Rereading sources",
    #Off task
    location == "Other sources" ~ "Off Task",
    #Move to document
    output == "Wordlog_inText" & type == "focus" ~ "Shift to writing document",
    
    ## ALL these are location == Wordlog_inText
    
    # REVISIONS (Thorson, 2000)
    # final revision
    #revision_no == max(revision_no, na.rm = T) ~ "Final revision", 
    # immediate deletion (max. 1 char away from prev. cursor position) 
    # immediate large deletion: two or more (partial) words deleted
    #   (check for removed characters with one or more spaces (_), 
    #   surrounded by one or more non-space characters)
    revtype == "deletion" &
      abs(positionchange) <= 1 &
      grepl("([^_])+(_)+([^_])+", removed_chars) 
      ~ "Immediate large deletion",
    # immediate small deletion
    revtype == "deletion" &
      abs(positionchange) <= 1 ~ "Immediate small deletion",
    # distant small deletion
    revtype == "deletion" &
      abs(positionchange) > 1 &
      grepl("([^_])+(_)+([^_])+", removed_chars) ~ "Distant large deletion",
    # distant large deletion
    revtype == "deletion" &
      abs(positionchange) > 1 ~ "Distant small deletion",
    # (distant) insertion
    revtype == "insertion" | type == "insert" ~ "Distant insertion",
    
    #Generating text (transcribing)
    type == "keyboard" & (nchar(output2) == 1 | 
                            output %in% c("SPACE", "ENTER", "RETURN", "TAB", 
                                          "CAPS LOCK", "RSHIFT", "LSHIFT") |
                            grepl("Ã|Â", output)) ~ 
      "Generating text", 
    #Using function keys
    type == "keyboard" &  (output %in% c("NUMLOCK", "OEM_6", "RSHIFT + LSHIFT",
                                        "LSHIFT + RSHIFT", "LWIN", "INSERT") |
      grepl("ALT|CTRL", output)) ~ "Using function keys",
    #Navigating in own text: local & global navigation
    (type == "keyboard" &  (output %in% c("UP", "DOWN", "LEFT", "RIGHT", "HOME", 
                                          "END", "UP", "DOWN", "NEXT", 
                                          "END + UP", "PRIOR") 
                            | grepl("RIGHT", output)) |
    (type == "mouse"))  ~ "Navigation",  
      
    #Pausing 
    # NOT USED (will be used when aggregating script)
    
    #unknown behavior 
    TRUE ~ "unknown"
    ) 
  ) %>% 
  # categorize leftovers:
  # selection of text is categorized based on the next action 
  #    (either navigation or revision)
  # consecutive backs at end of doc as large immediate deletion
  mutate(action = case_when(
    action == "unknown" & type == "replacement" & lead(action) %in% 
      c("Navigation", "Generating text","Using function keys") ~ "Navigation",
    action == "unknown" & type == "replacement" & lead(action) != "unknown" ~ 
      lead(action), 
    action == "unknown" & output %in% c("BACK","DELETE") ~
      "Immediate large deletion",
    action == "unknown" & type == "replacement" ~ "Navigation",
    TRUE ~ action)) %>%
  ungroup()


# check distribution of actions
table(log_ind$action)

# check undefined actions
unknown <- log_ind %>%
  filter(action == "unknown" )


###### Merge actions ###### 
action_log <- log_ind %>%
  group_by(ParticipantID) %>%
  mutate(action_start = (row_number() == 1 | lag(action) != action), 
         action_count = cumsum(action_start)) %>%
  group_by(ParticipantID, action_count) %>%
  summarize(
    position_change = sum(positionchange),
    time_start = first(startTime),
    time_end = last(endTime),
    action = first(action),
    output = paste0(output, collapse = ""),
  ) %>%
  mutate(
 # Distinguish between local /broader navigation 
 # Now local is within 10 characters (later we can check per sentence/ x words)
    action = case_when(action == "Navigation" & abs(position_change) <= 7 ~
                       "Local navigation",
                       action == "Navigation" & abs(position_change) > 7 ~
                       "Global navigation", 
                       action == "Distant insertion" & grepl(".+NA.+", output) ~
                         "Distant large insertion",
                       action == "Distant insertion" ~
                         "Distant small insertion",
                       action == "Generating text" & grepl(".+SPACE.+", output) ~
                         "Generating large chunk of text",
                       action == "Generating text" ~
                         "Generating small chunk of text",
                       TRUE ~ action)
  ) %>% 
  ungroup()

# check distribution of actions
table(action_log$action)

# write action log without pauses
write.csv(action_log, "data_out/actionlog_nopauses_nofinal_textsplit.csv")


action_log_pauses <- log_ind %>%
  group_by(ParticipantID) %>%
  mutate(action_start = (row_number() == 1 | lag(action) != action) |
           # pauseTime > 2 secs results in new activity
           pauseTime > 2000,
         action_count = cumsum(action_start)) %>%
  group_by(ParticipantID, action_count) %>%
  summarize(
    position_change = sum(positionchange),
    time_start = first(startTime),
    time_end = last(endTime),
    action = first(action),
    output = paste0(output, collapse = "")
  ) %>%
  mutate(
    # Distinguish between local /broader navigation 
    # Now local is within 10 characters (later we can check per sentence/ x words)
    action = case_when(action == "Navigation" & abs(position_change) <= 10 ~
                         "Local Navigation",
                       action == "Navigation" & abs(position_change) > 10 ~
                         "Global Navigation", 
                       action == "Distant insertion" & grepl(".+NA.+", output) ~
                         "Distant large insertion",
                       action == "Distant insertion" ~
                         "Distant small insertion",
                       action == "Generating text" & grepl(".+SPACE.+", output) ~
                         "Generating large chunk of text",
                       action == "Generating text" ~
                         "Generating small chunk of text",
                       TRUE ~ action)
  ) %>% 
  ungroup()

# check distribution of actions
table(action_log_pauses$action)

# write action log with pauses
write.csv(action_log_pauses, "data_out/actionlog_pauses_2sec_nofinal_textsplit.csv")


###  log_pauses_activity : pauses added as an activity
action_log_act <- bind_rows(
  log_ind,
  log_ind %>% filter(pauseTime >2000) %>% mutate(id = id - .1,
                                                        action = "Pause")) %>%
  arrange(ParticipantID, id, rev_char_no) %>%
  mutate(
    startTime = case_when(action == "Pause" ~ lag(endTime), TRUE ~ startTime),
    endTime = case_when(action == "Pause" ~ lead(startTime), TRUE ~ endTime)
  )  %>%
  group_by(ParticipantID) %>%
  mutate(action_start = (row_number() == 1 | lag(action) != action) ,
           action_count = cumsum(action_start)) %>%
  group_by(ParticipantID, action_count) %>%
  summarize(
    position_change = sum(positionchange),
    time_start = first(startTime),
    time_end = last(endTime),
    action = first(action),
    output = paste0(output, collapse = "")
  ) %>%
  mutate(
    # Distinguish between local /broader navigation 
    # Now local is within 10 characters (later we can check per sentence/ x words)
    action = case_when(action == "Navigation" & abs(position_change) <= 10 ~
                         "Local Navigation",
                       action == "Navigation" & abs(position_change) > 10 ~
                         "Global Navigation", 
                       action == "Distant insertion" & grepl(".+NA.+", output) ~
                         "Distant large insertion",
                       action == "Distant insertion" ~
                         "Distant small insertion",
                       action == "Generating text" & grepl(".+SPACE.+", output) ~
                         "Generating large chunk of text",
                       action == "Generating text" ~
                         "Generating small chunk of text",
                       TRUE ~ action)
  ) %>% 
  ungroup()

# check distribution of actions
table(action_log_act$action)

# write action log with pauses
write.csv(action_log_act, "data_out/actionlog_pauses_activity_nofinal_textsplit.csv")



############################################

## EXPLORATION
# note taking
# read - writing (maybe typo) & read again
notetaking <- action_log %>% 
  filter(!action %in% c("unknown", "Global Navigation", "Local Navigation")) %>% 
  filter(action %in% c("Rereading sources", "Initial reading sources") & 
           lead(action) == "Generating text")

# reading source, navigation, text generation
notetaking <- action_log %>% filter(action != "unknown") %>%
  filter(action %in% c("Rereading sources", "Initial reading sources") | lead(action) %in% c("Rereading sources", "Initial reading sources") | lead(action,2) %in% c("Rereading sources", "Initial reading sources")|lag(action) %in% c("Rereading sources", "Initial reading sources"))


# copypasting
copy <- log_ind %>% filter(grepl("CTRL +",output ), !grepl("ALT", output),
                           pauseLocationFull == "COMBINATION KEY")
table(copy$output)  
# copy pasting occurs only 7 times via keyboard, so not used