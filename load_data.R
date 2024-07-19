require(tidyverse)
require(rio)

###------------ LOAD LOG DATA ----------------------------------------------####
# Function to list wanted csv files of GA and unzip them in a temporary folder
list_zipfiles <- function(zipfolder, analysis = "GA"){
  
  # list files (depending on the analysis)
  files <- grep(paste0(analysis, '.*csv'), 
                unzip(zipfolder, list=TRUE)$Name, 
                ignore.case=TRUE, value=TRUE)
  
  # unzip wanted XML files in temporary folder
  unzip(zipfolder, files=files, exdir = tf)
}

# Function to rename filenames with special characters in temporary folder
fix_zipfilenames <- function(unzippedfolder){
  invisible(sapply(list.files(tf, recursive = T, full.names = T), 
                   FUN = function(eachPath){
                     file.rename(from = eachPath,
                                 to = sub(pattern="Ã ", replacement="a",  
                                          eachPath))}))
  invisible(sapply(list.files(tf, recursive = T, full.names = T), 
                   FUN = function(eachPath){
                     file.rename(from = eachPath,
                                 to = sub(pattern="Ã«|Ã©|‚|Š|‰", replacement="e", 
                                          eachPath))}))
  invisible(sapply(list.files(tf, recursive = T, full.names = T), 
                   FUN = function(eachPath){
                     file.rename(from = eachPath,
                                 to = sub(pattern="â€¹|‹|Ã", replacement="i",  
                                          eachPath))}))
  invisible(sapply(list.files(tf, recursive = T, full.names = T), 
                   FUN = function(eachPath){
                     file.rename(from = eachPath,
                                 to = sub(pattern="Â¢|™|”|¢", replacement="o",  
                                          eachPath))}))
  invisible(sapply(list.files(tf, recursive = T, full.names = T), 
                   FUN = function(eachPath){
                     file.rename(from = eachPath,
                                 to = sub(pattern="\u0081|Ã¼", replacement="u",  
                                          eachPath))}))
  
  # output filenames
  list.files(tf, recursive = T, full.names = T)
}


###############################################################################
# run functions on zipfolders
# Make a temporary file (tf) and a temporary folder (tdir) to unzip
tf <- tempfile(tmpdir = tdir <- tempdir())

# list csv files and unzip them in temporary folder
list_zipfiles('ga-files_20221201.zip', 'GA')

# fix special characters in filenames
files_a <- fix_zipfilenames(tf)

# load all logs 
logs <-  do.call(bind_rows, lapply(files_a, read.csv,  sep = ";", 
                   stringsAsFactors = F, header = T, 
                   fileEncoding = "UTF-16LE")
)

# Delete temporary files
unlink(tdir, T, T)

###############################################################################
# anonymize and clean log
names(logs) = gsub(pattern = "__E_|__S_|__SL_|event_|sessionID_",
                   replacement = "", x = names(logs))

logs_a <- logs %>%
  select(-Session, -Group, -Experience, -Restricted_Logging, -filepath) %>%
  mutate(
    # clean columns
    Gender = tolower(gsub(" ", "", Gender)),
    Gender = ifelse(Gender %in% c("m", "man", "male", "jongen", "kerel"), 
                    "M",
             ifelse(Gender %in%  c("meisje", "vrouw", "female", "f", "v"), 
                    "F", NA)),
    RevisionNumber = as.numeric(gsub("\'", "", RevisionNumber)),
    RevisionPos = as.numeric(gsub("\'", "", RevisionNumber)),
    ParticipantID = as.numeric(as.factor(Participant))) %>%
  select(-Participant)


# write logs
write.csv(logs_a, "data_out/all_data.csv", row.names = F)



