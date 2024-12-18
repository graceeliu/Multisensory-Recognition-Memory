# fam_resps_group.R
#
# Created: 04/21/2024 By: Evan Layher (fam_resps_group.R)
# Revised: 04/21/2024 By: Evan Layher
#
# --- input file (csv) --- #
# [01] sub: subject ID
# [02] test: test type (aud vs. mem)
# [03] pair: stimulus pairing type
# [04] old4: old item, high confidence old response
# [05] old3: old item, low confidence old response
# [06] old2: old item, low confidence new response
# [07] old1: old item, high confidence new response
# [08] new4: new item, high confidence old response
# [09] new3: new item, low confidence old response
# [10] new2: new item, low confidence new response
# [11] new1: new item, high confidence new response

rm(list = ls()) # clear environment
require(dplyr)

dataDir <- 'C:\\Users\\ealay\\Box Sync\\google_drive\\ucsb\\lab\\face_audio_mem\\data\\'
dataFile <- paste(dataDir, 'face_aud_mem_resps.csv', sep = '', collapse = NULL)
outFile <- paste(dataDir, 'face_aud_mem_resps_group.csv', sep = '', collapse = NULL)

tConds <- c('aud','img')
pConds <- c('none','familiar','unfamiliar')

outHd <- 'cond,old4,old3,old2,old1,new4,new3,new2,new1' # output header

### CODE STARTS HERE ###
if (!file.exists(dataFile)) {
  stop('MISSING FILE: ', dataFile)
}

data <- read.csv(dataFile, header = TRUE) # read data

write.table(outHd, file = outFile, append = FALSE, quote = FALSE, sep = ',', row.names = FALSE, col.names = FALSE)
for (i in 1:length(tConds)) { # loop thru test conditions
  tC <- tConds[i] # test condition (audio vs. image)
  
  for (j in 1:length(pConds)) { # loop thru pairing condition
    pC <- pConds[j] # pair condition
    
    jC <- subset(data, test == tC & pair == pC)
    
    outC <- paste(tC, '-', pC, sep = '', collapse = NULL) # output condition label
    
    # sum response types across people
    o4 <- sum(jC$old4)
    o3 <- sum(jC$old3)
    o2 <- sum(jC$old2)
    o1 <- sum(jC$old1)

    if (j == 1) { # new responses recorded on first pass only
      n4 <- sum(as.numeric(as.character(jC$new4)))
      n3 <- sum(as.numeric(as.character(jC$new3)))
      n2 <- sum(as.numeric(as.character(jC$new2)))
      n1 <- sum(as.numeric(as.character(jC$new1)))
    }
    
    outLine <- paste(outC,o4,o3,o2,o1,n4,n3,n2,n1, sep = ',', collapse = NULL)
    
    write.table(outLine, file = outFile, append = TRUE, quote = FALSE, sep = ',', row.names = FALSE, col.names = FALSE)
  } # for (j in 1:length(pConds))
} # for (i in 1:length(tConds))

message('CREATED FILE: ', outFile)