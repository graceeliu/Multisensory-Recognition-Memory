# behave_face_aud_mem.R: face/audio memory task
#
# Created: 03/14/2024 By: Evan Layher (behave_face_aud_mem.R)
# Revised: 04/08/2024 By: Evan Layher
#
# --- Input file (csv) --- #
# [01] sub: subject ID
# [02] comp: computer name
# [03] date-time: date and time of event
# [04] taskTime: time since task start (seconds)
# [05] evType: type of event (e.g. misc, study, test)
# [06] evCnt: running count of all events
# [07] blk: block number
# [08] blkTrl: trial number within block
# [09] expTrl: trial number within experiment
# [10] testType: which modality presentation (aud, img, aud+img)
# [11] pair: type of test pairing (familiar, unfamiliar, none)
# [12] img: image file name
# [13] aud: audio file name
# [14] ans: correct answer
# [15] button: response button
# [16] conf: confidence rating
# [17] resp: response
# [18] acc: accuracy
# [19] rt: reaction time

# --- output file 1 (csv) --- #
# [01] sub: subject ID
# [02] test: test type (aud vs. mem)
# [03] pair: stimulus pairing type
# [04] c: criterion
# [05] d: d'
# [06] ca: unequal-variance criterion
# [07] da: unequal-variance d'
# [08] ht: hit count
# [09] ms: miss count
# [10] cr: correct rejection count
# [11] fa: false alarm count
# [12] oTrls: total old trials
# [13] nTrls: total new trials
# [14] mdRtO: median RT (old items)
# [15] mdRtN: median RT (new items)
# [16] mnRtO: mean RT (old items)
# [17] mnRtN: mean RT (new items)

# --- output file 2 (csv) --- #
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
require(stringr) # str_pad
require(Rmisc) # summarySEwithin

mainDir <- '/Users/graceliu/Desktop/face_audio_mem/'
codeDir <- paste(mainDir, 'code/', sep = '', collapse = NULL)
dataDir <- paste(mainDir, 'data/', sep = '', collapse = NULL)
rawDir <- paste(dataDir, 'raw/', sep = '', collapse = NULL)

source(paste(codeDir, 'sdt.R', sep = '', collapse = NULL)) # SDT

subs <- c(1:72) # subject numbers
dEx <- 0.5 # exclude if mean image d' less than this

outFile1 <- paste(dataDir, 'face_aud_mem_sdt.csv', sep = '', collapse = NULL) # output file (signal detection theory)
outFile2 <- paste(dataDir, 'face_aud_mem_resps.csv', sep = '', collapse = NULL) # output file (confidence/response types)

h1 <- c('sub','test','pair','c','d','ht','ms','cr','fa','oTrls','nTrls','mdRtO','mdRtN','mnRtO','mnRtN') # output file header 1
h2 <- c('sub','test','pair','old4','old3','old2','old1','new4','new3','new2','new1') # output file header 2

iHd <- 'mem_face_aud_' # input file header
iEx <- '.csv' # input file extension

# input file labels
accs <- c('hit','miss','cr','fa') # accuracy 'acc' (hit,miss,correct rejection,false alarm)
cnfs <- c('high','low') # confidence 'conf' (high,low)
errL <- 'err' # response 'resp' error label in file

tTps <- c('aud','img') # test types
pTps <- c('none','familiar','unfamiliar') # pairing types

adjVal <- 1 / 144 # adjust for infinite values
dp <- 3 # round to this many decimal places

exSubs <- c() # subjects to exclude
for (i in 1:length(subs)) { # loop thru subjects
  sub <- subs[i] # subject ID
  subIn <- str_pad(subs[i], 3, pad = '0') # subject ID with '0' pad

  subFile <- paste(iHd, subIn, iEx, sep = '', collapse = NULL)
  inFile <- paste(rawDir, subFile, sep = '', collapse = NULL)
  
  if (!file.exists(inFile)) { # check if file exists
    message('MISSING FILE: ', inFile)
    next()
  }
  
  message('LOADING FILE: ', subFile)
  
  data <- read.csv(inFile, header = TRUE) # read data
  tData <- subset(data, evType == 'test' & resp != errL & button != '') # test data

  chkD <- c() # reset d' check
  for (j in 1:length(tTps)) { # loop thru test types
    tTp <- tTps[j] # test type
    
    tD <- subset(tData, testType == tTp) # modality specific test
    
    nD <- subset(tD, ans == 'new') # new items
    mdRtN <- median(as.numeric(as.character(nD$rt)), na.rm = TRUE)
    mnRtN <- mean(as.numeric(as.character(nD$rt)), na.rm = TRUE)

    cr <- nrow(subset(tD, acc == accs[3])) # correct rejections
    fa <- nrow(subset(tD, acc == accs[4])) # false alarms
    
    n4 <- nrow(subset(tD, acc == accs[4] & conf == cnfs[1]))
    n3 <- nrow(subset(tD, acc == accs[4] & conf == cnfs[2]))
    n2 <- nrow(subset(tD, acc == accs[3] & conf == cnfs[2]))
    n1 <- nrow(subset(tD, acc == accs[3] & conf == cnfs[1]))
    
    for (k in 1:length(pTps)) { # loop thru pairing types
      pTp <- pTps[k] # pairing type
      
      cD <- subset(tD, pair == pTp) # condition specific data
      
      oD <- subset(cD, ans == 'old') # old items
      mdRtO <- median(as.numeric(as.character(oD$rt)), na.rm = TRUE)
      mnRtO <- mean(as.numeric(as.character(oD$rt)), na.rm = TRUE)
      
      ht <- nrow(subset(cD, acc == accs[1])) # hits
      ms <- nrow(subset(cD, acc == accs[2])) # misses
      
      hr <- ht / (ht + ms) # hit rate
      fr <- fa / (cr + fa) # false alarm rate
      sdtVals <- sdt(hrs = hr, fars = fr, adj = adjVal) # calculate equal variance SDT values

      outLine <- data.frame(sub,tTp,pTp,
                            format(round(sdtVals[1], dp), nsmall = dp), # c (equal variance)
                            format(round(sdtVals[2], dp), nsmall = dp), # d' (equal variance)
                            ht, ms, cr, fa, nrow(oD), nrow(nD), 
                            round(mdRtO, dp), round(mdRtN, dp),
                            round(mnRtO, dp), round(mnRtN, dp))
      
      if (j == 2) { # potentially exclude subject
        chkD <- c(chkD,round(sdtVals[2], dp))
      }
      
      colnames(outLine) <- h1
      if (!exists('df1')) { # initialize data frame on first pass
        df1 <- data.frame(outLine)
      } else { # append to data frame
        df1 <- rbind(df1,outLine)
      }
      
      o4 <- nrow(subset(cD, acc == accs[1] & conf == cnfs[1]))
      o3 <- nrow(subset(cD, acc == accs[1] & conf == cnfs[2]))
      o2 <- nrow(subset(cD, acc == accs[2] & conf == cnfs[2]))
      o1 <- nrow(subset(cD, acc == accs[2] & conf == cnfs[1]))
      
      if (pTp == 'none') {
        outLine <- data.frame(sub,tTp,pTp,o4,o3,o2,o1,n4,n3,n2,n1)
      } else {
        outLine <- data.frame(sub,tTp,pTp,o4,o3,o2,o1,'.','.','.','.')
      }
      
      colnames(outLine) <- h2
      if (!exists('df2')) { # initialize data frame on first pass
        df2 <- data.frame(outLine)
      } else { # append to data frame
        df2 <- rbind(df2,outLine)
      }
    } # for (k in 1:length(pTps))
  } # for (j in 1:length(tTps))
  
  if (mean(chkD) < dEx) { # do not include subject in final analysis
    message('EXLDUE SUBJECT: ', sub, " (mean image d' = ", format(round(mean(chkD), dp), nsmall = dp), ')')
    exSubs <- c(exSubs,sub)
  }
} # for (i in 1:length(subs))

df1fin <- df1 # create final df1 (after excluding subjects)
df2fin <- df2 # create final df2 (after excluding subjects)
for (i in 1:length(exSubs)) { # loop thru subjects to exclude
  exSub <- exSubs[i]
  df1fin <- subset(df1fin, sub != exSub)
  df2fin <- subset(df2fin, sub != exSub)
} # for (i in 1:length(exSubs))

# assign factors and make values numeric for stats analyses
df1fin$sub <- factor(df1fin$sub) # make sub column a factor
df1fin$c <- as.numeric(as.character(df1fin$c)) # make numeric
df1fin$d <- as.numeric(as.character(df1fin$d)) # make numeric

# mean values in each condition
cMean <- summarySEwithin(df1fin, measurevar = 'c', withinvars = c('test','pair'), idvar = 'sub') # mean c
dMean <- summarySEwithin(df1fin, measurevar = 'd', withinvars = c('test','pair'), idvar = 'sub') # mean d'

# ANOVA: audio
aDf1 <- subset(df1fin, test == 'aud') # audio only
aAov <- aov(d ~ pair, data = aDf1) # anova across audio conditions

# ANOVA: images
iDf1 <- subset(df1fin, test == 'img') # image only
iAov <- aov(d ~ pair, data = iDf1) # anova across image conditions

# if ANOVA is significant use t-test to determine which pairs are significantly different
aFam <- subset(df1fin, test == 'aud' & pair == 'unfamiliar') %>% dplyr::select(d)
aNon <- subset(df1fin, test == 'aud' & pair == 'none') %>% dplyr::select(d)
aUnf <- subset(df1fin, test == 'aud' & pair == 'familiar') %>% dplyr::select(d)

# t-tests
aFvU <- t.test(aFam, aUnf)

# output summary of subject results
write.table(df1fin, file = outFile1, append = FALSE, quote = FALSE, sep = ',', row.names = FALSE, col.names = TRUE)
write.table(df2fin, file = outFile2, append = FALSE, quote = FALSE, sep = ',', row.names = FALSE, col.names = TRUE)

message('CREATED FILE 1: ', outFile1)
message('CREATED FILE 2: ', outFile2)

message('EXCLUDED ', length(exSubs), ' SUBJECTS')