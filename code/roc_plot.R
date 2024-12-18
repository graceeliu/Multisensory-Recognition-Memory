# roc_plot.R: ROC (maximum likelihood)

# Created: 04/22/2024 By: Evan Layher (roc_plot.R)
# Revised: 04/22/2024 By: Evan Layher

# --- input file 1 (csv) --- #
# [01] cond: condition
# [02] t1: threshold 1 (full model)
# [03] t2: threshold 2 (full model)
# [04] t3: threshold 3 (full model)
# [05] a : A'
# [06] b : B'
# [07] da: da
# [08] az: Az
# [09] d : d' (target distribution mean)
# [10] vo: vo (target distribution standard deviation)

rm(list = ls()) # clear environment
require(dplyr)
require(tidyverse)
require(stringr) # str_pad, str_detect
require(Rmisc) # summarySEwithin

dataDir <- '/Users/graceliu/Desktop/face_audio_mem/data/'
plotDir <- dataDir
inFile1 <- paste(dataDir, 'fam_max_est.csv', sep = '', collapse = NULL) # maximum likelihood estimates
inFile2 <- paste(dataDir, 'face_aud_mem_resps_group.csv', sep = '', collapse = NULL) # response counts
outFig <- 'group_roc.jpg'

# graph text
szLne <- 1 # size of graph lines
szTtl <- 30 # size of title
szAxi <- 25 # size of axis titles
szTxt <- 20 # size of axis text
szLab <- 8 # size of label
szLgn <- 20 # size of legend
szPnt <- 4 # size of points
szStk <- 2 # size of stroke
dp <- 2 # round to this many decimal places

# graph parameters (ROC)
fDim <- c(8,8) # ROC plot: width, height
fLim <- c(0,1) # limits for ROC
zfLim <- c(-3.1,3.1) # limits for zROC
fBrk <- c(0,0.2,0.4,0.6,0.8,1) # break points for x/y axes
zfBrk <- c(-3,-2,-1,0,1,2,3) # break points for x/y axes
fR <- 80 # image resolution (DPI)

lgnx <- 0.70 # legend position x
lgny <- 0.21 # legend position y
yDwn <- 0.065 # (0 - 1) second label this far below first

ttl <- 'ROC Curve'
xName <- 'False Alarm Rate' # x-axis name
yName <- 'Hit Rate' # y-axis name

data1 <- read.csv(inFile1, header = TRUE)
data2 <- read.csv(inFile2, header = TRUE)

# compute cumulative hit and false alarm rates
data2$cm4Hr <- data2$old4 / (data2$old4 + data2$old3 + data2$old2 + data2$old1)
data2$cm3Hr <- (data2$old4 + data2$old3) / (data2$old4 + data2$old3 + data2$old2 + data2$old1)
data2$cm2Hr <- (data2$old4 + data2$old3 + data2$old2) / (data2$old4 + data2$old3 + data2$old2 + data2$old1)
data2$cm1Hr <- 1
  
data2$cm4Fa <- data2$new4 / (data2$new4 + data2$new3 + data2$new2 + data2$new1)
data2$cm3Fa <- (data2$new4 + data2$new3) / (data2$new4 + data2$new3 + data2$new2 + data2$new1)
data2$cm2Fa <- (data2$new4 + data2$new3 + data2$new2) / (data2$new4 + data2$new3 + data2$new2 + data2$new1)
data2$cm1Fa <- 1

# cumulative sum hit rates
anHr <- as.numeric(unlist(subset(data2, cond == 'aud-none') %>% select(cm1Hr, cm2Hr, cm3Hr, cm4Hr)))
afHr <- as.numeric(unlist(subset(data2, cond == 'aud-familiar') %>% select(cm1Hr, cm2Hr, cm3Hr, cm4Hr)))
auHr <- as.numeric(unlist(subset(data2, cond == 'aud-unfamiliar') %>% select(cm1Hr, cm2Hr, cm3Hr, cm4Hr)))
inHr <- as.numeric(unlist(subset(data2, cond == 'img-none') %>% select(cm1Hr, cm2Hr, cm3Hr, cm4Hr)))
ifHr <- as.numeric(unlist(subset(data2, cond == 'img-familiar') %>% select(cm1Hr, cm2Hr, cm3Hr, cm4Hr)))
iuHr <- as.numeric(unlist(subset(data2, cond == 'img-unfamiliar') %>% select(cm1Hr, cm2Hr, cm3Hr, cm4Hr)))
  
# cumulative sum false alarm rates
anFr <- as.numeric(unlist(subset(data2, cond == 'aud-none') %>% select(cm1Fa, cm2Fa, cm3Fa, cm4Fa)))
afFr <- as.numeric(unlist(subset(data2, cond == 'aud-familiar') %>% select(cm1Fa, cm2Fa, cm3Fa, cm4Fa)))
auFr <- as.numeric(unlist(subset(data2, cond == 'aud-unfamiliar') %>% select(cm1Fa, cm2Fa, cm3Fa, cm4Fa)))
inFr <- as.numeric(unlist(subset(data2, cond == 'img-none') %>% select(cm1Fa, cm2Fa, cm3Fa, cm4Fa)))
ifFr <- as.numeric(unlist(subset(data2, cond == 'img-familiar') %>% select(cm1Fa, cm2Fa, cm3Fa, cm4Fa)))
iuFr <- as.numeric(unlist(subset(data2, cond == 'img-unfamiliar') %>% select(cm1Fa, cm2Fa, cm3Fa, cm4Fa)))

# data frames
anDf <- data.frame(cond = 'aud-none', cHr = anHr, cFr = anFr)
afDf <- data.frame(cond = 'aud-familiar', cHr = afHr, cFr = afFr)
auDf <- data.frame(cond = 'aud-unfamiliar', cHr = auHr, cFr = auFr)
inDf <- data.frame(cond = 'img-none', cHr = inHr, cFr = inFr)
ifDf <- data.frame(cond = 'img-familiar', cHr = ifHr, cFr = ifFr)
iuDf <- data.frame(cond = 'img-unfamiliar', cHr = iuHr, cFr = iuFr)

idf <- rbind(anDf,afDf,auDf,inDf,ifDf,iuDf) # combine all points into single data frame
idf$cond <- factor(idf$cond, levels = c('aud-none','aud-familiar','aud-unfamiliar','img-none','img-familiar','img-unfamiliar'))

# create ROC curve from minimum false alarm value to 1
anMinFa <- min(anDf$cFr)
afMinFa <- min(afDf$cFr)
auMinFa <- min(auDf$cFr)
inMinFa <- min(inDf$cFr)
ifMinFa <- min(ifDf$cFr)
iuMinFa <- min(iuDf$cFr)
  
anFRs <- seq(anMinFa, .999, by = .001) # sequence of false alarm values
afFRs <- seq(afMinFa, .999, by = .001) # sequence of false alarm values
auFRs <- seq(auMinFa, .999, by = .001) # sequence of false alarm values
inFRs <- seq(inMinFa, .999, by = .001) # sequence of false alarm values
ifFRs <- seq(ifMinFa, .999, by = .001) # sequence of false alarm values
iuFRs <- seq(iuMinFa, .999, by = .001) # sequence of false alarm values
  
aN <- subset(data1, cond == 'aud-none')       # audio: no pairing
aF <- subset(data1, cond == 'aud-familiar')   # audio: familiar pairing
aU <- subset(data1, cond == 'aud-unfamiliar') # audio: unfamiliar pairing
iN <- subset(data1, cond == 'img-none')       # image: no pairing
iF <- subset(data1, cond == 'img-familiar')   # image: familiar pairing
iU <- subset(data1, cond == 'img-unfamiliar') # image: unfamiliar pairing

# compute curves from hit rates given false alarm rates
anh <- pnorm(qnorm(anFRs) * aN$b + aN$a) # predicted hit rate (audio: no pairing)
afh <- pnorm(qnorm(afFRs) * aF$b + aF$a) # predicted hit rate (audio: familiar pairing)
auh <- pnorm(qnorm(auFRs) * aU$b + aU$a) # predicted hit rate (audio: unfamiliar pairing)
inh <- pnorm(qnorm(inFRs) * iN$b + iN$a) # predicted hit rate (image: no pairing)
ifh <- pnorm(qnorm(ifFRs) * iF$b + iF$a) # predicted hit rate (image: familiar pairing)
iuh <- pnorm(qnorm(iuFRs) * iU$b + iU$a) # predicted hit rate (image: unfamiliar pairing)
      
# create data frames for hit/FA rates
anDfCrv <- data.frame(cHr = anh, cFr = anFRs)
afDfCrv <- data.frame(cHr = afh, cFr = afFRs)
auDfCrv <- data.frame(cHr = auh, cFr = auFRs)
inDfCrv <- data.frame(cHr = inh, cFr = inFRs)
ifDfCrv <- data.frame(cHr = ifh, cFr = ifFRs)
iuDfCrv <- data.frame(cHr = iuh, cFr = iuFRs)
  
lgnLabs <- c(paste('Audio (none)'), 
            paste('Audio (familiar)'),
            paste('Audio (unfamliar)'), 
            paste('Image (none)'), 
            paste('Image (familiar)'),
            paste('Image (unfamliar)')) # d' label

iCols <- c("deepskyblue", "yellow", "orange", "red", "purple", "green") # colors

# ROC plot
ggplot(idf, aes(x = cFr, y = cHr)) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), linetype = 'solid', color = 'black', linewidth = szLne) + # diagonal line (more control than geom_abline)
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 0), linetype = 'solid', color = 'black', linewidth = szLne) + # x-axis line
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1), linetype = 'solid', color = 'black', linewidth = szLne) + # y-axis line
  theme_minimal() +
  geom_path(data = anDfCrv, aes(x = cFr, y = cHr), color = iCols[1], linewidth = szLne) + # audio: no pairing curve
  geom_path(data = afDfCrv, aes(x = cFr, y = cHr), color = iCols[2], linewidth = szLne) + # audio: familiar pairing curve
  geom_path(data = auDfCrv, aes(x = cFr, y = cHr), color = iCols[3], linewidth = szLne) + # audio: unfamiliar pairing curve
  geom_path(data = inDfCrv, aes(x = cFr, y = cHr), color = iCols[4], linewidth = szLne) + # image: no pairing curve
  geom_path(data = ifDfCrv, aes(x = cFr, y = cHr), color = iCols[5], linewidth = szLne) + # image: familiar pairing curve
  geom_path(data = iuDfCrv, aes(x = cFr, y = cHr), color = iCols[6], linewidth = szLne) + # image: unfamiliar pairing curve
  geom_point(data = idf, aes(x = cFr, y = cHr, shape = factor(cond), color = factor(cond), fill = factor(cond)), stroke = szStk, size = szPnt) + # HR/FAR data
  theme(axis.title = element_text(size = szAxi, face = "bold"), # format title axes
      axis.title.x = element_text(margin = margin(10,0,5,0)), # format x-axis margin (top, right, bottom, left)
      axis.title.y = element_text(margin = margin(0,10,0,5)), # format y-axis margin (top, right, bottom, left)
      axis.text = element_text(size = szTxt, face = "bold", color = "black"), # format axis text
      plot.title = element_text(size = szTtl, face = "bold", hjust = '0.5', margin = margin(0,0,10,0)), # format plot title (if any)
      legend.text = element_text(size = szLgn, face = "bold"),  # format legend text
      legend.title = element_text(size = szLgn, face = "bold"), # format legend title
      legend.position = c(lgnx,lgny), # x/y coordinates for place legend
      legend.key.size = unit(0.9, 'cm'), # spacing between legend items
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) + 
  xlab(xName) + ylab(yName) + # Name of x/y labels
  ggtitle(ttl) +
  scale_x_continuous(limits = fLim, breaks = fBrk, labels = fBrk, expand = c(.005,.005)) + # format x-axis value range
  scale_y_continuous(limits = fLim, breaks = fBrk, labels = fBrk, expand = c(.005,.005)) + # format y-axis value range
  scale_fill_manual(name = 'Condition', labels = lgnLabs, values = iCols) + # specify colors (fill)
  scale_color_manual(name = 'Condition', labels = lgnLabs, values = rep("black", each = length(iCols))) + # specify colors (line border)
  scale_shape_manual(name = 'Condition', labels = lgnLabs, values = c(21,21,21,21,21,21)) # specify shapes (23 = filled diamond, 21 = filled circle)
  ggsave(paste(plotDir, outFig, sep = '', collapse = NULL), height = fDim[2], width = fDim[1], units = 'in', dpi = fR) # save plot (specify dimensions and resolution)
message('CREATED PLOT: ', outFig) # alert user when plot is created
