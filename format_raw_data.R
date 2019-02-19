# data cleaning
# 
# organize the information into a format (data frames) that will be amenable for
# sorting the data as appropriate

# where the data's at
bPath <- paste(getwd(),'/data/', sep='')

# folders within data
initListbPath <- paste(bPath,'init_lists/', sep='')
survListbPath <- paste(bPath,'survivor_lists/', sep='')
qcmetListbPath <- paste(bPath,'qc_metrics/', sep='')

################################################################################
# init list

allSubsDf <- read.table(paste(initListbPath,'t1+dwi_sublist.txt',sep=''))
colnames(allSubsDf) <- c("sub_name")

################################################################################
# survivor lists

survFiles <- dir(survListbPath, pattern = ".txt", full.names = TRUE)

for (idx in 1:length(survFiles)) {
    
    tmpFileName <- survFiles[idx]
    tmpDat <- read.table(tmpFileName, header = FALSE)
    colnames(tmpDat) <- c("sub_name")
    
    newColName <- tail(unlist(
        strsplit(sub('.txt','',tmpFileName),'_')),n=1)
    
    allSubsDf[[newColName]] <- allSubsDf$sub_name %in% tmpDat$sub_name 
    
}

################################################################################
# now get the qc metrics in order

tmpDf  <- as.data.frame(allSubsDf[,'sub_name'])
tmpFiles <- dir(qcmetListbPath, pattern = "numout.*csv", full.names = TRUE)

for (idx in 1:length(tmpFiles)) {
    
    tmpFileName <- tmpFiles[idx]
    tmpDat <- read.table(tmpFileName, header = FALSE, sep = ',')

    tmpStr <- tail(unlist(
        strsplit(sub('.csv','',tmpFileName),'_')),n=1)
    
    newColName <- paste("numO_",tmpStr, sep = '')
    colnames(tmpDat) <- c("sub_name",newColName,"ignore")

    # get the inds
    tmpInd <- allSubsDf$sub_name %in% tmpDat$sub_name 
    
    tmpDf[tmpInd,newColName] <- tmpDat[[newColName]]
    
}

# add the tmpDf to the allSubsDf
allSubsDf <- cbind(allSubsDf,tmpDf[,-1])

################################################################################
# qa_dti

tmpDf  <- as.data.frame(allSubsDf[,'sub_name'])

tmpFileName <- dir(qcmetListbPath, pattern = "qa_dti_group.csv", full.names = TRUE)
tmpDat <- read.table(tmpFileName, header = FALSE, sep = ',')

cc <- c("tsnr_t1","outlmean_t1","outlmax_t1","meanABSrms_t1","meanRELrms_t1")
colnames(tmpDat) <- c("sub_name",cc)

# get the inds
# this is reversed from before, because tmpData biger than allsubs
tmpInd <- tmpDat$sub_name  %in% allSubsDf$sub_name

tmpDf[,cc] <- tmpDat[tmpInd,cc]
  
# all
allSubsDf <- cbind(allSubsDf,tmpDf[,cc])

################################################################################
# time2

tmpDf  <- as.data.frame(allSubsDf[,'sub_name'])

tmpFileName <- dir(qcmetListbPath, pattern = "qa_dti_group_time2.csv", full.names = TRUE)
tmpDat <- read.table(tmpFileName, header = FALSE, sep = ',')

cc2 <- c("tsnr_t2","outlmean_t2","outlmax_t2","meanABSrms_t2","meanRELrms_t2")
colnames(tmpDat) <- c("sub_name",cc2)

# get the inds
# this is reversed from before, because tmpData biger than allsubs
tmpInd <- tmpDat$sub_name  %in% allSubsDf$sub_name

tmpDf[,cc2] <- tmpDat[tmpInd,cc]

# all
allSubsDf <- cbind(allSubsDf,tmpDf[,cc2])

################################################################################
# eddy qc

tmpDf  <- as.data.frame(allSubsDf[,'sub_name'])

tmpFileName <- dir(qcmetListbPath, pattern = "nki3_eddy_qc.csv", full.names = TRUE)
tmpDat <- read.table(tmpFileName, header = TRUE, sep = ',')

tmpInd <- tmpDat$sub_name  %in% allSubsDf$sub_name
tmpDf <- cbind(tmpDf,tmpDat[tmpInd,-1])

allSubsDf <- cbind(allSubsDf,tmpDf[,-1])

################################################################################
# parse the mriqc fmri file

tmpFileName <- dir(qcmetListbPath, pattern = "mri_qc_group_bold.tsv", full.names = TRUE)
fmri_mriqcDat <- read.table(tmpFileName, header = TRUE, sep = '\t')

acq1400Df <- fmri_mriqcDat[grep(".*acq-1400_bold",fmri_mriqcDat$bids_name),]
acq2500Df <- fmri_mriqcDat[grep(".*acq-2500_bold",fmri_mriqcDat$bids_name),]
acq645Df <- fmri_mriqcDat[grep(".*acq-645_bold",fmri_mriqcDat$bids_name),]

tmpDat <- strsplit(as.character(acq1400Df$bids_name),split = '_')
tmpStr <- rapply(tmpDat, function(x){paste(sub('sub-','',x[1]),'-',x[2],sep = '')})
acq1400Df['sub_name'] <- tmpStr

tmpDat <- strsplit(as.character(acq2500Df$bids_name),split = '_')
tmpStr <- rapply(tmpDat, function(x){paste(sub('sub-','',x[1]),'-',x[2],sep = '')})
acq2500Df['sub_name'] <- tmpStr

tmpDat <- strsplit(as.character(acq645Df$bids_name),split = '_')
tmpStr <- rapply(tmpDat, function(x){paste(sub('sub-','',x[1]),'-',x[2],sep = '')})
acq645Df['sub_name'] <- tmpStr

################################################################################






