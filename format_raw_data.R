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
# 




