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

allSubs <- read.table(paste(initListbPath,'t1+dwi_sublist.txt',sep=''))
colnames(allSubs) <- c("sub_name")

################################################################################
# survivor lists

survFiles <- dir(survListbPath, pattern = ".txt", full.names = TRUE)

for (idx in 1:length(survFiles)) {
    
    tmpFileName <- survFiles[idx]
    tmpDat <- read.table(tmpFileName, header = FALSE)
    colnames(tmpDat) <- c("sub_name")
    
    newColName <- tail(unlist(
        strsplit(sub('.txt','',tmpFileName),'_')),n=1)
    
    tmpFileName
    
    
    
}



