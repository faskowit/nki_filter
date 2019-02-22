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
colnames(tmpDf) <- "sub_name"

tmpFiles <- dir(qcmetListbPath, pattern = "numout.*csv", full.names = TRUE)

for (idx in 1:length(tmpFiles)) {
    
    tmpFileName <- tmpFiles[idx]
    tmpDat <- read.table(tmpFileName, header = FALSE, sep = ',')

    tmpStr <- tail(unlist(
        strsplit(sub('.csv','',tmpFileName),'_')),n=1)
    
    newColName <- paste("numO_",tmpStr, sep = '')
    colnames(tmpDat) <- c("sub_name",newColName,"ignore")
    
    # label 'no' outliers as 0 outliers
    tmpDat[is.na(tmpDat[,newColName]),newColName] = 0
    
    tmpDf <- merge(tmpDf,tmpDat[c("sub_name",newColName)],by = "sub_name", all.x = TRUE )

}

# add the tmpDf to the allSubsDf
allSubsDf <- merge(allSubsDf,tmpDf,by = "sub_name", all.x = TRUE )

################################################################################
# qa_dti

tmpDf  <- as.data.frame(allSubsDf[,'sub_name'])

tmpFileName <- dir(qcmetListbPath, pattern = "qa_dti_group.csv", full.names = TRUE)
tmpDat <- read.table(tmpFileName, header = FALSE, sep = ',')

cc <- c("tsnr_t1","outlmean_t1","outlmax_t1","meanABSrms_t1","meanRELrms_t1")
colnames(tmpDat) <- c("sub_name",cc)

allSubsDf <- merge(allSubsDf,tmpDat,by = "sub_name", all.x = TRUE )

################################################################################
# time2

tmpDf  <- as.data.frame(allSubsDf[,'sub_name'])

tmpFileName <- dir(qcmetListbPath, pattern = "qa_dti_group_time2.csv", full.names = TRUE)
tmpDat <- read.table(tmpFileName, header = FALSE, sep = ',')

cc2 <- c("tsnr_t2","outlmean_t2","outlmax_t2","meanABSrms_t2","meanRELrms_t2")
colnames(tmpDat) <- c("sub_name",cc2)

allSubsDf <- merge(allSubsDf,tmpDat,by = "sub_name", all.x = TRUE )

################################################################################
# eddy qc

tmpDf  <- as.data.frame(allSubsDf[,'sub_name'])

tmpFileName <- dir(qcmetListbPath, pattern = "nki3_eddy_qc.csv", full.names = TRUE)
tmpDat <- read.table(tmpFileName, header = TRUE, sep = ',')

allSubsDf <- merge(allSubsDf,tmpDat,by = "sub_name", all.x = TRUE )

################################################################################
# parse the mriqc fmri file

tmpFileName <- dir(qcmetListbPath, pattern = "mri_qc_group_bold.tsv", full.names = TRUE)
fmri_mriqcDat <- read.table(tmpFileName, header = TRUE, sep = '\t')

acq1400Df <- fmri_mriqcDat[grep(".*acq-1400_bold",fmri_mriqcDat$bids_name),]
attr(acq1400Df,"acqname") <- "acq-1400"
acq2500Df <- fmri_mriqcDat[grep(".*acq-2500_bold",fmri_mriqcDat$bids_name),]
attr(acq2500Df,"acqname") <- "acq-2500"
acq645Df <- fmri_mriqcDat[grep(".*acq-645_bold",fmri_mriqcDat$bids_name),]
attr(acq645Df,"acqname") <- "acq-645"

tmpDat <- strsplit(as.character(acq1400Df$bids_name),split = '_')
tmpStr <- rapply(tmpDat, function(x){paste(sub('sub-','',x[1]),'-',x[2],sep = '')})
acq1400Df['sub_name'] <- tmpStr

tmpDat <- strsplit(as.character(acq2500Df$bids_name),split = '_')
tmpStr <- rapply(tmpDat, function(x){paste(sub('sub-','',x[1]),'-',x[2],sep = '')})
acq2500Df['sub_name'] <- tmpStr

tmpDat <- strsplit(as.character(acq645Df$bids_name),split = '_')
tmpStr <- rapply(tmpDat, function(x){paste(sub('sub-','',x[1]),'-',x[2],sep = '')})
acq645Df['sub_name'] <- tmpStr

fmri_mriqc_list <- list() 
fmri_mriqc_list[[1]] <- acq1400Df 
fmri_mriqc_list[[2]] <- acq2500Df 
fmri_mriqc_list[[3]] <- acq645Df 

################################################################################
# parse the mriqc t1 file

tmpFileName <- dir(qcmetListbPath, pattern = "mri_qc_group_T1w.tsv", full.names = TRUE)
t1_mriqcDf <- read.table(tmpFileName, header = TRUE, sep = '\t')

t1_mriqcDat <- strsplit(as.character(t1_mriqcDf$bids_name),split = '_')
tmpStr <- rapply(t1_mriqcDat, function(x){paste(sub('sub-','',x[1]),'-',x[2],sep = '')})
t1_mriqcDf['sub_name'] <- tmpStr

################################################################################
# add some of the IQMs to the allSubsDf

tmpDf  <- as.data.frame(allSubsDf[,'sub_name'])

# T1 IQCMs: cjv, cnr, snr_total, snrd_total
t1_iqm <- c("cjv", "cnr", "snr_total", "snrd_total")

tmpDf <- t1_mriqcDf[c("sub_name",t1_iqm)]
colnames(tmpDf) <- c("sub_name",paste(t1_iqm,"_t1w",sep=""))

allSubsDf <- merge(allSubsDf,tmpDf,by = "sub_name", all.x = TRUE )

################################################################################

tmpDf  <- as.data.frame(allSubsDf[,'sub_name'])
colnames(tmpDf) <- "sub_name"

# FMRI IQMs: dvars_nstd, tsnr, fd_mean, aqi
fmri_qcm <- c("dvars_nstd", "dvars_vstd", "tsnr", "fd_mean", "aqi")

for (idx in 1:length(fmri_mriqc_list)) {
    
    tmpDat <- as.data.frame(fmri_mriqc_list[[idx]])
    tmpStr <- attr(tmpDat,"acqname")

    tmpDf2 <- tmpDat[c("sub_name",fmri_qcm)]
    colnames(tmpDf2) <- c("sub_name",paste(fmri_qcm,"_",tmpStr,sep=""))
    
    tmpDf <- merge(tmpDf, tmpDf2, by = "sub_name", all.x = TRUE )
    
}

allSubsDf <- merge(allSubsDf, tmpDf, by = "sub_name", all.x = TRUE )

################################################################################

tmpDat <- strsplit(as.character(allSubsDf$sub_name),'-ses-')
allSubsDf["sub_id"] <- rapply(tmpDat, function(x)x[1])
allSubsDf["sub_visit"] <- rapply(tmpDat, function(x)x[2])

################################################################################

bPathOut <- paste(getwd(),'/proc_data/', sep='')
dir.create(path = bPathOut)

write.csv(allSubsDf, file = paste(bPathOut,'/allSubsDataProc.csv', sep=''))


