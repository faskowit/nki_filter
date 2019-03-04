# load in the data

inFile <- paste(getwd(),'/proc_data/allSubsDataProc.csv', sep='')

allSubsDf <- read.table(inFile, sep = ',', header = TRUE, row.names = 1)

################################################################################
# which colums should we care about?
#
# T1:       possible: cjv_t1w, cnr_t1w, snr_total_t1w, snrd_total_t1w
# DWI       possible: outlier_prcnt, mov_tot, mov_abs, tsnr_t1, tsnr_t2 
# FMRI      possible: dvars_nstd.{acq}, dvars_vstd.{acq}, tsnr.{acq}, 
#                     fd_mean.{acq}, aqi.{acq}, numO.{acq} gcor.{acq} aor.{acq}
#                     fd_perc.{acq} 

PRCNTO <- 15

# FMRI numO thresholds
# 900 - 4
Othr645 <- floor(896 * (PRCNTO / 100))
# 404 - 4
Othr1400 <- floor(400 * (PRCNTO / 100))
# 120 - 4
Othr2500 <- floor(116 * (PRCNTO / 100))

################################################################################

# 645
tmp <- subset(allSubsDf, acq.645 == TRUE )
tmp2 <- subset(tmp, numO_acq.645 < Othr645, c(1:6, 
                                              grep("645",colnames(tmp)),
                                              grep("sub",colnames(tmp))) )
outlmat <- matrix(0, nrow = nrow(tmp2), ncol = 6)
outlmat[,1] <- outliers::scores(tmp2$dvars_nstd_acq.645, type = "iqr", lim = 1.5)
outlmat[,2] <- outliers::scores(tmp2$dvars_vstd_acq.645, type = "iqr", lim = 1.5)
outlmat[,3] <- outliers::scores(tmp2$tsnr_acq.645, type = "iqr") < -1.5
outlmat[,4] <- outliers::scores(tmp2$fd_mean_acq.645, type = "iqr", lim = 1.5)
outlmat[,5] <- outliers::scores(tmp2$aor_acq.645, type = "iqr", lim = 1.5)
outlmat[,6] <- outliers::scores(tmp2$aqi_acq.645, type = "iqr", lim = 1.5)
thrtmpBool <- rowSums(outlmat) < 3
filterDF.645 <- subset(tmp2 , thrtmpBool)
filterDF.645subs <- filterDF.645$sub_name

# 1400
tmp <- subset(allSubsDf, acq.1400 == TRUE )
tmp2 <- subset(tmp, numO_acq.1400 < Othr1400, c(1:6,
                                                grep("1400",colnames(tmp)),
                                                grep("sub",colnames(tmp))) )
outlmat <- matrix(0, nrow = nrow(tmp2), ncol = 6)
outlmat[,1] <- outliers::scores(tmp2$dvars_nstd_acq.1400, type = "iqr", lim = 1.5)
outlmat[,2] <- outliers::scores(tmp2$dvars_vstd_acq.1400, type = "iqr", lim = 1.5)
outlmat[,3] <- outliers::scores(tmp2$tsnr_acq.1400, type = "iqr") < -1.5
outlmat[,4] <- outliers::scores(tmp2$fd_mean_acq.1400, type = "iqr", lim = 1.5)
outlmat[,5] <- outliers::scores(tmp2$aor_acq.1400, type = "iqr", lim = 1.5)
outlmat[,6] <- outliers::scores(tmp2$aqi_acq.1400, type = "iqr", lim = 1.5)
thrtmpBool <- rowSums(outlmat) < 3
filterDF.1400 <- subset(tmp2 , thrtmpBool)
filterDF.1400subs <- filterDF.1400$sub_name

# 2500
tmp <- subset(allSubsDf, acq.2500 == TRUE )
tmp2 <- subset(tmp, numO_acq.2500 < Othr2500, c(1:6,
                                               grep("2500",colnames(tmp)),
                                               grep("sub",colnames(tmp))) )
outlmat <- matrix(0, nrow = nrow(tmp2), ncol = 6)
outlmat[,1] <- outliers::scores(tmp2$dvars_nstd_acq.2500, type = "iqr", lim = 1.5)
outlmat[,2] <- outliers::scores(tmp2$dvars_vstd_acq.2500, type = "iqr", lim = 1.5)
outlmat[,3] <- outliers::scores(tmp2$tsnr_acq.2500, type = "iqr") < -1.5
outlmat[,4] <- outliers::scores(tmp2$fd_mean_acq.2500, type = "iqr", lim = 1.5)
outlmat[,5] <- outliers::scores(tmp2$aor_acq.2500, type = "iqr", lim = 1.5)
outlmat[,6] <- outliers::scores(tmp2$aqi_acq.2500, type = "iqr", lim = 1.5)
thrtmpBool <- rowSums(outlmat) < 3
filterDF.2500 <- subset(tmp2 , thrtmpBool)
filterDF.2500subs <- filterDF.2500$sub_name

fmriFilterList <- list(filterDF.645subs, filterDF.1400subs, filterDF.2500subs)

################################################################################

bPathOut <- paste(getwd(),'/proc_data/', sep='')

write.table(filterDF.645subs, file = paste(bPathOut,'/acq645filtersubs.csv', sep =''), 
          col.names = NA, sep = ",")

write.table(filterDF.1400subs, file = paste(bPathOut,'/acq1400filtersubs.csv', sep =''), 
            col.names = NA, sep = ",")

write.table(filterDF.2500subs, file = paste(bPathOut,'/acq2500filtersubs.csv', sep =''), 
            col.names = NA, sep = ",")

################################################################################
# DWI

numOdwiPrcnt <- PRCNTO

tmp <- subset(allSubsDf, detertrack == TRUE )
tmp2 <- subset(tmp, outlier_prcnt < numOdwiPrcnt, c(1:6, 
                                              grep("outlier_prcnt",colnames(tmp)), 
                                              grep("mov",colnames(tmp)),
                                              grep("_t1$",colnames(tmp)),
                                              grep("_t2$",colnames(tmp)),
                                              grep("sub",colnames(tmp))) )
outlmat <- matrix(0, nrow = nrow(tmp2), ncol = 3)
outlmat[,1] <- outliers::scores(tmp2$tsnr_t2, type = "iqr") < -1.5
outlmat[,2] <- outliers::scores(tmp2$outlmean_t2, type = "iqr", lim = 1.5)
outlmat[,3] <- outliers::scores(tmp2$outlmax_t2, type = "iqr", lim = 1.5)
thrtmpBool <- rowSums(outlmat) < 2
filterDF.DWI <- subset(tmp2 , thrtmpBool)
filterDF.DWIsubs <- filterDF.DWI$sub_name

bPathOut <- paste(getwd(),'/proc_data/', sep='')

write.table(filterDF.DWIsubs, file = paste(bPathOut,'/DWIfiltersubs.csv', sep =''), 
            col.names = NA, sep = ",")

################################################################################
# T1

tmp2 <- allSubsDf[,c(1:6,grep("_t1w$",colnames(allSubsDf)),
                     grep("sub",colnames(tmp))) ]
outlmat <- matrix(0, nrow = nrow(tmp2), ncol = 6)
outlmat[,1] <- outliers::scores(tmp2$cjv_t1w, type = "iqr") < -1.5
outlmat[,2] <- outliers::scores(tmp2$cnr_t1w, type = "iqr") < -1.5
outlmat[,3] <- outliers::scores(tmp2$snr_total_t1w, type = "iqr") < -1.5
outlmat[,4] <- outliers::scores(tmp2$snrd_total_t1w, type = "iqr") < -1.5
outlmat[,5] <- outliers::scores(tmp2$fber_t1w, type = "iqr") < -1.5
outlmat[,6] <- outliers::scores(tmp2$efc_t1w, type = "iqr", lim = 1.5)
thrtmpBool <- rowSums(outlmat) < 3
filterDF.T1 <- subset(tmp2 , thrtmpBool)
filterDF.T1subs <- filterDF.T1$sub_name

bPathOut <- paste(getwd(),'/proc_data/', sep='')

write.table(filterDF.T1subs, file = paste(bPathOut,'/T1filtersubs.csv', sep =''), 
            col.names = NA, sep = ",")

