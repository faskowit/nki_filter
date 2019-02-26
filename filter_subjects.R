# load in the data

inFile <- paste(getwd(),'/proc_data/allSubsDataProc.csv', sep='')

allSubsDf <- read.table(inFile, sep = ',', header = TRUE, row.names = 1)

################################################################################
# which colums should we care about?
#
# T1:       possible: cjv_t1w, cnr_t1w, snr_total_t1w, snrd_total_t1w
# DWI       possible: outlier_prcnt, mov_tot, mov_abs, tsnr_t1, tsnr_t2 
# FMRI      possible: dvars_nstd.{acq}, dvars_vstd.{acq}, tsnr.{acq}, 
#                     fd_mean.{acq}, aqi.{acq}, numO.{acq}
#
# zscore?
#
# T1:       a
# DWI:      a
# FMRI:     a

################################################################################
# initial filter all subjects, regardless of visual qc check
#
# T1:
# DWI: 
# fmri: 
 


# 645

# 

tmp <- subset(allSubsDf, acq.645 == TRUE)




