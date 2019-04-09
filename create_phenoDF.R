# make a mapping from COINS <-> NITRC/AWS
#

# first load up the data
inFile <- paste(getwd(),'/proc_data/allSubsDataProc.csv', sep='')
allSubsDf <- read.table(inFile, sep = ',', header = TRUE, row.names = 1)

wantCols <- c("sub_name", "sub_id", "sub_visit")
subNameDf <- allSubsDf[wantCols] 

# can probably just hard code this 
coinsVisit <- as.character(subNameDf$sub_visit)
#unique(coinsVisit)
#[1] DS2   CLG4  CLG4R CLG5  CLG2R CLG2  ALGA  DSA   CLGA 
coinsVisit[coinsVisit=="DSA"] <- "VA"
coinsVisit[coinsVisit=="CLGA"] <- "VA"
coinsVisit[coinsVisit=="ALGA"] <- "VA_ALG"
coinsVisit[coinsVisit=="DS2"] <- "V2"
coinsVisit[coinsVisit=="CLG2"] <- "V2"
coinsVisit[coinsVisit=="CLG2R"] <- "V2REP"
coinsVisit[coinsVisit=="CLG4"] <- "V4"
coinsVisit[coinsVisit=="CLG4R"] <- "NaN"
coinsVisit[coinsVisit=="CLG5"] <- "V5"

# make a new phenotype Df
phenoDf <- as.data.frame(subNameDf)
phenoDf['coins_id'] <- paste(as.character(subNameDf$sub_id), coinsVisit,sep = "-")
phenoDf[grep("NaN",phenoDf$coins_id),'coins_id'] <- NaN

################################################################################
# CALCULATED AGE
inFile <- paste(getwd(),'/data/assessment_data/8100_Age_20170607.csv', sep='')
loadDF <- read.csv(inFile)
loadDF['coins_id'] <- paste(loadDF$X, loadDF$X.2, sep = "-")
# make the DF to merge
ageDF <- loadDF[c("Calculated.Age","coins_id")]

# and merge base on coinsID
phenoDf <- merge(phenoDf,ageDF,by = "coins_id", all.x = TRUE )
rm(ageDF)

################################################################################
# DEMO
inFile <- paste(getwd(),'/data/assessment_data/8100_Demos_20170608.csv', sep='')
loadDF <- read.csv(inFile)
# need to do this for when testing was two days
levels(loadDF$X.2)[levels(loadDF$X.2)=="V1"] <- "V2"
loadDF['coins_id'] <- paste(loadDF$X, loadDF$X.2, sep = "-")
# make the DF to merge
demoDf <- loadDF[c("What.is.your.current.age.in.years.",
                   "What.is.your.sex.","What.is.your.native.language.",
                   "coins_id")]
colnames(demoDf) <- c("demo_age", "demo_sex","demo_native_lang",
                     "coins_id")

# and merge base on coinsID
phenoDf <- merge(phenoDf,demoDf,by = "coins_id", all.x = TRUE )
rm(demoDf)

################################################################################
# DIAGNOSTIC SUMMARY
inFile <- paste(getwd(),'/data/assessment_data/8100_ConsensusDiagnosticSummary_20170607.csv', sep='')
loadDF <- read.csv(inFile)
# need to do this for when testing was two days
levels(loadDF$X.2)[levels(loadDF$X.2)=="V1"] <- "V2"
loadDF['coins_id'] <- paste(loadDF$X, loadDF$X.2, sep = "-")
loadDF['diag'] <- as.numeric(loadDF$Is.there.a.consensus.diagnosis. == 1) 
# make the DF to merge
diagDf <- loadDF[c("diag","coins_id")]

# and merge base on coinsID
phenoDf <- merge(phenoDf,diagDf,by = "coins_id", all.x = TRUE )
rm(diagDf)

################################################################################
# WASI
inFile <- paste(getwd(),'/data/assessment_data/8100_WASI-II_20170606.csv', sep='')
loadDF <- read.csv(inFile)
# need to do this for when testing was two days
levels(loadDF$X.2)[levels(loadDF$X.2)=="V1"] <- "V2"
loadDF['coins_id'] <- paste(loadDF$X, loadDF$X.2, sep = "-")
# make the DF to merge
wasiDf <- loadDF[c("Verbal.Comp..Percentile.Rank",
                   "Perc..Rsng..Percentil.Rank",
                   "Full.Scale...4.Percentile.Rank",
                   "coins_id")]
colnames(wasiDf) <- c("wasi_vci_prcntile",
                      "wasi_pri_prcntile",
                      "wasi_fsiq_prcntile",
                      "coins_id")

# and merge base on coinsID
phenoDf <- merge(phenoDf,wasiDf,by = "coins_id", all.x = TRUE )
rm(wasiDf)


