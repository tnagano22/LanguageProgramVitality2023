###############################################
# R procedures
# Tomonori Nagano <tnagano@lagcc.cuny.edu>
# https://github.com/tnagano22/LanguageProgramVitality2023
###############################################
# To analyze the Modern Language Association (MLA)'s Enrollment Survey
# clear the cache
rm(list = ls())

# load packages
library(ggplot2); library(gdata); library(ggthemes); library(openxlsx); library(plyr)

# moved to the directory
setwd("~/Desktop/xxx")

# change the default width
width.default <- getOption("width"); options(width=120)

# the add comma fonction
addComma<-function(x) {format(x, big.mark = ',', trim = TRUE, scientific = FALSE)}

# creating a notin function
`%notin%` <- Negate(`%in%`)

######################
# Analyzing Snyder (1993)
######################
# reading csv data
Snyder = read.csv(file="data/Snyder1993e_Table24.csv",header=TRUE,sep=",")
# The data between 1957 and 1962 are missing in Snyder (1993) and have been extrapolated from the figures in 1956 and 1962.
Snyder$X1957 <- c(1499,361,1266,59)
Snyder$X1958 <- c(1640,424,1319,67)
Snyder$X1959 <- c(1780,487,1373,76)
Snyder$X1960 <- c(1920,551,1427,85)
Snyder$X1961 <- c(2060,614,1481,94)
Snyder$X1962 <- c(2201,677,1534,102)
Snyder$Year <- as.factor(Snyder$Year)
colnames(Snyder) <- sub("^X(\\d+)", "\\1", colnames(Snyder), perl=TRUE)
Snyder <- melt(Snyder,id=c("Year"))
colnames(Snyder) <- c("Control","Year","Enrollment")
Snyder$Control <- factor(Snyder$Control, levels=c("Public 4-year","Public 2-year", "Private 4-year", "Private 2-year"))
# fixing the order of Year level
Snyder <- drop.levels(Snyder)

# plotting
p <- ggplot(Snyder, aes(Year, Enrollment, group = Control)) + 
	geom_point(aes(color=Control)) + 
	geom_line(aes(color=Control)) + 
	geom_text(data=subset(Snyder, as.numeric(as.character(Snyder$Year))%%5==2 & as.numeric(as.character(Snyder$Year)) > 1950), 
		aes(label=paste(addComma(Enrollment),"K",sep="")), 
		position=position_dodge2(width=0.1, preserve="single", padding=5), vjust=2, size=2.5)

# using ggplot theme and modifying axis labels
p + theme_hc()+ scale_colour_hc() +
	theme(axis.text.x = element_text(angle = 50, vjust = 0.8, hjust=1, size=8)) +
	scale_y_continuous(name = "Enrollment", limits = c(0, 6000),
					 breaks = c(0, 1500, 3000, 4500, 6000), position = 'left',
					 labels = c('0', '1.5M', '3.0M', '4.5M', '6.0M')) +
	ggtitle("Enrollment in Higher Education Institutions by Control and Type of Institution",
		subtitle="From Table 24 of Snyder (1993)") +
	theme(plot.margin = unit(c(1,1,1,1), "cm"))

ggsave("ROutput/LanguageProgramVitality2021FigEnrollment.pdf", width = 10, height = 6)
write.csv(Snyder, file= "ROutput/Snyder1993e_Table24_summary.csv", row.names = TRUE)



######################
# Analyzing Cohen et al. (2013)
######################
# reading csv data
Cohen = read.csv(file="data/Cohen2013_Table1_1.csv",header=TRUE,sep=",")
colnames(Cohen) <- c("Year","Public 2-year","Private 2-year")
Cohen$Year <- as.factor(Cohen$Year)
Cohen <- melt(Cohen,id=c("Year"))
colnames(Cohen) <- c("Year","Control","Number")

# plotting
p <- ggplot(Cohen, aes(Year, Number, group = Control)) + 
	geom_point(aes(color=Control)) + 
	geom_line(aes(color=Control)) + 
	geom_text(aes(label=paste(addComma(Number),sep="")), 
		position=position_dodge2(width=0.1, preserve="single", padding=5), vjust=2, size=2.5)

# using ggplot theme and modifying axis labels
p + theme_hc()+ scale_colour_hc() +
	theme(axis.text.x = element_text(angle = 40, vjust = 0.8, hjust=1, size=8)) +
	scale_y_continuous(name = "Number", limits = c(0, 1200),
					 breaks = c(0, 300, 600, 900, 1200), position = 'left',
					 labels = c('0', '300', '600', '900', '1,200')) +
	ggtitle("Number of Community Colleges by Year and Control",
		subtitle="Data from Table 1.1. of Cohen et al., (2019) and Table 317.10 of Digest of Education Statsitics 2021") +
	theme(plot.margin = unit(c(1,1,1,1), "cm")) +
	theme(axis.title.x = element_text(margin=margin(t=10)))

ggsave("ROutput/LanguageProgramVitality2021FigNum2Year.pdf", width = 10, height = 6)
write.csv(Cohen, file= "ROutput/Cohen2013_Table1_1_summary.csv", row.names = TRUE)
  


####################
# MLA Enrollment Survey
####################
# importing data
MLA <- read.csv("data/MLA_Historical_enrollments_1958-2016_(3-13-19).csv", sep = ",")
# filling empty "UNIV_NAME_HISTORY"
MLA[MLA$UNIV_NAME_HISTORY=="",c("UNIV_NAME_HISTORY")] <- MLA[MLA$UNIV_NAME_HISTORY=="",c("UNIV")]
MLA$SRVY_YEAR <- as.factor(MLA$SRVY_YEAR)
MLA$TERM <- as.factor(MLA$TERM)
MLA$YR.TERM <- as.factor(MLA$YR.TERM)
MLA$UNIV <- as.factor(MLA$UNIV)
MLA$UNIV_NAME_HISTORY <- as.factor(MLA$UNIV_NAME_HISTORY)
MLA$CAMPUS <- as.factor(MLA$CAMPUS)
MLA$STATE <- as.factor(MLA$STATE)
MLA$STATE_ID <- as.factor(MLA$STATE_ID)
MLA$MLA.ICLEVEL <- as.factor(MLA$MLA.ICLEVEL)
levels(MLA$MLA.ICLEVEL) = c("4 year","2 year")
MLA$LANG_CODE <- as.factor(MLA$LANG_CODE)
MLA$CITY <- as.factor(MLA$CITY)
MLA$LANGUAGE <- as.factor(MLA$LANGUAGE)
MLA$LANG_REGION <- as.factor(MLA$LANG_REGION)
MLA$OTHEROutput_LANG <- as.factor(MLA$OTHEROutput_LANG)
MLA$GEOGRAPHY_CODE <- as.factor(MLA$GEOGRAPHY_CODE)
MLA$N_RESP <- as.factor(MLA$N_RESP)
MLA$ZERO_ERL <- as.factor(MLA$ZERO_ERL)

# Between 1963 - 1972 many institutions did not report "UNDERGRAD_TOTAL" and "GRAD_TOTAL". We beed to use "ALL_LEVEL_TOTAL" instead
MLA[is.na(MLA$UNDERGRAD_TOTAL),c("UNDERGRAD_TOTAL")] <- MLA[is.na(MLA$UNDERGRAD_TOTAL),c("ALL_LEVEL_TOTAL")]

summary(MLA)

# extracting the aggregated enrollment data by type (2 vs. 4-year)
MLA.type <- as.data.frame(xtabs(UNDERGRAD_TOTAL ~ YR.TERM + MLA.ICLEVEL, data = MLA))
# removing years with anomal data
MLA.type <- MLA.type[MLA.type$YR.TERM %notin% c("1958 Fall", "1961 Fall", "1969 Summer","1971 Summer", "2016 Summer"),]
colnames(MLA.type) <- c("Year","Type","WLEnrollment")

# plotting
p <- ggplot(MLA.type, aes(Year, WLEnrollment, group = Type)) + 
	geom_point(aes(color=Type)) + 
	geom_line(aes(color=Type)) + 
	geom_text(aes(label=paste(addComma(WLEnrollment),sep="")), 
		position=position_dodge2(width=0.1, preserve="single", padding=5), vjust=2, size=2.5)

# using ggplot theme and modifying axis labels
p + theme_hc()+ scale_colour_hc() +
	theme(axis.text.x = element_text(angle = 40, vjust = 0.8, hjust=1, size=8)) +
	scale_y_continuous(name = "WLEnrollment", limits = c(0, 1250000),
					 breaks = c(0, 250000, 500000, 750000, 1000000, 1250000), position = 'left',
					 labels = c('0', '250K', '500K', '750K', '1M', "1.25M")) +
	ggtitle("Undergraduate World Language Enrollment by Year and Institutional Type",
		subtitle="Data from MLA Language Enrollment Database, 1958–2016 (https://apps.mla.org/flsurvey_search)") +
	theme(plot.margin = unit(c(1,1,1,1), "cm")) + 
	theme(axis.title.x = element_text(margin=margin(t=10)))

ggsave("ROutput/LanguageProgramVitality2021FigMLEnrollment.pdf", width = 10, height = 6)
write.csv(MLA.type, file= "ROutput/MLA_Historical_enrollments_1958-2016_(3-13-19)_summary.csv", row.names = TRUE)


######################
# Analyzing MLA enrollment data with the DES Table 303-70
######################
# reading CSV data
# the files should be converted to csv(windows)
DES = read.csv(file="data/USEDDigestofEdStat_tabn303_70.csv",header=TRUE,sep=",")
DES <- melt(DES,id=c("Year"))
colnames(DES) <- c("Year","Classification","Enrollment")
DES[grep(".*4.year.*",as.character(DES$Classification)),"Type"] <- "4 year"
DES[grep(".*2.year.*",as.character(DES$Classification)),"Type"] <- "2 year"
DES[grep(".*private.for.profit",as.character(DES$Classification)),"Control"] <- "Private for-profit"
DES[grep(".*private.nonprofit",as.character(DES$Classification)),"Control"] <- "Private"
DES[grep(".*public",as.character(DES$Classification)),"Control"] <- "Public"
DES$Type <- as.factor(DES$Type)
DES$Year <- as.factor(DES$Year)
DES <- DES[!DES$Control=="Private for-profit",]
DES <- drop.levels(DES[,-which(names(DES) %in% c("Classification"))],reorder=FALSE)
levels(MLA.type$Year) <- gsub('(\\d+) (\\w+)','\\1',levels(MLA.type$Year))

DES2 <- aggregate(Enrollment ~ Type + Year, DES, sum)

thisData.e1 <- merge(MLA.type, DES2, by.x = c("Year","Type"), by.y = c("Year","Type"), all.x = TRUE, all.y = TRUE)
thisData.e2 <- cast(thisData.e1, Year ~ Type)
# matches only between 1996 to 2016
write.csv(thisData.e2, file= "ROutput/EnrollmentComparison1.csv", row.names = TRUE)


######################
# Analyzing MLA enrollment data with other data
######################
summary(Cohen)

# renaming the levels
levels(Cohen$Year) <- gsub('(\\d+)-(\\d+)','\\1',levels(Cohen$Year))
levels(MLA.type$Year) <- gsub('(\\d+) (Fall|Summer)','\\1',levels(MLA.type$Year))
colnames(MLA.type) <- c("Year","Type","ML Enrollment")

Snyder[grep(".*4-year",as.character(Snyder$Control)),"Type"] <- "4 year"
Snyder[grep(".*2-year",as.character(Snyder$Control)),"Type"] <- "2 year"
Snyder$Type <- as.factor(Snyder$Type)
levels(Snyder$Control) <- gsub('(\\w+) (\\d-year)','\\1',levels(Snyder$Control))
Snyder2 <- aggregate(Enrollment ~ Type + Year, Snyder, sum)

thisData.e3 <- merge(MLA.type, Snyder2, by.x = c("Year","Type"), by.y = c("Year","Type"), all.x = TRUE, all.y = TRUE)
# matches between only 1963 and 1990
thisData.e4 <- cast(thisData.e3, Year ~ Type)

write.csv(thisData.e4, file= "ROutput/EnrollmentComparison2.csv", row.names = TRUE)


######################
# Analyzing MLA, Snyder, and DES data
# MLA 1960 was removed since there was no undergraduate enrollment data
# Snyder's 4-year include graduate enrollment
# ML Enrollment	(MLA)
# Undergraduate Enrollment (Snyder (between 1963 - 1990), DES (1995-2016))
######################
# reading CSV data
# the files should be converted to csv(windows)
thisData = read.csv(file="data/MLA_DES2021EnrollmentCombinedData.csv",header=TRUE,sep=",")
colnames(thisData) <- c("Year","Type","MLEnrollment","UndergraduateEnrollment")
thisData$Year <- as.factor(thisData$Year)
thisData$Type <- as.factor(thisData$Type)

# computing growth rates
thisData <- ddply(thisData, "Type", transform, WL_Growth=c(NA,exp(diff(log(MLEnrollment)))-1))
thisData <- ddply(thisData, "Type", transform, UG_Growth=c(NA,exp(diff(log(UndergraduateEnrollment)))-1))
thisData <- thisData[,names(thisData) %notin% c("MLEnrollment","UndergraduateEnrollment")]
thisData$WL_Growth <- thisData$WL_Growth*100
thisData$UG_Growth <- thisData$UG_Growth*100

thisData2 <- melt(thisData,id=c("Year","Type"))
colnames(thisData2) <- c("Year","Type","EnrollmentType","Growth")

# plotting
p <- ggplot(subset(thisData2,Type=="2 year" & Year %notin% c("1970 Fall")), aes(Year, Growth, group = EnrollmentType))+
 	geom_point(aes(color=EnrollmentType)) + 
 	geom_line(aes(color=EnrollmentType)) + 
 	geom_text(aes(label=paste(format(Growth,digits = 1),"%",sep="")), 
 		position=position_dodge2(width=0.1, preserve="single", padding=5), vjust=2, size=2.5)

# using ggplot theme and modifying axis labels
p + theme_hc()+ scale_colour_hc() +
	theme(axis.text.x = element_text(angle = 40, vjust = 0.8, hjust=1, size=8)) +
	scale_y_continuous(name = "Growth Rates", limits = c(-30, 60),
					 breaks = c(-20, 0, 20, 40), position = 'left',
					 labels = c('-20%', '0%', '20%', '40%')) +
	ggtitle("Growth Rates of Undergraduate Enrollment and World Language Enrollment at Community Colleges",
		subtitle="MLA Enrollment Survey (WL enrollment) and Digest of Education Statsitics 2021 (undergraduate enrollment)") +
	theme(plot.margin = unit(c(1,1,1,1), "cm")) +
	theme(axis.title.x = element_text(margin=margin(t=10)))

ggsave("ROutput/LanguageProgramVitality2021FigGrowth2Year.pdf", width = 10, height = 6)



# plotting
p <- ggplot(subset(thisData2,Type=="4 year" & Year %notin% c("1970 Fall")), aes(Year, Growth, group = EnrollmentType))+
 	geom_point(aes(color=EnrollmentType)) + 
 	geom_line(aes(color=EnrollmentType)) + 
 	geom_text(aes(label=paste(format(Growth,digits = 1),"%",sep="")), 
 		position=position_dodge2(width=0.1, preserve="single", padding=5), vjust=2, size=2.5)

# using ggplot theme and modifying axis labels
p + theme_hc()+ scale_colour_hc() +
	theme(axis.text.x = element_text(angle = 40, vjust = 0.8, hjust=1, size=8)) +
	scale_y_continuous(name = "Number", limits = c(-30, 60),
					 breaks = c(-20, 0, 20, 40), position = 'left',
					 labels = c('-20%', '0%', '20%', '40%')) +
	ggtitle("Growth Rates of Undergraduate Enrollment and World Language Enrollment at 4-year Institutions",
		subtitle="MLA Enrollment Survey (WL enrollment) and Digest of Education Statsitics 2021 (undergraduate enrollment)") +
	theme(plot.margin = unit(c(1,1,1,1), "cm")) +
	theme(axis.title.x = element_text(margin=margin(t=10)))

ggsave("ROutput/LanguageProgramVitality2021FigGrowth4Year.pdf", width = 10, height = 6)  

