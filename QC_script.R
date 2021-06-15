# Connect recruitment QC rules for Sanford
    # PURPOSE: TO CHECK FOR INCONSISTENCIES IN DATA FROM CONNECT SITE(S)
    # VERSION: 1.0
    # LAST UPDATED: 04_05_2021
    # AUTHOR: LORENA SANDOVAL 
    # EMAIL: SANDOVALL2@NIH.GOV
               
# install.packages("stringr")
# install.packages("dplyr")
library(stringr)
library(dplyr)

# set working directory
setwd("C:/Users/sandovall2/Box/Confluence Project/Confluence Data Platform/R_code_Lorena/Connect Code/BQ_TABLES/pull_site_data")
connectData = read.csv("HP_Sanford_Kaiser_recruitment_04282021.csv")
# function to exclude rows with specified values
"%!in%" <- function(x,y)!("%in%"(x,y))
# function to check for numeric values
testInteger <- function(x){test <- all.equal(x, as.integer(x), check.attributes = FALSE)
if(test == TRUE){ return(TRUE) } else { return(FALSE) }}

# make qc dataframe
df = data.frame(matrix(, nrow=1066, ncol=4))
names(df) = c("QC checks","permissible vlaues in CID1", "invalid values in CID1", "invalid values in CID2")

######## QC d_512820379
# valid value check
d_512820379= c(180583933, 486306141, 854703046)
QCcheck1 =which(connectData$"d_512820379"%!in%d_512820379)
d_512820379_invalid = addNA(connectData$"d_512820379")[QCcheck1]
df[3,1]<-paste0("d_512820379_invalid")
df[3,2]<-paste0("180583933, 486306141, 854703046")
df[3,3]<-paste0(d_512820379_invalid, collapse=", ")
######## QC d_471593703
# valid dateTime check
d_471593703 = connectData$"d_471593703"
d_471593703_dateTime_invalid = which(!grepl("[0-9]?[1-9]-[0-9]?[1-9]-[1-2][0,9][0-9]?[1-9] [0-9]?[1-9]:[0-9]?[1-9]:[0-9]?[1-9]", d_471593703))
d_471593703_dateTime2_invalid = levels(addNA(connectData$"d_471593703"[d_471593703_dateTime_invalid]))

            df[4,1]<-paste0("d_471593703_dateTime_invalid")
df[4,2]<-paste0("MMDDYYYY 00:00:00")
df[4,3]<-paste0(d_471593703_dateTime2_invalid, collapse=", ")
######## QC d_934298480
# valid value check
d_934298480= c(124276120, 450985724, 363147933, 636706443, 771230670)
QCcheck1 =which(connectData$"d_934298480"%!in%d_934298480)
d_934298480_invalid = addNA(connectData$"d_934298480")[QCcheck1]
df[5,1]<-paste0("d_934298480_invalid")
df[5,2]<-paste0("124276120, 450985724, 363147933, 636706443, 771230670")
df[5,3]<-paste0(d_934298480_invalid, collapse=", ")
######## QC d_849518448
# valid value check
d_849518448= c(768826601, 181769837, 178420302)
QCcheck1 =which(connectData$"d_849518448"%!in%d_849518448)
d_849518448_invalid = addNA(connectData$"d_849518448")[QCcheck1]
df[6,1]<-paste0("d_849518448_invalid")
df[6,2]<-paste0("768826601, 181769837, 178420302")
df[6,3]<-paste0(d_849518448_invalid, collapse=", ")
######## QC d_119643471
# cross valid value check

            d_119643471_a = c(NA)

            d_119643471_b = c(232334767, 211228524, 308427446, 635279662, 432722256, 232663805, 785578696, 200929978, 490725843, 965998904, 986445321, 746038746, 178420302)

            mylist_a1 =  paste0(rep("connectData$d_827220437 == "), c(657167265), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_827220437 != "), c(657167265), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_119643471"[aa]%!in%d_119643471_a)

            d_119643471_invalid_cross_a = addNA(connectData$"d_119643471"[QCcheck1])

            QCcheck2 =which(connectData$"d_119643471"[bb]%!in%d_119643471_b)

            d_119643471_invalid_cross_b = addNA(connectData$"d_119643471"[QCcheck2])

            df[7,1]<-paste0("d_119643471_d_827220437_crossinvalid_values")

            df[7,2]<-paste0(d_119643471_b, collapse=", ")

            df[7,3]<-paste0(d_119643471_invalid_cross_a, collapse=", ")

            df[7,4]<-paste0(d_119643471_invalid_cross_b, collapse=", ")

######## QC d_684926335
# cross valid value check

            d_684926335_a = c(NA)

            d_684926335_b = c(232334767, 635279662, 401335456, 178420302)

            mylist_a1 =  paste0(rep("connectData$d_827220438 == "), c(548392715), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_827220438 != "), c(548392715), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_684926335"[aa]%!in%d_684926335_a)

            d_684926335_invalid_cross_a = addNA(connectData$"d_684926335"[QCcheck1])

            QCcheck2 =which(connectData$"d_684926335"[bb]%!in%d_684926335_b)

            d_684926335_invalid_cross_b = addNA(connectData$"d_684926335"[QCcheck2])

            df[8,1]<-paste0("d_684926335_d_827220438_crossinvalid_values")

            df[8,2]<-paste0(d_684926335_b, collapse=", ")

            df[8,3]<-paste0(d_684926335_invalid_cross_a, collapse=", ")

            df[8,4]<-paste0(d_684926335_invalid_cross_b, collapse=", ")

######## QC d_527823810
# cross valid value check

            d_527823810_a = c(NA)

            d_527823810_b = c(628733063, 319570637, 178420302)

            mylist_a1 =  paste0(rep("connectData$d_827220439 == "), c(548392715), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_827220439 != "), c(548392715), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_527823810"[aa]%!in%d_527823810_a)

            d_527823810_invalid_cross_a = addNA(connectData$"d_527823810"[QCcheck1])

            QCcheck2 =which(connectData$"d_527823810"[bb]%!in%d_527823810_b)

            d_527823810_invalid_cross_b = addNA(connectData$"d_527823810"[QCcheck2])

            df[9,1]<-paste0("d_527823810_d_827220439_crossinvalid_values")

            df[9,2]<-paste0(d_527823810_b, collapse=", ")

            df[9,3]<-paste0(d_527823810_invalid_cross_a, collapse=", ")

            df[9,4]<-paste0(d_527823810_invalid_cross_b, collapse=", ")

######## QC d_412947828
# cross valid value check

            d_412947828_a = c(NA)

            d_412947828_b = c(473807808, 269950058, 998778678, 871169055, 211847969, 613506991, 646444521, 480568177, 601070694, 724873055, 819077778, 228278549







)

            mylist_a1 =  paste0(rep("connectData$d_827220440 == "), c(548392715), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_827220440 != "), c(548392715), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_412947828"[aa]%!in%d_412947828_a)

            d_412947828_invalid_cross_a = addNA(connectData$"d_412947828"[QCcheck1])

            QCcheck2 =which(connectData$"d_412947828"[bb]%!in%d_412947828_b)

            d_412947828_invalid_cross_b = addNA(connectData$"d_412947828"[QCcheck2])

            df[10,1]<-paste0("d_412947828_d_827220440_crossinvalid_values")

            df[10,2]<-paste0(d_412947828_b, collapse=", ")

            df[10,3]<-paste0(d_412947828_invalid_cross_a, collapse=", ")

            df[10,4]<-paste0(d_412947828_invalid_cross_b, collapse=", ")

######## QC d_538553381
# cross valid value check

            d_538553381_a = c(NA)

            d_538553381_b = c(628733063, 319570637, 986445321, 746038746, 178420302




)

            mylist_a1 =  paste0(rep("connectData$d_827220441 == "), c(657167265), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_827220441 != "), c(657167265), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_538553381"[aa]%!in%d_538553381_a)

            d_538553381_invalid_cross_a = addNA(connectData$"d_538553381"[QCcheck1])

            QCcheck2 =which(connectData$"d_538553381"[bb]%!in%d_538553381_b)

            d_538553381_invalid_cross_b = addNA(connectData$"d_538553381"[QCcheck2])

            df[11,1]<-paste0("d_538553381_d_827220441_crossinvalid_values")

            df[11,2]<-paste0(d_538553381_b, collapse=", ")

            df[11,3]<-paste0(d_538553381_invalid_cross_a, collapse=", ")

            df[11,4]<-paste0(d_538553381_invalid_cross_b, collapse=", ")

######## QC d_706256705
# valid value check
d_706256705= c(536341288, 654207589, 830573274, 178420302, NA)
QCcheck1 =which(connectData$"d_706256705"%!in%d_706256705)
d_706256705_invalid = addNA(connectData$"d_706256705")[QCcheck1]
df[12,1]<-paste0("d_706256705_invalid")
df[12,2]<-paste0("536341288, 654207589, 830573274, 178420302, NA")
df[12,3]<-paste0(d_706256705_invalid, collapse=", ")
######## QC d_678756255
# cross valid value check

            d_678756255_a = c(NA)

            d_678756255_b = c(536341288, 654207589, 395528052, 181769837, 178420302)

            mylist_a1 =  paste0(rep("connectData$d_827220437 == "), c(125001209, 327912200, 300267574, 452412599), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_827220437 != "), c(125001209, 327912200, 300267574, 452412599), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_678756255"[aa]%!in%d_678756255_a)

            d_678756255_invalid_cross_a = addNA(connectData$"d_678756255"[QCcheck1])

            QCcheck2 =which(connectData$"d_678756255"[bb]%!in%d_678756255_b)

            d_678756255_invalid_cross_b = addNA(connectData$"d_678756255"[QCcheck2])

            df[13,1]<-paste0("d_678756255_d_827220437_crossinvalid_values")

            df[13,2]<-paste0(d_678756255_b, collapse=", ")

            df[13,3]<-paste0(d_678756255_invalid_cross_a, collapse=", ")

            df[13,4]<-paste0(d_678756255_invalid_cross_b, collapse=", ")

######## QC d_435027713
# cross valid value check

            d_435027713_a = c(NA)

            d_435027713_b = c(536341288, 654207589178420302
)

            mylist_a1 =  paste0(rep("connectData$d_827220437 == "), c(657167265), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_827220437 != "), c(657167265), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_435027713"[aa]%!in%d_435027713_a)

            d_435027713_invalid_cross_a = addNA(connectData$"d_435027713"[QCcheck1])

            QCcheck2 =which(connectData$"d_435027713"[bb]%!in%d_435027713_b)

            d_435027713_invalid_cross_b = addNA(connectData$"d_435027713"[QCcheck2])

            df[14,1]<-paste0("d_435027713_d_827220437_crossinvalid_values")

            df[14,2]<-paste0(d_435027713_b, collapse=", ")

            df[14,3]<-paste0(d_435027713_invalid_cross_a, collapse=", ")

            df[14,4]<-paste0(d_435027713_invalid_cross_b, collapse=", ")

######## QC d_749475364
# valid character length check
valid_length= 
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_749475364"))
variable =connectData$"d_749475364"
            
list_lengths = unique(sapply(variable,nchar))
            
d_749475364_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[15,1]<-paste0("d_749475364_invalid_char_length")
            
df[15,2]<-paste0("invalid length(s) found:",d_749475364_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_477091792
# valid value check
d_477091792= c(939572698, 512786135, 582670006 
)
QCcheck1 =which(connectData$"d_477091792"%!in%d_477091792)
d_477091792_invalid = addNA(connectData$"d_477091792")[QCcheck1]
df[16,1]<-paste0("d_477091792_invalid")
df[16,2]<-paste0("939572698, 512786135, 582670006 
")
df[16,3]<-paste0(d_477091792_invalid, collapse=", ")
######## QC d_667474224
# valid value check
d_667474224= c(926338735, 348281054, 324692899, 351257378, 647148178, 834544960, 682916147, 153365143, 663706936, 181769837)
QCcheck1 =which(connectData$"d_667474224"%!in%d_667474224)
d_667474224_invalid = addNA(connectData$"d_667474224")[QCcheck1]
df[17,1]<-paste0("d_667474224_invalid")
df[17,2]<-paste0("926338735, 348281054, 324692899, 351257378, 647148178, 834544960, 682916147, 153365143, 663706936, 181769837")
df[17,3]<-paste0(d_667474224_invalid, collapse=", ")
######## QC d_481139103
# valid value check
d_481139103= c(910533468, 456806539)
QCcheck1 =which(connectData$"d_481139103"%!in%d_481139103)
d_481139103_invalid = addNA(connectData$"d_481139103")[QCcheck1]
df[18,1]<-paste0("d_481139103_invalid")
df[18,2]<-paste0("910533468, 456806539")
df[18,3]<-paste0(d_481139103_invalid, collapse=", ")
######## QC d_158291096
# valid value check
d_158291096= c(104430631, 353358909, NA)
QCcheck1 =which(connectData$"d_158291096"%!in%d_158291096)
d_158291096_invalid = addNA(connectData$"d_158291096")[QCcheck1]
df[19,1]<-paste0("d_158291096_invalid")
df[19,2]<-paste0("104430631, 353358909, NA")
df[19,3]<-paste0(d_158291096_invalid, collapse=", ")
######## QC d_196038514
# cross valid value check

            d_196038514_a = c(NA)

            d_196038514_b = c(353358909, 104430631, NA)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_196038514"[aa]%!in%d_196038514_a)

            d_196038514_invalid_cross_a = addNA(connectData$"d_196038514"[QCcheck1])

            QCcheck2 =which(connectData$"d_196038514"[bb]%!in%d_196038514_b)

            d_196038514_invalid_cross_b = addNA(connectData$"d_196038514"[QCcheck2])

            df[20,1]<-paste0("d_196038514_d_158291096_crossinvalid_values")

            df[20,2]<-paste0(d_196038514_b, collapse=", ")

            df[20,3]<-paste0(d_196038514_invalid_cross_a, collapse=", ")

            df[20,4]<-paste0(d_196038514_invalid_cross_b, collapse=", ")

######## QC d_873405723
# cross valid value check

            d_873405723_a = c(104430631)

            d_873405723_b = c(353358909, 104430631)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_873405723"[aa]%!in%d_873405723_a)

            d_873405723_invalid_cross_a = addNA(connectData$"d_873405723"[QCcheck1])

            QCcheck2 =which(connectData$"d_873405723"[bb]%!in%d_873405723_b)

            d_873405723_invalid_cross_b = addNA(connectData$"d_873405723"[QCcheck2])

            df[21,1]<-paste0("d_873405723_d_158291096_crossinvalid_values")

            df[21,2]<-paste0(d_873405723_b, collapse=", ")

            df[21,3]<-paste0(d_873405723_invalid_cross_a, collapse=", ")

            df[21,4]<-paste0(d_873405723_invalid_cross_b, collapse=", ")

######## QC d_517101990
# cross valid value check

            d_517101990_a = c(104430631)

            d_517101990_b = c(353358909, 104430631, NA)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_517101990"[aa]%!in%d_517101990_a)

            d_517101990_invalid_cross_a = addNA(connectData$"d_517101990"[QCcheck1])

            QCcheck2 =which(connectData$"d_517101990"[bb]%!in%d_517101990_b)

            d_517101990_invalid_cross_b = addNA(connectData$"d_517101990"[QCcheck2])

            df[22,1]<-paste0("d_517101990_d_158291096_crossinvalid_values")

            df[22,2]<-paste0(d_517101990_b, collapse=", ")

            df[22,3]<-paste0(d_517101990_invalid_cross_a, collapse=", ")

            df[22,4]<-paste0(d_517101990_invalid_cross_b, collapse=", ")

######## QC d_347614743
# cross valid value check

            d_347614743_a = c(104430631)

            d_347614743_b = c(353358909, 104430631)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_347614743"[aa]%!in%d_347614743_a)

            d_347614743_invalid_cross_a = addNA(connectData$"d_347614743"[QCcheck1])

            QCcheck2 =which(connectData$"d_347614743"[bb]%!in%d_347614743_b)

            d_347614743_invalid_cross_b = addNA(connectData$"d_347614743"[QCcheck2])

            df[23,1]<-paste0("d_347614743_d_158291096_crossinvalid_values")

            df[23,2]<-paste0(d_347614743_b, collapse=", ")

            df[23,3]<-paste0(d_347614743_invalid_cross_a, collapse=", ")

            df[23,4]<-paste0(d_347614743_invalid_cross_b, collapse=", ")

######## QC d_535928798
# cross valid value check

            d_535928798_a = c(104430631)

            d_535928798_b = c(353358909, 104430631, NA)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_535928798"[aa]%!in%d_535928798_a)

            d_535928798_invalid_cross_a = addNA(connectData$"d_535928798"[QCcheck1])

            QCcheck2 =which(connectData$"d_535928798"[bb]%!in%d_535928798_b)

            d_535928798_invalid_cross_b = addNA(connectData$"d_535928798"[QCcheck2])

            df[24,1]<-paste0("d_535928798_d_158291096_crossinvalid_values")

            df[24,2]<-paste0(d_535928798_b, collapse=", ")

            df[24,3]<-paste0(d_535928798_invalid_cross_a, collapse=", ")

            df[24,4]<-paste0(d_535928798_invalid_cross_b, collapse=", ")

######## QC d_897366187
# cross valid value check

            d_897366187_a = c(104430631)

            d_897366187_b = c(353358909, 104430631)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_897366187"[aa]%!in%d_897366187_a)

            d_897366187_invalid_cross_a = addNA(connectData$"d_897366187"[QCcheck1])

            QCcheck2 =which(connectData$"d_897366187"[bb]%!in%d_897366187_b)

            d_897366187_invalid_cross_b = addNA(connectData$"d_897366187"[QCcheck2])

            df[25,1]<-paste0("d_897366187_d_158291096_crossinvalid_values")

            df[25,2]<-paste0(d_897366187_b, collapse=", ")

            df[25,3]<-paste0(d_897366187_invalid_cross_a, collapse=", ")

            df[25,4]<-paste0(d_897366187_invalid_cross_b, collapse=", ")

######## QC d_415693436
# valid character length check
valid_length= 800
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_415693436"))
variable =connectData$"d_415693436"
            
list_lengths = unique(sapply(variable,nchar))
            
d_415693436_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[26,1]<-paste0("d_415693436_invalid_char_length")
            
df[26,2]<-paste0("invalid length(s) found:",d_415693436_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_719451909
# cross valid value check

            d_719451909_a = c(104430631)

            d_719451909_b = c(353358909, 104430631)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_719451909"[aa]%!in%d_719451909_a)

            d_719451909_invalid_cross_a = addNA(connectData$"d_719451909"[QCcheck1])

            QCcheck2 =which(connectData$"d_719451909"[bb]%!in%d_719451909_b)

            d_719451909_invalid_cross_b = addNA(connectData$"d_719451909"[QCcheck2])

            df[27,1]<-paste0("d_719451909_d_158291096_crossinvalid_values")

            df[27,2]<-paste0(d_719451909_b, collapse=", ")

            df[27,3]<-paste0(d_719451909_invalid_cross_a, collapse=", ")

            df[27,4]<-paste0(d_719451909_invalid_cross_b, collapse=", ")

######## QC d_377633816
# cross valid value check

            d_377633816_a = c(104430631)

            d_377633816_b = c(353358909, 104430631, NA)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_377633816"[aa]%!in%d_377633816_a)

            d_377633816_invalid_cross_a = addNA(connectData$"d_377633816"[QCcheck1])

            QCcheck2 =which(connectData$"d_377633816"[bb]%!in%d_377633816_b)

            d_377633816_invalid_cross_b = addNA(connectData$"d_377633816"[QCcheck2])

            df[28,1]<-paste0("d_377633816_d_158291096_crossinvalid_values")

            df[28,2]<-paste0(d_377633816_b, collapse=", ")

            df[28,3]<-paste0(d_377633816_invalid_cross_a, collapse=", ")

            df[28,4]<-paste0(d_377633816_invalid_cross_b, collapse=", ")

######## QC d_211023960
# cross valid value check

            d_211023960_a = c(104430631)

            d_211023960_b = c(353358909, 104430631)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_211023960"[aa]%!in%d_211023960_a)

            d_211023960_invalid_cross_a = addNA(connectData$"d_211023960"[QCcheck1])

            QCcheck2 =which(connectData$"d_211023960"[bb]%!in%d_211023960_b)

            d_211023960_invalid_cross_b = addNA(connectData$"d_211023960"[QCcheck2])

            df[29,1]<-paste0("d_211023960_d_158291096_crossinvalid_values")

            df[29,2]<-paste0(d_211023960_b, collapse=", ")

            df[29,3]<-paste0(d_211023960_invalid_cross_a, collapse=", ")

            df[29,4]<-paste0(d_211023960_invalid_cross_b, collapse=", ")

######## QC d_209509101
# cross valid value check

            d_209509101_a = c(104430631)

            d_209509101_b = c()

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_209509101"[aa]%!in%d_209509101_a)

            d_209509101_invalid_cross_a = addNA(connectData$"d_209509101"[QCcheck1])

            QCcheck2 =which(connectData$"d_209509101"[bb]%!in%d_209509101_b)

            d_209509101_invalid_cross_b = addNA(connectData$"d_209509101"[QCcheck2])

            df[30,1]<-paste0("d_209509101_d_158291096_crossinvalid_values")

            df[30,2]<-paste0(d_209509101_b, collapse=", ")

            df[30,3]<-paste0(d_209509101_invalid_cross_a, collapse=", ")

            df[30,4]<-paste0(d_209509101_invalid_cross_b, collapse=", ")

######## QC d_363026564
# cross valid value check

            d_363026564_a = c(104430631)

            d_363026564_b = c(353358909, 104430631)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_363026564"[aa]%!in%d_363026564_a)

            d_363026564_invalid_cross_a = addNA(connectData$"d_363026564"[QCcheck1])

            QCcheck2 =which(connectData$"d_363026564"[bb]%!in%d_363026564_b)

            d_363026564_invalid_cross_b = addNA(connectData$"d_363026564"[QCcheck2])

            df[31,1]<-paste0("d_363026564_d_158291096_crossinvalid_values")

            df[31,2]<-paste0(d_363026564_b, collapse=", ")

            df[31,3]<-paste0(d_363026564_invalid_cross_a, collapse=", ")

            df[31,4]<-paste0(d_363026564_invalid_cross_b, collapse=", ")

######## QC d_405352246
# cross valid value check

            d_405352246_a = c(104430631)

            d_405352246_b = c()

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_405352246"[aa]%!in%d_405352246_a)

            d_405352246_invalid_cross_a = addNA(connectData$"d_405352246"[QCcheck1])

            QCcheck2 =which(connectData$"d_405352246"[bb]%!in%d_405352246_b)

            d_405352246_invalid_cross_b = addNA(connectData$"d_405352246"[QCcheck2])

            df[32,1]<-paste0("d_405352246_d_158291096_crossinvalid_values")

            df[32,2]<-paste0(d_405352246_b, collapse=", ")

            df[32,3]<-paste0(d_405352246_invalid_cross_a, collapse=", ")

            df[32,4]<-paste0(d_405352246_invalid_cross_b, collapse=", ")

######## QC d_755545718
# cross valid value check

            d_755545718_a = c(104430631)

            d_755545718_b = c(353358909, 104430631)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_755545718"[aa]%!in%d_755545718_a)

            d_755545718_invalid_cross_a = addNA(connectData$"d_755545718"[QCcheck1])

            QCcheck2 =which(connectData$"d_755545718"[bb]%!in%d_755545718_b)

            d_755545718_invalid_cross_b = addNA(connectData$"d_755545718"[QCcheck2])

            df[33,1]<-paste0("d_755545718_d_158291096_crossinvalid_values")

            df[33,2]<-paste0(d_755545718_b, collapse=", ")

            df[33,3]<-paste0(d_755545718_invalid_cross_a, collapse=", ")

            df[33,4]<-paste0(d_755545718_invalid_cross_b, collapse=", ")

######## QC d_831137710
# cross valid value check

            d_831137710_a = c(104430631)

            d_831137710_b = c()

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_831137710"[aa]%!in%d_831137710_a)

            d_831137710_invalid_cross_a = addNA(connectData$"d_831137710"[QCcheck1])

            QCcheck2 =which(connectData$"d_831137710"[bb]%!in%d_831137710_b)

            d_831137710_invalid_cross_b = addNA(connectData$"d_831137710"[QCcheck2])

            df[34,1]<-paste0("d_831137710_d_158291096_crossinvalid_values")

            df[34,2]<-paste0(d_831137710_b, collapse=", ")

            df[34,3]<-paste0(d_831137710_invalid_cross_a, collapse=", ")

            df[34,4]<-paste0(d_831137710_invalid_cross_b, collapse=", ")

######## QC d_496935183
# cross valid value check

            d_496935183_a = c(104430631)

            d_496935183_b = c(353358909, 104430631)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_496935183"[aa]%!in%d_496935183_a)

            d_496935183_invalid_cross_a = addNA(connectData$"d_496935183"[QCcheck1])

            QCcheck2 =which(connectData$"d_496935183"[bb]%!in%d_496935183_b)

            d_496935183_invalid_cross_b = addNA(connectData$"d_496935183"[QCcheck2])

            df[35,1]<-paste0("d_496935183_d_158291096_crossinvalid_values")

            df[35,2]<-paste0(d_496935183_b, collapse=", ")

            df[35,3]<-paste0(d_496935183_invalid_cross_a, collapse=", ")

            df[35,4]<-paste0(d_496935183_invalid_cross_b, collapse=", ")

######## QC d_491099823
# cross valid value check

            d_491099823_a = c(104430631)

            d_491099823_b = c()

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_491099823"[aa]%!in%d_491099823_a)

            d_491099823_invalid_cross_a = addNA(connectData$"d_491099823"[QCcheck1])

            QCcheck2 =which(connectData$"d_491099823"[bb]%!in%d_491099823_b)

            d_491099823_invalid_cross_b = addNA(connectData$"d_491099823"[QCcheck2])

            df[36,1]<-paste0("d_491099823_d_158291096_crossinvalid_values")

            df[36,2]<-paste0(d_491099823_b, collapse=", ")

            df[36,3]<-paste0(d_491099823_invalid_cross_a, collapse=", ")

            df[36,4]<-paste0(d_491099823_invalid_cross_b, collapse=", ")

######## QC d_836460125
# cross valid value check

            d_836460125_a = c(104430631)

            d_836460125_b = c(353358909, 104430631)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_836460125"[aa]%!in%d_836460125_a)

            d_836460125_invalid_cross_a = addNA(connectData$"d_836460125"[QCcheck1])

            QCcheck2 =which(connectData$"d_836460125"[bb]%!in%d_836460125_b)

            d_836460125_invalid_cross_b = addNA(connectData$"d_836460125"[QCcheck2])

            df[37,1]<-paste0("d_836460125_d_158291096_crossinvalid_values")

            df[37,2]<-paste0(d_836460125_b, collapse=", ")

            df[37,3]<-paste0(d_836460125_invalid_cross_a, collapse=", ")

            df[37,4]<-paste0(d_836460125_invalid_cross_b, collapse=", ")

######## QC d_163534562
# cross valid value check

            d_163534562_a = c(104430631)

            d_163534562_b = c()

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_163534562"[aa]%!in%d_163534562_a)

            d_163534562_invalid_cross_a = addNA(connectData$"d_163534562"[QCcheck1])

            QCcheck2 =which(connectData$"d_163534562"[bb]%!in%d_163534562_b)

            d_163534562_invalid_cross_b = addNA(connectData$"d_163534562"[QCcheck2])

            df[38,1]<-paste0("d_163534562_d_158291096_crossinvalid_values")

            df[38,2]<-paste0(d_163534562_b, collapse=", ")

            df[38,3]<-paste0(d_163534562_invalid_cross_a, collapse=", ")

            df[38,4]<-paste0(d_163534562_invalid_cross_b, collapse=", ")

######## QC d_331787113
# cross valid value check

            d_331787113_a = c(104430631)

            d_331787113_b = c(353358909, 104430631)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_331787113"[aa]%!in%d_331787113_a)

            d_331787113_invalid_cross_a = addNA(connectData$"d_331787113"[QCcheck1])

            QCcheck2 =which(connectData$"d_331787113"[bb]%!in%d_331787113_b)

            d_331787113_invalid_cross_b = addNA(connectData$"d_331787113"[QCcheck2])

            df[39,1]<-paste0("d_331787113_d_158291096_crossinvalid_values")

            df[39,2]<-paste0(d_331787113_b, collapse=", ")

            df[39,3]<-paste0(d_331787113_invalid_cross_a, collapse=", ")

            df[39,4]<-paste0(d_331787113_invalid_cross_b, collapse=", ")

######## QC d_705732561
# cross valid value check

            d_705732561_a = c(104430631)

            d_705732561_b = c()

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_705732561"[aa]%!in%d_705732561_a)

            d_705732561_invalid_cross_a = addNA(connectData$"d_705732561"[QCcheck1])

            QCcheck2 =which(connectData$"d_705732561"[bb]%!in%d_705732561_b)

            d_705732561_invalid_cross_b = addNA(connectData$"d_705732561"[QCcheck2])

            df[40,1]<-paste0("d_705732561_d_158291096_crossinvalid_values")

            df[40,2]<-paste0(d_705732561_b, collapse=", ")

            df[40,3]<-paste0(d_705732561_invalid_cross_a, collapse=", ")

            df[40,4]<-paste0(d_705732561_invalid_cross_b, collapse=", ")

######## QC d_381509125
# cross valid value check

            d_381509125_a = c(104430631)

            d_381509125_b = c(353358909, 104430631)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_381509125"[aa]%!in%d_381509125_a)

            d_381509125_invalid_cross_a = addNA(connectData$"d_381509125"[QCcheck1])

            QCcheck2 =which(connectData$"d_381509125"[bb]%!in%d_381509125_b)

            d_381509125_invalid_cross_b = addNA(connectData$"d_381509125"[QCcheck2])

            df[41,1]<-paste0("d_381509125_d_158291096_crossinvalid_values")

            df[41,2]<-paste0(d_381509125_b, collapse=", ")

            df[41,3]<-paste0(d_381509125_invalid_cross_a, collapse=", ")

            df[41,4]<-paste0(d_381509125_invalid_cross_b, collapse=", ")

######## QC d_497530905
# cross valid value check

            d_497530905_a = c(104430631)

            d_497530905_b = c()

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_497530905"[aa]%!in%d_497530905_a)

            d_497530905_invalid_cross_a = addNA(connectData$"d_497530905"[QCcheck1])

            QCcheck2 =which(connectData$"d_497530905"[bb]%!in%d_497530905_b)

            d_497530905_invalid_cross_b = addNA(connectData$"d_497530905"[QCcheck2])

            df[42,1]<-paste0("d_497530905_d_158291096_crossinvalid_values")

            df[42,2]<-paste0(d_497530905_b, collapse=", ")

            df[42,3]<-paste0(d_497530905_invalid_cross_a, collapse=", ")

            df[42,4]<-paste0(d_497530905_invalid_cross_b, collapse=", ")

######## QC d_627995442
# cross valid value check

            d_627995442_a = c(104430631)

            d_627995442_b = c(353358909, 104430631)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_627995442"[aa]%!in%d_627995442_a)

            d_627995442_invalid_cross_a = addNA(connectData$"d_627995442"[QCcheck1])

            QCcheck2 =which(connectData$"d_627995442"[bb]%!in%d_627995442_b)

            d_627995442_invalid_cross_b = addNA(connectData$"d_627995442"[QCcheck2])

            df[43,1]<-paste0("d_627995442_d_158291096_crossinvalid_values")

            df[43,2]<-paste0(d_627995442_b, collapse=", ")

            df[43,3]<-paste0(d_627995442_invalid_cross_a, collapse=", ")

            df[43,4]<-paste0(d_627995442_invalid_cross_b, collapse=", ")

######## QC d_208102461
# cross valid value check

            d_208102461_a = c(104430631)

            d_208102461_b = c()

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_208102461"[aa]%!in%d_208102461_a)

            d_208102461_invalid_cross_a = addNA(connectData$"d_208102461"[QCcheck1])

            QCcheck2 =which(connectData$"d_208102461"[bb]%!in%d_208102461_b)

            d_208102461_invalid_cross_b = addNA(connectData$"d_208102461"[QCcheck2])

            df[44,1]<-paste0("d_208102461_d_158291096_crossinvalid_values")

            df[44,2]<-paste0(d_208102461_b, collapse=", ")

            df[44,3]<-paste0(d_208102461_invalid_cross_a, collapse=", ")

            df[44,4]<-paste0(d_208102461_invalid_cross_b, collapse=", ")

######## QC d_579618065
# cross valid value check

            d_579618065_a = c(104430631)

            d_579618065_b = c(353358909, 104430631)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_579618065"[aa]%!in%d_579618065_a)

            d_579618065_invalid_cross_a = addNA(connectData$"d_579618065"[QCcheck1])

            QCcheck2 =which(connectData$"d_579618065"[bb]%!in%d_579618065_b)

            d_579618065_invalid_cross_b = addNA(connectData$"d_579618065"[QCcheck2])

            df[45,1]<-paste0("d_579618065_d_158291096_crossinvalid_values")

            df[45,2]<-paste0(d_579618065_b, collapse=", ")

            df[45,3]<-paste0(d_579618065_invalid_cross_a, collapse=", ")

            df[45,4]<-paste0(d_579618065_invalid_cross_b, collapse=", ")

######## QC d_702433259
# cross valid value check

            d_702433259_a = c(104430631)

            d_702433259_b = c()

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_702433259"[aa]%!in%d_702433259_a)

            d_702433259_invalid_cross_a = addNA(connectData$"d_702433259"[QCcheck1])

            QCcheck2 =which(connectData$"d_702433259"[bb]%!in%d_702433259_b)

            d_702433259_invalid_cross_b = addNA(connectData$"d_702433259"[QCcheck2])

            df[46,1]<-paste0("d_702433259_d_158291096_crossinvalid_values")

            df[46,2]<-paste0(d_702433259_b, collapse=", ")

            df[46,3]<-paste0(d_702433259_invalid_cross_a, collapse=", ")

            df[46,4]<-paste0(d_702433259_invalid_cross_b, collapse=", ")

######## QC d_771146804
# cross valid value check

            d_771146804_a = c(104430631)

            d_771146804_b = c(353358909, 104430631)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_771146804"[aa]%!in%d_771146804_a)

            d_771146804_invalid_cross_a = addNA(connectData$"d_771146804"[QCcheck1])

            QCcheck2 =which(connectData$"d_771146804"[bb]%!in%d_771146804_b)

            d_771146804_invalid_cross_b = addNA(connectData$"d_771146804"[QCcheck2])

            df[47,1]<-paste0("d_771146804_d_158291096_crossinvalid_values")

            df[47,2]<-paste0(d_771146804_b, collapse=", ")

            df[47,3]<-paste0(d_771146804_invalid_cross_a, collapse=", ")

            df[47,4]<-paste0(d_771146804_invalid_cross_b, collapse=", ")

######## QC d_163284008
# cross valid value check

            d_163284008_a = c(104430631)

            d_163284008_b = c()

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_163284008"[aa]%!in%d_163284008_a)

            d_163284008_invalid_cross_a = addNA(connectData$"d_163284008"[QCcheck1])

            QCcheck2 =which(connectData$"d_163284008"[bb]%!in%d_163284008_b)

            d_163284008_invalid_cross_b = addNA(connectData$"d_163284008"[QCcheck2])

            df[48,1]<-paste0("d_163284008_d_158291096_crossinvalid_values")

            df[48,2]<-paste0(d_163284008_b, collapse=", ")

            df[48,3]<-paste0(d_163284008_invalid_cross_a, collapse=", ")

            df[48,4]<-paste0(d_163284008_invalid_cross_b, collapse=", ")

######## QC d_387198193
# cross valid value check

            d_387198193_a = c(104430631)

            d_387198193_b = c(353358909, 104430631)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_387198193"[aa]%!in%d_387198193_a)

            d_387198193_invalid_cross_a = addNA(connectData$"d_387198193"[QCcheck1])

            QCcheck2 =which(connectData$"d_387198193"[bb]%!in%d_387198193_b)

            d_387198193_invalid_cross_b = addNA(connectData$"d_387198193"[QCcheck2])

            df[49,1]<-paste0("d_387198193_d_158291096_crossinvalid_values")

            df[49,2]<-paste0(d_387198193_b, collapse=", ")

            df[49,3]<-paste0(d_387198193_invalid_cross_a, collapse=", ")

            df[49,4]<-paste0(d_387198193_invalid_cross_b, collapse=", ")

######## QC d_566047367
# cross valid value check

            d_566047367_a = c(104430631)

            d_566047367_b = c()

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_566047367"[aa]%!in%d_566047367_a)

            d_566047367_invalid_cross_a = addNA(connectData$"d_566047367"[QCcheck1])

            QCcheck2 =which(connectData$"d_566047367"[bb]%!in%d_566047367_b)

            d_566047367_invalid_cross_b = addNA(connectData$"d_566047367"[QCcheck2])

            df[50,1]<-paste0("d_566047367_d_158291096_crossinvalid_values")

            df[50,2]<-paste0(d_566047367_b, collapse=", ")

            df[50,3]<-paste0(d_566047367_invalid_cross_a, collapse=", ")

            df[50,4]<-paste0(d_566047367_invalid_cross_b, collapse=", ")

######## QC d_400259098
# cross valid value check

            d_400259098_a = c(104430631)

            d_400259098_b = c(353358909, 104430631)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_400259098"[aa]%!in%d_400259098_a)

            d_400259098_invalid_cross_a = addNA(connectData$"d_400259098"[QCcheck1])

            QCcheck2 =which(connectData$"d_400259098"[bb]%!in%d_400259098_b)

            d_400259098_invalid_cross_b = addNA(connectData$"d_400259098"[QCcheck2])

            df[51,1]<-paste0("d_400259098_d_158291096_crossinvalid_values")

            df[51,2]<-paste0(d_400259098_b, collapse=", ")

            df[51,3]<-paste0(d_400259098_invalid_cross_a, collapse=", ")

            df[51,4]<-paste0(d_400259098_invalid_cross_b, collapse=", ")

######## QC d_260703126
# cross valid value check

            d_260703126_a = c(104430631)

            d_260703126_b = c()

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_260703126"[aa]%!in%d_260703126_a)

            d_260703126_invalid_cross_a = addNA(connectData$"d_260703126"[QCcheck1])

            QCcheck2 =which(connectData$"d_260703126"[bb]%!in%d_260703126_b)

            d_260703126_invalid_cross_b = addNA(connectData$"d_260703126"[QCcheck2])

            df[52,1]<-paste0("d_260703126_d_158291096_crossinvalid_values")

            df[52,2]<-paste0(d_260703126_b, collapse=", ")

            df[52,3]<-paste0(d_260703126_invalid_cross_a, collapse=", ")

            df[52,4]<-paste0(d_260703126_invalid_cross_b, collapse=", ")

######## QC d_744197145
# cross valid value check

            d_744197145_a = c(104430631)

            d_744197145_b = c(353358909, 104430631)

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_744197145"[aa]%!in%d_744197145_a)

            d_744197145_invalid_cross_a = addNA(connectData$"d_744197145"[QCcheck1])

            QCcheck2 =which(connectData$"d_744197145"[bb]%!in%d_744197145_b)

            d_744197145_invalid_cross_b = addNA(connectData$"d_744197145"[QCcheck2])

            df[53,1]<-paste0("d_744197145_d_158291096_crossinvalid_values")

            df[53,2]<-paste0(d_744197145_b, collapse=", ")

            df[53,3]<-paste0(d_744197145_invalid_cross_a, collapse=", ")

            df[53,4]<-paste0(d_744197145_invalid_cross_b, collapse=", ")

######## QC d_950040334
# cross valid value check

            d_950040334_a = c(104430631)

            d_950040334_b = c()

            mylist_a1 =  paste0(rep("connectData$d_158291096 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_158291096 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_950040334"[aa]%!in%d_950040334_a)

            d_950040334_invalid_cross_a = addNA(connectData$"d_950040334"[QCcheck1])

            QCcheck2 =which(connectData$"d_950040334"[bb]%!in%d_950040334_b)

            d_950040334_invalid_cross_b = addNA(connectData$"d_950040334"[QCcheck2])

            df[54,1]<-paste0("d_950040334_d_158291096_crossinvalid_values")

            df[54,2]<-paste0(d_950040334_b, collapse=", ")

            df[54,3]<-paste0(d_950040334_invalid_cross_a, collapse=", ")

            df[54,4]<-paste0(d_950040334_invalid_cross_b, collapse=", ")

######## QC d_875549268
# valid value check
d_875549268= c(104430631, 353358909)
QCcheck1 =which(connectData$"d_875549268"%!in%d_875549268)
d_875549268_invalid = addNA(connectData$"d_875549268")[QCcheck1]
df[55,1]<-paste0("d_875549268_invalid")
df[55,2]<-paste0("104430631, 353358909")
df[55,3]<-paste0(d_875549268_invalid, collapse=", ")
######## QC d_230663853
# valid value check
d_230663853= c(104430631, 353358909)
QCcheck1 =which(connectData$"d_230663853"%!in%d_230663853)
d_230663853_invalid = addNA(connectData$"d_230663853")[QCcheck1]
df[56,1]<-paste0("d_230663853_invalid")
df[56,2]<-paste0("104430631, 353358909")
df[56,3]<-paste0(d_230663853_invalid, collapse=", ")
######## QC d_335767902
# valid dateTime check
d_335767902 = connectData$"d_335767902"
d_335767902_dateTime_invalid = which(!grepl("[0-9]?[1-9]-[0-9]?[1-9]-[1-2][0,9][0-9]?[1-9] [0-9]?[1-9]:[0-9]?[1-9]:[0-9]?[1-9]", d_335767902))
d_335767902_dateTime2_invalid = levels(addNA(connectData$"d_335767902"[d_471593703_dateTime_invalid]))

            df[57,1]<-paste0("d_335767902_dateTime_invalid")
df[57,2]<-paste0("MMDDYYYY 00:00:00")
df[57,3]<-paste0(d_335767902_dateTime2_invalid, collapse=", ")
######## QC d_828729648
# valid value check
d_828729648= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_828729648"%!in%d_828729648)
d_828729648_invalid = addNA(connectData$"d_828729648")[QCcheck1]
df[58,1]<-paste0("d_828729648_invalid")
df[58,2]<-paste0("353358909, 104430631, NA")
df[58,3]<-paste0(d_828729648_invalid, collapse=", ")
######## QC d_379080287
# valid character length check
valid_length= 6
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_379080287"))
variable =connectData$"d_379080287"
            
list_lengths = unique(sapply(variable,nchar))
            
d_379080287_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[59,1]<-paste0("d_379080287_invalid_char_length")
            
df[59,2]<-paste0("invalid length(s) found:",d_379080287_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_948195369
# valid value check
d_948195369= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_948195369"%!in%d_948195369)
d_948195369_invalid = addNA(connectData$"d_948195369")[QCcheck1]
df[60,1]<-paste0("d_948195369_invalid")
df[60,2]<-paste0("353358909, 104430631, NA")
df[60,3]<-paste0(d_948195369_invalid, collapse=", ")
######## QC d_827220437
# cross valid value check

            d_827220437_a = c(NA)

            d_827220437_b = c(531629870, 548392715, 125001209, 327912200, 300267574, 452412599, 303349821, 657167265, 809703864, 517700004, 181769837)

            mylist_a1 =  paste0(rep("connectData$d_512820379 == "), c(854703046), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_512820379 != "), c(854703046), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_827220437"[aa]%!in%d_827220437_a)

            d_827220437_invalid_cross_a = addNA(connectData$"d_827220437"[QCcheck1])

            QCcheck2 =which(connectData$"d_827220437"[bb]%!in%d_827220437_b)

            d_827220437_invalid_cross_b = addNA(connectData$"d_827220437"[QCcheck2])

            df[61,1]<-paste0("d_827220437_d_512820379_crossinvalid_values")

            df[61,2]<-paste0(d_827220437_b, collapse=", ")

            df[61,3]<-paste0(d_827220437_invalid_cross_a, collapse=", ")

            df[61,4]<-paste0(d_827220437_invalid_cross_b, collapse=", ")

######## QC d_461488577
# valid value check
d_461488577= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_461488577"%!in%d_461488577)
d_461488577_invalid = addNA(connectData$"d_461488577")[QCcheck1]
df[62,1]<-paste0("d_461488577_invalid")
df[62,2]<-paste0("353358909, 104430631, NA")
df[62,3]<-paste0(d_461488577_invalid, collapse=", ")
######## QC d_942255248
# valid value check
d_942255248= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_942255248"%!in%d_942255248)
d_942255248_invalid = addNA(connectData$"d_942255248")[QCcheck1]
df[63,1]<-paste0("d_942255248_invalid")
df[63,2]<-paste0("353358909, 104430631, NA")
df[63,3]<-paste0(d_942255248_invalid, collapse=", ")
######## QC d_607081902
# valid value check
d_607081902= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_607081902"%!in%d_607081902)
d_607081902_invalid = addNA(connectData$"d_607081902")[QCcheck1]
df[64,1]<-paste0("d_607081902_invalid")
df[64,2]<-paste0("353358909, 104430631, NA")
df[64,3]<-paste0(d_607081902_invalid, collapse=", ")
######## QC d_639721694
# valid value check
d_639721694= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_639721694"%!in%d_639721694)
d_639721694_invalid = addNA(connectData$"d_639721694")[QCcheck1]
df[65,1]<-paste0("d_639721694_invalid")
df[65,2]<-paste0("353358909, 104430631, NA")
df[65,3]<-paste0(d_639721694_invalid, collapse=", ")
######## QC d_196856782
# valid value check
d_196856782= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_196856782"%!in%d_196856782)
d_196856782_invalid = addNA(connectData$"d_196856782")[QCcheck1]
df[66,1]<-paste0("d_196856782_invalid")
df[66,2]<-paste0("353358909, 104430631, NA")
df[66,3]<-paste0(d_196856782_invalid, collapse=", ")
######## QC d_177402915
# valid value check
d_177402915= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_177402915"%!in%d_177402915)
d_177402915_invalid = addNA(connectData$"d_177402915")[QCcheck1]
df[67,1]<-paste0("d_177402915_invalid")
df[67,2]<-paste0("353358909, 104430631, NA")
df[67,3]<-paste0(d_177402915_invalid, collapse=", ")
######## QC d_791389099
# valid value check
d_791389099= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_791389099"%!in%d_791389099)
d_791389099_invalid = addNA(connectData$"d_791389099")[QCcheck1]
df[68,1]<-paste0("d_791389099_invalid")
df[68,2]<-paste0("353358909, 104430631, NA")
df[68,3]<-paste0(d_791389099_invalid, collapse=", ")
######## QC d_684726272
# valid value check
d_684726272= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_684726272"%!in%d_684726272)
d_684726272_invalid = addNA(connectData$"d_684726272")[QCcheck1]
df[69,1]<-paste0("d_684726272_invalid")
df[69,2]<-paste0("353358909, 104430631, NA")
df[69,3]<-paste0(d_684726272_invalid, collapse=", ")
######## QC d_241590841
# valid value check
d_241590841= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_241590841"%!in%d_241590841)
d_241590841_invalid = addNA(connectData$"d_241590841")[QCcheck1]
df[70,1]<-paste0("d_241590841_invalid")
df[70,2]<-paste0("353358909, 104430631, NA")
df[70,3]<-paste0(d_241590841_invalid, collapse=", ")
######## QC d_206879104
# valid value check
d_206879104= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_206879104"%!in%d_206879104)
d_206879104_invalid = addNA(connectData$"d_206879104")[QCcheck1]
df[71,1]<-paste0("d_206879104_invalid")
df[71,2]<-paste0("353358909, 104430631, NA")
df[71,3]<-paste0(d_206879104_invalid, collapse=", ")
######## QC d_642287621
# valid value check
d_642287621= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_642287621"%!in%d_642287621)
d_642287621_invalid = addNA(connectData$"d_642287621")[QCcheck1]
df[72,1]<-paste0("d_642287621_invalid")
df[72,2]<-paste0("353358909, 104430631, NA")
df[72,3]<-paste0(d_642287621_invalid, collapse=", ")
######## QC d_520301146
# valid value check
d_520301146= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_520301146"%!in%d_520301146)
d_520301146_invalid = addNA(connectData$"d_520301146")[QCcheck1]
df[73,1]<-paste0("d_520301146_invalid")
df[73,2]<-paste0("353358909, 104430631, NA")
df[73,3]<-paste0(d_520301146_invalid, collapse=", ")
######## QC d_285130077
# valid value check
d_285130077= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_285130077"%!in%d_285130077)
d_285130077_invalid = addNA(connectData$"d_285130077")[QCcheck1]
df[74,1]<-paste0("d_285130077_invalid")
df[74,2]<-paste0("353358909, 104430631, NA")
df[74,3]<-paste0(d_285130077_invalid, collapse=", ")
######## QC d_549687190
# valid value check
d_549687190= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_549687190"%!in%d_549687190)
d_549687190_invalid = addNA(connectData$"d_549687190")[QCcheck1]
df[75,1]<-paste0("d_549687190_invalid")
df[75,2]<-paste0("353358909, 104430631, NA")
df[75,3]<-paste0(d_549687190_invalid, collapse=", ")
######## QC d_326825649
# valid value check
d_326825649= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_326825649"%!in%d_326825649)
d_326825649_invalid = addNA(connectData$"d_326825649")[QCcheck1]
df[76,1]<-paste0("d_326825649_invalid")
df[76,2]<-paste0("353358909, 104430631, NA")
df[76,3]<-paste0(d_326825649_invalid, collapse=", ")
######## QC d_819377306
# valid value check
d_819377306= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_819377306"%!in%d_819377306)
d_819377306_invalid = addNA(connectData$"d_819377306")[QCcheck1]
df[77,1]<-paste0("d_819377306_invalid")
df[77,2]<-paste0("353358909, 104430631, NA")
df[77,3]<-paste0(d_819377306_invalid, collapse=", ")
######## QC d_829269606
# valid value check
d_829269606= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_829269606"%!in%d_829269606)
d_829269606_invalid = addNA(connectData$"d_829269606")[QCcheck1]
df[78,1]<-paste0("d_829269606_invalid")
df[78,2]<-paste0("353358909, 104430631, NA")
df[78,3]<-paste0(d_829269606_invalid, collapse=", ")
######## QC d_967372009
# valid value check
d_967372009= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_967372009"%!in%d_967372009)
d_967372009_invalid = addNA(connectData$"d_967372009")[QCcheck1]
df[79,1]<-paste0("d_967372009_invalid")
df[79,2]<-paste0("353358909, 104430631, NA")
df[79,3]<-paste0(d_967372009_invalid, collapse=", ")
######## QC d_462314689
# valid value check
d_462314689= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_462314689"%!in%d_462314689)
d_462314689_invalid = addNA(connectData$"d_462314689")[QCcheck1]
df[80,1]<-paste0("d_462314689_invalid")
df[80,2]<-paste0("353358909, 104430631, NA")
df[80,3]<-paste0(d_462314689_invalid, collapse=", ")
######## QC d_412000022
# valid character length check
valid_length= 11
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_412000022"))
variable =connectData$"d_412000022"
            
list_lengths = unique(sapply(variable,nchar))
            
d_412000022_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[81,1]<-paste0("d_412000022_invalid_char_length")
            
df[81,2]<-paste0("invalid length(s) found:",d_412000022_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_558435199
# valid value check
d_558435199= c(353358909, 104430631)
QCcheck1 =which(connectData$"d_558435199"%!in%d_558435199)
d_558435199_invalid = addNA(connectData$"d_558435199")[QCcheck1]
df[82,1]<-paste0("d_558435199_invalid")
df[82,2]<-paste0("353358909, 104430631")
df[82,3]<-paste0(d_558435199_invalid, collapse=", ")
######## QC d_262613359
# valid dateTime check
d_262613359 = connectData$"d_262613359"
d_262613359_dateTime_invalid = which(!grepl("[0-9]?[1-9]-[0-9]?[1-9]-[1-2][0,9][0-9]?[1-9] [0-9]?[1-9]:[0-9]?[1-9]:[0-9]?[1-9]", d_262613359))
d_262613359_dateTime2_invalid = levels(addNA(connectData$"d_262613359"[d_471593703_dateTime_invalid]))

            df[83,1]<-paste0("d_262613359_dateTime_invalid")
df[83,2]<-paste0("MMDDYYYY 00:00:00")
df[83,3]<-paste0(d_262613359_dateTime2_invalid, collapse=", ")
######## QC d_454205108
# valid character length check
valid_length= 
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_454205108"))
variable =connectData$"d_454205108"
            
list_lengths = unique(sapply(variable,nchar))
            
d_454205108_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[84,1]<-paste0("d_454205108_invalid_char_length")
            
df[84,2]<-paste0("invalid length(s) found:",d_454205108_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_866095545
# valid value check
d_866095545= c(353358909, 104430631,)
QCcheck1 =which(connectData$"d_866095545"%!in%d_866095545)
d_866095545_invalid = addNA(connectData$"d_866095545")[QCcheck1]
df[85,1]<-paste0("d_866095545_invalid")
df[85,2]<-paste0("353358909, 104430631,")
df[85,3]<-paste0(d_866095545_invalid, collapse=", ")
######## QC d_471168198
# valid character length check
valid_length= 
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_471168198"))
variable =connectData$"d_471168198"
            
list_lengths = unique(sapply(variable,nchar))
            
d_471168198_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[86,1]<-paste0("d_471168198_invalid_char_length")
            
df[86,2]<-paste0("invalid length(s) found:",d_471168198_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_736251808
# valid character length check
valid_length= 
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_736251808"))
variable =connectData$"d_736251808"
            
list_lengths = unique(sapply(variable,nchar))
            
d_736251808_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[87,1]<-paste0("d_736251808_invalid_char_length")
            
df[87,2]<-paste0("invalid length(s) found:",d_736251808_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_436680969
# valid character length check
valid_length= 
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_436680969"))
variable =connectData$"d_436680969"
            
list_lengths = unique(sapply(variable,nchar))
            
d_436680969_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[88,1]<-paste0("d_436680969_invalid_char_length")
            
df[88,2]<-paste0("invalid length(s) found:",d_436680969_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_480305327
# valid value check
d_480305327= c(612166858, 255907182, 226924545, 270793412, 959021713, 643664527, 537892528, NA)
QCcheck1 =which(connectData$"d_480305327"%!in%d_480305327)
d_480305327_invalid = addNA(connectData$"d_480305327")[QCcheck1]
df[89,1]<-paste0("d_480305327_invalid")
df[89,2]<-paste0("612166858, 255907182, 226924545, 270793412, 959021713, 643664527, 537892528, NA")
df[89,3]<-paste0(d_480305327_invalid, collapse=", ")
######## QC d_919254129
# valid value check
d_919254129= c(353358909, 104430631)
QCcheck1 =which(connectData$"d_919254129"%!in%d_919254129)
d_919254129_invalid = addNA(connectData$"d_919254129")[QCcheck1]
df[92,1]<-paste0("d_919254129_invalid")
df[92,2]<-paste0("353358909, 104430631")
df[92,3]<-paste0(d_919254129_invalid, collapse=", ")
######## QC d_492983562
# valid character length check
valid_length= 240
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_492983562"))
variable =connectData$"d_492983562"
            
list_lengths = unique(sapply(variable,nchar))
            
d_492983562_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[93,1]<-paste0("d_492983562_invalid_char_length")
            
df[93,2]<-paste0("invalid length(s) found:",d_492983562_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_756862764
# valid character length check
valid_length= 50
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_756862764"))
variable =connectData$"d_756862764"
            
list_lengths = unique(sapply(variable,nchar))
            
d_756862764_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[96,1]<-paste0("d_756862764_invalid_char_length")
            
df[96,2]<-paste0("invalid length(s) found:",d_756862764_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_995036844
# valid value check
d_995036844= c(943488874, 101178950, 804918759)
QCcheck1 =which(connectData$"d_995036844"%!in%d_995036844)
d_995036844_invalid = addNA(connectData$"d_995036844")[QCcheck1]
df[97,1]<-paste0("d_995036844_invalid")
df[97,2]<-paste0("943488874, 101178950, 804918759")
df[97,3]<-paste0(d_995036844_invalid, collapse=", ")
######## QC d_399159511
# valid character length check
valid_length= 50
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_399159511"))
variable =connectData$"d_399159511"
            
list_lengths = unique(sapply(variable,nchar))
            
d_399159511_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[98,1]<-paste0("d_399159511_invalid_char_length")
            
df[98,2]<-paste0("invalid length(s) found:",d_399159511_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_231676651
# valid character length check
valid_length= 50
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_231676651"))
variable =connectData$"d_231676651"
            
list_lengths = unique(sapply(variable,nchar))
            
d_231676651_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[99,1]<-paste0("d_231676651_invalid_char_length")
            
df[99,2]<-paste0("invalid length(s) found:",d_231676651_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_996038075
# valid character length check
valid_length= 50
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_996038075"))
variable =connectData$"d_996038075"
            
list_lengths = unique(sapply(variable,nchar))
            
d_996038075_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[100,1]<-paste0("d_996038075_invalid_char_length")
            
df[100,2]<-paste0("invalid length(s) found:",d_996038075_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_506826178
# valid value check
d_506826178= c(612166858, 255907182, 226924545, 270793412, 959021713, 643664527, 537892528, NA)
QCcheck1 =which(connectData$"d_506826178"%!in%d_506826178)
d_506826178_invalid = addNA(connectData$"d_506826178")[QCcheck1]
df[101,1]<-paste0("d_506826178_invalid")
df[101,2]<-paste0("612166858, 255907182, 226924545, 270793412, 959021713, 643664527, 537892528, NA")
df[101,3]<-paste0(d_506826178_invalid, collapse=", ")
######## QC d_153211406
# valid character length check
valid_length= 50
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_153211406"))
variable =connectData$"d_153211406"
            
list_lengths = unique(sapply(variable,nchar))
            
d_153211406_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[102,1]<-paste0("d_153211406_invalid_char_length")
            
df[102,2]<-paste0("invalid length(s) found:",d_153211406_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_544150384
# valid year check
d_544150384 = connectData$"d_544150384"
d_544150384_year_invalid = which(!grepl("^[1]{1}[9]{1}[0-9]{1}[0-9]{1}$|^[2]{1}[0]{1}[0-9]{1}[0-9]{1}$", d_544150384))
d_544150384_age_invalid = levels(addNA(connectData$"d_544150384"[d_471593703_age_invalid]))

            df[105,1]<-paste0("d_544150384_year_check")
df[105,2]<-paste0("valid year format:[1-2][0,9][0:9][0:9]")
ndf[105,3]<-paste0("invalid year:",d_544150384_year_invalid, collapse=", ")
######## QC d_117249500
# valid numeric length check
valid_length= 3
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_117249500"))
variable =connectData$"d_117249500"
            
list_lengths = unique(sapply(variable ,nchar))
            
d_117249500_invalid_num_length = list_lengths[list_lengths > valid_length]
            
df[107,1]<-paste0("d_117249500_invalid_char_length")
            
df[107,2]<-paste0("invalid length(s) found:",d_117249500_invalid_num_length,"non integer value(s) found:",!var.is.integer, collapse=", ")
######## QC d_271757434
# valid value check
d_271757434= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_271757434"%!in%d_271757434)
d_271757434_invalid = addNA(connectData$"d_271757434")[QCcheck1]
df[109,1]<-paste0("d_271757434_invalid")
df[109,2]<-paste0("353358909, 104430631, NA")
df[109,3]<-paste0(d_271757434_invalid, collapse=", ")
######## QC d_646873644
# valid value check
d_646873644= c(353358909, 104430631)
QCcheck1 =which(connectData$"d_646873644"%!in%d_646873644)
d_646873644_invalid = addNA(connectData$"d_646873644")[QCcheck1]
df[110,1]<-paste0("d_646873644_invalid")
df[110,2]<-paste0("353358909, 104430631")
df[110,3]<-paste0(d_646873644_invalid, collapse=", ")
######## QC d_187894482
# valid value check
d_187894482= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_187894482"%!in%d_187894482)
d_187894482_invalid = addNA(connectData$"d_187894482")[QCcheck1]
df[112,1]<-paste0("d_187894482_invalid")
df[112,2]<-paste0("353358909, 104430631, NA")
df[112,3]<-paste0(d_187894482_invalid, collapse=", ")
######## QC d_983278853
# valid value check
d_983278853= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_983278853"%!in%d_983278853)
d_983278853_invalid = addNA(connectData$"d_983278853")[QCcheck1]
df[114,1]<-paste0("d_983278853_invalid")
df[114,2]<-paste0("353358909, 104430631, NA")
df[114,3]<-paste0(d_983278853_invalid, collapse=", ")
######## QC d_869588347
# valid character length check
valid_length= 120
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_869588347"))
variable =connectData$"d_869588347"
            
list_lengths = unique(sapply(variable,nchar))
            
d_869588347_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[115,1]<-paste0("d_869588347_invalid_char_length")
            
df[115,2]<-paste0("invalid length(s) found:",d_869588347_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_849786503
# valid character length check
valid_length= 120
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_849786503"))
variable =connectData$"d_849786503"
            
list_lengths = unique(sapply(variable,nchar))
            
d_849786503_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[116,1]<-paste0("d_849786503_invalid_char_length")
            
df[116,2]<-paste0("invalid length(s) found:",d_849786503_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_635101039
# valid character length check
valid_length= 120
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_635101039"))
variable =connectData$"d_635101039"
            
list_lengths = unique(sapply(variable,nchar))
            
d_635101039_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[117,1]<-paste0("d_635101039_invalid_char_length")
            
df[117,2]<-paste0("invalid length(s) found:",d_635101039_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_714419972
# valid character length check
valid_length= 120
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_714419972"))
variable =connectData$"d_714419972"
            
list_lengths = unique(sapply(variable,nchar))
            
d_714419972_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[118,1]<-paste0("d_714419972_invalid_char_length")
            
df[118,2]<-paste0("invalid length(s) found:",d_714419972_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_524461170
# valid value check
d_524461170= c(357184057, 127547625

)
QCcheck1 =which(connectData$"d_524461170"%!in%d_524461170)
d_524461170_invalid = addNA(connectData$"d_524461170")[QCcheck1]
df[119,1]<-paste0("d_524461170_invalid")
df[119,2]<-paste0("357184057, 127547625

")
df[119,3]<-paste0(d_524461170_invalid, collapse=", ")
######## QC d_521824358
# valid character length check
valid_length= 70
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_521824358"))
variable =connectData$"d_521824358"
            
list_lengths = unique(sapply(variable,nchar))
            
d_521824358_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[120,1]<-paste0("d_521824358_invalid_char_length")
            
df[120,2]<-paste0("invalid length(s) found:",d_521824358_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_442166669
# valid character length check
valid_length= 70
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_442166669"))
variable =connectData$"d_442166669"
            
list_lengths = unique(sapply(variable,nchar))
            
d_442166669_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[121,1]<-paste0("d_442166669_invalid_char_length")
            
df[121,2]<-paste0("invalid length(s) found:",d_442166669_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_703385619
# valid character length check
valid_length= 45
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_703385619"))
variable =connectData$"d_703385619"
            
list_lengths = unique(sapply(variable,nchar))
            
d_703385619_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[122,1]<-paste0("d_703385619_invalid_char_length")
            
df[122,2]<-paste0("invalid length(s) found:",d_703385619_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_634434746
# valid character length check
valid_length= 48
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_634434746"))
variable =connectData$"d_634434746"
            
list_lengths = unique(sapply(variable,nchar))
            
d_634434746_invalid_char_length = list_lengths[list_lengths > valid_length]
            
df[123,1]<-paste0("d_634434746_invalid_char_length")
            
df[123,2]<-paste0("invalid length(s) found:",d_634434746_invalid_char_length,"all integer value(s) found:",var.is.integer, collapse=", ")
######## QC d_892050548
# valid numeric length check
valid_length= 5
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_892050548"))
variable =connectData$"d_892050548"
            
list_lengths = unique(sapply(variable ,nchar))
            
d_892050548_invalid_num_length = list_lengths[list_lengths > valid_length]
            
df[124,1]<-paste0("d_892050548_invalid_char_length")
            
df[124,2]<-paste0("invalid length(s) found:",d_892050548_invalid_num_length,"non integer value(s) found:",!var.is.integer, collapse=", ")
######## QC d_452166062
# valid value check
d_452166062= c(353358909, 104430631, NA)
QCcheck1 =which(connectData$"d_452166062"%!in%d_452166062)
d_452166062_invalid = addNA(connectData$"d_452166062")[QCcheck1]
df[125,1]<-paste0("d_452166062_invalid")
df[125,2]<-paste0("353358909, 104430631, NA")
df[125,3]<-paste0(d_452166062_invalid, collapse=", ")
######## QC d_650597106
# cross valid year check

            d_650597106_a = c(NA)

            d_650597106_b = c(NA, num(yyyy))

            year = "^[1]{1}[9]{1}[0-9]{1}[0-9]{1}$|^[2]{1}[0]{1}[0-9]{1}[0-9]{1}$"
            mylist_a1 =  paste0(rep("connectData$d_452166062
 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_452166062
 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            
            QCcheck1 =which(!grepl(year, d_650597106"[aa]))

            d_650597106_invalid_year = addNA(connectData$"d_650597106"[QCcheck1])

            
            QCcheck2 =which(connectData$"d_650597106"[bb]%!in%d_650597106_b)

            d_650597106_invalid_not_year = addNA(connectData$"d_650597106"[QCcheck2])

            
            df[126,1]<-paste0("d_650597106_d_452166062
_crossinvalidyear_values")

            df[126,2]<-paste0("valid year format:[1-2][0,9][0:9][0:9]")

            df[126,3]<-paste0("invalid year values:",d_650597106_invalid_year, collapse=", ")

            df[126,4]<-paste0("values that should be blank:",d_650597106_invalid_not_year, collapse=", ")

######## QC d_266952173
# cross valid value check

            d_266952173_a = c(NA)

            d_266952173_b = c(NA, char(800))

            mylist_a1 =  paste0(rep("connectData$d_452166062
 == "), c(353358910), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_452166062
 != "), c(353358910), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_266952173"[aa]%!in%d_266952173_a)

            d_266952173_invalid_cross_a = addNA(connectData$"d_266952173"[QCcheck1])

            QCcheck2 =which(connectData$"d_266952173"[bb]%!in%d_266952173_b)

            d_266952173_invalid_cross_b = addNA(connectData$"d_266952173"[QCcheck2])

            df[127,1]<-paste0("d_266952173_d_452166062
_crossinvalid_values")

            df[127,2]<-paste0(d_266952173_b, collapse=", ")

            df[127,3]<-paste0(d_266952173_invalid_cross_a, collapse=", ")

            df[127,4]<-paste0(d_266952173_invalid_cross_b, collapse=", ")

######## QC d_494982282
# cross valid value check

            d_494982282_a = c(NA)

            d_494982282_b = c(NA, char(800))

            mylist_a1 =  paste0(rep("connectData$d_452166062
 == "), c(353358911), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_452166062
 != "), c(353358911), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_494982282"[aa]%!in%d_494982282_a)

            d_494982282_invalid_cross_a = addNA(connectData$"d_494982282"[QCcheck1])

            QCcheck2 =which(connectData$"d_494982282"[bb]%!in%d_494982282_b)

            d_494982282_invalid_cross_b = addNA(connectData$"d_494982282"[QCcheck2])

            df[128,1]<-paste0("d_494982282_d_452166062
_crossinvalid_values")

            df[128,2]<-paste0(d_494982282_b, collapse=", ")

            df[128,3]<-paste0(d_494982282_invalid_cross_a, collapse=", ")

            df[128,4]<-paste0(d_494982282_invalid_cross_b, collapse=", ")

######## QC d_699625233
# valid value check
d_699625233= c(353358909, 104430631)
QCcheck1 =which(connectData$"d_699625233"%!in%d_699625233)
d_699625233_invalid = addNA(connectData$"d_699625233")[QCcheck1]
df[129,1]<-paste0("d_699625233_invalid")
df[129,2]<-paste0("353358909, 104430631")
df[129,3]<-paste0(d_699625233_invalid, collapse=", ")
######## QC d_430551721
# valid dateTime check
d_430551721 = connectData$"d_430551721"
d_430551721_dateTime_invalid = which(!grepl("[0-9]?[1-9]-[0-9]?[1-9]-[1-2][0,9][0-9]?[1-9] [0-9]?[1-9]:[0-9]?[1-9]:[0-9]?[1-9]", d_430551721))
d_430551721_dateTime2_invalid = levels(addNA(connectData$"d_430551721"[d_471593703_dateTime_invalid]))

            df[130,1]<-paste0("d_430551721_dateTime_invalid")
df[130,2]<-paste0("MMDDYYYY 00:00:00")
df[130,3]<-paste0(d_430551721_dateTime2_invalid, collapse=", ")
######## QC d_204002618
# valid numeric length check
valid_length= 18
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_204002618"))
variable =connectData$"d_204002618"
            
list_lengths = unique(sapply(variable ,nchar))
            
d_204002618_invalid_num_length = list_lengths[list_lengths > valid_length]
            
df[131,1]<-paste0("d_204002618_invalid_char_length")
            
df[131,2]<-paste0("invalid length(s) found:",d_204002618_invalid_num_length,"non integer value(s) found:",!var.is.integer, collapse=", ")
######## QC d_765924958
# valid numeric length check
valid_length= 18
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_765924958"))
variable =connectData$"d_765924958"
            
list_lengths = unique(sapply(variable ,nchar))
            
d_765924958_invalid_num_length = list_lengths[list_lengths > valid_length]
            
df[132,1]<-paste0("d_765924958_invalid_char_length")
            
df[132,2]<-paste0("invalid length(s) found:",d_765924958_invalid_num_length,"non integer value(s) found:",!var.is.integer, collapse=", ")
######## QC d_821247024
# valid value check
d_821247024= c(875007964, 197316935, 219863910, 922622075, 160161595)
QCcheck1 =which(connectData$"d_821247024"%!in%d_821247024)
d_821247024_invalid = addNA(connectData$"d_821247024")[QCcheck1]
df[133,1]<-paste0("d_821247024_invalid")
df[133,2]<-paste0("875007964, 197316935, 219863910, 922622075, 160161595")
df[133,3]<-paste0(d_821247024_invalid, collapse=", ")
######## QC d_914594314
# valid dateTime check
d_914594314 = connectData$"d_914594314"
d_914594314_dateTime_invalid = which(!grepl("[0-9]?[1-9]-[0-9]?[1-9]-[1-2][0,9][0-9]?[1-9] [0-9]?[1-9]:[0-9]?[1-9]:[0-9]?[1-9]", d_914594314))
d_914594314_dateTime2_invalid = levels(addNA(connectData$"d_914594314"[d_471593703_dateTime_invalid]))

            df[134,1]<-paste0("d_914594314_dateTime_invalid")
df[134,2]<-paste0("MMDDYYYY 00:00:00")
df[134,3]<-paste0(d_914594314_dateTime2_invalid, collapse=", ")
######## QC d_139603724
# valid numeric length check
valid_length= 2
            
var.is.integer =suppressWarnings(testInteger(connectData$"d_139603724"))
variable =connectData$"d_139603724"
            
list_lengths = unique(sapply(variable ,nchar))
            
d_139603724_invalid_num_length = list_lengths[list_lengths > valid_length]
            
df[135,1]<-paste0("d_139603724_invalid_char_length")
            
df[135,2]<-paste0("invalid length(s) found:",d_139603724_invalid_num_length,"non integer value(s) found:",!var.is.integer, collapse=", ")
######## QC d_444699761
# valid value check
d_444699761= c(734437214, 426360242)
QCcheck1 =which(connectData$"d_444699761"%!in%d_444699761)
d_444699761_invalid = addNA(connectData$"d_444699761")[QCcheck1]
df[136,1]<-paste0("d_444699761_invalid")
df[136,2]<-paste0("734437214, 426360242")
df[136,3]<-paste0(d_444699761_invalid, collapse=", ")
######## QC d_188797763
# valid value check
d_188797763= c(353358909, 104430631)
QCcheck1 =which(connectData$"d_188797763"%!in%d_188797763)
d_188797763_invalid = addNA(connectData$"d_188797763")[QCcheck1]
df[137,1]<-paste0("d_188797763_invalid")
df[137,2]<-paste0("353358909, 104430631")
df[137,3]<-paste0(d_188797763_invalid, collapse=", ")
######## QC d_953614051
# valid value check
d_953614051= c(734437214, 426360242)
QCcheck1 =which(connectData$"d_953614051"%!in%d_953614051)
d_953614051_invalid = addNA(connectData$"d_953614051")[QCcheck1]
df[138,1]<-paste0("d_953614051_invalid")
df[138,2]<-paste0("734437214, 426360242")
df[138,3]<-paste0(d_953614051_invalid, collapse=", ")
######## QC d_148197146
# valid value check
d_148197146= c(NA)
QCcheck1 =which(connectData$"d_148197146"%!in%d_148197146)
d_148197146_invalid = addNA(connectData$"d_148197146")[QCcheck1]
df[139,1]<-paste0("d_148197146_invalid")
df[139,2]<-paste0("NA")
df[139,3]<-paste0(d_148197146_invalid, collapse=", ")
######## QC d_793822265
# valid value check
d_793822265= c(132080040, 854903954, 965707001)
QCcheck1 =which(connectData$"d_793822265"%!in%d_793822265)
d_793822265_invalid = addNA(connectData$"d_793822265")[QCcheck1]
df[140,1]<-paste0("d_793822265_invalid")
df[140,2]<-paste0("132080040, 854903954, 965707001")
df[140,3]<-paste0(d_793822265_invalid, collapse=", ")
######## QC d_147176963
# valid value check
d_147176963= c(356674370, 219803804)
QCcheck1 =which(connectData$"d_147176963"%!in%d_147176963)
d_147176963_invalid = addNA(connectData$"d_147176963")[QCcheck1]
df[141,1]<-paste0("d_147176963_invalid")
df[141,2]<-paste0("356674370, 219803804")
df[141,3]<-paste0(d_147176963_invalid, collapse=", ")
######## QC d_557461333
# valid value check
d_557461333= c(356674370, 219803804)
QCcheck1 =which(connectData$"d_557461333"%!in%d_557461333)
d_557461333_invalid = addNA(connectData$"d_557461333")[QCcheck1]
df[142,1]<-paste0("d_557461333_invalid")
df[142,2]<-paste0("356674370, 219803804")
df[142,3]<-paste0(d_557461333_invalid, collapse=", ")
######## QC d_725929722
# valid value check
d_725929722= c(356674370, 219803804)
QCcheck1 =which(connectData$"d_725929722"%!in%d_725929722)
d_725929722_invalid = addNA(connectData$"d_725929722")[QCcheck1]
df[143,1]<-paste0("d_725929722_invalid")
df[143,2]<-paste0("356674370, 219803804")
df[143,3]<-paste0(d_725929722_invalid, collapse=", ")
######## QC d_711794630
# cross valid value check

            d_711794630_a = c(NA)

            d_711794630_b = c(356674370, 219803804)

            mylist_a1 =  paste0(rep("connectData$d_827220437 == "), c(548392715, 125001209, 327912200, 300267574, 452412599, 657167265




), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$d_827220437 != "), c(548392715, 125001209, 327912200, 300267574, 452412599, 657167265




), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"d_711794630"[aa]%!in%d_711794630_a)

            d_711794630_invalid_cross_a = addNA(connectData$"d_711794630"[QCcheck1])

            QCcheck2 =which(connectData$"d_711794630"[bb]%!in%d_711794630_b)

            d_711794630_invalid_cross_b = addNA(connectData$"d_711794630"[QCcheck2])

            df[144,1]<-paste0("d_711794630_d_827220437_crossinvalid_values")

            df[144,2]<-paste0(d_711794630_b, collapse=", ")

            df[144,3]<-paste0(d_711794630_invalid_cross_a, collapse=", ")

            df[144,4]<-paste0(d_711794630_invalid_cross_b, collapse=", ")

######## QC d_679832994
# valid value check
d_679832994= c(NA)
QCcheck1 =which(connectData$"d_679832994"%!in%d_679832994)
d_679832994_invalid = addNA(connectData$"d_679832994")[QCcheck1]
df[145,1]<-paste0("d_679832994_invalid")
df[145,2]<-paste0("NA")
df[145,3]<-paste0(d_679832994_invalid, collapse=", ")
######## QC d_559534463
# valid value check
d_559534463= c(NA)
QCcheck1 =which(connectData$"d_559534463"%!in%d_559534463)
d_559534463_invalid = addNA(connectData$"d_559534463")[QCcheck1]
df[146,1]<-paste0("d_559534463_invalid")
df[146,2]<-paste0("NA")
df[146,3]<-paste0(d_559534463_invalid, collapse=", ")
######## QC d_570452130
# valid value check
d_570452130= c(539025306, 427405444)
QCcheck1 =which(connectData$"d_570452130"%!in%d_570452130)
d_570452130_invalid = addNA(connectData$"d_570452130")[QCcheck1]
df[147,1]<-paste0("d_570452130_invalid")
df[147,2]<-paste0("539025306, 427405444")
df[147,3]<-paste0(d_570452130_invalid, collapse=", ")
######## QC d_629484663
# valid value check
d_629484663= c(539025306, 427405444)
QCcheck1 =which(connectData$"d_629484663"%!in%d_629484663)
d_629484663_invalid = addNA(connectData$"d_629484663")[QCcheck1]
df[148,1]<-paste0("d_629484663_invalid")
df[148,2]<-paste0("539025306, 427405444")
df[148,3]<-paste0(d_629484663_invalid, collapse=", ")
######## QC d_547895941
# valid value check
d_547895941= c(539025306, 427405444)
QCcheck1 =which(connectData$"d_547895941"%!in%d_547895941)
d_547895941_invalid = addNA(connectData$"d_547895941")[QCcheck1]
df[149,1]<-paste0("d_547895941_invalid")
df[149,2]<-paste0("539025306, 427405444")
df[149,3]<-paste0(d_547895941_invalid, collapse=", ")
######## QC token
# cross valid value check

            token_a = c(NA)

            token_b = c(!NA)

            mylist_a1 =  paste0(rep("connectData$512820379 == "), c(486306140, 854703046), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$512820379 != "), c(486306140, 854703046), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"token"[aa]%!in%token_a)

            token_invalid_cross_a = addNA(connectData$"token"[QCcheck1])

            QCcheck2 =which(connectData$"token"[bb]%!in%token_b)

            token_invalid_cross_b = addNA(connectData$"token"[QCcheck2])

            df[150,1]<-paste0("token_512820379_crossinvalid_values")

            df[150,2]<-paste0(token_b, collapse=", ")

            df[150,3]<-paste0(token_invalid_cross_a, collapse=", ")

            df[150,4]<-paste0(token_invalid_cross_b, collapse=", ")

######## QC pin
# cross valid value check

            pin_a = c(NA)

            pin_b = c(!NA)

            mylist_a1 =  paste0(rep("connectData$512820379 == "), c(486306141), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$512820379 != "), c(486306141), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"pin"[aa]%!in%pin_a)

            pin_invalid_cross_a = addNA(connectData$"pin"[QCcheck1])

            QCcheck2 =which(connectData$"pin"[bb]%!in%pin_b)

            pin_invalid_cross_b = addNA(connectData$"pin"[QCcheck2])

            df[151,1]<-paste0("pin_512820379_crossinvalid_values")

            df[151,2]<-paste0(pin_b, collapse=", ")

            df[151,3]<-paste0(pin_invalid_cross_a, collapse=", ")

            df[151,4]<-paste0(pin_invalid_cross_b, collapse=", ")

######## QC pin
# cross valid value check

            pin_a = c(NA)

            pin_b = c(NA)

            mylist_a1 =  paste0(rep("connectData$512820379 == "), c(854703046), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$512820379 != "), c(854703046), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"pin"[aa]%!in%pin_a)

            pin_invalid_cross_a = addNA(connectData$"pin"[QCcheck1])

            QCcheck2 =which(connectData$"pin"[bb]%!in%pin_b)

            pin_invalid_cross_b = addNA(connectData$"pin"[QCcheck2])

            df[152,1]<-paste0("pin_512820379_crossinvalid_values")

            df[152,2]<-paste0(pin_b, collapse=", ")

            df[152,3]<-paste0(pin_invalid_cross_a, collapse=", ")

            df[152,4]<-paste0(pin_invalid_cross_b, collapse=", ")

######## QC studyId
# cross valid value check

            studyId_a = c(NA)

            studyId_b = c(!NA)

            mylist_a1 =  paste0(rep("connectData$512820379 == "), c(854703046), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$512820379 != "), c(854703046), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"studyId"[aa]%!in%studyId_a)

            studyId_invalid_cross_a = addNA(connectData$"studyId"[QCcheck1])

            QCcheck2 =which(connectData$"studyId"[bb]%!in%studyId_b)

            studyId_invalid_cross_b = addNA(connectData$"studyId"[QCcheck2])

            df[153,1]<-paste0("studyId_512820379_crossinvalid_values")

            df[153,2]<-paste0(studyId_b, collapse=", ")

            df[153,3]<-paste0(studyId_invalid_cross_a, collapse=", ")

            df[153,4]<-paste0(studyId_invalid_cross_b, collapse=", ")

######## QC Connect_ID
# cross valid value check

            Connect_ID_a = c(NA)

            Connect_ID_b = c(!NA)

            mylist_a1 =  paste0(rep("connectData$919254129 == "), c(353358909), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$919254129 != "), c(353358909), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"Connect_ID"[aa]%!in%Connect_ID_a)

            Connect_ID_invalid_cross_a = addNA(connectData$"Connect_ID"[QCcheck1])

            QCcheck2 =which(connectData$"Connect_ID"[bb]%!in%Connect_ID_b)

            Connect_ID_invalid_cross_b = addNA(connectData$"Connect_ID"[QCcheck2])

            df[154,1]<-paste0("Connect_ID_919254129_crossinvalid_values")

            df[154,2]<-paste0(Connect_ID_b, collapse=", ")

            df[154,3]<-paste0(Connect_ID_invalid_cross_a, collapse=", ")

            df[154,4]<-paste0(Connect_ID_invalid_cross_b, collapse=", ")

######## QC 512820379
# cross valid value check

            512820379_a = c(180583933, 486306141)

            512820379_b = c()

            mylist_a1 =  paste0(rep("connectData$token == "), c(!NA), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$token != "), c(!NA), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"512820379"[aa]%!in%512820379_a)

            512820379_invalid_cross_a = addNA(connectData$"512820379"[QCcheck1])

            QCcheck2 =which(connectData$"512820379"[bb]%!in%512820379_b)

            512820379_invalid_cross_b = addNA(connectData$"512820379"[QCcheck2])

            df[155,1]<-paste0("512820379_token_crossinvalid_values")

            df[155,2]<-paste0(512820379_b, collapse=", ")

            df[155,3]<-paste0(512820379_invalid_cross_a, collapse=", ")

            df[155,4]<-paste0(512820379_invalid_cross_b, collapse=", ")

######## QC pin
# cross valid value check

            pin_a = c(NA)

            pin_b = c()

            mylist_a1 =  paste0(rep("connectData$studyID == "), c(!NA), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$studyID != "), c(!NA), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"pin"[aa]%!in%pin_a)

            pin_invalid_cross_a = addNA(connectData$"pin"[QCcheck1])

            QCcheck2 =which(connectData$"pin"[bb]%!in%pin_b)

            pin_invalid_cross_b = addNA(connectData$"pin"[QCcheck2])

            df[156,1]<-paste0("pin_studyID_crossinvalid_values")

            df[156,2]<-paste0(pin_b, collapse=", ")

            df[156,3]<-paste0(pin_invalid_cross_a, collapse=", ")

            df[156,4]<-paste0(pin_invalid_cross_b, collapse=", ")

######## QC 447051482
# valid numeric length check
valid_length= 
            
var.is.integer =suppressWarnings(testInteger(connectData$"447051482"))
variable =connectData$"447051482"
            
list_lengths = unique(sapply(variable ,nchar))
            
447051482_invalid_num_length = list_lengths[list_lengths > valid_length]
            
df[159,1]<-paste0("447051482_invalid_char_length")
            
df[159,2]<-paste0("invalid length(s) found:",447051482_invalid_num_length,"non integer value(s) found:",!var.is.integer, collapse=", ")
######## QC 311580100
# cross valid value check

            311580100_a = c(104430631)

            311580100_b = c(353358909)

            mylist_a1 =  paste0(rep("connectData$447051482 == "), c(!NA), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$447051482 != "), c(!NA), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"311580100"[aa]%!in%311580100_a)

            311580100_invalid_cross_a = addNA(connectData$"311580100"[QCcheck1])

            QCcheck2 =which(connectData$"311580100"[bb]%!in%311580100_b)

            311580100_invalid_cross_b = addNA(connectData$"311580100"[QCcheck2])

            df[160,1]<-paste0("311580100_447051482_crossinvalid_values")

            df[160,2]<-paste0(311580100_b, collapse=", ")

            df[160,3]<-paste0(311580100_invalid_cross_a, collapse=", ")

            df[160,4]<-paste0(311580100_invalid_cross_b, collapse=", ")

######## QC 914639140
# cross valid value check

            914639140_a = c(104430631)

            914639140_b = c(353358909)

            mylist_a1 =  paste0(rep("connectData$920333151 == "), c(!NA), sep =" || ")

            mylist_a2 = str_c(mylist_a1, sep = "", collapse ="") # make many or statements

            mylist_a3 = str_sub(mylist_a2, end =-5) #remove extra " ||" at the end of string

            aa = which(eval(parse(text=mylist_a3))) # remove quotes to make logical expression

            mylist_b1 =  paste0(rep("connectData$920333151 != "), c(!NA), sep =" || ")

            mylist_b2 = str_c(mylist_b1, sep = "", collapse ="") # make many or statements

            mylist_b3 = str_sub(mylist_b2, end =-5) #remove extra " ||" at the end of string

            bb = which(eval(parse(text=mylist_b3))) # remove quotes to make logical expression

            QCcheck1 =which(connectData$"914639140"[aa]%!in%914639140_a)

            914639140_invalid_cross_a = addNA(connectData$"914639140"[QCcheck1])

            QCcheck2 =which(connectData$"914639140"[bb]%!in%914639140_b)

            914639140_invalid_cross_b = addNA(connectData$"914639140"[QCcheck2])

            df[163,1]<-paste0("914639140_920333151_crossinvalid_values")

            df[163,2]<-paste0(914639140_b, collapse=", ")

            df[163,3]<-paste0(914639140_invalid_cross_a, collapse=", ")

            df[163,4]<-paste0(914639140_invalid_cross_b, collapse=", ")

######## QC 463625040
# valid numeric length check
valid_length= 
            
var.is.integer =suppressWarnings(testInteger(connectData$"463625040"))
variable =connectData$"463625040"
            
list_lengths = unique(sapply(variable ,nchar))
            
463625040_invalid_num_length = list_lengths[list_lengths > valid_length]
            
df[165,1]<-paste0("463625040_invalid_char_length")
            
df[165,2]<-paste0("invalid length(s) found:",463625040_invalid_num_length,"non integer value(s) found:",!var.is.integer, collapse=", ")
######## QC 512820379 (remove recruitment type default rule?)
# valid value check
512820379 (remove recruitment type default rule?)= c()
QCcheck1 =which(connectData$"512820379 (remove recruitment type default rule?)"%!in%512820379 (remove recruitment type default rule?))
512820379 (remove recruitment type default rule?)_invalid = addNA(connectData$"512820379 (remove recruitment type default rule?)")[QCcheck1]
df[166,1]<-paste0("512820379 (remove recruitment type default rule?)_invalid")
df[166,2]<-paste0("")
df[166,3]<-paste0(512820379 (remove recruitment type default rule?)_invalid, collapse=", ")
######## filter df to show QC errors
qc_errors = filter(df, (!is.na(df$"invalid values in CID1") |!is.na(df$"invalid values in CID2")))
qc_script = filter(qc_script, (qc_script$"invalid values in CID1" != "" | qc_script$"invalid values in CID2" != ""))
write.csv(qc_script,"qc_recruitment_errors_04_05_2021_Sanford.csv")

######## SAVE QC SCRIPT TO BOXFOLDER (123) 
box_auth()
box_auth(client_id = "xoxo" , client_secret = "xoxo")
box_write(qc_errors, paste0("qc_report_",gsub("-","",Sys.Date()),".csv"),dir_id =134691197438)
