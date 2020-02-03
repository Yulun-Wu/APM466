install.packages("jrvFinance")
library("jrvFinance")
close_price_date <- c("2020-01-02", "2020-01-03", "2020-01-06", "2020-01-07" ,"2020-01-08","2020-01-09","2020-01-10","2020-01-13","2020-01-14","2020-01-15")
bond_selected_one <- read.csv(file.choose(),header = T) # read one bond data from csv files
bond<- as.matrix(bond_selected_one) # store the bond data in a matrix
bond_coupon <- as.numeric(bond[1])/100 #get bond coupon 
bond_maturity_date <- bond[2] #get the bond maturity date
bond_close_price_data <- rev(bond[3:12]) #store bond 10 days close prices in a vector,starting from 2020-01-02
ytm <- vector('numeric',length =10) # a vector to store 10 days YTM
for(i in c(1:10)){
  now_close_price_date <- close_price_date[i]
  now_close_price <- as.numeric(bond_close_price_data[i])
  ytm[i] <- bond.yield(now_close_price_date, bond_maturity_date, coupon = bond_coupon, freq=2, now_close_price, convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E"), comp.freq = Inf, redemption_value = 100)
}
ytm

### ALL YTM for 10 selected bonds for 10 days(colunmns)
###                   
CAN_1.5_Mar_1_2020 <- c(0.02406730, 0.02360272, 0.02407196, 0.02424001, 0.02441442, 0.02459557, 0.02478384, 0.02539588, 0.02561731, 0.02584839)
CAN_3.5_2020_06_01 <- c(0.01790792, 0.01779307, 0.01768674, 0.01756730, 0.01769712, 0.01808147, 0.01821677, 0.01785452, 0.01799212, 0.01786803)
CAN_0.75_2020_09_01 <-c(0.01867487, 0.01841697, 0.01855629, 0.01875854, 0.01865118, 0.01869923, 0.01874771, 0.01905478, 0.01894590, 0.01867563)
CAN_0.75_2021_03_01 <-c(0.01709712, 0.01677094, 0.01666209, 0.01677226, 0.01697130, 0.01699425, 0.01737237, 0.01726601, 0.01728997, 0.01704453)
CAN_0.75_2021_09_01 <-c(0.01715870, 0.01692910, 0.01672966, 0.01693055, 0.01694640, 0.01714840, 0.01716467, 0.01752619, 0.01735545, 0.01730954)
CAN_0.5_2022_03_01 <-c(0.01640878, 0.01613774, 0.01603737, 0.01609939, 0.01616158, 0.01636784, 0.01633449, 0.01657150, 0.01653823, 0.01640818)
CAN_1.75_2023_03_01 <-c(0.01642020, 0.01606464, 0.01586739, 0.01596306, 0.01599419, 0.01628463, 0.01641349, 0.01654117, 0.01641012, 0.01618115)
CAN_2.25_2024_03_01 <-c(0.01614895, 0.01582810, 0.01557167, 0.01598249, 0.01573399, 0.01609676, 0.01623981, 0.01625265, 0.01605218,0.01580251)
CAN_1.5_2024_09_01 <-c(0.01779154, 0.01727821, 0.01652193, 0.01692530, 0.01661320, 0.01719664, 0.01710820, 0.01704460, 0.01720326, 0.01695711)
CAN_1.25_2025_03_01 <-c(0.01599983, 0.01561627, 0.01531702, 0.01552139, 0.01552300, 0.01589068, 0.01599435, 0.01602035, 0.01581811, 0.01555471)


total_YTM <- rbind(CAN_1.5_Mar_1_2020, CAN_3.5_2020_06_01, CAN_0.75_2020_09_01, CAN_0.75_2021_03_01, 
                   CAN_0.75_2021_09_01, CAN_0.5_2022_03_01, CAN_1.75_2023_03_01, 
                   CAN_2.25_2024_03_01, CAN_1.5_2024_09_01, CAN_1.25_2025_03_01)

### To get yield curve for 5 years, we have to estimate the YTM for the bonds matures 
### on September 1 2022 and September 1 2023 by using the data we get from above and linear interpolation.
### first, we compute year fracs for each trading day to March 1 2022, September 1 2022, March 1 2023, September 1 2023 and March 1 2024.

future_dates <- c('2022-03-01','2022-09-01','2023-03-01','2023-09-01','2024-03-01')

Jan02 <-vector('numeric',length = 5)
Jan03 <-vector('numeric',length = 5)
Jan06 <-vector('numeric',length = 5)
Jan07 <-vector('numeric',length = 5)
Jan08 <-vector('numeric',length = 5)
Jan09 <-vector('numeric',length = 5)
Jan10 <-vector('numeric',length = 5)
Jan13 <-vector('numeric',length = 5)
Jan14 <-vector('numeric',length = 5)
Jan15 <-vector('numeric',length = 5)

CAN_2022_09_01 <-vector('numeric',length = 10)
CAN_2023_09_01 <-vector('numeric',length = 10)

#for 2020-01-02
for(i in c(1:5)){
  Jan02[i] <- yearFraction('2020-01-02',future_dates[i])
}
total_dis_Between_2022_and_2023_1 <- 1/(Jan02[3]-Jan02[1])
total_dis_Between_2023_and_2024_1 <- 1/(Jan02[5]-Jan02[3])
first_frac_1 <- Jan02[2]-Jan02[1]
second_frac_1 <-Jan02[4]-Jan02[3]
CAN_2022_09_01[1] <- CAN_0.5_2022_03_01[1] + (CAN_1.75_2023_03_01[1]-CAN_0.5_2022_03_01[1])*total_dis_Between_2022_and_2023_1 * first_frac_1
CAN_2023_09_01[1] <- CAN_1.75_2023_03_01[1]+ (CAN_2.25_2024_03_01[1]-CAN_1.75_2023_03_01[1])*total_dis_Between_2023_and_2024_1 *second_frac_1

  
#for 2020-01-03
for(i in c(1:5)){
  Jan03[i] <- yearFraction('2020-01-03',future_dates[i])
}
total_dis_Between_2022_and_2023_2 <- 1/(Jan03[3]-Jan03[1])
total_dis_Between_2023_and_2024_2 <- 1/(Jan03[5]-Jan03[3])
first_frac_2 <- Jan03[2]-Jan03[1]
second_frac_2 <-Jan03[4]-Jan03[3]
CAN_2022_09_01[2] <- CAN_0.5_2022_03_01[2] + (CAN_1.75_2023_03_01[2]-CAN_0.5_2022_03_01[2])*total_dis_Between_2022_and_2023_2 * first_frac_2
CAN_2023_09_01[2] <- CAN_1.75_2023_03_01[2]+ (CAN_2.25_2024_03_01[2]-CAN_1.75_2023_03_01[2])*total_dis_Between_2023_and_2024_2 *second_frac_2


#for 2020-01-06
for(i in c(1:5)){
  Jan06[i] <- yearFraction('2020-01-06',future_dates[i])
}
total_dis_Between_2022_and_2023_3 <- 1/(Jan06[3]-Jan06[1])
total_dis_Between_2023_and_2024_3 <- 1/(Jan06[5]-Jan06[3])
first_frac_3 <- Jan06[2]-Jan06[1]
second_frac_3 <-Jan06[4]-Jan06[3]
CAN_2022_09_01[3] <- CAN_0.5_2022_03_01[3] + (CAN_1.75_2023_03_01[3]-CAN_0.5_2022_03_01[3])*total_dis_Between_2022_and_2023_3 * first_frac_3
CAN_2023_09_01[3] <- CAN_1.75_2023_03_01[3]+ (CAN_2.25_2024_03_01[3]-CAN_1.75_2023_03_01[3])*total_dis_Between_2023_and_2024_3 *second_frac_3

#for 2020-01-07
for(i in c(1:5)){
  Jan07[i] <- yearFraction('2020-01-07',future_dates[i])
}
total_dis_Between_2022_and_2023_4 <- 1/(Jan07[3]-Jan07[1])
total_dis_Between_2023_and_2024_4 <- 1/(Jan07[5]-Jan07[3])
first_frac_4 <- Jan07[2]-Jan07[1]
second_frac_4 <-Jan07[4]-Jan07[3]
CAN_2022_09_01[4] <- CAN_0.5_2022_03_01[4] + (CAN_1.75_2023_03_01[4]-CAN_0.5_2022_03_01[4])*total_dis_Between_2022_and_2023_4 * first_frac_4
CAN_2023_09_01[4] <- CAN_1.75_2023_03_01[4]+ (CAN_2.25_2024_03_01[4]-CAN_1.75_2023_03_01[4])*total_dis_Between_2023_and_2024_4 *second_frac_4

#for 2020-01-08
for(i in c(1:5)){
  Jan08[i] <- yearFraction('2020-01-08',future_dates[i])
}
total_dis_Between_2022_and_2023_5 <- 1/(Jan08[3]-Jan08[1])
total_dis_Between_2023_and_2024_5 <- 1/(Jan08[5]-Jan08[3])
first_frac_5 <- Jan08[2]-Jan08[1]
second_frac_5 <-Jan08[4]-Jan08[3]
CAN_2022_09_01[5] <- CAN_0.5_2022_03_01[5] + (CAN_1.75_2023_03_01[5]-CAN_0.5_2022_03_01[5])*total_dis_Between_2022_and_2023_5 * first_frac_5
CAN_2023_09_01[5] <- CAN_1.75_2023_03_01[5]+ (CAN_2.25_2024_03_01[5]-CAN_1.75_2023_03_01[5])*total_dis_Between_2023_and_2024_5 *second_frac_5

#for 2020-01-09
for(i in c(1:5)){
  Jan09[i] <- yearFraction('2020-01-09',future_dates[i])
}
total_dis_Between_2022_and_2023_6 <- 1/(Jan09[3]-Jan09[1])
total_dis_Between_2023_and_2024_6 <- 1/(Jan09[5]-Jan09[3])
first_frac_6 <- Jan09[2]-Jan09[1]
second_frac_6 <-Jan09[4]-Jan09[3]
CAN_2022_09_01[6] <- CAN_0.5_2022_03_01[6] + (CAN_1.75_2023_03_01[6]-CAN_0.5_2022_03_01[6])*total_dis_Between_2022_and_2023_6 * first_frac_6
CAN_2023_09_01[6] <- CAN_1.75_2023_03_01[6]+ (CAN_2.25_2024_03_01[6]-CAN_1.75_2023_03_01[6])*total_dis_Between_2023_and_2024_6 *second_frac_6

#for 2020-01-10
for(i in c(1:5)){
  Jan10[i] <- yearFraction('2020-01-10',future_dates[i])
}
total_dis_Between_2022_and_2023_7 <- 1/(Jan10[3]-Jan10[1])
total_dis_Between_2023_and_2024_7 <- 1/(Jan10[5]-Jan10[3])
first_frac_7 <- Jan10[2]-Jan10[1]
second_frac_7 <-Jan10[4]-Jan10[3]
CAN_2022_09_01[7] <- CAN_0.5_2022_03_01[7] + (CAN_1.75_2023_03_01[7]-CAN_0.5_2022_03_01[7])*total_dis_Between_2022_and_2023_7 * first_frac_7
CAN_2023_09_01[7] <- CAN_1.75_2023_03_01[7]+ (CAN_2.25_2024_03_01[7]-CAN_1.75_2023_03_01[7])*total_dis_Between_2023_and_2024_7 *second_frac_7

#for 2020-01-13
for(i in c(1:5)){
  Jan13[i] <- yearFraction('2020-01-13',future_dates[i])
}
total_dis_Between_2022_and_2023_8 <- 1/(Jan13[3]-Jan13[1])
total_dis_Between_2023_and_2024_8 <- 1/(Jan13[5]-Jan13[3])
first_frac_8 <- Jan13[2]-Jan13[1]
second_frac_8 <-Jan13[4]-Jan13[3]
CAN_2022_09_01[8] <- CAN_0.5_2022_03_01[8] + (CAN_1.75_2023_03_01[8]-CAN_0.5_2022_03_01[8])*total_dis_Between_2022_and_2023_8 * first_frac_8
CAN_2023_09_01[8] <- CAN_1.75_2023_03_01[8]+ (CAN_2.25_2024_03_01[8]-CAN_1.75_2023_03_01[8])*total_dis_Between_2023_and_2024_8 *second_frac_8

#for 2020-01-14
for(i in c(1:5)){
  Jan14[i] <- yearFraction('2020-01-14',future_dates[i])
}
total_dis_Between_2022_and_2023_9 <- 1/(Jan14[3]-Jan14[1])
total_dis_Between_2023_and_2024_9 <- 1/(Jan14[5]-Jan14[3])
first_frac_9 <- Jan14[2]-Jan14[1]
second_frac_9 <-Jan14[4]-Jan14[3]
CAN_2022_09_01[9] <- CAN_0.5_2022_03_01[9] + (CAN_1.75_2023_03_01[9]-CAN_0.5_2022_03_01[9])*total_dis_Between_2022_and_2023_9 * first_frac_9
CAN_2023_09_01[9] <- CAN_1.75_2023_03_01[9]+ (CAN_2.25_2024_03_01[9]-CAN_1.75_2023_03_01[9])*total_dis_Between_2023_and_2024_9 *second_frac_9

#for 2020-01-15
for(i in c(1:5)){
  Jan15[i] <- yearFraction('2020-01-15',future_dates[i])
}
total_dis_Between_2022_and_2023_10 <- 1/(Jan15[3]-Jan15[1])
total_dis_Between_2023_and_2024_10 <- 1/(Jan15[5]-Jan15[3])
first_frac_10 <- Jan15[2]-Jan15[1]
second_frac_10 <-Jan15[4]-Jan15[3]
CAN_2022_09_01[10] <- CAN_0.5_2022_03_01[10] + (CAN_1.75_2023_03_01[10]-CAN_0.5_2022_03_01[10])*total_dis_Between_2022_and_2023_10 * first_frac_10
CAN_2023_09_01[10] <- CAN_1.75_2023_03_01[10]+ (CAN_2.25_2024_03_01[10]-CAN_1.75_2023_03_01[10])*total_dis_Between_2023_and_2024_10 *second_frac_10



### YTM for 5 years  each bond for 10 days 
CAN_1.5_Mar_1_2020 <- c(0.02406730, 0.02360272, 0.02407196, 0.02424001, 0.02441442, 0.02459557, 0.02478384, 0.02539588, 0.02561731, 0.02584839)
CAN_0.75_2020_09_01 <-c(0.01867487, 0.01841697, 0.01855629, 0.01875854, 0.01865118, 0.01869923, 0.01874771, 0.01905478, 0.01894590, 0.01867563)
CAN_0.75_2021_03_01 <-c(0.01709712, 0.01677094, 0.01666209, 0.01677226, 0.01697130, 0.01699425, 0.01737237, 0.01726601, 0.01728997, 0.01704453)
CAN_0.75_2021_09_01 <-c(0.01715870, 0.01692910, 0.01672966, 0.01693055, 0.01694640, 0.01714840, 0.01716467, 0.01752619, 0.01735545, 0.01730954)
CAN_0.5_2022_03_01 <-c(0.01640878, 0.01613774, 0.01603737, 0.01609939, 0.01616158, 0.01636784, 0.01633449, 0.01657150, 0.01653823, 0.01640818)
CAN_2022_09_01 <-c(0.01641449, 0.01610119, 0.01595238, 0.01603123, 0.01607789, 0.01632624, 0.01637399, 0.01655633, 0.01647418, 0.01629466)
CAN_1.75_2023_03_01 <-c(0.01642020, 0.01606464, 0.01586739, 0.01596306, 0.01599419, 0.01628463, 0.01641349, 0.01654117, 0.01641012, 0.01618115)
CAN_2023_09_01 <- c( 0.01628457, 0.01594637, 0.01571953, 0.01597277, 0.01586409, 0.01619070, 0.01632665, 0.01639691, 0.01623115, 0.01599183)
CAN_2.25_2024_03_01 <-c(0.01614895, 0.01582810, 0.01557167, 0.01598249, 0.01573399, 0.01609676, 0.01623981, 0.01625265, 0.01605218,0.01580251)
CAN_1.5_2024_09_01 <-c(0.01779154, 0.01727821, 0.01652193, 0.01692530, 0.01661320, 0.01719664, 0.01710820, 0.01704460, 0.01720326, 0.01695711)
CAN_1.25_2025_03_01 <-c(0.01599983, 0.01561627, 0.01531702, 0.01552139, 0.01552300, 0.01589068, 0.01599435, 0.01602035, 0.01581811, 0.01555471)

YTM_for_5_years <-rbind(CAN_1.5_Mar_1_2020, CAN_0.75_2020_09_01, CAN_0.75_2021_03_01, 
                        CAN_0.75_2021_09_01, CAN_0.5_2022_03_01,CAN_2022_09_01, CAN_1.75_2023_03_01,CAN_2023_09_01, 
                        CAN_2.25_2024_03_01, CAN_1.5_2024_09_01, CAN_1.25_2025_03_01)

YTM_of_10Bonds_at_Jan_02 <- c(0.02406730, 0.01867487, 0.01709712,0.01715870, 0.01640878, 0.01641449, 0.01642020, 0.01628457, 0.01614895, 0.01779154, 0.01599983)
YTM_of_10Bonds_at_Jan_03 <- c(0.02360272, 0.01841697, 0.01677094, 0.01692910, 0.01613774, 0.01610119, 0.01606464, 0.01594637, 0.01582810, 0.01727821, 0.01561627)
YTM_of_10Bonds_at_Jan_06 <- c(0.02407196, 0.01855629, 0.01666209, 0.01672966, 0.01603737, 0.01595238, 0.01586739, 0.01571953, 0.01557167, 0.01652193, 0.01531702)
YTM_of_10Bonds_at_Jan_07 <- c(0.02424001,0.01875854, 0.01677226, 0.01693055, 0.01609939, 0.01603123, 0.01596306, 0.01597277, 0.01598249, 0.01692530,0.01552139)
YTM_of_10Bonds_at_Jan_08 <- c(0.02441442, 0.01865118, 0.01697130, 0.01694640, 0.01616158, 0.01607789, 0.01599419, 0.01586409, 0.01573399, 0.01661320, 0.01552300)
YTM_of_10Bonds_at_Jan_09 <- c(0.02459557, 0.01869923, 0.01699425, 0.01714840, 0.01636784, 0.01632624, 0.01628463, 0.01619070, 0.01609676, 0.01719664, 0.01589068)
YTM_of_10Bonds_at_Jan_10 <- c(0.02478384, 0.01874771, 0.01737237, 0.01716467, 0.01633449, 0.01637399, 0.01641349, 0.01632665, 0.01623981, 0.01710820, 0.01599435)
YTM_of_10Bonds_at_Jan_13 <- c(0.02539588, 0.01905478, 0.01726601, 0.01752619, 0.01657150, 0.01655633, 0.01654117, 0.01639691, 0.01625265, 0.01704460, 0.01602035)
YTM_of_10Bonds_at_Jan_14 <- c(0.02561731, 0.01894590, 0.01728997, 0.01735545, 0.01653823, 0.01647418, 0.01641012, 0.01623115, 0.01605218,0.01720326,0.01581811)
YTM_of_10Bonds_at_Jan_15 <- c(0.02584839, 0.01867563, 0.01704453, 0.01730954, 0.01640818, 0.01629466, 0.01618115,0.01599183, 0.01580251,0.01695711,0.01555471)


future_dates_1 <- c('2020-03-01','2020-09-1','2021-03-01','2021-09-01','2022-03-01','2022-09-01','2023-03-01','2023-09-01','2024-03-01','2024-09-01','2025-03-01')
year_frac_for_Jan_02  <- vector('numeric',length = 11) #create a empty vector with length 11
for(i in c(1:11)){
  year_frac_for_Jan_02[i] <- yearFraction('2020-01-02',future_dates_1[i])
}
year_frac_for_Jan_02 # get the year fraction from 2020_01-02 to each future dates

year_frac_for_Jan_02 <-c(0.1638889, 0.6638889,1.1638889, 1.6638889, 2.1638889, 2.6638889, 3.1638889, 3.6638889, 4.1638889, 4.6638889, 5.1638889)
year_frac_for_Jan_03 <-c(0.1611111, 0.6611111, 1.1611111, 1.6611111, 2.1611111, 2.6611111, 3.1611111, 3.6611111, 4.1611111, 4.6611111, 5.1611111)
year_frac_for_Jan_06 <-c(0.1527778, 0.6527778, 1.1527778, 1.6527778, 2.1527778, 2.6527778, 3.1527778, 3.6527778, 4.1527778, 4.6527778, 5.1527778)
year_frac_for_Jan_07 <-c(0.15,0.65, 1.15, 1.65, 2.15, 2.65, 3.15, 3.65, 4.15, 4.65, 5.15)
year_frac_for_Jan_08 <-c(0.1472222, 0.6472222, 1.1472222, 1.6472222, 2.1472222, 2.6472222, 3.1472222, 3.6472222, 4.1472222, 4.6472222, 5.1472222)
year_frac_for_Jan_09 <-c(0.1444444, 0.6444444, 1.1444444, 1.6444444, 2.1444444, 2.6444444, 3.1444444, 3.6444444, 4.1444444, 4.6444444, 5.1444444)
year_frac_for_Jan_10 <-c(0.1416667, 0.6416667, 1.1416667, 1.6416667, 2.1416667, 2.6416667, 3.1416667, 3.6416667, 4.1416667, 4.6416667, 5.1416667)
year_frac_for_Jan_13 <-c(0.1333333, 0.6333333, 1.1333333, 1.6333333, 2.1333333, 2.6333333, 3.1333333, 3.6333333, 4.1333333, 4.6333333, 5.1333333)
year_frac_for_Jan_14 <-c(0.1305556, 0.6305556, 1.1305556, 1.6305556, 2.1305556, 2.6305556, 3.1305556, 3.6305556, 4.1305556, 4.6305556, 5.1305556)
year_frac_for_Jan_15 <-c(0.1277778, 0.6277778, 1.1277778, 1.6277778, 2.1277778, 2.6277778, 3.1277778, 3.6277778, 4.1277778, 4.6277778, 5.1277778)



plot(year_frac_for_Jan_02,YTM_of_10Bonds_at_Jan_02,col=1,type="l",xlab = "Years",ylab ="Yield to maturity")
lines(year_frac_for_Jan_03,YTM_of_10Bonds_at_Jan_03,col=2,type="l")
lines(year_frac_for_Jan_06,YTM_of_10Bonds_at_Jan_06,col=3,type="l")
lines(year_frac_for_Jan_07,YTM_of_10Bonds_at_Jan_07,col=4,type="l")
lines(year_frac_for_Jan_08,YTM_of_10Bonds_at_Jan_08,col=5,type="l")
lines(year_frac_for_Jan_09,YTM_of_10Bonds_at_Jan_09,col=6,type="l")
lines(year_frac_for_Jan_10,YTM_of_10Bonds_at_Jan_10,col=7,type="l")
lines(year_frac_for_Jan_13,YTM_of_10Bonds_at_Jan_13,col=8,type="l")
lines(year_frac_for_Jan_14,YTM_of_10Bonds_at_Jan_14,col=9,type="l")
lines(year_frac_for_Jan_15,YTM_of_10Bonds_at_Jan_15,col=10,type="l")
legend("topright",pch=c(15,15),legend=c("2020-01-02","2020-01-03",'2020-01-06','2020-01-07','2020-01-08','2020-01-09','2020-01-10','2020-01-13','2020-01-14','2020-01-15'),col=c(1,2,3,4,5,6,7,8,9,10),lty=1,cex=0.6)


### Spot rate

spot_rate_for_Jan_02 <-vector('numeric',length = 11)
spot_rate_for_Jan_03 <-vector('numeric',length = 11)
spot_rate_for_Jan_06 <-vector('numeric',length = 11)
spot_rate_for_Jan_07 <-vector('numeric',length = 11)
spot_rate_for_Jan_08 <-vector('numeric',length = 11)
spot_rate_for_Jan_09 <-vector('numeric',length = 11)
spot_rate_for_Jan_10 <-vector('numeric',length = 11)
spot_rate_for_Jan_13 <-vector('numeric',length = 11)
spot_rate_for_Jan_14 <-vector('numeric',length = 11)
spot_rate_for_Jan_15 <-vector('numeric',length = 11)







### "2020-01-06"
###At this date, all bond clean price as follows,
today <- "2020-01-15"
      ### Spot_rate_for_bond1
        dp_1 <- bond.TCF(today,'2020-03-01',0.015,freq = 2,redemption_value =100)$accrued + 99.86
        cf_1 <- bond.TCF(today,'2020-03-01',0.015,freq = 2,redemption_value =100)$cf
        year_fra_for_bond1 <- bond.TCF(today,'2020-03-01',0.015,freq = 2,redemption_value =100)$t
        spot_rate_for_Jan_15[1] <- log(cf_1[1]/dp_1)/year_fra_for_bond1[1]
        
        ### Spot_rate_for_bond2
        dp_2 <- bond.TCF(today,'2020-09-01',0.0075,freq = 2,redemption_value =100)$accrued + 99.30
        cf_2 <- bond.TCF(today,'2020-09-01',0.0075,freq = 2,redemption_value =100)$cf
        year_fra_for_bond2 <- bond.TCF(today,'2020-09-01',0.0075,freq = 2,redemption_value =100)$t
        differ_2 <- dp_2 - cf_2[1] * exp(-1*spot_rate_for_Jan_15[1] * year_fra_for_bond2[1])
        spot_rate_for_Jan_15[2] <- log(cf_2[2]/differ_2)/year_fra_for_bond2[2]
        
        
        ### Spot_rate_for_bond3
        dp_3 <- bond.TCF(today,'2021-03-01',0.0075,freq = 2,redemption_value =100)$accrued + 98.93
        cf_3 <- bond.TCF(today,'2021-03-01',0.0075,freq = 2,redemption_value =100)$cf
        year_fra_for_bond3 <- bond.TCF(today,'2021-03-01',0.0075,freq = 2,redemption_value =100)$t
        differ_3 <- dp_3 - cf_3[1] * exp(-1*spot_rate_for_Jan_15[1] * year_fra_for_bond3[1]) - cf_3[2] * exp(-1*spot_rate_for_Jan_15[2] * year_fra_for_bond3[2])
        spot_rate_for_Jan_15[3] <- log(cf_3[3]/differ_3)/year_fra_for_bond3[3]
        
        
        ### Spot_rate_for_bond4
        dp_4 <- bond.TCF(today,'2021-09-01',0.0075,freq = 2,redemption_value =100)$accrued + 98.42
        cf_4 <- bond.TCF(today,'2021-09-01',0.0075,freq = 2,redemption_value =100)$cf
        year_fra_for_bond4 <- bond.TCF(today,'2021-09-01',0.0075,freq = 2,redemption_value =100)$t
        differ_4 <- dp_4 - cf_4[1] * exp(-1*spot_rate_for_Jan_15[1] * year_fra_for_bond4[1]) - cf_4[2] * exp(-1*spot_rate_for_Jan_15[2] * year_fra_for_bond4[2]) -cf_4[3] * exp(-1*spot_rate_for_Jan_15[3]*year_fra_for_bond4[3])
        spot_rate_for_Jan_15[4] <- log(cf_4[4]/differ_4)/year_fra_for_bond4[4]
        
        
        ### Spot_rate_for_bond5
        dp_5 <- bond.TCF(today,'2022-03-01',0.005,freq = 2,redemption_value =100)$accrued + 97.61
        cf_5 <- bond.TCF(today,'2022-03-01',0.005,freq = 2,redemption_value =100)$cf
        year_fra_for_bond5 <- bond.TCF(today,'2022-03-01',0.005,freq = 2,redemption_value =100)$t
        differ_5 <- dp_5 - cf_5[1] * exp(-1*spot_rate_for_Jan_15[1] * year_fra_for_bond3[1]) - cf_5[2] * exp(-1*spot_rate_for_Jan_15[2] * year_fra_for_bond5[2]) -cf_5[3] * exp(-1*spot_rate_for_Jan_15[3]*year_fra_for_bond5[3]) - cf_5[4]*exp(-1*spot_rate_for_Jan_15[4]*year_fra_for_bond5[4])
        spot_rate_for_Jan_15[5] <- log(cf_5[5]/differ_5)/year_fra_for_bond5[5]
       
        
        
        ### estimate Spot_rate_for_bond matures on 2023-03-01
        dp_7 <- bond.TCF(today,'2023-03-01',0.0175,freq = 2,redemption_value =100)$accrued + 100.38
        cf_7 <- bond.TCF(today,'2023-03-01',0.0175,freq = 2,redemption_value =100)$cf
        year_fra_for_bond7 <- bond.TCF(today,'2023-03-01',0.0175,freq = 2,redemption_value =100)$t
        differ_7 <- dp_7 - cf_7[1] * exp(-1*spot_rate_for_Jan_15[1] * year_fra_for_bond7[1]) - cf_7[2] * exp(-1*spot_rate_for_Jan_15[2] * year_fra_for_bond7[2]) -cf_7[3] * exp(-1*spot_rate_for_Jan_15[3]*year_fra_for_bond7[3]) - cf_7[4]*exp(-1*spot_rate_for_Jan_15[4]*year_fra_for_bond7[4])-cf_7[5]*exp(-1*spot_rate_for_Jan_15[5]*year_fra_for_bond7[5]) - cf_7[6]*exp(-1*spot_rate_for_Jan_15[5]*year_fra_for_bond7[6])
        spot_rate_for_Jan_15[7] <- log(cf_7[7]/differ_7)/year_fra_for_bond7[7]
        
        
        
        ### estimate Spot_rate_for_bond matures on 2022-09-01 by liner interpolation
        differ_6 <- spot_rate_for_Jan_15[7] - spot_rate_for_Jan_15[5]
        total_differ_between_2022_03_01_and_2023_03_01 <-yearFraction('2022-03-01','2023-03-01')
        differ_between_2022_03_01_and_2022_09_01 <-yearFraction('2022-03-01','2022-09-01')
        spot_rate_for_Jan_15[6] <- spot_rate_for_Jan_15[5] + differ_between_2022_03_01_and_2022_09_01*differ_6/total_differ_between_2022_03_01_and_2023_03_01
        
        
        
        ### estimate Spot_rate_for_bond matures on 2024-03-01
        dp_9 <- bond.TCF(today,'2024-03-01',0.0225,freq = 2,redemption_value =100)$accrued + 102.64
        cf_9 <- bond.TCF(today,'2024-03-01',0.0225,freq = 2,redemption_value =100)$cf
        year_fra_for_bond9 <- bond.TCF(today,'2024-03-01',0.0225,freq = 2,redemption_value =100)$t
        differ_9 <- dp_9 - cf_9[1] * exp(-1*spot_rate_for_Jan_15[1] * year_fra_for_bond9[1]) - cf_9[2] * exp(-1*spot_rate_for_Jan_15[2] * year_fra_for_bond9[2]) -cf_9[3] * exp(-1*spot_rate_for_Jan_15[3]*year_fra_for_bond9[3]) - cf_9[4]*exp(-1*spot_rate_for_Jan_15[4]*year_fra_for_bond9[4])-cf_9[5]*exp(-1*spot_rate_for_Jan_15[5]*year_fra_for_bond9[5]) - cf_9[6]*exp(-1*spot_rate_for_Jan_15[6]*year_fra_for_bond9[6])-cf_9[7]*exp(-1*spot_rate_for_Jan_15[7]*year_fra_for_bond9[7])-cf_9[8]*exp(-1*spot_rate_for_Jan_15[7]*year_fra_for_bond9[8])
        spot_rate_for_Jan_15[9] <- log(cf_9[9]/differ_9)/year_fra_for_bond9[9]
        
       
        
        ### estimate Spot_rate_for_bond matures on 2023-09-01 by liner interpolation
        differ_8 <- spot_rate_for_Jan_15[9] - spot_rate_for_Jan_15[7]
        total_differ_between_2023_03_01_and_2024_03_01 <-yearFraction('2023-03-01','2024-03-01')
        differ_between_2023_03_01_and_2023_09_01 <-yearFraction('2023-03-01','2023-09-01')
        spot_rate_for_Jan_15[8] <- spot_rate_for_Jan_15[7] + differ_between_2023_03_01_and_2023_09_01*differ_8/total_differ_between_2023_03_01_and_2024_03_01
        
        
        ### estimate Spot_rate_for_bond matures on 2024-09-01
        dp_10 <- bond.TCF(today,'2024-09-01',0.015,freq = 2,redemption_value =100)$accrued + 99.10
        cf_10 <- bond.TCF(today,'2024-09-01',0.015,freq = 2,redemption_value =100)$cf
        year_fra_for_bond10 <- bond.TCF(today,'2024-09-01',0.015,freq = 2,redemption_value =100)$t
        differ_10 <- dp_10 - cf_10[1] * exp(-1*spot_rate_for_Jan_15[1] * year_fra_for_bond10[1]) - cf_10[2] * exp(-1*spot_rate_for_Jan_15[2] * year_fra_for_bond10[2]) -cf_10[3] * exp(-1*spot_rate_for_Jan_15[3]*year_fra_for_bond10[3]) - cf_10[4]*exp(-1*spot_rate_for_Jan_15[4]*year_fra_for_bond10[4])-cf_10[5]*exp(-1*spot_rate_for_Jan_15[5]*year_fra_for_bond10[5]) - cf_10[6]*exp(-1*spot_rate_for_Jan_15[6]*year_fra_for_bond10[6])-cf_10[7]*exp(-1*spot_rate_for_Jan_15[7]*year_fra_for_bond10[7])-cf_10[8]*exp(-1*spot_rate_for_Jan_15[8]*year_fra_for_bond10[8])-cf_10[9]*exp(-1*spot_rate_for_Jan_15[9]*year_fra_for_bond10[9])
        spot_rate_for_Jan_15[10] <- log(cf_10[10]/differ_10)/year_fra_for_bond10[10]
        
        
        ### estimate Spot_rate_for_bond matures on 2025-03-01
        dp_11 <- bond.TCF(today,'2025-03-01',0.0125,freq = 2,redemption_value =100)$accrued + 98.47
        cf_11 <- bond.TCF(today,'2025-03-01',0.0125,freq = 2,redemption_value =100)$cf
        year_fra_for_bond11 <- bond.TCF(today,'2025-03-01',0.0125,freq = 2,redemption_value =100)$t
        differ_11 <- dp_11 - cf_11[1] * exp(-1*spot_rate_for_Jan_15[1] * year_fra_for_bond11[1]) - cf_11[2] * exp(-1*spot_rate_for_Jan_15[2] * year_fra_for_bond11[2]) -cf_11[3] * exp(-1*spot_rate_for_Jan_15[3]*year_fra_for_bond11[3]) - cf_11[4]*exp(-1*spot_rate_for_Jan_15[4]*year_fra_for_bond11[4])-cf_11[5]*exp(-1*spot_rate_for_Jan_15[5]*year_fra_for_bond11[5]) - cf_11[6]*exp(-1*spot_rate_for_Jan_15[6]*year_fra_for_bond11[6])-cf_11[7]*exp(-1*spot_rate_for_Jan_15[7]*year_fra_for_bond11[7])-cf_11[8]*exp(-1*spot_rate_for_Jan_15[8]*year_fra_for_bond11[8])-cf_11[9]*exp(-1*spot_rate_for_Jan_15[9]*year_fra_for_bond11[9])-cf_11[10]*exp(-1*spot_rate_for_Jan_15[10]*year_fra_for_bond11[10])
        spot_rate_for_Jan_15[11] <- log(cf_11[11]/differ_11)/year_fra_for_bond11[11]
        spot_rate_for_Jan_15
        
        
        
        spot_rate_for_Jan_02 <-c(0.02401996, 0.01866990, 0.01709004, 0.01715400, 0.01640316, 0.01640506, 0.01640696, 0.01626585, 0.01612474, 0.01783075, 0.01597572)
        spot_rate_for_Jan_03 <-c(0.02355796, 0.01841225, 0.01676384, 0.01692484, 0.01613217, 0.01609085, 0.01604953, 0.01592670, 0.01580387, 0.01731103, 0.01559131)
        spot_rate_for_Jan_15 
        
        
spot_rate_for_Jan_02 <-c(0.02401996, 0.01866990, 0.01709004, 0.01715400, 0.01640316, 0.01640506, 0.01640696, 0.01626585, 0.01612474, 0.01783075, 0.01597572)
spot_rate_for_Jan_03 <-c(0.02355796, 0.01841225, 0.01676384, 0.01692484, 0.01613217, 0.01609085, 0.01604953, 0.01592670, 0.01580387, 0.01731103, 0.01559131)
spot_rate_for_Jan_06 <-c(0.02402781, 0.01855147, 0.01665435, 0.01672454, 0.01603180, 0.01594092, 0.01585004, 0.01569662, 0.01554319, 0.01653625, 0.01529194)
spot_rate_for_Jan_07 <-c(0.02419605, 0.01875381, 0.01676437, 0.01692574, 0.01609340, 0.01601944, 0.01594548, 0.01595615, 0.01596681, 0.01694606, 0.01549522)
spot_rate_for_Jan_08 <-c(0.02437065, 0.01864627, 0.01696413, 0.01694128, 0.01615563, 0.01606582, 0.01597600, 0.01584109, 0.01570619, 0.01662552, 0.01549986)
spot_rate_for_Jan_09 <-c(0.02455198, 0.01869429, 0.01698702, 0.01714403, 0.01636231, 0.01631586, 0.01626941, 0.01617198, 0.01607454, 0.01721913, 0.01586875)
spot_rate_for_Jan_10 <-c(0.02474043, 0.01874272, 0.01736600, 0.01715929, 0.01632824, 0.01636430, 0.01640037, 0.01631000, 0.01621962, 0.01712444, 0.01597347)
spot_rate_for_Jan_13 <-c(0.02535298, 0.01904978, 0.01725864, 0.01752220, 0.01656545, 0.01654573, 0.01652600, 0.01637607, 0.01622614, 0.01705492, 0.01599776)
spot_rate_for_Jan_14 <-c(0.02557456, 0.01894073, 0.01728286, 0.01735079, 0.01653242, 0.01646284, 0.01639325, 0.01620739, 0.01602152, 0.01722273, 0.01579170)
spot_rate_for_Jan_15 <-c(0.02580580,0.01867016, 0.01703735, 0.01730570, 0.01640245, 0.01628231, 0.01616217, 0.01596575, 0.01576934, 0.01697521, 0.01552680)
        
        
all_spot_rate <- rbind(spot_rate_for_Jan_02,spot_rate_for_Jan_03,spot_rate_for_Jan_06,spot_rate_for_Jan_07,spot_rate_for_Jan_08,spot_rate_for_Jan_09,spot_rate_for_Jan_10,spot_rate_for_Jan_13,spot_rate_for_Jan_14,spot_rate_for_Jan_15)
        
plot(year_frac_for_Jan_02,spot_rate_for_Jan_02,col=1,type="l",xlab = "Years",ylab ="Spot rates")
lines(year_frac_for_Jan_03,spot_rate_for_Jan_03,col=2,type="l")
lines(year_frac_for_Jan_06,spot_rate_for_Jan_06,col=3,type="l")
lines(year_frac_for_Jan_07,spot_rate_for_Jan_07,col=4,type="l")
lines(year_frac_for_Jan_08,spot_rate_for_Jan_08,col=5,type="l")
lines(year_frac_for_Jan_09,spot_rate_for_Jan_09,col=6,type="l")
lines(year_frac_for_Jan_10,spot_rate_for_Jan_10,col=7,type="l")
lines(year_frac_for_Jan_13,spot_rate_for_Jan_13,col=8,type="l")
lines(year_frac_for_Jan_14,spot_rate_for_Jan_14,col=9,type="l")
lines(year_frac_for_Jan_15,spot_rate_for_Jan_15,col=10,type="l")
legend("topright",pch=c(15,15),legend=c("2020-01-02","2020-01-03",'2020-01-06','2020-01-07','2020-01-08','2020-01-09','2020-01-10','2020-01-13','2020-01-14','2020-01-15'),col=c(1,2,3,4,5,6,7,8,9,10),lty=1,cex=0.6)



        
        
        
# forward rate for 2020-01-02
# first estimate five spot rates for 2021-01-02,2022-01-02,2023-01-02,2024-01-02,2025-01-02 from the Mar and Sep spot rates

year_fra_for_est_spot_fixed <-c(yearFraction('2020-09-01','2021-03-01'), 
                          yearFraction('2021-09-01','2022-03-01'),
                          yearFraction('2022-09-01','2023-03-01'),
                          yearFraction('2023-09-01','2024-03-01'),
                          yearFraction('2024-09-01','2025-03-01'))





                        ##2020-03-01 2020-09-01  2021-03-01  2021-09-01  5.2022-03-01 2022-09-01 2023-03-01  2023-09-01  2024-03-01 2024-09-01   2025-03-01
                        ## by the liner interpolation to get spot rates on 01-02 for 5 years
spot_rate_for_Jan_02 <-c(0.02401996, 0.01866990, 0.01709004, 0.01715400, 0.01640316, 0.01640506, 0.01640696, 0.01626585, 0.01612474, 0.01783075, 0.01597572)
                        

                        


est_spot_rate_01_02 <-vector('numeric',length = 5)
est_spot_rate_01_03 <-vector('numeric',length = 5)
est_spot_rate_01_06 <-vector('numeric',length = 5)
est_spot_rate_01_07 <-vector('numeric',length = 5)
est_spot_rate_01_08 <-vector('numeric',length = 5)
est_spot_rate_01_09 <-vector('numeric',length = 5)
est_spot_rate_01_10 <-vector('numeric',length = 5)
est_spot_rate_01_13 <-vector('numeric',length = 5)
est_spot_rate_01_14 <-vector('numeric',length = 5)
est_spot_rate_01_15 <-vector('numeric',length = 5)


year_fra_for_est_spot_change<-c(yearFraction('2020-09-01','2021-01-14'), 
                                yearFraction('2021-09-01','2022-01-14'),
                                yearFraction('2022-09-01','2023-01-14'),
                                yearFraction('2023-09-01','2024-01-14'),
                                yearFraction('2024-09-01','2025-01-14'))




for(i in c(2,4,6,8,10)){
  est_spot_rate_01_14[i/2] <- spot_rate_for_Jan_14[i] + year_fra_for_est_spot_change[i/2] * (spot_rate_for_Jan_14[i+1] - spot_rate_for_Jan_14[i])/year_fra_for_est_spot_fixed[i/2]
}
est_spot_rate_01_14






est_spot_rate_01_02 <-c(0.01760788, 0.01664927, 0.01640634, 0.01617099, 0.01658376)
est_spot_rate_01_03 <-c(0.01729499, 0.01638759, 0.01606284, 0.01584345, 0.01614544)
est_spot_rate_01_06 <-c(0.01723403, 0.01624347, 0.01587781, 0.01559007, 0.01567215)
est_spot_rate_01_07 <-c(0.01736120, 0.01634310, 0.01596767, 0.01596361, 0.01593047)
est_spot_rate_01_08 <-c(0.01745943, 0.01638696, 0.01600245, 0.01574591, 0.01583130)
est_spot_rate_01_09 <-c(0.01748023, 0.01658814, 0.01628283, 0.01610269, 0.01625886)
est_spot_rate_01_10 <-c(0.01775607, 0.01656370, 0.01639015, 0.01624523, 0.01629958)
est_spot_rate_01_13 <-c(0.01773628, 0.01682058, 0.01653126, 0.01626612, 0.01627967)
est_spot_rate_01_14 <-c(0.01771575, 0.01674611, 0.01641142, 0.01607005, 0.01616536)
est_spot_rate_01_15 <-c(0.01745462, 0.01663328, 0.01619287, 0.01581953, 0.01589695)




fwd_01_02 <-vector('numeric',length = 4)
fwd_01_03 <-vector('numeric',length = 4)
fwd_01_06 <-vector('numeric',length = 4)        
fwd_01_07 <-vector('numeric',length = 4)
fwd_01_08 <-vector('numeric',length = 4)     
fwd_01_09 <-vector('numeric',length = 4)
fwd_01_10 <-vector('numeric',length = 4)
fwd_01_13 <-vector('numeric',length = 4)
fwd_01_14 <-vector('numeric',length = 4)
fwd_01_15 <-vector('numeric',length = 4)

#forward rate for 2020-01-02
spot_differ <-c(yearFraction('2020-01-02','2021-01-02'),
                yearFraction('2020-01-02','2022-01-02'),
                yearFraction('2020-01-02','2023-01-02'),
                yearFraction('2020-01-02','2024-01-02'),
                yearFraction('2020-01-02','2025-01-02'))

for(i in c(1:4)){
  fwd_01_02[i] <- (est_spot_rate_01_02[i+1]*spot_differ[i+1] - est_spot_rate_01_02[1]*spot_differ[1])/(spot_differ[i+1]-spot_differ[1])
}



            #f1_2         f1_3        f1_4        f1_5
fwd_01_02 <-c(0.01569066, 0.01580557, 0.01569203, 0.01632773)
fwd_01_03 <-c(0.01548019, 0.01544676, 0.01535960, 0.01585805)
fwd_01_06 <-c(0.01525291, 0.01519970, 0.01504208, 0.01528168)
fwd_01_07 <-c(0.01532500, 0.01527090, 0.01549775, 0.01557279)
fwd_01_08 <-c(0.01531449, 0.01527396, 0.01517474, 0.01542427)
fwd_01_09 <-c(0.01569605, 0.01568413, 0.01564351, 0.01595352)
fwd_01_10 <-c(0.01537133, 0.01570719, 0.01574162, 0.01593546)
fwd_01_13 <-c(0.01590488, 0.01592875, 0.01577607, 0.01591552)
fwd_01_14 <-c(0.01577647, 0.01575925, 0.01552148, 0.01577776)
fwd_01_15 <-c(0.01581194, 0.01556200, 0.01527450, 0.01550753)
all_fwd_mat <-t(rbind(fwd_01_02,fwd_01_03,fwd_01_06,fwd_01_07,fwd_01_08,fwd_01_09,fwd_01_10,fwd_01_13,fwd_01_14,fwd_01_15))


plot(spot_differ[2:5], fwd_01_02,col=1,type="l",xlab = "Years",ylab ="Forward rates",ylim = c(0.014,0.017))
lines(spot_differ[2:5],fwd_01_03,col=2,type="l")
lines(spot_differ[2:5],fwd_01_06,col=3,type="l")
lines(spot_differ[2:5],fwd_01_07,col=4,type="l")
lines(spot_differ[2:5],fwd_01_08,col=5,type="l")
lines(spot_differ[2:5],fwd_01_09,col=6,type="l")
lines(spot_differ[2:5],fwd_01_10,col=7,type="l")
lines(spot_differ[2:5],fwd_01_13,col=8,type="l")
lines(spot_differ[2:5],fwd_01_14,col=9,type="l")
lines(spot_differ[2:5],fwd_01_15,col=10,type="l")
legend("topright",pch=c(15,15),legend=c("2020-01-02","2020-01-03",'2020-01-06','2020-01-07','2020-01-08','2020-01-09','2020-01-10','2020-01-13','2020-01-14','2020-01-15'),col=c(1,2,3,4,5,6,7,8,9,10),lty=1,cex=0.6)





########## Covarince matrix for the Forward rate 

             #f1_2         f1_3        f1_4        f1_5
row_1 <- all_fwd_mat[1,]
row_2 <- all_fwd_mat[2,]
row_3 <- all_fwd_mat[3,]
row_4 <- all_fwd_mat[4,]
all_fwd_mat <-t(rbind(fwd_01_02,fwd_01_03,fwd_01_06,fwd_01_07,fwd_01_08,fwd_01_09,fwd_01_10,fwd_01_13,fwd_01_14,fwd_01_15))

res <- vector('numeric',length = 9)
for(i in c(1:9)){
    res[i] <- log(row_4[i+1]/row_4[i])
}
res

fwd_log_row1 <- c(-0.0135044889, -0.0147908375,  0.0047151773, -0.0006860428,  0.0246096493, -0.0209050023,  0.0341218944, -0.0081063909, 0.0022457613)
fwd_log_row2 <-c(-0.022963137, -0.016123582, 0.004673366,0.000200361, 0.026499955,  0.001469196,  0.014007083, -0.010698158, -0.012595449)
fwd_log_row3 <-c(-0.021412254, -0.020889079,  0.029843245, -0.021062649,  0.030423931,  0.006252026,  0.002186075, -0.016269364, -0.016040100)
fwd_log_row4 <-c(-0.029187632, -0.037022532,0.018870435, -0.009582918,  0.033737252, -0.001132680, -0.001252081, -0.008693380, -0.017275641)

all_fwd_matrix <-t(rbind(fwd_log_row1,fwd_log_row2,fwd_log_row3,fwd_log_row4))
mean(all_fwd_matrix)
cov(all_fwd_matrix) 
eigen(cov(all_fwd_matrix))




########## Covarince matrix for the yield to maturity rate 
### ALL YTM for 10 selected bonds for 10 days(colunmns)
###close price for        01-02         01-03    
CAN_1.5_Mar_1_2020 <- c(0.02406730, 0.02360272, 0.02407196, 0.02424001, 0.02441442, 0.02459557, 0.02478384, 0.02539588, 0.02561731, 0.02584839)
CAN_0.75_2020_09_01 <-c(0.01867487, 0.01841697, 0.01855629, 0.01875854, 0.01865118, 0.01869923, 0.01874771, 0.01905478, 0.01894590, 0.01867563)
CAN_0.75_2021_03_01 <-c(0.01709712, 0.01677094, 0.01666209, 0.01677226, 0.01697130, 0.01699425, 0.01737237, 0.01726601, 0.01728997, 0.01704453)
CAN_0.75_2021_09_01 <-c(0.01715870, 0.01692910, 0.01672966, 0.01693055, 0.01694640, 0.01714840, 0.01716467, 0.01752619, 0.01735545, 0.01730954)
CAN_0.5_2022_03_01 <-c(0.01640878, 0.01613774, 0.01603737, 0.01609939, 0.01616158, 0.01636784, 0.01633449, 0.01657150, 0.01653823, 0.01640818)
CAN_2022_09_01 <-c(0.01641449, 0.01610119, 0.01595238, 0.01603123, 0.01607789, 0.01632624, 0.01637399, 0.01655633, 0.01647418, 0.01629466)
CAN_1.75_2023_03_01 <-c(0.01642020, 0.01606464, 0.01586739, 0.01596306, 0.01599419, 0.01628463, 0.01641349, 0.01654117, 0.01641012, 0.01618115)
CAN_2023_09_01 <- c( 0.01628457, 0.01594637, 0.01571953, 0.01597277, 0.01586409, 0.01619070, 0.01632665, 0.01639691, 0.01623115, 0.01599183)
CAN_2.25_2024_03_01 <-c(0.01614895, 0.01582810, 0.01557167, 0.01598249, 0.01573399, 0.01609676, 0.01623981, 0.01625265, 0.01605218,0.01580251)
CAN_1.5_2024_09_01 <-c(0.01779154, 0.01727821, 0.01652193, 0.01692530, 0.01661320, 0.01719664, 0.01710820, 0.01704460, 0.01720326, 0.01695711)
CAN_1.25_2025_03_01 <-c(0.01599983, 0.01561627, 0.01531702, 0.01552139, 0.01552300, 0.01589068, 0.01599435, 0.01602035, 0.01581811, 0.01555471)

YTM_for_5_years <-rbind(CAN_1.5_Mar_1_2020, CAN_0.75_2020_09_01, CAN_0.75_2021_03_01, 
                        CAN_0.75_2021_09_01, CAN_0.5_2022_03_01,CAN_2022_09_01, CAN_1.75_2023_03_01,CAN_2023_09_01, 
                        CAN_2.25_2024_03_01, CAN_1.5_2024_09_01, CAN_1.25_2025_03_01)



year_fra_for_est_spot_fixed <-c(yearFraction('2020-09-01','2021-03-01'), 
                                yearFraction('2021-09-01','2022-03-01'),
                                yearFraction('2022-09-01','2023-03-01'),
                                yearFraction('2023-09-01','2024-03-01'),
                                yearFraction('2024-09-01','2025-03-01'))







##2020-03-01 2020-09-01  2021-03-01  2021-09-01  5.2022-03-01 2022-09-01 2023-03-01  2023-09-01  2024-03-01 2024-09-01   2025-03-01
## by the liner interpolation to get spot rates on 01-02 for 5 years

YTM_for_02 <-YTM_for_5_years[,1][2:11]
YTM_for_03 <-YTM_for_5_years[,2][2:11]
YTM_for_06 <-YTM_for_5_years[,3][2:11]
YTM_for_07 <-YTM_for_5_years[,4][2:11]
YTM_for_08 <-YTM_for_5_years[,5][2:11]
YTM_for_09 <-YTM_for_5_years[,6][2:11]
YTM_for_10 <-YTM_for_5_years[,7][2:11]
YTM_for_13 <-YTM_for_5_years[,8][2:11]
YTM_for_14 <-YTM_for_5_years[,9][2:11]
YTM_for_15 <-YTM_for_5_years[,10][2:11]

est_YTM_01_02 <-vector('numeric',length = 5)
est_YTM_01_03 <-vector('numeric',length = 5)
est_YTM_01_06 <-vector('numeric',length = 5)
est_YTM_01_07 <-vector('numeric',length = 5)
est_YTM_01_08 <-vector('numeric',length = 5)
est_YTM_01_09 <-vector('numeric',length = 5)
est_YTM_01_10 <-vector('numeric',length = 5)
est_YTM_01_13 <-vector('numeric',length = 5)
est_YTM_01_14 <-vector('numeric',length = 5)
est_YTM_01_15 <-vector('numeric',length = 5)

year_fra_for_est_spot_change<-c(yearFraction('2020-09-01','2021-01-15'), 
                                yearFraction('2021-09-01','2022-01-15'),
                                yearFraction('2022-09-01','2023-01-15'),
                                yearFraction('2023-09-01','2024-01-15'),
                                yearFraction('2024-09-01','2025-01-15'))

for(i in c(1,3,5,7,9)){
  est_YTM_01_15[(i+1)/2] <- YTM_for_15[i] + year_fra_for_est_spot_change[(i+1)/2] * (YTM_for_15[i+1] - YTM_for_15[i])/year_fra_for_est_spot_fixed[(i+1)/2]
}
est_YTM_01_15


 

est_YTM_01_02 <-c(0.01761427, 0.01665459, 0.01641833, 0.01619340, 0.01658711)
est_YTM_01_03 <-c(0.01730133, 0.01639273, 0.01607642, 0.01586621, 0.01615178)
est_YTM_01_06 <-c(0.01724087, 0.01624890, 0.01589336 ,0.01561685 ,0.01568519)
est_YTM_01_07 <-c(0.01736814, 0.01634874 ,0.01598351 ,0.01597957 ,0.01594256)
est_YTM_01_08 <-c(0.01746593, 0.01639267, 0.01601883, 0.01577230 ,0.01584400)
est_YTM_01_09 <-c(0.01748680, 0.01659334, 0.01629665 ,0.01612390 ,0.01626796)
est_YTM_01_10 <-c(0.01776969, 0.01657432 ,0.01640208 ,0.01626490 ,0.01631613)
est_YTM_01_13 <-c(0.01774302, 0.01682608 ,0.01654521 ,0.01629112 ,0.01629348)
est_YTM_01_14 <-c(0.01772235, 0.01675162 ,0.01642685 ,0.01609891 ,0.01617979)
est_YTM_01_15 <-c(0.01746137, 0.01663853, 0.01621016 ,0.01585089, 0.01591310)

total_est_YTM <-rbind(est_YTM_01_02,est_YTM_01_03,est_YTM_01_06,est_YTM_01_07,est_YTM_01_08,est_YTM_01_09,est_YTM_01_10,est_YTM_01_13,est_YTM_01_14,est_YTM_01_15)

YTM_row_1 <-total_est_YTM[,1]
YTM_row_2 <-total_est_YTM[,2]
YTM_row_3 <-total_est_YTM[,3]
YTM_row_4 <-total_est_YTM[,4]
YTM_row_5 <-total_est_YTM[,5]

res <- vector('numeric',length = 9)
for(i in c(1:9)){
  res[i] <- log(YTM_row_5[i+1]/YTM_row_5[i])
}
res

YTM_res_1 <- c(-0.017926218, -0.003500308 , 0.007354802,  0.005614479 , 0.001194095,  0.016047924, -0.001502286 ,-0.001165284 ,-0.014835831)
YTM_res_2 <- c(-0.015847506 ,-0.008812772  ,0.006125306 , 0.002683325 , 0.012167083 ,-0.001146619 , 0.015075805 ,-0.004435617, -0.006773742)
YTM_res_3 <- c(-0.021044866 ,-0.011452070 , 0.005656269  ,0.002207589,  0.017194389 , 0.006448482 , 0.008688707 ,-0.007179800, -0.013278905)
YTM_res_4 <- c(-0.020412309 ,-0.015841218 , 0.022960858 ,-0.013056221,  0.022047472,  0.008706701,  0.001610899 ,-0.011868500 ,-0.015525871)
YTM_res_5 <- c(-0.026595544 ,-0.029313748 , 0.016275691, -0.006201361 , 0.026406275 , 0.002956756 ,-0.001388889 ,-0.007002420, -0.016620073)
YTM_Martix <-t(rbind(YTM_res_1,YTM_res_2,YTM_res_3,YTM_res_4,YTM_res_5))
cov(YTM_Martix)
eigen(cov(YTM_Martix))








