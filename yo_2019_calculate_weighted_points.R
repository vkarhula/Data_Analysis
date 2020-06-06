#########################################################################################
#
# This code calculates weighted points for medical faculty entrance exam for all students,
# who have grade in chemistry and biology.
# Data is sorted and stored in data frame, which is written in csv file.
# Some visualization implemented, further in Power BI.
#
# Code written by Virpi Karhula, 11st of May, 2020.
#
#########################################################################################

#--------------------- Reading data from csv file ---------------------------------------

# Read in grades of students from spring 2019
# Data file available in https://www.ylioppilastutkinto.fi/ext/data/FT2019KD4001.csv
yo2019k <- FT2019KD4001


#--------------------- Calculating weighted points, functions --------------------------------------

# Function retuns grade value weighted with corresponting coefficient for medical faculty
# grade is student's grade, 1 means I, 2 is A, 3 is B,... 7 is L
# co1 equals to i, co2 to A, co3 to B,... co7 to L
add_weighted_grade <- function(grade, co1, co2, co3, co4, co5, co6, co7){
  weighted_grade_value <- 0
  switch(grade,
         "1" = weighted_grade_value <- co1,
         "2" = weighted_grade_value <- co2,
         "3" = weighted_grade_value <- co3,
         "4" = weighted_grade_value <- co4,
         "5" = weighted_grade_value <- co5,
         "6" = weighted_grade_value <- co6,
         "7" = weighted_grade_value <- co7
  )
  # print(weighted_grade_value)
  return(weighted_grade_value)
}

# Testing
# add_weighted_grade(3,1,2,3,4,5,6,7)  # testing


# Function calculates weighted points for one student
# Most important grades for medical faculty are included in calculations in this demo
# Max six subjects may be taken into account in calculations
# Note: Not all subjects have been included for calcultations,
# because limitations in data cause bigger errors than this
# Coefficients for grades can be found in opintopolku:
# https://opintopolku.fi/wp/opo/korkeakoulujen-haku/mika-korkeakoulujen-opiskelijavalinnoissa-muuttuu-vuoteen-2020-menessa/yliopistojen-todistusvalinnat-2020/
calculate_weighted_points <- function(index){
  sum <- 0
  sum_weighted <- 0
  class(sum)

  # A [,8], mother tongue, Finnish
  # print(yo2019k[index,8])
  if (!is.na(yo2019k[index,8])) {
    # sum <- sum + yo2019k[index,8]
    sum_weighted <- sum_weighted + add_weighted_grade(yo2019k[index,8], 0, 5.5, 11.0, 16.5, 22.0, 27.5, 33.0) 
  }
  # O [,9], mother tongue, Swedish 
  if (!is.na(yo2019k[index,9])) {
    sum_weighted <- sum_weighted + add_weighted_grade(yo2019k[index,9], 0, 5.5, 11.0, 16.5, 22.0, 27.5, 33.0) 
  }
  # M [,16], mathematics, long
  if (!is.na(yo2019k[index,16])) {
    sum_weighted <- sum_weighted + add_weighted_grade(yo2019k[index,16], 0, 6.6, 13.2, 19.8, 26.4, 33.1, 39.7) 
  }
  # N [,17], mathematics, short
  if (!is.na(yo2019k[index,17])) {
    sum_weighted <- sum_weighted + add_weighted_grade(yo2019k[index,17], 0, 4.7, 9.4, 14.1, 18.9, 23.6, 28.3) 
  } 
  # KE [,27], chemistry
  if (!is.na(yo2019k[index,27])) {
    sum_weighted <- sum_weighted + add_weighted_grade(yo2019k[index,27], 0, 5.7, 11.3, 17.0, 22.7, 28.3, 34.0) 
  }
  # BI [,18], biology
  if (!is.na(yo2019k[index,18])) {
    sum_weighted <- sum_weighted + add_weighted_grade(yo2019k[index,18], 0, 5.4, 10.8, 16.2, 21.5, 26.9, 32.3) 
  }
  
  # Two best points from the following three subjects are selected
  max_three <- c()
  #length(max_three)
  # FY [,20], physics
  if (!is.na(yo2019k[index,20])) {
    max_three[length(max_three)+1] <- add_weighted_grade(yo2019k[index,20], 0, 5.3, 10.6, 15.9, 21.2, 26.5, 31.7) 
  } 
  # PS [,22], psychologia
  if (!is.na(yo2019k[index,22])) {
    max_three[length(max_three)+1] <- add_weighted_grade(yo2019k[index,22], 0, 4.1, 8.2, 12.3, 16.4, 20.5, 24.6) 
  } 
  # EA [34], long language, english
  if (!is.na(yo2019k[index,34])) {
    max_three[length(max_three)+1] <- add_weighted_grade(yo2019k[index,34], 0, 4.7, 9.4, 14.1, 18.9, 23.6, 28.3) 
  } 
  
  # Testing
  # Third subject added for testing, because selected student has only two values
  # max_three[length(max_three)+1] <- 33.3
  # print(max_three)
  
  sum_two_values <- 0
  if(length(max_three) == 0) {sum_two_values <- 0}
  if(length(max_three) == 1) {sum_two_values <- max_three[1]}
  if(length(max_three) == 2) {sum_two_values <- max_three[1] + max_three[2]}
  # Select two best values of (physist, pshycology and language)
  if (length(max_three) == 3) {
    sum[1] <- c(max_three[1] + max_three[2])
    sum[2] <- c(max_three[2] + max_three[3])
    sum[3] <- c(max_three[1] + max_three[1])
    # print(sum)
    # print(max(sum))
    sum_two_values <- max(sum)
    # print(sum_two_values)
  } 
  # Add max two selected values to sum_weighted
  sum_weighted <- sum_weighted + sum_two_values
  return(sum_weighted)
}

# Test case
# print(calculate_weighted_points(25539))  #25540 index in table view
# result is 124
student_weighted <- as.double(calculate_weighted_points(25539))


#########################################################################################
# Calculate weighted points for students who have grade in chemistry and biology
# Variable Points_weighted_ke_bi is used to store weighted points for medical faculty
#########################################################################################
Points_weighted_ke_bi <- c()
count_ke_bi <- 0
print(paste("size:",nrow(yo2019k)))
for (i in 1:nrow(yo2019k)) {
  if ((!is.na(yo2019k[i,27])) & (!is.na(yo2019k[i,18]))) {
    count_ke_bi <- count_ke_bi + 1
    # Calculate point using weighted coefficients for medical faculty
    Points_weighted_ke_bi[count_ke_bi] <- calculate_weighted_points(i)
    # print(paste("Points_weighted are: ", calculate_weighted_points(i), "for student index",i))
  }
}
length(Points_weighted_ke_bi)
count_ke_bi
head(Points_weighted_ke_bi)


#----------- Sorting points and writing results to csv file -----------------------------

# Sort results from smallest to biggest
ordered_ke_bi <- sort(Points_weighted_ke_bi)
plot(ordered_ke_bi)
mean(ordered_ke_bi)
head(ordered_ke_bi, n=10)
tail(ordered_ke_bi, n=382)
# write to csv file (ordered values in one column)
write.table(ordered_ke_bi, file="ordered_ke_bi.csv", sep = ",", row.names = F)

# Set index and ke&bi points to data frame
length(ordered_ke_bi)
ind <- 1:length(ordered_ke_bi)
ordered_ke_bi_data_frame <- data.frame(ind, ordered_ke_bi)
head(ordered_ke_bi_data_frame)
# write to csv file (index and value columns)
write.table(ordered_ke_bi_data_frame, file="ordered_ke_bi_data_frame.csv", sep = ",", row.names = F)


#------------- Visualization ------------------------------------------------------------

# Weighted points for medical faculty for students, who wrote chemistry and biology

# Make histogram of ordered results
histogram_ke_bi <- hist(ordered_ke_bi, breaks = 60)
mean(ordered_ke_bi)
d <- density(ordered_ke_bi)
plot(d)

# normal deviation
x <- ordered_ke_bi
curve(dnorm(x, mean=mean(ordered_ke_bi), sd=sd(ordered_ke_bi)), add=TRUE, col='red', lwd=2)

# Empirical cumulative distribution function
P_ke_bi <- ecdf(Points_weighted_ke_bi)
plot(P_ke_bi)
