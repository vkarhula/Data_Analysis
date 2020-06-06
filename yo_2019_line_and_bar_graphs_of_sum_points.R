#########################################################################################
# 
# This program copies sum points of each student to a data frame
# Occurence of sum points of each student is presented by line graph.
# One student is selected and his results are presented by bar graphs.
# Three bars represent his points in spring 2019, autums 2019 and spring 2020.
#
# Code written by Virpi Karhula, in May 2020
#
#########################################################################################


################## Reading data from csv file ###########################################

# Read in grades of students from spring 2019
# Data file available in https://www.ylioppilastutkinto.fi/ext/data/FT2019KD4001.csv
yo2019k <- FT2019KD4001

# Function copy_sum_points copies sum points for one student 
# Sum value is in 'yht' column in table
# Returns the value of sum point of one student
copy_sum_points <- function(index){
  sum_points <- 0
  # yht [,7], sum points
  # print(yo2019k[index,7])
  if (!is.na(yo2019k[index,7])) {
    sum_points <- yo2019k[index,7]
  }
  return(sum_points)
}


# This code collects the needed data from the data source file
# Copies sum points of all students into a data frame
Sum_points <- c()
print(paste("size:",nrow(yo2019k)))
for (i in 1:nrow(yo2019k)) {
  # Use sum column 'yht' in csv file
  Sum_points[i] <- copy_sum_points(i)
}

head(Sum_points)


############### Preparing data for visualization ########################################

# Preparing data to data frame
yo_sum <- data.frame(Sum_points)
head(yo_sum)

# table counts how many times each value exists and writes down value and its occurrence
yo_values <- table(yo_sum[1])
head(yo_values)

# as.data.frame sets results to two columns
yo_values <- as.data.frame(yo_values)
head(yo_values)

# Rename column titles
names(yo_values) <- c("value","amount")
head(yo_values)


############## Adding one student sum values for 3rd, 4th and 5th columns ###############

# One student values will be presented as bars on top of the line plot
# Third column corresponds to spring 2019 sum result (27)
# Fourth column corresponds to autumn 1019 sum result (35)
# Fifth column corresponds to spring 2020 sum result (37)
# Height is selected to be 700 for visual reasons
# Other values are set to -10, so they won't get visible on the view, when y-scale min is set to 0
# (This causes warning messages in ggplot)

one_student_1st <- as.integer(c(replicate(19,-50),700, replicate(40,-50)))
one_student_2nd <- as.integer(c(replicate(27,-50),700, replicate(32,-50)))
one_student_3rd <- as.integer(c(replicate(29,-50),700, replicate(30,-50)))

# Column of one student values are bound to data frame as a 3rd, 4th and 5th columns
yo_values_and_one_student <- cbind(yo_values, y1=one_student_1st, y2=one_student_2nd, y3=one_student_3rd)
names(yo_values_and_one_student) <- c('sum_points','amount','one_student_1st','one_student_2nd','one_student_3rd')
yo_values_and_one_student


################# Visualization #####################################################

# Visualization curve for all students and a bar for one student value (27) -> presentation
ggplot(yo_values_and_one_student, aes(x=sum_points,y=amount)) + 
  # line graph of all students
  geom_line(group = 1, size = 1,colour='black') +
  # bar of one student in spring 2019 (3rd column in data frame selected to y axis data source)
  geom_col(aes(x=sum_points,y=one_student_1st), size=4, colour='#b0d0d0') +
  # number on top of the bar
  geom_text(aes(y=one_student_1st, label = "27"), vjust = -1, hjust = "center", show.legend = FALSE) +
  ggtitle("Kevään 2019 ylioppilaskirjoitusten tulokset ja yhden oppilaan yhteispisteet") +
  # set values for x axis ticks and labels
  scale_x_discrete("Yhteispisteet", c(10,20,30,40,50,60),c("10","20","30","40","50","60")) +
  # y axis scale
  ylim(0, 1350) +
  ylab("Lukumäärä")

# Visualization curve for all students and three bars for one student with values 27, 35 and 37 -> presentation!
ggplot(yo_values_and_one_student, aes(x=sum_points,y=amount)) + 
  geom_line(group = 1, size = 1,colour='black') +
  geom_col(aes(x=sum_points,y=one_student_1st), size=4, colour='#b0d0d0') +
  geom_text(aes(y=one_student_1st, label = "27"), vjust = -1, hjust = "center", show.legend = FALSE) +
  geom_col(aes(x=sum_points,y=one_student_2nd), size=4, colour='#7c9999') +
  geom_text(aes(y=one_student_2nd, label = "35"), vjust = -1, hjust = "center", show.legend = FALSE) +
  geom_col(aes(x=sum_points,y=one_student_3rd), size=4, colour='#4f6062') +
  geom_text(aes(y=one_student_3rd, label = "37"), vjust = -1, hjust = "center", show.legend = FALSE) +
  ggtitle("Kevään 2019 ylioppilaskirjoitusten tulokset ja yhden oppilaan korotetut yhteispisteet") +
  scale_x_discrete("Yhteispisteet", c(10,20,30,40,50,60),c("10","20","30","40","50","60")) +
  ylim(0, 1350) +
  ylab("Lukumäärä") 

