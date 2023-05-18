library("readxl")
df=read_xlsx("C:/Users/hp/Desktop/main data mfe.xlsx")
I_mean <- mean(df$Runs_India)
O_mean <- mean(df$Runs)
std_I <- sd(df$Runs_India)
std_O <- sd(df$Runs)
n_I<-1016
n_O<-1016
print("NULL HYPOTHESIS : There is no significant difference of score mean between Team India and Other Teams.")
print("ALTERNATIVE HYPOTHESIS : There is a significant difference of score mean between Team India and Other Teams.")
std=sqrt((((std_I)^2)/n_I)+(((std_O)^2)/n_O))
test_stat = ((I_mean-O_mean)-0)/std
test_stat
d_f <- n_I + n_O -2
p_value=2*pt(q=test_stat, d_f, lower.tail=FALSE)
p_value
alpha <- 0.05
if(p_value < alpha){
  print("We reject our null hypothesis and say that there is significant difference between the average scores of team India and other teams.")
}else{
  print("We failed to reject our null hypothesis and say that there is no significant difference between the average scores of team India and other teams.")
}
