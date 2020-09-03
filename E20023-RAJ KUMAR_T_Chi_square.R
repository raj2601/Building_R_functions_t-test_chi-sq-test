fram_data <-read.csv("C:\\Users\\MY PC\\Desktop\\e-files\\Trimester-1\\Machine_Learning\\framingham.csv")
View(fram_data)
names(fram_data)
?t.test


# Function 1:
# The below function named compare_variables() is used to compare all predicted variables with Target variables mentioned.
# The (data_frame,numerical variable, catergorical variable and Target variable) are given into the function()
# Here for demonstration purpose framingham dataset is used.The main objective is to perform t-test and chi-test
# depending upon the variable type.




numerical <- c('age','diaBP','BMI','totChol','sysBP','diaBP','heartRate','glucose','cigsPerDay')
categorical <- c('male','education','currentSmoker','prevalentHyp','diabetes','prevalentStroke')

compare_variables = function(data,n_var,cat_var,target_var)
{
  t_result <- c()
  chi_result <- c()
  
    for(i in 1 : length(n_var)) #Iterates through each numerical continuous variable. 
    {
      pval = t.test( data[n_var[i]][data[target_var] == 0], data[n_var[i]][data[target_var] == 1])['p.value']
      #Gives the p-value output of t-test.
      t_result[i] = as.numeric(pval)
    }
  names(t_result) = n_var   #Matching the names of var with their names using index
  t_values=data.frame(n_var,t_result)
  names(t_values)=c("","T-test results")
  
  
    for(j in 1:length(cat_var)) #Iterates through all categorical variables.
    {
  chival=chisq.test(data[cat_var[j]],data[[target_var]])['p.value'] #p-value of chi-square tests.
  chi_result[j]=as.numeric(chival)
    }

  names(chi_result) = cat_var
  chi_values=data.frame(chi_result)
  names(chi_values)="Chi square-test results"

  print(t_values)
  print("===============================================================")
  print(chi_values)
}

compare_variables(fram_data,numerical,categorical,'TenYearCHD')


#=================#=======================#===========================#================================



























