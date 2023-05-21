#' @title OUTLIER ANALYSIS
#'
#' @description This function is intended to automate process of statistical tests which are generally performed on the data. Statistical tests includes two sample T-test, Chi-Squared test and ANOVA. This function takes 3 inputs1. data - Takes datatype of dataframe from csv file2. dataname - This is the name of the dataset, this helps to save the file with proper name in excel. 3. summary - default = F, If summary = T, It prints the summary of tests in console
#'
#'
#' @param data
#' @param dataname
#' @param sumary
#' @return Statistical test results are exported to excel file
#'
#' @examples statistical_tests(data,'dataname',T)
#'
#' @export

statisticaltests <-function(data, dataname, summary = FALSE)
  
  
{
  options(warn=-1)
  library(xlsx)
  binary <- c()   # segregated data(indexes of the columns of the data) in lists
  multi = c()
  numer_cont = c()
  numer_cat = c()
  for(i in 1:ncol(data))
  {
    
    if (length(unique(data[,i])) == 2) # find binary variables
    {
      binary = c(binary,i)
      numer_cat = c(numer_cat,i)
      
    }
    
    else if (length(unique(data[,i])) >2 & length(unique(data[,i])) < 15) # find binary variables
    {
      multi = c(multi,i)
      numer_cat = c(numer_cat,i)
    }
    
    else if (is.numeric(data[,i]) & length(unique(data[,i])) >= 15)
    {
      numer_cont = c(numer_cont,i)
    }
    
    
  }
  
  PtestT = c()
  fir_var_t = c()
  sec_var_t = c()
  fir_var_chi = c()
  sec_var_chi = c()
  Ptestchi = c()
  Ptestaov = c()
  fir_var_aov = c()
  sec_var_aov = c()
  for (j in binary)
  {
    for (k in numer_cont)
    {
      fir_var_t = c(fir_var_t,names(data)[j])
      sec_var_t = c(sec_var_t,names(data)[k])
      PtestT = c(PtestT, round((t.test(data[,k][data[,j] == unique(data[,j])[1]], data[,k][data[,j] == unique(data[,j])[2]]))$p.value,3))
      if (summary == T)
      {
        print(paste('T-test between', names(data[j]), '(data[,j]) and', names(data[k]),'(data[,k])'))
        print(t.test(data[,k][data[,j] == unique(data[,j])[1]], data[,k][data[,j] == unique(data[,j])[2]]))
      }
    }
    
  }
  for (j in numer_cat)
  {
    for (a in numer_cat)
    {
      if (a !=j)
      {
        
        fir_var_chi = c(fir_var_chi,names(data)[j])
        sec_var_chi = c(sec_var_chi,names(data)[a])
        Ptestchi = c(Ptestchi, round((chisq.test(data[,a], data[,j]))$p.value,3))
        if (summary == T)
        {
          print(paste('Chi_Squared test between', names(data[j]), '(data[,j]) and', names(data[a]),'(data[,a])'))
          print(chisq.test(data[,a], data[,j]))
        }
      }
      
    }
  }
  
  for (j in multi)
  {
    for (k in numer_cont)
    {
      fir_var_aov = c(fir_var_aov,names(data)[j])
      sec_var_aov = c(sec_var_aov,names(data)[k])
      Ptestaov = c(Ptestaov, round((summary(aov(data[,k]~ data[,j])))[[1]][["Pr(>F)"]][1],3))
      if (summary == T)
      {
        print(paste('ANOVA between', names(data[j]), '(data[,j]) and', names(data[k]),'(data[,k])'))
        print(summary(aov(data[,k]~ data[,j])))
        cat('\n')
      }
    }
    
  }
  
  File = paste(getwd(),'/',dataname ,'_Statistical_Tests', ".xlsx", sep = "")
  if(!file.exists(File))
  {
    dir.create(dirname(File),showWarnings = F)
  }
  else
  {
    unlink("./data", recursive = TRUE)
    dir.create(dirname(File),showWarnings = F)
  }
  
  
  
  if ((length(binary)>=1) & (length(numer_cat))>=2 & length(multi)>=1 & length(numer_cont)>=1)
  {
    write.xlsx(data.frame('First_Variable'=fir_var_t,'Second_Variable'=sec_var_t,'P_value'= PtestT), file = File, sheetName = "T-test",
               col.names = TRUE, row.names = F, append = F)
    write.xlsx(data.frame('First_Variable'=fir_var_chi,'Second_Variable'=sec_var_chi,'P_value'= Ptestchi), file = File, sheetName = "Chi-Squared test",
               col.names = TRUE, row.names = F, append = T)
    write.xlsx(data.frame('First_Variable'=fir_var_aov,'Second_Variable'=sec_var_aov,'P_value' = Ptestaov), file = File, sheetName = "ANOVA",
               col.names = TRUE, row.names = F, append = T)
  }
  
  else if ((length(binary)==0) & (length(numer_cat))>=2 & length(multi)>=1 & length(numer_cont)>=1)
  {
    
    write.xlsx(data.frame('First_Variable'=fir_var_chi,'Second_Variable'=sec_var_chi,'P_value'= Ptestchi), file = File, sheetName = "Chi-Squared test",
               col.names = TRUE, row.names = F, append = F)
    write.xlsx(data.frame('First_Variable'=fir_var_aov,'Second_Variable'=sec_var_aov,'P_value' = Ptestaov), file = File, sheetName = "ANOVA",
               col.names = TRUE, row.names = F, append = T)
  }
  
  else if ((length(binary)>=1) & (length(numer_cat))>=2 & length(multi)==0 & length(numer_cont)>=1)
  {
    write.xlsx(data.frame('First_Variable'=fir_var_t,'Second_Variable'=sec_var_t,'P_value'= PtestT), file = File, sheetName = "T-test",
               col.names = TRUE, row.names = F, append = T)
    write.xlsx(data.frame('First_Variable'=fir_var_chi,'Second_Variable'=sec_var_chi,'P_value'= Ptestchi), file = File, sheetName = "Chi-Squared test",
               col.names = TRUE, row.names = F, append = F)
    
  }
  
  else if ((length(numer_cat))>=2 & length(numer_cont)==0)
  {
    
    write.xlsx(data.frame('First_Variable'=fir_var_chi,'Second_Variable'=sec_var_chi,'P_value'= Ptestchi), file = File, sheetName = "Chi-Squared test",
               col.names = TRUE, row.names = F, append = T)
    
  }
  
  else if ((length(numer_cat))>=2 & length(numer_cont)==0)
  {
    
    print("You should change the data")
    
  }
  
  
  
  
}