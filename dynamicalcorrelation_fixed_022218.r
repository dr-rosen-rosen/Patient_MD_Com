########################################################
###          IMPORTANT NOTE                          ###
########################################################

# The original R script published in Liu et al. (2016), Psychological Methods, 21(3), 291-308 contained 
# multiple errors that lead to incorrect significance testing results. These errors have been corrected
# in this updated script. Specifically, we removed argument 'na' from function 'ind_DC' and argument 'ms'  
# from function 'boot_test_DC'. We added codes to check if there is any missing value in the data, and 
# to compute proportion of missing values in the data. We also added a warning when too many missing values 
# are present. Finally, argument 't' is now correctly labelled 'a vector of time points where x,y are observed'.
# We are grateful to Jianning Chen, PhD candidate in Statistics at UC Davis, for making these corrections.


########################################################
### Estimating and Testing for Dynamical Correlation ###
########################################################

# Use the ind_DC function below to estimate dynamical correlations for each individual (Eq. 7)
# The sample-level dynamical correlation can then be computed by averaging the individual dynamical correlations (Eq. 8)

install.packages("caTools")                          ### load package {caTools} to call 'trapz' ###                                      
library(caTools)   

ind_DC=function(x,y,t){                     ### input x, y (data matrices with rows representing measurements and columns representing subjects)
                                                     #         t  (a vector of time points where x,y are observed) 
                                                                            

      na =  sum(is.na(x)+is.na(y))                     #check na                
      if(na/ncol(x)/nrow(x)/2 > 0.5){
        print('warning: too many missings may cause distortion')
      }
      if (na>0) {                                      ### impute missing values by linear interpolation ###
      for (i in 1:dim(x)[2]){ 
          x[,i]=approx(t,x[,i],xout=t,rule=2)$y      
          y[,i]=approx(t,y[,i],xout=t,rule=2)$y
          }
      }

      temp1_x=temp1_y=matrix(0,nrow=dim(x)[1],ncol=dim(x)[2])
      temp2_x=temp2_y=matrix(0,nrow=dim(x)[1],ncol=dim(x)[2])
      M_x=M_y=z=numeric()

      for (i in 1:dim(x)[2]){
          aver_x=trapz(t,x[,i])/(tail(t,1)-head(t,1))             ### integrals approximated by trapezoidal rule ###
          aver_y=trapz(t,y[,i])/(tail(t,1)-head(t,1))
          temp1_x[,i]=x[,i]-aver_x                                ### center within subjects (vertical shift; Eq. 4) ###
          temp1_y[,i]=y[,i]-aver_y
          }

      M_x=rowMeans(temp1_x)                                       ### the population-mean of vertically shifted curves (Eq. 5) ###
      M_y=rowMeans(temp1_y) 
      for (i in 1:dim(x)[2]){
          temp2_x[,i]=(temp1_x[,i]-M_x)/sqrt(trapz(t,(temp1_x[,i]-M_x)^2)/(tail(t,1)-head(t,1))) ### population-centering and standardization (Eq. 6) ###
          temp2_y[,i]=(temp1_y[,i]-M_y)/sqrt(trapz(t,(temp1_y[,i]-M_y)^2)/(tail(t,1)-head(t,1)))
          z[i]=trapz(t,temp2_x[,i]*temp2_y[,i])/(tail(t,1)-head(t,1))   ### obtain individual dynamical correlation ###
          }
      return(z)
}


# Use the boot_test_DC function to conduct significance testing 

boot_test_DC=function(x1,y1,t1,x2,y2,t2,B=1000){        ### input x1,y1 (data matrices with rows representing measurements and columns representing subjects)
                                                                          #         x2,y2 (paired data matrices (if there are any) with rows representing measurements and columns representing subjects)
                                                                          #         t1    (a vector of time points where x1,y1)
                                                                          #         t2    (a vector of time points where x2,y2)
                                                                          #         B     (the number of bootstrap samples)                                                                            ###
      
     n=dim(x1)[2]
     if (missing(x2)) {                                           ### one-sample test ###
     na1 = sum(is.na(x1)+is.na(y1))
     if(na1/ncol(x1)/nrow(x1)/2 > 0.5){
       print('warning: too many missings may cause distortion')
     }
     if (na1>0) {                                      ### impute missing values by linear interpolation ###
         for (i in 1:dim(x1)[2]){ 
           x1[,i]=approx(t1,x1[,i],xout=t1,rule=2)$y      
           y1[,i]=approx(t1,y1[,i],xout=t1,rule=2)$y
         }
     }
     dyncor_1=ind_DC(x1,y1,t1)                           ### observed DC ###
     obs_1_stud=mean(dyncor_1)*sqrt(n)/sd(dyncor_1)               ### observed standardized version ###
     boot_1_stud=numeric()
     boot_x1=boot_y1=matrix(0,nrow=length(t1),ncol=n)

     for(b in 1:B){                                               
        idx=sample(c(1:n),replace = TRUE)                         
        boot_x1=x1[,idx]                                          ### bootstrap replicates ###
        boot_y1=y1[,idx]
        boot_dyncor_1=ind_DC(boot_x1,boot_y1,t1)                  ### DC based on bootstrap samples ###
        boot_1_stud[b]=mean(boot_dyncor_1-dyncor_1)*sqrt(n)/sd(boot_dyncor_1)    ### bootstrap standardized version ###       
        }

     emp.stat=obs_1_stud                                          ### bootstrap test statistic ###
     emp.pval=length(boot_1_stud[boot_1_stud>abs(obs_1_stud) | boot_1_stud< -abs(obs_1_stud)])/B    ### p-value based on bootstrap null distribution ###
     outp=list(stats=emp.stat, pval=emp.pval) 
     return(outp)
     }  else {                                                    ### two-sample paired test (similar as above) ###
        na1 = sum(is.na(x1)+is.na(y1))
        na2 = sum(is.na(x2)+is.na(y2))                      #check missing
        if(na1/ncol(x1)/nrow(x1)/2 > 0.5 || na2/ncol(x2)/nrow(x2)/2 > 0.5){
          print('warning: too many missings may cause distortion')
        }
        if (na1 > 0) {                                      ### impute missing values by linear interpolation ###
         for (i in 1:dim(x1)[2]){ 
           x1[,i]=approx(t1,x1[,i],xout=t1,rule=2)$y      
           y1[,i]=approx(t1,y1[,i],xout=t1,rule=2)$y
         }
        }
       if (na2 > 0) {                                      ### impute missing values by linear interpolation ###
         for (i in 1:dim(x1)[2]){ 
           x2[,i]=approx(t1,x1[,i],xout=t1,rule=2)$y      
           y2[,i]=approx(t2,y2[,i],xout=t2,rule=2)$y
         }
       }
        dyncor_1=ind_DC(x1,y1,t1)
        dyncor_2=ind_DC(x2,y2,t2)
        obs_1_stud=mean(dyncor_1)*sqrt(n)/sd(dyncor_1)
        obs_2_stud=mean(dyncor_2)*sqrt(n)/sd(dyncor_2)
        obsdiff=dyncor_2-dyncor_1
        obsdiff_stud=mean(obsdiff)*sqrt(n)/sd(obsdiff)
        boot_1_stud=boot_2_stud=boot_diff_stud=numeric()

        boot_x1=boot_y1=matrix(0,nrow=length(t1),ncol=n)
        boot_x2=boot_y2=matrix(0,nrow=length(t2),ncol=n)
             
              for(b in 1:B){
                  idx=sample(c(1:n),replace = TRUE)
                  boot_x1=x1[,idx]
                  boot_y1=y1[,idx]
                  boot_x2=x2[,idx]
                  boot_y2=y2[,idx]
                  boot_dyncor_1=ind_DC(boot_x1,boot_y1,t1)
                  boot_dyncor_2=ind_DC(boot_x2,boot_y2,t2)
                  boot_1_stud[b]=mean(boot_dyncor_1-dyncor_1)*sqrt(n)/sd(boot_dyncor_1)
                  boot_2_stud[b]=mean(boot_dyncor_2-dyncor_2)*sqrt(n)/sd(boot_dyncor_2)
                  boot_diff_stud[b]=mean(boot_dyncor_2-boot_dyncor_1-obsdiff)*sqrt(n)/sd(boot_dyncor_2-boot_dyncor_1)
                  }

       emp.stat=obsdiff_stud
       emp.pval=length(boot_diff_stud[boot_diff_stud>abs(obsdiff_stud) | boot_diff_stud< -abs(obsdiff_stud)])/B
       outp=list(stats=emp.stat, pval=emp.pval) 
       return(outp)
       } 
}



##########################
### Simulation Example ###
##########################
library(MASS)
###### DC=1 ######
n=100             # sample size
t=seq(0,1,length.out=100)       # length of data

# fixed effects models for x and y
mu_quad_x=8*t^2-4*t+5
mu_quad_y=8*t^2-12*t+6

# simulation
fun=rbind(rep(1,length(t)),-t,t^2)
z1=mvrnorm(n,rep(0,3),diag(c(2,16/3,4)))   # covariance matrix of random effects
x1_quad=y1_quad=x1_quad_error=y1_quad_error=matrix(0,nrow=length(t),ncol=n)


for (i in 1:n){
  x1_quad[,i]=mu_quad_x+z1[i,]%*%fun           # x without error
  y1_quad[,i]=mu_quad_y+2*z1[i,]%*%fun         # y without error
  x1_quad_error[,i]=x1_quad[,i]+rnorm(length(t),0,0.5)      # x with error
  y1_quad_error[,i]=y1_quad[,i]+rnorm(length(t),0,0.5)      # y  with error
}

                                
dyn1_quad=ind_DC(x1_quad,y1_quad,t)                     
dyn1_quad         
mean(dyn1_quad)    # DC=1 because there is no error
             
dyn1_quad_error=ind_DC(x1_quad_error,y1_quad_error,t)         
dyn1_quad_error
mean(dyn1_quad_error)    # DC close to 1 because of error



###### test H0: DC=0 ######
bt_DC=boot_test_DC(x1_quad_error,y1_quad_error,t,B=1000)        ### use bootstrap sample 1000 (default) for example ###
bt_DC                                                        ### test results ###
bt_DC$stats                                                  ### test statistic ###
bt_DC$pval                                                   ### p-value ###

###### test H0: DC1=DC2 
bt2_DC=boot_test_DC(x1_quad,y1_quad,t,x1_quad_error,y1_quad_error,t,B=1000) 
