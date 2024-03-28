
library(snow)
library(snowfall) 
library(parallel)

cat("is doing now! please wait")
m=1000
test=T
main_result=F

if(test){
  ### power log_logistic
  sfInit(parallel = TRUE, cpus = detectCores())
  sfLibrary(survival)
  sfLibrary(dplyr)
  sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/function.R')
  sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/data_generation.R')
  sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/pvalue_result.R')
  dir_name = '/public/home/shiyifeng/PairedSurv/Rcode_server/result/test'
  if(!dir.exists(dir_name)){
    dir.create(dir_name)
  }
  setwd(dir_name)
  
  m=1000
  set.seed(1)
  # change para
 # beta_0=0
 # beta_1=10
 # beta_t=1
  output9 <- data.frame()
  output9 <- rbind(output9,work_H1(model="weibull",n=50,m=m,c1=20,c2=20
                                   ,beta_0=-50,beta_1=3,beta_2=3,beta_3=1,beta_a=-0.2,p=10))
  output9 <- rbind(output9,work_H1(model="weibull",n=50,m=m,c1=30,c2=30
                                   ,beta_0=-50,beta_1=3,beta_2=3,beta_3=1,beta_a=-0.2,p=10))
  output9 <- rbind(output9,work_H1(model="weibull",n=50,m=m,c1=60,c2=60
                                   ,beta_0=-50,beta_1=3,beta_2=3,beta_3=1,beta_a=-0.2,p=10))
  output9 <- rbind(output9,work_H1(model="weibull",n=100,m=m,c1=20,c2=20
                                   ,beta_0=-50,beta_1=3,beta_2=3,beta_3=1,beta_a=-0.2,p=10))
  output9 <- rbind(output9,work_H1(model="weibull",n=100,m=m,c1=30,c2=30
                                   ,beta_0=-50,beta_1=3,beta_2=3,beta_3=1,beta_a=-0.2,p=10))
  output9 <- rbind(output9,work_H1(model="weibull",n=100,m=m,c1=60,c2=60
                                   ,beta_0=-50,beta_1=3,beta_2=3,beta_3=1,beta_a=-0.2,p=10))

  save(output9,file = 'test.rda')
  
  sfStop()

}

if(main_result){
### typeI error
sfInit(parallel = TRUE, cpus = detectCores())
sfLibrary(survival)
sfLibrary(dplyr)
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/function.R')
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/data_generation.R')
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/pvalue_result.R')


dir_name = '/public/home/shiyifeng/PairedSurv/Rcode_server/result/tpIerror'
if(!dir.exists(dir_name)){
  dir.create(dir_name)
}
setwd(dir_name)

m=1000
set.seed(1)
output0 <- data.frame()
output0 <- rbind(output0,work_H0(model="add_weibull",n=100,m=m,c1=40,c2=50
                                 ,scale=8,shape=2))
output0 <- rbind(output0,work_H0(model="add_gompertz",n=100,m=m,c1=40,c2=50
                                 ,scale=8,shape=2))
output0 <- rbind(output0,work_H0(model="add_lognormal",n=100,m=m,c1=40,c2=50
                                 ,scale=2,shape=2))

output0 <- rbind(output0,work_H0(model="exp",n=100,m=m,c1=5,c2=6,
                                 beta_0=-0.5,beta_1=0.1,beta_2=-0.4,beta_3=-0.14))
output0 <- rbind(output0,work_H0(model="lognormal",n=100,m=m,c1=10,c2=12,
                                 beta_0=-0.5,beta_1=0.1,beta_2=-0.4,beta_3=-0.14,sigma=2))
output0 <- rbind(output0,work_H0(model="weibull",n=100,m=m,c1=3,c2=4,
                                 beta_0=-0.5,beta_1=0.1,beta_2=-0.4,beta_3=-0.14,p=3))
output0 <- rbind(output0,work_H0(model="Gompertz",n=100,m=m,c1=4,c2=6,
                                 beta_0=-0.5,beta_1=0.1,beta_2=-0.4,beta_3=-0.14,k=3))
output0 <- rbind(output0,work_H0(model="log_logistic",n=100,m=m,c1=5,c2=6,
                                 beta_0=-0.5,beta_1=0.1,beta_2=-0.4,beta_3=-0.14,pl=2))
save(output0,file = 'H0.rda')

sfStop()


###power  add_weibull 
sfInit(parallel = TRUE, cpus = detectCores())
sfLibrary(survival)
sfLibrary(dplyr)
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/function.R')
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/data_generation.R')
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/pvalue_result.R')
dir_name = '/public/home/shiyifeng/PairedSurv/Rcode_server/result/add_weibull'
if(!dir.exists(dir_name)){
  dir.create(dir_name)
}
setwd(dir_name)

m=1000
set.seed(1)
output1 <- data.frame()
output1 <- rbind(output1,work_H1(model="add_weibull",n=30,m=m,c1=150,c2=150
                                 ,scale1=8,shape1=2,scale2=5,shape2=4))
output1 <- rbind(output1,work_H1(model="add_weibull",n=50,m=m,c1=150,c2=150
                                 ,scale1=8,shape1=2,scale2=5,shape2=4))
output1 <- rbind(output1,work_H1(model="add_weibull",n=100,m=m,c1=150,c2=150
                                 ,scale1=8,shape1=2,scale2=5,shape2=4))
output1 <- rbind(output1,work_H1(model="add_weibull",n=300,m=m,c1=150,c2=150
                                 ,scale1=8,shape1=2,scale2=5,shape2=4))
output1 <- rbind(output1,work_H1(model="add_weibull",n=500,m=m,c1=150,c2=150
                                 ,scale1=8,shape1=2,scale2=5,shape2=4))

output1 <- rbind(output1,work_H1(model="add_weibull",n=100,m=m,c1=60,c2=60
                                 ,scale1=8,shape1=2,scale2=5,shape2=4))
output1 <- rbind(output1,work_H1(model="add_weibull",n=100,m=m,c1=100,c2=100
                                 ,scale1=8,shape1=2,scale2=5,shape2=4))
output1 <- rbind(output1,work_H1(model="add_weibull",n=100,m=m,c1=150,c2=150
                                 ,scale1=8,shape1=2,scale2=5,shape2=4))
output1 <- rbind(output1,work_H1(model="add_weibull",n=100,m=m,c1=300,c2=300
                                 ,scale1=8,shape1=2,scale2=5,shape2=4))
output1 <- rbind(output1,work_H1(model="add_weibull",n=100,m=m,c1=900,c2=900
                                 ,scale1=8,shape1=2,scale2=5,shape2=4))

save(output1,file = 'add_weibull.rda')

sfStop()


### power add_gompertz
sfInit(parallel = TRUE, cpus = detectCores())
sfLibrary(survival)
sfLibrary(dplyr)
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/function.R')
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/data_generation.R')
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/pvalue_result.R')
dir_name = '/public/home/shiyifeng/PairedSurv/Rcode_server/result/add_gompertz'
if(!dir.exists(dir_name)){
  dir.create(dir_name)
}
setwd(dir_name)

m=1000
set.seed(1)
output2 <- data.frame()
output2 <- rbind(output2,work_H1(model="add_gompertz",n=30,m=m,c1=150,c2=150
                                 ,scale1=8,shape1=2,scale2=3,shape2=1/2))
output2 <- rbind(output2,work_H1(model="add_gompertz",n=50,m=m,c1=150,c2=150
                                 ,scale1=8,shape1=2,scale2=3,shape2=1/2))
output2 <- rbind(output2,work_H1(model="add_gompertz",n=100,m=m,c1=150,c2=150
                                 ,scale1=8,shape1=2,scale2=3,shape2=1/2))
output2 <- rbind(output2,work_H1(model="add_gompertz",n=300,m=m,c1=150,c2=150
                                 ,scale1=8,shape1=2,scale2=3,shape2=1/2))
output2 <- rbind(output2,work_H1(model="add_gompertz",n=500,m=m,c1=150,c2=150
                                 ,scale1=8,shape1=2,scale2=3,shape2=1/2))

output2 <- rbind(output2,work_H1(model="add_gompertz",n=100,m=m,c1=60,c2=60
                                 ,scale1=8,shape1=2,scale2=3,shape2=1/2))
output2 <- rbind(output2,work_H1(model="add_gompertz",n=100,m=m,c1=100,c2=100
                                 ,scale1=8,shape1=2,scale2=3,shape2=1/2))
output2 <- rbind(output2,work_H1(model="add_gompertz",n=100,m=m,c1=150,c2=150
                                 ,scale1=8,shape1=2,scale2=3,shape2=1/2))
output2 <- rbind(output2,work_H1(model="add_gompertz",n=100,m=m,c1=300,c2=300
                                 ,scale1=8,shape1=2,scale2=3,shape2=1/2))
output2 <- rbind(output2,work_H1(model="add_gompertz",n=100,m=m,c1=900,c2=900
                                 ,scale1=8,shape1=2,scale2=3,shape2=1/2))

save(output2,file = 'add_gompertz.rda')

sfStop()


### power add_lognormal
sfInit(parallel = TRUE, cpus = detectCores())
sfLibrary(survival)
sfLibrary(dplyr)
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/function.R')
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/data_generation.R')
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/pvalue_result.R')
dir_name = '/public/home/shiyifeng/PairedSurv/Rcode_server/result/add_lognormal'
if(!dir.exists(dir_name)){
  dir.create(dir_name)
}
setwd(dir_name)

m=1000
set.seed(1)
output3 <- data.frame()
output3 <- rbind(output3,work_H1(model="add_lognormal",n=30,m=m,c1=150,c2=150
                                 ,scale1=-1,shape1=1/4,scale2=1.5,shape2=1/4))
output3 <- rbind(output3,work_H1(model="add_lognormal",n=50,m=m,c1=150,c2=150
                                 ,scale1=-1,shape1=1/4,scale2=1.5,shape2=1/4))
output3 <- rbind(output3,work_H1(model="add_lognormal",n=100,m=m,c1=150,c2=150
                                 ,scale1=-1,shape1=1/4,scale2=1.5,shape2=1/4))
output3 <- rbind(output3,work_H1(model="add_lognormal",n=300,m=m,c1=150,c2=150
                                 ,scale1=-1,shape1=1/4,scale2=1.5,shape2=1/4))
output3 <- rbind(output3,work_H1(model="add_lognormal",n=500,m=m,c1=150,c2=150
                                 ,scale1=-1,shape1=1/4,scale2=1.5,shape2=1/4))

output3 <- rbind(output3,work_H1(model="add_lognormal",n=100,m=m,c1=20,c2=20
                                 ,scale1=-1,shape1=1/4,scale2=1.5,shape2=1/4))
output3 <- rbind(output3,work_H1(model="add_lognormal",n=100,m=m,c1=90,c2=90
                                 ,scale1=-1,shape1=1/4,scale2=1.5,shape2=1/4))
output3 <- rbind(output3,work_H1(model="add_lognormal",n=100,m=m,c1=150,c2=150
                                 ,scale1=-1,shape1=1/4,scale2=1.5,shape2=1/4))
output3 <- rbind(output3,work_H1(model="add_lognormal",n=100,m=m,c1=300,c2=300
                                 ,scale1=-1,shape1=1/4,scale2=1.5,shape2=1/4))
output3 <- rbind(output3,work_H1(model="add_lognormal",n=100,m=m,c1=500,c2=500
                                 ,scale1=-1,shape1=1/4,scale2=1.5,shape2=1/4))

save(output3,file = 'add_lognormal.rda')

sfStop()



### power exp
sfInit(parallel = TRUE, cpus = detectCores())
sfLibrary(survival)
sfLibrary(dplyr)
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/function.R')
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/data_generation.R')
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/pvalue_result.R')

dir_name = '/public/home/shiyifeng/PairedSurv/Rcode_server/result/exp'
if(!dir.exists(dir_name)){
  dir.create(dir_name)
}
setwd(dir_name)

m=1000
set.seed(1)
# change para 

output4 <- data.frame() # ??????????λ??
output4 <- rbind(output4,work_H1(model="exp",n=30,m=m,c1=50,c2=60
                                 ,beta_0=-2.5,beta_1=-0.5,beta_2=0.2,beta_3=0.4,beta_a=-0.8))

output4 <- rbind(output4,work_H1(model="exp",n=50,m=m,c1=100,c2=110
                                 ,beta_0=-2.5,beta_1=-0.5,beta_2=0.2,beta_3=0.4,beta_a=-1.6))

output4 <- rbind(output4,work_H1(model="exp",n=100,m=m,c1=60,c2=70
                                 ,beta_0=-5.5,beta_1=-0.5,beta_2=0.2,beta_3=0.4,beta_a=-0.4))

output4 <- rbind(output4,work_H1(model="exp",n=300,m=m,c1=100,c2=110
                                 ,beta_0=-2.5,beta_1=-0.5,beta_2=0.2,beta_3=0.4,beta_a=-1.6))
output4 <- rbind(output4,work_H1(model="exp",n=500,m=m,c1=100,c2=110
                                 ,beta_0=-2.5,beta_1=-0.5,beta_2=0.2,beta_3=0.4,beta_a=-1.6))

output4 <- rbind(output4,work_H1(model="exp",n=100,m=m,c1=50,c2=50
                                 ,beta_0=-2.5,beta_1=-0.5,beta_2=0.2,beta_3=0.4,beta_a=-1.6))
output4 <- rbind(output4,work_H1(model="exp",n=100,m=m,c1=100,c2=100
                                 ,beta_0=-2.5,beta_1=-0.5,beta_2=0.2,beta_3=0.4,beta_a=-1.6))
output4 <- rbind(output4,work_H1(model="exp",n=100,m=m,c1=200,c2=200
                                 ,beta_0=-2.5,beta_1=-0.5,beta_2=0.2,beta_3=0.4,beta_a=-1.6))
output4 <- rbind(output4,work_H1(model="exp",n=100,m=m,c1=400,c2=400
                                 ,beta_0=-2.5,beta_1=-0.5,beta_2=0.2,beta_3=0.4,beta_a=-1.6))
output4 <- rbind(output4,work_H1(model="exp",n=100,m=m,c1=500,c2=500
                                 ,beta_0=-2.5,beta_1=-0.5,beta_2=0.2,beta_3=0.4,beta_a=-1.6))
save(output4,file = 'exp.rda')

sfStop()



### power lognormal
sfInit(parallel = TRUE, cpus = detectCores())
sfLibrary(survival)
sfLibrary(dplyr)
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/function.R')
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/data_generation.R')
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/pvalue_result.R')
dir_name = '/public/home/shiyifeng/PairedSurv/Rcode_server/result/lognormal'
if(!dir.exists(dir_name)){
  dir.create(dir_name)
}
setwd(dir_name)

m=1000
set.seed(1)
output5 <- data.frame()
output5 <- rbind(output5,work_H1(model="lognormal",n=30,m=m,c1=10,c2=12
                                 ,beta_0=-0.5,beta_1=2,beta_2=-2,beta_3=-1.4,beta_a=-0.5,sigma=1/4))
output5 <- rbind(output5,work_H1(model="lognormal",n=50,m=m,c1=10,c2=12
                                 ,beta_0=-0.5,beta_1=2,beta_2=-2,beta_3=-1.4,beta_a=-0.5,sigma=1/4))
output5 <- rbind(output5,work_H1(model="lognormal",n=100,m=m,c1=10,c2=12
                                 ,beta_0=-0.5,beta_1=2,beta_2=-2,beta_3=-1.4,beta_a=-0.5,sigma=1/4))
output5 <- rbind(output5,work_H1(model="lognormal",n=300,m=m,c1=10,c2=12
                                 ,beta_0=-0.5,beta_1=2,beta_2=-2,beta_3=-1.4,beta_a=-0.5,sigma=1/4))
output5 <- rbind(output5,work_H1(model="lognormal",n=500,m=m,c1=10,c2=12
                                 ,beta_0=-0.5,beta_1=2,beta_2=-2,beta_3=-1.4,beta_a=-0.5,sigma=1/4))


 
output5 <- rbind(output5,work_H1(model="lognormal",n=100,m=m,c1=3,c2=3
                                 ,beta_0=-0.5,beta_1=2,beta_2=-2,beta_3=-1.4,beta_a=-0.5,sigma=1/4))
output5 <- rbind(output5,work_H1(model="lognormal",n=100,m=m,c1=5,c2=5
                                 ,beta_0=-0.5,beta_1=2,beta_2=-2,beta_3=-1.4,beta_a=-0.5,sigma=1/4))
output5 <- rbind(output5,work_H1(model="lognormal",n=100,m=m,c1=10,c2=10
                                 ,beta_0=-0.5,beta_1=2,beta_2=-2,beta_3=-1.4,beta_a=-0.5,sigma=1/4))
output5 <- rbind(output5,work_H1(model="lognormal",n=100,m=m,c1=60,c2=60
                                 ,beta_0=-0.5,beta_1=2,beta_2=-2,beta_3=-1.4,beta_a=-0.5,sigma=1/4))
output5 <- rbind(output5,work_H1(model="lognormal",n=100,m=m,c1=150,c2=150
                                 ,beta_0=-0.5,beta_1=2,beta_2=-2,beta_3=-1.4,beta_a=-0.5,sigma=1/4))
save(output5,file = 'lognormal.rda')

sfStop()



### power weibull
sfInit(parallel = TRUE, cpus = detectCores())
sfLibrary(survival)
sfLibrary(dplyr)
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/function.R')
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/data_generation.R')
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/pvalue_result.R')
dir_name = '/public/home/shiyifeng/PairedSurv/Rcode_server/result/weibull'
if(!dir.exists(dir_name)){
  dir.create(dir_name)
}
setwd(dir_name)

m=1000
set.seed(1)
output6 <- data.frame()
output6 <- rbind(output6,work_H1(model="weibull",n=30,m=m,c1=10000,c2=10000
                                 ,beta_0=7.5,beta_1=2.5,beta_2=-2,beta_3=-1.4,beta_a=-0.2,p=10))
output6 <- rbind(output6,work_H1(model="weibull",n=50,m=m,c1=10000,c2=10000
                                 ,beta_0=7.5,beta_1=2.5,beta_2=-2,beta_3=-1.4,beta_a=-0.2,p=10))
output6 <- rbind(output6,work_H1(model="weibull",n=100,m=m,c1=10000,c2=10000
                                 ,beta_0=7.5,beta_1=2.5,beta_2=-2,beta_3=-1.4,beta_a=-0.2,p=10))
output6 <- rbind(output6,work_H1(model="weibull",n=300,m=m,c1=10000,c2=10000
                                 ,beta_0=7.5,beta_1=2.5,beta_2=-2,beta_3=-1.4,beta_a=-0.2,p=10))
output6 <- rbind(output6,work_H1(model="weibull",n=500,m=m,c1=10000,c2=10000
                                 ,beta_0=7.5,beta_1=2.5,beta_2=-2,beta_3=-1.4,beta_a=-0.2,p=10))

output6 <- rbind(output6,work_H1(model="weibull",n=100,m=m,c1=1000,c2=1000
                                 ,beta_0=7.5,beta_1=2.5,beta_2=-2,beta_3=-1.4,beta_a=-0.2,p=10))
output6 <- rbind(output6,work_H1(model="weibull",n=100,m=m,c1=5000,c2=5000
                                 ,beta_0=7.5,beta_1=2.5,beta_2=-2,beta_3=-1.4,beta_a=-0.2,p=10))
output6 <- rbind(output6,work_H1(model="weibull",n=100,m=m,c1=10000,c2=10000
                                 ,beta_0=7.5,beta_1=2.5,beta_2=-2,beta_3=-1.4,beta_a=-0.2,p=10))
output6 <- rbind(output6,work_H1(model="weibull",n=100,m=m,c1=30000,c2=30000
                                 ,beta_0=7.5,beta_1=2.5,beta_2=-2,beta_3=-1.4,beta_a=-0.2,p=10))
output6 <- rbind(output6,work_H1(model="weibull",n=100,m=m,c1=100000,c2=100000
                                 ,beta_0=7.5,beta_1=2.5,beta_2=-2,beta_3=-1.4,beta_a=-0.2,p=10))
save(output6,file = 'weibull.rda')

sfStop()



### power gompertz
sfInit(parallel = TRUE, cpus = detectCores())
sfLibrary(survival)
sfLibrary(dplyr)
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/function.R')
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/data_generation.R')
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/pvalue_result.R')
dir_name = '/public/home/shiyifeng/PairedSurv/Rcode_server/result/gompertz'
if(!dir.exists(dir_name)){
  dir.create(dir_name)
}
setwd(dir_name)

m=1000
set.seed(1)
output7 <- data.frame()
output7 <- rbind(output7,work_H1(model="Gompertz",n=30,m=m,c1=30,c2=30
                                 ,beta_0=-5,beta_1=2.5,beta_2=-2,beta_3=-1.4,beta_a=-0.5,k=2))
output7 <- rbind(output7,work_H1(model="Gompertz",n=50,m=m,c1=30,c2=30
                                 ,beta_0=-5,beta_1=2.5,beta_2=-2,beta_3=-1.4,beta_a=-0.5,k=2))
output7 <- rbind(output7,work_H1(model="Gompertz",n=100,m=m,c1=30,c2=30
                                 ,beta_0=-5,beta_1=2.5,beta_2=-2,beta_3=-1.4,beta_a=-0.5,k=2))
output7 <- rbind(output7,work_H1(model="Gompertz",n=300,m=m,c1=30,c2=30
                                 ,beta_0=-5,beta_1=2.5,beta_2=-2,beta_3=-1.4,beta_a=-0.5,k=2))
output7 <- rbind(output7,work_H1(model="Gompertz",n=500,m=m,c1=30,c2=30
                                 ,beta_0=-5,beta_1=2.5,beta_2=-2,beta_3=-1.4,beta_a=-0.5,k=2))

output7 <- rbind(output7,work_H1(model="Gompertz",n=100,m=m,c1=10,c2=10
                                 ,beta_0=-5,beta_1=2.5,beta_2=-2,beta_3=-1.4,beta_a=-0.5,k=2))
output7 <- rbind(output7,work_H1(model="Gompertz",n=100,m=m,c1=15,c2=15
                                 ,beta_0=-5,beta_1=2.5,beta_2=-2,beta_3=-1.4,beta_a=-0.5,k=2))
output7 <- rbind(output7,work_H1(model="Gompertz",n=100,m=m,c1=30,c2=30
                                 ,beta_0=-5,beta_1=2.5,beta_2=-2,beta_3=-1.4,beta_a=-0.5,k=2))
output7 <- rbind(output7,work_H1(model="Gompertz",n=100,m=m,c1=50,c2=50
                                 ,beta_0=-5,beta_1=2.5,beta_2=-2,beta_3=-1.4,beta_a=-0.5,k=2))
output7 <- rbind(output7,work_H1(model="Gompertz",n=100,m=m,c1=100,c2=100
                                 ,beta_0=-5,beta_1=2.5,beta_2=-2,beta_3=-1.4,beta_a=-0.5,k=2))
save(output7,file = 'Gompertz.rda')

sfStop()


### power log_logistic
sfInit(parallel = TRUE, cpus = detectCores())
sfLibrary(survival)
sfLibrary(dplyr)
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/function.R')
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/data_generation.R')
sfSource('/public/home/shiyifeng/PairedSurv/Rcode_server/pvalue_result.R')
dir_name = '/public/home/shiyifeng/PairedSurv/Rcode_server/result/log_logistic'
if(!dir.exists(dir_name)){
  dir.create(dir_name)
}
setwd(dir_name)

m=1000
set.seed(1)
# change para

output8 <- data.frame()
output8 <- rbind(output8,work_H1(model="log_logistic",n=30,m=m,c1=5000,c2=200
                                 ,beta_0=-10.5,beta_1=2,beta_2=-2,beta_3=-1.4,beta_a=-1,pl=2))
output8 <- rbind(output8,work_H1(model="log_logistic",n=50,m=m,c1=5000,c2=200
                                 ,beta_0=-10.5,beta_1=2,beta_2=-2,beta_3=-1.4,beta_a=-1,pl=2))
output8 <- rbind(output8,work_H1(model="log_logistic",n=100,m=m,c1=5000,c2=200
                                 ,beta_0=-10.5,beta_1=2,beta_2=-2,beta_3=-1.4,beta_a=-1,pl=2))
output8 <- rbind(output8,work_H1(model="log_logistic",n=300,m=m,c1=5000,c2=200
                                 ,beta_0=-10.5,beta_1=2,beta_2=-2,beta_3=-1.4,beta_a=-1,pl=2))
output8 <- rbind(output8,work_H1(model="log_logistic",n=500,m=m,c1=5000,c2=200
                                 ,beta_0=-10.5,beta_1=2,beta_2=-2,beta_3=-1.4,beta_a=-1,pl=2))

output8 <- rbind(output8,work_H1(model="log_logistic",n=100,m=m,c1=1000,c2=1000
                                 ,beta_0=-10.5,beta_1=2,beta_2=-2,beta_3=-1.4,beta_a=-1,pl=2))
output8 <- rbind(output8,work_H1(model="log_logistic",n=100,m=m,c1=2000,c2=2000
                                 ,beta_0=-10.5,beta_1=2,beta_2=-2,beta_3=-1.4,beta_a=-1,pl=2))
output8 <- rbind(output8,work_H1(model="log_logistic",n=100,m=m,c1=5000,c2=5000
                                 ,beta_0=-10.5,beta_1=2,beta_2=-2,beta_3=-1.4,beta_a=-1,pl=2))
output8 <- rbind(output8,work_H1(model="log_logistic",n=100,m=m,c1=7000,c2=7000
                                 ,beta_0=-10.5,beta_1=2,beta_2=-2,beta_3=-1.4,beta_a=-1,pl=2))
output8 <- rbind(output8,work_H1(model="log_logistic",n=100,m=m,c1=10000,c2=10000
                                 ,beta_0=-10.5,beta_1=2,beta_2=-2,beta_3=-1.4,beta_a=-1,pl=2))
save(output8,file = 'log_logistic.rda')

sfStop()



setwd('/public/home/shiyifeng/PairedSurv/Rcode_server/result') 
data_name<-gsub(" ","",gsub("-","",substr(Sys.time(),6,13)))
#save(output0,output8,
#  file = paste(data_name,'all.rda',sep=''))
save(output0,output1,output2,output3,output4,output5,output6,output7,output8,
     file = paste(data_name,'all.rda',sep=''))
}









