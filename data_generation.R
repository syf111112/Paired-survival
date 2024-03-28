
#用于制造删失分布函???
{
  censor_status <- function(time, c){
    if(c==0){
      data = data.frame(time = time,
                        status = rep(1,length(time)))
    }
    else{
      cen = round(runif(length(time), min = 0, max = c),4)
      stat = as.numeric(time <= cen)
      data = data.frame(time = ifelse(time <= cen, time, cen),
                        status = stat)
    }
    #cen_rate <- round(1-sum(stat)/length(time),4)
    return(data)
  }
  
  censoring_status <- function(x_t, y_t, c1, c2){
    data1 <- censor_status(x_t,c1)
    data2 <- censor_status(y_t,c2)   ## use c1, c2 to control cen_rate
    x_i <- data1[,1]
    delta_xi <- data1[,2]
    y_i <- data2[,1]
    delta_yi <- data2[,2]
    survival_data <- data.frame(x_i=x_i,delta_xi = delta_xi,y_i = y_i,delta_yi = delta_yi)
    return(survival_data)
  }
  
}

#辅助函数
{
  weibull_gen <- function(n,scale,shape){
    #weibull_time = (-log(runif(n))/(scale))^(1/shape)
    return(rweibull(n,shape,scale))
    
  }
  
  gompertz_gen <- function(n,scale,shape){
    time  <- (1/ shape)*log(1- (shape/scale)*log(runif(n)))
  }
  
  exp_gen <- function(n,lambda){
    time  <- -(1/ lambda)*log(runif(n))
  }
  
  lognorm_gen <- function(n,lambda,sigma){
    time <- exp(sigma*rnorm(n))*lambda
  }
  lognorm_gen_test <- function(n,u,sigma){
    time <- exp(sigma*rnorm(n)+u+4)
  }
  
  weibull_gen2 <- function(n,lambda,p){
    time  <- (-(1/ lambda)*log(runif(n)))^(1/p)
  }
  Gompertz_gen <- function(n,lambda,k=3){
    Gamma = log(k)
    time = 1/Gamma *log(1+Gamma/lambda*(-log(runif(n))))
  }
  log_logistic_gen <- function(n,lambda,pl=2){
    time <- (1/lambda *(1/runif(n)-1))^(1/pl)
  }
  
  ### 20220922新增 check函数，剔除生存时间为0的数???
  check <- function(surv.data){
    xi = surv.data[,1]
    yi = surv.data[,3]
    if(min(xi)<0.0001 | min(yi)<0.0001)
      return(T)
    
    return(F)
  }
  
}

### H0情况???
{
#Formula2 <- parse(text = "exp(2*x+3)")
data_gen_h0 <- function(model="add_weibull",n=100,c1=40,c2=60,beta_0=-0.5,beta_1=0.1,
                        beta_2=-0.4,beta_3=-0.14,
                        scale=8,shape=2,sigma=2,p=3,k=3,pl=2){
  x1 <- rnorm(n,0,1)
  x2 <- rbinom(n,1,0.3)
  x3 <- rbinom(n,1,0.2)
  mx = exp(3-0.2*x1+0.5*x2)
  lambda1 <- exp(beta_0+beta_1*x1+beta_2*x2+beta_3*x3)
  lambda2 <- exp(beta_0+beta_1*x1+beta_2*x2+beta_3*x3)
  if(model=="add_weibull"){
    x_t = round(mx+weibull_gen(n,scale,shape),4)
    y_t = round(mx+weibull_gen(n,scale,shape),4)
  }
  else if(model=="add_gompertz"){
    x_t = round(mx+gompertz_gen(n,scale,shape),4)
    y_t = round(mx+gompertz_gen(n,scale,shape),4)
  }
  else if(model=="add_lognormal"){
    x_t = round(mx+rlnorm(n,scale,shape),4)
    y_t = round(mx+rlnorm(n,scale,shape),4)
  }
  else if(model=="exp"){
    x_t <- round(exp_gen(n,lambda1),4)
    y_t <- round(exp_gen(n,lambda2),4)
  }
  else if(model=="lognormal"){
    x_t <- round(lognorm_gen(n,lambda1,sigma),4)
    y_t <- round(lognorm_gen(n,lambda2,sigma),4)
  }
  else if(model=="weibull"){
    x_t <- round(weibull_gen2(n,lambda1,p),4)
    y_t <- round(weibull_gen2(n,lambda2,p),4)
  }
  else if(model=="Gompertz"){
    x_t <- round(Gompertz_gen(n,lambda1,k),4)
    y_t <- round(Gompertz_gen(n,lambda2,k),4)
  }
  else if(model=="log_logistic"){
    x_t <- round(log_logistic_gen(n,lambda1,pl),4)
    y_t <- round(log_logistic_gen(n,lambda2,pl),4)
  }
  
  data <- censoring_status(x_t,y_t,c1,c2)

}

}

### H1情况下：
{
  #Formula2 <- parse(text = "exp(2*x+3)")
  data_gen_h1 <- function(model="add_weibull",n=100,c1=40,c2=60,beta_0=-0.5,beta_1=0.1,
                          beta_2=-0.4,beta_3=-0.14,beta_a=-0.2,
                          scale1=8,shape1=2,scale2=5,shape2=2,sigma=2,p=3,k=3,pl=2){
    x1 <- rnorm(n,0,1)
    x2 <- rbinom(n,1,0.3)
    x3 <- rbinom(n,1,0.2)
    mx = exp(3+2*x1)
    lambda1 <- exp(beta_0+beta_1*x1+beta_2*x2+beta_3*x3+beta_a)
    lambda2 <- exp(beta_0+beta_1*x1+beta_2*x2+beta_3*x3)
    if(model=="add_weibull"){
      x_t = round(mx+weibull_gen(n,scale1,shape1),4)
      y_t = round(mx+weibull_gen(n,scale2,shape2),4)
    }
    else if(model=="add_gompertz"){
      x_t = round(mx+gompertz_gen(n,scale1,shape1),4)
      y_t = round(mx+gompertz_gen(n,scale2,shape2),4)
    }
    else if(model=="add_lognormal"){
      x_t = round(mx+rlnorm(n,c(scale1,shape1)),4)
      y_t = round(mx+rlnorm(n,c(scale2,shape2)),4)
    }
    else if(model=="exp"){
      x_t <- round(exp_gen(n,lambda1),4)
      y_t <- round(exp_gen(n,lambda2),4)
    }
    else if(model=="lognormal"){
      x_t <- round(lognorm_gen(n,lambda1,sigma),4)
      y_t <- round(lognorm_gen(n,lambda2,sigma),4)
    }
    else if(model=="weibull"){
      x_t <- round(weibull_gen2(n,lambda1,p),4)
      y_t <- round(weibull_gen2(n,lambda2,p),4)
    }
    else if(model=="Gompertz"){
      x_t <- round(Gompertz_gen(n,lambda1,k),4)
      y_t <- round(Gompertz_gen(n,lambda2,k),4)
    }
    else if(model=="log_logistic"){
      x_t <- round(log_logistic_gen(n,lambda1,pl),4)
      y_t <- round(log_logistic_gen(n,lambda2,pl),4)
    }
    data <- censoring_status(x_t,y_t,c1,c2)

  }

}




