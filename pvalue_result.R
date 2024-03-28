### 该函数使用了snowfall的sfSapply编写,所以无法单独载入哦~ 
### 如果需要本地跑，需要将所有的sfSapply改为sapply(注意大小写)
### 与原版不同，这里统一了n为样本量，m为进行模拟的次数


work_H0 <- function(model="add_weibull",n=100,m=1000,c1=40,c2=60,beta_0=-0.5,beta_1=0.1,
                            beta_2=-0.4,beta_3=-0.14,
                            scale=8,shape=2,sigma=2,p=3,k=k,pl=pl){
  p1 <- sfSapply(1:m,function(x){
    data <-data_gen_h0(model=model,n=n,c1=c1,c2=c2,beta_0=beta_0,beta_1=beta_1,
                       beta_2=beta_2,beta_3=beta_3,
                       scale=scale,shape=shape,sigma=sigma,p=p,k=k,pl=pl)
    while(check(data)) {
      data <-data_gen_h0(model=model,n=n,c1=c1,c2=c2,beta_0=beta_0,beta_1=beta_1,
                         beta_2=beta_2,beta_3=beta_3,
                         scale=scale,shape=shape,sigma=sigma,p=p)
    }
    ## 这里是为了剔除生存时间为0的数据，避免log转换遇到麻烦
    
    GSR_test_log = GSR_log(data$x_i,data$delta_xi,data$y_i,data$delta_yi)
    GSR_test_time = GSR(data$x_i,data$delta_xi,data$y_i,data$delta_yi)
    Gehan_test = Gehan(data$x_i,data$delta_xi,data$y_i,data$delta_yi)
    PPW_test = PPW(data$x_i,data$delta_xi,data$y_i,data$delta_yi)

    p_Z_GSR_log <- 2*(1-pnorm(abs(GSR_test_log$Z_GSR)))
    p_Z_GSR_time <- 2*(1-pnorm(abs(GSR_test_time$Z_GSR)))
    p_Z_gehan <- 2*(1-pnorm(abs(Gehan_test$Z_gehan)))
    p_Z_ppw <- 2*(1-pnorm(abs(PPW_test$Z_ppw)))
    p_Z_mppw <- mPPW(data$x_i,data$delta_xi,data$y_i,data$delta_yi)
    p_Akritas <- Akritas(data)$p.value
    #   p_D2[i] <- bootstracp_survED(data$x_i,data$delta_xi,data$y_i,data$delta_yi,199)
    p_D1 <- bootstracp_survED(data$x_i,data$delta_xi,data$y_i,data$delta_yi,199,F)
    p_D1_trans <- bootstracp_survED(data$x_i,data$delta_xi,data$y_i,data$delta_yi,199,T)
    return(c(p_Z_GSR_log,p_Z_GSR_time,p_Z_gehan,p_Z_ppw,p_Z_mppw,p_Akritas,p_D1,p_D1_trans))
  })
  
  error1_GSR_log = mean(p1[1,]<0.05)
  error1_GSR_time = mean(p1[2,]<0.05)
  error1_Gehan = mean(p1[3,]<0.05)
  error1_Z_ppw = mean(p1[4,]<0.05)
  error1_Z_mppw = mean(p1[5,]<0.05)
  error1_Akritas = mean(p1[6,]<0.05)
  error1_D1 = mean(p1[7,]<0.05)
  error1_D1_trans = mean(p1[8,]<0.05)
  data <-data_gen_h0(model=model,n=n,c1=c1,c2=c2,beta_0=beta_0,beta_1=beta_1,
                     beta_2=beta_2,beta_3=beta_3,
                     scale=scale,shape=shape,sigma=sigma,p=p,k=k,pl=pl)
  
  censor_rate_x <- 1-mean(data$delta_xi)
  censor_rate_y <- 1-mean(data$delta_yi)
  k = paste(model,c1,c2,n,m, sep = ",", collapse = "")
  
  output2 <- data.frame(censor_rate_x,censor_rate_y,k,error1_GSR_log,
                        error1_GSR_time,error1_Gehan,error1_Z_ppw, 
                        error1_Z_mppw,error1_Akritas,error1_D1,error1_D1_trans)
}



work_H1 <- function(model="add_weibull",n=100,m=1000,c1=40,c2=60,beta_0=-0.5,beta_1=0.1,
                    beta_2=-0.4,beta_3=-0.14,beta_a=-0.2,
                    scale1=8,shape1=2,scale2=5,shape2=2,sigma=2,p=3,k=k,pl=pl){
  p1 <- sfSapply(1:m,function(x){
    data <-data_gen_h1(model=model,n=n,c1=c1,c2=c2,beta_0=beta_0,beta_1=beta_1,
                       beta_2=beta_2,beta_3=beta_3,beta_a=beta_a,
                       scale1=scale1,shape1=shape1,scale2=scale2,shape2=shape2
                       ,sigma=sigma,p=p,k=k,pl=pl)
    while(check(data)){
      data <-data_gen_h1(model=model,n=n,c1=c1,c2=c2,beta_0=beta_0,beta_1=beta_1,
                         beta_2=beta_2,beta_3=beta_3,beta_a=beta_a,
                         scale1=scale1,shape1=shape1,scale2=scale2,shape2=shape2
                         ,sigma=sigma,p=p)
    }
    
    GSR_test_log = GSR_log(data$x_i,data$delta_xi,data$y_i,data$delta_yi)
    GSR_test_time = GSR(data$x_i,data$delta_xi,data$y_i,data$delta_yi)
    Gehan_test = Gehan(data$x_i,data$delta_xi,data$y_i,data$delta_yi)
    PPW_test = PPW(data$x_i,data$delta_xi,data$y_i,data$delta_yi)
    
    p_Z_GSR_log <- 2*(1-pnorm(abs(GSR_test_log$Z_GSR)))
    p_Z_GSR_time <- 2*(1-pnorm(abs(GSR_test_time$Z_GSR)))
    p_Z_gehan <- 2*(1-pnorm(abs(Gehan_test$Z_gehan)))
    p_Z_ppw <- 2*(1-pnorm(abs(PPW_test$Z_ppw)))
    p_Z_mppw <- mPPW(data$x_i,data$delta_xi,data$y_i,data$delta_yi)
    p_Akritas <- Akritas(data)$p.value
    #   p_D2[i] <- bootstracp_survED(data$x_i,data$delta_xi,data$y_i,data$delta_yi,199)
    p_D1 <- bootstracp_survED(data$x_i,data$delta_xi,data$y_i,data$delta_yi,199,F)
    p_D1_trans <- bootstracp_survED(data$x_i,data$delta_xi,data$y_i,data$delta_yi,199,T)
    return(c(p_Z_GSR_log,p_Z_GSR_time,p_Z_gehan,p_Z_ppw,p_Z_mppw,p_Akritas,p_D1,p_D1_trans))
  })
  
  power_GSR_log = mean(p1[1,]<0.05)
  power_GSR_time = mean(p1[2,]<0.05)
  power_Gehan = mean(p1[3,]<0.05)
  power_Z_ppw = mean(p1[4,]<0.05)
  power_Z_mppw = mean(p1[5,]<0.05)
  power_Akritas = mean(p1[6,]<0.05)
  power_D1 = mean(p1[7,]<0.05)
  power_D1_trans = mean(p1[8,]<0.05)
  data <-data_gen_h1(model=model,n=n,c1=c1,c2=c2,beta_0=beta_0,beta_1=beta_1,
                     beta_2=beta_2,beta_3=beta_3,beta_a=beta_a,
                     scale1=scale1,shape1=shape1,scale2=scale2,shape2=shape2
                     ,sigma=sigma,p=p,k=k,pl=pl)
  
  censor_rate_x <- 1-mean(data$delta_xi)
  censor_rate_y <- 1-mean(data$delta_yi)
  k = paste(model,c1,c2,n,m, sep = ",", collapse = "")
  
  output2 <- data.frame(censor_rate_x,censor_rate_y,k,power_GSR_log,
                        power_GSR_time,power_Gehan,power_Z_ppw, 
                        power_Z_mppw,power_Akritas,power_D1,power_D1_trans)
}




