
### 辅助函数
{
  find_poi <- function(x,survtime){
    k = length(survtime)
    for(i in 1:k){
      if(x<survtime[i])
        return(i-1)
    }
    return(k)
    
  }   #x是一个时间点，survtime是向???  本函数用于计算两条生存曲线的平均??????
  #本函数返回x在y中的位置[y(i),y(i+1)]以找打s(y)的km估计值s(y(i))
  #data = data_gen1_1(100,Formula2,3,2,8,2,0.1)
  find_censored_rank2 <-  function(censored_survtime,t_j){
    for(i in 1: (length(t_j)-1))
      if(t_j[i]<= censored_survtime & t_j[i+1]>censored_survtime)
        return(i+1)
    return(length(t_j))
  }
  
  find_nj_pj <- function(vector0,Z){
    N = length(Z)
    n0=0;n = numeric(N)
    for(i in vector0){
      if(i > 0 & i < Z[1]) {
        n0=n0+1;next
      }
      else if(i >= Z[N]) {
        n[N] = n[N]+1;next
      }
      else{
        for(j in 1:N)
          if(i >= Z[j] & i < Z[j+1]) {
            n[j] = n[j]+1
            next
          }
      }
    }
    n = c(n0,n)
    return(n)          #注：此时的n[1]是n0?????? 
  }
  
find_nj_pj <- function(vector0,Z){
    N = length(Z)
    n0=0;n = numeric(N)
    if(N==0)
      return(0)
    
    for(i in vector0){
      if(i > 0 & i < Z[1]) {
        n0=n0+1;next
      }
      else if(i >= Z[N]) {
        n[N] = n[N]+1;next
      }
      else{
        for(j in 1:N)
          if(i >= Z[j] & i < Z[j+1]) {
            n[j] = n[j]+1
            next
          }
      }
    }
    n = c(n0,n)
    return(n)          
  }


  calc_Sc <- function(xi,delta_xi,yi,delta_yi){
    kmfit_x <- survfit((Surv(xi,1-delta_xi))~1)
    kmfit_y <- survfit((Surv(yi,1-delta_yi))~1)
    Sc_x <- kmfit_x$surv[find_rank(xi)]
    Sc_y <- kmfit_y$surv[find_rank(yi)]
    Sc = Sc_x*Sc_y
    return(Sc)
    #  wi <- outer(delta_xi*delta_yi,delta_xi*delta_yi,"*")/outer(Sc_x*Sc_y,Sc_x*Sc_y,"*")
  }
  
  find_rank <- function(x){
    u <- factor(x)           
    return(as.integer(u))
  }
  
  find_censored_rank <-  function(censored_survtime,t_j){
    for(i in 1: (length(t_j)-1))
      if(t_j[i]<= censored_survtime & t_j[i+1]>censored_survtime)
        return(i)
    
    return(length(t_j))
  }
}

#### 计算  算法函数
{
  Akritas <- function(data){
    xi = round(data[,1],3);delta_xi = data[,2]
    yi = round(data[,3],3);delta_yi = data[,4]
    X = Surv(xi,delta_xi==1)
    kmfit_x = survfit(X~1)
    Y = Surv(yi,delta_yi==1)
    kmfit_y = survfit(Y~1)          #k_M est for x and y
    n = length(xi)
    
    rank_xi = numeric(n)
    rank_yi = numeric(n)
    
    for(i in 1:n){
      # i=5
      poi_x1 <- which(kmfit_x$time==xi[i]);  surv_x1 <- kmfit_x$surv[poi_x1]
      poi_x2 <- find_poi(xi[i],kmfit_y$time)
      if(poi_x2==0) rank_xi[i] = (1+surv_x1)/2 else rank_xi[i] = (surv_x1+kmfit_y$surv[poi_x2])/2
      if(delta_xi[i]==1) rank_xi[i] = 2*n*(1-rank_xi[i]) else rank_xi[i] = 2*n*(1-0.5*rank_xi[i])
    }   #计算Akritas中的“rank??? xi
    
    for(i in 1:n){
      # i=5
      poi_y1 <- which(kmfit_y$time==yi[i]);  surv_y1 <- kmfit_y$surv[poi_y1]
      poi_y2 <- find_poi(yi[i],kmfit_x$time)
      if(poi_y2==0) rank_yi[i] = (1+surv_y1)/2 else rank_yi[i] = (surv_y1+kmfit_x$surv[poi_y2])/2
      if(delta_yi[i]==1) rank_yi[i] = 2*n*(1-rank_yi[i]) else rank_yi[i] = 2*n*(1-0.5*rank_yi[i])
    }  #计算Akritas中的“rank??? yi
    
    Akritas <- t.test(rank_xi,rank_yi,paired=T)
    
    return(Akritas)
  }
  
  Gehan <- function(xi,delta_xi,yi,delta_yi){
    n = 2*length(xi)
    num = c(1:n)
    survtime = c(xi,yi)
    censored = c(delta_xi,delta_yi)
    rank = numeric(length = n)                    #2.21修改
    data = data.frame(num,survtime,censored,rank)
    uncensored_num = data$num[data$censored==1]; N = length(uncensored_num)
    censored_num = data$num[data$censored==0]
    
    data$rank[uncensored_num] = rank(data$survtime[uncensored_num])
    t_j = sort(data$survtime[uncensored_num])
    
    for(k in censored_num)
      data$rank[k] = find_censored_rank2(data$survtime[k],t_j) 
    
    p1 <- vector(mode ="numeric",length = n)
    pg <- vector(mode ="numeric",length = n)
    n_j <- vector(mode ="numeric",length = N)
    for(i in 1:N)
      n_j[i] = sum(data$survtime>=t_j[i])
    
    
    for(i in 1:n)
      p1[i] = (data$rank[i]-1)/n
    for(i in 1:n){
      if(data$censored[i]==1)
        pg[i] = (n_j[data$rank[i]]-1)/n
      else pg[i] = 0
    }
    
    
    for(i in 1:n)
      data$k[i]=p1[i]-pg[i]
    k_xi = data$k[1:length(xi)]
    k_yi = data$k[(length(xi)+1):n]
    p1_xi = p1[1:length(xi)]
    p1_yi = p1[(length(xi)+1):n]
    pg_xi = pg[1:length(xi)]
    pg_yi = pg[(length(xi)+1):n]
    
    
    data_final = data.frame(xi,delta_xi,yi,delta_yi,p1_xi,pg_xi,p1_yi,pg_yi,k_xi,k_yi)
    data_final$Delta_i = data_final$k_xi-data_final$k_yi
    sum_delta = sum(data_final$Delta_i)
    sum_delta_2 = sum((data_final$Delta_i)^2)
    Z_gehan = sum_delta/sqrt(sum_delta_2)
    
    list(table = data_final,
         sum_delta=sum_delta,sum_delta_2=sum_delta_2,Z_gehan=Z_gehan)
  }
  
  GSR <- function(xi,delta_xi,yi,delta_yi){
    test.data = data.frame(xi,delta_xi,yi,delta_yi)
    uncensored.data = test.data[test.data[,2]==1 & test.data[,4]==1,]
    k = length(uncensored.data[,1])
    D_i = uncensored.data[,1]-uncensored.data[,3]
    Z = sort(abs(D_i))
    D_i_trans = D_i[order(abs(D_i))]
    d_j = vector(mode ="numeric",length = k)
    if(k>=1){
      for(j in 1:k)
      {
        if(D_i_trans[j]>0) d_j[j]=1
        else d_j[j] = 0
      }
    }
    censored.data = test.data[test.data[,2]==0 | test.data[,4]==0,]
    censored_difference = censored.data[,1]-censored.data[,3]
    abs_difference = abs(censored_difference)
    positive_difference = censored_difference[censored_difference>0]
    
    nj = find_nj_pj(abs_difference,Z);n0 = nj[1];n_j=0;
    if(length(nj)>=2)
      n_j= nj[2:length(nj)]
    
    pj = find_nj_pj(positive_difference,Z);p0 = pj[1];p_j=0
    if(length(pj)>=2)
      p_j= pj[2:length(pj)]
    
    
    if(k==0)
      m = 1
    if(k>=1){ 
      m = numeric(k)
      m[k]=n_j[k]+1
    }
    
    if(k>=2){                       
      for(i in seq(k-1,1,by=-1))
        m[i] = n_j[i]+1+m[i+1]
    }
    
    p = numeric(k)
    p[1] = m[1]/(m[1]+1)
    if(k>=2){
      for(i in 2:k)
        p[i] = m[i]/(m[i]+1)*p[i-1]
    }
    
    Z_GSR_up = sum((2*d_j-1)*(1-p))+1/2*(2*p0-n0)+sum((2*p_j-n_j)*(1-1/2*p))
    Z_GSR_down = sqrt(sum((1-p)^2)+sum(n_j*(1-1/2*p)^2)+1/4*n0)
    Z_GSR = Z_GSR_up/Z_GSR_down
    
    test.data$D_j = xi-yi
    
    
    list(table = test.data,k=k,Z_j = Z,d_j = d_j,nj = n_j,n0=n0,
         pj=p_j,p0=p0,mj=m,Pj = p,Z_GSR = Z_GSR)
  }
  
  GSR_log <- function(xi,delta_xi,yi,delta_yi){
    test.data = data.frame(xi,delta_xi,yi,delta_yi)
    uncensored.data = test.data[test.data[,2]==1 & test.data[,4]==1,]
    k = length(uncensored.data[,1])
    D_i = log(uncensored.data[,1])-log(uncensored.data[,3])
    Z = sort(abs(D_i))
    D_i_trans = D_i[order(abs(D_i))]
    d_j = vector(mode ="numeric",length = k)
    
    if(k>=1){
      for(j in 1:k)
      {
        if(D_i_trans[j]>0) d_j[j]=1
        else d_j[j] = 0
      }
    }
    
    censored.data = test.data[test.data[,2]==0 | test.data[,4]==0,]
    censored_difference = log(censored.data[,1])-log(censored.data[,3])
    abs_difference = abs(censored_difference)
    positive_difference = censored_difference[censored_difference>0]
    
    nj = find_nj_pj(abs_difference,Z);n0 = nj[1];n_j=0;
    if(length(nj)>=2)
      n_j= nj[2:length(nj)]
    
    pj = find_nj_pj(positive_difference,Z);p0 = pj[1];p_j=0
    if(length(pj)>=2)
      p_j= pj[2:length(pj)]
    
    if(k==0)
      m = 1
    if(k>=1){ 
      m = numeric(k)
      m[k]=n_j[k]+1
    }
    
    if(k>=2){
      for(i in seq(k-1,1,by=-1))
        m[i] = n_j[i]+1+m[i+1]
    }
    
    p = numeric(k)
    p[1] = m[1]/(m[1]+1)
    if(k>=2){
      for(i in 2:k)
        p[i] = m[i]/(m[i]+1)*p[i-1]
    }
    
    Z_GSR_up = sum((2*d_j-1)*(1-p))+1/2*(2*p0-n0)+sum((2*p_j-n_j)*(1-1/2*p))
    Z_GSR_down = sqrt(sum((1-p)^2)+sum(n_j*(1-1/2*p)^2)+1/4*n0)
    Z_GSR = Z_GSR_up/Z_GSR_down
    
    test.data$log_xi = log(xi)
    test.data$log_yi = log(yi)
    test.data$D_j = test.data$log_xi- test.data$log_yi
    
    
    list(table = test.data,k=k,Z_j = Z,d_j = d_j,nj = n_j,n0=n0,
         pj=p_j,p0=p0,mj=m,Pj = p,Z_GSR = Z_GSR)
  }
  
  
  
  survED <- function(xi,delta_xi,yi,delta_yi,logtrans = T){
    if(logtrans){
      xi = log(xi)
      yi = log(yi)
    }
    #  xi <- data[,1];delta_xi <- data[,2];yi <- data[,3];delta_yi <- data[,4]
    #  kmfit_x <- survfit((Surv(xi,1-delta_xi))~1)
    #  kmfit_y <- survfit((Surv(yi,1-delta_yi))~1)
    #  Sc_x <- kmfit_x$surv[find_rank(xi)]
    #  Sc_y <- kmfit_y$surv[find_rank(yi)]
    Sc = calc_Sc(xi,delta_xi,yi,delta_yi)
    wi <- outer(delta_xi*delta_yi,delta_xi*delta_yi,"*")/outer(Sc,Sc,"*")
    stat <- (abs(outer(xi-yi,xi-yi,"+"))-abs(outer(xi-yi,xi-yi,"-")))*wi
    stat[is.na(stat)] <- 0
    
    return(mean(stat))
  }
  
  bootstracp_survED <- function(xi,delta_xi,yi,delta_yi,B,logtrans = T){
    #Sc <- calc_Sc(xi,delta_xi,yi,delta_yi)
    nVn_sn <- survED(xi,delta_xi,yi,delta_yi,logtrans)
    
    nVn_b <- c()
    for(j in 1:B){
      n=length(xi)
      k = 1:n
      x_i <- c();y_i <- c();delta_x_i <- c();delta_y_i <- c();S_c <- c()
      for(i in 1:n){
        r = rbinom(1,1,0.5)
        x_i[i] = r*xi[k[i]]+(1-r)*yi[k[i]]
        y_i[i] = r*yi[k[i]]+(1-r)*xi[k[i]]
        delta_x_i[i] = r*delta_xi[k[i]]+(1-r)*delta_yi[k[i]]
        delta_y_i[i] = (1-r)*delta_xi[k[i]]+r*delta_yi[k[i]]
        #S_c[i] = Sc[k[i]]
      }
      nVn_b[j] <- survED(x_i,delta_x_i,y_i,delta_y_i,logtrans)
    }
    return((1+sum(nVn_b>nVn_sn))/(1+B))
  }
  
  PPW <- function(xi,delta_xi,yi,delta_yi){
    n = 2*length(xi)
    num = c(1:n)
    survtime = c(xi,yi)
    censored = c(delta_xi,delta_yi)
    rank = numeric(length = n)    #2.21修改
    data = data.frame(num,survtime,censored,rank)
    uncensored_num = data$num[data$censored==1]; N = length(uncensored_num)
    censored_num = data$num[data$censored==0]
    
    data$rank[uncensored_num] = rank(data$survtime[uncensored_num])
    t_j = sort(data$survtime[uncensored_num])
    
    for(k in censored_num)
      data$rank[k] = find_censored_rank(data$survtime[k],t_j)
    
    s <- vector(mode ="numeric",length = N)
    n_j <- vector(mode ="numeric",length = N)
    n_j[1] = sum(data$survtime>=t_j[1])  
    s[1] = n_j[1]/(n_j[1]+1)
    for(i in 2:N){
      n_j[i] = sum(data$survtime>=t_j[i]) 
      s[i] = n_j[i]/(n_j[i]+1) *s[i-1]
    }
    
    
    for(i in 1:n){
      if(data$censored[i]==1)
        data$k[i]=1-2*s[data$rank[i]]
      else data$k[i]=1-s[data$rank[i]]
    }
    
    k_xi = data$k[1:length(xi)]
    k_yi = data$k[(length(xi)+1):n]  
    
    data_final = data.frame(xi,delta_xi,yi,delta_yi,k_xi,k_yi)
    data_final$Delta_i = data_final$k_xi-data_final$k_yi
    sum_delta = sum(data_final$Delta_i)
    sum_delta_2 = sum((data_final$Delta_i)^2)
    Z_ppw = sum_delta/sqrt(sum_delta_2)
    
    list(table = data_final,t_j = t_j,n_j = n_j,s_j = s,
         sum_delta=sum_delta,sum_delta_2=sum_delta_2,Z_ppw=Z_ppw)
  }
  
  mPPW <- function(xi,delta_xi,yi,delta_yi){
    n = 2*length(xi)
    num = c(1:n)
    survtime = c(xi,yi)
    censored = c(delta_xi,delta_yi)
    rank = numeric(length = n)    #2.21修改
    data = data.frame(num,survtime,censored,rank)
    uncensored_num = data$num[data$censored==1]; N = length(uncensored_num)
    censored_num = data$num[data$censored==0]
    
    data$rank[uncensored_num] = rank(data$survtime[uncensored_num])
    t_j = sort(data$survtime[uncensored_num])
    
    for(k in censored_num)
      data$rank[k] = find_censored_rank(data$survtime[k],t_j)
    
    s <- vector(mode ="numeric",length = N)
    n_j <- vector(mode ="numeric",length = N)
    n_j[1] = sum(data$survtime>=t_j[1])  
    s[1] = n_j[1]/(n_j[1]+1)
    for(i in 2:N){
      n_j[i] = sum(data$survtime>=t_j[i]) 
      s[i] = n_j[i]/(n_j[i]+1) *s[i-1]
    }
    
    
    for(i in 1:n){
      if(data$censored[i]==1)
        data$k[i]=1-2*s[data$rank[i]]
      else data$k[i]=1-s[data$rank[i]]
    }
    
    k_xi = data$k[1:length(xi)]
    k_yi = data$k[(length(xi)+1):n]  
    
    data_final = data.frame(xi,delta_xi,yi,delta_yi,k_xi,k_yi)
    data_final$Delta_i = data_final$k_xi-data_final$k_yi
    wilcox.test(data_final$Delta_i)$p.value
    #  I = length(data_final$Delta_i)
    #  q <- rank(data_final$Delta_i)
    #  Index_i <- data_final$Delta_i>0
    
    #  W_mppw <- sum(q*Index_i)
    
    #  Z_mppw = (W_mppw-I*(I+1)/4)/(sqrt(I*(I+1)*(2*I+1)/24))
    
    #  list(table = data_final,t_j = t_j,n_j = n_j,s_j = s,
    #       W_mppw = W_mppw,Z_mppw=Z_mppw)
  }
  
  
  
  
}
