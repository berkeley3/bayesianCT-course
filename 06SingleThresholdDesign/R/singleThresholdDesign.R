stage2=function(ru, pi, epsilon=0.05, lambda, alpha=NULL, beta=NULL){
  if (is.null(alpha)) alpha= pi+1
  if (is.null(beta)) beta= 1-pi+1
   N=9
  post=0
  while(post<lambda){
    N=N+1
    x=(ru+epsilon)*N
     alpha1 = alpha+x
     beta1 = beta+N-x
    post=1-pbeta(ru,alpha1, beta1)
    }
  res=list(N=N, posterior=post)
  res
}

stage1=function(ru, pi, epsilon=0.05, lambda, N, alpha=NULL, beta=NULL){
  n=4
  x=(ru+epsilon)*n
  if (is.null(alpha)) alpha = pi+1
  if (is.null(beta)) beta = 1-pi+1
  alpha1 = alpha+x
  beta1 = beta+n-x
  post=1-pbeta(ru,alpha1, beta1)
  if(post>=lambda){print("n<5")}else{while(post<lambda & n<=(N-5)){
    n=n+1
    x=(ru+epsilon)*n
    alpha1 = alpha+x
    beta1 = beta+n-x
    post=1-pbeta(ru,alpha1, beta1)
  }
  res=list(n=n, posterior=post)
  res}
}


## Examples
## non informative priors
# stage2(ru=0.55, pi=0.7, lambda=0.7)
# stage1(ru=0.55, pi=0.7, lambda=0.6, N=26)
# 
# stage2(ru=0.4, pi=0.3, lambda=0.7)
# stage1(ru=0.4, pi=0.3, lambda=0.6, N=28)
# 
# stage2(ru=0.3, pi=0.1, lambda=0.7)
# stage1(ru=0.3, pi=0.1, lambda=0.6, N=24)


## informative priors
#stage2(ru=0.80, lambda=0.8, alpha=14.23, beta=3.56)


