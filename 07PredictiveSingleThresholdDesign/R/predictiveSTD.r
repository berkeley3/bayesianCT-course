

pred.stage1=function(piA, piD,thetau, lambda1, gamma1, nD=10, n1=100){
  
  mD=function(piA, piD,thetau, lambda1, nD, n1=100){
    alphaA=piA+1
    betaA=1-piA+1
    pn1=NULL
    for(s1 in 0:n1)
      pn1[s1+1]=pbeta(thetau,s1+alphaA, n1-s1+betaA, lower.tail=F)
    r1=match(1,(pn1>=lambda1)*1)-1
    alphaD=nD*piD+1
    betaD=nD*(1-piD)+1
    mD=NULL
    for(i in r1:n1)
      mD[i-r1+1]=choose(n1,i)*beta(alphaD+i, betaD+n1-i)/beta(alphaD, betaD)
    m=sum(mD)
    res=list(r1=r1,mD=m)
    res}
  
  m=mD(piA, piD,thetau, lambda1, nD, n1)$mD
  if (m<gamma1){
  print('a sensible sample size does not exist')}else{
    while(m>=gamma1 & n1>0){
    n1=n1-1
    m=mD(piA, piD,thetau, lambda1, nD, n1)$mD
    r1=mD(piA, piD,thetau, lambda1, nD, n1)$r1}}
    if(n1==0){
      print('the condition is already satisfied')
    }else{
  res=list(n1=n1+1, r1=r1-1, mD=m)
  res}
}


pred.stage2=function(piA, piD,thetau, lambda2, gamma2, nD=10, r1, n1, n=100){
  
  mD2=function(piD, r1, n1, n){
    alphaD=nD*piD+1
    betaD=nD*(1-piD)+1
    md=NULL
    for (s in (r1+1):n){
    for(i in (r1+1):min(n1,s)){
      a[i-r1]=choose(n1,i)*choose(n-n1, s-i)}
    md[s-r1]=beta(alphaD+s, betaD+n-s)*sum(a)}
    md
    }
#     
#     const=function(s){
#       a=0
#       for(i in (r1+1):min(n1,s))
#         a=a+choose(n1,i)*choose(n-n1, s-i)
#       res=beta(alphaD+s, betaD+n-s)*a
#       res
#     }
#     denom=NULL
#     for(k in (r1+1):n)
#       denom[k-r1]=const(k)
#     denom=sum(denom)
#     
#     res=num/denom
#    res
#   }
  
  mD3=function(piA, piD,thetau, lambda2, nD=10, r1, n1, n=100){
    alphaA=piA+1
    betaA=1-piA+1
    pn=NULL
    for(s in (r1+1):n)
      pn[s-r1]=pbeta(thetau,s+alphaA, n-s+betaA, lower.tail=F)
    r=match(TRUE,pn>=lambda2)+r1
#     alphaD=nD*piD+1
#     betaD=nD*(1-piD)+1
    md=mD2(piD, r1, n1,n)
    m=md[(r-r1):(n-r1)]/sum(md)
    m=sum(m, na.rm=T)
    res=list(r=r,mD=m)
    res
    }
   
  
  if(n<n1)
  {print('choose n greater than n1')}else{
    m=mD3(piA, piD,thetau, lambda2, nD, r1, n1, n)$mD
  if (m<gamma2){
    print('a sensible sample size does not exist')}else{
      while(m>=gamma2 & n>n1){
        n=n-1
        m=mD3(piA, piD,thetau, lambda2, nD, r1, n1, n=n)$mD
        r=mD3(piA, piD,thetau, lambda2, nD, r1, n1, n)$r}}
  if(n==n1){
    print('the condition is already satisfied')
  }else{
    res=list(n=n+1, r=r-1, mD=m)
    res}}
}


lambda1=0.7
gamma1=0.6
thetau=0.2
piA=thetau-0.1
piD=thetau+0.1

stage1=pred.stage1(piA=piA, piD=piD, thetau=thetau, lambda1=lambda1, gamma1=gamma1, nD=10)
r1=stage1$r1
n1=stage1$n1
lambda2=0.7
gamma2=0.9
pred.stage2(piA=piA, piD=piD, thetau=thetau, lambda2=lambda2, gamma2=gamma2, r1=r1, n1=n1, nD=10, n=100)



