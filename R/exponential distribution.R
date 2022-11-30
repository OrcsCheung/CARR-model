LL_Exp<-function(para){
  ww=para[1]  # w
  alpha=para[2]  # alpha
  beta=para[3]  # beta
  n=length(data)
  psi <- rep(0, n)
  # read data from file , named as RR
  psi[1]=data[1]  # replace with mean (R) or R(1)

  sum1 = -log(psi[1])
  sum2 = -data[1]/psi[1]
  sum=0
  for (j in 2:n) {
    psi[j]=ww+alpha*psi[j-1]+beta*data[j-1]
    sum1=sum1-log(psi[j])
    sum2=sum2-(data[j]/psi[j])
  }
  sum=sum1+sum2
  return(-sum)
}
