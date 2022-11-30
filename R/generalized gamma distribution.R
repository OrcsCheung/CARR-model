LL_GG<-function(para){
  beta00=para[1]
  beta11=para[2]
  beta21=para[3]
  omega=para[4]
  Kennel=para[5]

  n=length(data)
  psi <- rep(0, n)

  psi[1]=data[1]  # replace with mean (R) or R(1)
  sum1 = -log(psi[1])
  sum2 = n*(log(omega)-lgamma(Kennel))
  sum3 = n*(Kennel*omega)*(lgamma(Kennel+(1/omega))-lgamma(Kennel))
  sum4 = (Kennel*omega-1)*(log(data[1])-log(psi[1]))
  sum5 = -((data[1]*exp(lgamma(Kennel+1/omega)))/(psi[1]*exp(lgamma(Kennel))))^omega

  sum=0
  for (i in 2:n) {
    psi[i]=beta00+beta11*data[i-1]+beta21*psi[i-1]
    sum1=sum1-log(psi[i])
    sum4=sum4+(Kennel*omega-1)*(log(data[i])-log(psi[i]))
    sum5=sum5-((data[i]*exp(lgamma(Kennel+1/omega)))/(psi[i]*exp(lgamma(Kennel))))^omega
  }
  sum=sum1+sum2+sum3+sum4+sum5

  return(-sum)
}
