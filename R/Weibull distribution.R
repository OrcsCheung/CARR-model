LL_WB<-function(para){
  beta00=para[1]
  beta11=para[2]
  beta21=para[3]
  aa=para[4]
  #aa=1 #constant not given but boleh
  n=length(data)
  psi <- rep(0, n)
  # read data from file , named as RR
  psi[1]=0.5  # replace with mean (R) or R(1)
  sum1 = log(aa)-log(data[1])
  sum2 = n*aa*log(gamma(1+1/aa))
  sum3 = aa*(log(data[1])-log(psi[1]))
  sum4 = -(aa*gamma(1+1/aa)*data[1]/psi[1])^aa
  sum=0
  for (i in 2:n) {
    psi[i]=beta00+beta11*data[i-1]+beta21*psi[i-1]
    sum1=sum1+log(aa/data[i])
    sum3=sum3+ aa*log(data[i]/psi[i])
    sum4=sum4-(gamma(1+1/aa)*data[i]/psi[i])^aa
  }
  sum=sum1+sum2+sum3+sum4
  return(-sum)
}
