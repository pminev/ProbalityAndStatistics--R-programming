
cardDeal<- function(n,p=1/52)
{
  #make matrix of n dealing of cards, n times
  alldeals <- matrix(sample(1:52,size=n*n,prob=rep(1/52,52),replace = T), ncol=n)
  #represent the matrix as table of occurances of every card
  dealsAsTable<-table(alldeals)
  #make histogram of density for every card
  hist(alldeals,freq = F,breaks = 52)
  curve(1/52*(x/x), from=1, to=52,add=TRUE)
}