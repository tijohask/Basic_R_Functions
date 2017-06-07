sampling=function(n,dist)
{
  x=sample(dist$value,size=n,replace=TRUE,prob=dist$prob)
  return(x)
}
dist = list(value = c(0, 1, 2, 3), prob = c(0.1, 0.2, 0.4, 0.3))
data=sampling(200,dist)
data
counts=table(data)
counts
counts/sum(counts)
barplot(counts/sum(counts),main="Distribution",xlab="Value")
rbinom(10, 25, 0.2)  # sample 10 observation from Bin(n=25,p=0.2)
rhyper(10, 12, 13, 8)  # sample 10 observation from Hypergeo(N=25,n=8,k=12)
rpois(10, 4)  # sample 10 observations from Poisson(4)
runif(10, 1,3)  # sample 10 observations from Unif(1,3)
X=rexp(10, 4) # sample 10 observation from EXP(4)
rnorm(10, 4,sqrt(2)) # sample 10 observations from N(mean=4, variance=2)

# Now we track the sample averages with a sequence of sample size, say from 1 to 1000. 

mu=sum(dist$value * dist$prob) 
mu
sdv=sqrt(sum((dist$value - mu)^2 * dist$prob)) 
sdv
mean_tracking = function(n, dist) 
{
  x = sample(dist$value, size = n, replace = TRUE, prob = dist$prob) 
  s = cumsum(x)
  r = s/(1:n)
  return(r)
}
n = 1000
result = mean_tracking(n, dist)
result
plot(result, type = "l", ylim = c(mu - 0.5, mu + 0.5), 
     ylab = expression(bar(X)[n]), xlab = "n")
lines(c(0, n), c(mu, mu))


# Central Limit Theorem

# Case 1: population distribution is discrete and skewed

dist=list(value=c(0,1,2,3),prob=c(0.1,0.2,0.4,0.3))
mu=sum(dist$value*dist$prob)
sdv=sqrt(sum((dist$value-mu)^2*dist$prob)) 
mu
sdv

N = 10000
n=1000
result=replicate(N,sampling(n,dist))
dim(result)
hist(colSums(result)/n, xlab = expression(bar(X)[n]), main = "", prob = TRUE)
mean(colSums(result)/n)
sd(colSums(result)/n)
sdv/sqrt(n)

# Case 2: population distribution is continuous and skewed

n=1000
result=rexp(n, 4)
hist(result, xlab="x", main="", prob=TRUE)
mean(result)
sd(result)

N=10000
result=replicate(N,rexp(n,4))
hist(colSums(result)/n, prob=TRUE)
mean(colSums(result)/n)
sd(colSums(result)/n)
(1/4)/sqrt(n)


