
#### function to generate the entire binomial distribution
gbinom <- function(n,prob){
xseq <- seq(0,n)
prob_seq <- sapply(xseq,dbinom,size=n,prob=prob)
return(prob_seq)
}



library(ggplot2)

n=10
p=0.05
try1 <- gbinom(n=n,prob=p)
df <- data.frame(X=seq(0,n),prob=try1)

g1<-ggplot(df, aes(X, prob)) +
  geom_linerange(
    aes(x = X, ymin = 0, ymax = prob), 
    color = "black", size = 1.5
    )+theme(panel.background = element_rect(fill = "white", colour = "grey50"))+ylab("Binomial Probability")+
  scale_x_continuous(breaks = seq(0, n, by = 1))




n=10
p=0.5
try1 <- gbinom(n=n,prob=p)
df <- data.frame(X=seq(0,n),prob=try1)

g2<-ggplot(df, aes(X, prob)) +
  geom_linerange(
    aes(x = X, ymin = 0, ymax = prob), 
    color = "black", size = 1.5
    )+theme(panel.background = element_rect(fill = "white", colour = "grey50"))+ylab("Binomial Probability")+
  scale_x_continuous(breaks = seq(0, n, by = 1))


n=10
p=0.85
try1 <- gbinom(n=n,prob=p)
df <- data.frame(X=seq(0,n),prob=try1)

g3<-ggplot(df, aes(X, prob)) +
  geom_linerange(
    aes(x = X, ymin = 0, ymax = prob), 
    color = "black", size = 1.5
    )+theme(panel.background = element_rect(fill = "white", colour = "grey50"))+ylab("Binomial Probability")+
  scale_x_continuous(breaks = seq(0, n, by = 1))

library("gridExtra")
grid.arrange(g1, g2,g3,ncol=3)


####################################




library(ggplot2)

n=10
try1 <- gbinom(n=n,prob=0.15)
df <- data.frame(X=seq(0,n),prob=try1)

g1<-ggplot(df, aes(X, prob)) +
  geom_linerange(
    aes(x = X, ymin = 0, ymax = prob), 
    color = "black", size = 1.5
    )+theme(panel.background = element_rect(fill = "white", colour = "grey50"))+ylab("Binomial Probability")+
  scale_x_continuous(breaks = seq(0, n, by = 1))




n=30
p=0.15
try1 <- gbinom(n=n,prob=p)
df <- data.frame(X=seq(0,n),prob=try1)

g2<-ggplot(df, aes(X, prob)) +
  geom_linerange(
    aes(x = X, ymin = 0, ymax = prob), 
    color = "black", size = 1.5
    )+theme(panel.background = element_rect(fill = "white", colour = "grey50"))+ylab("Binomial Probability")+
  scale_x_continuous(breaks = seq(0, n, by = 2))


n=100
p=0.15
try1 <- gbinom(n=n,prob=p)
df <- data.frame(X=seq(0,n),prob=try1)

g3<-ggplot(df, aes(X, prob)) +
  geom_linerange(
    aes(x = X, ymin = 0, ymax = prob), 
    color = "black", size = 1.5
    )+theme(panel.background = element_rect(fill = "white", colour = "grey50"))+ylab("Binomial Probability")+
  scale_x_continuous(breaks = seq(0, n, by = 10))+xlim(0,50)

library("gridExtra")
grid.arrange(g1, g2,g3,ncol=3)



calc1<- gbinom(200,0.6)
df <- data.frame(X=seq(0,200),prob=calc1)
sum(df[df$X>100,2])


