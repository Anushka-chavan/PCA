### Unsupervised learning: PRINCIPAL COMPONENT ANALYSIS.

# PCA using covariance matrix.
View(iris)
data=data.frame(iris)[,-5]
data=as.matrix(data)
dim=dim(data)
n=dim[1]
p=dim[2]
var=cov(data);var
eval=eigen(var)$values;eval
evec=eigen(var)$vectors;evec
prop_variation=eval/sum(eval);prop_variation
cum_prop_variation=cumsum(eval)/sum(eval);cum_prop_variation
# 1st pincipal component (pc) explains 92% of variation inn data is required.
# 1 pc would be enough.

evec_mat=as.matrix(evec);evec_mat
round(t(evec_mat)%*%evec_mat) # orthogonal eigenvectors.

# scree plot
plot(1:p,eval,"b")

# score plot
scores=data%*%evec_mat;scores

#biplot
biplot(prcomp(data))

plot(scores)
#setosa
p1=scores[1:50,1]
p11=scores[1:50,2]
#versicolor
p2=scores[51:100,1]
p22=scores[51:100,2]
#virginica
p3=scores[101:150,1]
p33=scores[101:150,2]

plot(p1,p11,type="p",xlim=c(min(p1,p2,p3),max(p1,p2,p3)),
     ylim=c(min(p11,p22,p33),max(p11,p22,p33)),col="red")
lines(p2,p22,type="p",col="blue")
lines(p3,p33,type="p",col="green")

col=colnames(data)
var1=col[which.max(abs(evec_mat[,1]))];var1
var2=col[which.max(abs(evec_mat[,2]))];var2
# var1 and var2 are most important variables.most information is given by this variables

# PCA using correlation matrix.
cor=cor(data)
cor_eval=eigen(cor)$values;cor_eval
cor_evec=eigen(cor)$vectors;cor_evec
cor_evec_mat=as.matrix(cor_evec);cor_evec_mat
prop_variation=cor_eval/sum(cor_eval);prop_variation
cum_prop_variation=cumsum(cor_eval)/sum(cor_eval);cum_prop_variation
# 2pcs are required.

plot(1:p,cor_eval,type="b")

sd=as.matrix(scale(data))
cor_scores=sd%*%cor_evec_mat
plot(cor_scores)

q1=cor_scores[1:50,1]
q11=cor_scores[1:50,2]
q2=cor_scores[51:100,1]
q22=cor_scores[51:100,2]
q3=cor_scores[101:150,1]
q33=cor_scores[101:150,2]

plot(q1,q11,type="p",xlim=c(min(q1,q2,q3),max(q1,q2,q3)),
     ylim=c(min(q11,q22,q33),max(q11,q22,q33)),col="red")
lines(q2,q22,type="p",col="blue")
lines(q3,q33,type="p",col="green")

  
  