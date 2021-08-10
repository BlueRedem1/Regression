Xt_X=matrix(c(9,136,269,260,136,2114,4176,3583,269,4176,8257,7104,260,3583,7104,
              12276),nrow=4,ncol=4)
Xt_X_inv=matrix(c(9.61093203,0.008587789,-0.279147542,-0.0445216881,0.008587789,0.50996407,
           -0.258863585,0.000776541,-0.279147542,-0.258863585,0.139499959,0.000739563,
           -0.044521688,0.000776541,0.000739563,0.000369781),nrow=4,ncol=4)
Beta_gorro=matrix(c(-1.1634611,0.135270,0.019950,0.121954),nrow=4,ncol=1)
Xt_Y=matrix(c(45,648,1283,1821),nrow=4,ncol=1)
Yt_Y=matrix(285,nrow=1,ncol=1)

k=3
n=9

GL=c(k, n-k-1, n-1)

SC_reg=t(Xt_Y)%*%Xt_X_inv%*%Xt_Y-225
SC_err=Yt_Y-t(Xt_Y)%*%Xt_X_inv%*%Xt_Y
SC_tot=Yt_Y-225

SC=c(SC_reg,SC_err,SC_tot)

CM_reg=SC_reg/k
CM_err=SC_err/(n-k-1)

CM=c(CM_reg,CM_err,0)

F=c(CM_reg/CM_err,0,0)

ANOVA=data.frame(GL, SC, CM, F) 
rownames(ANOVA)=c("Regresion", "Error", "Total") 
colnames(ANOVA)=c("GL", "SC", "CM", "F")

ANOVA

sigma2=SC_err/(n-k-1)

var_covar_beta_gorro=(sigma2[1,1]*Xt_X_inv)
#Esta es la matriz de varianza covarianza, por lo que las varizas son
var_beta_gorro=matrix(c(var_covar_beta_gorro[1,1],var_covar_beta_gorro[2,2],
                        var_covar_beta_gorro[3,3],var_covar_beta_gorro[4,4]),nrow=4,ncol=1)
rownames(var_beta_gorro)=c("Beta_0", "Beta_1", "Beta_2",'Beta_3') 
sd_beta_gorro=sqrt(var_beta_gorro)

alpha=0.05

#A un nivel alpha=0.05 de confianza, 
int_inf=-qt(1-(alpha/2),df=n-k-1)*sd_beta_gorro+Beta_gorro
int_sup=qt(1-(alpha/2),df=n-k-1)*sd_beta_gorro+Beta_gorro

#Podemos ver que solo beta_3 no contiene al 0 en un intervalo del 95% de confianza
estadistica_t<-Beta_gorro/(sd_beta_gorro)

rechazamos_H0<-abs(estadistica_t)>qt(1-(alpha/2),df=n-k-1)

#A un nivel de confianza del 95%, solo podemos decir que beta_3 es distinto de 
#cero, mientras que beta_0, beta_1 y beta_2 aceptamos la hipotesis nula de que
#son 0