library(ggplot2)
datos= as.data.frame(state.x77) 
datos
X=datos[,'HS Grad']
Y=datos[,'Illiteracy']
Observaciones=data.frame(X,Y)

ggplot(Observaciones , mapping=aes(X, Y))+
  geom_point(color=" royalblue ", alpha=0.7, size=5)+ 
  labs( title = " Observaciones de porcentaje de graduadosvs. tasa de
analfabetizacion ",
        x =" Porcentaje de graduados ",
        y =" Analfabetizacion ") +
  geom_smooth(method="lm", color=" darkred ") + 
  theme_minimal()+
  theme(legend.title.align=0.5)

x_bar=mean(X)
y_bar=mean(Y)

Sxx=sum((X-x_bar)^2)
Sxy=sum((X-x_bar)*(Y-y_bar))
Syy=sum((Y-y_bar)^2)

B1=Sxy/Sxx
B0=y_bar-x_bar*B1

Modelo=lm(Y~X,data=Observaciones)

Ajustados=B0+B1*X
Residuales=(Y-Ajustados)
sum(Residuales)

n=length(X)
sigma2=sum(Residuales^2)/(n-2)

sigma=sqrt(sigma2)

Var_B1=sigma2/Sxx
Var_B0=(1/n+(x_bar^2)/Sxx)*sigma2

incisoa=data.frame(Sxx,sum((X-x_bar)*X),sum((X^2))-n*x_bar^2)
incisob=data.frame(Sxy,sum((X-x_bar)*Y),sum((Y-y_bar)*X),sum(X*Y)-n*x_bar*y_bar)

alpha=0.05
Estadistica_b0=B0/(sqrt((1/n+(x_bar^2)/Sxx)*sigma2))
Estadistica_b1=B1/(sqrt(sigma2/Sxx))
b0_distinta_de_cero=((Estadistica_b0>qt(alpha/2,df=n-2))||(Estadistica_b0<-qt(alpha/2,df=n-2)))
b1_distinta_de_cero=((Estadistica_b1>qt(alpha/2,df=n-2))||(Estadistica_b1<-qt(alpha/2,df=n-2)))

GL=c(1,n-2,n-1)

SC_reg=B1*Sxy
SC_total=Syy
SC_error=SC_total-SC_reg

SC=c(SC_reg,SC_error,SC_total)

CM=c(SC_reg,SC_error/(n-2),0)

F=c(SC_reg/(SC_error/(n-2)),0,0)

ANOVA=data.frame(GL,SC,CM,F)
rownames(ANOVA)=c('Regresion','Error','Total')
colnames(ANOVA)=c('GL','SC','CM','F')

ANOVA

p_value=pf(F[1],df1=1,df2=n-2,lower.tail = FALSE)
p_value

#Se observa que se rechaza la hipotesis nula hasta a un nivel de confianza del
##99%, por lo que se obtiene que, podemos asumir B1 distinto de cero, es decir,
#que existe una asociación lineal entre la variable respuesta 'y' y la variable 
#explicativa 'x'

R2=SC_reg/SC_total

Estimacion=B0+B1*80

int_pred_inf=B0+B1*80-qt(alpha/2,df=n-2)*sqrt((1+1/n-(80-x_bar)^2/Sxx)*sigma2)
int_pred_sup=B0+B1*80+qt(alpha/2,df=n-2)*sqrt((1+1/n-(80-x_bar)^2/Sxx)*sigma2)

int_conf_inf=B0+B1*80-qt(alpha/2,df=n-2)*sqrt((1/n+(80-x_bar)^2/Sxx)*sigma2)
int_conf_sup=B0+B1*80+qt(alpha/2,df=n-2)*sqrt((1/n+(80-x_bar)^2/Sxx)*sigma2)
