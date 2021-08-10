n=74
x_bar=296.4
Sxx=349858.2
y_bar=253.9
Syy=241343.5
Sxy=280510.8

#a)
B1=Sxy/Sxx
B0=y_bar-B1*x_bar
sigma2=(Syy-B1*Sxy)/(n-2)

#b)
VarB0=(1/n+x_bar^2/Sxx)*sigma2
VarB1=(1/Sxx)*sigma2

#c)
SCtc=Syy
SCreg=B1*Sxy
SCer=SCtc-SCreg

#d)
R2=SCreg/SCtc

#e)
Estimacion=B0+B1*300

#f)
alpha=0.05
int_conf_inf=B0+B1*300-qt(alpha/2,df=n-2)*sqrt((1/n+(300-x_bar)^2/Sxx)*sigma2)
int_conf_sup=B0+B1*300+qt(alpha/2,df=n-2)*sqrt((1/n+(300-x_bar)^2/Sxx)*sigma2)

#g)
int_pred_inf=B0+B1*300-qt(alpha/2,df=n-2)*sqrt((1+1/n-(300-x_bar)^2/Sxx)*sigma2)
int_pred_sup=B0+B1*300+qt(alpha/2,df=n-2)*sqrt((1+1/n-(300-x_bar)^2/Sxx)*sigma2)