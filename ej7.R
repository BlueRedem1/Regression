p_value=pf(4.1253508961347976,df1=1,df2=19,lower.tail = FALSE)
#Como alpha>p_value no se rechaza la hipótesis nula de que B_1=0
dif=92.62/19
alpha=0.05
int_inf=(dif/(qchisq((1-alpha/2),df=19)))
int_sup=(dif/(qchisq((alpha/2),df=19)))