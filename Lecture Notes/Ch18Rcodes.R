#Step 1:  Formulate Hypotheses (PARAMETERS)
# Assign the value from H0 to p0
p0 <- 0.029

#Step 2: Calcualte the standardized test statistic
n <- 533
phat <- 11/533 #or a percent if given

zstat <- (phat - p0)/sqrt(p0*(1-p0)/n)

cat("The standardized test statistic is",zstat)

#Step 3: Calcualte the P-value
#P-value = P(get what you got or more when Ho is true)

#less than
LTPval <- pnorm(zstat)
#greater than
GTPval <- 1-pnorm(zstat)
#not equal
NEPval <- 2*pnorm(-abs(zstat))


list(Less=LTPval,Greater=GTPval,NotEqual=NEPval)


n <- 533
p0 <- 0.029
n*p0 > 10 
n*(1-p0) >10

#Testing Hypotheses about mean μ

#Step 0: Find(or enter) the sample mean (y¯) and standard deviation (s) and n
#FBfriends <- c(317,	1236,	1197	0	707	1078	480	595	665	1654	693	151	507	1234	766	1001	172	895	595	209	806	783	329	701	289	699	0	863	706	547	1115	1152	902	594	622	807	381	782	248	1741	1086	593	1456	581	1295	694	798	763	808	782	454	946	731	952	1002	1017	1159	840	439	453	508	896	697	975	986	691	233	1358	592	322	895	986	769	734	404	2	533	296	136	516	902	1328	410	691	0	225	678	1308	997	1253	991	649	1267	634	283	787	902	697	437	418	823	407	445	445	1462	906	770	602	1691	1506	459	333	308	839	653	734	912	594	1192	819	1143	985	477	903	509	674	694	765	1035	922	1009	715	563	942	307	556	937	700	1423	1128	250	1149	974	0	589	261	955	161	877	1036	714	1350	642	0	888	1110	1403	350	950	933	505	1008	921	679	776	756	473	725	1261	1209	601	685	1217	99	655	718	640	131	873	720	818	776	1228	607	228	889	652	194	692	780	241	719	502	1465	779	598	450	1164	868	1280	221	1256	993	1001	831	1392	91	890	953	1050	744	1103	504	889	956	209	670	501	960	967	1224	179	1016	1124	230	523	191	601	1046	749	1363	756	764	452	1310	311	991	857	800	398	512	1237	991	678	806	861	1167	1025	719	523	407	1069	557	1045	853	245	642	435	464	0	227	1098	1243	583	500	701	862	403	1204	1391	1303	742	1129	635	405	772	603	545	840	817	291	190	1305	939	813	626	266	466	2009	758	721	784	897	506)

library(readxl)
q24 <- read_excel("Lecture Notes/q24.xlsx")
head(q24)

ybar <- mean(q24$nof)

s <- sd(q24$nof)

n <- length(q24$nof)


#Step 1:  Formulate Hypotheses (PARAMETERS)

mu0 <- 649 #This is the value in H0 : mu = mu0

# Step 2:  Calcualte the standardized test statistic
tstat <- (ybar - mu0)/(s/sqrt(n))

cat("The t-stat is",tstat)

#P-value = P(get what you got or more when Ho is true)

df <- n - 1 # degrees of freedom

#less
LTPval<- pt(tstat,df)

#more
GTPval<- 1-pt(tstat,df)

#not equal
NEPval <- 2*pt(-abs(tstat), df)

list(Less=LTPval,Greater=GTPval,NotEqual=NEPval)




CIlev <- .95
s <- 29.31
ME <- 0.02

alpha <- 1-CIlev
alphaovertwo <- alpha/2
oneminusalphaovertwo <- 1- alpha/2
zinCI <- qnorm(oneminusalphaovertwo)

nprelim <- (zinCI * s/ME)^2
df <- nprelim-1

tstar <- qt(oneminusalphaovertwo,df)

nfinal <- (tstar * s/ME)^2
ceiling(nfinal)






