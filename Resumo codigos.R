library(readxl)
c1 <- read_excel("C:/Mariana/UFSCar/TCC/TG1/Dados/c1.xlsx")
# View(c2)
cromo1 = c1
View(cromo1)

n_colunas = ncol(cromo1)
n_linhas = nrow(cromo1)

for (j in 1:n_colunas){
  for (i in 1:n_linhas){
    if (cromo1[i,j]=='A/A'|cromo1[i,j]=='T/T'){
      cromo1[i,j]=1
    }
    if (cromo1[i,j]=='C/C'|cromo1[i,j]=='G/G'){
      cromo1[i,j]=-1
    }
    if (cromo1[i,j]=='A/T'|cromo1[i,j]=='A/C'|cromo1[i,j]=='A/G'|
        cromo1[i,j]=='T/A'|cromo1[i,j]=='T/C'|cromo1[i,j]=='T/G'|
        cromo1[i,j]=='C/A'|cromo1[i,j]=='C/G'|cromo1[i,j]=='C/T'|
        cromo1[i,j]=='G/A'|cromo1[i,j]=='G/C'|cromo1[i,j]=='G/T'){
      cromo1[i,j]=0
    }
  }
}

write.csv(cromo1,'C:/Mariana/UFSCar/TCC/Cromossomo1.csv')
cromo1 = read.csv('C:/Mariana/UFSCar/TCC/Cromossomo1.xlxs')

# Filtro das variáveis com resposta em apenas uma categoria
cromo1 = Cromossomo1
cromo1 = cromo1[,-1]
tam1 = 0
for (i in 1:ncol(cromo1)) {
  tam1[i]=length(table(cromo1[,i]))
}
table(tam1)
cromo1.1 = rbind(cromo1,tam1)

for(i in 1:ncol(cromo1.1)){
  if (cromo1.1[698,i]==1){
    (print(i))
  }
  
}

cromo1.2 = cromo1.1[,-c(87,498)]

for (i in 1:ncol(cromo1.2)){
  if (cromo1.2[698,i]==1){
    (print(i))
  }
  
}
write.csv(cromo1.2,'C:/Mariana/UFSCar/TCC/Cromossomo1f.csv')
cromo1f = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 1/Cromossomo1f.csv')

# Dividindo o banco em Teste x Validação
library(stringi)
library(caret)

indice = cromo1f[-698,1]
cromof1 = cromo1f[-698,-1]
divide <- createDataPartition(indice, p=0.7, list=FALSE)
write.table(divide,'C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/divide.csv', dec = ',', sep = ';')
# ta errado (pegar do excel) divide = c(1	2	3	4	6	8	9	10	12	13	14	15	16	17	18	19	20	23	24	25	27	28	30	32	33	36	37	39	40	41	44	45	46	49	50	51	53	55	58	60	61	62	63	64	66	69	72	74	75	79	80	81	83	84	85	86	88	89	90	91	92	93	94	95	96	97	98	99	100	101	102	104	105	106	107	108	111	112	113	114	117	118	119	121	122	123	124	126	127	128	129	130	131	132	133	135	137	138	140	142	143	145	146	148	149	150	151	152	153	155	156	158	159	160	163	164	165	166	167	170	171	174	175	177	178	180	181	182	183	185	186	188	189	190	191	192	193	194	195	197	198	200	204	206	207	208	209	210	211	213	214	215	217	221	222	223	225	227	228	231	233	234	235	236	237	238	239	241	242	243	244	246	247	249	250	251	253	254	255	256	257	258	259	260	261	263	265	266	268	269	270	271	272	274	275	276	278	279	280	282	284	287	288	292	293	296	297	299	300	301	302	303	304	306	307	308	309	310	311	312	313	314	318	319	320	321	322	325	328	329	330	331	333	334	335	337	338	339	341	342	343	344	346	347	349	350	352	353	354	357	360	362	363	364	367	369	372	374	375	376	377	378	379	380	381	383	384	385	387	389	391	394	395	396	400	401	402	403	404	406	407	408	409	410	414	415	416	417	421	423	424	425	426	427	428	429	430	431	433	434	436	437	438	439	440	441	442	443	444	445	446	447	449	450	452	455	456	457	459	460	461	462	463	464	466	467	468	469	471	473	476	478	479	480	481	482	483	486	487	488	489	491	493	494	495	496	498	499	500	502	504	505	506	507	508	509	510	511	512	513	514	515	516	517	519	520	522	524	525	526	530	531	532	533	536	537	538	539	540	541	542	545	546	547	548	549	550	553	556	557	558	559	562	563	564	565	566	567	568	571	574	575	577	578	579	580	581	582	583	585	587	588	590	592	594	595	596	597	598	599	602	603	606	607	608	611	613	614	616	617	618	619	620	621	622	623	624	625	626	628	629	630	632	633	634	635	636	637	638	640	641	643	645	646	648	650	651	652	653	654	655	656	657	658	659	660	662	664	666	668	670	671	673	674	675	676	677	678	680	683	684	685	688	690	693	694	695	696	697)
cromo1treino <- cromof1[divide, ]
cromo1validacao = cromof1[-divide,]
write.csv(cromo1treino,'C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 1/Cromossomo1Treino.csv')
write.csv(cromo1validacao,'C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 1/Cromossomo1Validacao.csv')

##### Modelos

# 1ª Semente: 1

library(dplyr)
library(glmnet)
library(plotmo)
library(caret)

cromo1treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 1/Cromossomo1Treino.csv')
dados = cromo1treino
dados = dados[,-1]
X <- model.matrix(AFFECTED~., dados)[,-1]
y <- dados$AFFECTED
set.seed(1)
index <- createDataPartition(y, p=0.70, list=FALSE)
X_train <- X[index, ]
X_test <- X[-index, ]
y_train <- y[index]
y_test<-y[-index]

xtr = as.matrix(X_train)
ytr = y_train
# cojunto validação
xval = as.matrix(X_test)
yval = y[-index]

novo = cbind(xval,yval)

#
### LASSO
#

cv.out <- cv.glmnet(xtr, ytr, alpha = 1)

plot(cv.out, ylab = "EQM", xlab = "log(lambda)")

pdf("C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 1/1aSemente - LASSO - Lambda - 1.pdf")

lambda = plot(cv.out, ylab = "EQM", xlab = "log(lambda)")
dev.off()

###

png("C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 1/1aSemente - LASSO - Lambda - 1.png")

lambda = plot(cv.out, ylab = "EQM", xlab = "log(lambda)")

dev.off()


bestlam <- cv.out$lambda.min

lasso.mod <- glmnet(xtr, ytr, alpha = 1, family='binomial')
lasso.pred <- predict(lasso.mod, s = bestlam, newx = xval, family='binomial')
mean((lasso.pred-yval)^2)

lasso.coef  <- predict(lasso.mod, type = 'coefficients', s = bestlam)

plot(lasso.coef, xlab = 'Ordem dos Parâmetros', ylab = "Valor estimado do Parâmetro",
     main = "Parâmetros estimados", pch = 16, col = 'deeppink2' )

pdf("C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 1/1aSemente - LASSO - 1.pdf")

lasso = plot(lasso.coef, xlab = 'Ordem dos Parâmetros', ylab = "Valor estimado do Parâmetro",
             main = "Parâmetros estimados", pch = 16, col = 'deeppink2' )
dev.off()

###

png("C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 1/1aSemente - LASSO - 1.png")

lasso = plot(lasso.coef, xlab = 'Ordem dos Parâmetros', ylab = "Valor estimado do Parâmetro",
             main = "Parâmetros estimados", pch = 16, col = 'deeppink2' )

dev.off()
seed1 = as.matrix(lasso.coef)
write.table(seed1, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 1/1aSemente - LASSO - 1.csv",sep=';',dec=',')

# 
### Florestas
# 
library(randomForest)

# Izbick
m=sqrt(ncol(dados)) 
ajuste = randomForest(X,y,mtry=m,
                      importance = TRUE, type = regression, proximity = T)
rf1 = ajuste$importance
write.table(rf1, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 1/1aSemente - RF - 1.csv",sep=';',dec=',')

pdf("C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 1/1aSemente - RF - 1.pdf")

rf = varImpPlot(ajuste, main = "Ajuste via Florestas Aleatórias",
                pch = 16, col = 'turquoise4') 
dev.off()

###

png("C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 1/1aSemente - RF - 1.png")

rf = varImpPlot(ajuste, main = "Ajuste via Florestas Aleatórias",
                pch = 16, col = 'turquoise4') 
dev.off()


# O mesmo procedimento foi repetido mais 20 vezes
# mudando-se a semente em cada uma delas
# para os demais 21 cromossomos

library('dplyr')


##### Treino

### Variaveis selecionadas - Cromossomo 1
cromo1treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 1/Cromossomo1Treino.csv')
c1rf = cromo1treino %>% select(C1S6634,C1S8166,C1S7419,C1S8894,C1S874,
                               C1S10412,C1S9939,C1S1442,C1S2918,C1S1003,
                               C1S3073,C1S6587,C1S6752)
write.table(c1rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c1rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 2
cromo2treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 2/Cromossomo2Treino.csv')
c2rf = cromo2treino %>% select(C2S1135,C2S2485,C2S4740,C2S6847,C2S6997,
                               C2S7753,C2S5945,C2S6125,C2S1247,C2S3103,
                               C2S6899,C2S7926,C2S3630,C2S5927)
write.table(c2rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c2rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 3
cromo3treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 3/Cromossomo3Treino.csv')
c3lasso = cromo3treino %>% select(C3S5389,C3S5742,C3S4611)
write.table(c3lasso, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c3lasso.csv",sep=';',dec=',')

c3rf = cromo3treino %>% select(C3S455,C3S2149,C3S2621,C3S3621,C3S4611,
                               C3S5389,C3S5670,C3S5742,C3S2600,C3S3141,
                               C3S4529,C3S646,C3S988,C3S1174,C3S4387,
                               C3S2569,C3S5650,C3S2954,C3S5703)
write.table(c3rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c3rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 4
cromo4treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 4/Cromossomo4Treino.csv')
c4rf = cromo4treino %>% select(C4S489,C4S1251,C4S1553,C4S1809,C4S3917,
                               C4S4178,C4S4342,C4S580,C4S518,C4S3375,
                               C4S1001,C4S5043,C4S796,C4S2799,C4S4783,
                               C4S668,C4S5106,C4S3374,C4S3975,C4S236)
write.table(c4rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c4rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 5
cromo5treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 5/Cromossomo5Treino.csv')
c5rf = cromo5treino %>% select(C5S3051,C5S3151,C5S3339,C5S3209,C5S3163,
                               C5S3192,C5S1018,C5S1793,C5S2894,C5S3236,
                               C5S1239,C5S2020,C5S3027,C5S1866,C5S3140,
                               C5S2491,C5S3150,C5S3191,C5S3269,C5S4089)
write.table(c5rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c5rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 6
cromo6treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 6/Cromossomo6Treino.csv')
c6rf = cromo6treino %>% select(C6S3891,C6S4554,C6S5619,C6S397,C6S4303,
                               C6S4481,C6S1126,C6S1162,C6S1761,C6S1924,
                               C6S1088,C6S1504,C6S1905,C6S3730)
write.table(c6rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c6rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 7
cromo7treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 7/Cromossomo7Treino.csv')
c7rf = cromo7treino %>% select(C7S1818,C7S76,C7S151,C7S2352,C7S3495,
                               C7S3622,C7S5454,C7S1589,C7S2562,C7S2773,
                               C7S3604,C7S663,C7S2340,C7S1002,C7S2549,
                               C7S1598,C7S4034,C7S4012,C7S4199,C7S5488,
                               C7S1894,C7S2279,C7S2677,C7S5096)
write.table(c7rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c7rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 8
cromo8treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 8/Cromossomo8Treino.csv')
c8rf = cromo8treino %>% select(C8S322,C8S1225,C8S2383,C8S3416,C8S3858,
                               C8S850,C8S633,C8S929,C8S964,C8S2802,
                               C8S3435,C8S4540,C8S4809,C8S464,C8S1730,
                               C8S1732,C8S1781,C8S4932,C8S271,C8S323,
                               C8S2043,C8S4340,C8S4379)
write.table(c8rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c8rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 9
cromo9treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 9/Cromossomo9Treino.csv')
c9rf = cromo9treino %>% select(C9S1473,C9S1506,C9S4298,C9S4721,C9S1723,
                               C9S1802,C9S3126,C9S3244,C9S3725,C9S3000,
                               C9S3117,C9S3718,C9S392,C9S2992,C9S3231,
                               C9S3600,C9S1654,C9S3312)
write.table(c9rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c9rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 10
cromo10treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 10/Cromossomo10Treino.csv')
c10rf = cromo10treino %>% select(C10S297,C10S2761,C10S3061,C10S5143,C10S2891,
                                 C10S4403,C10S5083,C10S4018,C10S2413,C10S5019,
                                 C10S4334,C10S6369,C10S6418,C10S2829,C10S4824,
                                 C10S5540,C10S6621,C10S2841,C10S4466,C10S5768)
write.table(c10rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c10rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 11
cromo11treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 11/Cromossomo11Treino.csv')
c11rf = cromo11treino %>% select(C11S666,C11S5465,C11S6472,C11S38,C11S3309,
                                 C11S5651,C11S5641,C11S2664,C11S2458,C11S3912,
                                 C11S5633,C11S6914,C11S189,C11S2916)
write.table(c11rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c11rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 12
cromo12treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 12/Cromossomo12Treino.csv')
c12rf = cromo12treino %>% select(C12S6558,C12S6748,C12S6028,C12S1264,C12S3909,
                                 C12S6573,C12S5036,C12S3581,C12S3732,C12S675,
                                 C12S3140,C12S1339,C12S5139,C12S574)
write.table(c12rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c12rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 13
cromo13treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 13/Cromossomo13Treino.csv')
c13rf = cromo13treino %>% select(C13S105,C13S320,C13S756,C13S761,C13S779,
                                 C13S845,C13S999,C13S1034,C13S1216,C13S1337,
                                 C13S1429,C13S1581,C13S1966,C13S1994,C13S626,
                                 C13S838,C13S978,C13S1304,C13S1697,C13S212,
                                 C13S780,C13S1097,C13S951,C13S744,C13S753,
                                 C13S524,C13S163)
write.table(c13rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c13rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 14
cromo14treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 14/Cromossomo14Treino.csv')
c14rf = cromo14treino %>% select(C14S56,C14S68,C14S595,C14S710,C14S987,
                                 C14S1000,C14S1669,C14S2839,C14S3575,C14S988,
                                 C14S2003,C14S2384,C14S23,C14S342,C14S438,
                                 C14S2849,C14S1910,C14S972,C14S2894,C14S3200,
                                 C14S700,C14S1135,C14S1039,C14S1462)
write.table(c14rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c14rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 15
cromo15treino = read.table('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 15/Cromossomo15Treino.csv', sep=';', head=T)
c15lasso = cromo15treino %>% select(C15S774)
write.table(c15lasso, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c15lasso.csv",sep=';',dec=',')

c15rf = cromo15treino %>% select(C15S303,C15S774,C15S2128,C15S3513,C15S4023,
                                 C15S4286,C15S4354,C15S4644,C15S241,C15S252,
                                 C15S3062,C15S4033,C15S4330,C15S197,C15S3576,
                                 C15S4083,C15S2418,C15S3815,C15S4057,C15S1933,
                                 C15S1684,C15S430,C15S3683,C15S3825,C15S4041,
                                 C15S4477,C15S4285,C15S4072)
write.table(c15rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c15rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 16
cromo16treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 16/Cromossomo16Treino.csv')
c16rf = cromo16treino %>% select(C16S220,C16S370,C16S1800,C16S3093,C16S3138,
                                 C16S3228,C16S3619,C16S3666,C16S2546,C16S408,
                                 C16S464,C16S2590,C16S549,C16S1345,C16S3234,
                                 C16S3882,C16S2816,C16S3109,C16S3229,C16S3493,
                                 C16S366,C16S3298,C16S3377,C16S3430,C16S77,
                                 C16S1740,C16S3108)
write.table(c16rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c16rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 17
cromo17treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 17/Cromossomo17Treino.csv')
c17rf = cromo17treino %>% select(C17S2868,C17S2960,C17S3304,C17S4431,C17S4432,
                                 C17S5273,C17S3437,C17S2160,C17S2842,C17S4907,
                                 C17S5244,C17S3305,C17S4420,C17S4722,C17S4040,
                                 C17S3288,C17S3289,C17S3424,C17S5338)
write.table(c17rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c17rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 18
cromo18treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 18/Cromossomo18Treino.csv')
c18lasso = cromo18treino %>% select(C18S2320)
write.table(c18lasso, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c18lasso.csv",sep=';',dec=',')

c18rf = cromo18treino %>% select(C18S463,C18S764,C18S783,C18S1012,C18S1077,
                                 C18S1621,C18S1639,C18S1796,C18S2264,C18S2320,
                                 C18S2491,C18S700,C18S1631,C18S793,C18S1744,
                                 C18S1220,C18S602,C18S1121,C18S936,C18S1017,
                                 C18S699,C18S871,C18S873,C18S935)
write.table(c18rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c18rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 19
cromo19treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 19/Cromossomo19Treino.csv')
c19rf = cromo19treino %>% select(C19S3754,C19S4536,C19S5484,C19S740,C19S1918,
                                 C19S3534,C19S4035)
write.table(c19rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c19rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 20
cromo20treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 20/Cromossomo20Treino.csv')
c20rf = cromo20treino %>% select(C20S968,C20S1000,C20S1510,C20S2308,C20S2532,
                                 C20S2141,C20S2142,C20S1103,C20S1351,C20S2310,
                                 C20S333,C20S1730,C20S2098,C20S335,C20S1708,
                                 C20S2486,C20S2178,C20S1892,C20S2136,C20S2251,
                                 C20S669,C20S1198,C20S637,C20S2092,C20S2212)
write.table(c20rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c20rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 21
cromo21treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 21/Cromossomo21Treino.csv')
c21rf = cromo21treino %>% select(C21S575,C21S862,C21S900,C21S926,C21S954,
                                 C21S985,C21S1001,C21S1296,C21S700,C21S730,
                                 C21S1208,C21S216,C21S1118,C21S1151,C21S356,
                                 C21S1144,C21S1538,C21S355,C21S1177,C21S1510,
                                 C21S469,C21S898,C21S1096,C21S1436,C21S194,
                                 C21S247,C21S613)
write.table(c21rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c21rf.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 22
cromo22treino = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 22/Cromossomo22Treino.csv')
c22rf = cromo22treino %>% select(C22S360,C22S904,C22S1254,C22S1262,C22S1592,
                                 C22S2131,C22S2288,C22S992,C22S2325,C22S1281,
                                 C22S1334,C22S1854,C22S163,C22S359,C22S1263,
                                 C22S2429,C22S1911,C22S445,C22S1464,C22S2136,
                                 C22S861,C22S1271,C22S1905)
write.table(c22rf, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c22rf.csv",sep=';',dec=',')

### Banco das selecionadas
AFFECTED = cromo22treino %>% select(AFFECTED)


# LASSO
selecionados_lasso_validacao = cbind(AFFECTED,c3lasso,c15lasso,c18lasso)
write.table(selecionados_lasso_validacao, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/selecionados_lasso_validacao.csv",sep=';',dec=',')

# Random Forest
selecionados_rf_validacao = cbind(AFFECTED,c1rf,c2rf,c3rf,c4rf,c5rf,c6rf,c7rf,c8rf,c9rf,c10rf,
                                  c11rf,c12rf,c13rf,c14rf,c15rf,c16rf,c17rf,c18rf,c19rf,c20rf,
                                  c21rf,c22rf)
write.table(selecionados_rf_validacao, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/selecionados_rf_validacao.csv",sep=';',dec=',')

##### Validação

### Variaveis selecionadas - Cromossomo 1
cromo1Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 1/Cromossomo1Validacao.csv')
c1rfv = cromo1Validacao %>% select(C1S6634,C1S8166,C1S7419,C1S8894,C1S874,
                                   C1S10412,C1S9939,C1S1442,C1S2918,C1S1003,
                                   C1S3073,C1S6587,C1S6752)
write.table(c1rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c1rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 2
cromo2Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 2/Cromossomo2Validacao.csv')
c2rfv = cromo2Validacao %>% select(C2S1135,C2S2485,C2S4740,C2S6847,C2S6997,
                                   C2S7753,C2S5945,C2S6125,C2S1247,C2S3103,
                                   C2S6899,C2S7926,C2S3630,C2S5927)
write.table(c2rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c2rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 3
cromo3Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 3/Cromossomo3Validacao.csv')
c3lassov = cromo3Validacao %>% select(C3S5389,C3S5742,C3S4611)
write.table(c3lassov, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c3lassovalid.csv",sep=';',dec=',')

c3rfv = cromo3Validacao %>% select(C3S455,C3S2149,C3S2621,C3S3621,C3S4611,
                                   C3S5389,C3S5670,C3S5742,C3S2600,C3S3141,
                                   C3S4529,C3S646,C3S988,C3S1174,C3S4387,
                                   C3S2569,C3S5650,C3S2954,C3S5703)
write.table(c3rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c3rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 4
cromo4Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 4/Cromossomo4Validacao.csv')
c4rfv = cromo4Validacao %>% select(C4S489,C4S1251,C4S1553,C4S1809,C4S3917,
                                   C4S4178,C4S4342,C4S580,C4S518,C4S3375,
                                   C4S1001,C4S5043,C4S796,C4S2799,C4S4783,
                                   C4S668,C4S5106,C4S3374,C4S3975,C4S236)
write.table(c4rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c4rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 5
cromo5Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 5/Cromossomo5Validacao.csv')
c5rfv = cromo5Validacao %>% select(C5S3051,C5S3151,C5S3339,C5S3209,C5S3163,
                                   C5S3192,C5S1018,C5S1793,C5S2894,C5S3236,
                                   C5S1239,C5S2020,C5S3027,C5S1866,C5S3140,
                                   C5S2491,C5S3150,C5S3191,C5S3269,C5S4089)
write.table(c5rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c5rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 6
cromo6Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 6/Cromossomo6Validacao.csv')
c6rfv = cromo6Validacao %>% select(C6S3891,C6S4554,C6S5619,C6S397,C6S4303,
                                   C6S4481,C6S1126,C6S1162,C6S1761,C6S1924,
                                   C6S1088,C6S1504,C6S1905,C6S3730)
write.table(c6rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c6rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 7
cromo7Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 7/Cromossomo7Validacao.csv')
c7rfv = cromo7Validacao %>% select(C7S1818,C7S76,C7S151,C7S2352,C7S3495,
                                   C7S3622,C7S5454,C7S1589,C7S2562,C7S2773,
                                   C7S3604,C7S663,C7S2340,C7S1002,C7S2549,
                                   C7S1598,C7S4034,C7S4012,C7S4199,C7S5488,
                                   C7S1894,C7S2279,C7S2677,C7S5096)
write.table(c7rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c7rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 8
cromo8Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 8/Cromossomo8Validacao.csv')
c8rfv = cromo8Validacao %>% select(C8S322,C8S1225,C8S2383,C8S3416,C8S3858,
                                   C8S850,C8S633,C8S929,C8S964,C8S2802,
                                   C8S3435,C8S4540,C8S4809,C8S464,C8S1730,
                                   C8S1732,C8S1781,C8S4932,C8S271,C8S323,
                                   C8S2043,C8S4340,C8S4379)
write.table(c8rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c8rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 9
cromo9Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 9/Cromossomo9Validacao.csv')
c9rfv = cromo9Validacao %>% select(C9S1473,C9S1506,C9S4298,C9S4721,C9S1723,
                                   C9S1802,C9S3126,C9S3244,C9S3725,C9S3000,
                                   C9S3117,C9S3718,C9S392,C9S2992,C9S3231,
                                   C9S3600,C9S1654,C9S3312)
write.table(c9rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c9rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 10
cromo10Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 10/Cromossomo10Validacao.csv')
c10rfv = cromo10Validacao %>% select(C10S297,C10S2761,C10S3061,C10S5143,C10S2891,
                                     C10S4403,C10S5083,C10S4018,C10S2413,C10S5019,
                                     C10S4334,C10S6369,C10S6418,C10S2829,C10S4824,
                                     C10S5540,C10S6621,C10S2841,C10S4466,C10S5768)
write.table(c10rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c10rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 11
cromo11Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 11/Cromossomo11Validacao.csv')
c11rfv = cromo11Validacao %>% select(C11S666,C11S5465,C11S6472,C11S38,C11S3309,
                                     C11S5651,C11S5641,C11S2664,C11S2458,C11S3912,
                                     C11S5633,C11S6914,C11S189,C11S2916)
write.table(c11rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c11rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 12
cromo12Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 12/Cromossomo12Validacao.csv')
c12rfv = cromo12Validacao %>% select(C12S6558,C12S6748,C12S6028,C12S1264,C12S3909,
                                     C12S6573,C12S5036,C12S3581,C12S3732,C12S675,
                                     C12S3140,C12S1339,C12S5139,C12S574)
write.table(c12rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c12rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 13
cromo13Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 13/Cromossomo13Validacao.csv')
c13rfv = cromo13Validacao %>% select(C13S105,C13S320,C13S756,C13S761,C13S779,
                                     C13S845,C13S999,C13S1034,C13S1216,C13S1337,
                                     C13S1429,C13S1581,C13S1966,C13S1994,C13S626,
                                     C13S838,C13S978,C13S1304,C13S1697,C13S212,
                                     C13S780,C13S1097,C13S951,C13S744,C13S753,
                                     C13S524,C13S163)
write.table(c13rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c13rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 14
cromo14Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 14/Cromossomo14Validacao.csv')
c14rfv = cromo14Validacao %>% select(C14S56,C14S68,C14S595,C14S710,C14S987,
                                     C14S1000,C14S1669,C14S2839,C14S3575,C14S988,
                                     C14S2003,C14S2384,C14S23,C14S342,C14S438,
                                     C14S2849,C14S1910,C14S972,C14S2894,C14S3200,
                                     C14S700,C14S1135,C14S1039,C14S1462)
write.table(c14rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c14rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 15
cromo15Validacao = read.table('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 15/Cromossomo15Validacao.csv', sep=';', head=T)
c15lassov = cromo15Validacao %>% select(C15S774)
write.table(c15lassov, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c15lassovalid.csv",sep=';',dec=',')

c15rfv = cromo15Validacao %>% select(C15S303,C15S774,C15S2128,C15S3513,C15S4023,
                                     C15S4286,C15S4354,C15S4644,C15S241,C15S252,
                                     C15S3062,C15S4033,C15S4330,C15S197,C15S3576,
                                     C15S4083,C15S2418,C15S3815,C15S4057,C15S1933,
                                     C15S1684,C15S430,C15S3683,C15S3825,C15S4041,
                                     C15S4477,C15S4285,C15S4072)
write.table(c15rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c15rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 16
cromo16Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 16/Cromossomo16Validacao.csv')
c16rfv = cromo16Validacao %>% select(C16S220,C16S370,C16S1800,C16S3093,C16S3138,
                                     C16S3228,C16S3619,C16S3666,C16S2546,C16S408,
                                     C16S464,C16S2590,C16S549,C16S1345,C16S3234,
                                     C16S3882,C16S2816,C16S3109,C16S3229,C16S3493,
                                     C16S366,C16S3298,C16S3377,C16S3430,C16S77,
                                     C16S1740,C16S3108)
write.table(c16rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c16rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 17
cromo17Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 17/Cromossomo17Validacao.csv')
c17rfv = cromo17Validacao %>% select(C17S2868,C17S2960,C17S3304,C17S4431,C17S4432,
                                     C17S5273,C17S3437,C17S2160,C17S2842,C17S4907,
                                     C17S5244,C17S3305,C17S4420,C17S4722,C17S4040,
                                     C17S3288,C17S3289,C17S3424,C17S5338)
write.table(c17rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c17rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 18
cromo18Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 18/Cromossomo18Validacao.csv')
c18lassov = cromo18Validacao %>% select(C18S2320)
write.table(c18lassov, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c18lassovalid.csv",sep=';',dec=',')

c18rfv = cromo18Validacao %>% select(C18S463,C18S764,C18S783,C18S1012,C18S1077,
                                     C18S1621,C18S1639,C18S1796,C18S2264,C18S2320,
                                     C18S2491,C18S700,C18S1631,C18S793,C18S1744,
                                     C18S1220,C18S602,C18S1121,C18S936,C18S1017,
                                     C18S699,C18S871,C18S873,C18S935)
write.table(c18rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c18rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 19
cromo19Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 19/Cromossomo19Validacao.csv')
c19rfv = cromo19Validacao %>% select(C19S3754,C19S4536,C19S5484,C19S740,C19S1918,
                                     C19S3534,C19S4035)
write.table(c19rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c19rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 20
cromo20Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 20/Cromossomo20Validacao.csv')
c20rfv = cromo20Validacao %>% select(C20S968,C20S1000,C20S1510,C20S2308,C20S2532,
                                     C20S2141,C20S2142,C20S1103,C20S1351,C20S2310,
                                     C20S333,C20S1730,C20S2098,C20S335,C20S1708,
                                     C20S2486,C20S2178,C20S1892,C20S2136,C20S2251,
                                     C20S669,C20S1198,C20S637,C20S2092,C20S2212)
write.table(c20rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c20rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 21
cromo21Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 21/Cromossomo21Validacao.csv')
c21rfv = cromo21Validacao %>% select(C21S575,C21S862,C21S900,C21S926,C21S954,
                                     C21S985,C21S1001,C21S1296,C21S700,C21S730,
                                     C21S1208,C21S216,C21S1118,C21S1151,C21S356,
                                     C21S1144,C21S1538,C21S355,C21S1177,C21S1510,
                                     C21S469,C21S898,C21S1096,C21S1436,C21S194,
                                     C21S247,C21S613)
write.table(c21rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c21rfvalid.csv",sep=';',dec=',')

### Variaveis selecionadas - Cromossomo 22
cromo22Validacao = read.csv('C:/Mariana/UFSCar/TCC/Dados transformados/Dados filtrados/Cromossomo 22/Cromossomo22Validacao.csv')
c22rfv = cromo22Validacao %>% select(C22S360,C22S904,C22S1254,C22S1262,C22S1592,
                                     C22S2131,C22S2288,C22S992,C22S2325,C22S1281,
                                     C22S1334,C22S1854,C22S163,C22S359,C22S1263,
                                     C22S2429,C22S1911,C22S445,C22S1464,C22S2136,
                                     C22S861,C22S1271,C22S1905)
write.table(c22rfv, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/c22rfvalid.csv",sep=';',dec=',')

### Banco das selecionadas
AFFECTED = cromo22Validacao %>% select(AFFECTED)


# LASSO
selecionados_lasso_validacao = cbind(AFFECTED,c3lassov,c15lassov,c18lassov)
write.table(selecionados_lasso_validacao, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/selecionados_lasso_validacao.csv",sep=';',dec=',')

# Random Forest
selecionados_rf_validacao = cbind(AFFECTED,c1rfv,c2rfv,c3rfv,c4rfv,c5rfv,c6rfv,c7rfv,c8rfv,c9rfv,c10rfv,
                                  c11rfv,c12rfv,c13rfv,c14rfv,c15rfv,c16rfv,c17rfv,c18rfv,c19rfv,c20rfv,
                                  c21rfv,c22rfv)
write.table(selecionados_rf_validacao, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/selecionados_rf_validacao.csv",sep=';',dec=',')

# Descritiva Selecionadas LASSO

table(lasso$C3S5389)
table(lasso_valid$C3S5389)
table(lasso$C3S5742)
table(lasso_valid$C3S5742)
table(lasso$C3S4611)
table(lasso_valid$C3S4611)
table(lasso$C15S774)
table(lasso_valid$C15S774)
table(lasso$C18S2320)
table(lasso_valid$C18S2320)
Cromossomo2 = Cromossomo2[-698,]
table(lasso_all$C3S5389,Cromossomo1$C1S9189)

table(lasso_all$C3S5389)
table(Cromossomo1$C1S9189)

table(lasso_all$C3S5742,Cromossomo1$C1S9432)

table(lasso_all$C3S5742)
table(Cromossomo1$C1S9432)


table(lasso_all$C15S774,Cromossomo14$C14S1382)
table(lasso_all$C15S774,Cromossomo14$C14S3704)

table(lasso_all$C18S2320,Cromossomo1$C1S9455)
table(lasso_all$C18S2320,Cromossomo1$C1S9266)
table(lasso_all$C18S2320,Cromossomo2$C2S2288)

table(lasso_all$C3S4611,Cromossomo1$C1S9455)
table(lasso_all$C3S4611,Cromossomo1$C1S9266)
table(lasso_all$C3S4611,Cromossomo2$C2S2288)

##### Modelos
library("gamlss")
### LASSO + GLM
lasso_all = rbind(lasso, lasso_valid)
lasso = read.csv("C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/selecionados_lasso.csv",sep=';',dec=',')
lasso_valid = selecionados_lasso_validacao
lasso_glm <- gamlss(AFFECTED ~.,data=lasso, family = BI)
plot(lasso_glm)
fit_treino_lasso = fitted(lasso_glm)
sqres_lasso = sum((lasso$AFFECTED - fit_treino_lasso)^2)
lasso_glm_pred = predictAll(lasso_glm,newdata=lasso_valid)
sqres_lasso_valid = sum((lasso_glm_pred$y - lasso_glm_pred$mu)^2)

lasso_glm$mu.coefficients


### RF + RF

rf_treino = read.csv("C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/selecionados_rf.csv",sep=';',dec=',')
rf_valid = selecionados_rf_validacao

# 1ª Semente: 1
library(dplyr)
library(glmnet)
library(plotmo)
library(caret)

dados = rf_treino
#dados = dados[,-1]
X <- model.matrix(AFFECTED~., dados)[,-1]
y <- dados$AFFECTED
set.seed(1)
index <- createDataPartition(y, p=0.70, list=FALSE)
X_train <- X[index, ]
X_test <- X[-index, ]
y_train <- y[index]
y_test<-y[-index]

xtr = as.matrix(X_train)
ytr = y_train
# cojunto validação
xval = as.matrix(X_test)
yval = y[-index]

novo = cbind(xval,yval)

library(randomForest)

# Izbick
m=sqrt(ncol(dados)) 
ajuste1 = randomForest(X,y,mtry=m,
                       importance = TRUE, type = regression, proximity = T)
rf1 = ajuste1$importance
write.table(rf1, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/1aSemente - RF - 1 - selecionadas.csv",sep=';',dec=',')

pdf("C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/1aSemente - RF - 1 - selecionadas.pdf")

rf = varImpPlot(ajuste1, main = "Ajuste via Florestas Aleatórias",
                pch = 16, col = 'turquoise4') 
dev.off()

###

png("C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/1aSemente - RF - 1 - selecionadas.png")

rf = varImpPlot(ajuste1, main = "Ajuste via Florestas Aleatórias",
                pch = 16, col = 'turquoise4')
dev.off()

sqres_ajuste1 = sum((ajuste1$y-ajuste1$predicted)^2) # Resíduos treino

# Validacao

valid_ajuste1 = predict(ajuste1,rf_valid) # Aplicandeo o modelo
sqres_ajuste1_valid = sum((rf_valid$AFFECTED-valid_ajuste1)^2) # Resíduos validação



rf_treino = read.csv("C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/selecionados_rf.csv",sep=';',dec=',')
rf_valid = selecionados_rf_validacao

# 2ª Semente: 31
library(dplyr)
library(glmnet)
library(plotmo)
library(caret)

dados = rf_treino
#dados = dados[,-1]
X <- model.matrix(AFFECTED~., dados)[,-1]
y <- dados$AFFECTED
set.seed(31)
index <- createDataPartition(y, p=0.70, list=FALSE)
X_train <- X[index, ]
X_test <- X[-index, ]
y_train <- y[index]
y_test<-y[-index]

xtr = as.matrix(X_train)
ytr = y_train
# cojunto validação
xval = as.matrix(X_test)
yval = y[-index]

novo = cbind(xval,yval)

library(randomForest)

# Izbick
m=sqrt(ncol(dados)) 
ajuste2 = randomForest(X,y,mtry=m,
                       importance = TRUE, type = regression, proximity = T)
rf1 = ajuste2$importance
write.table(rf1, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/2aSemente - RF - 31 - selecionadas.csv",sep=';',dec=',')

pdf("C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/2aSemente - RF - 31 - selecionadas.pdf")

rf = varImpPlot(ajuste2, main = "Ajuste via Florestas Aleatórias",
                pch = 16, col = 'turquoise4') 
dev.off()

###

png("C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/2aSemente - RF - 31 - selecionadas.png")

rf = varImpPlot(ajuste2, main = "Ajuste via Florestas Aleatórias",
                pch = 16, col = 'turquoise4')
sqres_ajuste2 = sum((ajuste2$y-ajuste2$predicted)^2) # Resíduos treino

# Validacao

valid_ajuste2 = predict(ajuste2,rf_valid) # Aplicandeo o modelo
sqres_ajuste2_valid = sum((rf_valid$AFFECTED-valid_ajuste2)^2) # Resíduos validação



rf_treino = read.csv("C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/selecionados_rf.csv",sep=';',dec=',')
rf_valid = selecionados_rf_validacao

# 3ª Semente: 13
library(dplyr)
library(glmnet)
library(plotmo)
library(caret)

dados = rf_treino
#dados = dados[,-1]
X <- model.matrix(AFFECTED~., dados)[,-1]
y <- dados$AFFECTED
set.seed(13)
index <- createDataPartition(y, p=0.70, list=FALSE)
X_train <- X[index, ]
X_test <- X[-index, ]
y_train <- y[index]
y_test<-y[-index]

xtr = as.matrix(X_train)
ytr = y_train
# cojunto validação
xval = as.matrix(X_test)
yval = y[-index]

novo = cbind(xval,yval)

library(randomForest)

# Izbick
m=sqrt(ncol(dados)) 
ajuste3 = randomForest(X,y,mtry=m,
                       importance = TRUE, type = regression, proximity = T)
rf1 = ajuste3$importance
write.table(rf1, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/3aSemente - RF - 13 - selecionadas.csv",sep=';',dec=',')

pdf("C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/3aSemente - RF - 13 - selecionadas.pdf")

rf = varImpPlot(ajuste3, main = "Ajuste via Florestas Aleatórias",
                pch = 16, col = 'turquoise4') 
dev.off()

###

png("C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/3aSemente - RF - 13 - selecionadas.png")

rf = varImpPlot(ajuste3, main = "Ajuste via Florestas Aleatórias",
                pch = 16, col = 'turquoise4')
sqres_ajuste3 = sum((ajuste3$y-ajuste3$predicted)^2) # Resíduos treino

# Validacao

valid_ajuste3 = predict(ajuste3,rf_valid) # Aplicandeo o modelo
sqres_ajuste3_valid = sum((rf_valid$AFFECTED-valid_ajuste3)^2) # Resíduos validação



rf_treino = read.csv("C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/selecionados_rf.csv",sep=';',dec=',')
rf_valid = selecionados_rf_validacao

# 4ª Semente: 24
library(dplyr)
library(glmnet)
library(plotmo)
library(caret)

dados = rf_treino
#dados = dados[,-1]
X <- model.matrix(AFFECTED~., dados)[,-1]
y <- dados$AFFECTED
set.seed(24)
index <- createDataPartition(y, p=0.70, list=FALSE)
X_train <- X[index, ]
X_test <- X[-index, ]
y_train <- y[index]
y_test<-y[-index]

xtr = as.matrix(X_train)
ytr = y_train
# cojunto validação
xval = as.matrix(X_test)
yval = y[-index]

novo = cbind(xval,yval)

library(randomForest)

# Izbick
m=sqrt(ncol(dados)) 
ajuste4 = randomForest(X,y,mtry=m,
                       importance = TRUE, type = regression, proximity = T)
rf1 = ajuste4$importance
write.table(rf1, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/4aSemente - RF - 24 - selecionadas.csv",sep=';',dec=',')

pdf("C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/4aSemente - RF - 24 - selecionadas.pdf")

rf = varImpPlot(ajuste4, main = "Ajuste via Florestas Aleatórias",
                pch = 16, col = 'turquoise4') 
dev.off()

###

png("C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/4aSemente - RF - 24 - selecionadas.png")

rf = varImpPlot(ajuste4, main = "Ajuste via Florestas Aleatórias",
                pch = 16, col = 'turquoise4')

sqres_ajuste4 = sum((ajuste4$y-ajuste4$predicted)^2) # Resíduos treino

# Validacao

valid_ajuste4 = predict(ajuste4,rf_valid) # Aplicandeo o modelo
sqres_ajuste4_valid = sum((rf_valid$AFFECTED-valid_ajuste4)^2) # Resíduos validação



rf_treino = read.csv("C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/selecionados_rf.csv",sep=';',dec=',')
rf_valid = selecionados_rf_validacao

# 5ª Semente: 12
library(dplyr)
library(glmnet)
library(plotmo)
library(caret)

dados = rf
#dados = dados[,-1]
X <- model.matrix(AFFECTED~., dados)[,-1]
y <- dados$AFFECTED
set.seed(12)
index <- createDataPartition(y, p=0.70, list=FALSE)
X_train <- X[index, ]
X_test <- X[-index, ]
y_train <- y[index]
y_test<-y[-index]

xtr = as.matrix(X_train)
ytr = y_train
# cojunto validação
xval = as.matrix(X_test)
yval = y[-index]

novo = cbind(xval,yval)

library(randomForest)

# Izbick
m=sqrt(ncol(dados)) 
ajuste5 = randomForest(X,y,mtry=m,
                       importance = TRUE, type = regression, proximity = T)
rf1 = ajuste5$importance
write.table(rf1, "C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/5aSemente - RF - 12 - selecionadas.csv",sep=';',dec=',')

pdf("C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/5aSemente - RF - 12 - selecionadas.pdf")

rf = varImpPlot(ajuste5, main = "Ajuste via Florestas Aleatórias",
                pch = 16, col = 'turquoise4') 
dev.off()

### 

png("C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/5aSemente - RF - 12 - selecionadas.png")

rf = varImpPlot(ajuste5, main = "Ajuste via Florestas Aleatórias",
                pch = 16, col = 'turquoise4')

sqres_ajuste5 = sum((ajuste5$y-ajuste5$predicted)^2) # Resíduos treino

# Validacao

valid_ajuste5 = predict(ajuste5,rf_valid) # Aplicandeo o modelo
sqres_ajuste5_valid = sum((rf_valid$AFFECTED-valid_ajuste5)^2) # Resíduos validação

### RF + GLM
library(gamlss)
rf_select = read.csv("C:/Mariana/UFSCar/TCC/Dados transformados/Dados Selecionados/selecionados_rf.csv",sep=';',dec=',')

rf_glm <- gamlss(AFFECTED ~.,data=rf_select, family = BI)
plot(rf_glm)

fit_treino_rf = fitted(rf_glm)
sqres_rf = sum((rf_treino$AFFECTED - fit_treino_rf)^2)
rf_glm_pred = predictAll(rf_glm,newdata=rf_valid)
sqres_rf_valid = sum((rf_glm_pred$y - rf_glm_pred$mu)^2)
length(rf_glm$mu.coefficients)


library(MASS)
# require(MASS)
#AIC = 764 - inicio
#AIC = 152.0005 - final
step.model = step(rf_glm, direction = "backward", steps = 1000)
step_backward_1000 = step.model
step_backward_1000$aic 

fit_treino_rf_back = fitted(step_backward_1000)
sqres_rf_back = sum((rf_treino$AFFECTED - fit_treino_rf_back)^2)
rf_glm_pred_back = predictAll(step_backward_1000,newdata=rf_valid)
sqres_rf_valid_back = sum((rf_glm_pred_back$y - rf_glm_pred_back$mu)^2)
length(step_backward_1000$mu.coefficients)


#AIC = 764 - inicio
step_both_1000 = step(rf_glm, direction = "both", steps = 1000)
step_both_1000$aic
length(step_both_1000$mu.coefficients)
table(Cromossomo22f$AFFECTED)
#rm(objeto)
step_both_1000$mu.coefficients

fit_treino_rf_step = fitted(step_both_1000)
sqres_rf_step = sum((step_both_1000 - fit_treino_rf_step)^2)
rf_glm_pred_step = predictAll(step_both_1000,newdata=rf_valid)
sqres_rf_valid_step = sum((rf_glm_pred_step$y - rf_glm_pred_step$mu)^2)

rf_valid_select = rf_valid  %>% select(AFFECTED,C1S7419,C1S9939,C1S1442,C2S4740,
                                       C2S7753,C2S6125,C3S2149,C3S5742,C3S3141,
                                       C3S646,C3S5703,C4S1809,C5S3051,C5S3339,
                                       C5S3236,C6S5619,C7S3622,C7S5454,C7S4199,
                                       C8S322,C8S3416,C8S850,C8S929,C8S464,
                                       C8S271,C8S2043,C9S1654,C10S2761,C10S5143, 
                                       C10S4403,C10S5019,C10S5540,C11S666,C11S6472,
                                       C11S5651,C11S3912,C11S189,C12S6028,C12S3581,
                                       C12S3732,C12S574,C13S320,C13S999,C13S1966,
                                       C13S951,C14S342,C14S438,C14S1039,C15S774,
                                       C15S3513,C15S4354,C15S4644,C15S3825,C15S4285,
                                       C16S3666,C16S549,C16S3109,C16S3430,C16S1740,
                                       C16S3108,C17S3305,C17S3424,C18S463,C18S2320,
                                       C18S871,C20S2308,C20S1351,C20S2098,C20S1708,
                                       C20S669,C21S216,C21S898,C21S194,C22S2131,C22S1281)
rf_glm_pred_step_test = predictAll(step_both_1000,newdata=rf_valid_select)
sqres_rf_valid_step_t = sum((rf_glm_pred_step_test$y - rf_glm_pred_step_test$mu)^2)
