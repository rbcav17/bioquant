alim1=Tabela_Aves_Campo_Completa_ALIMENTO_export_r_ALIMENTO
alim2=alim1 [, -1]
rownames(alim2)=alim1[, 1]
goweralim2=daisy(alim2,metric="gower")
clusteralim2gower=hclust(goweralim2, method= "ward.D")
#importando a tabela corrigida preenchendo os dados ausentes para o dataframe alimen1
alimen1=Tabela_Aves_Campo_25set2018_ALIMENTO_ALIMENTO_para_r
#retirando a primeira coluna com os nomes das especies
alimen2=alimen1 [, -1]
row.names(alimen2)=alimen1[, 1]
row.names(alimen2)
#fazendo a matriz goweralimen2 de dissimilaridade com distancia de gower entre especies
#em funcao das dietas e substrato de forrageamento 
goweralimen2=daisy(alimen2, metric="gower")
#fazendo a analise de cluster usando o algoritmo de ward em cima da matriz de dissimiliaridade
#das especies goweralimen2
clusteralimen2gower=hclust(goweralimen2, method="ward.D")
#fazendo o grafico de cluster das especies
#exportei jpeg com largura 2000 para ver todas as especies
plot(clusteralimen2gower)
#repetindo o wilcoxon da primeira versao do trabalho
#importando 2 planilhas de dados com as contagens de cada especie nos 12 censos CRAD e 12 censos CO
#uma planilha com todas as especies vistas
#uma planilha com as especies vistas nas duas areas
cocradcomum=aves_crad_co_12amostras_so_visitas_26set2018_versao_r_Dias_total_sp_em_comum
cocradtotal=aves_crad_co_12amostras_so_visitas_26set2018_versao_r_Dias_total_todas_sp
# fazer wilcoxon:wilcox.test(tab1$CRAD, tab1$CO, paired=TRUE)
#----------------------especies em comum
wilcox.test (cocradcomum$`Total CRAD`,cocradcomum$`Total CO`, paired=TRUE)
#resultado Wilcoxon signed rank test with continuity correction

#data:  cocradcomum$`Total CRAD` and cocradcomum$`Total CO`
#V = 262, p-value = 0.02801
#alternative hypothesis: true location shift is not equal to 0
#------------------diferenca significativa especies em comum
#---------agora para todas as especies
wilcox.test (cocradtotal$`Total CRAD`, cocradtotal$`Total CO`, paired = TRUE)
#	Wilcoxon signed rank test with continuity correction
#data:  cocradtotal$`Total CRAD` and cocradtotal$`Total CO`
#V = 897, p-value = 0.0001733
#alternative hypothesis: true location shift is not equal to 0
