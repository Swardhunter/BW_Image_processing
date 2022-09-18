library(readxl)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(sqldf)
library(stringr)
library(reshape2)
library(ggpattern)
library(gridExtra)
library(cowplot)

#LOAD Data
setwd(r"(D:/mskenawi/one drive/OneDrive - NTNU/PhD/LUHP/Summary/Summary of Data/Direct_LU_Res)")
DirectLO =  list.files(pattern='*.xlsx')
info <- read_excel("D:/mskenawi/one drive/OneDrive - NTNU/PhD/LUHP/Summary/Summary of Data/info.xlsx", 
                   col_types = c("text", "text", "text", 
                                 "numeric", "text", "numeric", "text", 
                                 "numeric", "text", "numeric", "numeric", 
                                 "numeric", "numeric", "text", "text", 
                                 "text", "text", "text"))#Fix the strings in info
info$magasinNavn = str_replace(info$magasinNavn, "-", "_")
info$magasinNavn = str_replace(info$magasinNavn, "/", "_")
info$magasinNavn = str_replace(info$magasinNavn, " ", "")
info$magasinNavn  =toupper(info$magasinNavn)


DLO = lapply(DirectLO, read_xlsx)
names(DLO) = DirectLO

for (i in 1:length(DLO)) {
  print(substring(DirectLO[i],1, nchar(DirectLO[i])-5))
  DLO[[i]]$Sch_Name = substring(DirectLO[i],1, nchar(DirectLO[i])-5)
  DLO[[i]]$Res_Name =toupper(DLO[[i]]$Res_Name)
  DLO[[i]]$Res_Name =str_replace(DLO[[i]]$Res_Name, "-", "_")
  DLO[[i]]$Res_Name =str_replace(DLO[[i]]$Res_Name, " ", "")
  print(DLO[[i]]$Res_Name)
  T2 =DLO[[i]]
  print(T2)
  TT2 = sqldf("select a.Res_Name, b.`Reservoir Type`,b.magasinNr,b.HP_Nr
      from info b join T2 a on a.Res_Name like '%' || b.magasinNavn || '%'")
  TT2 = na.omit(TT2)
  DLO[[i]]= merge(T2,TT2,by.x = "Res_Name",by.y = "Res_Name",all.x = T,all.y = F)
  DLO[[i]] = DLO[[i]][!duplicated(DLO[[i]]),]
  DLO[[i]] = DLO[[i]][,-2]
  TT2 = NA
}



#Plotting 
TR1 = DLO[[36]]
TR1 = TR1[,-2]
#TR1$Value = as.factor(as.character(TR1$Value))
TR1$Value[TR1$Value==0]="Urban"
TR1$Value[TR1$Value==1]="Vegetation"
TR1$Value[TR1$Value==2]="Water"
TR1$Value[TR1$Value==4]="Cultivated"
TR1$Value[TR1$Value==5]="Bare"
TR1$Value = factor(TR1$Value, levels=c("Urban","Cultivated",'Vegetation', 'Bare','Water'))
TR1$Res_Name = str_to_title(TR1$Res_Name)
#TR1 = melt(TR1,id.vars = c("Res_Name","Reservoir.Type","Value"))
Ord = aggregate(TR1$Area_KM2,by =list(TR1$Res_Name),FUN=sum)
tt = merge(Ord,TR1,by.x = "Group.1",by.y = "Res_Name",all = T)
TR1$ord = tt$x
TR1$Res_Name= reorder(as.factor(TR1$Res_Name),(TR1$ord))
theme_set(theme_bw(base_size=12))
TR1$Area_KM2 = round(TR1$Area_KM2,3)
ggplot(TR1,aes(x=Res_Name, y=Area_KM2,label=(ord))) +  #stat_summary(geom = "text",aes(x=Res_Name, y=ord),fun.y=mean,vjust =-0.5)+
  geom_col_pattern(
    aes(fill = Value,
        pattern=as.factor(as.character(`Reservoir Type`))),  width = 0.25,pattern_density = 0.1,pattern_spacing = .01,pattern_fill =NA)+
  scale_fill_manual(values =  c("Urban"= "#D16103","Cultivated"="#C4961A","Vegetation"="#52854C","Bare"="#F4EDCA","Water"="#4E84C4"))+ 
  xlab("Reservoir Name")+ ylab("Area Km2")+labs(fill = 'Land Class',pattern = "Reservoir Type")+
    theme(legend.box.just = "left",
          legend.justification = c(0.99,0.99),
          legend.position = c(0.99,0.99),
          legend.margin = unit(0.11,"lines"),
          axis.text.x = element_text(angle = 45,size = 12,vjust =1, hjust=1),
          axis.text.y= element_text(size = 12),
          axis.title=element_text(size=14))
          #legend.box = "vertical",
          #legend.key.size = unit(1,"lines"),
          #legend.text.align = 1,
          #legend.title.align = 3)
ggsave(paste(unique(TR1$Sch_Name),".png"),width = 297,height = 195,units = "mm",dpi = 2000)

setwd("D:/mskenawi/one drive/OneDrive - NTNU/PhD/LUHP/Summary/Summary of Data/Direct_LU_Res/Plot")

for (j in 1:length(DLO)) {
  
  #Plotting 
  TR1 = DLO[[j]]
 # TR1 = TR1[,-1]
  #TR1$Value = as.factor(as.character(TR1$Value))
  TR1$Value[TR1$Value==0]="Urban"
  TR1$Value[TR1$Value==1]="Vegetation"
  TR1$Value[TR1$Value==2]="Water"
  TR1$Value[TR1$Value==4]="Cultivated"
  TR1$Value[TR1$Value==5]="Bare"
  TR1$Value = factor(TR1$Value, levels=c("Urban","Cultivated",'Vegetation', 'Bare','Water'))
  TR1$Res_Name = str_to_title(TR1$Res_Name)
  #TR1 = melt(TR1,id.vars = c("Res_Name","Reservoir.Type","Value"))
  Ord = aggregate(TR1$Area_KM2,by =list(TR1$Res_Name),FUN=sum)
  tt = merge(Ord,TR1,by.x = "Group.1",by.y = "Res_Name",all = T)
  TR1$ord = tt$x
  TR1$Res_Name= reorder(as.factor(TR1$Res_Name),-1*(TR1$ord))
  theme_set(theme_bw(base_size=12))
  TR1$Area_KM2 = round(TR1$Area_KM2,3)
  ggplot(TR1,aes(x=Res_Name, y=Area_KM2,label=(ord))) +  #stat_summary(geom = "text",aes(x=Res_Name, y=ord),fun.y=mean,vjust =-0.5)+
    geom_col_pattern(
      aes(fill = Value,
          pattern=as.factor(as.character(`Reservoir Type`))),  width = 0.25,pattern_density = 0.1,pattern_spacing = .01,pattern_fill =NA)+
    scale_fill_manual(values =  c("Urban"= "#D16103","Cultivated"="#C4961A","Vegetation"="#52854C","Bare"="#F4EDCA","Water"="#4E84C4"))+ 
    xlab("Reservoir Name")+ ylab("Area Km2")+labs(fill = 'Land Class',pattern = "Reservoir Type")+
    theme(legend.box.just = "left",
          legend.justification = c(0.99,0.99),
          legend.position = c(0.99,0.99),
          legend.margin = unit(0.11,"lines"),
          axis.text.x = element_text(angle = 45,size = 12,vjust =1, hjust=1),
          axis.text.y= element_text(size = 12),
          axis.title=element_text(size=14))
  #legend.box = "vertical",
  #legend.key.size = unit(1,"lines"),
  #legend.text.align = 1,
  #legend.title.align = 3)
  ggsave(paste(unique(TR1$Sch_Name),".png"),width = 297,height = 195,units = "mm",dpi = 2000)
}

#Main Results PLotting 
#Aggregate resu;ts 
DLO_2 = do.call('rbind',DLO)
DLO_2$Res_Name  = as.factor(str_to_title(DLO_2$Res_Name))
Ord = aggregate(DLO_2$Area_KM2,by =list(DLO_2$Res_Name),FUN=sum)
tt = merge(Ord,DLO_2,by.x = "Group.1",by.y = "Res_Name",all = T)
DLO_2$LV = tt$x
DLO_2$Res_Name= reorder(as.factor(DLO_2$Res_Name),-1*(DLO_2$LV))
#PLOTTING

#TR1$Value = as.factor(as.character(TR1$Value))
DLO_2$Value[DLO_2$Value==0]="Urban"
DLO_2$Value[DLO_2$Value==1]="Vegetation"
DLO_2$Value[DLO_2$Value==2]="Water"
DLO_2$Value[DLO_2$Value==4]="Cultivated"
DLO_2$Value[DLO_2$Value==5]="Bare"
#DLO_2$Value = factor(DLO_2$Value, levels=c("Urban","Cultivated",'Vegetation', 'Bare','Water'))
DLO_2$Res_Name = str_to_title(DLO_2$Res_Name)
#TR1 = melt(TR1,id.vars = c("Res_Name","Reservoir.Type","Value"))

theme_set(theme_bw(base_size=12))
DLO_2$Area_KM2 = round(DLO_2$Area_KM2,3)
ggplot(DLO_2, aes(x=magasinNr, y = Area_KM2,fill = Value,pattern=as.factor(as.character(`Reservoir Type`))))+  #stat_summary(geom = "text",aes(x=Res_Name, y=ord),fun.y=mean,vjust =-0.5)+
  geom_col_pattern(width = 0.25,pattern_density = 0.1,pattern_spacing = .01,pattern_fill =NA)+
  scale_fill_manual(values =  c("Urban"= "#D16103","Cultivated"="#C4961A","Vegetation"="#52854C","Bare"="#F4EDCA","Water"="#4E84C4"))+ 
  xlab("Reservoir Name")+ ylab("Area Km2")+labs(fill = 'Land Class',pattern = "Reservoir Type")+
  theme(legend.box.just = "left",
        legend.justification = c(0.99,0.99),
        legend.position = c(0.99,0.99),
        legend.margin = unit(0.11,"lines"),
        axis.text.x = element_text(angle = 45,size = 10,vjust =1, hjust=1),
        axis.text.y= element_text(size = 10),
        axis.title=element_text(size=12))







TT = DLO_2[DLO_2$`Reservoir Type` =="Built",]
TT$Res_Name = as.factor(TT$Res_Name)
TT$Res_Name= reorder(TT$Res_Name,-1*as.numeric(as.character(TT$Area_KM2)),FUN = sum)
TT = TT[order(TT$Res_Name),]

TT=  arrange(TT, Res_Name, LV)
theme_set(theme_bw(base_size=12))
DLO_2$Area_KM2 = round(DLO_2$Area_KM2,3)

P1 = ggplot(TT, aes(x=Res_Name, y = Area_KM2,fill = Value))+  #stat_summary(geom = "text",aes(x=Res_Name, y=ord),fun.y=mean,vjust =-0.5)+
  geom_col(width = 0.65)+
  scale_fill_manual(values =  c("Urban"= "#D16103","Cultivated"="#C4961A","Vegetation"="#52854C","Bare"="#F4EDCA","Water"="#4E84C4"))+ 
  xlab("Reservoir Name")+ ylab("Area Km2")+labs(fill = 'Land Class',pattern = "Reservoir Type")+
  theme(legend.box.just = "left",
        legend.justification = c(0.99,0.99),
        legend.position = "none",
        legend.margin = unit(0.11,"lines"),
        axis.text.x = element_text(angle = 45,size = 10,vjust =1, hjust=1),
        axis.text.y= element_text(size = 10),
        axis.title=element_text(size=12),
        axis.title.y =element_blank(),axis.title.x =element_blank())

TT2 = DLO_2[DLO_2$`Reservoir Type` =="Expand",]
TT2$Res_Name = as.factor(TT2$Res_Name)
TT2$Res_Name= reorder(TT2$Res_Name,-1*as.numeric(as.character(TT2$Area_KM2)),FUN = sum)
P2 = ggplot(TT2, aes(x=Res_Name, y = Area_KM2,fill = Value))+  #stat_summary(geom = "text",aes(x=Res_Name, y=ord),fun.y=mean,vjust =-0.5)+
  geom_col(width = 0.65)+
  scale_fill_manual(values =  c("Urban"= "#D16103","Cultivated"="#C4961A","Vegetation"="#52854C","Bare"="#F4EDCA","Water"="#4E84C4"))+ 
  xlab("Reservoir Name")+ ylab("Area Km2")+labs(fill = 'Land Class',pattern = "Reservoir Type")+
  theme(legend.box.just = "left",
        legend.justification = c(0.99,0.99),
        legend.position = "none",
        legend.margin = unit(0.11,"lines"),
        axis.text.x = element_text(angle = 45,size = 10,vjust =1, hjust=1),
        axis.text.y= element_text(size = 10),
        axis.title=element_text(size=12),axis.title.x =element_blank())

TT3 = DLO_2[DLO_2$`Reservoir Type` =="Regulated",]
TT3$Res_Name = as.factor(TT3$Res_Name)
TT3$Res_Name= reorder(TT3$Res_Name,-1*as.numeric(as.character(TT3$Area_KM2)),FUN = sum)

P3 = ggplot(TT3, aes(x=Res_Name, y = Area_KM2,fill = Value))+  #stat_summary(geom = "text",aes(x=Res_Name, y=ord),fun.y=mean,vjust =-0.5)+
  geom_col(width = 0.65)+
  scale_fill_manual(values =  c("Urban"= "#D16103","Cultivated"="#C4961A","Vegetation"="#52854C","Bare"="#F4EDCA","Water"="#4E84C4"))+ 
  xlab("Reservoir Name")+ ylab("Area Km2")+labs(fill = 'Land Class',pattern = "Reservoir Type")+
  theme(legend.box.just = "left",
        legend.justification = c(0.99,0.99),
        legend.position = c(0.99,0.99),
        legend.margin = unit(0.11,"lines"),
        axis.text.x = element_text(angle = 45,size = 10,vjust =1, hjust=1),
        axis.text.y= element_text(size = 10),
        axis.title.x =element_blank())

ggarrange(vjust =1.9 ,font.label = list(face = "plain"),
  P3,                # First row with line plot
  # Second row with box and dot plots
  ggarrange(P2, P1,vjust =1.9 , ncol = 2,widths = c(1,0.5), labels = c("Expanded Reservoirs", "Built Reservoirs"),font.label = list(face = "plain")), 
  nrow = 2, 
  labels = "Regulated Reservoirs"
)       # Label of the line plot

ggsave("Direct Land Oc.png",width = 297,height = 195,units = "mm",dpi = 2000)


#Load Norwegian Reservoirs 
D = read_excel(r"(D:\mskenawi\one drive\OneDrive - NTNU\PhD\LUHP\Summary\Summary of Data\Reservoir_Nor.xlsx)")
D$magasinNr = as.factor(as.character(D$magasinNr))
D_F = D[D$magasinFormal_Liste =="Kraftproduksjon",]
#Remove Duplicates 
D_F = D_F[!duplicated(D_F$magasinNr),]
SUM  = sum(D_F$magasinArealHRV_km2,na.rm = T )

D2 = read_excel(r"(D:\mskenawi\one drive\OneDrive - NTNU\PhD\LUHP\Summary\Summary of Data\HP_Nor.xlsx)")
D2$vannkraftverkNr = as.factor(as.character(D2$vannkraftverkNr))
D2 = D2[!duplicated(D2$vannkraftverkNr),]
S_MW = sum(D2$maksYtelse_MW,na.rm = T)

# pLOT FOR ALL THE LAND CLASSES USED 
sUM_LC = aggregate(DLO_2$Area_KM2,by =list(DLO_2$Value),FUN=sum)
sUM_LC$Group.1 = reorder(sUM_LC$Group.1,-sUM_LC$x,sum)
sUM_LC$x = round(sUM_LC$x,3)
sUM_LC$PER = round(100*sUM_LC$x/sum(sUM_LC$x),2)

ggplot(sUM_LC, aes(x=Group.1, y = x,fill = Group.1))+geom_text(aes(label =paste(PER,"%")), vjust = -0.5)+
  geom_bar(position="stack", stat="identity")+scale_fill_manual(values =  c("Urban"= "#D16103","Cultivated"="#C4961A","Vegetation"="#52854C","Bare"="#F4EDCA","Water"="#4E84C4"))+xlab("Reservoir Name")+ ylab("Area Km2")+labs(fill = 'Land Class',pattern = "Reservoir Type")+
  theme(legend.box.just = "left",
        legend.justification = c(0.99,0.99),
        legend.position = c(0.99,0.99),
        legend.margin = unit(0.11,"lines"),
        axis.text.x = element_text(angle = 45,size = 10,vjust =1, hjust=1),
        axis.text.y= element_text(size = 10),
        axis.title.x =element_blank())
ggsave("Direct Land Oc area summary.png",width = 297,height = 210,units = "mm",dpi = 2000)


# pLOT FOR ALL THE LAND CLASSES USED 
RES_TY = aggregate(DLO_2$Area_KM2,by =list(DLO_2$`Reservoir Type`),FUN=sum)
RES_TY$Group.1 = reorder(RES_TY$Group.1,-RES_TY$x,sum)
RES_TY$x = round(RES_TY$x,3)
RES_TY$PER = round(100*RES_TY$x/sum(RES_TY$x),2)

ggplot(RES_TY, aes(x=Group.1, y = x,fill = Group.1))+geom_text(aes(label =paste(PER,"%")), vjust = -0.5)+
  geom_bar(position="stack", stat="identity")+scale_fill_manual(values =  c("#D16103","#C4961A","#52854C","#F4EDCA","#4E84C4"))+xlab("Reservoir Name")+ ylab("Area Km2")+labs(fill = 'Reservoir Type',pattern = "Reservoir Type")+
  theme(legend.box.just = "left",
        legend.justification = c(0.99,0.99),
        legend.position = c(0.99,0.99),
        legend.margin = unit(0.11,"lines"),
        axis.text.x = element_text(angle = 45,size = 12,vjust =1, hjust=1),
        axis.text.y= element_text(size = 12),
        axis.title.x =element_blank())

ggsave("Direct Land Oc area summary Res type .png",width = 297,height = 210,units = "mm",dpi = 2000)


#Load Confusion Matrix Data
CM  = read_excel(r"(D:\mskenawi\one drive\OneDrive - NTNU\PhD\LUHP\Summary\Summary of Data\ConfusionMatrix_Sum.xlsx)")


ggplot(UA_C, aes(x=CM.ClassValue, y = CM.U_Accuracy,fill = CM.ClassValue))+  #stat_summary(geom = "text",aes(x=Res_Name, y=ord),fun.y=mean,vjust =-0.5)+
  stat_summary(fun = mean, geom = "bar") + 
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  
  
  
  scale_fill_manual(values =  c("Urban"= "#D16103","Cultivated"="#C4961A","Vegetation"="#52854C","Bare"="#F4EDCA","Water"="#4E84C4"))+ 
  xlab("Class Name")+ ylab("User Accuracy")+labs(fill = 'Class Type',pattern = "Reservoir Type")+
  theme(legend.box.just = "left",
        legend.justification = c(0.99,0.99),
        legend.position = "none",
        legend.margin = unit(0.11,"lines"),
        axis.text.x = element_text(angle = 45,size = 12,vjust =1, hjust=1),
        axis.text.y= element_text(size = 12),
        axis.title=element_text(size=14),axis.title.x =element_blank())
#Get Summary of The accuracy 
UA = data.frame(CM$ClassValue,CM$U_Accuracy,CM$Kappa,CM$`Scheme Name`)
PA = CM[CM$ClassValue=="P_Accuracy",]
PA = PA[,c(3,4,5,6,7,8,12)]
PA = melt(PA,id.vars = c("ClassValue","Scheme Name"))
UA = UA[UA$CM.ClassValue!= "Kappa",]
UA = UA[UA$CM.ClassValue!= "P_Accuracy",]
UA = UA[UA$CM.ClassValue!= "Total",]
PA = na.omit(PA)
OA = merge(PA,UA,by.x = c("variable","Scheme Name"),by.y = c("CM.ClassValue","CM..Scheme.Name."),all = T)
names(OA) = c("CL_N","sCH_N","NON","PA","UA","KAB")
OA = OA[,-3]
OA = OA[,-5]
UA = aggregate(OA$UA,by =list(OA$sCH_N),FUN=mean)
PA = aggregate(OA$PA,by =list(OA$sCH_N),FUN=mean)
O_ALL = CM[CM$ClassValue=="P_Accuracy",c(10,12)]
OA = merge(OA,O_ALL, by.x = "sCH_N" , by.y ="Scheme Name",all = T)
names(OA)[5]= "OA"
OA = melt(OA,id.vars = c("sCH_N","CL_N"))
#plotting 
ggplot(OA, aes(x=sCH_N, y = value,fill =variable ))+  #stat_summary(geom = "text",aes(x=Res_Name, y=ord),fun.y=mean,vjust =-0.5)+
  stat_summary(fun = mean, geom = "bar",width = 0.5,position=position_dodge(0.5))+
  scale_fill_manual(values =  c("#999999", "#E69F00", "#56B4E9"))+ 
  xlab("Scheme Name")+ ylab("% Accuracy")+labs(fill = 'Accuracy Type',pattern = "Reservoir Type")+
  theme(legend.box.just = "left",
        legend.justification = c(0.99,0.99),
        legend.position = "top",
        legend.margin = unit(0.11,"lines"),
        axis.text.x = element_text(angle = 45,size = 12,vjust =1, hjust=1),
        axis.text.y= element_text(size = 12),
        axis.title=element_text(size=14),axis.title.x =element_blank())
  #stat_summary(fun.data = mean_se, geom = "errorbar")
ggsave("Confusion Matrix_bY sCH.png",width = 297,height = 210,units = "mm",dpi = 2000)

#plotting 
ggplot(OA, aes(x=variable, y = value,fill =variable ))+  #stat_summary(geom = "text",aes(x=Res_Name, y=ord),fun.y=mean,vjust =-0.5)+
  #stat_summary(fun = mean, geom = "bar",width = 0.5,position=position_dodge(0.5))+
  #stat_summary(fun.data = mean_se, geom = "errorbar")
  stat_summary(fun.data=f, geom="boxplot",width = 0.5) +  stat_summary(fun = o, geom="point",size = 1.6)   +   
  stat_summary(fun.data = f, geom = "errorbar",width= 0.25, size = 1.2)+ 
  scale_fill_manual(values =  c("#999999", "#E69F00", "#56B4E9"))+ 
  xlab("Scheme Name")+ ylab("% Accuracy")+labs(fill = 'Accuracy Type',pattern = "Reservoir Type")+
  theme(legend.box.just = "left",
        legend.justification = c(0.99,0.99),
        legend.position = c(0.99,0.99),
        legend.margin = unit(0.11,"lines"),
        axis.text.x = element_text(angle = 45,size = 12,vjust =1, hjust=1),
        axis.text.y= element_text(size = 12),
        axis.title=element_text(size=14))

ggsave("Confusion Matrix_bY Type .png",width = 297,height = 210,units = "mm",dpi = 2000)


#oVER Oall Accuaraacy 
#MAX DOWN RAMPING PLOTS 
#Adjusting the box plot whiskers to 95/5 percentile 
f <- function(x) {
  r <- quantile(x, probs = c(.05, 0.25, 0.5, 0.75, 0.95),na.rm = T)
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
g <- function(x) {
  r <- c(quantile(x, probs = c(.05, 0.25, 0.5, 0.75),na.rm = T),0.00461+(1.31*quantile(x, probs = c( 0.95),na.rm = T)))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

o <- function(x) {
  subset(x, x < quantile(x,.05,na.rm = T) | quantile(x,.95,na.rm = T) < x)
}
#User Accuracy Classes 
UA_C = data.frame(CM$ClassValue,CM$U_Accuracy,CM$`Scheme Name`)
UA_C= UA_C[UA_C$CM.ClassValue != "Kappa",]
UA_C= UA_C[UA_C$CM.ClassValue != "P_Accuracy",]
UA_C= UA_C[UA_C$CM.ClassValue != "Total",]
UA_C$CM.ClassValue = str_replace(UA_C$CM.ClassValue, "-", "_")
UA_C$CM.ClassValue[UA_C$CM.ClassValue=="C_0"]="Urban"
UA_C$CM.ClassValue[UA_C$CM.ClassValue=="C_1"]="Vegetation"
UA_C$CM.ClassValue[UA_C$CM.ClassValue=="C_2"]="Water"
UA_C$CM.ClassValue[UA_C$CM.ClassValue=="C_4"]="Cultivated"
UA_C$CM.ClassValue[UA_C$CM.ClassValue=="C_5"]="Bare"
names(UA_C)[3]= "Scheme Name"
names(UA_C)[2]= "UA"
#PA CLASSES 
PA_C = CM[CM$ClassValue=="P_Accuracy",c(4,5,6,7,8,12)]
names(PA_C) = c("Urban","Vegetation","Water","Cultivated","Bare","Scheme Name")

PA_C = melt(PA_C,id.vars = "Scheme Name" )
names(PA_C)[3]="PA"



# Getting oVERAll Accuracy for Each Class 
Urb = CM[CM$ClassValue=="C_0",c(4,9,12)]
Urb$OA = Urb$C_0/Urb$Total
Urb = data.frame(Urb$`Scheme Name`,Urb$OA)
names(Urb)[1] = "SCH"
#
VEG = CM[CM$ClassValue=="C_1",c(5,9,12)]
VEG$OA = VEG$C_1/VEG$Total
VEG = data.frame(VEG$`Scheme Name`,VEG$OA)
names(VEG)[1] = "SCH"

#
WAT = CM[CM$ClassValue=="C_2",c(6,9,12)]
WAT$OA = WAT$C_2/WAT$Total
WAT = data.frame(WAT$`Scheme Name`,WAT$OA)
names(WAT)[1] = "SCH"

#
CULT = CM[CM$ClassValue=="C_4",c(7,9,12)]
CULT$OA = CULT$C_4/CULT$Total
CULT = data.frame(CULT$`Scheme Name`,CULT$OA)
names(CULT)[1] = "SCH"


#
BARE = CM[CM$ClassValue=="C_5",c(8,9,12)]
BARE$OA = BARE$C_5/BARE$Total
BARE = data.frame(BARE$`Scheme Name`,BARE$OA)
names(BARE)[1] = "SCH"


#put all data frames into list
df_list =  list(Urb, VEG,WAT,CULT,BARE)

#merge all data frames in list
OA_CLS = Reduce(function(x, y) merge(x, y, by.x="SCH",by.y = "SCH",all=TRUE), df_list)
names(OA_CLS) = c("Scheme Name","Urban","Vegetation","Water","Cultivated","Bare")
OA_CLS = melt(OA_CLS,id.vars = "Scheme Name")
names(OA_CLS)[3] = "OA"
df =  merge(PA_C,OA_CLS,by.x = c("Scheme Name","variable"),by.y = c("Scheme Name","variable"),all = T)
df = merge(df,UA_C,by.x = c("Scheme Name","variable"),by.y = c("Scheme Name","CM.ClassValue"))
df = melt(df, id.vars = c("variable","Scheme Name"))
names(df)[1]="Class Name"
# pLOTTING 
#plotting 
ggplot(df, aes(x=`Class Name`, y = value,fill =variable ))+  #stat_summary(geom = "text",aes(x=Res_Name, y=ord),fun.y=mean,vjust =-0.5)+
  #stat_summary(fun = mean, geom = "bar",width = 0.5,position=position_dodge(0.5))+
  #stat_summary(fun.data = mean_se, geom = "errorbar")
  stat_summary(geom = "bar",fun = mean,width = 0.5,position = "dodge",stat = "identity" )+
#geom_bar(") +    
  stat_summary(aes(fill = variable), geom = "errorbar",width= 0.25, size = 0.5,position = position_dodge(0.5))+ 
  scale_fill_manual(values =  c("#999999", "#E69F00", "#56B4E9"))+ 
  xlab("Class Name")+ ylab("% Accuracy")+labs(fill = 'Accuracy Type',pattern = "Reservoir Type")+
  theme(legend.box.just = "top",
        legend.justification = c(0.99,0.99),
        legend.position = "top",
        legend.margin = unit(0.11,"lines"),
        axis.text.x = element_text(angle = 45,size = 12,vjust =1, hjust=1),
        axis.text.y= element_text(size = 12),
        axis.title=element_text(size=14))
ggsave("Confusion Matrix_bY class sum .png",width = 297,height = 210,units = "mm",dpi = 2000)



#Load Buffer Data 
buf = read_excel("D:/mskenawi/one drive/OneDrive - NTNU/PhD/LUHP/Summary/Summary of Data/info.xlsx", 
                   sheet = "Sheet2", col_names = FALSE)
buf_Dif = buf[buf$...3=="Diff",]
buf_Dif = buf_Dif[,c(1,2,4,5)]
names(buf_Dif) = c("Scheme Name","Res Buffer","Urbanisation","Veg Change")
buf_Dif = na.omit(buf_Dif)
buf_Dif$Urbanisation = as.numeric(buf_Dif$Urbanisation)
buf_Dif$`Veg Change` = as.numeric(buf_Dif$`Veg Change`)

buf_plt = melt(buf_Dif,id.vars = c("Scheme Name","Res Buffer"))

ggplot(buf_plt, aes(x=`Res Buffer`, y=value,fill = variable,color = variable)) +
  geom_segment( aes(x=`Res Buffer`, xend=`Res Buffer`, y=0, yend=value), color="grey") +
  geom_point(  size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Value of Y")


buf_plt$value = buf_plt$value
buf_plt$`Scheme Name` = reorder(buf_plt$`Scheme Name`,-buf_plt$value,sum)



ggplot(buf_plt, aes(x=`Scheme Name`, y=value,group = variable,color = variable)) +
  geom_linerange (position= position_dodge(0.2), aes(x=`Scheme Name`, xmax=`Scheme Name`, ymin=0, ymax=value,group= variable), color="grey") +
  geom_point(  position=position_dodge(width = 0.2),size=2,alpha=2) +scale_color_manual(values = c("#D16103","#52854C"))+
  theme_light() +
  theme(legend.box.just = "left",
        legend.justification = c(0.99,0.99),
        legend.position = c(0.99,0.99),
        legend.margin = unit(0.11,"lines"),
        axis.text.x = element_text(angle = 45,size = 12,vjust =1, hjust=1),
        axis.text.y= element_text(size = 12),
        axis.title=element_text(size=14),
    panel.grid.major.x = element_blank(),
    
    axis.ticks.x = element_blank()
  ) +ylim(min(-1),max(buf_plt$value))+
  xlab("Scheme Name") +
  ylab("Vegetation Change KM2")+labs(color = "Change Type",fill= "")

theme(legend.box.just = "left",
      legend.justification = c(0.99,0.99),
      legend.position = c(0.99,0.99),
      legend.margin = unit(0.11,"lines"),
      axis.text.x = element_text(angle = 45,size = 12,vjust =1, hjust=1),
      axis.text.y= element_text(size = 12),
      axis.title=element_text(size=14))
ggsave("Land Transformation Summary Lollipop.png",width = 297,height = 195,units = "mm",dpi = 2000)
#Bar 

ggplot(buf_plt, aes(x=`Scheme Name`, y=value,fill = variable,color = variable)) +
  geom_segment( aes(x=`Scheme Name`, xend=`Scheme Name`, y=0, yend=value), color="grey") +
  geom_bar(stat = "identity",position = ) +scale_color_manual(values = c("#D16103","#52854C"))+
  theme_light() +
  theme(legend.box.just = "left",
        legend.justification = c(0.99,0.99),
        legend.position = c(0.99,0.99),
        legend.margin = unit(0.11,"lines"),
        axis.text.x = element_text(angle = 45,size = 12,vjust =1, hjust=1),
        axis.text.y= element_text(size = 12),
        axis.title=element_text(size=14),
        panel.grid.major.x = element_blank(),
        
        axis.ticks.x = element_blank()
  ) +ylim(min(-5),max(buf_plt$value))+
  xlab("Scheme Name") +
  ylab("Vegetation Change KM2")

theme(legend.box.just = "left",
      legend.justification = c(0.99,0.99),
      legend.position = c(0.99,0.99),
      legend.margin = unit(0.11,"lines"),
      axis.text.x = element_text(angle = 45,size = 12,vjust =1, hjust=1),
      axis.text.y= element_text(size = 12),
      axis.title=element_text(size=14))




dodge <- position_dodge(width = 0.9)

ggplot(buf_plt, aes(x = interaction(buf_plt$`Res Buffer`,buf_plt$`Scheme Name`), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge())+
theme(legend.box.just = "left",
      legend.justification = c(0.99,0.99),
      legend.position = c(0.99,0.99),
      legend.margin = unit(0.11,"lines"),
      axis.text.x = element_text(angle = 45,size = 12,vjust =1, hjust=1),
      axis.text.y= element_text(size = 12),
      axis.title=element_text(size=14))

ggplot(data=buf_plt, aes(x=`Res Buffer`, y=buf_plt$value, fill=variable)) +
  geom_bar(position = 'dodge', stat="identity") +
  facet_grid(. ~ `Scheme Name`) + 
  theme(legend.position="none")


#Load Buffer Data 100m 
#LOAD Data
setwd(r"(D:\mskenawi\one drive\OneDrive - NTNU\PhD\LUHP\Summary\Summary of Data\Buffer_Analysis_Res)")
BUF_AN =  list.files(pattern='*.xlsx')
LT = lapply(BUF_AN, read_xlsx)
names(LT) = BUF_AN
for (i in 1:length(LT)) {
  LT[[i]]= LT[[i]][,-3]
  LT[[i]]$Perc_Urb = abs(LT[[i]]$Area_Dif_U)/sum(abs(LT[[i]]$Area_Dif_U),na.rm = T)
  LT[[i]]$Perc_Veg = abs(LT[[i]]$Area_Dif_NDEF)/sum(abs(LT[[i]]$Area_Dif_NDEF),na.rm = T)
  
}
LT =  do.call("rbind",LT)
LT[is.na(LT)==T]= 0
LT$Area_Dif_NDEF = -1*LT$Area_Dif_NDEF
#Seperate Variables 

LT_ABS = melt(LT[,c(-4,-5)], id.vars = c("Buffer_Zone"))
LT_PR = melt(LT[,c(-3,-2)], id.vars = c("Buffer_Zone"))

LT$Buffer_Zone=as.factor(LT$Buffer_Zone)
g$PER = g$percent

LT_ABS = cbind (LT_ABS,LT_PR)
LT_ABS$Buffer_Zone = as.factor(LT_ABS$Buffer_Zone)
LT_ABS= LT_ABS[,-4]
names(LT_ABS) = c("BUF_ZONE","ABS_VAR","V_ABS_VAR","PR_VAR","V_PR_VAR")
  LT_ABS$V_ABS_VAR = 100*  LT_ABS$V_ABS_VAR

#pLOTTING 
ggplot(LT_ABS, aes(x=BUF_ZONE, y = V_PR_VAR,fill =PR_VAR))+  #stat_summary(geom = "text",aes(x=Res_Name, y=ord),fun.y=mean,vjust =-0.5)+
  #stat_summary(fun = mean, geom = "bar",width = 0.5,position=position_dodge(0.5))+
  #stat_summary(fun.data = mean_se, geom = "errorbar")
  stat_summary(geom = "bar",fun = mean,width = 0.5,position = "dodge",stat = "identity" )+
  #geom_label(stat = "identity")+
  #geom_bar(") +    
  stat_summary( geom = "errorbar",position = "dodge",width= 0.5, size = .5)+ 
  
  scale_fill_manual(labels=c('Urban Change','Vegetation change'),values =  c(  "#999999","#E69F00", "#E69F00", "#56B4E9"))+
  xlab("Buffer Zone (M)")+ ylab("%Areal Change")+labs(fill = 'Land Change Type',pattern = "Reservoir Type")+
  theme(legend.box.just = "left",
        legend.justification = c(0.99,0.99),
        legend.position = c(0.99,0.99),
        legend.margin = unit(0.11,"lines"),
        axis.text.x = element_text(angle = 45,size = 12,vjust =1, hjust=1),
        axis.text.y= element_text(size = 12),
        axis.title=element_text(size=14))
ggsave("Buffer Analysis Reservoir_PER.png",width = 297,height = 195,units = "mm",dpi = 2000)

#Box Plot            
ggplot(LT, aes(x=Buffer_Zone, y = value,fill =variable ))+  #stat_summary(geom = "text",aes(x=Res_Name, y=ord),fun.y=mean,vjust =-0.5)+
  #stat_summary(fun = mean, geom = "bar",width = 0.5,position=position_dodge(0.5))+
  #stat_summary(fun.data = mean_se, geom = "errorbar")
  stat_summary(fun.data=f, geom="boxplot",position = "dodge",width = 0.5,aes(fill = variable)) +  stat_summary(aes(color= variable),fun = o, geom="point",position =position_dodge(0.5),stat = "identity",size = 1.9)   +   
  stat_summary(fun.data = f, geom = "errorbar",position = "dodge",width= 0.5, size = .5)+ 
  scale_fill_manual(labels=c('Vegetation change', 'Urban Change'),values =  c( "#E69F00", "#999999", "#E69F00", "#56B4E9"))+
  scale_color_manual(values =  c( "#E69F00", "#999999", "#E69F00", "#56B4E9"))+guides(colour = "none")+
  ylim(-50,200)+
  xlab("Buffer Zone (M)")+ ylab("Areal Change (Ha)")+labs(fill = 'Land Change Type',pattern = "Reservoir Type")+
  theme(legend.box.just = "left",
        legend.justification = c(0.99,0.99),
        legend.position = c(0.99,0.99),
        legend.margin = unit(0.11,"lines"),
        axis.text.x = element_text(angle = 45,size = 12,vjust =1, hjust=1),
        axis.text.y= element_text(size = 12),
        axis.title=element_text(size=14))
ggsave("Buffer Analysis Reservoir box Plot.png",width = 297,height = 195,units = "mm",dpi = 2000)

ggplot(data = LT, aes(x = factor(Buffer_Zone), y = prop.table(stat(VU)),fill = factor(variable), label = scales::percent(prop.table(stat(VU))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) 



#Load Buffer Data 100m 
#LOAD Data
setwd(r"(D:\mskenawi\one drive\OneDrive - NTNU\PhD\LUHP\Summary\Summary of Data\LT_Buf_Intk)")
BUF_IN =  list.files(pattern='*.xlsx')
LT = lapply(BUF_IN, read_xlsx)
names(LT) = BUF_IN
LT =  do.call("rbind",LT)
LT = LT[,c(2,3,4,5)]
LT[is.na(LT)==T]= 0
LT$Area_Dif_NDEF = -1*LT$Area_Dif_NDEF
LT = melt(LT, id.vars = c("Dam","Buffer_Zone"))
LT$value = LT$value/10000
LT$Buffer_Zone = as.factor(LT$Buffer_Zone)
LT$Buffer_Zone = reorder(LT$Buffer_Zone,as.numeric(as.character(LT$Buffer_Zone)))

#pLOTTING 
ggplot(LT, aes(x=Buffer_Zone, y = value,fill =variable))+  #stat_summary(geom = "text",aes(x=Res_Name, y=ord),fun.y=mean,vjust =-0.5)+
  #stat_summary(fun = mean, geom = "bar",width = 0.5,position=position_dodge(0.5))+
  #stat_summary(fun.data = mean_se, geom = "errorbar")
  stat_summary(geom = "bar",fun = mean,width = 0.5,position = "dodge",stat = "identity" )+
  #geom_label(stat = "identity")+
  #geom_bar(") +    
  stat_summary( geom = "errorbar",position = "dodge",width= 0.5, size = .5)+ 
  
  scale_fill_manual(labels=c('Vegetation change','Urban Change'),values =  c("#E69F00",  "#999999", "#E69F00", "#56B4E9"))+
  xlab("Buffer Zone (M)")+ ylab("Areal Change (Ha)")+labs(fill = 'Land Change Type',pattern = "Reservoir Type")+
  theme(legend.box.just = "left",
        legend.justification = c(0.99,0.99),
        legend.position = c(0.99,0.99),
        legend.margin = unit(0.11,"lines"),
        axis.text.x = element_text(angle = 45,size = 12,vjust =1, hjust=1),
        axis.text.y= element_text(size = 12),
        axis.title=element_text(size=14))

ggsave("Buffer Analysis Intake.png",width = 297,height = 195,units = "mm",dpi = 2000)
