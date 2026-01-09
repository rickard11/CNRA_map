#read csvs
DP_E2<-read.csv("data/12-1-2025 CalGro Download/JLDP Escondido 2 Well Well 20231102-20251201.csv")
DP_E3<-read.csv("data/12-1-2025 CalGro Download/JLDP Escondido 3 Well Well 20231102-20251201.csv")
DP_E5<-read.csv("data/12-1-2025 CalGro Download/JLDP Escondido 5 Well Well 20231102-20251201.csv")
DP_G1<-read.csv("data/12-1-2025 CalGro Download/JLDP Gaspar 1 Well Well 20231102-20251201.csv")
DP_JV<-read.csv("data/12-1-2025 CalGro Download/JLDP Jalama vaqueros Well Well 20231102-20251201.csv")
DP_LE<-read.csv("data/12-1-2025 CalGro Download/JLDP Lower Espada New Well Well 20231102-20251201.csv")
DP_O1<-read.csv("data/12-1-2025 CalGro Download/JLDP Oaks 1 Well Well 20231102-20251201.csv")
DP_O3<-read.csv("data/12-1-2025 CalGro Download/JLDP Oaks 3 B Well Well 20231102-20251201.csv")
DP_O4<-read.csv("data/12-1-2025 CalGro Download/JLDP Oaks 4 Well Well 20231102-20251201.csv")
DP_O5<-read.csv("data/12-1-2025 CalGro Download/JLDP Oaks 5 Well Well 20231102-20251201.csv")
DP_QC<-read.csv("data/12-1-2025 CalGro Download/JLDP Quail Canyon Well Well 20231102-20251101.csv")
DP_Q1<-read.csv("data/12-1-2025 CalGro Download/JLDP Quarry 1 Well Well 20231102-20251201.csv")
DP_T1<-read.csv("data/12-1-2025 CalGro Download/JLDP Tinta 1 Well Well 20231102-20251201.csv")
DP_T3<-read.csv("data/12-1-2025 CalGro Download/JLDP Tinta 3 Well Well 20231102-20251201.csv")
DP_T4<-read.csv("data/12-1-2025 CalGro Download/JLDP Tinta 4 Well Well 20231102-20251201.csv")
DP_T5<-read.csv("data/12-1-2025 CalGro Download/JLDP Tinta 5 Well Well 20231102-20251201.csv")
DP_T6<-read.csv("data/12-1-2025 CalGro Download/JLDP Tinta 6 Well Well 20231102-20251201.csv")
DP_T8<-read.csv("data/12-1-2025 CalGro Download/JLDP Tinta 8 Well Well 20231102-20251201.csv")
DP_T10<-read.csv("data/12-1-2025 CalGro Download/JLDP Tinta 10 Well Well 20231102-20251201.csv")
DP_T11A<-read.csv("data/12-1-2025 CalGro Download/JLDP Tinta 11A Well Well 20231102-20251201.csv")
DP_T11B<-read.csv("data/12-1-2025 CalGro Download/JLDP Tinta 11B Well Well 20231102-20251201.csv")
DP_WC<-read.csv("data/12-1-2025 CalGro Download/JLDP Wood Canyon Well Well 20231102-20251201.csv")

#Las Piletas
LP_C<-read.csv("data/12-1-2025 CalGro Download/Cooper plus Well 20231102-20251201.csv")
LP_NE<-read.csv("data/12-1-2025 CalGro Download/Las Piletas NE Corner Well 20231102-20251201.csv")
LP_W<-read.csv("data/12-1-2025 CalGro Download/Las Piletas Windmill Well 20231102-20251201.csv")

#Randall
R_B_C<-read.csv("data/12-1-2025 CalGro Download/Randall B and C Ranch Well Well 20231102-20251201.csv")
R_LT<-read.csv("data/12-1-2025 CalGro Download/Randall Lower Tweedy well Well 20231102-20251201.csv")
R_CP<-read.csv("data/12-1-2025 CalGro Download/Randall Cactus Pasture Well Well 20231102-20251201.csv")

#SCI
SCI_P<-read.csv("data/12-1-2025 CalGro Download/SCI Prisoners Harbor well 2.0  Well 20251102-20251201.csv")
SCI_MR<-read.csv("data/12-1-2025 CalGro Download/SCI Main Ranch South well Well 20251102-20251201.csv")

#Parks Creek
PC_PC<-read.csv("data/12-1-2025 CalGro Download/Parks Creek Well Well 20231102-20251201.csv")

#Tulare
Tul_Pix<-read.csv("data/12-1-2025 CalGro Download/Pixley_ Tulare_ Capinero Creek Well Well 20231101-20251201.csv")

# cut to Oct 1, 2025
library(dplyr)
library(purrr)

cutoff <- as.Date("2025-10-01")

df_list <- list(
  DP_E2=DP_E2,DP_E3=DP_E3,DP_E5=DP_E5,
  DP_G1=DP_G1,DP_JV=DP_JV,DP_LE=DP_LE,
  DP_O1=DP_O1,DP_O3=DP_O3,DP_O4=DP_O4,
  DP_O5=DP_O5,DP_QC=DP_QC,DP_Q1=DP_Q1,
  DP_T1=DP_T1,DP_T3=DP_T3,DP_T4=DP_T4,
  DP_T5=DP_T5,DP_T6=DP_T6,DP_T8=DP_T8,
  DP_T10=DP_T10,DP_T11A=DP_T11A,DP_T11B=DP_T11B,
  DP_WC=DP_WC
)

df_list_clean <- map(df_list, ~ filter(.x, Date.and.Time >= cutoff))

list2env(df_list_clean, envir = .GlobalEnv)

head(DP_O5)

# Make new column that adjusts first number to 0 and the rest relative thereafter
DP_E2 <- DP_E2 %>% arrange(Date.and.Time)
DP_E2$ft_adj <- DP_E2$ft..below.ground.-152.39
DP_E2$Site<-"E2"
DP_E3 <- DP_E3 %>% arrange(Date.and.Time)
DP_E3$ft_adj <- DP_E3$ft..below.ground.-57.07
DP_E3$Site<-"E3"
DP_E5 <- DP_E5 %>% arrange(Date.and.Time)
DP_E5$ft_adj <- DP_E5$ft..below.ground.-74.64
DP_E5$Site<-"E5"
DP_G1 <- DP_G1 %>% arrange(Date.and.Time)
DP_G1$ft_adj <- DP_G1$ft..below.ground.-122.49
DP_G1$Site<-"G1"
DP_JV <- DP_JV %>% arrange(Date.and.Time)
DP_JV$ft_adj <- DP_JV$ft..below.ground.-32.27
DP_JV$Site<-"JV"
#DP_LE <- DP_LE %>% arrange(Date.and.Time)
#DP_LE$ft_adj <- DP_LE$ft..below.ground.-20.77
DP_LE$Site<-"LE"
DP_O1 <- DP_O1 %>% arrange(Date.and.Time)
DP_O1$ft_adj <- DP_O1$ft..below.ground.-128.94
DP_O1$Site<-"O1"
DP_O3 <- DP_O3 %>% arrange(Date.and.Time)
DP_O3$ft_adj <- DP_O3$ft..below.ground.-147.39
DP_O3$Site<-"O3"
DP_O4 <- DP_O4 %>% arrange(Date.and.Time)
DP_O4$ft_adj <- DP_O4$ft..below.ground.-119.10
DP_O4$Site<-"O4"
#DP_O5 <- DP_O5 %>% arrange(Date.and.Time)
#DP_O5$ft_adj <- DP_O5$ft..below.ground.-74.64
#DP_O5$Site<-"O5"
DP_QC <- DP_QC %>% arrange(Date.and.Time)
DP_QC$ft_adj <- DP_QC$ft..below.ground.-60.68
DP_QC$Site<-"QC"
DP_Q1 <- DP_Q1 %>% arrange(Date.and.Time)
DP_Q1$ft_adj <- DP_Q1$ft..below.ground.-261.79
DP_Q1$Site<-"Q1"
DP_T1 <- DP_T1 %>% arrange(Date.and.Time)
DP_T1$ft_adj <- DP_T1$ft..below.ground.-86.81
DP_T1$Site<-"T1"
DP_T3 <- DP_T3 %>% arrange(Date.and.Time)
DP_T3$ft_adj <- DP_T3$ft..below.ground.-109.78
DP_T3$Site<-"T3"
DP_T4 <- DP_T4 %>% arrange(Date.and.Time)
DP_T4$ft_adj <- DP_T4$ft..below.ground.-79.33
DP_T4$Site<-"T4"
DP_T5 <- DP_T5 %>% arrange(Date.and.Time)
DP_T5$ft_adj <- DP_T5$ft..below.ground.-165.33
DP_T5$Site<-"T5"
DP_T6 <- DP_T6 %>% arrange(Date.and.Time)
DP_T6$ft_adj <- DP_T6$ft..below.ground.-188.89
DP_T6$Site<-"T6"
DP_T8 <- DP_T8 %>% arrange(Date.and.Time)
DP_T8$ft_adj <- DP_T8$ft..below.ground.-365.63
DP_T8$Site<-"T8"
DP_T10 <- DP_T10 %>% arrange(Date.and.Time)
DP_T10$ft_adj <- DP_T10$ft..below.ground.-206.35
DP_T10$Site<-"T10"
DP_T11A <- DP_T11A %>% arrange(Date.and.Time)
DP_T11A$ft_adj <- DP_T11A$ft..below.ground.-76.42
DP_T11A$Site<-"T11A"
DP_T11B <- DP_T11B %>% arrange(Date.and.Time)
DP_T11B$ft_adj <- DP_T11B$ft..below.ground.-150.03
DP_T11B$Site<-"T11B"
DP_WC <- DP_WC %>% arrange(Date.and.Time)
DP_WC$ft_adj <- DP_WC$ft..below.ground.-30.35
DP_WC$Site<-"WC"


# Merge datasets
All<-rbind(DP_E2,DP_E3,DP_E5,DP_G1,DP_JV,DP_O1,DP_O3,DP_O4,DP_QC,DP_Q1,DP_T1,DP_T3,
           DP_T4,DP_T5,DP_T6,DP_T8,DP_T10,DP_T11A,DP_T11B,DP_WC)
All$Date<-format(All$Date.and.Time,format="%Y-%m-%d")
Allagg<-aggregate(ft_adj~Date+Site,All,FUN=mean)
Allagg$Date<-as.Date(Allagg$Date,format="%Y-%m-%d")
#Make sure dates are dates
str(All)
All$Date.and.Time<-as.POSIXct(All$Date.and.Time,format="%Y-%m-%d %H:%M:%S")
#Plot
#library(ggplot2)
All
ggplot(Allagg,aes(x=Date))+geom_line(aes(y=ft_adj,color=Site))


##
SomeD<-rbind(DP_E2,DP_E3,DP_E5,DP_G1,DP_O1,DP_O3,DP_O4,DP_QC,DP_Q1)
SomeD$Date.and.Time<-as.POSIXct(SomeD$Date.and.Time,format="%Y-%m-%d %H:%M:%S")

SomeD$Date<-format(SomeD$Date.and.Time,format="%Y-%m-%d")
SomeDagg<-aggregate(ft_adj~Date+Site,SomeD,FUN=mean)
SomeDagg$Date<-as.Date(SomeDagg$Date,format="%Y-%m-%d")

ggplot(SomeDagg,aes(x=Date))+geom_line(aes(y=ft_adj,color=Site))
