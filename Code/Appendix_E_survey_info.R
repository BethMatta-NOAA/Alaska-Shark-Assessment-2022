###2022 Shark Assessment Appendix E 
##(Survey Biomass and Population Indices, Catch Distribution, and Length and Weights)


# LOAD LIBRARIES AND DEFINITIONS ------------------------------------------

library(tidyverse)
library(ggpubr)
library(sf)
library(rnaturalearth)
library(rgdal)
library(gridExtra)
library(grid)
library(ggridges)
library(patchwork)

AYR <- 2022  #Set assessment year
'%nin%' <- Negate('%in%')  #Make function to do inverse of %in%

# BIOMASS TABLES ----------------------------------------------------------

sharks.RACE <- read.csv("../Data/RACE_biomass_sharks2022.csv")
sharks.RACE$roundBiomass <- round(sharks.RACE$Biomass,0)
sharks.RACE$roundCV <- round(sharks.RACE$CV,2)

EBSslope.biomass.RACE <- sharks.RACE %>% 
  group_by(Group, YEAR) %>% 
  filter(REGULATORY_AREA_NAME == "EBS_SLOPE") %>% 
  select(YEAR, HAUL_COUNT, CATCH_COUNT, roundBiomass, roundCV)
write.csv(EBSslope.biomass.RACE, "../Tables/EBS_slope_biomass.csv", row.names = FALSE)

AI.biomass.RACE <- sharks.RACE %>% 
  group_by(Group, YEAR) %>% 
  filter(REGULATORY_AREA_NAME == "AI") %>% 
  select(YEAR, HAUL_COUNT, CATCH_COUNT, roundBiomass, roundCV)
write.csv(AI.biomass.RACE, "../Tables/AI_slope_biomass.csv", row.names = FALSE)

EBSshelf.biomass.RACE <- sharks.RACE %>% 
  group_by(Group, YEAR) %>% 
  filter(REGULATORY_AREA_NAME == "EBS_SHELF") %>% 
  select(YEAR, HAUL_COUNT, CATCH_COUNT, roundBiomass, roundCV)
write.csv(EBSshelf.biomass.RACE, "../Tables/EBS_shelf_biomass.csv", row.names = FALSE)

GOA.biomass.RACE <- sharks.RACE %>% 
  group_by(Group, YEAR) %>% 
  filter(REGULATORY_AREA_NAME == "GOA") %>% 
  select(YEAR, HAUL_COUNT, CATCH_COUNT, roundBiomass, roundCV)
write.csv(GOA.biomass.RACE, "../Tables/GOA_biomass.csv", row.names = FALSE)



# NONCOMMERCIAL CATCH SUMMARY-ALL SHARKS ----------------------------------
non.com <- read.csv("../Data/Noncommercial_Fishery_Catch.csv")
non.com <- non.com %>% rename(Year = "ï..Collection.Year")

ADFG <- non.com[non.com$Collection.Agency=="ADFG",]
IPHC <- non.com[non.com$Collection.Agency=="IPHC",]
AFSC <- non.com[non.com$Collection.Agency=="NMFS",]

AFSC.TWL <- AFSC[str_detect(AFSC$Collection.Name, "Trawl") | 
                   str_detect(AFSC$Collection.Name, "Acoustic"), ]
AFSC.TWL <- AFSC.TWL %>% 
  group_by(Year) %>% 
  summarize(t = round(sum(Weight)/1000,2)) %>% 
  mutate(Source = "AFSC.Trawl.Survey")

AFSC.LL <- AFSC %>% 
  filter(Collection.Name == "AFSC Annual Longline Survey") %>% 
  group_by(Year) %>% 
  summarize(numbers = sum(Number.Animals),
            t = round(sum(Weight)/1000,2)) %>% 
  mutate(Source = "AFSC.LL.Survey")

IPHC.LL <- IPHC %>% 
  filter(Collection.Name == "IPHC Annual Longline Survey") %>% 
  group_by(Year) %>% 
  summarize(t = round(sum(Weight)/1000,2)) %>% 
  mutate(Source = "IPHC.LL.Survey")

ADFG.all <- ADFG %>% 
  group_by(Year) %>% 
  summarize(t = round(sum(Weight)/1000,2)) %>% 
  mutate(Source = "ADFG.sport.and.research")

df_list <- list(AFSC.TWL, AFSC.LL, IPHC.LL, ADFG.all)
combo.table.l <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list, accumulate=FALSE)

combo.table.w <- combo.table.l %>% 
  pivot_wider(names_from = Source, 
              values_from = c(numbers, t)) %>% 
  select(Year, 
         "AFSC.Trawl.Survey.t" = "t_AFSC.Trawl.Survey",
         "AFSC.LL.Survey.n" = "numbers_AFSC.LL.Survey",
         "AFSC.LL.Survey.t" = "t_AFSC.LL.Survey",
         "IPHC.LL.Survey.n" = "numbers_IPHC.LL.Survey",
         "IPHC.LL.Survey.t" = "t_IPHC.LL.Survey",
         "ADFG.Sport.Research.t" = "t_ADFG.sport.and.research")

write.csv(combo.table.w, "../Tables/Noncommercial_catch.csv", 
          row.names = FALSE, na="")


# GRAPHS ------------------------------------------------------------------
# .Set up RACE survey data ------------------------------------------------

shark_biomass <- read.csv("../Data/RACE_biomass_sharks2022.csv")

#make pretty labels
shark_biomass$SURVEY <- factor(shark_biomass$SURVEY)
levels(shark_biomass$SURVEY) <- list(AI = "AI",
                                     "EBS Shelf" = "EBS_SHELF",
                                     "EBS Slope" = "EBS_SLOPE",
                                     NBS = "NBS",
                                     GOA = "GOA")

shark_biomass$FMP <- 0
BSAIsurv <-c ("AI","EBS Shelf","EBS Slope")
shark_biomass[shark_biomass$SURVEY %in% BSAIsurv,]$FMP <- "BSAI"
shark_biomass[shark_biomass$SURVEY=="GOA",]$FMP <- "GOA"

#need to calculate CI first
shark_biomass$UL <- shark_biomass$Biomass+sqrt(shark_biomass$Variance)*1.96
shark_biomass$LL <- shark_biomass$Biomass-sqrt(shark_biomass$Variance)*1.96

#get rid of extra groups
keeps <- c("Pacific Sleeper Shark","Spiny Dogfish","Salmon Shark")
shark_biomass <- shark_biomass[shark_biomass$Group %in% keeps,]


# ..RACE Survey Biomass BSAI (Fig 19.12 2020 BSAI SAFE)------------------------------------------
#Based on Cindy's code
#all surveys and species in one plot
RACE.BSAI.biomass.graph <- 
  ggplot(shark_biomass[shark_biomass$FMP=="BSAI" & 
                         shark_biomass$Group!="Salmon Shark",], aes(x=YEAR, y=Biomass,group=Group,fill=Group)) + 
  geom_bar(data=shark_biomass[(shark_biomass$Group=="Spiny Dogfish"&shark_biomass$FMP=="BSAI"),],
           stat="identity",aes(x=YEAR,y=Biomass),color="orange",fill="yellow",size=1) +
  geom_bar(data=shark_biomass[(shark_biomass$Group=="Pacific Sleeper Shark"&shark_biomass$FMP=="BSAI"),],
           stat="identity",aes(x=YEAR,y=Biomass),color="dark green",fill="light green",size=1) +
  facet_grid(Group~SURVEY,scales="free_y") +
  geom_linerange(aes(ymax=UL,ymin=Biomass),colour="black") +
  labs(y="Biomass (t)",x="Year") +
  scale_y_continuous(expand=c(0,0), labels=scales::comma_format()) +
  scale_x_continuous(breaks=seq(min(shark_biomass$YEAR), AYR, 5)) +
  theme_pubr(border=TRUE, base_size = 12) +
  theme(panel.grid.major=element_line(color="grey90"),
        axis.text=element_text(size=12,colour='black'),
        axis.text.x = element_text(angle = 45, hjust=1),
        axis.title = element_text(size=15),
        strip.text=element_text(size=12,colour='black',face="bold"))

ggsave("../Figures/RACE_BSAI_biomass.png", plot=RACE.BSAI.biomass.graph, dpi=600, height=7, width=8)


# ..All surveys GOA Dogfish combo figure (Fig. 19.13 2020 GOA SAFE) --------------------------
#Based on Cindy's code

#ADFG SEAK LL
#read in data
ADFGSEAKLL <- read.csv("../Data/ADFG_SEAK_LL2022.csv")

#Calculate annual CPUE
SEAK <- plyr::ddply(ADFGSEAKLL,c("Year"), 
              summarize,
              Tot_hks=sum(hooks_tot, na.rm = TRUE),
              ineff_hks=sum(hooks_inval, na.rm = TRUE),
              toteffhks=Tot_hks-ineff_hks,
              dogfish=sum(dogf_no, na.rm = TRUE),
              PSS=sum(PSS_no, na.rm = TRUE),
              CPUE_dog=dogfish/toteffhks,
              CPUE_PSS=PSS/toteffhks)
SEAKm <- reshape2::melt(SEAK[,c("Year","CPUE_dog","CPUE_PSS")], 
                        id.vars=c("Year"))

#Plot
ADFG_SEAK_doc <- ggplot(SEAKm[SEAKm$variable=="CPUE_dog",],
                        aes(x=Year,y=value,color=variable,shape=variable)) +
  geom_line(color="orange",size=1) +
  geom_point(color="orange",size=4) +
  geom_point(color="yellow",size=3) +
  labs(y="CPUE (#/efhks)",x="",title="ADFG SEAK Longline Survey") +
  scale_x_continuous(limits=c(1982,AYR))+
  theme_bw()+
  theme(plot.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y=element_text(colour='black'),
        axis.title.x=element_text(colour='black'),
        axis.text.y=element_text(colour='black',size=10),
        axis.text.x=element_text(colour='black',size=10),
        plot.title=element_text(size=rel(1), colour='black',hjust=0.5),
        axis.ticks=element_line(colour='black'),
        axis.line=element_line(size=1, color = "black"),
        axis.text=element_text(size=rel(1.2)),
        legend.position="none")


#PWS Trawl Bay survey
ADFG_LRGTWL <- read.csv("../Data/ADFG_LRGTWL2020.csv")

ADFG_Kshak_doc <- ggplot(ADFG_LRGTWL[ADFG_LRGTWL$year>1997 &
                                     ADFG_LRGTWL$species=="Spiny Dogfish" &
                                     ADFG_LRGTWL$proj=="T06",],
                       aes(x=year,y=Kg_CPUE,color=species,shape=species)) +
  geom_line(color="orange",size=1) +
  geom_point(color="orange",size=4) +
  geom_point(color="yellow",size=3) +
  labs(y="CPUE (kg/nmi)",x="",title="ADFG PWS Trawl Survey") +
  scale_x_continuous(limits=c(1982,AYR)) +
  theme_bw() +
  theme(plot.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y=element_text(colour='black'),
        axis.title.x=element_text(colour='black'),
        axis.text.y=element_text(colour='black',size=10),
        axis.text.x=element_text(colour='black',size=10),
        plot.title=element_text(size=rel(1), colour='black',hjust=0.5),
        axis.ticks=element_line(colour='black'),
        axis.line=element_line(size=1, color = "black"),
        axis.text=element_text(size=rel(1.2)),
        legend.position="none")


#GOA AFSC Trawl Survey
GOASD <- shark_biomass[shark_biomass$REGULATORY_AREA_NAME=="GOA" & shark_biomass$Group=="Spiny Dogfish",]

AFSC_TWLSD_doc <- ggplot(GOASD, aes(x=YEAR, y=Biomass/1000,group=Group,fill=Group)) + 
  geom_bar(stat="identity",color="orange",fill="yellow",size=1) +
  geom_errorbar(aes(ymax=UL/1000,ymin=LL/1000),colour="black",width=1) +
  labs(y="Biomass (1000s t)",x="",title="AFSC Trawl Survey") +
  coord_cartesian(y=c(0,300)) +
  scale_x_continuous(limits=c(1982,AYR)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_bw()+
  theme(plot.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y=element_text(colour='black'),
        axis.title.x=element_text(colour='black'),
        axis.text.y=element_text(colour='black',size=10),
        axis.text.x=element_text(colour='black',size=10),
        plot.title=element_text(size=rel(1), colour='black',hjust=0.5),
        axis.ticks=element_line(colour='black'),
        axis.line=element_line(size=1, color = "black"),
        axis.text=element_text(size=rel(1.2)),
        legend.position="none")

#AFSC Longline Survey RPNs
AFSCLL <- read.csv("../Data/AFSCLL_RPN_sharks2022.csv")

AFSCLL_doc <- ggplot(AFSCLL[AFSCLL$species_code==310 & AFSCLL$mgmt_area=="GOA",], aes(x=year, y=rpn/1000)) + 
  geom_bar(stat="identity",color="orange",fill="yellow",size=1)+
  labs(y="RPNs",x="",title="AFSC Longline Survey")+
  coord_cartesian(y=c(0,max(110)))+
  scale_x_continuous(limits=c(1982,AYR))+
  scale_y_continuous(expand=c(0,0))+
  theme_bw()+
  theme(plot.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y=element_text(colour='black'),
        axis.title.x=element_text(colour='black'),
        axis.text.y=element_text(colour='black',size=10),
        axis.text.x=element_text(colour='black',size=10),
        plot.title=element_text(size=rel(1), colour='black',hjust=0.5),
        axis.ticks=element_line(colour='black'),
        axis.line=element_line(size=1, color = "black"),
        axis.text=element_text(size=rel(1.2)),
        legend.position="none")

#IPHC RPNs
IPHC_dat <- read.csv("../Data/IPHC_RPN_sharks_2021.csv")

IPHC2 <- plyr::ddply(IPHC_dat,c("survey_year","fmp_sub_area","species"),summarize,
             totRPN=sum(strata_rpn),totLL=sum(boot_lci),totUL=sum(boot_uci))
colnames(IPHC2)<-c("Year","FMP","Species","RPN","LL","UL")

drops <- c("AI","BS")
IPHC_GOA <- IPHC2[IPHC2$FMP %nin% drops,]
IPHC_GOA2 <- plyr::ddply(IPHC_GOA,c("Year","Species"),summarize,FMPRPN=sum(RPN),FMPLL=sum(LL),FMPUL=sum(UL))

IPHC_dog_doc <- ggplot(IPHC_GOA2[IPHC_GOA2$Species=="Spiny dogfish",], aes(x=Year, y=FMPRPN/1000)) + 
  geom_bar(stat="identity",color="orange",fill="yellow",size=1) +
  geom_errorbar(aes(ymax=FMPUL/1000,ymin=FMPLL/1000),colour="black",width=1) +
  labs(y="RPNs (1000s)",x="",title="IPHC Longline Survey") +
  coord_cartesian(y=c(0,max(IPHC_GOA2[IPHC_GOA2$Species=="Spiny dogfish",]$FMPUL)/1000)) +
  scale_x_continuous(limits=c(1982,AYR)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_bw()+
  theme(plot.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y=element_text(colour='black'),
        axis.title.x=element_text(colour='black'),
        axis.text.y=element_text(colour='black',size=10),
        axis.text.x=element_text(colour='black',size=10),
        plot.title=element_text(size=rel(1), colour='black',hjust=0.5),
        axis.ticks=element_line(colour='black'),
        axis.line=element_line(size=1, color = "black"),
        axis.text=element_text(size=rel(1.2)),
        legend.position="none")

#Dogfish plot
Dog.combined.indices.doc <- grid.arrange(arrangeGrob(AFSC_TWLSD_doc,
                                                   ADFG_Kshak_doc,
                                                   IPHC_dog_doc, 
                                                   ADFG_SEAK_doc,
                                                   AFSCLL_doc, ncol = 2))
ggsave("../Figures/GOA_Survey_Dogfish_Combined.png", Dog.combined.indices.doc, 
       width = 7, height = 7, dpi=300)


# ..All surveys GOA Sleeper Sharks combo (Figure 19.14 in 2020 SAFE) --------

#GOA trawl survey
GOAPSS <- shark_biomass[shark_biomass$REGULATORY_AREA_NAME=="GOA" & 
                          shark_biomass$Group=="Pacific Sleeper Shark",]

AFSC_TWLPSS_doc <- ggplot(GOAPSS, aes(x=YEAR, y=Biomass/1000,group=Group,fill=Group)) + 
  geom_bar(stat="identity",color="dark green",fill="light green",size=1) +
  geom_errorbar(aes(ymax=UL/1000,ymin=LL/1000),colour="black",width=1) +
  labs(y="Biomass (1000s t)",x="",title="AFSC Trawl Survey") +
  coord_cartesian(y=c(0,160)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(limits=c(1982,AYR)) +
  theme_bw()+
  theme(plot.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y=element_text(colour='black'),
        axis.title.x=element_text(colour='black'),
        axis.text.y=element_text(colour='black',size=10),
        axis.text.x=element_text(colour='black',size=10),
        plot.title=element_text(size=rel(1), colour='black',hjust=0.5),
        axis.ticks=element_line(colour='black'),
        axis.line=element_line(size=1, color = "black"),
        axis.text=element_text(size=rel(1.2)),
        legend.position="none")


#ADFG SEAK LL
sl_SEAKLL_doc <- ggplot(SEAKm[SEAKm$variable=="CPUE_PSS",],aes(x=Year,y=value,color=variable,shape=variable)) +
  geom_line(color="dark green",size=1,show.legend = F) +
  geom_point(color="dark green",size=4,show.legend = F) +
  geom_point(color="light green",size=3,show.legend = F) +
  labs(y="CPUE (#/efhks)",x="",title="ADFG SEAK Longline Survey") +
  scale_x_continuous(limits=c(1982,AYR)) +
  theme_bw()+
  theme(plot.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y=element_text(colour='black'),
        axis.title.x=element_text(colour='black'),
        axis.text.y=element_text(colour='black',size=10),
        axis.text.x=element_text(colour='black',size=10),
        plot.title=element_text(size=rel(1), colour='black',hjust=0.5),
        axis.ticks=element_line(colour='black'),
        axis.line=element_line(size=1, color = "black"),
        axis.text=element_text(size=rel(1.2)),
        legend.position="none")

#IPHC RPNs
sl_IPHC_doc <- ggplot(IPHC_GOA2[IPHC_GOA2$Species=="Sleeper shark",], aes(x=Year, y=FMPRPN/1000)) + 
  geom_bar(stat="identity",color="dark green",fill="light green",size=1) +
  geom_errorbar(aes(ymax=FMPUL/1000,ymin=FMPLL/1000),colour="black",width=1) +
  labs(y="RPNs (1000s)",x="",title="IPHC Longline Survey") +
  coord_cartesian(y=c(0,max(IPHC_GOA2[IPHC_GOA2$Species=="Sleeper shark",]$FMPUL/1000))) +
  scale_x_continuous(limits=c(1982,AYR)) +
  scale_y_continuous(expand=c(0,0))+
  theme_bw()+
  theme(plot.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y=element_text(colour='black'),
        axis.title.x=element_text(colour='black'),
        axis.text.y=element_text(colour='black',size=10),
        axis.text.x=element_text(colour='black',size=10),
        plot.title=element_text(size=rel(1), colour='black',hjust=0.5),
        axis.ticks=element_line(colour='black'),
        axis.line=element_line(size=1, color = "black"),
        axis.text=element_text(size=rel(1.2)),
        legend.position="none")

sl_survindices_doc <- grid.arrange(arrangeGrob(AFSC_TWLPSS_doc, 
                                               sl_SEAKLL_doc, 
                                               sl_IPHC_doc, nrow = 2))

ggsave("../Figures/GOA_Survey_PSS_Combined.png", sl_survindices_doc, dpi=300)


# ..IPHC BSAI RPN (Figure 19.13 from 2020 SAFE) ---------------------------

BIPHC <- IPHC2[IPHC2$FMP=="BS"|IPHC2$FMP=="AI",]
BIPHC$Species <- factor(BIPHC$Species, levels=c("Sleeper shark","Spiny dogfish"))

BS_IPHC_doc <- ggplot(BIPHC[(BIPHC$FMP=="BS"),], aes(x=Year,y=RPN,group=Species,fill=Species)) + 
  geom_bar(data=BIPHC[(BIPHC$Species=="Sleeper shark" & BIPHC$FMP=="BS"),],
           aes(x=Year, y=RPN), stat="identity",color="dark green",fill="light green",size=1) +
  geom_linerange(data=BIPHC[(BIPHC$Species=="Sleeper shark" & BIPHC$FMP=="BS"),],
                 aes(ymax=UL,ymin=LL),colour="black",width=1)+
  geom_bar(data=BIPHC[(BIPHC$Species=="Spiny dogfish" & BIPHC$FMP=="BS"),],
           aes(x=Year, y=RPN), stat="identity",color="orange",fill="yellow",size=1) +
  geom_linerange(data=BIPHC[(BIPHC$Species=="Spiny dogfish"&BIPHC$FMP=="BS"),],
                 aes(ymax=UL,ymin=LL),colour="black",width=1)+
  facet_grid(Species~FMP,scale="free_y")+
  scale_y_continuous(expand=c(0,0), labels=scales::comma_format())+
  labs(y="",x="")+
  theme_pubr(border=TRUE, base_size = 24) +
  theme(panel.grid.major=element_line(color="grey90"),
        axis.text.x = element_text(angle = 45, hjust=1),
        strip.text=element_text(colour='black',face="bold"))


AI_IPHC_doc <- ggplot(BIPHC[(BIPHC$FMP=="AI"),], aes(x=Year,y=RPN,group=Species,fill=Species)) + 
  geom_bar(data=BIPHC[(BIPHC$Species=="Sleeper shark" & BIPHC$FMP=="AI"),],
           aes(x=Year, y=RPN), stat="identity",color="dark green",fill="light green",size=1) +
  geom_linerange(data=BIPHC[(BIPHC$Species=="Sleeper shark" & BIPHC$FMP=="AI"),],
                 aes(ymax=UL,ymin=LL),colour="black",width=1) +
  geom_bar(data=BIPHC[(BIPHC$Species=="Spiny dogfish" & BIPHC$FMP=="AI"),],
           aes(x=Year, y=RPN), stat="identity",color="orange",fill="yellow",size=1) +
  geom_linerange(data=BIPHC[(BIPHC$Species=="Spiny dogfish" & BIPHC$FMP=="AI"),],
                 aes(ymax=UL,ymin=LL),colour="black",width=1) +
  facet_grid(Species~FMP,scale="free_y") +
  scale_y_continuous(expand=c(0,0), labels=scales::comma_format()) +
  labs(y="",x="") +
  theme_pubr(border=TRUE, base_size = 24) +
  theme(panel.grid.major=element_line(color="grey90"),
        axis.text.x = element_text(angle = 45, hjust=1),
        strip.text.y = element_blank(),
        strip.background.y = element_blank())


IPHC_RPN_doc <- grid.arrange(arrangeGrob(AI_IPHC_doc, BS_IPHC_doc, nrow = 1,
                                       left = textGrob("Relative Population Numbers", rot = 90, vjust = 1.5,
                                                       gp=gpar(col="black", fontsize=20)),
                                       bottom = textGrob("Year", vjust=0,gp=gpar(col="black", fontsize=20))))

ggsave("../Figures/BSAI_IPHC_RPN.png",plot=IPHC_RPN_doc,dpi=600,width=15,height=15)



# ..IPHC CPUE Coast-wide (Fig 19.15 in 2020 GOA SAFE)---------------------------------------------------

sharkCPUE <- read.csv("../Data/IPHC_CPUE_sharks_2021.csv")

sC2 <- sharkCPUE[sharkCPUE$area_combo=="FMP (without Inside waters)",]
sC2$area <- factor(sC2$area,c("BSAI","GOA","CAN","WC"))

PSS_CPUE_doc <- ggplot(sC2[sC2$species=="Sleeper shark",],
                     aes(x=survey_year,y=area_cpue,color=area,fill=area)) +
  geom_point(show.legend = F) +
  geom_line(show.legend = F) +
  geom_errorbar(aes(ymax=boot_lci,ymin=boot_uci), width=0.5, show.legend = F) +
  labs(title=sC2[sC2$species=="Sleeper shark",]$species[1],x="",y="") +
  facet_grid(area~.,scales="free_y") +
  theme_pubr(border=TRUE, base_size = 12) +
  theme(panel.grid.major=element_line(color="grey90"),
        axis.text.x = element_text(angle = 45, hjust=1),
        strip.text.y = element_blank(),
        strip.background.y = element_blank())

SD_CPUE_doc <- ggplot(sC2[sC2$species=="Spiny dogfish",],
                    aes(x=survey_year,y=area_cpue,color=area,fill=area)) +
  geom_point(show.legend = F) +
  geom_line(show.legend = F) +
  geom_errorbar(aes(ymax=boot_lci,ymin=boot_uci), width=0.5, show.legend = F) +
  labs(title=sC2[sC2$species=="Spiny dogfish",]$species[1],x="",y="") +
  facet_grid(area~.,scales="free_y") +
  theme_pubr(border=TRUE, base_size = 12) +
  theme(panel.grid.major=element_line(color="grey90"),
        axis.text.x = element_text(angle = 45, hjust=1),
        strip.text=element_text(colour='black',face="bold"))

IPHC_CPUE_doc <- grid.arrange(arrangeGrob(PSS_CPUE_doc, SD_CPUE_doc, nrow = 1,
                                        left = textGrob("CPUE (#/effhks)", rot = 90, vjust = 1.5,
                                                        gp=gpar(col="black", fontsize=12)),
                                        bottom = textGrob("Year", vjust=0,gp=gpar(col="black", fontsize=12))))

ggsave("../Figures/IPHC_CPUE_Coastwide.png", IPHC_CPUE_doc, dpi=600)


# ..Dogfish lengths GOA (Fig 19.8 and 19.9 in 2020 GOA SAFE)-------------------------------------------------------
#Joined figures for males and females to be more like coastwide figure

survey.L <- read.csv("../Data/SURVEY_shark_L_dat2022.csv")

SD.GOA <- survey.L %>% 
  filter(Species == 310 & FMP == "GOA")

#there is one erroneous male dogfish that can't be real, take it out
SD.GOA <- SD.GOA[SD.GOA$Length !=133,] #this is adding two rows of NAs, I don't know why
SD.GOA <- na.omit(SD.GOA)

SD.GOA.hist.F <- ggplot(SD.GOA[SD.GOA$Sex=="F",], aes(x=Length, y=factor(Year))) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2, fill="#c2174a") +
  facet_wrap(~Source, ncol=3) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0), limits=rev) +
  coord_cartesian(clip = "off") +
  labs(x="Length (cm)") +
  theme_ridges(center_axis_labels = TRUE, font_size = 12) +
  theme(axis.title = element_blank(),
        legend.position = "none")

SD.GOA.hist.M <- ggplot(SD.GOA[SD.GOA$Sex=="M",], aes(x=Length, y=factor(Year))) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2, fill="#1767c2") +
  facet_wrap(~Source, ncol=3) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0), limits=rev) +
  coord_cartesian(clip = "off") +
  labs(x="Length (cm)") +
  theme_ridges(center_axis_labels = TRUE, font_size = 12) +
  theme(axis.title = element_blank(),
        legend.position = "none")

SD.GOA.boxplot.F <- ggplot(SD.GOA[SD.GOA$Sex=="F",], aes(x=Source, y=Length, fill=Source)) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  labs(x="", y="Length (cm)") +
  theme_pubr(legend="none")

SD.GOA.boxplot.M <- ggplot(SD.GOA[SD.GOA$Sex=="M",], aes(x=Source, y=Length, fill=Source)) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  labs(x="", y="Length (cm)") +
  theme_pubr(legend="none")

Dogfish.length.GOA <-
  (SD.GOA.hist.F | (SD.GOA.hist.M)) / (SD.GOA.boxplot.F | SD.GOA.boxplot.M) +
  plot_layout(heights = c(6,1))

ggsave("../Figures/Dogfish_lengths_GOA.png", Dogfish.length.GOA, 
       dpi=600, width=12, height=7)



# ..Dogfish lengths IPHC (coastwide) (Fig 19.10 in 2020 GOA SAFE)--------------------------------------------

Idat <- read.csv("../Data/IPHC_dogfish_lengths2022.csv")

Idat$fmp <- factor(Idat$fmp, levels=c("BSAI","GOA","CAN","WC"))

#there is one erroneous male dogfish that can't be real, take it out
Idat <- Idat[Idat$length !=133,] #this is adding two rows of NAs, I don't know why
Idat <- na.omit(Idat)

DF.F.length.hist <-
  ggplot(Idat[Idat$sex=="F",], aes(x=length, fill=fmp)) + 
  facet_grid(year~fmp) +
  geom_histogram(binwidth = 5, color="black") +
  labs(title="Female", x="Length (cm)", y="Number of fish\n") +
  theme_pubr(border = TRUE, legend = "none") +
  theme(panel.grid.major=element_line(color="grey90"),
        strip.text=element_text(size=12,colour='black',face="bold"),
        axis.title.x = element_blank())

DF.F.length.boxplot <-
  ggplot(Idat[Idat$sex=="F",], aes(x=fmp, y=length, fill=fmp)) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  labs(x="", y="Length (cm)") +
  theme_pubr(border=TRUE, legend="none")

DF.M.length.hist <-
  ggplot(Idat[Idat$sex=="M",], aes(x=length, fill=fmp)) + 
  facet_grid(year~fmp) +
  geom_histogram(binwidth = 5, color="black") +
  labs(title="Male", x="Length (cm)", y="Number of fish\n") +
  theme_pubr(border = TRUE, legend = "none") +
  theme(panel.grid.major=element_line(color="grey90"),
        strip.text=element_text(size=12,colour='black',face="bold"),
        axis.title = element_blank())

DF.M.length.boxplot <-
  ggplot(Idat[Idat$sex=="M",], aes(x=fmp, y=length, fill=fmp)) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  labs(x="", y="Length (cm)") +
  theme_pubr(border=TRUE, legend="none")

Dogfish.length.coastwide <-
  (DF.F.length.hist | (DF.M.length.hist)) / (DF.F.length.boxplot | DF.M.length.boxplot) +
  plot_layout(heights = c(6,1))

ggsave("../Figures/Dogfish_lengths_coastwide.png", Dogfish.length.coastwide, 
       dpi=600, width=12, height=7)

#Beth alt plots to coast-wide dogfish figure

DF.F.length.hist <- 
  ggplot(Idat[Idat$sex=="F",], aes(x=length, y=factor(year), fill=fmp)) + 
  geom_density_ridges(jittered_points = TRUE,
                      position = position_points_jitter(width = 0.05, height = 0),
                      point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.5) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2) +
  facet_wrap(~fmp, ncol=4) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(x="Length (cm)") +
  theme_ridges(center_axis_labels = TRUE, font_size = 12) +
  theme(axis.title.y = element_blank(),
        legend.position = "none")

ggplot(Idat[Idat$sex=="F",], aes(x=factor(year), y=length)) +
  geom_jitter(alpha = 0.3, aes(color=fmp), width=0.3) +
  geom_boxplot(outlier.shape = NA, fill=NA, size=1) +
  facet_grid(~fmp) +
  labs(x="Year",y="Length (cm)") +
  theme_pubr(border=TRUE) +
  theme(legend.position="none",
        panel.grid.major=element_line(color="grey90"),
        strip.text=element_text(size=12,colour='black',face="bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


# ..Sleeper shark lengths coast-wide (Figure 19.11 from 2020 SAFE --------
#used same data as in stock structure doc

#Load and prep data
#same as size summary datafile but includes FMP (note that WC records are listed as GOA)
LW.survey <- read.csv("../Data/PSS_size_joins.csv")
#remove outliers
#LW.survey <- LW.survey[-c(3,599),]
LW.survey <- LW.survey[LW.survey$TLcm != 0 & LW.survey$Weight !=27.398,]

#fix coding for West Coast
LW.survey$FMP <- ifelse(LW.survey$NMFS_Sub_A == "WC", "WC", LW.survey$FMP)

#Read in additional file from NORPAC (Cindy sent 7/29/22 and updated 8/16/22)
#Coordinates have been converted to non-confidential points
LW.NORPAC <- read.csv("../Data/NCF_Obj1_obsSPdat_updated081622.csv")

LW.NORPAC <- LW.NORPAC %>% 
  select(year, haul_date, sex_obs, wt, pcl, ncf_lat, ncf_lon, nmfs_area) %>% 
  rename("Year" = "year",
         "Sex" = "sex_obs",
         "Weight" = "wt",
         "PCL" = "pcl",
         "LAT" = "ncf_lat",
         "LONG" = "ncf_lon",
         "NMFS_AREA" = "nmfs_area") %>% 
  filter(PCL > 0) %>% 
  mutate(TLcm = (17.78 + 1.1*PCL),
         FMP = ifelse(NMFS_AREA < 600, "BSAI",
                      ifelse(between(NMFS_AREA, 610, 659), "GOA", 
                             "OTHER")))

#merge with survey data
LW.all <- full_join(LW.NORPAC, LW.survey)
table(LW.all$FMP)
table(LW.all$NMFS_AREA)

#Blank FMP are from GOA Hulbert
LW.all <- LW.all %>% 
  mutate(NMFS_Sub_A = ifelse(NMFS_AREA < 541, "BS",
                             ifelse(between(NMFS_AREA, 541, 543), "AI",
                                    ifelse(between(NMFS_AREA, 610, 659)|NMFS_AREA=="", "GOA",
                                           "WC"))))
table(LW.all$NMFS_Sub_A)
#No canada?
#Pull in Canadian data (From 2020 assessment; didn't use rest of data for fear of duplication)
SAFE2020 <- read.csv("../Data/Sleeper_lengths_coast2020.csv")
SAFE2020.BC <- SAFE2020 %>% 
  filter(FMP == "BC") %>% 
  mutate(NMFS_Sub_A = "BC")

#merge with rest
LW.all$Sex <- as.character(LW.all$Sex)
LW.all <- full_join(LW.all, SAFE2020.BC)

LW.all <- LW.all[!is.na(LW.all$NMFS_Sub_A),]

LW.all$NMFS_Sub_A <- factor(LW.all$NMFS_Sub_A, 
                            levels = c("BS", "AI", "GOA", "BC", "WC"))

PSS.length.coastwide <- ggplot(LW.all,aes(x=factor(Year), y=TLcm)) +
  geom_jitter(alpha = 0.5, aes(color=NMFS_Sub_A), width=0.1) +
  geom_boxplot(outlier.shape = NA, alpha=0) +
  facet_grid(~NMFS_Sub_A) +
  labs(x="Year",y="Total Length (cm)\n") +
  scale_x_discrete(breaks = seq(1985,AYR, 5)) +
  theme_pubr(legend = "none", border = TRUE) +
  theme(panel.grid.major=element_line(color="grey90"),
        panel.spacing.x = unit(0,"line"),
        strip.text=element_text(size=12,colour='black',face="bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave("../Figures/PSS_lengths_coastwide.png",
       plot=PSS.length.coastwide, dpi=600, width = 11, height = 7)



# MAPS --------------------------------------------------------------------
#..set up map---------------------------------------------------------------------------------------------------------
#get land
world <- ne_countries(scale = "medium", returnclass = "sp")

#usa
usa <- subset(world, admin == "United States of America")
usa <- fortify(usa)
usa$long <- ifelse(usa$long > 0, usa$long - 360, usa$long)
#russia
russia <- subset(world, admin == "Russia")
russia <- fortify(russia)
russia$long = ifelse(russia$long > 0, russia$long - 360, russia$long)
#canada
canada <- subset(world, admin == "Canada")
canada <- fortify(canada)

#bathymetry (from downloaded OFIS bathymetry shapefile)
race_bathy = readOGR("../Data/race_bathy_to_200_NAD1983_HARN")
race_bathy_df = fortify(race_bathy)
race_bathy_df$long = ifelse(race_bathy_df$long > 0, race_bathy_df$long - 360, race_bathy_df$long)


# ..AFSC bottom trawl -----------------------------------------------------

RACE.bt <- read.csv("../Data/RACE_CATCH2021.csv", 
                    check.names=FALSE, skip = 6)

names(RACE.bt) <- gsub(" ", "_", names(RACE.bt))

RACE.bt$fYear<-factor(RACE.bt$Year)

#rename columns
RACE.bt <- RACE.bt %>% 
  rename(Lat = "Starting_Latitude_(dd)",
         Long = "Starting_Longitude_(dd)",
         Weight_kg = "Weight_(kg)")

#adjust data points for 180 line
RACE.bt$Long <- ifelse(RACE.bt$Long > 0, RACE.bt$Long - 360, 
                                        RACE.bt$Long)

#set FMP areas and divide into BSAI and GOA FMP areas
RACE.bt <- RACE.bt %>%
  mutate(FMP_area =
           ifelse(Survey %in% c("GOA"), "GOA",
                  ifelse(Survey %in% c("AI", "EBS_SLOPE", "EBS_SHELF"), "BSAI", "other")))

RACE.SD <- RACE.bt %>% 
  filter(Species_Code == 310)


RACE.SD.map <- ggplot() +
  facet_wrap(~Year) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = russia, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_point(data = RACE.SD[RACE.SD$Survey == "GOA" & RACE.SD$Year>=1999,], 
             aes(x = Long, y = Lat, color=Number_of_Fish),
             shape = 15, alpha=0.8, size=1) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, 
                        guide="colourbar", label = scales::comma) +
  labs(color = "Number of fish") +
  coord_sf(crs = st_crs(4326), xlim = c(-186,-130), ylim = c(50, 65), expand = FALSE) +
  theme_bw() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=10), #change legend text font size
        legend.box.margin = margin(-10,-10,0,-10),
        legend.box.spacing = unit(0,'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        plot.margin = margin(-0.5,-0.5,-0.5,-0.5, "pt"))

ggsave("../Figures/GOA_RACE_SD_number_per_year.png", plot=RACE.SD.map, width = 11, height = 7, dpi=600)


# ..IPHC ------------------------------------------------------------------

IPHC.map <- read.csv("../Data/IPHC_FISS_survey_2021_trackoff.csv")

#filter out stations with zero catch and only show GOA and BSAI
IPHC.map <- IPHC.map %>% 
  filter(obs_catch > 0) %>% 
  filter(fmp_area == "GOA" | fmp_area == "BSAI")

#adjust data points for 180 line
IPHC.map$start_lon = ifelse(IPHC.map$start_lon > 0, IPHC.map$start_lon - 360, IPHC.map$start_lon)

IPHC.SD <- IPHC.map %>% 
  filter(species_scientific_name == "Squalus acanthias" | species_scientific_name == "Squalus suckleyi")
IPHC.PSS <- IPHC.map %>% 
  filter(species_scientific_name == "Somniosus pacificus")


IPHC.SD.map <- 
  ggplot() +
  facet_wrap(~survey_year) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = russia, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_point(data = IPHC.SD, 
             aes(x = start_lon, y = start_lat, color=obs_catch),
             shape = 15, alpha=0.8, size=1) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, 
                        guide="colourbar", label = scales::comma) +
  labs(color = "Number of fish") +
  coord_sf(crs = st_crs(4326), xlim = c(-186,-130), ylim = c(50, 65), expand = FALSE) +
  theme_bw() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=10), #change legend text font size
        legend.box.margin = margin(-10,-10,0,-10),
        legend.box.spacing = unit(0,'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        plot.margin = margin(-0.5,-0.5,-0.5,-0.5, "pt"))

ggsave("../Figures/IPHC_SD_number_per_year.png", plot=IPHC.SD.map, width = 11, height = 7, dpi=600)


IPHC.PSS.map <- 
  ggplot() +
  facet_wrap(~survey_year) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = russia, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_point(data = IPHC.PSS, 
             aes(x = start_lon, y = start_lat, color=obs_catch),
             shape = 15, alpha=0.8, size=1) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, 
                        guide="colourbar", label = scales::comma) +
  labs(color = "Number of fish") +
  coord_sf(crs = st_crs(4326), xlim = c(-186,-130), ylim = c(50, 65), expand = FALSE) +
  theme_bw() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=10), #change legend text font size
        legend.box.margin = margin(-10,-10,0,-10),
        legend.box.spacing = unit(0,'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        plot.margin = margin(-0.5,-0.5,-0.5,-0.5, "pt"))

ggsave("../Figures/IPHC_PSS_number_per_year.png", plot=IPHC.PSS.map, width = 11, height = 7, dpi=600)



#..AFSC Longline -----------------------------------------------------------

AFSC.LL <- read.csv("../Data/AFSCLL_sharkcatch_2022trackoff.csv")

#remove zero catch values
AFSC.LL <- filter(AFSC.LL, catch_freq > 0)

#adjust data points for 180 line
AFSC.LL$start_longitude = ifelse(AFSC.LL$start_longitude > 0, AFSC.LL$start_longitude - 360, AFSC.LL$start_longitude)

#set FMP areas and divide into BSAI and GOA FMP areas
AFSC.LL <- AFSC.LL %>%
  mutate(FMP_area =
           ifelse(nmfs_area_code %in% c(610,620,630,640,649,650,659), "GOA",
                  ifelse(nmfs_area_code %in% c(508,509,512,513,514,516,517,518,519,521,523,524,530,541,542,543,550), "BSAI", "other")))
AFSC.LL$FMP_area<-factor(AFSC.LL$FMP_area)

AFSC.LL.SD <- AFSC.LL %>% 
  filter(species_code == 310)

AFSC.LL.SD.map <-
ggplot() +
  facet_wrap(~year) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = russia, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_point(data = AFSC.LL.SD[AFSC.LL.SD$year>=1991,], 
             aes(x = start_longitude, y = start_latitude, color=catch_freq),
             shape = 15, alpha=0.8, size=1) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, 
                        guide="colourbar", label = scales::comma) +
  labs(color = "Number of fish") +
  coord_sf(crs = st_crs(4326), xlim = c(-186,-130), ylim = c(50, 65), expand = FALSE) +
  theme_bw() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=10), #change legend text font size
        legend.box.margin = margin(-10,-10,0,-10),
        legend.box.spacing = unit(0,'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        plot.margin = margin(-0.5,-0.5,-0.5,-0.5, "pt"))

ggsave("../Figures/ABL_LL_SD_number_per_year.png", plot=AFSC.LL.SD.map, width = 11, height = 7, dpi=600)



#..ADFG --------------------------------------------------------------------

ADFG <- read.csv("../Data/ADFG_SEAK_LL2022.csv")

ADFG$fYear<-factor(ADFG$Year)

#rearrange species from columns to rows
ADFG <- ADFG %>% 
  gather(Common_Name, Number_of_Fish, dogf_no:PSS_no)
ADFG$Common_Name <- factor(ADFG$Common_Name)
levels(ADFG$Common_Name) <- c("spiny dogfish", "Pacific sleeper shark")

#remove zero catch values
ADFG <- filter(ADFG, Number_of_Fish > 0)

#adjust data points for 180 line
ADFG$Long = ifelse(ADFG$Long > 0, ADFG$Long - 360, ADFG$Long)


ADFG.SD <- 
  ggplot() +
  facet_wrap(~Year) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = russia, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_point(data = ADFG[ADFG$Common_Name == "spiny dogfish",], 
             aes(x = Long, y = Lat, color=Number_of_Fish),
             shape = 16, alpha=0.9, size=1) +
  scale_color_viridis_c(option="plasma", space="Lab", 
                        begin = 0.8, end = 0, guide="colourbar") +
  scale_x_continuous(breaks = seq(-136,-131, by = 2)) +
  scale_y_continuous(breaks = seq(54,58, by = 1)) +
  labs(color = "Number of sharks") +
  coord_sf(crs = st_crs(4274), xlim = c(-136,-131), ylim = c(54, 58.2), expand = FALSE) +
  theme_bw() +
  theme(legend.position = "right",
        axis.title = element_blank(),
        axis.text = element_text(),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0.2, "lines"),
        panel.grid = element_blank())

ggsave("../Figures/SD_ADFG_SEAK_LL.png", plot=ADFG.SD, width = 11, height = 7, dpi=600)

ADFG.PSS <- 
  ggplot() +
  facet_wrap(~Year) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = russia, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_point(data = ADFG[ADFG$Common_Name == "Pacific sleeper shark",], 
             aes(x = Long, y = Lat, color=Number_of_Fish),
             shape = 16, alpha=0.9, size=1) +
  scale_color_viridis_c(option="plasma", space="Lab", 
                        begin = 0.8, end = 0, guide="colourbar",
                        breaks = seq(0,10,2)) +
  scale_x_continuous(breaks = seq(-136,-131, by = 2)) +
  scale_y_continuous(breaks = seq(54,58, by = 1)) +
  labs(color = "Number of sharks") +
  coord_sf(crs = st_crs(4274), xlim = c(-136,-131), ylim = c(54, 58.2), expand = FALSE) +
  theme_bw() +
  theme(legend.position = "right",
        axis.title = element_blank(),
        axis.text = element_text(),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0.2, "lines"),
        panel.grid = element_blank())

ggsave("../Figures/PSS_ADFG_SEAK_LL.png", plot=ADFG.PSS, width = 11, height = 7, dpi=600)


