library(reshape2)
library(ggpubr)
library(dplyr)
library(ggpmisc)
library(ggrepel)
library(ggtern)
library(ggExtra)
library(devtools)
library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(ggplot2)

#fig1
library(maps)
library(RColorBrewer)

#setwd("D:/PycharmProjects/gender改/data/extended data")
setwd("D:/PycharmProjects/gender改/data")
world_map <- map_data("world")
#d <- data.frame(country=unique(world_map$region))
#write.csv(d, file = "country.csv")
#print(world_map)
data <- read.csv('fig1_minmax_total.csv',header = TRUE)
#data <- read.csv('fig1_gender_check_total.csv',header = TRUE)
unique(data$Country.Name)
world_map %>% 
  inner_join(data, by = c("region" = "Country.Name")) -> rca_world_map
print(rca_world_map)
unique(rca_world_map$region)
#unique(world_map$region)
#(unique(world_map$region),"country.csv",row.names=FALSE,col.names=TRUE,sep=",")
#分5个领域子图，每个子图分面两个图male和female
l <- list("Arts & Humanities","Life Sciences & Biomedicine","Physical Sciences",
          "Social Sciences","Technology")

#pal<-c('#CB5C5E','#E49467','#EECF91','#F7F7C6','#D2E4A6','#90C6A6','#4D8EA8')
pal<-c('#1E469B','#2681B6','#35B9C5','#96D2B0','#F9F8CA') #黄蓝配色
#pal<-c('#982C2C','#C74647','#F8984F','#F3D78A','#F2F1E6')
pal<-c('#6F62A8','#9A87C1','#C5AFD6','#FFE7E3','#F3866E') #红紫配色
mycolor<-colorRampPalette(pal)
image(x=1:6,y=1,z=as.matrix(1:6),col=mycolor(6))

pal2<-c('#427AB4','#9CD5E4','#F2F6C0','#F37454','#D83B3A')
mycolor2<-colorRampPalette(pal2)
image(x=1:6,y=1,z=as.matrix(1:6),col=mycolor2(6))

pic <- function(i){
  data = rca_world_map[rca_world_map$dis==as.character(i),]
  p <- ggplot(data, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill=minmax), colour = "white") +
    scale_fill_gradientn(colors = mycolor(20))+
    facet_wrap(~gender,labeller = as_labeller(c('female'='Female','male'='Male')))+
    theme_minimal() +
    ggtitle(l[i])+
    labs(fill = 'Normalized(RCA)')+
    theme(legend.position = "bottom",
          legend.title = element_text(size=14),
          legend.key.size = unit(25,'pt'),
          legend.text=element_text(size=12),
          plot.title = element_text(size = 15, hjust = 0.5),
          strip.text = element_text(size = 14),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid = element_blank())
  p
}
plot <- ggarrange(pic(1),pic(2),pic(3),pic(4),pic(5),nrow = 5,
          common.legend = T, legend = "bottom")
plot
#ggsave('fig1.png', plot, width = 10, height = 12, dpi = 500)
#ggsave('fig1_gender_check.png', plot, width = 10, height = 12, dpi = 500)

#雷达图
setwd("D:/PycharmProjects/gender改/data")
data <- read.csv('fig1_minmax.csv',header = TRUE)
data <- data %>% select(gender,Region,minmax,dis)
data <- unique(data)
#dis 列转行
#data <- data %>%
  #mutate(dis = recode(dis,"1"="x1","2"="x2","3"="x3",
                      #"4"="x4","5"="x5"))
data <- data %>%
  mutate(dis = recode(dis,"1"="Arts &\nHumanities","2"="Life Sciences &\nBiomedicine","3"="Physical\nSciences",
                      "4"="Social\nSciences","5"="Technology"))
data <- dcast(data,gender+Region~dis,value.var = 'minmax')
data_female <- data %>% filter(gender=='female')
data_female <- subset(data_female,select = -gender)
#rownames(data_female) <- data_female$Region
#data_female$Region <- NULL
library("ggradar")
#pal2<-c('#427AB4','#9CD5E4','#eda300','#F37454','#D83B3A')
#pal2<-c('#000000','#AFAFAF','#929292','#DBDBDB','#737373')
#mycolor2<-colorRampPalette(pal2)
#mycolors <- c('#C5AFD6','#808080','#800000','#EED5B7','#6F62A8','#F9C00F','#FEB4A9')
r <- data_female %>% ggradar(
    #data_female[i,], 
    values.radar = c("", "", ""),
    grid.min = 0, grid.mid = 0.5, grid.max = 1,
    # Polygons
    group.line.width = 1, 
    group.point.size = 3,
    #group.colours = mycolor2(7)[i],
    #group.colours = mycolors[i],
    group.colours = '#424242',
    # Background and grid lines
    background.circle.colour = "white",
    gridline.mid.colour = "grey",
    axis.labels = c("A&H","LSB","PHS","SOS","TEC"),
    axis.label.size = 5,
    plot.legend = TRUE,
    legend.position = "none",
    base.size = 0.5,
    legend.text.size = 13,
    fill = TRUE,
    fill.alpha = 0.25
  )+
  facet_wrap(~Region,nrow = 2)+
  theme(strip.background = element_rect(color = "white", fill = "white"),
        strip.text = element_text(size = 13),
        panel.spacing.x = unit(1.5,'cm'))

#柱状图
df <- read.csv('fig1_minmax.csv',header = TRUE)
df <- df %>% select(gender,Region,minmax,dis)
df <- unique(df)
df <- df %>% filter(gender=='female')
df$gender <- NULL
df_sum <- df %>% group_by(Region)%>%
  summarise(sum = sum(minmax))
df_sd <- df %>% group_by(Region)%>%
  summarise(sd = sd(minmax))
df_ <- merge(df_sum,df_sd)
df_ <- melt(df_,id.vars="Region")
df_ <- df_ %>% mutate(Region = recode(Region,"East Asia & Pacific"="EAP","Europe & Central Asia"="ECA",
                                      "Latin America & Caribbean"="LAC","Middle East & North Africa"="MEA",
                                      "North America"="NAC","South Asia"="SAS",
                                      "Sub-Saharan Africa"="SSA"))
z <- ggplot(df_,aes(x = Region, y = ifelse(variable == "sd", value, -value/10), fill = variable)) +
  geom_col(alpha = 0.6,width = 0.8)+
  theme_minimal()+
  theme(#axis.text.y = element_blank(),
        #axis.ticks=element_blank(),
        legend.position = 'none',
        panel.grid.major=element_line(colour=NA),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=13,angle = 45, hjust = 1),
        axis.text.y = element_text(size = 11))+
  ylab(NULL)+xlab(NULL)+
  scale_x_discrete(limits=as.character(c('ECA','NAC','EAP','MEA','SAS','LAC','SSA')))+
  scale_fill_manual(values = c('#231F20','#AEAFB3'))+
  annotate(geom = "text", x = 0.9, y = 0.38, 
    label = "sd", hjust = 0, vjust = 1, size = 4)+
  annotate(geom = "text", x = 0.8, y = -0.4, 
    label = "sum/10", hjust = 0, vjust = 1, size = 4)
  

x = c(0,500)
y = c(0,1)
empty_data <- data.frame(X=x,Y=y)
empty_plot <- ggplot(empty_data,aes(x, y))+
  geom_blank()+
  theme_void()
empty_plot

rp <- r+inset_element(z,left = 0.75, bottom = 0, right = 1, top = 0.55)

q <- ggarrange(plot,empty_plot,rp,labels = c('a','','b'),
               font.label = list(size = 18, face = "bold"),
               ncol = 1,heights = c(0.95,0.015,0.27))
#ggsave('fig1b.png', r, width = 10, height = 5, dpi = 500)
ggsave('fig1ab.png', q, width = 12, height = 20, dpi = 500)



#fig2
setwd("D:/PycharmProjects/gender改/data/picture")
setwd("D:/PycharmProjects/gender改/data/extended data")
data5 <- read.csv('fig2 question.csv',header = TRUE)
#data5 <- read.csv('fig2_gender_check.csv',header = TRUE)
data5_copy <- data5 %>%
  mutate(dis = recode(dis,"1"="Arts &\nHumanities","2"="Life Sciences &\nBiomedicine","3"="Physical\nSciences",
                      "4"="Social\nSciences","5"="Technology")) %>%
  mutate(region = recode(region,"East Asia & Pacific"="East Asia &\nPacifica","Europe & Central Asia"="Europe &\nCentral Asia",
                         "Middle East & North Africa"="Middle East &\nNorth Africa","North America"="North America",
                         "South Asia"="South Asia","Latin America & Caribbean"="Latin America &\nCaribbean",
                         "Sub-Saharan Africa"="Sub-Saharan\nAfrica"))


poly_or_mean <- function(formula, data, ...) {
  fm <- lm(formula = formula, data = data, ...)
  if (anova(fm)[["Pr(>F)"]][1] > 0.9) {
    lm(formula = y ~ 1, data = data, ...)
  } else {
    fm
  }
}

reg_plot <- ggplot(data5_copy,aes(x=region_pro,y=fem))+
  #geom_point(color='#DBDBDB',size=1)+
  stat_smooth(method = "lm",colour="#3188a2",level = 0.99,fullrange = TRUE)+
  stat_correlation(mapping = use_label("p"), size=2.5,small.p = TRUE) +
  stat_poly_eq(method = poly_or_mean,mapping = use_label("eq"), label.x = "right", label.y = "bottom", size = 2.5,
               formula = y~x)+
  facet_grid(dis ~ region,scales = "free")+
  theme_minimal()+
  theme(
    axis.ticks = element_blank(), 
    axis.text = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey50", fill = NA, size = 1),
    strip.background = element_blank(),
    strip.text = element_text(size = 11),
    axis.title = element_text(size = 11)
  )+
  labs(x= 'Topic popularity', y = 'Feminization')
reg_plot

ggsave('fig2 question.png', reg_plot, width = 9, height = 7, dpi = 500)
#ggsave('fig2 question point.png', reg_plot, width = 9, height = 7, dpi = 500)
#ggsave('fig2_gender_check.png', reg_plot, width = 9, height = 7, dpi = 500)


#fig3
data6 <- read.csv('fig3 question tech.csv',header = TRUE)
topic_list <- c('COVID-19', 'Molecular-Biology', 'Molecular-Biological-Techniques', 'PCR2', 'Machine-Learning', 'Primer', 'Plasmids', 'Western-Blot', 'Running', 'Extracts', 'Statistical-Analysis', 'Real-Time-PCR2', 'Staining', 'Dataset', 'Microbiology', 'Flow-Cytometry', 'Soil-Analysis', 'Methods', 'Proteins', 'Bioinformatics-and-Computational-Biology')

data6_label <- data6 %>%
  mutate(topic = ifelse(topic %in% topic_list, topic, "")) %>%
  filter(topic != "")
data6_label

#辅助线
data6_ave <- data6 %>% filter(tech != '2')
#detach('package:plyr')
data6_ave <- data6_ave %>% group_by(region,tech)%>%
  summarise(ave_fem = mean(fem),ave_pro = mean(region_pro))
data6_total_ave <- data6 %>% filter(tech != '2') %>% group_by(region) %>%
  summarise(ave_fem = mean(fem),ave_pro = mean(region_pro))
data6_total_ave['tech'] <- 3
data6_total_ave <- rbind(data6_ave,data6_total_ave)
data6_total_ave$region[data6_total_ave$region=='East Asia & Pacific'] <- 'EAP'
data6_total_ave$region[data6_total_ave$region=='Europe & Central Asia'] <- 'ECA'
data6_total_ave$region[data6_total_ave$region=='Latin America & Caribbean'] <- 'LAC'
data6_total_ave$region[data6_total_ave$region=='Middle East & North Africa'] <- 'MEA'
data6_total_ave$region[data6_total_ave$region=='North America'] <- 'NAC'
data6_total_ave$region[data6_total_ave$region=='South Asia'] <- 'SAS'
data6_total_ave$region[data6_total_ave$region=='Sub-Saharan Africa'] <- 'SSA'

point_plot <- ggplot(data = data6_total_ave,aes(x=ave_pro,y=ave_fem,
                                          color=factor(tech)))+
  geom_point(size=5)+
  #scale_shape_manual(values = c(1:7))+
  theme_minimal()+
  theme(#text = element_text(size=18),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey50", fill = NA, size = 1), 
    legend.direction = 'horizontal',
    legend.position = c(0.5,0.85),
    legend.title=element_text(size=13),
    axis.text.y = element_blank(),
    axis.text = element_text(size = 12),
    legend.background = element_rect(fill = 'white',color='grey50'))+
  scale_x_continuous(labels = scales::percent)+
  #scale_y_continuous(labels = scales::percent)+
  scale_y_continuous(limits = c(0.1, 0.5))+
  xlab(NULL)+ylab(NULL)+
  scale_color_manual(values=c("#DFC286","#80AFBF","#608595"),
                    labels=c('Yes','No','All'))+
    geom_text_repel(data=data6_total_ave,aes(label=region),color = 'black', size=3, 
                    max.overlaps= 20)+
  labs(color='Technique')

#ggsave('fig3 p.png', point_plot, width = 4.8, height = 2.1, dpi = 500)

a_plot <- ggplot(data=data6,aes(x=region_pro,y=fem,size=proportion,color=factor(tech)))+
  geom_point()+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  geom_text_repel(data=data6_label,aes(label=topic),color = 'black', size=3, 
                  max.overlaps= 20)+
  facet_wrap(~ region,nrow=4,scales = "free_x")+
  geom_vline(data=data6_ave, aes(xintercept = ave_pro,color=factor(tech)))+
  geom_hline(data=data6_ave, aes(yintercept = ave_fem,color=factor(tech)))+
  scale_color_manual(values=c("#DFC286","#80AFBF","grey"),labels=c('Yes','No','Not defined'))+
  #scale_color_gradientn(colors = 
                          #wesanderson::wes_palette("Zissou1", 3, type = "continuous"))+
  labs(x= 'Topic popularity', y = 'Feminization',color="Technique",size="Topic proportion")+
  theme_minimal()+
  theme(#text = element_text(size=18),
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    panel.border = element_rect(color = "grey50", fill = NA, size = 1),
    strip.text = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text=element_text(size=13),
    legend.title = element_text(size = 13),
    axis.title = element_text(size = 13))

a_plot

install.packages("patchwork")
library(patchwork)

ap_plot <- a_plot+inset_element(point_plot, 
                     left = 0.492, bottom = -0.0384, right = 1.006, top = 0.202)

ggsave('fig3.png', ap_plot, width = 10, height = 10, dpi = 500)



#fig4
library(ggsignif) 
library(dplyr)
setwd("D:/PycharmProjects/gender改/data/picture")
#对整体网络做出入度图
#A图 
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm = T),
      sd = sd(x[[col]], na.rm = T))
  }
  data_sum <- ddply(data, groupnames, .fun = summary_func, varname)
  data_sum <- rename(data_sum)
  return(data_sum)
}

data_A <- read.csv('fig4A_inout.csv',header = TRUE)
data_a <- data_A %>% filter(network_type == 'total')
df_a <- data_summary(data_a, varname = "centrality", 
                     groupnames = c("inout","gender"))
df_in_total <- data_A %>% filter(inout=="in-degree")
compare_means(centrality ~ gender, data = df_in_total)
df_out_total <- data_A %>% filter(inout=="out-degree")
compare_means(centrality ~ gender, data = df_out_total)
df_female_total <- data_A %>% filter(gender=="female")
compare_means(centrality ~ inout, data = df_female_total)
df_male_total <- data_A %>% filter(gender=="male")
compare_means(centrality ~ inout, data = df_male_total)

plot_total <- ggplot(df_a,aes(inout,mean,fill=gender))+
  geom_col(position = 'dodge')+
  theme_minimal()+
  geom_signif(data=df_a,
              aes(xmax=0.70, xmin=1.25, annotations="****", y_position=0.0000105),
              textsize = 5, vjust = 0.05, tip_length = c(0.03, 0.2),
              manual=TRUE)+
  geom_signif(data=df_a,
              aes(xmax=1.70, xmin=2.25, annotations="****", y_position=0.0000112),
              textsize = 5, vjust = 0.05, tip_length = c(0.02, 0.6),
              manual=TRUE)+
  geom_signif(data=df_a,
              aes(xmax=0.65, xmin=1.65, annotations="****", y_position=0.000012),
              textsize = 5, vjust = 0.05, tip_length = c(0.7,0.05),
              manual=TRUE)+
  geom_signif(data=df_a,
              aes(xmax=1.30, xmin=2.30, annotations="****", y_position=0.0000125),
              textsize = 5, vjust = 0.05, tip_length = c(0.08,0.05),
              manual=TRUE)+
  xlab ('Overall')+ylab('Centrality')+
  geom_blank(aes(y = 0.000013))+
  scale_fill_manual(values = c("#6490B3","#B5C0DC"),
                    name = 'Gender',
                    labels=c('Female','Male'))+
  scale_x_discrete(labels = c('In-degree','Out-degree'))+
  theme(axis.text.x = element_text(size = 11),
        legend.text=element_text(size=12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 14))


#covid stain
data_B <- read.csv('fig4B_inout.csv',header = TRUE)
df_b_mean <- data_summary(data_B,varname="centrality",
                          groupnames = c("inout","network_type","gender"))
df_b_in <- df_b_mean %>% filter(inout=="in-degree")
df_b_out <- df_b_mean %>% filter(inout=="out-degree")
#in degree
df_in_covid <- data_B %>% filter(network_type == "COVID-19",inout == "in-degree")
df_in_stain <- data_B %>% filter(network_type == "Staining",inout == "in-degree")
df_in_female <- data_B %>% filter(gender == "female",inout == "in-degree")
df_in_male <- data_B %>% filter(gender == "male",inout == "in-degree")
compare_means(centrality ~ gender, data = df_in_covid)
compare_means(centrality ~ gender, data = df_in_stain)
compare_means(centrality ~ network_type, data = df_in_female)
compare_means(centrality ~ network_type, data = df_in_male)
#out degree
df_out_covid <- data_B %>% filter(network_type == "COVID-19",inout == "out-degree")
df_out_stain <- data_B %>% filter(network_type == "Staining",inout == "out-degree")
df_out_female <- data_B %>% filter(gender == "female",inout == "out-degree")
df_out_male <- data_B %>% filter(gender == "male",inout == "out-degree")
compare_means(centrality ~ gender, data = df_out_covid)
compare_means(centrality ~ gender, data = df_out_stain)
compare_means(centrality ~ network_type, data = df_out_female)
compare_means(centrality ~ network_type, data = df_out_male)


plot_in <- ggplot(df_b_in,aes(network_type,mean,fill=gender))+
  geom_col(position = "dodge")+
  geom_signif(data=df_b_in,
              aes(xmax=0.70, xmin=1.25, annotations="***", y_position=0.00033),
              textsize = 5, vjust = 0.05, tip_length = c(0.03, 0.3),
              manual=TRUE)+
  geom_signif(data=df_b_in,
              aes(xmax=1.70, xmin=2.25, annotations="***", y_position=0.00027),
              textsize = 5, vjust = 0.05, tip_length = c(0.45, 0.01),
              manual=TRUE)+
  geom_signif(data=df_b_in,
              aes(xmax=0.65, xmin=1.65, annotations="***", y_position=0.00035),
              textsize = 5, vjust = 0.05, tip_length = c(0.3,0.05),
              manual=TRUE)+
  geom_signif(data=df_b_in,
              aes(xmax=1.30, xmin=2.30, annotations="*", y_position=0.00037),
              textsize = 5, vjust = 0.05, tip_length = c(0.3,0.05),
              manual=TRUE)+
  theme_minimal()+
  geom_blank(aes(y = 0.00039))+
  scale_fill_manual(values = c("#6490B3","#B5C0DC"),
                    name = 'Gender',
                    labels=c('Female','Male'))+
  theme(axis.text.x = element_text(size = 11),
        legend.text=element_text(size=12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 14))+
  xlab ('In-degree')+ylab('Centrality')

plot_out <- ggplot(df_b_out,aes(network_type,mean,fill=gender))+
  geom_col(position = "dodge")+
  geom_signif(data=df_b_out,
              aes(xmax=0.70, xmin=1.25, annotations="ns", y_position=0.00033),
              textsize = 4, vjust = 0.05, tip_length = c(0.03, 0.2),
              manual=TRUE)+
  geom_signif(data=df_b_out,
              aes(xmax=1.70, xmin=2.25, annotations="****", y_position=0.00022),
              textsize = 5, vjust = 0.05, tip_length = c(0.03, 0.25),
              manual=TRUE)+
  geom_signif(data=df_b_out,
              aes(xmax=0.65, xmin=1.65, annotations="****", y_position=0.00035),
              textsize = 5, vjust = 0.05, tip_length = c(0.5,0.05),
              manual=TRUE)+
  geom_signif(data=df_b_out,
              aes(xmax=1.30, xmin=2.30, annotations="**", y_position=0.00037),
              textsize = 5, vjust = 0.05, tip_length = c(0.6,0.05),
              manual=TRUE)+
  theme_minimal()+
  geom_blank(aes(y = 0.00039))+
  scale_fill_manual(values = c("#6490B3","#B5C0DC"),
                    name = 'Gender',
                    labels=c('Female','Male'))+
  theme(axis.text.x = element_text(size = 11),
        legend.text=element_text(size=12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 14))+
  xlab ('Out-degree')+ylab('Centrality')

plot_inout <- ggarrange(plot_total,plot_in,plot_out,nrow=1,
                        common.legend = T, legend = "right")


data_B_asso <- read.csv('fig4B_asso.csv',header = TRUE)
data_B_asso <- data_B_asso %>% melt(id.vars = c('network_type'))
plot_m <- ggplot(data_B_asso,aes(network_type,value,fill=variable))+
  geom_col(width=0.5)+
  facet_wrap(.~variable,nrow=1,scales = "free_y")+
  theme_minimal()+
  xlab (NULL)+
  ylab (NULL)+
  scale_fill_manual(values = c("#C9B089","#946543","#999F7E","#8C8C8C"),
                    name="Index",
                    breaks = c("gender_asso","region_asso","deg_asso","clustering"),
                    labels = c("Gender assortativity","Region assortativity",
                               "Degree assortativity","Clustering"))+
  scale_x_discrete(limits=as.character(c('overall','COVID-19','Staining')),
                   labels = c('Overall','COVID-19','Staining'))+
  theme (plot.title = element_text (hjust = 0.5),
         legend.position = 'bottom',
         strip.text.x = element_blank(),
         axis.text.x = element_text(size = 10.5),
         legend.text=element_text(size=12),
         legend.title = element_text(size = 12))

p1 <- ggarrange(plot_m,plot_inout,nrow=2,labels = c("a","b"), # 添加标签
                font.label = list(size = 14, face = "bold"))
ggsave('fig4 new.png', p1, width = 10, height = 5.8, dpi = 500)

#extended data
setwd("D:/PycharmProjects/gender改/data/extended data")
#s fig1
df <- read.csv('distribution_reg_gen.csv',header = TRUE)
#df <- df %>%
#  mutate(Region = recode(Region,"East Asia & Pacific"="East Asia\n& Pacific","Europe & Central Asia"="Europe &\nCentral Asia",
#                         "Latin America & Caribbean"="Latin America\n& Caribbean","Middle East & North Africa"="Middle East\n& North Africa",
#                         "North America"="North\nAmerica","South Asia"="South\nAsia",
#                         "Sub-Saharan Africa"="Sub-Saharan\nAfrica"))
#region分布
#summarise可能失效
detach("package:plyr")
detach("package:dplyr")
library("plyr") 
library("dplyr")
df_region <- df %>% group_by(Region) %>% summarise(count = sum(name)) 
options(scipen=200)
p_region <- ggplot(data = df_region,aes(x=Region,y=count))+
  geom_col(position = 'dodge',width=0.8,fill='grey50')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 12),
        axis.title = element_text(size = 14),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"))+
  xlab('Region')+ylab('Count')
  
#gender分布
df_gender <- df %>% group_by(gender) %>% summarise(count=sum(name))
p_gender <- ggplot(data = df_gender,aes(x=gender,y=count,fill=gender))+
  geom_col(position = 'dodge',width=0.8)+
  theme_bw()+
  xlab('Gender')+ylab("Count")+
  theme(axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = 'none',
        plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"))+
  scale_x_discrete(labels = c('Female','Male'))+
  scale_fill_manual(values=c("#6490B3","#B5C0DC"))


#region和gender共同的分布比例
df_dis <- df %>%
  group_by(gender) %>%
  mutate(proportion = name/sum(name))
p_dis <- ggplot(data=df_dis,aes(x=Region,y=proportion,fill=gender, 
                            label = scales::percent(proportion, accuracy = 0.1)))+
  geom_col(position = 'dodge')+
  theme_bw()+
  scale_fill_manual(values=c("#6490B3","#B5C0DC"),
                    name='Gender',
                    labels=c('Female','Male'))+
  theme(legend.position = c(0.8, 0.15),
        legend.background = element_rect(colour = "white"))+
  ylab('Proportion')+
  theme(axis.text.y = element_text(angle = 30, hjust = 1,size = 12),
        legend.key = element_rect(color="white"),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"),
        legend.position = c(0.7,0.12)) +
  geom_text(position = position_dodge(width = 1.1),hjust=0.8,size=3)+
  coord_flip ()+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_discrete(limits=c("Sub-Saharan Africa",
                            "South Asia","North America",
                            "Middle East & North Africa",
                            "Latin America & Caribbean",
                            "Europe & Central Asia",
                            "East Asia & Pacific"))

#p_ex1 <- ggarrange(p_region,p_field,labels = c("a","c"),ncol = 1,heights = c(1,0.9))
#p_gender1 <- ggarrange(NULL,p_gender,widths = c(0.25,1))
#p_ex2 <- ggarrange(p_gender1,p_dis,labels = c('b','d'),ncol = 1,heights = c(0.4,1))
p_ex1 <- ggarrange(p_region,p_gender,labels = c("a","b"),ncol = 1,heights = c(1,0.6))
p_ex2 <- ggarrange(p_dis,labels = c("c"))
plot_ex1 <- ggarrange(p_ex1,p_ex2,widths = c(0.8,1))
ggsave('researcher description.png', plot_ex1, width = 9, height = 5, dpi = 500)

#s fig3
data_ave <- read.csv('ave question.csv',header=TRUE)
data_ave$dis <- factor(data_ave$dis)

pal<-c('#F0E6E4','#CEB5B9','#AEBFCE','#98A1B1','#b36a6f')
mycolor<-colorRampPalette(pal)
image(x=1:6,y=1,z=as.matrix(1:6),col=mycolor(6))

hot_plot <- ggplot(data_ave, aes(identity, dis, fill = ave_question))+
  geom_raster()+ 
  scale_fill_gradientn(colours=mycolor(20))+
  theme (
    axis.title.x = element_blank (), 
    axis.title.y = element_blank (),
    legend.position='right',
    legend.title = element_blank(),
    text = element_text(size=14, colour = 'grey30'),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12)
  )+
  scale_x_discrete(
    label =  c('East Asia & Pacific\nfemale','East Asia & Pacific\nmale','Europe & Central Asia\nfemale','Europe & Central Asia\nmale',
               'Latin America & Caribbean\nfemale','Latin America & Caribbean\nmale','Middle East & North Africa\nfemale','Middle East & North Africa\nmale',
               'North America\nfemale','North America\nmale',
               'South Asia\nfemale','South Asia\nmale','Sub-Saharan Africa\nfemale','Sub-Saharan Africa\nmale')
  )+
  scale_y_discrete(
    label = c("Technology","Social\nSciences","Physical\nSciences","Life Sciences &\nBiomedicine","Arts &\nHumanities")
  )
hot_plot

df_q <- read.csv('question number.csv',header=TRUE)
df_q <- df_q %>%
    mutate(Region = recode(Region,"East Asia & Pacific"="East Asia\n& Pacific","Europe & Central Asia"="Europe &\nCentral Asia",
                           "Latin America & Caribbean"="Latin America\n& Caribbean","Middle East & North Africa"="Middle East\n& North Africa",
                           "North America"="North\nAmerica","South Asia"="South\nAsia",
                           "Sub-Saharan Africa"="Sub-Saharan\nAfrica"))
ylim1<-c(1,20)
p_region <- ggplot(df_q,aes(x=Region,y=filename,fill=Region))+
  geom_boxplot(outlier.shape = NA)+
  theme_minimal()+
  coord_cartesian(ylim = ylim1)+
  stat_summary(fun="mean", geom="point", shape=20, size=2.5)+
  scale_fill_manual(values=mycolor(7))+
  theme(legend.position="none")+
  ylab('Count')+
  #stat_compare_means(method = "anova")+
  annotate(
    geom = "text", x = 1, y = 20, 
    label = "Anova, p<2.2e-16", hjust = 0, vjust = 1, size = 4
  )+
  theme(axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 14))

compare_list <- list(c("female","male"))
p_gender <- ggplot(df_q,aes(x=gender,y=filename,fill=gender))+
  geom_boxplot(outlier.shape = NA)+
  theme_minimal()+
  #stat_compare_means(comparisons = compare_list,method = "wilcox.test",label = "p.signif")+
  coord_cartesian(ylim = ylim1)+
  stat_summary(fun="mean", geom="point", shape=20, size=2.5)+
  scale_fill_manual(values=mycolor(2))+
  theme(legend.position="none",
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 14))+
  ylab('Count')+xlab('Gender')+
  geom_signif(data=df_q,
              aes(xmax=1, xmin=2, annotations="****", y_position=18),
              textsize = 5, vjust = 0.05, tip_length = c(0,0),
              manual=TRUE)+
  scale_x_discrete(labels=c("Female","Male"))

p_reg_gender <- ggarrange(p_region,p_gender,labels = c("b","c"),widths = c(1,0.3),
                          font.label = list(size = 14, face = "bold"))
knowledge_re <- ggarrange(hot_plot,p_reg_gender,labels = c("a",""),ncol=1,heights = c(1,0.6),
                          font.label = list(size = 14, face = "bold"))
ggsave('knowlege_re.png', knowledge_re, width = 11, height = 8, dpi = 500)

#s fig2
#参与不同field的researcher topic question answer分布
df_field <- read.csv('distribution_field.csv',header = TRUE)
df_field <- df_field %>% filter(dis!=6) %>% 
  mutate(dis = recode(dis, "1"="Arts &\nHumanities","2"="Life Sciences\n& Biomedicine","3"="Physical\nSciences",
                      "4"="Social\nSciences","5"="Technology"))
df_field <- melt(df_field,id.vars = c('dis'))
labels <- c(researchers = "Researchers", topics = "Topics",
            questions="Questions",answers="Answers")
p_field <- ggplot(data = df_field,aes(x=dis,y=value))+
  facet_wrap(.~variable,nrow = 1,scales = 'free_x',
             labeller = labeller(variable=labels))+
  geom_col(position = 'dodge',width=0.6,fill='grey50')+
  theme_bw()+
  ylab('Count')+xlab('Field')+
  coord_flip()+
  theme(strip.background = element_blank())+
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))
ggsave('field.png', p_field, width = 8, height = 3.5, dpi = 500)

#男性女性作者顺序分布
#绘制一半小提琴图用的函数
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

setwd("D:/PycharmProjects/gender改/女性技术参与")
df <- read.csv('2index_gender_tech.csv',header = TRUE)
df$x <- 'x'
compare_list <- list(c("female","male"))
#一半小提琴图
a <- ggplot(df, aes(x = x,y = pro, fill = gender))+
  geom_split_violin(trim = T,colour="white")+
  theme_minimal()+
  ylab('Average order index')+
  xlab(NULL)+
  scale_fill_manual(values = c("#6490B3","#B5C0DC"),
                    name="Gender",
                    labels=c('Female','Male'))+
  stat_summary(data = df, aes(x = x,y = pro,fill = gender),
               fun.min = function(x){quantile(x)[2]},
               fun.max = function(x){quantile(x)[4]},
               geom = 'errorbar', color='black',
               width=0.01,size=0.5,
               position = position_dodge(width = 0.2))+
  geom_point(stat = 'summary',fun=mean,
             position = position_dodge(width = 0.2))+
  stat_compare_means(data = df, aes(x = x,y = pro,fill=gender),
                     label = "p.signif",
                     label.y = max(df$pro),
                     hide.ns = F)+
  theme(axis.text.x = element_blank(),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

b <- ggplot(df, aes(x = x,y = not_first_pro, fill = gender))+
  geom_split_violin(trim = T,colour="white")+
  theme_minimal()+
  ylab('Non-first author proportion')+
  xlab(NULL)+
  scale_fill_manual(values = c("#6490B3","#B5C0DC"),
                    name="Gender",
                    labels=c('Female','Male'))+
  stat_summary(data = df, aes(x = x,y = not_first_pro,fill = gender),
               fun.min = function(x){quantile(x)[2]},
               fun.max = function(x){quantile(x)[4]},
               geom = 'errorbar', color='black',
               width=0.01,size=0.5,
               position = position_dodge(width = 0.2))+
  geom_point(stat = 'summary',fun=mean,
             position = position_dodge(width = 0.2))+
  stat_compare_means(data = df, aes(x = x,y = not_first_pro,fill=gender),
                     label = "p.signif",
                     label.y = max(df$pro),
                     hide.ns = F)+
  theme(axis.text.x = element_blank(),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

#男女性作者贡献分布
df_contri <- read.csv('index_contri.csv',header = TRUE)
#contri0值为1
df_contri1 <- df_contri %>% filter(contri0==1)
#去除异常值
df_contri1 <- df_contri1 %>% filter(!contri %in% boxplot.stats(df_contri1$contri)$out)
c <- ggplot(df_contri1,aes(x=gender,y=contri,fill=gender))+
  geom_boxplot(outlier.shape = NA)+
  theme_minimal()+
  stat_compare_means(comparisons = compare_list,method = "wilcox.test",
                     label = "p.signif")+
  ylab('Experimental contribution')+
  scale_fill_manual(values = c("#6490B3","#B5C0DC"),
                    name="Gender",
                    labels=c('Female','Male'))+
  scale_y_continuous(labels = scales::comma_format())+
  xlab(NULL)+
  stat_summary(fun="mean", geom="point", shape=20, size=2.5)+
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_blank())+
  scale_x_discrete(labels=c("Female","Male"))

#百分比堆积柱状图
dc <-read.csv('实验贡献作者比例.csv',header = TRUE)
dc$contri = factor(dc$contri)
d <- ggplot(dc,aes(x=gender,y=percent,fill=contri,
                   label = scales::percent(percent, accuracy = 0.1)))+
  geom_bar(stat = 'identity',width = 0.5)+
  scale_fill_manual(values = c("#C25759","#F5DFDB"), #"#6490B3","#B5C0DC"蓝的
                    labels = c('No','Yes'))+
  theme_minimal()+
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = 'bottom',
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12))+
  labs(fill='Experimental\ncontribution')+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_text(position = position_fill(vjust = 0.5),size=3)+
  xlab(NULL)+ylab('Percent')+
  scale_x_discrete(labels=c("Female","Male"))

p1 <- ggarrange(a,b,c,nrow = 1,labels = c("a","b","c"),
                common.legend = T,legend = "bottom",
                font.label = list(size = 14, face = "bold"))
p <- ggarrange(p1,d,nrow=1,labels = c("","d"),widths = c(0.75,0.25),
               font.label = list(size = 14, face = "bold"))
ggsave('实验参与.png',p,width = 12,height = 3.5,dpi = 500)


#转化性分布
#参与
df_tran <- read.csv('pmid_transfer_author.csv',header = TRUE)
apt <- ggplot(df_tran,aes(x=gender,y=apt,fill=gender))+
  geom_boxplot(outlier.shape = NA)+
  theme_minimal()+
  theme(plot.title = element_text (hjust = -0.25),
        legend.position="none")+
  stat_compare_means(comparisons = compare_list,method = "wilcox.test",
                     label = "p.signif")+
  stat_summary(fun="mean", geom="point", shape=20, size=2.5)+
  scale_fill_manual(values = c("#6490B3","#B5C0DC"))+
  xlab(NULL)+ylab('APT')+
  scale_x_discrete(labels=c("Female","Male"))+
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12))

df_tran1 <- df_tran %>% filter(author==1)
apt1 <- ggplot(df_tran1,aes(x=gender,y=apt,fill=gender))+
  geom_boxplot(outlier.shape = NA)+
  theme_minimal()+
  theme(plot.title = element_text (hjust = -0.25),
        legend.position="none")+
  stat_compare_means(comparisons = compare_list,method = "wilcox.test",
                     label = "p.signif")+
  stat_summary(fun="mean", geom="point", shape=20, size=2.5)+
  scale_fill_manual(values = c("#6490B3","#B5C0DC"))+
  xlab(NULL)+ylab('APT')+
  scale_x_discrete(labels=c("Female","Male"))+
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12))

#绘制三元相图
library(ggtern)
labels <- c(female='Female',male='Male')
tr <- ggtern(data = df_tran,aes(molecular_cellular,human,animal))+
  facet_wrap(.~gender,labeller = labeller(gender=labels))+
  geom_mask()+
  theme (plot.title = element_text (hjust = 0),
         strip.background = element_blank(),
         axis.title = element_text(size = 12),
         legend.title = element_text(size = 12),
         legend.position = 'bottom',
         strip.text = element_text(size = 12))+
  stat_density_tern(geom='polygon',bdl.val = NA,
    aes(fill=..level..,alpha = ..level..))+
  scale_fill_gradient(low = '#94c8c5',high = '#183f7f')+
  labs(x ="M/C",y = "H",z = "A",fill='Density')+
  guides(alpha = "none")+
  ggtitle('a')
ggsave('tr.png',tr,width = 8,height = 5,dpi = 500)

tr1 <- ggtern(data = df_tran1,aes(molecular_cellular,human,animal))+
  facet_wrap(.~gender,labeller = labeller(gender=labels))+
  geom_mask()+
  theme (plot.title = element_text (hjust = 0),
         strip.background = element_blank(),
         axis.title = element_text(size = 12),
         legend.title = element_text(size = 12),
         legend.position = 'bottom',
         strip.text = element_text(size = 12))+
  stat_density_tern(geom='polygon',bdl.val = NA,
                    aes(fill=..level..,alpha = ..level..))+
  scale_fill_gradient(low = '#94c8c5',high = '#183f7f')+
  labs(x ="M/C",y = "H",z = "A",fill='Density')+
  guides(alpha = "none")+
  ggtitle('b')
ggsave('tr1.png',tr1,width = 8,height = 5,dpi = 500)

#文章主题
df_tech_pro <- read.csv('tech文章占比.csv',header = TRUE)
tech_pro <- ggplot(df_tech_pro,aes(x=gender,y=tech_pro,fill=gender))+
  geom_boxplot(outlier.shape = NA)+
  theme_minimal()+
  theme(plot.title = element_text (hjust = -0.25),
        legend.position="none")+
  stat_compare_means(comparisons = compare_list,method = "wilcox.test",
                     label = "p.signif")+
  stat_summary(fun="mean", geom="point", shape=20, size=2.5)+
  scale_fill_manual(values = c("#6490B3","#B5C0DC"))+
  ylab('Technical article proportion')+
  xlab(NULL)+
  scale_x_discrete(labels=c("Female","Male"))+
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12))

df_tech_pro1 <- read.csv('一作tech文章占比.csv',header = TRUE)
tech_pro1 <- ggplot(df_tech_pro1,aes(x=gender,y=tech_pro,fill=gender))+
  geom_boxplot(outlier.shape = NA)+
  theme_minimal()+
  theme(plot.title = element_text (hjust = -0.25),
        legend.position="none")+
  stat_compare_means(comparisons = compare_list,method = "wilcox.test",
                     label = "p.signif")+
  stat_summary(fun="mean", geom="point", shape=20, size=2.5)+
  scale_fill_manual(values = c("#6490B3","#B5C0DC"))+
  ylab('Technical article proportion')+
  xlab(NULL)+
  scale_x_discrete(labels=c("Female","Male"))+
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12))

tt <- ggarrange(apt,tech_pro,apt1,tech_pro1,
                labels = c("c","d","e","f"),nrow = 1,
                font.label = list(size = 14, face = "bold"))
ggsave('转化性和主题.png',tt,width = 12,height = 3.5,dpi = 500)

#技术问题占比
df_q <- read.csv('提出技术问题占比.csv',header = TRUE)
#有文章vs无文章
compare_list_article <- list(c('no','have'))
df_q$x <- 'x'
qa <- ggplot(df_q, aes(x = x,y = pro, fill = article))+
  geom_split_violin(trim = T,colour="white")+
  theme_minimal()+
  ylab('Techinal question proportion')+
  xlab(NULL)+
  scale_fill_manual(values = c("#6490B3","#B5C0DC"),
                    name='Article',
                    labels=c('Have','No'))+
  stat_summary(data = df_q, aes(x = x,y = pro),
               fun.min = function(x){quantile(x)[2]},
               fun.max = function(x){quantile(x)[4]},
               geom = 'errorbar', color='black',
               width=0.01,size=0.5,
               position = position_dodge(width = 0.2))+
  geom_point(stat = 'summary',fun=mean,
             position = position_dodge(width = 0.2))+
  stat_compare_means(data = df_q, aes(x = x,y = pro),
                     label = "p.signif",
                     label.y = max(df_q$pro),
                     hide.ns = F)+
  theme(axis.text.x = element_blank(),
        legend.position = 'bottom',
        axis.title = element_text(element_text(size = 14)),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

article.labs <- c("Have article published", "No article published") 
names(article.labs) <- c("have", "no")  
qb <- ggplot(df_q, aes(x = x,y = pro, fill = gender))+
  geom_split_violin(trim = T,colour="white")+
  facet_wrap(.~article,labeller = labeller(article=article.labs))+
  theme_minimal()+
  ylab('Techinal question proportion')+
  xlab(NULL)+
  scale_fill_manual(values = c("#6490B3","#B5C0DC"),
                    name='Gender',
                    labels=c('Female','Male'))+
  stat_summary(data = df_q, aes(x = x,y = pro),
               fun.min = function(x){quantile(x)[2]},
               fun.max = function(x){quantile(x)[4]},
               geom = 'errorbar', color='black',
               width=0.01,size=0.5,
               position = position_dodge(width = 0.2))+
  geom_point(stat = 'summary',fun=mean,
             position = position_dodge(width = 0.2))+
  stat_compare_means(data = df_q, aes(x = x,y = pro),
                     label = "p.signif",
                     label.y = max(df_q$pro),
                     hide.ns = F)+
  theme(axis.text.x = element_blank(),
        legend.position = 'bottom',
        axis.title = element_text(element_text(size = 14)),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12))

q <- ggarrange(qa,qb,nrow = 1,labels = c("a","b"),widths = c(0.33,0.6))
ggsave('技术问题参与.png',q,width = 8,height = 3.5,dpi = 500)
