library(ggplot2)
library(readxl)
library(ggpubr)
file.choose()
SED1vsmTq2<-read_xlsx("/Users/itzelmeneses/Desktop/SED1 vs mTq2-mNG levadura con NaCl.xlsx")

ggplot(data = SED1vsmTq2, mapping= aes(x=Tratamiento, y=Normalización, color=Construcción)) + 
  geom_smooth ( ) + geom_point() +
  scale_y_continuous(limits = c (0.9,1.7), breaks = seq(from = 0.9, to = 1.7, by = 0.2)) +
  scale_x_continuous(limits = c (0,2.0), breaks = seq(from = 0, to = 2.0, by = 0.2)) +
  scale_color_manual(values=c("cyan", "blue")) +
  xlab("NaCl (M)") +
  ylab("normalized DxAm/DxDm")+
  labs(color = NULL) +
  theme_classic() +
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20), legend.text = element_text(size = 20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = c(0.3,0.8)) +
  theme(legend.background = element_rect(linetype=NULL)) +
  guides(color = guide_legend(override.aes = list(size = 4)))
ggsave(filename = "/Users/itzelmeneses/Documents/mtq2 vs sed1.png", device = "png",width = 6,height = 5,
       units = "in",dpi = 650)

