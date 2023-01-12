


Inset.data <- read.csv("C:/Users/ohler/Downloads/Inset-data.csv")

mod.1 <- lm(Perc.red~mean_drt, data = Inset.data)
mod.2 <- lm(Perc.red~mean_drt+mean_drt^2, data = Inset.data)
mod.3 <- lm(Perc.red~mean_drt, data = Inset.data)




fake.dat <- data.frame(x=c(1, 0.2,0.1, 0.0, -0.1, -0.2, -0.3, -0.4, -0.5, -0.6, -0.7, -.9))

fake.dat$y <- (exp(fake.dat$x*0.49 - 0.12)-1)*100



Fig4.in<-
  
  ggplot(fake.dat, aes(x = x, y = y))+
  geom_hline(yintercept=0,lty=2,color="black",size=1,show.legend = FALSE) +
  geom_vline(xintercept=0,lty=1,color="gray30",size=1,show.legend = FALSE)+
  #geom_abline()
geom_smooth(method = "loess", size = 2)+
#geom_smooth(method="lm", formula=y~x)+
#geom_point()+
  coord_cartesian(xlim = c(-0.99, 0.2))+
  labs(y = "% change", x="Drought Severity")+
  ylim(-50,5)+
  theme_tufte(ticks=T,base_size = 22)+ geom_rangeframe()+
theme(text=element_text(size=16,  family="Calibri",color="black"),
     axis.line.x = element_line(color="black", size = 1),
      axis.line.y = element_line(color="black", size = 1),
      legend.position="none")
Fig4.in
#












Fig4.in<-
  
  ggplot(Inset.data, aes(x = mean_drt, y = Perc.red))+
  geom_hline(yintercept=0,lty=2,color="black",size=1,show.legend = FALSE) +
  geom_vline(xintercept=0,lty=1,color="gray30",size=1,show.legend = FALSE)+
  geom_abline()
  #geom_smooth(method = "loess")+
  #geom_smooth(method="lm", formula=y~x)+
  geom_point()+
  labs(y = "% reduction in ANPP", x="Drought Severity")+
  theme_tufte(ticks=T,base_size = 22)+ geom_rangeframe()#+
  #theme(text=element_text(size=16,  family="Calibri",color="black"),
   #     axis.line.x = element_line(color="black", size = 1),
  #      axis.line.y = element_line(color="black", size = 1),
  #      legend.position="none")
Fig4.in






