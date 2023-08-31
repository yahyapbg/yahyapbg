statbmt1$Trials <- factor(statbmt1$Trials, levels = c("WAR20","WAR18","GAT18", "GAT20","GAT19", "TOS19" ))
#select<-statbmt1[which(statbmt1$env!="GAT20-T1-S2"), ]
#statbmt1$Trial <- factor(statbmt1$Trial, levels = c("GAT18", "WAR18", "WAR20","TOS19","GAT19"))

p6<-ggplot(select, aes(x = avg_tgw, y = avg_prot, col = Trials,group=Sowing)) +
geom_point(aes(shape = Sowing)) +
#scale_color_manual(values = c( "Zissou1", "Royal1" ,"Royal2" ,"Darjeeling1","Chevalier1","GrandBudapest1", "GrandBudapest2" ))+
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x, aes(group = interaction(Sowing, Trials))) +
  #geom_smooth(method = "lm", se = FALSE, formula = y ~ x, aes(group = Sowing)) +
  facet_grid(~Sowing, scales = "free_y") +
  #facet_wrap(~ paste(Sowing, env_class), scales = "free_y") +  # Combine Sowing and env_class
  #stat_summary(fun.y = mean, geom = "pointrange", size = 0.35, aes(group = Trial)) +
  #theme(strip.text.x = element_text(size = 8, face = "bold")) +
  xlab("Thousand grain weight (mg)") +
  ylab("Total crude protein %") +
  theme_test() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10,face = "bold"),
    axis.title = element_text(size = 10,face = "bold"),
    strip.text.x = element_text(size = 10,face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )+ 
  scale_color_manual(values=c("forestgreen","blue","purple","maroon","red","coral"))+
  #scale_color_manual(values=c("forestgreen","purple","blue","maroon","red"))+
  scale_shape_manual(values = c("S1" = 16, "S2" = 15)) +  # Set the shapes for S1 and S2
  ylim(5, 20)+
  ######if i want to bold right legends bold(after + is below code)######
theme(legend.text = element_text(face = "bold")) +  # Bold legend text
  guides(shape = guide_legend(override.aes = list(size = 2)))  # Increase legend shape size"red",

p6 <- p6 +
  theme(legend.title = element_text(face = "bold")) +  # Bold legend title
  theme(
    legend.text = element_text(face = "bold"),
    legend.position = "right",  # Move legend to the right side
    legend.box = "vertical"     # Display legend in a vertical layout
  )+
  labs(shape = "Time of Sowing")
  
p6



p6 + stat_poly_eq(formula = y ~ x, 
                  aes(group = 4, label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                  parse = TRUE, label.x.npc = "right", size = 3)
