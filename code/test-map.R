plot <- ggmap(googleterrainsyria)  + 
  geom_point(aes( x = long, y = lat, colour= documentation, alpha =documentation), size =3, data=data.agreement.score.centers , alpha = 0.85 ) +
  ggtitle(paste("Map: ", title , sep="")) +
  scale_colour_gradient2() +
  theme(plot.title=element_text(face="bold", size=10),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))
ggsave(filename="out/map_.png", plot=plot, width=8, height=6,units="in", dpi=300)
plot