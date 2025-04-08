library(dagitty)
library(ggplot2)

dag {
  Agricultural_productivity [latent,pos="0.079,0.040"]
  Community_forest [exposure,pos="-0.097,0.541"]
  Economic_wellbeing [exposure,pos="-0.765,0.570"]
  Fire [latent,pos="0.044,-0.808"]
  Human_migration [exposure,pos="-1.021,-0.043"]
  Human_population [exposure,pos="-0.900,-0.756"]
  Landslide [latent,pos="0.340,-0.211"]
  Lightning [latent,pos="0.358,-1.010"]
  Preexisting_Gaps [exposure,pos="-0.755,-1.050"]
  Precipitation_anomalies [exposure,pos="0.248,-0.573"]
  Road_density [exposure,pos="-0.322,-0.808"]
  Tree_removal [outcome,pos="-0.525,-0.172"]
  slope [adjusted,pos="0.380,0.032"]
  Agricultural_productivity -> Economic_wellbeing
  Community_forest -> Agricultural_productivity
  Community_forest -> Preexisting_Gaps
  Community_forest -> Tree_removal
  Economic_wellbeing -> Preexisting_Gaps
  Economic_wellbeing -> Tree_removal
  Fire -> Landslide
  Fire -> Tree_removal
  Human_migration -> Tree_removal
  Human_migration <-> Human_population
  Human_migration <-> Road_density
  Human_population -> Preexisting_Gaps
  Human_population -> Road_density
  Human_population -> Tree_removal
  Landslide <-> Tree_removal
  Lightning -> Fire
  Preexisting_Gaps -> Tree_removal
  Precipitation_anomalies -> Agricultural_productivity
  Precipitation_anomalies -> Fire
  Precipitation_anomalies -> Landslide
  Road_density -> Fire
  Road_density -> Preexisting_Gaps
  Road_density -> Tree_removal
  slope -> Agricultural_productivity
  slope -> Landslide
  slope -> Preexisting_Gaps
}






DAG<-dagitty("dag {
Agricultural_productivity -> Economic_wellbeing
  Community_forest -> Agricultural_productivity
  Community_forest -> Tree_removal
  Economic_wellbeing -> Preexisting_gaps
  Economic_wellbeing -> Tree_removal
  Community_forest -> Preexisting_gaps
  Fire -> Landslide
  Fire -> Tree_removal
  Human_migration -> Tree_removal
  Human_migration <-> Human_population
  Human_migration <-> Road_density
  Human_population -> Preexisting_gaps
  Human_population -> Road_density
  Human_population -> Tree_removal
  Human_population -> Economic_wellbeing
  Landslide <-> Tree_removal
  Lightning -> Fire
  Preexisting_gaps -> Tree_removal
  Precipitation_anomalies -> Agricultural_productivity
  Precipitation_anomalies -> Fire
  Precipitation_anomalies -> Landslide
  Road_density -> Fire
  Road_density -> Preexisting_gaps
  Road_density -> Tree_removal
  Slope -> Agricultural_productivity
  Slope -> Landslide
  Slope -> Preexisting_gaps
}")

library(ggdag)

plot(DAG)

tidy_dag <- ggdag::tidy_dagitty(DAG)

tidy_dag$data$name<-gsub("_","\n",tidy_dag$data$name)
tidy_dag$data$VariableType<-NA

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Agricultural\nproductivity","Unobserved",tidy_dag$data$VariableType)

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Community\nforest","Estimated",tidy_dag$data$VariableType)

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Lightning","Unobserved",tidy_dag$data$VariableType)

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Economic\nwellbeing","Estimated",tidy_dag$data$VariableType)

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Fire","Unobserved",tidy_dag$data$VariableType)

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Human\nmigration","Estimated",tidy_dag$data$VariableType)

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Human\npopulation","Estimated",tidy_dag$data$VariableType)

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Landslide","Unobserved",tidy_dag$data$VariableType)

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Precipitation\nanomalies","Estimated",tidy_dag$data$VariableType)

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Road\ndensity","Estimated",tidy_dag$data$VariableType)

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Tree\nremoval","Outcome",tidy_dag$data$VariableType)

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Slope","Control",tidy_dag$data$VariableType)

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Preexisting\ngaps","Estimated",tidy_dag$data$VariableType)


cols <- c("Estimated" = "#009E73", "Control" = "#0072B2","Outcome"="#E69F00","Unobserved"="#999999")

d<-tidy_dag%>%
ggplot(aes(
  x = x,
  y = y,
  xend = xend,
  yend = yend#,
  #shape = adjusted,
  #col = d_relationship
)) +
  geom_dag_point(size=20,aes(color=VariableType)) +
  geom_dag_text(col = "white",size=2.5) +
  geom_dag_edges(curvature = 0.3,  arrow_bidirected = grid::arrow(length = grid::unit(5, "pt"), ends = "both", type =
                                                    "closed"))+
  #geom_dag_edges(end_cap = ggraph::circle(15, "mm")) +
  #geom_dag_collider_edges() +

  theme_dag() +
  scale_adjusted() +
  scale_color_manual(values=cols)+
  expand_plot(expand_y = expansion(c(0.1, 0.1)))+ 
  theme(legend.title=element_blank(),
        legend.text = element_text(color="black",size=10),
        legend.position = "bottom",
        legend.key.height= unit(1, 'mm'))
  
d
ggsave("../Figures/DAG.png",d,dpi=350,units = "in",width = 8.5, height = 7, bg="white")




####PreexistingGaps Dag

dag {
  Agricultural_productivity [latent,pos="0.079,0.040"]
  Community_forest [exposure,pos="-0.097,0.541"]
  Economic_wellbeing [exposure,pos="-0.765,0.570"]
  Fire [latent,pos="0.044,-0.808"]
  Human_migration [exposure,pos="-1.021,-0.043"]
  Human_population [exposure,pos="-0.900,-0.756"]
  Landslide [latent,pos="0.340,-0.211"]
  Lightning [latent,pos="0.358,-1.010"]
  Preexisting_Gaps [outcome,pos="-0.755,-1.050"]
  Road_density [exposure,pos="-0.322,-0.808"]
  slope [adjusted,pos="0.380,0.032"]
  Agricultural_productivity -> Economic_wellbeing
  Community_forest -> Agricultural_productivity
  Community_forest -> Preexisting_Gaps
  Economic_wellbeing -> Preexisting_Gaps
  Fire -> Landslide
  Human_migration <-> Human_population
  Human_migration <-> Road_density
  Human_population -> Preexisting_Gaps
  Human_population -> Road_density
  Lightning -> Fire
  Road_density -> Fire
  Road_density -> Preexisting_Gaps
  slope -> Agricultural_productivity
  slope -> Landslide
  slope -> Preexisting_Gaps
}

DAG<-dagitty("dag {
Agricultural_productivity -> Economic_wellbeing
  Community_forest -> Agricultural_productivity
  Community_forest -> Preexisting_gaps
  Economic_wellbeing -> Preexisting_gaps
  Fire -> Landslide
  Human_migration <-> Human_population
  Human_migration <-> Road_density
  Human_population -> Preexisting_gaps
  Human_population -> Road_density
  Human_population -> Economic_wellbeing
  Lightning -> Fire
  Road_density -> Fire
  Road_density -> Preexisting_gaps
  Slope -> Agricultural_productivity
  Slope -> Landslide
  Slope -> Preexisting_gaps
}")



tidy_dag <- ggdag::tidy_dagitty(DAG)

tidy_dag$data$name<-gsub("_","\n",tidy_dag$data$name)
tidy_dag$data$VariableType<-NA

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Agricultural\nproductivity","Unobserved",tidy_dag$data$VariableType)

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Community\nforest","Estimated",tidy_dag$data$VariableType)

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Lightning","Unobserved",tidy_dag$data$VariableType)

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Economic\nwellbeing","Estimated",tidy_dag$data$VariableType)

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Fire","Unobserved",tidy_dag$data$VariableType)

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Human\nmigration","Estimated",tidy_dag$data$VariableType)

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Human\npopulation","Estimated",tidy_dag$data$VariableType)

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Landslide","Unobserved",tidy_dag$data$VariableType)


tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Road\ndensity","Estimated",tidy_dag$data$VariableType)


tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Slope","Control",tidy_dag$data$VariableType)

tidy_dag$data$VariableType<-
  ifelse(tidy_dag$data$name=="Preexisting\ngaps","Outcome",tidy_dag$data$VariableType)


cols <- c("Estimated" = "#009E73", "Control" = "#0072B2","Outcome"="#E69F00","Unobserved"="#999999")

d<-tidy_dag%>%
  ggplot(aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend#,
    #shape = adjusted,
    #col = d_relationship
  )) +
  geom_dag_point(size=20,aes(color=VariableType)) +
  geom_dag_text(col = "white",size=2.5) +
  geom_dag_edges(curvature = 0.3,  arrow_bidirected = grid::arrow(length = grid::unit(5, "pt"), ends = "both", type =
                                                                    "closed"))+
  #geom_dag_edges(end_cap = ggraph::circle(15, "mm")) +
  #geom_dag_collider_edges() +
  
  theme_dag() +
  scale_adjusted() +
  scale_color_manual(values=cols)+
  expand_plot(expand_y = expansion(c(0.1, 0.1)))+ 
  theme(legend.title=element_blank(),
        legend.text = element_text(color="black",size=10),
        legend.position = "bottom",
        legend.key.height= unit(1, 'mm'))

d
ggsave("../Figures/DAG2.png",d,dpi=350,units = "in",width = 8.5, height = 7, bg="white")



