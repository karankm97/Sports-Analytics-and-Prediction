#loading the packages we'll need
require(RCurl) # import csv from URL
require(dplyr) # data manipulation/filtering
require(visNetwork) # producing interactive graphs
require(igraph) # to calculate graph properties
require(ggplot2) # vanilla graphs
require(purrr) # map lists to functions

options(stringsAsFactors = FALSE)
epl_1213 <- read.csv(text=getURL("http://www.football-data.co.uk/mmz4281/1213/E0.csv"), 
                     stringsAsFactors = FALSE)
head(epl_1213[,1:10])

#convert data frame to head to head record
epl_1213 <- epl_1213 %>% dplyr::select(HomeTeam, AwayTeam, FTHG, FTAG) %>% 
  dplyr::rename(team1=HomeTeam, team2= AwayTeam, team1FT = FTHG, team2FT = FTAG) %>%
  dplyr::filter(team1!="")

epl_1213 <- bind_rows(list(epl_1213 %>% 
                        dplyr::group_by(team1,team2) %>%
                        dplyr::summarize(points = sum(case_when(team1FT>team2FT~3,
                                                                team1FT==team2FT~1,
                                                                TRUE ~ 0))),
                      epl_1213 %>% dplyr::rename(team2=team1,team1=team2) %>%
                        dplyr::group_by(team1,team2) %>%
                        dplyr::summarize(points = sum(case_when(team2FT>team1FT~3,
                                                                team2FT==team1FT~1,
                                                                TRUE ~ 0))))) %>%
  dplyr::group_by(team1, team2) %>% dplyr::summarize(tot_points = sum(points)) %>% 
  dplyr::ungroup() %>% dplyr::arrange(team1,team2)

head(epl_1213)

# construct nodes
nodes <- dplyr::group_by(epl_1213, team1) %>% 
  dplyr::summarize(value = sum(tot_points)) %>%
  dplyr::rename(id = team1) %>% 
  dplyr::inner_join(crests, by=c("id"= "team")) %>%
  dplyr::arrange(desc(value)) %>%
  dplyr::mutate(shape="image", label = "", 
                title = paste0("<p><b>",id,"</b><br>Points: ",
                               value,"<br>Position: ",row_number(),"</p>"))

head(nodes)

# construct edges
edge_list <- epl_1213 %>% dplyr::filter(as.character(team1)<as.character(team2)) %>% 
  dplyr::filter(!tot_points %in% c(0,6)) %>%
  dplyr::rename(from=team1,to=team2,value=tot_points) %>% dplyr::select(from, to)

head(edge_list)

# plot network graph
visNetwork(nodes,edge_list,main = "EPL 2012-13 Season",width="800px") %>%
  visEdges(color = list(color="gray",opacity=0.25)) %>%
  visOptions( highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visEvents(stabilizationIterationsDone="function () {this.setOptions( { physics: false } );}") %>%
  visLayout(randomSeed=91)

  # cumulative distribution of degrees
epl_igraph_1213 <- graph_from_data_frame(d=edge_list, vertices=nodes, directed=F)
degs <- rep(0, nrow(nodes)+1)
deg_dist <- degree_distribution(epl_igraph_1213, cumulative=T, mode="all")
degs[1:length(deg_dist)] <-  deg_dist
plot( x=0:(length(degs)-1), y=1-degs, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency", main= " Cumulative Frequency of EPL 2012-13 Degrees")

# pageRank
data.frame(pageRank = 
             round(page_rank(epl_igraph_1213)$vector[order(-page_rank(epl_igraph_1213)$vector)],4))

# betweeness
data.frame(betweeness = 
             round(betweenness(epl_igraph_1213)[order(-betweenness(epl_igraph_1213))],2))

# correlation between the pointSD and meanDegree measures of competitiveness
cor(x = epl_data$pointSD, y = epl_data$meanDegree)
## [1] -0.8579008

# plot historical competitiveness of EPL
ggplot(rbind(epl_data %>% dplyr::select(Season, meanDegree) %>% 
               dplyr::rename(value = meanDegree) %>% dplyr::mutate(measure="Mean Degree"),
             epl_data %>% dplyr::select(Season, pointSD) %>% 
               dplyr::rename(value = pointSD) %>% dplyr::mutate(measure="Point SD")), 
       aes(x=Season, y= value ,color = measure,group=measure)) + geom_line(size=1) +
  geom_point(size=2,color="black") + xlab("EPL Season") + ylab("Value") + 
  ggtitle("Historical Competitiveness of English Premier League") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

 # meanDegree linear model
summary(lm(meanDegree~Season,data=epl_data %>% dplyr::mutate(Season=1:nrow(epl_data))))

# pointSD linear model
summary(lm(pointSD~Season,data=epl_data %>% dplyr::mutate(Season=1:nrow(epl_data))))

# coefficients of linear models
all_leagues %>%  dplyr::group_by(league) %>% dplyr::mutate(Season = 0:(n()-1)) %>% 
  split(.$league) %>% purrr::map(~ lm(pointSD ~ Season, data=.)) %>%
    purrr::map(summary) %>%  purrr::map("coefficients")

# R-squared values for respective linear models
all_leagues %>%  dplyr::group_by(league) %>% dplyr::mutate(Season = 0:(n()-1)) %>% 
  split(.$league) %>% purrr::map(~ lm(pointSD ~ Season, data=.)) %>%
    purrr::map(summary) %>% purrr::map("r.squared")