# 
if (!require(tidyverse)) install.packages("tidyverse", 
                                          dependencies = TRUE)
if (!require(ggforce)) install.packages("ggforce", 
                                          dependencies = TRUE)

# uygun paketlerde birden fazla CPU kullanmak için
if (!require(doParallel)) install.packages("doParallel", 
                                           dependencies = TRUE)
cl <- makePSOCKcluster(parallel::detectCores())
registerDoParallel(cl)

options(scipen=99, digits=3) # 1e-1 şekilde gösterimleri kaldırmak için
options(OutDec= ",") # ondalık ayıracı olak virgül

fm <- function(x )format(round(as.numeric(x),3), big.mark = ".", 
                         decimal.mark = ",")
introText <- function(x) {
  if (!require(DataExplorer)) install.packages("DataExploere", 
                                               dependencies = TRUE)
  i <- introduce(x)
  t1 <- sprintf("Veri seti, %s satır,  %s tanesi sürekli, %s tanesi kategorik veri içeren toplam %s sütun ve  %s gözlemden oluşmakta olup toplamda %s eksik gözlem tespit edilmiştir.",
                fm(i$rows),fm(i$continuous_columns),fm(i$discrete_columns),
                fm(i$columns),fm(i$total_observations), fm(i$total_missing_values))
  print(t1)
}

############################################################
#        HARİTALAR İÇİN BOŞ HARİTA GRAFİĞİ TASLAĞI         #
############################################################
if (!require(rnaturalearth)) install.packages("rnaturalearth", 
                                              dependencies = TRUE)
if (!require(rnaturalearth)) install.packages("rnaturalearthdata", 
                                              dependencies = TRUE)
if (!require(sf)) install.packages("sf", dependencies = TRUE)

tr <- ne_states(country = "turkey",returnclass = "sf")
trmap <- ggplot(tr) +
  geom_sf()+
  theme_minimal()
dissList <- tr  %>%  select(diss_me,iso_3166_2)  %>%  
  st_drop_geometry() %>% 
  transmute(IK = str_extract(string = iso_3166_2, 
                             pattern = "\\d+")  %>% 
              as.integer(), diss_me) 
tr <- tr  %>% left_join(dissList)

############################################################
#                  EKSİK VERİ BAR GRAFİĞİ                  #
############################################################

eksik_plot <- function(x) {
  if (!require(ggthemes)) install.packages("ggthemes", 
                                           dependencies = TRUE)
  if (!require(ggplot2)) install.packages("ggplot2", 
                                          dependencies = TRUE)  
  missing <- apply(x,2,function(x)sum(is.na(x)))
  missing <- missing[missing>0]
  missing <- tibble(OZ = names(missing), ES = missing)  %>% 
    mutate(EV = ES/ nrow(x))
  missing  %>% ggplot(aes(x=fct_reorder(OZ,EV,.desc =TRUE),y = EV))+
    geom_col(aes(fill=EV),alpha=.8)+
    coord_flip()+
    theme_clean()+
    geom_label(aes(label=round(EV,2)),color="grey25",size =2)+
    ylab("Eksik Veri Oranı")+
    xlab("Öz nitelikler")+
    theme(legend.position = "none")
}

############################################################
#                    KORELASYON GRAFİĞİ                    #
############################################################

cor_plot <- function(x) {
  if (!require("ggplot2")) install.packages("ggplot2",
                                            dependencies = TRUE)
  if (!require("ggthemes")) install.packages("ggthemes",
                                             dependencies = TRUE)
  if (!require("ggcorrplot")) install.packages("ggcorrplot",
                                               dependencies = TRUE)
  #if (!requireNamespace("devtools")) install.packages("devtools")
  #devtools::install_github("caijun/ggcorrplot2")
  require(ggcorrplot2)
  tmp <- x  %>% select(-any_of(c("IK","IA"))) %>% 
    select_if(is.numeric)  %>% 
    as.matrix
  corr <- round(cor(tmp),2)
  caret::findCorrelation(x = corr  )
  p.mat <- cor_pmat(tmp)
  ggcorrplot.mixed(corr, upper = "circle", lower = "number", p.mat = p.mat, 
                   insig = "label_sig", sig.lvl = c(0.05, 0.01, 0.001))+
    theme(plot.background = element_rect(color = "black"))
}


############################################################
#                        VAT GRAFİĞİ                       #
############################################################

vat_plot <- function(x) {
  if (!require("ggplot2")) install.packages("ggplot2",
                                            dependencies = TRUE)
  if (!require("ggthemes")) install.packages("ggthemes",
                                             dependencies = TRUE)
  if (!require("factoextra")) install.packages("factoextra",
                                               dependencies = TRUE)
  
  fviz_dist(dist(x))+
    theme_clean()+
    theme(axis.text.x=element_blank(), 
          axis.ticks.x=element_blank(), 
          axis.text.y=element_blank(),  
          axis.ticks.y=element_blank() 
    )+
    xlab("")+ylab("")+
    theme(legend.title = element_blank())
}


############################################################
#           EN UYGUN KÜMELEME YÖNTEMİNİN TESPİTİ           #
############################################################


kum_bul <- function(x) {
  if (!require("NbClust")) install.packages("NbClust",
                                            dependencies = TRUE)
  if (!require("clValid")) install.packages("clValid",
                                            dependencies = TRUE)
  if (!require("cluster")) install.packages("cluster",
                                            dependencies = TRUE)
  if (!require("mclust")) install.packages("mclust",
                                           dependencies = TRUE)
  bestclust <- NbClust(x, method = "ward.D2", 
                       distance = "euclidean", 
                       index = "all", max.nc = 8)
  tibble(index = colnames(bestclust$Best.nc), 
         ks = as.integer(bestclust$Best.nc[1,]),
         id = bestclust$Best.nc[2,]) %>% 
    print() 
  noclust <- as.numeric(names(which.max(table(bestclust$Best.nc[1,]))))
  clmethods <- c("hierarchical", "kmeans", "diana",  "model", "pam", "clara","agnes")
  suppressWarnings(clstFind <- clValid(x, 
                                       nClust = noclust,
                                       method = "ward",
                                       metric = "euclidean",
                                       clMethods = clmethods, 
                                       validation = c("internal","stability")))
  summary(clstFind)
}

############################################################
#                 2 BOYUT KÜMELEME GRAFİĞİ                 #
############################################################

kum_plot <- function(x) {
  if (!require("factoextra")) install.packages("factoextra",
                                               dependencies = TRUE)
  if (!require("ggplot2")) install.packages("ggplot2",
                                            dependencies = TRUE)
  if (!require("ggthemes")) install.packages("ggthemes",
                                             dependencies = TRUE)
  factoextra::fviz_cluster(x, main = "",
                           repel = TRUE, 
                           ggtheme = theme_clean())+
    theme(legend.position = "right")+
    theme_bw() + theme(panel.border = element_blank(), 
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), 
                       axis.line = element_line(colour = "black"))+
    theme(plot.background = element_rect(color = "black"))
}


############################################################
#                  H2O AUTOML MODEL EĞİTME                 #
############################################################


h2o <- function(dt, n=40,seed=2023) {
  if (!require("h2o")) install.packages("h2o",
                                        dependencies = TRUE)
  if (!require("caret")) install.packages("caret",
                                          dependencies = TRUE)
  #h2o.shutdown(prompt = FALSE)
  localH2O = h2o.init(nthreads = -1)
  
  splits <- caret::createDataPartition(dt$CAT, p = 0.8, list = FALSE)
  y <- "CAT"
  x <- setdiff(names(dt), y)
  train <- dt[splits,]
  test <- dt[-splits,]
  dt.hex <- as.h2o(dt)
  train.hex <- as.h2o(train)
  test.hex <- as.h2o(test)
  train.hex[,y] <- h2o::as.factor(train.hex[,y])
  aml <- h2o.automl(x = x,
                    y = y,
                    training_frame = train.hex,
                    max_models = n,
                    seed = seed)
  m <- h2o.get_best_model(aml)
  print(m)
  m_perf <- h2o.performance(model = m, newdata = test.hex)
  print(m_perf)
  
  print(h2o.performance(model = m,newdata = train.hex))
  print(h2o.performance(model = m,newdata = test.hex))
  return(m)
}

kumeDALEX_plot <- function(model, data) {
  if (!require("DALEX")) install.packages("DALEX",
                                          dependencies = TRUE)
  if (!require("DALEXtra")) install.packages("DALEXtra",
                                             dependencies = TRUE)
  
  exp <- explain_h2o(model = model, data  %>% select(-CAT),
                     data$CAT,
                     type = "classification")
  al_aml = model_profile(explainer = exp,type = 'accumulated',
                         N = NULL ,center = FALSE) 
  
  plot(al_aml)+
    geom_hline(yintercept = 0.5, linetype="dashed", color="red")+ 
    ggtitle("",subtitle = "") +
    xlab("") + ylab("")+
    scale_y_continuous(labels = scales::percent)+
    theme_clean()+
    theme(legend.position = "bottom") +
    guides(color=guide_legend(nrow=2,byrow=TRUE))
  
}

kumeleme_cog_gor <- function(x, harita){
  if (!require(ggthemes)) install.packages("ggthemes", 
                                           dependencies = TRUE)
  if (!require(ggplot2)) install.packages("ggplot2", 
                                          dependencies = TRUE) 
  temp <- harita  %>%  left_join(x)
  ggplot(temp) + geom_sf(aes(fill=CAT)) +
    geom_sf_text(mapping = aes(label=name_tr),size=1.25, 
                 alpha = .7,check_overlap = TRUE) +
    xlab("")+
    ylab("")+ 
    #scale_fill_distiller(palette = "YlOrBr")+
    theme(legend.position="right")+ #plot.margin=grid::unit(c(0,0,0,0), "mm"),
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    theme(plot.background = element_rect(color = "black"),legend.position = "bottom")
}


frekans_plot <- function(dt,var,lab1="",lab2 = "Frekans") {
  if (!require(ggthemes)) install.packages("ggthemes", 
                                           dependencies = TRUE)
  if (!require(ggplot2)) install.packages("ggplot2", 
                                          dependencies = TRUE) 
  dt  %>% 
    count({{ var }}) %>% 
    ggplot(aes(x = reorder({{ var }}, n), y= n, fill = {{ var }})) +
    labs(x = lab1,
         y = lab2) +
    geom_bar(stat = "identity", show.legend = FALSE)+
    geom_label(aes(label=n), position=position_dodge(width=1), vjust=+1.2, angle=90)+
    theme_clean()+
    theme(text = element_text(size = 6))+
    theme(legend.position="none")+
    coord_flip()
}

normallik <- function(x,var) {
  if (!require(tseries)) install.packages("tseries", 
                                           dependencies = TRUE)
  x <- x  %>% group_by({{ var }})  %>% 
    summarise(ortalama = mean(OV), ortanca = median(OV),
              stn.hata = sd(OV), çarpıklık = psych::skew(OV),
              basıklık = psych::kurtosi(OV),
              jb.serbestlik = jarque.bera.test(OV)$parameter,
              jb.istatistik =jarque.bera.test(OV)$statistic,
              jb.pdegeri = jarque.bera.test(OV)$p.value) 
  return(x)
}

verim_permutasyon <- function(x,var, label = "", seed=2023){
  if (!require(ggthemes)) install.packages("ggthemes", 
                                           dependencies = TRUE)
  if (!require(ggplot2)) install.packages("ggplot2", 
                                          dependencies = TRUE)
  if (!require("rcompanion")) install.packages("rcompanion",
                                               dependencies = TRUE)
  if (!require("coin")) install.packages("coin",
                                         dependencies = TRUE)
  if (!require("forcats")) install.packages("forcats",
                                            dependencies = TRUE)
  options(OutDec= ".")
  set.seed(seed)
  means <- x  %>% 
    group_by({{ var }})  %>% 
    summarize(ORT = mean(OV, na.rm = TRUE)) 
  x <- x  %>%  left_join(means)  %>% 
    mutate( var1  = fct_reorder({{ var }}, ORT, .desc = TRUE))  %>% 
    ungroup()   %>%  dplyr::select(-ORT)
  x  %>% group_by( var1 )  %>% summarise(mean(OV)) %>% print()
  oneway <- oneway_test(OV ~ factor( var1), data = x,
              distribution = approximate(nresample = 10000)) 
  print(oneway)
  posthoc <-  pairwisePermutationTest(OV ~ var1,
                                      data = x, 
                                      distribution = approximate(nresample = 10000),
                                      method = "BY")
  print(posthoc)
  postList <- cldList(comparison = posthoc$Comparison,
                      p.value = posthoc$p.adjust,
                      threshold = 0.05)  %>% 
    left_join( x  %>% 
                 mutate( var1 = str_remove_all(string = var1, pattern = " "))  %>% 
                 group_by(var1)  %>% 
                 summarize(Ortalama = mean(OV,na.rm = TRUE), 
                           IQR1 = quantile(x = OV, probs = 0.25),
                           IQR3 = quantile(x = OV,probs = .75)),
               by = c("Group"= "var1" ))
  options(OutDec= ",")
  postList  %>% 
    ggplot(aes(x = fct_reorder(Group,Ortalama,.desc = FALSE), y= Ortalama , color = Letter ))+
    geom_linerange(aes(ymin = IQR1, ymax = IQR3), linewidth = 2, alpha = 0.85)+
    geom_label(aes(label = Letter), color="darkblue",size=2.5, vjust=-0.5)+
    geom_text(aes(label = round(Ortalama,0)),size=2.2, vjust = 2)+
    geom_point(size=3, color = "blue", alpha = .7)+
    theme_clean()+ 
    xlab(label)+ylab("Ortalama Verim")+
    theme(text = element_text(size = 6))+
    theme(legend.position="none")+
    coord_flip()
}

box_plot <- function(x,var,label = "") {
  if (!require(ggthemes)) install.packages("ggthemes", 
                                           dependencies = TRUE)
  if (!require(ggplot2)) install.packages("ggplot2", 
                                          dependencies = TRUE)  
  means <- x  %>% 
    group_by({{ var }})  %>% 
    summarize(ORT = mean(OV, na.rm = TRUE)) 
  x <- x  %>%  left_join(means)  %>% 
    mutate( var1  = fct_reorder({{ var }}, ORT, .desc = FALSE))  %>% 
    ungroup()   %>%  dplyr::select(-ORT)
  x  %>%
    ggplot(aes(x = var1, y = OV))+
    geom_boxplot(aes(fill = {{ var }}),alpha = .5)+
    theme_clean()+
    xlab(label)+ylab("Ortalama Verim")+
    theme(text = element_text(size = 6))+
    theme(legend.position="none")+
    coord_flip()
}

############################################################
#   ÇOKLU DOĞRUSAL REGRESYON VARSAYIM KONTROL GRAFİKLERİ   #
############################################################

varsayim_plot <- function(x) {
  if (!require(ggfortify)) install.packages("ggfortify", 
                                            dependencies = TRUE)
  autoplot(x, geom = "tile")+
    theme_clean()+
    theme(axis.text=element_text(size=4),
          axis.title=element_text(size=8),
          title = element_text(size = 4))
}

bootstrap_se <- function(x) {
  if (!require(car)) install.packages("car",
                                      dependencies = TRUE) 
  bhat <-  Boot(x)
  a <- S(x,vcov.=vcov(bhat))
  print(a)
}

