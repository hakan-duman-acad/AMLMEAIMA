library(bibliometrix)
library(tidyverse)
library(ggthemes)
library(knitr)
library(kableExtra)
if (!require(Cairo)) install.packages("Cairo", dependencies = TRUE)

M <- read_rds(file = "data/milkmlbib.rds")

results <- biblioAnalysis(M, sep = ";")
results$MostCitedPapers[1:15, ]
CR <- localCitations(M, sep = ";")
cnames <- c("Çalışma", "Toplam Yerel Atıf", "Toplam Global Atıf")

crTemp %>%
  mutate(Paper = cites) %>%
  select(Paper, LCS, GCS) %>%
  kable(
    caption = "Süt veriminin makine öğrenmesi modelleri ile araştırılmasına ait bibliyografik veri setinde en çok yerel atıf alan çalışmalar",
    format.args = list(decimal.mark = ",", big.mark = "."),
    label = "bib-milk-ml-lc",
    escape = FALSE,
    row.names = FALSE,
    col.names = cnames,
    booktabs = TRUE,
    digits = 0,
    format = "latex"
  ) %>%
  kable_styling(latex_options = "striped")

crTemp %>%
  mutate(Paper = cites) %>%
  select(Paper, LCS, GCS)
CR$Papers[1:15, ]

CR <- citations(M, field = "author", sep = ";")
a <- cbind(CR$Cited[1:10])

CR <- localCitations(M, sep = ";")
a <- CR$Authors[1:10, ]

DF <- dominance(results, k = 10)
DF

authors <- gsub(",", " ", names(results$Authors)[1:5])

indices <- Hindex(M, field = "author", elements = authors, sep = ";", years = 50)

indices$H

topAU <- authorProdOverTime(M, k = 10, graph = TRUE)

topAU$graph + ggtitle("") + xlab("Yazar") + ylab("Yıl") +
  guides(
    size = guide_legend(order = 1, "Makale Sayısı"),
    alpha = guide_legend(order = 2, "Yıllık atıf")
  )

fname <- "milk-ml-author-over-time"
ggsave(
  filename = paste0("./images/", fname, ".pdf"),
  width = 6,
  height = 8,
  device = cairo_pdf
)
L <- lotka(results)
L$Beta
L$C
L$R2
L$p.value
Observed <- L$AuthorProd[, 3]
Theoretical <- 10^(log10(L$C) - 2 * log10(L$AuthorProd[, 1]))
plot(L$AuthorProd[, 1], Theoretical, type = "l", col = "red", ylim = c(0, 1), xlab = "Articles", ylab = "Freq. of Authors", main = "Scientific Productivity")
lines(L$AuthorProd[, 1], Observed, col = "blue")
legend(x = "topright", c("Theoretical (B=2)", "Observed"), col = c("red", "blue"), lty = c(1, 1, 1), cex = 0.6, bty = "n")

A <- cocMatrix(M, Field = "SO", sep = ";")


a <- sort(Matrix::colSums(A), decreasing = TRUE)[1:10]

dt <- tibble(Dergi = names(a), Sayi = a) %>%
  mutate(Dergi = str_to_title(Dergi))
dt %>%
  ggplot(aes(y = Sayi, x = reorder(Dergi, Sayi))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_clean() +
  xlab("") +
  ylab("Yayın Sayısı")
fname <- "milk-ml-journals"
ggsave(
  filename = paste0("./images/", fname, ".pdf"),
  width = 6,
  height = 6,
  device = cairo_pdf
)
b <- bradford(M)
b$table %>% filter(Zone == "Zone 1")
b$graph + ggtitle("")
a <- b$table %>% filter(Zone == "Zone 1")
apply(a, 1, function(x) {
  paste0(
    str_to_title(x[1]), " ($n=", x[3], "$), "
  )
}) %>%
  paste(., collapse = " ") %>%
  clipr::write_clip(allow_non_interactive = TRUE)


sort(Matrix::colSums(A), decreasing = TRUE)[1:15]

A <- cocMatrix(M, Field = "CR", sep = ".  ")
sort(Matrix::colSums(A), decreasing = TRUE)[1:5]
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
netstat <- networkStat(NetMatrix)
net <- networkPlot(NetMatrix,
  n = 75, Title = "Anahtar kelime eş",
  size = TRUE, remove.multiple = FALSE,
  labelsize = 0.3
)
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
net <- networkPlot(NetMatrix,
  n = dim(NetMatrix)[1], Title = "Country Collaboration",
  size = TRUE, remove.multiple = FALSE,
  labelsize = 0.7, cluster = "none"
)

NetMatrix <- biblioNetwork(M,
  analysis = "co-citation", network = "references", sep = ";",
  short = TRUE, shortlabel = FALSE
)
fname <- "bib-milkml-co-citation-nt"
Cairo(
  file = paste0("./images/", fname, ".pdf"), type = "pdf",
  bg = "transparent", canvas = "white", units = "in", width = 6,
  height = 8
)
net <- networkPlot(NetMatrix,
  n = nrow(M), label.cex = TRUE, label.color = FALSE,
  Title = "", label.n = 20, halo = FALSE,
  type = "fruchterman", noloops = TRUE,
  size.cex = TRUE, remove.multiple = FALSE,
  labelsize = 0.25, size = 5, # edges.min = 3,
  edgesize = 3, alpha = .5
)
dev.off()
a1 <- net$cluster_res %>%
  arrange(cluster, desc(pagerank_centrality)) %>%
  group_by(cluster) %>%
  slice(1:5)

M <- metaTagExtraction(M, Field = "CR_AU", sep = ";")

fname <- "bib-milkml-references-coupling"
Cairo(
  file = paste0("./images/", fname, ".pdf"), type = "pdf",
  bg = "transparent", canvas = "white", units = "in", width = 6,
  height = 8
)
NetMatrix <- biblioNetwork(M,
  analysis = "coupling", network = "references", sep = ";", short = TRUE,
  shortlabel = FALSE
)
net <- networkPlot(NetMatrix,
  normalize = "association",
  label.n = 40, # halo=TRUE,
  weighted = T, n = 150,
  Title = "",
  type = "fruchterman",
  size = TRUE, edgesize = 3,
  labelsize = 0.3
)
dev.off()

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
Cairo(
  width = 6, height = 6, "./images/bib-milkml-keyword-co-occur.pdf",
  type = "pdf", bg = "transparent", canvas = "white", units = "in"
)
net <- networkPlot(NetMatrix,
  normalize = "association",
  weighted = T, n = 75,
  Title = "",
  type = "fruchterman",
  size = T, edgesize = 5,
  labelsize = 0.3
)
dev.off()


CS <- conceptualStructure(M,
  field = "ID",
  method = "MCA",
  minDegree = 4,
  clust = "auto",
  stemming = FALSE,
  labelsize = 10,
  documents = 10
)

options(width = 130)
histResults <- histNetwork(M, min.citations = 0, sep = ";")
net <- histPlot(histResults, n = 30, size = 10, labelsize = 2)


net$g + ggtitle("")
ggsave(
  filename = "./images/milkml-hist.pdf",
  width = 6,
  height = 4,
  device = cairo_pdf
)

res <- thematicMap(M,
  field = "ID",
  n = nrow(M) * 1,
  minfreq = 10,
  size = 0.5,
  n.labels = 1, repel = TRUE,
  synonyms = c(
    "ketosis;clinical ketosis;subclinical ketosis",
    "lameness;clinical lameness", "energy-balance;energy",
    "artificial neural networks;artificial neural network;artificial neural-network;artificial neural networks;neural-network;neural-networks",
    "mastitis;clinical mastitis", "dairy manure;manure", "milk-yield;milk-production;yield;performance",
    "digestion;digestibility", "ruminal fermentation;fermentation"
  ),
  remove.terms = c(
    "cows", "cow", "dairy-cows", "cattle", "milk", "dairy-cattle", "beef-cattle",
    "sheep", "heifers", "dairy", "lactating dairy-cows",
    "samples", "holstein", "cows milk", "bovine-milk", "technical-note", "calf"
  )
)
theme <- plot(res$map)
theme +
  xlab(label = "İlgi derecesi \n (Merkezilik)") +
  ylab(label = "Gelişim derecesi \n (Yoğunluk)") +
  theme_clean() +
  theme(legend.position = "none")
fname <- "milkml-themes-all"
ggsave(
  filename = paste0("./images/", fname, ".pdf"),
  width = 6,
  height = 4,
  device = cairo_pdf
)
Clusters <- res$words[order(res$words$Cluster, -res$words$Occurrences), ]
library(dplyr)
CL <- Clusters %>%
  group_by(.data$Cluster_Label) %>%
  top_n(10, .data$Occurrences)
CL %>% count(Cluster)
