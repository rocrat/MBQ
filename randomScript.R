library(compositions)
col1r <- read_excel("./RawData/OldData/Roche Column 1 Parsed Data 30-JUN-2015_QualityControlled.xlsx", skip = 7, sheet = 1, col_names = FALSE)
names(col1r) <- make.names(gsub("run244\\-(TRSPZ\\d{6})_1", "\\1", col1r[3, ]), unique = TRUE)
pnames <- col1r$Sample.Name[-c(1:4)]
col1r <- data.matrix(col1r[-c(1:4), -1])
rownames(col1r) <- pnames

col1.r <- col1.r[sameProbes,]
p2raw <- t(data.matrix(col1.r))
colnames(p2raw) <- col1[-c(1:4), ]$Sample.Name
p2raw <- p2raw[which(grepl("TRSPZ", rownames(p2raw))), ]
vmat <- variation(acomp(p2raw))
#useless image(vmat)
vmats <- variation(acomp(p2raw[1:4, 1:4]))
totals <- rowSums(vmat)