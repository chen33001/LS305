setwd("C:\\R\\中醫體質_GWAS\\痰盂_GWAS")
het <- read.table("het.het", header = TRUE)
het$meanHet <- (het$N.NM. - het$O.HOM.) / het$N.NM.
upplimit <- mean(het$meanHet) + (3 * sd(het$meanHet))
lowlimit <- mean(het$meanHet) - (3 * sd(het$meanHet))
remove <- het[which(het$meanHet < lowlimit | het$meanHet > upplimit), c("FID", "IID")]
write.table(remove, "fail-het-qc.txt", append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = FALSE)
