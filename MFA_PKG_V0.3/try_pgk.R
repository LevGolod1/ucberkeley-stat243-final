### Try installing and using the R package
### NB: if you're having a problem with seeing the documentation,
#     close R and re-open it. This problem arrises when you re-build
#     the package, I think.

# setwd("/Users/TAEHEEJUNG/Desktop/STAT243_HW/v2_new/mfaMKTLT")

install.packages("../mfaMKTLT_0.3.tar.gz", repos = NULL, type = "source")

# install.packages("/Users/TAEHEEJUNG/Desktop/STAT243_HW/MFA_PKG_V0.3/mfaMKTLT_0.3.tar.gz", repos = NULL, type = "source")
library(mfaMKTLT)

# View documentation:
help(package = mfaMKTLT)
?mfa
?plot.mfa
?winedata
?bootstrap
# view data
head(winedata)

# copy 'sets' from mfa documentation examples
sets.num <- list(c(1:6), c(7:12), c(13:18), c(19:23), c(24:29), c(30:34),
                 c(35:38), c(39:44), c(45:49), c(50:53))
sets.char <- list(c('V1','V2','V3','V4','V5','V6'),
                  c('V1.1','V2.1','V3.1','V4.1','V7','V8'),
                  c('V1.2','V2.2','V3.2','V4.2','V9','V10'),
                  c('V1.3','V2.3','V3.3','V4.3','V8.1'),
                  c('V1.4','V2.4','V3.4','V4.4','V11','V12'),
                  c('V1.5','V2.5','V3.5','V4.5','V13'),
                  c('V1.6','V2.6','V3.6','V4.6'),
                  c('V1.7','V2.7','V3.7','V4.7','V14','V5.1'),
                  c('V1.8','V2.8','V3.8','V4.8','V15'),
                  c('V1.9','V2.9','V3.9','V4.9'))
sets.mixed <- c(sets.num[1:4], sets.char[5:10])

## do MFA analysis
mfa1 <- mfa(winedata, sets.mixed, ncomps = NULL, center = TRUE,
            scale = 'vector.norm')

# Alpha table weights
round(mfa1$alpha, 3)

# Eigenvalues
round(mfa1$eigvals, 3)

# Common factor scores
round(mfa1$Fcommon[1:5,1:5], 3)

# Partial factor scores - subtable 1
round(mfa1$Fpartial[[1]][1:5,1:5], 3)

# Loadings
round(mfa1$Q[1:5,1:5], 3)

# --------------------------------------------------

# Eigenvalue summary table
summary <- eigsum(mfa1)
round(summary, 3)[1:5,1:5]

# contributions
obs <- obscont(mfa1)
var <- varcont(mfa1)
tab <- tabcont(mfa1)
round(summary, 3)[1:5,1:5]
lapply(list(obs = obs, var = var, tab = tab), function(x) round(x, 3)[1:6,1:5])

# Between-table coeffs
rv_coeff(mfa1$subtable[[1]], mfa1$subtable[[2]])

lg_coeff(mfa1$subtable[[1]], mfa1$subtable[[1]])
lg_coeff(mfa1$subtable[[2]], mfa1$subtable[[2]])
lg_coeff(mfa1$subtable[[3]], mfa1$subtable[[3]])

lg_coeff(mfa1$subtable[[1]], mfa1$subtable[[2]])
lg_coeff(mfa1$subtable[[1]], mfa1$subtable[[3]])
lg_coeff(mfa1$subtable[[2]], mfa1$subtable[[3]])

# test <- cbind(mfa1$subtable[[1]], mfa1$subtable[[2]], mfa1$subtable[[3]])
# gentable(test, sets.num[1:3],
#          paste0("sub-table_", 1:3), func = lg_coeff)
lg_coeff(mfa1$data_scale[,1:6], mfa1$data_scale[,1:6])
gentable(mfa1$data_scale, as.list(c(sets.num[1], sets.char[2:3])),
         paste0("sub-table_", 1:3), func = lg_coeff)


lg_coeff(mfa1$origdata, mfa1$origdata)

lg_coeff(as.matrix(mfa1$origdata[[1]]), as.matrix(mfa1$origdata[[1]]))
lg_coeff(as.matrix(mfa1$subtable[[1]]), as.matrix(mfa1$subtable[[1]]))

gentable(mfa1$origdata, as.list(c(sets.num[1], sets.char[2:3])),
          paste0("sub-table_", 1:3), func = lg_coeff)



# Different types of plots:
#1. Eigenvalues
plot(mfa1, type = "eigenvalues", subtabs = c(1,2,3,4,5,6),
     legend = c("cat pee", "passion fruit", "green pepper", "mineral","optional 1", "optional 2"))
plot(mfa1, type = "eigenvalues")

#2. Compromise
plot(mfa1, type = "compromise")
plot(mfa1, type = "compromise", legend=substr(rownames(mfa1$Fcommon),1,2), subtabs = c(1,2,3))
plot(mfa1, type = "compromise", legend=substr(rownames(mfa1$Fcommon),1,2),
     label=substr(rownames(mfa1$Fcommon),3,3))

#3. Partial_factor
plot(mfa1, type = "partial.factor", size = 2, xdim=2, ydim=4)
plot(mfa1, type = "partial.factor", subtabs = c(1,2,3), xdim = 1, ydim = 2, size = 4,
     legend=substr(rownames(mfa1$Fpartial[[1]]),1,2))
plot(mfa1, type = "partial.factor", subtabs = c(1,3,5,7,10,8), xdim = 1, ydim = 2, size = 4,
     label=substr(rownames(mfa1$Fcommon),3,3))
plot(mfa1, type = "partial.factor", subtabs = c(1,3,5,7,10,8), xdim = 1, ydim = 2, size = 4,
     legend=substr(rownames(mfa1$Fpartial[[1]]),1,2), label=substr(rownames(mfa1$Fcommon),3,3))

#4. Compromise_Partial
plot(mfa1, type = "compromise.partial", xdim = 1, ydim = 10)
plot(mfa1, type = "compromise.partial", xdim = 4, ydim = 5, size=5,
     label=substr(rownames(mfa1$Fcommon),3,3))
plot(mfa1, type = "compromise.partial", xdim = 1, ydim = 2,
     legend=substr(rownames(mfa1$Fcommon),1,2), label=substr(rownames(mfa1$Fcommon),3,3))

#5. Loadings
plot(mfa1, type = "loadings", subtabs = c(7,2,5,3,8,1), size=6)
plot(mfa1, type = "loadings", subtabs = c(1,2,3,4,5,6), legend = c("cat pee", "passion fruit", "green pepper", "mineral","optional 1", "optional 2"))

#6. Bootstrap ratio plots
plot(mfa1, type= "bootstrap", bootstrap_size = 1000, bootstrap_comps=c(1,2), facetrows=1)

# Bootstraps:
bt_test <- bootstrap(mfa1$Fpartial, 1000)
