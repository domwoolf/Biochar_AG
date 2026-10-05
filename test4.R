library(kableExtra)
tab <- data.frame(A="CO\\textsubscript{2} Transport \\\\& Storage")
k <- kbl(tab, format="latex", escape=FALSE)
print(k)
