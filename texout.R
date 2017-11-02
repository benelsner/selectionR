#######################################
# Selection project: latex output files


#-------------------------------------
# 1) calibration table

col<-c(beta, theta, sigma, tau)
symbols<-c("$\\beta$", "$\\theta$", "$\\sigma$", "$\\tau$")
table1<-matrix(cbind( symbols, col), length(col), 2)
rownames(table1)<-c("\\ Share good X in utility function", 
                    "\\ Elasticity of subs. goods X and Y",
                    "\\ Elasticity of subs. within X",
                    "\\ Iceberg trade costs")
tab<-xtable(table1)
digits(tab)<-c(0,0,1)
print.xtable(tab, 
  sanitize.rownames.function = identity,
  sanitize.text.function=identity,
  hline.after = NULL, 
  include.rownames=TRUE, 
  include.colnames=FALSE, 
  only.contents=TRUE, 
  type="latex", 
  file="calib1.tex")

symbols<-c("$\\Lambda$", "$L$", "$g_y$", "$g_x$", "$F_x$")
par<-as.matrix(cbind(symbols, rbind(round(lambda,2), round(M,2), gy, gx, Fx)))
rownames(par)<-c("\\ TFP",
                 "\\ Labor force",
                 "\\ Returns to education, sector Y",
                 "\\ Returns to education, sector X",
                 "\\ Fixed cost of production")
tab<-xtable(par)
digits(tab)<-c(0,0,2,2)
print.xtable(tab, 
  sanitize.rownames.function = identity, 
  sanitize.text.function=identity,  
  hline.after = NULL, 
  include.rownames=TRUE, 
  include.colnames=FALSE, 
  only.contents=TRUE, 
  type="latex", 
  file="calib2.tex")


#-------------------------------------
# 2) Baseline model

vars1<-data.matrix(eqvars1[[1]])
rownames(vars1)<-c()
colnames(vars1)<-c()
vars2<-data.matrix(eqvars2[[1]]*100)
rownames(vars2)<-c()
colnames(vars2)<-c()
vars3<-t(data.matrix(eq_expgdp[[1]]*100))
rownames(vars3)<-c()
colnames(vars3)<-c()

baseline<-as.matrix(rbind(vars1, vars2, vars3, zbar[[1]]))
rownames(baseline)<-c("\\ Real GDP pc",
                 "\\ Nominal wages",
                 "\\ Price index",
                 "\\ Share of workers in sector Y (in \\%)",
                 "\\ Share of exports in GDP (in \\%)",
                 "\\ $\\overline{Z}$")
tab<-xtable(baseline)
digits(tab)<-c(0,2,2)
print.xtable(tab, 
             sanitize.rownames.function = identity, 
             sanitize.text.function=identity,  
             hline.after = NULL, 
             include.rownames=TRUE, 
             include.colnames=FALSE, 
             only.contents=TRUE, 
             type="latex", 
             file="baseline.tex")


#-------------------------------------
# 3) main results
  
  vars1<-data.matrix(ch_vars1)
  rownames(vars1)<-c()
  colnames(vars1)<-c()
  vars2<-t(data.matrix(ch_vars2))
  rownames(vars2)<-c()
  colnames(vars2)<-c()
  vars3<-data.matrix(ch_realwage_bin)
  rownames(vars3)<-c()
  colnames(vars3)<-c()
  
  changes1<-as.matrix(rbind(vars1, vars2))
  rownames(changes1)<-c("\\ Real GDP pc",
                        "\\ Nominal wages",
                        "\\ Price index",
                        "\\ Share of workers in sector Y (in \\%)")
  tab<-xtable(changes1)
  digits(tab)<-c(0,2,2)
  print.xtable(tab, 
               sanitize.rownames.function = identity, 
               sanitize.text.function=identity,  
               hline.after = NULL, 
               include.rownames=TRUE, 
               include.colnames=FALSE, 
               only.contents=TRUE, 
               type="latex", 
               file="changes1.tex")
  
  changes2<-as.matrix(vars3)
  rownames(changes2)<-c("\\ Q1",
                        "\\ Q2",
                        "\\ Q3",
                        "\\ Q4",
                        "\\ Q5")
  tab<-xtable(changes2)
  digits(tab)<-c(0,2,2)
  print.xtable(tab, 
               sanitize.rownames.function = identity, 
               sanitize.text.function=identity,  
               hline.after = NULL, 
               include.rownames=TRUE, 
               include.colnames=FALSE, 
               only.contents=TRUE, 
               type="latex", 
               file="changes2.tex")




#-------------------------------------
# 4) sensitivity checks