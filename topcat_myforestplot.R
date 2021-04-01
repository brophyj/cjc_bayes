# my forest plot - need csv in special format could get all the data from bayesmeta function but here used std epi function
library(epiR)

dat <- read.csv("topcat.csv", header = TRUE)
# tic = spironolactone
# clo = placebo

total <- matrix(c(sum(dat$tic_O[1:6]), sum(dat$tic_N[1:6]) - sum(dat$tic_O[1:6]), sum(dat$clo_O[1:6]), sum(dat$clo_N[1:6]) - sum(dat$clo_O[1:6])), nrow = 2, byrow = TRUE)
epi.2by2(dat = as.table(total), method = "cohort.count", 
         conf.level = 0.95, units = 100,
         outcome = "as.columns")

us <- matrix(c(dat$tic_O[1],dat$tic_N[1] - dat$tic_O[1],dat$clo_O[1],dat$clo_N[1] - dat$clo_O[1]), nrow = 2, byrow = TRUE)
epi.2by2(dat = as.table(us), method = "cohort.count", 
         conf.level = 0.95, units = 100,
         outcome = "as.columns")

canada <- matrix(c(dat$tic_O[2],dat$tic_N[2] - dat$tic_O[2],dat$clo_O[2],dat$clo_N[2] - dat$clo_O[2]), nrow = 2, byrow = TRUE)
epi.2by2(dat = as.table(canada), method = "cohort.count", 
         conf.level = 0.95, units = 100,  
         outcome = "as.columns")

argentina <- matrix(c(dat$tic_O[3],dat$tic_N[3] - dat$tic_O[3],dat$clo_O[3],dat$clo_N[3] - dat$clo_O[3]), nrow = 2, byrow = TRUE)
epi.2by2(dat = as.table(argentina), method = "cohort.count", 
         conf.level = 0.95, units = 100,   
         outcome = "as.columns")

brazil <- matrix(c(dat$tic_O[4],dat$tic_N[4] - dat$tic_O[4],dat$clo_O[4],dat$clo_N[4] - dat$clo_O[4]), nrow = 2, byrow = TRUE)
epi.2by2(dat = as.table(brazil), method = "cohort.count", 
         conf.level = 0.95, units = 100,  
         outcome = "as.columns")

russia <- matrix(c(dat$tic_O[5],dat$tic_N[5] - dat$tic_O[5],dat$clo_O[5],dat$clo_N[5] - dat$clo_O[5]), nrow = 2, byrow = TRUE)
epi.2by2(dat = as.table(russia), method = "cohort.count", 
         conf.level = 0.95, units = 100,  
         outcome = "as.columns")

georgia <- matrix(c(dat$tic_O[6],dat$tic_N[6] - dat$tic_O[6],dat$clo_O[6],dat$clo_N[6] - dat$clo_O[6]), nrow = 2, byrow = TRUE)
epi.2by2(dat = as.table(georgia), method = "cohort.count", 
         conf.level = 0.95, units = 100,   
         outcome = "as.columns")

my.pval <- function(x){
  region <- matrix(c(dat$tic_O[x],dat$tic_N[x] - dat$tic_O[x],dat$clo_O[x],dat$clo_N[x] - dat$clo_O[x]), nrow = 2, byrow = TRUE)
  tt <- epi.2by2(dat = as.table(region), method = "cohort.count", 
                 conf.level = 0.95, units = 100,  
                 outcome = "as.columns")
  return(tt$massoc$chisq.strata$p.value)
}

my.pval(1)

#############

data <- read.csv("topcat_plotdata.csv", stringsAsFactors=FALSE)
## Labels defining subgroups are a little indented!
subgps <- c(3,4,7,8,11,12,15,16,19,20,23,24, 27,28,29)
data$Variable[subgps] <- paste("  ",data$Variable[subgps]) 

## Combine the count and percent column
np <- ifelse(!is.na(data$Count), paste(data$Count," (",round(data$Percent,2),")",sep=""), NA)

## The rest of the columns in the table. 
tabletext <- cbind(c("Region","\n",data$Variable), 
                   c("No. of Patients (%)","\n",np), 
                   c("Events\n Spironolactone","\n",data$Spironolactone), 
                   c("Events\n Placebo","\n",data$Placebo), 
                   c("P Value","\n",data$P.Value))

png(file.path("output/topcat_myforestRR.png"),width=960, height=640)
forestplot(labeltext=tabletext, graph.pos=3, 
           mean=c(NA,NA,data$Point.Estimate), 
           lower=c(NA,NA,data$Low), upper=c(NA,NA,data$High),
           title="TOPCAT Geographic Variation",
           xlab="<--Spironolactone Better         Placebo Better-->                    ",
           hrzl_lines=list("2" = gpar(lwd=1, col="#99999922"), 
                           "5" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "6" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "9" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "10" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "13" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "14" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "17" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "18" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "21" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "22" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "25" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "26" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           #"29" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#02552555"),
                           "30" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#02552555"),
                           "31" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#02552555")),
           txt_gp=fpTxtGp(label=gpar(cex=1.25),
                          ticks=gpar(cex=1.1),
                          xlab=gpar(cex = 1.2),
                          title=gpar(cex = 1.2)),
           col=fpColors(box="black", lines="red", zero = "gray50"),
           zero=1, cex=0.9, lineheight = "auto", boxsize=0.5, colgap=unit(6,"mm"),
           lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.4)
dev.off()

# RD 
data <- read.csv("topcat_plotdataRD.csv", stringsAsFactors=FALSE)
## Labels defining subgroups are a little indented!
subgps <- c(3,4,7,8,11,12,15,16,19,20,23,24, 27,28,29)
data$Variable[subgps] <- paste("  ",data$Variable[subgps]) 

## Combine the count and percent column
np <- ifelse(!is.na(data$Count), paste(data$Count," (",round(data$Percent,2),")",sep=""), NA)

## The rest of the columns in the table. 
tabletext <- cbind(c("Region","\n",data$Variable), 
                   c("No. of Patients (%)","\n",np), 
                   c("Events\n Spironolactone","\n",data$Spironolactone), 
                   c("Events\n Placebo","\n",data$Placebo), 
                   c("P Value","\n",data$P.Value))

png(file.path("output/topcat_fig6_myforestRD.png"),width=960, height=640)
forestplot(labeltext=tabletext, graph.pos=3, 
           mean=c(NA,NA,data$Point.Estimate), 
           lower=c(NA,NA,data$Low), upper=c(NA,NA,data$High),
           title="TOPCAT Geographic Variation - Risk difference (RD)",
           xlab="                     <--Spironolactone Better       RD       Placebo Better-->                    ",
           hrzl_lines=list("2" = gpar(lwd=1, col="#99999922"), 
                           "5" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "6" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "9" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "10" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "13" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "14" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "17" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "18" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "21" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "22" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "25" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "26" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           #"29" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#02552555"),
                           "30" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#02552555"),
                           "31" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#02552555")),
           txt_gp=fpTxtGp(label=gpar(cex=1.25),
                          ticks=gpar(cex=1.1),
                          xlab=gpar(cex = 1.2),
                          title=gpar(cex = 1.2)),
           col=fpColors(box="black", lines="red", zero = "gray50"),
           zero=1, cex=0.9, lineheight = "auto", boxsize=0.5, colgap=unit(6,"mm"),
           lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.4)
dev.off()