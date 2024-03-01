#code to make the coefficient plots from the multivariate models for assay and axy principal components (PC1, PC2)
#last edited Feb 29, 2024 by A. R. Martinig

library(lattice)
library(ggplot2)
library(stats)
library(cowplot)

#response variables
## name your predicted factor latent.mean, and the CI between latent.lower and latent.upper
latent.mean <- apply(c1,2, mean)
latent.lower <- apply(c1, 2, function(x) quantile(x, probs = c(0.025), na.rm=TRUE))
latent.upper <- apply(c1, 2, function(x) quantile(x, probs = c(0.975), na.rm=TRUE))
subject <- c("OFT1:OFT1", "OFT2:OFT1", "PC1:OFT1", "PC2:OFT1", "OFT1:OFT2", "OFT2:OFT2", "PC1:OFT2", "PC2:OFT2", "OFT1:PC1", "OFT2:PC1", "PC1:PC1", "PC2:PC1", "OFT1:PC2", "OFT2:PC2", "PC1:PC2", "PC2:PC2")
  
dat <- data.frame(latent.mean, latent.lower, latent.upper, subject)

## here, take only 50 of the 1500 observations (just for demonstration)
plot.dat <- dat[sample(1:nrow(dat), 1000, replace=TRUE),]

## order the observation IDs as well (seems redundant, but is necessary)
plot.dat$subject2 <- reorder(plot.dat$subject, plot.dat$latent.mean)

plot.dat<-plot.dat%>%filter(subject %in% c("OFT1:OFT2", "OFT1:PC1", "OFT1:PC2", "OFT2:PC1", "OFT2:PC2"))

plot.dat$subject2 <- factor(plot.dat$subject2, levels=c("OFT2:PC2", "OFT2:PC1", "OFT1:PC2", "OFT1:PC1", "OFT1:OFT2"))

## make plot using the ggplot2 package (not as nice as lattice plot):
A<-ggplot(plot.dat, 
	aes(x = latent.mean, y = subject2)) + 
	geom_vline(xintercept = 0, linetype="solid", 
                color = "gray", size=0.75) +	
	geom_segment(aes(x = latent.lower, 
		xend = latent.upper, 
		y = subject2, 
		yend = subject), size=1) +
	geom_point(shape=16,cex=4, color="black") + 
	scale_x_continuous(breaks = scales::pretty_breaks(6)) +
	scale_y_discrete(breaks = c("OFT1:OFT2", "OFT1:PC1", "OFT1:PC2", "OFT2:PC1", "OFT2:PC2"),
			labels = c("Assay BA1 \n& Assay BA2", "Assay BA1 \n& Axy BA1", "Assay BA1 \n& Axy BA2", "Assay BA2 \n& Axy BA1", "Assay BA2 \n& Axy BA2"),
			expand=c(0,0.5,0,0.5)) + #if you want to change the spacing between ticks
	xlab("Estimate ± 95% credible intervals") + 
	ylab("Among-individual correlations") +
	theme_bw() +
	theme(axis.line = element_line(colour = "black"),
        axis.text=element_text(size=15), #changes size of axes #s
        axis.title=element_text(size=15), #changes size of axes labels
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 15))
 A      


## name your predicted factor latent.mean, and the CI between latent.lower and latent.upper
latent.mean2 <- apply(c2,2, mean)
latent.lower2 <- apply(c2, 2, function(x) quantile(x, probs = c(0.025), na.rm=TRUE))
latent.upper2 <- apply(c2, 2, function(x) quantile(x, probs = c(0.975), na.rm=TRUE))
subject3 <- c("OFT1:OFT1", "OFT2:OFT1", "PC1:OFT1", "PC2:OFT1", "OFT1:OFT2", "OFT2:OFT2", "PC1:OFT2", "PC2:OFT2", "OFT1:PC1", "OFT2:PC1", "PC1:PC1", "PC2:PC1", "OFT1:PC2", "OFT2:PC2", "PC1:PC2", "PC2:PC2")

dat2 <- data.frame(latent.mean2, latent.lower2, latent.upper2, subject3)

## here, take 1000 observations (just for demonstration)
plot.dat2 <- dat2[sample(1:nrow(dat2), 1000, replace=TRUE),]

## order the observation IDs as well (seems redundant, but is necessary)
plot.dat2$subject4 <- reorder(plot.dat2$subject3, plot.dat2$latent.mean2)

plot.dat2<-plot.dat2%>%filter(subject3 %in% c("OFT1:OFT2", "OFT1:PC1", "OFT1:PC2", "OFT2:PC1", "OFT2:PC2"))

plot.dat2$subject4 <- factor(plot.dat2$subject4, levels=c("OFT2:PC2", "OFT2:PC1", "OFT1:PC2", "OFT1:PC1", "OFT1:OFT2"))

   

## make plot using the ggplot2 package (not as nice as lattice plot):
B<-ggplot(plot.dat2, 
	aes(x = latent.mean2, y =subject4)) + 
	geom_vline(xintercept = 0, linetype="solid", 
                color = "gray", size=0.75) +	
	geom_segment(aes(x = latent.lower2, 
		xend = latent.upper2, 
		y = subject4, 
		yend = subject4), size=1) +
	geom_point(shape=16,cex=4, color="black") +  
	scale_x_continuous(breaks = scales::pretty_breaks(4)) +
	scale_y_discrete(breaks = c("OFT1:OFT2", "OFT1:PC1", "OFT1:PC2", "OFT2:PC1", "OFT2:PC2"),
			labels = c("Assay BA1 \n& Assay BA2", "Assay BA1 \n& Axy BA1", "Assay BA1 \n& Axy BA2", "Assay BA2 \n& Axy BA1", "Assay BA2 \n& Axy BA2"),
			expand=c(0,0.5,0,0.5)) +
	xlab("Estimate ± 95% credible intervals") + 
	ylab("Within-individual correlations") +
	theme_bw() +
	theme(axis.line = element_line(colour = "black"),
        axis.text=element_text(size=15), #changes size of axes #s
        axis.title=element_text(size=15), #changes size of axes labels
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 15))
B

plot_grid(A, B, labels=c("(a)", "(b)"), ncol = 2, nrow =1, align = "hv", label_x=0.89, label_y=1)       



## figure for fixed effects
latent.mean <- apply(mod.1$Sol[,5:24],2,mean)
latent.lower <- apply(mod.1$Sol[,5:24], 2, function(x) quantile(x, probs = c(0.025)))
latent.upper <- apply(mod.1$Sol[,5:24], 2, function(x) quantile(x, probs = c(0.975)))
subject <- c("traitOFT1:sexM", 
			"traitOFT2:sexM", 
			"traitPC1:sexM",
			"traitPC2:sexM",
		"traitOFT1:age", 
		"traitOFT2:age", 
		"traitPC1:age",
		"traitPC2:age", 
				#"traitOFT1:age2", 
				#"traitOFT2:age2", 
				#"traitPC1:age2",
				#"traitPC2:age2", 
					"traitOFT1:local.density", 
					"traitOFT2:local.density", 
					"traitPC1:local.density",
					"traitPC2:local.density",  
		"traitOFT1:avg_fam", 
		"traitOFT2:avg_fam", 
		"traitPC1:avg_fam",
		"traitPC2:avg_fam",  		
				"traitOFT1:date", 
				"traitOFT2:date", 
				"traitPC1:date",
				"traitPC2:date")


dat <- data.frame(latent.mean, latent.lower, latent.upper, subject)

## here, take 1000 observations (just for demonstration)
plot.dat <- dat[sample(1:nrow(dat), 1000, replace=TRUE),]

## order the observation IDs as well (seems redundant, but is necessary)
plot.dat$subject2 <- reorder(plot.dat$subject, plot.dat$latent.mean)


#OFT1 plot only
plot.dat2<-plot.dat%>%filter(subject %in% c("traitOFT1:sexM",  "traitOFT1:age", "traitOFT1:local.density", "traitOFT1:avg_fam", "traitOFT1:date"))

plot.dat2$subject2 <- factor(plot.dat2$subject2, levels=c("traitOFT1:sexM",  "traitOFT1:age", "traitOFT1:local.density", "traitOFT1:avg_fam", "traitOFT1:date"))

C<-ggplot(plot.dat2, 
	aes(x = latent.mean, y = subject2)) + 
	geom_vline(xintercept = 0, linetype="solid", 
                color = "gray", size=0.75) +	
	geom_segment(aes(x = latent.lower, 
		xend = latent.upper, 
		y = subject2, 
		yend = subject2), size=0.3) +
	geom_point(shape=16,cex=2, color="black") + 
	#geom_point(shape=16,cex=5, color="white") + 
	#geom_point(shape=16,cex=3, color="#CC79A7") + 
	scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5, 1), position="top", limits = c(-1, 1), , expand = c(0,0)) +
	scale_y_discrete(
	breaks = c("traitOFT1:sexM",  "traitOFT1:age", "traitOFT1:local.density", "traitOFT1:avg_fam", "traitOFT1:date"),
			labels = c("Sex", "Age","Julian date", "Density", "Familiarity")) +
	xlab("Assay behavioural axis 1") + 
	ylab("") +
	theme_bw() +
	theme(plot.margin = margin(0, 0.5, 0, 0, "cm"),
		axis.line=element_line(),
		axis.line.y=element_blank(),
		axis.ticks.length=unit(0.4, "cm"),
		axis.ticks.y=element_blank(),
        axis.text=element_text(size=10), #changes size of axes #s
        axis.title=element_text(size=15), #changes size of axes labels
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 10))
C


#OFT2 plot only
plot.dat2<-plot.dat%>%filter(subject %in% c("traitOFT2:sexM",  "traitOFT2:age", "traitOFT2:local.density", "traitOFT2:avg_fam", "traitOFT2:date"))

plot.dat2$subject2 <- factor(plot.dat2$subject2, levels=c("traitOFT2:sexM",  "traitOFT2:age",  "traitOFT2:local.density", "traitOFT2:avg_fam", "traitOFT2:date"))

D<-ggplot(plot.dat2, 
	aes(x = latent.mean, y = subject2)) + 
	geom_vline(xintercept = 0, linetype="solid", 
                color = "gray", size=0.75) +	
	geom_segment(aes(x = latent.lower, 
		xend = latent.upper, 
		y = subject2, 
		yend = subject2), size=0.3) +	
	geom_point(shape=16,cex=2, color="black") + 
	#geom_point(shape=16,cex=5, color="white") + 
	#geom_point(shape=16,cex=3, color="#CC79A7") +
	scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5, 1.0), position="top", limits = c(-1, 1), , expand = c(0,0)) +
	scale_y_discrete(
	breaks = c("traitOFT2:sexM",  "traitOFT2:age", "traitOFT2:local.density", "traitOFT2:avg_fam", "traitOFT2:date"),
			labels = c("Sex", "Age", "Julian date", "Density", "Familiarity")) +
	xlab("Assay behavioural axis 2") + 
	ylab("") +
	theme_bw() +
	theme(plot.margin = margin(0, 0.5, 0, 0, "cm"),
		axis.line=element_line(),
		axis.line.y=element_blank(),
		axis.ticks.length=unit(0.4, "cm"),
		axis.ticks.y=element_blank(),
        axis.text=element_text(size=10), #changes size of axes #s
        axis.title=element_text(size=15), #changes size of axes labels
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 10))
D


#PC1 plot only
plot.dat2<-plot.dat%>%filter(subject %in% c("traitPC1:sexM",  "traitPC1:age", "traitPC1:local.density", "traitPC1:avg_fam", "traitPC1:date"))

plot.dat2$subject2 <- factor(plot.dat2$subject2, levels=c("traitPC1:sexM",  "traitPC1:age", "traitPC1:local.density", "traitPC1:avg_fam", "traitPC1:date"))

E<-ggplot(plot.dat2,  
	aes(x = latent.mean, y = subject2)) + 
	geom_vline(xintercept = 0, linetype="solid", 
                color = "gray", size=0.75) +	
	geom_segment(aes(x = latent.lower, 
		xend = latent.upper, 
		y = subject2, 
		yend = subject2), size=0.3) +
	geom_point(shape=16,cex=2, color="black") + 
	#geom_point(shape=16,cex=5, color="white") + 
	#geom_point(shape=16,cex=3, color="#CC79A7") +
	scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5, 1), position="top", limits = c(-1, 1), , expand = c(0,0)) +
	scale_y_discrete(
	breaks = c("traitPC1:sexM",  "traitPC1:age", "traitPC1:local.density", "traitPC1:avg_fam", "traitPC1:date"),
			labels = c("Sex", "Age", "Julian date", "Density", "Familiarity")) +
	xlab("Axy behavioural axis 1") + 
	ylab("") +
	theme_bw() +
	theme(plot.margin = margin(0, 0.5, 0, 0, "cm"),
		axis.line=element_line(),
		axis.line.y=element_blank(),
		axis.ticks.length=unit(0.4, "cm"),
		axis.ticks.y=element_blank(),
        axis.text=element_text(size=10), #changes size of axes #s
        axis.title=element_text(size=15), #changes size of axes labels
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 10))
E


#PC2 plot only
plot.dat2<-plot.dat%>%filter(subject %in% c("traitPC2:sexM",  "traitPC2:age", "traitPC2:local.density", "traitPC2:avg_fam", "traitPC2:date"))

plot.dat2$subject2 <- factor(plot.dat2$subject2, levels=c("traitPC2:sexM",  "traitPC2:age", "traitPC2:local.density", "traitPC2:avg_fam", "traitPC2:date"))

F<-ggplot(plot.dat2, 
	aes(x = latent.mean, y = subject2)) + 
	geom_vline(xintercept = 0, linetype="solid", 
                color = "gray", size=0.75) +	
	geom_segment(aes(x = latent.lower, 
		xend = latent.upper, 
		y = subject2, 
		yend = subject2), size=0.3) +
	geom_point(shape=16,cex=2, color="black") + 
	#geom_point(shape=16,cex=5, color="white") + 
	#geom_point(shape=16,cex=3, color="#CC79A7") + 
	scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5, 1), position="top", limits = c(-1, 1), , expand = c(0,0)) +
	scale_y_discrete(
	breaks = c("traitPC2:sexM",  "traitPC2:age", "traitPC2:local.density", "traitPC2:avg_fam", "traitPC2:date"),
			labels = c("Sex", "Age", "Julian date", "Density", "Familiarity")) +
	xlab("Axy behavioural axis 2") + 
	ylab("") +
	theme_bw() +
	theme(plot.margin = margin(0, 0.5, 0, 0, "cm"),
		axis.line=element_line(),
		axis.line.y=element_blank(),
		axis.ticks.length=unit(0.4, "cm"),
		axis.ticks.y=element_blank(),
    	axis.text=element_text(size=10), #changes size of axes #s
        axis.title=element_text(size=15), #changes size of axes labels
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 10))
F



plot_grid(C, D, E, F, 
#labels=c("(a)", "(b)", "(c)", "(d)"), 
ncol = 2, nrow =2, align = "hv", label_x=0.29, label_y=1)       