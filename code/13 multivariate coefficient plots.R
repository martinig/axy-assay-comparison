#code to make the coefficient plots from the multivariate models for assay and axy principal components (PC1, PC2)
#original code by A. R. Martinig
#last edited April 24, 2024 by A. R. Martinig


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
	scale_x_continuous(breaks = scales::pretty_breaks(4)) +
	scale_y_discrete(breaks = c("OFT1:OFT2", "OFT1:PC1", "OFT1:PC2", "OFT2:PC1", "OFT2:PC2"),
			labels = c("Activity &\nexploration", "Activity &\nforaging", "Activity &\n movement", "Exploration\n& foraging", "Exploration &\nmovement"),
			expand=c(0,0.5,0,0.5)) + #if you want to change the spacing between ticks
	xlab("Estimate ± 95% credible intervals") + 
	ylab("Among-individual correlations") +
	theme_squirrel
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
			labels = c("Activity &\nexploration", "Activity &\nforaging", "Activity &\n movement", "Exploration\n& foraging", "Exploration &\nmovement"),
			expand=c(0,0.5,0,0.5)) +
	xlab("Estimate ± 95% credible intervals") + 
	ylab("Within-individual correlations") +
	theme_squirrel

B

plot_grid(A, B, labels=c("(a)", "(b)"), ncol = 2, nrow =1, align = "hv", label_x=0.89, label_y=1)       



## figure for fixed effects
latent.mean <- apply(mod.1$Sol[,5:28],2,mean)
latent.lower <- apply(mod.1$Sol[,5:28], 2, function(x) quantile(x, probs = c(0.025)))
latent.upper <- apply(mod.1$Sol[,5:28], 2, function(x) quantile(x, probs = c(0.975)))
subject <- c("traitOFT1:sexM", 
			"traitOFT2:sexM", 
			"traitPC1:sexM",
			"traitPC2:sexM",
		"traitOFT1:age", 
		"traitOFT2:age", 
		"traitPC1:age",
		"traitPC2:age", 
				"traitOFT1:age2", 
				"traitOFT2:age2", 
				"traitPC1:age2",
				"traitPC2:age2", 
					"traitOFT1:date", 
					"traitOFT2:date", 
					"traitPC1:date",
					"traitPC2:date",
				"traitOFT1:local.density", 
				"traitOFT2:local.density", 
				"traitPC1:local.density",
				"traitPC2:local.density",  
		"traitOFT1:avg_fam", 
		"traitOFT2:avg_fam", 
		"traitPC1:avg_fam",
		"traitPC2:avg_fam")


dat <- data.frame(latent.mean, latent.lower, latent.upper, subject)

## here, take 1000 observations (just for demonstration)
plot.dat <- dat[sample(1:nrow(dat), 1000, replace=TRUE),]

## order the observation IDs as well (seems redundant, but is necessary)
plot.dat$subject2 <- reorder(plot.dat$subject, plot.dat$latent.mean)


#OFT1 plot only
plot.dat2<-plot.dat%>%filter(subject %in% c("traitOFT1:sexM",  "traitOFT1:age", "traitOFT1:age2", "traitOFT1:date", "traitOFT1:local.density", "traitOFT1:avg_fam"))

plot.dat2$subject2 <- factor(plot.dat2$subject2, levels=c("traitOFT1:sexM",  "traitOFT1:age", "traitOFT1:age2", "traitOFT1:date", "traitOFT1:local.density", "traitOFT1:avg_fam"))

C<-ggplot(plot.dat2, 
	aes(x = latent.mean, y = subject2)) + 
	geom_vline(xintercept = 0, linetype="solid", 
                color = "gray", size=0.75) +	
	geom_segment(aes(x = latent.lower, 
		xend = latent.upper, 
		y = subject2, 
		yend = subject2), size=0.3) +
	geom_point(shape=16,cex=2, color="black") + 
	scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5, 1), position="top", limits = c(-1, 1), expand = c(0,0)) +
	scale_y_discrete(
		limits=rev,
		labels = c("traitOFT1:sexM"="Sex", "traitOFT1:age"="Age", "traitOFT1:age2"=expression("Age"^2),   "traitOFT1:date"="Day of year", "traitOFT1:local.density"="Local density",  "traitOFT1:avg_fam"="Social familiarity")) +
	xlab("Activity") + 
	ylab("") +
	theme_squirrel_dot 
C


#OFT2 plot only
plot.dat2<-plot.dat%>%filter(subject %in% c("traitOFT2:sexM",  "traitOFT2:age", "traitOFT2:age2", "traitOFT2:date", "traitOFT2:local.density", "traitOFT2:avg_fam"))

plot.dat2$subject2 <- factor(plot.dat2$subject2, levels=c("traitOFT2:sexM",  "traitOFT2:age",  "traitOFT2:age2", "traitOFT2:date", "traitOFT2:local.density", "traitOFT2:avg_fam"))

D<-ggplot(plot.dat2, 
	aes(x = latent.mean, y = subject2)) + 
	geom_vline(xintercept = 0, linetype="solid", 
                color = "gray", size=0.75) +	
	geom_segment(aes(x = latent.lower, 
		xend = latent.upper, 
		y = subject2, 
		yend = subject2), size=0.3) +	
	geom_point(shape=16,cex=2, color="black") + 
	scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5), position="top", limits = c(-0.5, 0.54), expand = c(0,0)) +
	scale_y_discrete(
		limits=rev,
		labels = c("traitOFT2:sexM"="Sex", "traitOFT2:age"="Age", "traitOFT2:age2"=expression("Age"^2),   "traitOFT2:date"="Day of year", "traitOFT2:local.density"="Local density",  "traitOFT2:avg_fam"="Social familiarity")) +
	xlab("Exploration") + 
	ylab("") +
	theme_squirrel_dot 
D



#PC1 plot only
plot.dat2<-plot.dat%>%filter(subject %in% c("traitPC1:sexM",  "traitPC1:age",  "traitPC1:age2", "traitPC1:date", "traitPC1:local.density", "traitPC1:avg_fam"))

plot.dat2$subject2 <- factor(plot.dat2$subject2, levels=c("traitPC1:sexM",  "traitPC1:age", "traitPC1:age2", "traitPC1:date", "traitPC1:local.density", "traitPC1:avg_fam"))

E<-ggplot(plot.dat2,  
	aes(x = latent.mean, y = subject2)) + 
	geom_vline(xintercept = 0, linetype="solid", 
                color = "gray", size=0.75) +	
	geom_segment(aes(x = latent.lower, 
		xend = latent.upper, 
		y = subject2, 
		yend = subject2), size=0.3) +
	geom_point(shape=16,cex=2, color="black") + 
	scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5), position="top", limits = c(-0.5, 0.5), expand = c(0,0)) +
	scale_y_discrete(
		limits=rev,
		labels = c("traitPC1:sexM"="Sex", "traitPC1:age"="Age", "traitPC1:age2"=expression("Age"^2),   "traitPC1:date"="Day of year", "traitPC1:local.density"="Local density",  "traitPC1:avg_fam"="Social familiarity")) +
	xlab("Foraging") + 
	ylab("") +
	theme_squirrel_dot 
E


#PC2 plot only
plot.dat2<-plot.dat%>%filter(subject %in% c("traitPC2:sexM",  "traitPC2:age", "traitPC2:age2", "traitPC2:date", "traitPC2:local.density", "traitPC2:avg_fam"))

plot.dat2$subject2 <- factor(plot.dat2$subject2, levels=c("traitPC2:sexM",  "traitPC2:age", "traitPC2:age2", "traitPC2:date", "traitPC2:local.density", "traitPC2:avg_fam"))

F<-ggplot(plot.dat2, 
	aes(x = latent.mean, y = subject2)) + 
	geom_vline(xintercept = 0, linetype="solid", 
                color = "gray", size=0.75) +	
	geom_segment(aes(x = latent.lower, 
		xend = latent.upper, 
		y = subject2, 
		yend = subject2), size=0.3) +
	geom_point(shape=16,cex=2, color="black") +  
	scale_x_continuous(breaks = c(-0.25, -0.13, 0, 0.13, 0.25), position="top", limits = c(-0.25, 0.25), expand = c(0,0)) +
	scale_y_discrete(
		limits=rev,
		labels = c("traitPC2:sexM"="Sex", "traitPC2:age"="Age", "traitPC2:age2"=expression("Age"^2),   "traitPC2:date"="Day of year", "traitPC2:local.density"="Local density",  "traitPC2:avg_fam"="Social familiarity")) +
	xlab("Movement") + 
	ylab("") +
	theme_squirrel_dot 
F



plot_grid(C, D, E, F, 
#labels=c("(a)", "(b)", "(c)", "(d)"), 
ncol = 2, nrow =2, align = "hv", label_x=0.29, label_y=1)       