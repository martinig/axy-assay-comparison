#code to make the coefficient plots from the multivariate models for assay and axy principal components (PC1, PC2)
#original code by A. R. Martinig
#last edited April 23, 2024 by A. R. Martinig


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
plot.dat2<-plot.dat%>%filter(subject %in% c("traitOFT1:sexM",  "traitOFT1:age", "traitOFT1:age2", "traitOFT1:local.density", "traitOFT1:avg_fam", "traitOFT1:date"))

plot.dat2$subject2 <- factor(plot.dat2$subject2, levels=c("traitOFT1:sexM",  "traitOFT1:age", "traitOFT1:age2", "traitOFT1:local.density", "traitOFT1:avg_fam", "traitOFT1:date"))

C<-ggplot(plot.dat2, 
	aes(x = latent.mean, y = subject2)) + 
	geom_vline(xintercept = 0, linetype="solid", 
                color = "gray", size=0.75) +	
	geom_segment(aes(x = latent.lower, 
		xend = latent.upper, 
		y = subject2, 
		yend = subject2), size=0.3) +
	geom_point(shape=16,cex=2, color="black") + 
	scale_x_continuous(breaks = c(-1, 0, 1, 2, 3), position="top", limits = c(-1, 3), expand = c(0,0)) +
	scale_y_discrete(
	breaks = c("traitOFT1:sexM",  "traitOFT1:age", "traitOFT1:age2", "traitOFT1:local.density", "traitOFT1:avg_fam", "traitOFT1:date"),
	labels = c("Sex", "Age",expression("Age"^2),  "Local density", "Social familiarity", "Day of year")) +
	xlab("Activity") + 
	ylab("") +
	theme_squirrel_dot 
C


#OFT2 plot only
plot.dat2<-plot.dat%>%filter(subject %in% c("traitOFT2:sexM",  "traitOFT2:age", "traitOFT2:age2", "traitOFT2:local.density", "traitOFT2:avg_fam", "traitOFT2:date"))

plot.dat2$subject2 <- factor(plot.dat2$subject2, levels=c("traitOFT2:sexM",  "traitOFT2:age",  "traitOFT2:age2", "traitOFT2:local.density", "traitOFT2:avg_fam", "traitOFT2:date"))

D<-ggplot(plot.dat2, 
	aes(x = latent.mean, y = subject2)) + 
	geom_vline(xintercept = 0, linetype="solid", 
                color = "gray", size=0.75) +	
	geom_segment(aes(x = latent.lower, 
		xend = latent.upper, 
		y = subject2, 
		yend = subject2), size=0.3) +	
	geom_point(shape=16,cex=2, color="black") + 
	scale_x_continuous(breaks = c(-1, 0,1.0, 2), position="top", limits = c(-1.05, 2), expand = c(0,0)) +
	scale_y_discrete(
	breaks = c("traitOFT2:sexM",  "traitOFT2:age", "traitOFT2:age2", "traitOFT2:local.density", "traitOFT2:avg_fam", "traitOFT2:date"),
	labels = c("Sex", "Age", expression("Age"^2),  "Local density", "Social familiarity",  "Day of year")) +
	xlab("Exploration") + 
	ylab("") +
	theme_squirrel_dot 
D


#PC1 plot only
plot.dat2<-plot.dat%>%filter(subject %in% c("traitPC1:sexM",  "traitPC1:age",  "traitPC1:age2", "traitPC1:local.density", "traitPC1:avg_fam", "traitPC1:date"))

plot.dat2$subject2 <- factor(plot.dat2$subject2, levels=c("traitPC1:sexM",  "traitPC1:age", "traitPC1:age2", "traitPC1:local.density", "traitPC1:avg_fam", "traitPC1:date"))

E<-ggplot(plot.dat2,  
	aes(x = latent.mean, y = subject2)) + 
	geom_vline(xintercept = 0, linetype="solid", 
                color = "gray", size=0.75) +	
	geom_segment(aes(x = latent.lower, 
		xend = latent.upper, 
		y = subject2, 
		yend = subject2), size=0.3) +
	geom_point(shape=16,cex=2, color="black") + 
	scale_x_continuous(breaks = c(-2, -1, 0, 1), position="top", limits = c(-2, 1.1), expand = c(0,0)) +
	scale_y_discrete(
		breaks = c("traitPC1:sexM",  "traitPC1:age", "traitPC1:age2", "traitPC1:local.density", "traitPC1:avg_fam", "traitPC1:date"),
		labels = c("Sex", "Age", expression("Age"^2),  "Local density", "Social familiarity",  "Day of year")) +
	xlab("Foraging") + 
	ylab("") +
	theme_squirrel_dot 
E


#PC2 plot only
plot.dat2<-plot.dat%>%filter(subject %in% c("traitPC2:sexM",  "traitPC2:age", "traitPC2:age2", "traitPC2:local.density", "traitPC2:avg_fam", "traitPC2:date"))

plot.dat2$subject2 <- factor(plot.dat2$subject2, levels=c("traitPC2:sexM",  "traitPC2:age", "traitPC2:age2", "traitPC2:local.density", "traitPC2:avg_fam", "traitPC2:date"))

F<-ggplot(plot.dat2, 
	aes(x = latent.mean, y = subject2)) + 
	geom_vline(xintercept = 0, linetype="solid", 
                color = "gray", size=0.75) +	
	geom_segment(aes(x = latent.lower, 
		xend = latent.upper, 
		y = subject2, 
		yend = subject2), size=0.3) +
	geom_point(shape=16,cex=2, color="black") +  
	scale_x_continuous(breaks = c(-1, 0, 1, 2), position="top", limits = c(-1, 2), expand = c(0,0)) +
	scale_y_discrete(
		breaks = c("traitPC2:sexM",  "traitPC2:age", "traitPC2:age2", "traitPC2:local.density", "traitPC2:avg_fam", "traitPC2:date"),
		labels = c("Sex", "Age", expression("Age"^2),  "Local density", "Social familiarity", "Day of year")) +
	xlab("Movement") + 
	ylab("") +
	theme_squirrel_dot 
F



plot_grid(C, D, E, F, 
#labels=c("(a)", "(b)", "(c)", "(d)"), 
ncol = 2, nrow =2, align = "hv", label_x=0.29, label_y=1)       