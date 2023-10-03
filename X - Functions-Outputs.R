#' ####################################################################### #
#' PROJECT: [BioDT CWR] 
#' CONTENTS: 
#'  - SDM Output Visualisation
#'  - SDM Output posthoc summaries
#'  DEPENDENCIES:
#'  - None
#' AUTHOR: [Erik Kusch]
#' ####################################################################### #

# PACKAGES -----------------------------------------------------------------
package_vec <- c(
	"raster", # for handling SDM raster outputs
	"sp", # for points
	"ggplot2", # for plotting engine
	"ggpubr", # for t-test comparisons in ggplot
	"tidyr", # for gather()
	"viridis", # colour palettes
	"cowplot", # grid plotting
	"ggpmisc", # for table plotting in ggplot environment
	"gridExtra" # for smooth plot saving in PDF
)
sapply(package_vec, install.load.package)

# BIOCLIMATIC VARIABLE PLOTTING -------------------------------------------
Plot_BC <- function(BC_ras, Shp = NULL, Water_Var = "Precipitation", which = "All"){
	BC_names <- c("Annual Mean Temperature", "Mean Diurnal Range", "Isothermality", "Temperature Seasonality", "Max Temperature of Warmest Month", "Min Temperature of Coldest Month", "Temperature Annual Range (BIO5-BIO6)", "Mean Temperature of Wettest Quarter", "Mean Temperature of Driest Quarter", "Mean Temperature of Warmest Quarter", "Mean Temperature of Coldest Quarter", paste("Annual", Water_Var), paste(Water_Var, "of Wettest Month"), paste(Water_Var, "of Driest Month"), paste(Water_Var, "Seasonality"), paste(Water_Var, "of Wettest Quarter"), paste(Water_Var, "of Driest Quarter"), paste(Water_Var, "of Warmest Quarter"), paste(Water_Var, "of Coldest Quarter"))
	BC_names <- paste0("BIO", 1:19, " - ", BC_names)
	BC_df <- as.data.frame(BC_ras, xy = TRUE) # turn raster into dataframe
	if(length(which) == 1){
		if(which == "All"){paste0("BIO", 1:19)}
	}
	Iter <- which
	BCplots_ls <- as.list(rep(NA, length(Iter)))
	counter <- 1
	for(Plot_Iter in Iter){
		Legend <- Plot_Iter
		Plot_df <- BC_df[, c("x", "y", Plot_Iter)]
		colnames(Plot_df)[3] <- "value"
		if(as.numeric(strsplit(Plot_Iter, split = "BIO")[[1]][2]) < 12){
			col_grad <- inferno(1e3) 
		}else{
			col_grad <- mako(1e3)
		}
		BC_plot <- ggplot() + # create a plot
			geom_raster(data = Plot_df, aes(x = x, y = y, fill = value)) + # plot the raw data
			theme_bw() + labs(title = BC_names[as.numeric(strsplit(Plot_Iter, split = "BIO")[[1]][2])], 
												x = "Longitude", y = "Latitude") + # make plot more readable
			scale_fill_gradientn(name = "", colours = col_grad) # add colour and legend
		if(!is.null(Shp)){ # if a shape has been designated
			BC_plot <- BC_plot + geom_polygon(data = Shp, aes(x = long, y = lat, group = group), colour = "black", fill = "NA") # add shape
		}
		BCplots_ls[[counter]] <- BC_plot
		counter <- counter+1
	}
	
	if(length(which) > 1){
		cowplot::plot_grid(plotlist = BCplots_ls, nrow = ceiling(length(Iter)/2))
	}else{
		BCplots_ls[[1]]	
		}
}

# SDM PREDICTION VISUALISATION --------------------------------------------
FUN.Viz <- function(SDMModel_ls, SDMInput_ls, BV_ras, Dir){
	
	Plots_ls <- pblapply(names(SDMPred_ls), FUN = function(Species_iter){
		# Species_iter <- names(SDMPred_ls)[1]
		Plot_iter <- SDMPred_ls[[Species_iter]]
		Input_iter <- SDMInput_ls[[Species_iter]]
		
		PA_df <- Input_iter$PA
		
		P_sp <- PA_df[PA_df$PRESENCE == 1, ]
		coordinates(P_sp) <- ~lon + lat
		buffer_sf <- st_union(st_buffer(st_as_sf(P_sp), 15)) # 15 degree buffer around points
		
		BV_iter <- BV_ras[[Input_iter$SDMData@features.name]]
		
		## Fact Sheet ----
		PA_tab <- data.frame(table(PA_df$PRESENCE))
		colnames(PA_tab)[1] <- "P/A"
		Fact_gg <- ggplot(data.frame(x = 1:2, y = 3:10), aes(x = x, y = y)) + 
			geom_point(col = "white") + 
			labs(title = Species_iter) + 
			annotate(geom = "text", x = 1.2, y = 10,
							 label = "Absence/Presence") + 
			annotate(geom = "table",
							 x = 1.2,
							 y = 9.5,
							 label = list(PA_tab)) + 
			annotate(geom = "text", x = 1.6, y = 10,
							 label = "Model Evaluation") + 
			annotate(geom = "table",
							 x = 1.8,
							 y = 9.5,
							 label = list(SDMModel_ls[[Species_iter]]$evalalutation)) + 
			theme_void()
		
		## SDM Input Visualisation ----
		First_gg <- Plot_BC(BV_iter, as_Spatial(buffer_sf), which = names(BV_iter)[1]) + 
			geom_point(aes(x = lon, y = lat, color = factor(PRESENCE)), data = PA_df, size = 1, pch = 4) +
			scale_color_manual(values = c("black", "white")) +
			geom_polygon(aes(x = long, y = lat, group = id), data = fortify(as_Spatial(buffer_sf)),
									 colour = 'black', size = 0.2, fill = 'black', alpha = .1)
		Subseq_gg <- Plot_BC(BV_iter, as_Spatial(buffer_sf), which = names(BV_iter)[-1])
		
		Input_plot <- cowplot::plot_grid(First_gg, Subseq_gg, ncol = 1, 
																		 rel_heights = c(1, floor((length(names(BV_iter))-1)/3)))
		
		## Probability plot ----
		Probability_df <- as.data.frame(Plot_iter$prediction, xy = TRUE) # turn raster into dataframe
		Probability_df <- gather(data = Probability_df, key = Values, value = "value", colnames(Probability_df)[c(-1, -2)]) #  make ggplot-ready
		Probability_plot <- ggplot() + # create plot
			geom_raster(data = Probability_df, aes(x = x, y = y, fill = value)) + # plot the covariate data
			theme_bw() + facet_wrap(~Values, ncol = 4) + 
			labs(x = "Longitude", y = "Latitude") + # make plot more readable
			scale_fill_gradientn(colors = viridis(100), na.value = "transparent") + # add colour and legend
			theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + # reduce margins (for fusing of plots)
			theme(legend.key.size = unit(1, 'cm'), legend.position = "bottom")
		
		## Binarised plot ----
		Binarised_df <- as.data.frame(Plot_iter$binarised, xy = TRUE) # turn raster into dataframe
		Binarised_df <- gather(data = Binarised_df, key = Values, value = "value", colnames(Binarised_df)[c(-1, -2)]) #  make ggplot-ready
		Binarised_plot <- ggplot() + # create plot
			geom_raster(data = Binarised_df, aes(x = x, y = y, fill = value)) + # plot the covariate data
			theme_bw() + facet_wrap(~Values, ncol = 4) + 
			labs(x = "Longitude", y = "Latitude") + # make plot more readable
			scale_fill_discrete(na.value = "transparent") + # add colour and legend
			theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + # reduce margins (for fusing of plots)
			theme(legend.key.size = unit(1, 'cm'), legend.position = "bottom")
		
		## Proportion plot ----
		Proportion_df <- as.data.frame(Plot_iter$proportion, xy = TRUE) # turn raster into dataframe
		Proportion_df <- gather(data = Proportion_df, key = Values, value = "value", colnames(Proportion_df)[c(-1, -2)]) #  make ggplot-ready
		Proportion_plot <- ggplot() + # create plot
			geom_raster(data = Proportion_df, aes(x = x, y = y, fill = value)) + # plot the covariate data
			theme_bw() + labs(title = "Proportion of binarised agreement") + 
			labs(x = "Longitude", y = "Latitude") + # make plot more readable
			scale_fill_gradientn(colors = inferno(100), na.value = "transparent") + # add colour and legend
			theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + # reduce margins (for fusing of plots)
			theme(legend.key.size = unit(1.5, 'cm'))
		
		## Returning plots ----
		return_ls <- list(
			Facts = Fact_gg,
			Inputs = Input_plot,
			Probability = Probability_plot,
			Binarised = Binarised_plot,
			Proportion = Proportion_plot
		)
		
		ggsave(
			filename = file.path(Dir, paste0("TEMPPlot_", Species_iter, ".pdf")), 
			plot = marrangeGrob(return_ls, nrow=1, ncol=1), 
			width = 15, height = 12
		)
		
		return_ls
	})
	names(Plots_ls) <- names(SDMPred_ls)

	ggsave(
		filename = 
			file.path(Dir, paste0(
				unique(unlist(lapply(strsplit(names(SDMPred_ls), split = " "), "[[", 1))), "Plots.pdf")), 
		plot = marrangeGrob(unlist(Plots_ls, recursive = FALSE), nrow=1, ncol=1), 
		width = 15, height = 12
	)
	
	unlink(list.files(Dir, pattern = "TEMPPlot", full.names = TRUE))
	
	Plots_ls
}

# SDM PREDICTION POSTHOC COMPARISON ---------------------------------------
FUN.Posthoc <- function(SDMPred_ls, # SDM predictions in list where each element represents a species
												Covariates, # covariates against which to check predicted presences
												CutOff = 0.6, # if CutOff*100 % of SDM models predict presence in a cell, it is treated as a presence
												Dir # where to save the plots
												){
	PH_resample <- resample(Covariates, SDMPred_ls[[1]]$proportion)
	
	SDM_PH_ls <- pblapply(names(SDMPred_ls), FUN = function(Species_name){
		PH_iter <- SDMPred_ls[[Species_name]]
		proportion_SDM <- PH_iter$proportion
		binprop_SDM <- proportion_SDM >= CutOff # arbitrary CutOff proposed by Desalegn
		plot_df <- data.frame(Prediction = rep(values(binprop_SDM), nlayers(PH_resample)),
													Posthoc = as.vector(values(PH_resample)),
													Type = rep(names(PH_resample), each = ncell(PH_resample))
		)
		my_comparisons <- list(c("TRUE", "FALSE"))
		return_g <- ggplot(data = na.omit(plot_df), aes(fill = Prediction, y = Posthoc, x = Type)) + 
			# geom_boxplot() + 
			geom_violin() + 
			stat_compare_means(aes(group = Prediction), label = "p.format") + 
			theme_bw()
		
		ggsave(
			filename = file.path(Dir, paste0("TEMPPosthoc_", Species_name, ".pdf")), 
			plot = return_g, 
			width = 15, height = 12
		)
		
		return_g
	})
	names(SDM_PH_ls) <- names(SDMPred_ls)
	
	ggsave(
		filename = 
			file.path(Dir, paste0(
				unique(unlist(lapply(strsplit(names(SDMPred_ls), split = " "), "[[", 1))), "Posthoc.pdf")), 
		plot = marrangeGrob(SDM_PH_ls, nrow=1, ncol=1), 
		width = 15, height = 12
	)
	
	unlink(list.files(Dir, pattern = "TEMPPosthoc", full.names = TRUE))
	
	SDM_PH_ls
}
