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
FUN.ShinyPrep <- function(SDMModel_Iter, Dir_spec){
		Pres_df <- SDMModel_Iter$PA[SDMModel_Iter$PA$PRESENCE == 1, c("lat", "lon", "gbifID", "PRESENCE")]
		Abs_df <- SDMModel_Iter$PA[SDMModel_Iter$PA$PRESENCE == 0, c("lat", "lon", "gbifID", "PRESENCE")]
		
		Pres_sf <- st_as_sf(Pres_df, coords = c("lon", "lat"))
		Abs_sf <- st_as_sf(Abs_df, coords = c("lon", "lat"))
		
		Buffer_sf <- st_union(st_buffer(Pres_sf, 15))
		
		Shiny_ls <- list(Presences = Pres_sf,
				 Absences = Abs_sf,
				 Buffer = Buffer_sf,
				 BVs = colnames(SDMModel_Iter$PA)[startsWith(colnames(SDMModel_Iter$PA), "BIO")]
				 )
	
		save(Shiny_ls, file = file.path(Dir_spec, "ShinyData.RData"))

		return(Shiny_ls)
}

# SDM PREDICTION VISUALISATION --------------------------------------------
FUN.Viz <- function(Shiny_ls, Model_ras, BV_ras, Covariates, Dir_spec){
	
		PA_df <- cbind(
			data.frame(PRESENCE = c(Shiny_ls$Presences$PRESENCE,
															Shiny_ls$Absences$PRESENCE)),
			rbind(
				st_coordinates(Shiny_ls$Presences), 
				st_coordinates(Shiny_ls$Absences)
				)
			)
		
		buffer_sf <- Shiny_ls$Buffer
		
		BV_iter <- BV_ras[[Shiny_ls$BVs]]
		
		# ## Fact Sheet ----
		# PA_tab <- data.frame(table(PA_df$PRESENCE))
		# colnames(PA_tab)[1] <- "P/A"
		# imgurlROC <- file.path(Dir.Exports, paste0("ISDM-", strsplit(Species_iter, split = " ")[[1]][1]),
		# 											 str_replace(Species_iter, " ", "_"), "ROC.png")
		# ROC <- readPNG(imgurlROC)
		# ROC <- rasterGrob(ROC, interpolate=TRUE)
		# 
		# Fact_gg <- ggplot(data.frame(x = 1:5, y = 1:10), aes(x = x, y = y)) +
		# 	geom_point(col = "white") +
		# 	labs(title = Species_iter) +
		# 	annotate(geom = "text", x = 1.5, y = 10,
		# 					 label = "Absence/Presence") +
		# 	annotate(geom = "table",
		# 					 x = 1.2,
		# 					 y = 9.5,
		# 					 label = list(PA_tab)) +
		# 	annotation_custom(ROC, xmin=2, xmax=5, ymin=1, ymax=10) +
		# 	theme_void()
		# Fact_gg
		
		## SDM Input Visualisation ----
		First_gg <- Plot_BC(BV_iter, as_Spatial(buffer_sf), which = names(BV_iter)[1]) + 
			geom_point(aes(x = X, y = Y, color = factor(PRESENCE)), data = PA_df, size = 1, pch = 4) +
			scale_color_manual(values = c("black", "white")) 
		Subseq_gg <- Plot_BC(BV_iter, as_Spatial(buffer_sf), which = names(BV_iter)[-1])
		
		Input_plot <- cowplot::plot_grid(First_gg, Subseq_gg, ncol = 1, 
																		 rel_heights = c(1, floor((length(names(BV_iter))-1)/2)))
		ggsave(Input_plot, filename = file.path(Dir_spec, "INPUTS.png"), 
					 width = 32, height = 12*ceiling(length(names(BV_iter))/2), units = "cm")
		
		## Probability plot ----
		Probability1_df <- as.data.frame(exp(Model_ras$Suitability), xy = TRUE) # turn raster into dataframe
		Probability_df <- gather(data = Probability1_df, key = Values, value = "value", colnames(Probability1_df)[c(-1, -2)]) #  make ggplot-ready
		Probability_plot <- ggplot() + # create plot
			geom_raster(data = Probability_df, aes(x = x, y = y, fill = value)) + # plot the covariate data
			theme_bw() + facet_wrap(~Values, ncol = 4) + 
			labs(x = "Longitude", y = "Latitude") + # make plot more readable
			scale_fill_gradientn(colors = viridis(100), na.value = "transparent") + # add colour and legend
			theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + # reduce margins (for fusing of plots)
			theme(legend.key.size = unit(1, 'cm'), legend.position = "bottom")
		ggsave(Probability_plot, filename = file.path(Dir_spec, "OUT_Suitability.png"), 
					 width = 24, height = 16, units = "cm")
		
		## Binarised plot ----
		Binarised1_df <- as.data.frame(Model_ras$`Predicted Presence/Absence`, xy = TRUE) # turn raster into dataframe
		Binarised_df <- gather(data = Binarised1_df, key = Values, value = "value", colnames(Binarised1_df)[c(-1, -2)]) #  make ggplot-ready
		Binarised_plot <- ggplot() + # create plot
			geom_raster(data = Binarised_df, aes(x = x, y = y, fill = value)) + # plot the covariate data
			theme_bw() + facet_wrap(~Values, ncol = 4) + 
			labs(x = "Longitude", y = "Latitude") + # make plot more readable
			scale_fill_discrete(na.value = "transparent") + # add colour and legend
			theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + # reduce margins (for fusing of plots)
			theme(legend.key.size = unit(1, 'cm'), legend.position = "bottom")
		ggsave(Binarised_plot, filename = file.path(Dir_spec, "OUT_Binary.png"), 
					 width = 24, height = 16, units = "cm")
		
		## Response curves ----
		Mask_ras <- !is.na(Model_ras$Suitability)
		Covariates <- mask(Covariates, Model_ras$Suitability)
		Drivers_df <- as.data.frame(Covariates, xy = TRUE) # turn raster into dataframe
		PH_df <- cbind(Drivers_df, Binarised1_df$`Predicted Presence/Absence`, Probability1_df$Suitability)
		colnames(PH_df)[(nlyr(Covariates)+3):ncol(PH_df)] <- c("Presence/Absence", "Suitability")
		
		Drivers_ls <- lapply(colnames(PH_df)[-1:-2][colnames(PH_df)[-1:-2] %nin% c("Presence/Absence", "Suitability")],
					 FUN = function(x){
					 	PH_iter <- PH_df
					 	colnames(PH_iter)[which(colnames(PH_iter) == x)] <- "Driver"
					 	suitab_gg <- ggplot(PH_iter, aes(x = Driver, y = Suitability)) + 
					 		geom_smooth() + 
					 		theme_bw() + labs(title = x)
					 	my_comparisons <- list(c("TRUE", "FALSE"))
					 	binary_gg <- ggplot(na.omit(PH_iter), aes(y = Driver, x = `Presence/Absence`, fill = `Presence/Absence`)) + 
					 		geom_violin() + 
					 		stat_compare_means(aes(group = `Presence/Absence`), label = "p.format") + 
					 		theme_bw()
					 	# + labs(title = x)
					 	
					 	ggsave(cowplot::plot_grid(suitab_gg, binary_gg, ncol = 2), 
					 				 filename = file.path(Dir_spec, paste0("RESPCURV_", x,".png")), 
					 				 width = 32, height = 16, units = "cm")
					 })
		resp_curves <- cowplot::plot_grid(plotlist = Drivers_ls, ncol = 1)
		
		## Returning plots ----
		return_ls <- list(
			# Facts = Fact_gg,
			Inputs = Input_plot,
			Probability = Probability_plot,
			Binarised = Binarised_plot,
			Responses = resp_curves
		)
		return_ls
	# names(Plots_ls) <- names(SDM_outs)
	# 
	# ggsave(
	# 	filename = 
	# 		file.path(Dir, paste0(
	# 			unique(unlist(lapply(strsplit(names(SDM_outs), split = " "), "[[", 1))), "Plots.pdf")), 
	# 	plot = marrangeGrob(unlist(Plots_ls, recursive = FALSE), nrow=1, ncol=1), 
	# 	width = 15, height = 12
	# )
	# 
	# unlink(list.files(Dir, pattern = "TEMPPlot", full.names = TRUE))
	
	# Plots_ls
}