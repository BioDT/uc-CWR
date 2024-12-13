
# variable selection: vifcor approach from ModGP

FUN.VarSelection <- function(specdata = Species_ls$occs,
                             varstack = NULL,
                             buf = NULL){

  vifcor_res <- usdm::vifcor(varstack, th = 0.7) # variable inflation factor
  varstack_sel <- usdm::exclude(varstack, vifcor_res) # excluding variables with high cor and vif
  
 # extracting values from the raster file
  
  if(missing(buf)) {
        pnt <- sf::st_coordinates(specdata) #  pnt <- sf::st_coordinates(occ_ls)
        values <- terra::extract(varstack_sel, pnt)
        #values <- exactextractr::exact_extract(varstack_sel, occ_ls) # HJ: a lot faster than terra but gives an error: 'Unsupported geometry'
        } else {
        pntbuf <- sf::st_buffer(specdata, dist = buf) #  pntbuf <- sf::st_buffer(occ_ls, dist = buf)
        st_crs(pntbuf) <- 4326 # HJ: without this, warning about 'no CRS specified'. How to do this properly?
        values <- exactextractr::exact_extract(varstack_sel, pntbuf, 'mean') # HJ: is mean correct here?
        # HJ: terra not used here because gets stuck with buffer, probably I'm misunderstanding something 
        }
  
  # correcting column names
  colnames(values) <- names(varstack_sel)
 
  # adding coordinates
  ext_values <- as.data.frame(row.names(values), nm = "pointid")
  ext_values <- cbind(ext_values, st_coordinates(specdata), values)
  #ext_values <- cbind(pnt, values)
  #ext_values <- na.omit(ext_values)
 
 ext_values
 
}

