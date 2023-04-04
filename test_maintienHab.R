############################
# #Surface des differentes classes(main classes)
# ut19_classes <- read.csv("C:/Meghana/donnee_brutes/UT_2019_10m.csv", sep=";")
# ut19_classes$CODE_UT = as.numeric(ut19_classes$CODE_UT)
# ut19_classes = ut19_classes[,-1] # On supprime la 1ere colonne 'OID_'
# ut19_groups = ut19_classes %>% group_by(DESC_CAT)
UREC_merge = testNbrClass
UREC_merge = UREC_merge[1:5,]
# raster_file = raster('C:/Meghana/Belgique/decembre/data/raster/ut19_mask.tif')
# csv_class_correspondence = ut19_groups
# t= csv_class_correspondence %>% tally()



r=1
#TODO:
SurfaceClass <-
  function (UREC_merge,
            csv_class_correspondence,
            raster_file) {
    
    #UREC_merge = sf file of all spatiale units
    #raster_file = raster file of utilisation du territoire cliped to only have area covered by spatial units !!!Has to be loaded with raster library: raster("file_path")
    #csv__class_correspondence : group csv of class correspondence between UT classes and IQBR classes (this must have weights) --> then use group_by function with CODE UT column
    
    for (r in 1:nrow(UREC_merge)) {
      shp = UREC_merge[r, ]
      print(paste0('reading', shp$id))
      shp = st_transform(shp, crs = st_crs(raster_file)) # project vector UREC file to same projection as raster file
      ut_shp = terra::crop(raster_file, shp) #Reduce the extent of the ESA raster to the extent of UREC
      shp_raster = terra::rasterize(shp, ut_shp) #rasterize UREC shapefile by using the esa_clip extent as the extent of the output raster (see docs)
      ut_mask = terra::mask(ut_shp, shp_raster)
      
      #Get frequency of each class in the raster
      freq_dist = freq(ut_mask)
      freq_dist = as.data.frame(freq_dist)
      
      #freq_dist$value = as.data.frame.integer(freq_dist$value)
      # join_tbl = left_join(freq_dist, IQBR_UT_2018_correspondence, c('value'='Value')) #raster needs to be loaded with raster library to work
      join_tbl = left_join(freq_dist,
                           csv_class_correspondence,
                           c('value' = 'CODE_UT'))
      join_tbl = na.omit(join_tbl) #remove NA from table
      total_pix = sum(join_tbl$count) #Count the number of pixels to get proportional coverage
      join_tbl$perc_coverage = round((join_tbl$count / total_pix), 2)
      summarize_joint_tbl = join_tbl %>% group_by(DESC_CAT) %>% dplyr::summarise(prop_surf = sum(perc_coverage))
      
      df_summarize_joint_tbl = as.data.frame(summarize_joint_tbl)
      transpose = as.data.frame(t(df_summarize_joint_tbl))
      names(transpose)<- transpose[1,] #make first row column names
      
      
      transpose <- transpose[-1,] #remove first row that now is present twice
      
      for (c in colnames(transpose)){
        UREC_merge[r,c] = transpose[,c]
      }
    } 
  }

UREC_merge_surfaceClass = SurfaceClass(UREC_merge=UREC_merge, csv_class_correspondence = csv_class_correspondence, raster_file = raster_file)

