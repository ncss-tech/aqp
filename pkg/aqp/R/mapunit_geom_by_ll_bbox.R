

mapunit_geom_by_ll_bbox <- function(bbox)
  {
  # check to make sure the user's OGR has been compiled with GML support:
  if( ! "GML" %in% ogrDrivers()$name)
	stop('GML support is missing from your GDAL/OGR build.')
	
  
  # parse bbox and compose URL
  bbox.text <- paste(bbox, collapse=',')
  
  u <- paste( 'http://sdmdataaccess.nrcs.usda.gov/Spatial/SDMNAD83Geographic.wfs?Service=WFS&Version=1.0.0&Request=GetFeature&Typename=MapunitPoly&BBOX=', bbox.text, sep='')
  
  # get a temp location to save the file
  td <- tempdir()
  tf <- tempfile(pattern="file", tmpdir=td)
  tf.full <- paste(tf, '.gml', sep='')
  
  # save the file locally
  download.file(url=u, destfile=tf.full)
  
  # read in via OGR, into a SPolyDF. 
  # note hard-coded layer name from within the GML source
  ## this does not work, because of possibly bad GML!
  ## 
  ## Error in data.frame(dlist, row.names = fids, stringsAsFactors = stringsAsFactors) : 
  ## duplicate row.names: 464331, 464429, 464445, [...]
  ## 
  ## 
  ## submitted a patch to Roger, waiting...
  ## 
  d <- readOGR(dsn=tf.full, layer='MapunitPoly')
  
  
  # done, clean-up
  # leaves the .gfs file hanging around
  unlink(tf.full)
  
  return(d)
  }
  
