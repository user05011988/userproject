bruker2D <- function(inFile, outFile, nuc1, sfo1, bytordp, nc_proc, offset, sf,
  si, sw_p,	xdim){
  
  # if (missing(inFile))
  #   stop('The inFile file path is required')	
  # if (missing(outFile))
  #   stop('The outFile file path is required')	
  # if (missing(nuc1))
  #   stop('The Bruker paramater "nuc1" is required')
  # if (missing(sfo1))
  #   stop('The Bruker paramater "sfo1" is required')
  # if (missing(bytordp))
  #   stop('The Bruker paramater "bytordp" is required')
  # if (missing(nc_proc))
  #   stop('The Bruker paramater "nc_proc" is required')
  # if (missing(offset))
  #   stop('The Bruker paramater "offset" is required')
  # if (missing(sf))
  #   stop('The Bruker parameter "sf" is required')
  # if (missing(si))
  #   stop('The Bruker paramater "si" is required')
  # if (missing(sw_p))
  #   stop('The Bruker paramater "sw_p" is required')
  # if (missing(xdim))
  #   stop('The Bruker paramater "xdim" is required')
  # 
  ## Format input data
  # sfo1 <- as.numeric(sfo1)
  nc_proc <- as.numeric(nc_proc)
  offset <- as.numeric(offset)
  sf <- as.numeric(sf)
  si <- as.numeric(si)
  sw_p <- as.numeric(sw_p)
  xdim <- as.numeric(xdim)
  
  ## Test file connection
  readCon <- file(inFile, 'rb')
  testCon <- try(readBin(readCon,	size=4, what='integer',	n=si[1] * si[2], 
    endian=bytordp), silent=TRUE)
  if (class(testCon) == "try-error"){
    close(readCon)	
    stop(paste('Could not read Bruker processed data file:\n"', inFile, '"', 
      sep=''))
  }
  if (length(testCon) < si[1] * si[2]){
    close(readCon)	
    stop(paste('Could not convert Bruker processed data file:\n"', inFile, '"', 
      '\nFile size does not match data size.', sep=''))
  }
  seek(readCon, where=0)
  
  ## Read data a block at a time and reformat into a detiled matrix
  data <- matrix(nrow=si[2], ncol=si[1])
  tpc <- si[2] / xdim[2]
  tpr <- si[1] / xdim[1]
  for (i in 1:tpc){
    for (j in 1:tpr){
      rowNum <- (i - 1) * xdim[2] + 1
      colNum <- (j - 1) * xdim[1] + 1
      tileData <- matrix(as.numeric(readBin(readCon,	size=4,	what='integer',
        n=xdim[1] * xdim[2], endian=bytordp)), nrow=xdim[2], 
        ncol=xdim[1],	byrow=TRUE)
      data[rowNum:(rowNum + xdim[2] - 1), colNum:(colNum + xdim[1] - 1)] <- 
        tileData
    }
  }
  close(readCon)
  
  ## Resacale data
  data <- data / (2^-nc_proc)
  
  ## Calculate new tile size for ucsf format
  tileDim <- si
  size <- (tileDim[1] * tileDim[2] * 4) / 1024
  while (size > 32){
    tileDim <- tileDim / 2
    size <- (round(tileDim[1]) * round(tileDim[2]) * 4) / 1024
  }
  tileDim <- round(tileDim)
  
  ## Write main sparky header
  if (!file.exists(dirname(outFile)))
    dir.create(dirname(outFile), recursive=TRUE)
  writeCon <- file(outFile, "w+b")
  writeBin('UCSF NMR', writeCon, size=1, endian='big')
  writeBin(as.integer(0), writeCon, size=1)
  writeBin(as.integer(c(2, 1, 0, 2)), writeCon, size=1, endian='big')		
  writeBin(as.integer(rep(0, (180 - 14))), writeCon, size=1, endian='big')
  
  ## Calculate referenced carrier frequency in ppm and store the spectrometer
  car <- offset - sw_p / 2 / sf
  
  ## Write axis headers
  # for (i in c(2, 1)){
  #   writeBin(as.character(nuc1[i]), writeCon, size=1, endian='big')
  #   writeBin(as.integer(rep(0, (8 - nchar(nuc1[i]) - 1))), writeCon, size=1, 
  #     endian='big')
  #   writeBin(as.integer(si[i]), writeCon, size=4, endian='big')
  #   writeBin(as.integer(rep(0, 4)), writeCon, size=1, endian='big')
  #   writeBin(as.integer(tileDim[i]), writeCon, size=4, endian='big')
  #   writeBin(as.numeric(sfo1[i]), writeCon, size=4, endian='big')
  #   writeBin(as.numeric(sw_p[i]), writeCon, size=4, endian='big')
  #   writeBin(as.numeric(car[i]), writeCon, size=4, endian='big')
  #   writeBin(as.integer(rep(0, (128 - 32))), writeCon, size=1, endian='big')
  # }
  
  ## Get data for new tile
  tpc <- ceiling(si[2] / tileDim[2])
  tpr <- ceiling(si[1] / tileDim[1])
  for (i in 1:tpc){
    for (j in 1:tpr){
      rowNum <- (i - 1) * tileDim[2] + 1
      colNum <- (j - 1) * tileDim[1] + 1
      if (j == tpr)
        colOut <- ncol(data) - colNum + 1
      else
        colOut <- tileDim[1]
      if (i == tpc)
        rowOut <- nrow(data) - rowNum + 1
      else
        rowOut <- tileDim[2]
      ucsfData <- data[rowNum:(rowNum + rowOut - 1), 
        colNum:(colNum + colOut - 1)]
      
      ## Pad tiles if necessary
      tileRem <- si %% tileDim
      if (all(tileRem != 0) && j == tpr && i == tpc){
        
        ## Pad final tile
        if (colOut == 1){
          ucsfData <- c(ucsfData, rep(0, tileDim[2] - length(ucsfData)))
          ucsfData <- cbind(as.numeric(ucsfData), matrix(0, nrow=tileDim[2], 
            ncol=tileDim[1] - 1))
        }else if (rowOut == 1){
          ucsfData <- c(ucsfData, rep(0, tileDim[1] - length(ucsfData)))
          ucsfData <- rbind(as.numeric(ucsfData), matrix(0, 
            nrow=tileDim[2] - 1, ncol=tileDim[1]))
        }else{					
          ucsfData <- rbind(ucsfData, matrix(0, 
            nrow=tileDim[2] - nrow(ucsfData), 
            ncol=ncol(ucsfData)))
          ucsfData <- cbind(ucsfData, matrix(0, nrow=tileDim[2], 
            ncol=tileDim[1] - ncol(ucsfData)))
        }
      }else{
        
        ## Pad tile in last column
        if (tileRem[1] && j == tpr){
          if (colOut == 1)
            ucsfData <- cbind(as.numeric(ucsfData), matrix(0, nrow=tileDim[2], 
              ncol=tileDim[1] - 1))
          else
            ucsfData <- cbind(ucsfData, matrix(0, nrow=tileDim[2], 
              ncol=tileDim[1] - ncol(ucsfData)))
        }
        ## Pad tile in last row
        if (tileRem[2] && i == tpc){
          if (rowOut == 1)
            ucsfData <- rbind(as.numeric(ucsfData), matrix(0, 
              nrow=tileDim[2] - 1, ncol=tileDim[1]))
          else
            ucsfData <- rbind(ucsfData, matrix(0, 
              nrow=tileDim[2] - nrow(ucsfData), ncol=tileDim[1]))
        }
      }				
      
      ## Write out new tile
      writeBin(as.numeric(t(ucsfData)), writeCon, size=4, endian='big')
    }
  }
  close(writeCon)	
  
  return(outFile)
}