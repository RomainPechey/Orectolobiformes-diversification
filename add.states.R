# in progress...
add.states <- function(decx_states, cex, col = NULL, 
                       names = NULL, leg = T, distri = NULL,
                       limit = NULL, cex.limit = NULL,
                       nodes = NULL, cex.nodes = NULL,
                       leg.distri = F){
  
  
  lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  node <- (lastPP$Ntip + 1):length(lastPP$xx)
  XX <- lastPP$xx[node]
  YY <- lastPP$yy[node]
  coords <- data.frame(X = XX, Y = YY)
  coords$states <- decx_states
  
  # many warnings to put for nodes (length, etc.)
  
  if(is.null(limit)){
    limit <- max(XX)
  }
  if(is.null(cex.limit)){
    cex.limit <- cex
  }
  
  if(is.null(nodes)){
    limit <- 0
  }
  if(is.null(cex.nodes)){
    cex.nodes <- cex
  }
  
  levels <- sort(unique(unlist(strsplit(c(coords$states, distri), "_"))))
  if(is.null(col)){
    col <- RColorBrewer::brewer.pal(12,"Paired")[1:length(levels)]
  } else {
    if(length(col) == length(levels)){
      colors <- col
    } else {
      stop("argument 'col' should be the same length than the number of states.")
    }
  }
  ref.color <- data.frame(levels = levels, colors = colors)
  for(i in 1:length(coords$states)){
    coords$colors[i] <- paste(colors[levels %in% strsplit(coords$states[i], "_")[[1]]], collapse = "_")
  }
  
  for(co in 1:nrow(coords)){
    n_inf <- length(strsplit(coords$states, "_")[[co]])
    cols <- strsplit(coords$colors, "_")[[co]]
    
    if(max(lastPP$xx) - coords$X[co] > limit){
      cex1 <- cex.limit
    } else {
      cex1 <- cex
    }
    
    if(any(sapply(nodes, function(x) node[co] %in% x))){
      cex1 <- cex.nodes[sapply(nodes, function(x) node[co] %in% x)]
    } else {
      cex1 <- cex
    }
    
    added_width <- xinch(cex1)
    added_height <- yinch(cex1)
    # rectangle
    xl <- coords$X[co] - added_width/2
    xr <- coords$X[co] + added_width/2
    yb <- coords$Y[co] - added_height/2
    yt <- coords$Y[co] + added_height/2
    
    if(n_inf == 1){
      #1
      rect(xl, yb, xr, yt, col = cols[1], lwd = 0.5)
    }
    
    if(n_inf == 2){
      
      #1
      xl1 <- xl - abs(xl - xr)/2
      xr1 <- xr - abs(xl - xr)/2
      rect(xl1, yb, xr1, yt, col = cols[1], lwd = 0.5)
      #2
      xl2 <- xl + abs(xl - xr)/2
      xr2 <- xr + abs(xl - xr)/2
      rect(xl2, yb, xr2, yt, col = cols[2], lwd = 0.5)    
    }
    
    if(n_inf == 3 | n_inf == 4){
      
      #1
      xl1 <- xl - abs(xl - xr)/2
      xr1 <- xr - abs(xl - xr)/2
      yb1 <- yb + abs(yb - yt)/2
      yt1 <- yt + abs(yb - yt)/2
      rect(xl1, yb1, xr1, yt1, col = cols[1], lwd = 0.5)
      #2
      xl2 <- xl + abs(xl - xr)/2
      xr2 <- xr + abs(xl - xr)/2
      yb2 <- yb + abs(yb - yt)/2
      yt2 <- yt + abs(yb - yt)/2
      rect(xl2, yb2, xr2, yt2, col = cols[2], lwd = 0.5)
      #3
      xl3 <- xl - abs(xl - xr)/2
      xr3 <- xr - abs(xl - xr)/2
      yb3 <- yb - abs(yb - yt)/2
      yt3 <- yt - abs(yb - yt)/2
      rect(xl3, yb3, xr3, yt3, col = cols[3], lwd = 0.5)
      #4
      if(n_inf == 4 ){
        xl4 <- xl + abs(xl - xr)/2
        xr4 <- xr + abs(xl - xr)/2
        yb4 <- yb - abs(yb - yt)/2
        yt4 <- yt - abs(yb - yt)/2
        rect(xl4, yb4, xr4, yt4, col = cols[4], lwd = 0.5)
      }
    }
    
    if(n_inf == 5 | n_inf == 6){
      #1
      xl1 <- xl - abs(xl - xr)
      xr1 <- xr - abs(xl - xr)
      yb1 <- yb + abs(yb - yt)*0.5
      yt1 <- yt + abs(yb - yt)*0.5
      rect(xl1, yb1, xr1, yt1, col = cols[1], lwd = 0.5)
      #2
      xl2 <- xl
      xr2 <- xr
      yb2 <- yb + abs(yb - yt)*0.5
      yt2 <- yt + abs(yb - yt)*0.5
      rect(xl2, yb2, xr2, yt2, col = cols[2], lwd = 0.5)
      #3
      xl3 <- xl + abs(xl - xr)
      xr3 <- xr + abs(xl - xr)
      yb3 <- yb + abs(yb - yt)*0.5
      yt3 <- yt + abs(yb - yt)*0.5
      rect(xl3, yb3, xr3, yt3, col = cols[3], lwd = 0.5)
      #4
      xl4 <- xl - abs(xl - xr)
      xr4 <- xr - abs(xl - xr)
      yb4 <- yb - abs(yb - yt)*0.5
      yt4 <- yt - abs(yb - yt)*0.5
      rect(xl4, yb4, xr4, yt4, col = cols[4], lwd = 0.5)
      #5
      xl5 <- xl 
      xr5 <- xr
      yb5 <- yb - abs(yb - yt)*0.5
      yt5 <- yt - abs(yb - yt)*0.5
      rect(xl5, yb5, xr5, yt5, col = cols[5], lwd = 0.5)
      
      if(n_inf == 6){
        xl6 <- xl + abs(xl - xr)
        xr6 <- xr + abs(xl - xr)
        yb6 <- yb + abs(yb - yt)*0.5
        yt6 <- yt + abs(yb - yt)*0.5
        rect(xl6, yb6, xr6, yt6, col = cols[6], lwd = 0.5)
      }
    }
    
    if(n_inf > 6){
      added_width <- xinch(cex1*0.66)
      added_height <- yinch(cex1*0.66)
      # rectangle
      xl <- coords$X[co] - added_width/2
      xr <- coords$X[co] + added_width/2
      yb <- coords$Y[co] - added_height/2
      yt <- coords$Y[co] + added_height/2
    }
    
    if(n_inf >= 7 & n_inf < 10){
      
      #1
      xl1 <- xl - abs(xl - xr)*1
      xr1 <- xr - abs(xl - xr)*1
      yb1 <- yb + abs(yb - yt)*1
      yt1 <- yt + abs(yb - yt)*1
      rect(xl1, yb1, xr1, yt1, col = cols[1], lwd = 0.5)
      #2
      xl2 <- xl
      xr2 <- xr
      yb2 <- yb + abs(yb - yt)
      yt2 <- yt + abs(yb - yt)
      rect(xl2, yb2, xr2, yt2, col = cols[2], lwd = 0.5)
      #3
      xl3 <- xl + abs(xl - xr)
      xr3 <- xr + abs(xl - xr)
      yb3 <- yb + abs(yb - yt)
      yt3 <- yt + abs(yb - yt)
      rect(xl3, yb3, xr3, yt3, col = cols[3], lwd = 0.5)
      #4
      xl4 <- xl - abs(xl - xr)
      xr4 <- xr - abs(xl - xr)
      yb4 <- yb
      yt4 <- yt
      rect(xl4, yb4, xr4, yt4, col = cols[4], lwd = 0.5)
      #5
      xl5 <- xl
      xr5 <- xr
      yb5 <- yb
      yt5 <- yt
      rect(xl5, yb5, xr5, yt5, col = cols[5], lwd = 0.5)
      #6
      xl6 <- xl + abs(xl - xr)
      xr6 <- xr + abs(xl - xr)
      yb6 <- yb
      yt6 <- yt
      rect(xl6, yb6, xr6, yt6, col = cols[6], lwd = 0.5)
      #7
      xl7 <- xl - abs(xl - xr)
      xr7 <- xr - abs(xl - xr)
      yb7 <- yb - abs(yb - yt)
      yt7 <- yt - abs(yb - yt)
      rect(xl7, yb7, xr7, yt7, col = cols[7], lwd = 0.5)
      if(8 %in% 1:n_inf){
        #8
        xl8 <- xl
        xr8 <- xr
        yb8 <- yb - abs(yb - yt)
        yt8 <- yt - abs(yb - yt)
        rect(xl8, yb8, xr8, yt8, col = cols[8], lwd = 0.5)
      }
      if(9 %in% 1:n_inf){
        #9
        xl9 <- xl + abs(xl - xr)
        xr9 <- xr + abs(xl - xr)
        yb9 <- yb - abs(yb - yt)
        yt9 <- yt - abs(yb - yt)
        rect(xl9, yb9, xr9, yt9, col = cols[9], lwd = 0.5) 
      }
    }
    
    if(n_inf >= 10 & n_inf < 13){
      #1
      xl1 <- xl - abs(xl - xr)*1.5
      xr1 <- xr - abs(xl - xr)*1.5
      yb1 <- yb + abs(yb - yt)*1
      yt1 <- yt + abs(yb - yt)*1
      rect(xl1, yb1, xr1, yt1, col = cols[1], lwd = 0.5)
      #2
      xl2 <- xl - abs(xl - xr)*0.5
      xr2 <- xr - abs(xl - xr)*0.5
      yb2 <- yb + abs(yb - yt)*1
      yt2 <- yt + abs(yb - yt)*1
      rect(xl2, yb2, xr2, yt2, col = cols[2], lwd = 0.5)
      #3
      xl3 <- xl + abs(xl - xr)*0.5
      xr3 <- xr + abs(xl - xr)*0.5
      yb3 <- yb + abs(yb - yt)*1
      yt3 <- yt + abs(yb - yt)*1
      rect(xl3, yb3, xr3, yt3, col = cols[3], lwd = 0.5)
      #4
      xl4 <- xl + abs(xl - xr)*1.5
      xr4 <- xr + abs(xl - xr)*1.5
      yb4 <- yb + abs(yb - yt)*1
      yt4 <- yt + abs(yb - yt)*1
      rect(xl4, yb4, xr4, yt4, col = cols[4], lwd = 0.5)
      #5
      xl5 <- xl - abs(xl - xr)*1.5
      xr5 <- xr - abs(xl - xr)*1.5
      yb5 <- yb
      yt5 <- yt
      rect(xl5, yb5, xr5, yt5, col = cols[5], lwd = 0.5)
      #6
      xl6 <- xl - abs(xl - xr)*0.5
      xr6 <- xr - abs(xl - xr)*0.5
      yb6 <- yb
      yt6 <- yt
      rect(xl6, yb6, xr6, yt6, col = cols[6], lwd = 0.5)
      #7
      xl7 <- xl + abs(xl - xr)*0.5
      xr7 <- xr + abs(xl - xr)*0.5
      yb7 <- yb
      yt7 <- yt
      rect(xl7, yb7, xr7, yt7, col = cols[7], lwd = 0.5)
      #8
      xl8 <- xl + abs(xl - xr)*1.5
      xr8 <- xr + abs(xl - xr)*1.5
      yb8 <- yb
      yt8 <- yt
      rect(xl8, yb8, xr8, yt8, col = cols[8], lwd = 0.5)
      #9
      if(9 %in% 1:n_inf){
        xl9 <- xl - abs(xl - xr)*1.5
        xr9 <- xr - abs(xl - xr)*1.5
        yb9 <- yb - abs(yb - yt)*1
        yt9 <- yt - abs(yb - yt)*1
        rect(xl9, yb9, xr9, yt9, col = cols[9], lwd = 0.5)
      }
      #10
      if(10 %in% 1:n_inf){
        xl10 <- xl - abs(xl - xr)*0.5
        xr10 <- xr - abs(xl - xr)*0.5
        yb10 <- yb - abs(yb - yt)*1
        yt10 <- yt - abs(yb - yt)*1
        rect(xl10, yb10, xr10, yt10, col = cols[10], lwd = 0.5)
      }
      #11
      if(11 %in% 1:n_inf){
        xl11 <- xl + abs(xl - xr)*0.5
        xr11 <- xr + abs(xl - xr)*0.5
        yb11 <- yb - abs(yb - yt)*1
        yt11 <- yt - abs(yb - yt)*1
        rect(xl11, yb11, xr11, yt11, col = cols[11], lwd = 0.5)
      }
      #12
      if(12 %in% 1:n_inf){
        xl12 <- xl + abs(xl - xr)*1.5
        xr12 <- xr + abs(xl - xr)*1.5
        yb12 <- yb - abs(yb - yt)*1
        yt12 <- yt - abs(yb - yt)*1
        rect(xl12, yb12, xr12, yt12, col = cols[12], lwd = 0.5)
      }
    }
  }
  
  if(!is.null(distri)){
    
    xx <- c()
    yy <- c()
    for(i in 1:lastPP$Ntip){
      
      x <- lastPP$xx[1:lastPP$Ntip][i]+0.05
      y <- lastPP$yy[1:lastPP$Ntip][i]
      
      xx <- c(xx, x)
      yy <- c(yy, y)
    }
    coords <- data.frame(X = xx, Y = yy)
    coords$states <- distri
    
    distri_lev <- sort(unique(unlist(strsplit(distri, "_"))))
    distri_col <- ref.color$colors[ref.color$levels %in% distri_lev]
    ref <- data.frame(X = coords$X[1], Y = max(coords$Y)+1,
                      states = paste(distri_lev, collapse = "_"))
    coords <- rbind(ref, coords)
    
    if(leg.distri == F | length(unique(coords$X)) != 1){
      coords <- coords[-1,]
    }
    
    if(is.null(col)){
      col <- RColorBrewer::brewer.pal(12,"Paired")[1:length(levels)]
    } else {
      if(length(col) == length(levels)){
        colors <- col
      } else {
        stop("argument 'col' should be the same length than the number of states.")
      }
    }
    
    for(i in 1:length(coords$states)){
      coords$colors[i] <- paste(colors[levels %in% strsplit(coords$states[i], "_")[[1]]], collapse = "_")
    }
    
    for(co in 1:nrow(coords)){
      n_inf <- length(strsplit(coords$states, "_")[[co]])
      cols <- strsplit(coords$colors, "_")[[co]]
      cols <- ifelse(distri_col %in% cols, distri_col, "white")
      
      # rectangle
      xl <- coords$X[co]
      xr <- coords$X[co] + added_width
      yb <- coords$Y[co] - added_height/2
      yt <- coords$Y[co] + added_height/2
      
      rect(xl, yb, xr, yt, col = cols[1], lwd = 0.5)
      for(coi in 2:length(cols)){
        xl <- xl+added_width
        xr <- xr+added_width
        rect(xl, yb, xr, yt, col = cols[coi], lwd = 0.5)
      }
    }
    
    #text(coords$X[1]-0.4, coords$Y[1]+0.4, gsub("_", " ", coords$states[1]), cex1 = 1/3, pos = 4)
  }
  
  # add arguments
  if(!is.null(names)){
    levels <- names
  }
  if(leg == T){ # to change according to the ladderized
    legend("bottomleft", legend = levels, fill = colors, bty = "n", cex = 1.5)
  }
}


