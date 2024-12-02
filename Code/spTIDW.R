spTIDW <- function(Predict.Coor,  # Transformed the coordinates of the villages by spTransform() function of the R package sp, resulting in the distance in miles
                   Grid.Coord,    # Transformed coordinates of grids that were defined by spatial grid-level data
                   Grid.Data,     # Spatial grid-level outputs
                   range = 50,    # Spatial range
                   col_var = "",  # The variable names used to merge two datasets
                   distance.scale = 1e3,  # to Kilometers
                   from.var = "",   # Downscaled variable names
                   to.var = ""      # New variable names
                   ){
  setDF(Predict.Coor)
  setDF(Grid.Coord)
  n.row <- nrow(Predict.Coor)
  N.BAUs <- nrow(Grid.Coord)

  if("ID" %nin% colnames(Predict.Coor)){
    Predict.Coor$ID <- 1:nrow(Predict.Coor)
  }
  if("ID" %nin% colnames(Grid.Coord)){
    Grid.Coord$ID <- 1:nrow(Grid.Coord)

  }
  if("ID" %in% colnames(Grid.Data)){
    Grid.Data$ID <- NULL
  }

  Predict_D_dis <-  matrix(0, nrow = n.row, ncol = N.BAUs)

  D0 <- fields::rdist(Predict.Coor[, col_var], Grid.Coord[, col_var])/distance.scale



  Dist <- vector()
  for(pre in 1:n.row)
  {

    index <- which(D0[pre, ] <= range)

    tem.dis <- D0[pre, index]
    Dist[pre] <- max(tem.dis)

    tem.dis <- ifelse(tem.dis == 0, 1e-10, tem.dis)

    Predict_D_dis[pre, index] <- (1/tem.dis)/sum(1/tem.dis)


    print(max(D0[pre, index]))
    print(mean(D0[pre, index]))
    cat("...\n")
  }
  ###################################################################
  ###################################################################
  DATE_TIME <- unique(Grid.Data$DATE_TIME) %>% sort()

  Nt <- length(DATE_TIME)
  date.time <- data.frame(time.index = 1:Nt,
                          time.scale = seq(0, 1, , Nt),
                          DATE_TIME = DATE_TIME)
  model_output <- Grid.Data %>% left_join(date.time, by = c("DATE_TIME")) %>%
    left_join(Grid.Coord[, c(col_var, "ID")],
              by = col_var) #%>% setDF()
  setDT(model_output)
  model_output <- model_output[!is.null(ID)]
  ###################################################################
  ###################################################################
  # ID <- unique(model_output$ID)

  pb <- progress_bar$new(format = "|:bar| :current/:total (:percent in :elapsed)"
                         , total = Nt*length(from.var)
                         , clear = FALSE
                         # , width= para.ens$Nt
  )
  for (p in 1:length(from.var)) {
    model_output_Xts <- matrix(NA, nrow = Nt, ncol = N.BAUs)
    for(t in 1:Nt)
    {
      pb$tick()
      Sys.sleep(1 / (2*(Nt*p)))
      model_output_Xts[t, ] <- dcast(model_output[time.index == t, ]
                                     , . ~ ID
                                     , fun.aggregate = mean
                                     , value.var = from.var[p]
      )[1, 2:(N.BAUs + 1)]  %>% as.numeric()

    }

    temp1 <- temp2 <- Predict.Coor


    for(t in 1:Nt)
    {
      if(t == 1)
      {
        model_output_dis <- Predict_D_dis %*% ifelse(is.na(model_output_Xts[t, ]),
                                                     median(model_output_Xts[t, ], na.rm = TRUE),
                                                     model_output_Xts[t, ])

        temp1$model_output <- model_output_dis
        temp1$DATE_TIME <- as.Date(DATE_TIME[t])
        temp1$Distance <- Dist
      }else{
        temp1 <- rbind(temp1,
                       cbind(temp2, data.frame(model_output = Predict_D_dis %*% ifelse(is.na(model_output_Xts[t, ]),
                                                                                       median(model_output_Xts[t, ], na.rm = TRUE),
                                                                                       model_output_Xts[t, ]),
                                               DATE_TIME = as.Date(DATE_TIME[t]),
                                               Distance = Dist
                       )))

      }
    }
    temp1$YEAR = lubridate::year(temp1$DATE_TIME)
    temp1$MONTH = lubridate::month(temp1$DATE_TIME)
    temp1$DAY = lubridate::day(temp1$DATE_TIME)
    temp1$YEAR_MONTH = paste0(temp1$YEAR, ifelse(temp1$MONTH < 10,
                                                 paste0("0",
                                                        as.character(temp1$MONTH)
                                                 ),
                                                 as.character(temp1$MONTH))) %>% as.numeric()
    temp1 <- as.data.table(temp1)
    setnames(temp1, "model_output", to.var[p])

    if(p == 1){
      Result <- temp1
    }else{
      setDF(temp1)
      Result <- Result %>% left_join(temp1[, c("ID", "LON_X", "LAT_Y",
                                               "DATE_TIME", to.var[p])],
                                     by =c("ID", "LON_X", "LAT_Y",
                                           "DATE_TIME"))
    }
  }
  setDT(Result)
  return(Result)
}
