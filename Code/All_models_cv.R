rm(list = ls())
# source("./LoadPackages/RDependPackages.R")
h2o.no_progress()
#------------------------------load datasets------------------------------------
load('./data/Mod_SCORE_Kenya_Tanzania.RData')


#-----------------------------Parameter setting---------------------------------

#Select the arms used to develop prediction models
study_arm    <- c(1:6)

#Use definition I of persistent hotspots? If yes, Used.PHS.I = TRUE, FALSE otherwise (definition II).
Used.PHS.I  <- TRUE


#Select Predictor configurations used to develop prediction models
Combined.Predictors   <- c(0:7)
# 0. Predictors from ``baseline infection data''
# 1. Predictors from ``Infection data around villages'' and ``baseline infection data''
# 2. Predictors from ``Geography" and ``baseline infection data''
# 3. Predictors from ``Environment" and ``baseline infection data''
# 4. Predictors from ``Agriculture" and ``baseline infection data''
# 5. Predictors from ``Biology" and ``baseline infection data''
# 6. Predictors from ``Society" and ``baseline infection data''
# 7. Predictors from ``All"


#Select Scenarios used in cross-validation (CV)
Scenarios    <- c(1:5)
#@ 1. The within-country setting I (Tanzania): ---> Training data from Tanzania and testing data also from Tanzania
#@ 2. The within-country setting II (Kenya)    ---> Training data from Kenya and testing data also from Kenya
#@ 3. Combined-countries:                      ---> Training data from both countries and testing data also from both countries
#@ 4. Between-countries (Between I):           ---> Training data only from Kenya, predicting hotspots only from Tanzania
#@ 5. Between-countries (Between II):          ---> Training data only from Tanzania, predicting hotspots only from Kenya


#Set sample size for training data
train.size   <- 0.7                           # 70% data from each scenario were randomly selected to train models, while 30% were used to test the developed models

#Set CV numbers
nCV           <- 200;

#Whether to adjust the imbalanced training data
imbalance   <- F

#Select models used to run in CV
h2o.run          <- T  #Including GBM, RF, Tree, Logit, Logit.Lasso, LGT, SVM, Ensemble, and DNN
LogitGPs.run     <- T
Probit.run       <- T
reglogit.run     <- T
dynaTree.run     <- T
sparseSVM.run    <- T


#Assess importance?
Variable.importance <- F

# scale predictors?
predictor.scale    <- F

# Infection data used for developing prediction models only from which years
year         <- 1;

setDF(Mod_SCORE_Kenya_Tanzania)
Region     <- c(sort(unique(Mod_SCORE_Kenya_Tanzania$region)), "Combined")
region_num <- 1:length(Region)

#-----------------------------Define hotspots---------------------------------
#---- Definition I ----
# Condition 1.1
Mod_SCORE_Kenya_Tanzania$PHS_Year15_PreReduce10 <-
  ifelse(
    (Mod_SCORE_Kenya_Tanzania$Prevalence_Y1 - Mod_SCORE_Kenya_Tanzania$Prevalence_Y5)/
     Mod_SCORE_Kenya_Tanzania$Prevalence_Y1 >= 0.35, 0, 1)

# Condition 1.2
Mod_SCORE_Kenya_Tanzania$PHS_Year15_IntReduce10 <-
  ifelse(
    (Mod_SCORE_Kenya_Tanzania$Mean_intensity_epg_Overall_Y1 -
     Mod_SCORE_Kenya_Tanzania$Mean_intensity_epg_Overall_Y5)/
     Mod_SCORE_Kenya_Tanzania$Mean_intensity_epg_Overall_Y1  >= 0.5, 0, 1)

#---- Definition II ----
# Condition 2.1
Mod_SCORE_Kenya_Tanzania$PHS_Year5_PreReduce10 <-
  ifelse(Mod_SCORE_Kenya_Tanzania$Prevalence_Y5 >= 10, 1, 0)
# Condition 2.2
Mod_SCORE_Kenya_Tanzania$Persistent_Hotspot_Year5_IntReduce10 <-
  ifelse(Mod_SCORE_Kenya_Tanzania$Mean_intensity_epg_Overall_Y5 >= 1, 1, 0)


# ---- Study outcome: Persistent_Hotspot ----
if(Used.PHS.I){
  Mod_SCORE_Kenya_Tanzania$Persistent_Hotspot <- ifelse(
   (Mod_SCORE_Kenya_Tanzania$PHS_Year15_PreReduce10 == 1)|
   (Mod_SCORE_Kenya_Tanzania$PHS_Year15_IntReduce10 == 1),
    1, 0)
}else{
  Mod_SCORE_Kenya_Tanzania$Persistent_Hotspot <- ifelse(
    (Mod_SCORE_Kenya_Tanzania$PHS_Year15_PreReduce10 == 1)&
    (Mod_SCORE_Kenya_Tanzania$Prevalence_Y5 > 10),
    1, 0)
}


#---- Summary proportion of PHSs ----
xtabs(~ Persistent_Hotspot + region, data = Mod_SCORE_Kenya_Tanzania[Mod_SCORE_Kenya_Tanzania$Study_arm %in% study_arm,])


# ftable(Mod_SCORE_Kenya_Tanzania)

# ---- Select arms ----
all_Mod_SCORE_Data <- Mod_SCORE_Kenya_Tanzania %>% filter(Study_arm %in% c(study_arm))

# ---- select predictors used to scale ----
setDF(all_Mod_SCORE_Data)
ind0 <- which(colnames(all_Mod_SCORE_Data) %nin% c("LON_X"     ,
                                                   "LAT_Y"     ,
                                                   "Study_arm" ,
                                                   "Village_ID",
                                                   "region"    ,
                                                   "cluster"   ,
                                                   "Persistent_Hotspot"))


Covariate <- colnames(all_Mod_SCORE_Data)[c(ind0)];
Cov.Index <- which(base::colnames(all_Mod_SCORE_Data) %in% Covariate)

# ---- Scale predictors ----
if(predictor.scale){
  if(length(Cov.Index) > 2){
    mean_covariates <- apply(all_Mod_SCORE_Data[, Cov.Index], 2, mean)
    sd_covariates <- apply(all_Mod_SCORE_Data[, Cov.Index], 2, sd)
    all_Mod_SCORE_Data[, Cov.Index] <- scale(all_Mod_SCORE_Data[, Cov.Index],
                                             center = mean_covariates,
                                             scale = sd_covariates)
  }
}


# ---- Select infection data ----
if(year == 1){
  Baseline.Infection.Data <-  c("Prevalence_Y1"
                               , "Prevalence_gt200epg_Y1"
                               , "Mean_intensity_epg_Overall_Y1"
                               )
}

if(year == 3){
  # Predictors used in Shen, et.al. (2020)'s method
  Baseline.Infection.Data <-  c("Prevalence_Y1"
                               , "Prevalence_gt200epg_Y1"
                               , "Prevalence_gt400epg_Y1"
                               , "Mean_intensity_epg_Overall_Y1"
                               , "Y1_Coverage_SAC"
                               , "Y3_Coverage_SAC"
                               , "Prevalence_Y3"
                               , "Prevalence_gt200epg_Y3"
                               , "Prevalence_gt400epg_Y3"
                               , "Mean_intensity_epg_Overall_Y3"
                               , "coverchg_y1y3"
                               , "prevchg_y1y3"
                               , "intenchg_y1y3"
                               , "gt200chg_y1y3"
                               , "gt400chg_y1y3"
                                )
  all_Mod_SCORE_Data$coverchg_y1y3 <- (all_Mod_SCORE_Data$Y1_Coverage_SAC - all_Mod_SCORE_Data$Y3_Coverage_SAC)/ifelse(all_Mod_SCORE_Data$Y1_Coverage_SAC == 0,
                                                                                                                       1e-2, all_Mod_SCORE_Data$Y1_Coverage_SAC)

  all_Mod_SCORE_Data$prevchg_y1y3 <- (all_Mod_SCORE_Data$Prevalence_Y1 - all_Mod_SCORE_Data$Prevalence_Y3)/ifelse(all_Mod_SCORE_Data$Prevalence_Y1 == 0,
                                                                                                                  1e-2, all_Mod_SCORE_Data$Prevalence_Y1)
  all_Mod_SCORE_Data$intenchg_y1y3 <- (all_Mod_SCORE_Data$Mean_intensity_epg_Overall_Y1 - all_Mod_SCORE_Data$Mean_intensity_epg_Overall_Y3)/ifelse(all_Mod_SCORE_Data$Mean_intensity_epg_Overall_Y1 == 0,
                                                                                                                                                   1e-2, all_Mod_SCORE_Data$Mean_intensity_epg_Overall_Y1)
  all_Mod_SCORE_Data$gt200chg_y1y3 <- (all_Mod_SCORE_Data$Prevalence_gt200epg_Y1 - all_Mod_SCORE_Data$Prevalence_gt200epg_Y3)/ifelse(all_Mod_SCORE_Data$Prevalence_gt200epg_Y1 == 0,
                                                                                                                                     1e-2, all_Mod_SCORE_Data$Prevalence_gt200epg_Y1)
  all_Mod_SCORE_Data$gt400chg_y1y3 <- (all_Mod_SCORE_Data$Prevalence_gt400epg_Y1 - all_Mod_SCORE_Data$Prevalence_gt400epg_Y3)/ifelse(all_Mod_SCORE_Data$Prevalence_gt400epg_Y1 == 0,
                                                                                                                                     1e-2, all_Mod_SCORE_Data$Prevalence_gt400epg_Y1)


  ind <- which(colnames(all_Mod_SCORE_Data) %in% Baseline.Infection.Data)
  setDF(all_Mod_SCORE_Data)
  all_Mod_SCORE_Data[, ind]


}

# ---- Predictors from infection data around villages ----
Predictor.Around.Villages   <-  c( "sim_Prevalence_Y1"                       # Spatially weighted prevalence
                                  , "sim_Prevalence_gt200epg_Y1"             # Spatially weighted prevalence with infections >= 200 epg
                                  , "sim_s_Mean_intensity_epg_Overall_Y1"    # Spatially weighted intensity
                                  , "cluster"                                # A binary varibale generated by the K-means method based on the baseline infection data 
                                  )
# ---- Predictors from Geography ----
Predictor.Geography   <-  c("Longitude", "Latitude", "sim_Altitude")

# The spatially weighted predictors below were detailed in Table S2 of the supporting information file.
# ---- Spatially weighted predictors from Environment ----
Predictor.Environment   <-  c(
                              "sim_12_PM25"
                              , "sim_12_evatc.max_max"
                              , "sim_12_stl1.max_max"
                              , "sim_12_ssrd.max_max"
                              , "sim_12_sp.min_min"
                              , "sim_12_tp.min_min"
                              , "sim_12_d2m.min_min"
                              , "sim_12_sp.max_max"
                              , "sim_12_tp.max_max"
                              , "sim_12_d2m.max_max"
                              , "sim_12_beam_horizontal_irradiation_max_max"
                              , "sim_12_beam_normal_irradiation_max_max"
                              , "sim_12_diffuse_horizontal_irradiation_max_max"
                              , "sim_12_global_horizontal_irradiation_sum_sum"
                            )



# ---- Spatially weighted predictors from agriculture ----
Predictor.Agriculture <-  c(
                              "sim_12_Cropland"
                            , "sim_12_PermanentWater"
                            , "sim_12_SeasonalWater"
                            , "sim_12_scientific_min"
                            , "sim_12_scientific_max"
                            , "sim_12_return_period_min"
                            , "sim_12_return_period_max"
                            , "sim_12_anomaly_min"
                            , "sim_12_anomaly_max"
                          )

# ---- Spatially weighted predictors from biology ----
Predictor.Biology <-  c("sim_12_Forest_Cover", "sim_12_all_Mammals")

# ---- Spatially weighted predictors from society ----
Predictor.Society <- c(
                        "sim_12_Human_Mod_Ter"
                        , "sim_12_BuiltUp"
                        , "sim_12_Popu_Density"
                        , "sim_12_Deprivation_Index"
                        , "sim_12_Built_Component"
                        , "sim_12_Child_Dependency_Ratio"
                        , "sim_12_Nighttime_Lights_Slope"
                      )



#- Scenario ----

Results <- NULL
for(Com.Pred in c(Combined.Predictors)){

  # Using the baseline infection data only to develop prediction models
  if(Com.Pred == 0){
    Covariate <- c(Baseline.Infection.Data)
  }

  # Combining the baseline infection data with the predictors from infection data around villages to develop prediction models
  if(Com.Pred == 1){
    Covariate <- c(Baseline.Infection.Data, Predictor.Around.Villages)
  }

  # Combining the baseline infection data with the predictors from Geography to develop prediction models
  if(Com.Pred == 2){
    Covariate <- c(Baseline.Infection.Data, Predictor.Geography)

  }

  # Combining the baseline infection data with the predictors from Environment to develop prediction models
  if(Com.Pred == 3){
    Covariate <- c(Baseline.Infection.Data, Predictor.Environment)

  }

  # Combining the baseline infection data with the predictors from Agriculture to develop prediction models
  if(Com.Pred == 4){
    Covariate <- c(Baseline.Infection.Data, Predictor.Agriculture)

  }

  # Combining the baseline infection data with the predictors from Biology to develop prediction models
  if(Com.Pred == 5){
    Covariate <- c(Baseline.Infection.Data, Predictor.Biology)

  }

  # Combining the baseline infection data with the predictors from Society to develop prediction models
  if(Com.Pred == 6){
    Covariate <- c(Baseline.Infection.Data, Predictor.Society)
  }

  # Combining the baseline infection data with all predictors to develop prediction models
  if(Com.Pred == 7){
    Covariate <- c(Baseline.Infection.Data,
                   Predictor.Around.Villages,
                   Predictor.Geography,
                   Predictor.Environment,
                   Predictor.Agriculture,
                   Predictor.Biology,
                   Predictor.Society)
  }


  Cov.Index.Latitude <- which(base::colnames(all_Mod_SCORE_Data) %in% c("Latitude"))
  Cov.Index.unchange <- Cov.Index <- which(base::colnames(all_Mod_SCORE_Data) %in% c(Covariate))

  setDT(all_Mod_SCORE_Data)

  Temp.0 <- NULL
  Ken.0 <- all_Mod_SCORE_Data[((region %in% Region[1]) & (Persistent_Hotspot == 0))]
  Tan.1 <- all_Mod_SCORE_Data[((region %in% Region[2]) & (Persistent_Hotspot == 1))]

  for(k in c(Scenarios)){
    Cov.Index <- Cov.Index.unchange
    if(k <= 2){
      # Within-country
      Da <- all_Mod_SCORE_Data %>% filter(region %in% Region[k])
    }else{
      Da <- all_Mod_SCORE_Data
    }
    
    # The prediction models investigated by previous works
    GBM.accuracy        <- RF.accuracy        <- tree.accuracy       <-
    Logit.accuracy      <- lasso.accuracy     <- lgt.accuracy        <-
    svm.accuracy        <- ENS.accuracy        <-  vector()

    # The serveral different prediction models included by this work
    LogitGPs.accuracy   <- sparseSVM.accuracy <- dynaTrees.accuracy <- 
    reglogit.accuracy  <- probit.accuracy     <- CNN.accuracy       <-  vector()

    beta.Rank <- list()


    #---- Run prediction models with different predictor configurations ----
    if(h2o.run|Variable.importance){
      h2o.init(max_mem_size = "10G")
    }
    for(r in 1:nCV){
      formula.LogitGPs  <- paste0("Persistent_Hotspot ~ 1", paste(" + ",  Covariate, collapse = " "))
      setDF(Da)
      if(k <= 3){
        index.0 <- which(Da$Persistent_Hotspot == 0)
        index.1 <- which(Da$Persistent_Hotspot == 1)
        all.Ind <- c(index.0, index.1)
        length(unique(all.Ind))
        p0 <- length(index.0) /(length(index.0) + length(index.1))
        p1 <- length(index.1) /(length(index.0) + length(index.1))


        ind.train <- c(sample(index.0, p0*(train.size*length(all.Ind))),
                       sample(index.1, p1*(train.size*length(all.Ind))))


        ind <- all.Ind[which(all.Ind %nin% ind.train)]

        ind.valid <- ind.train


        Da.mod  <- Da[ind.train, ]
        Da.test <- Da[ind[which(ind %in% ind)], ]

        Reg     <- Region[k]
      }else if (k == 4){
        temp    <- Da %>% filter(region %in% Region[1])
        temp    <- Da %>% filter(region %nin% Region[1])
        Da.test <- temp[sample(1:nrow(temp), ceiling(nrow(temp)*(1 - train.size)), replace = F), ]

        # Reg       <- paste0("Between - training with ", Region[1])
        Reg       <- paste0("Between I")
        Cov.Index <- Cov.Index[which(Cov.Index %nin% Cov.Index.Latitude)]
        formula.LogitGPs  <- paste0("Persistent_Hotspot ~ 1", paste(" + ",  Covariate[which(Covariate %nin% "Latitude")], collapse = " "))
      }else if (k == 5){
        temp    <- Da %>% filter(region %in% Region[2])
        temp    <- Da %>% filter(region %nin% Region[2])
        Da.test <- temp[sample(1:nrow(temp),
                               ceiling(nrow(temp)*(1 - train.size)),
                               replace = F), ]
        Reg       <- paste0("Between II")
        Cov.Index         <- Cov.Index[which(Cov.Index %nin% Cov.Index.Latitude)]
        formula.LogitGPs  <- paste0("Persistent_Hotspot ~ 1", paste(" + ",  Covariate[which(Covariate %nin% "Latitude")], collapse = " "))
      }




      ind.y <- which(colnames(Da.mod) %in% "Persistent_Hotspot")
      prior.r <- round((sum(Da.mod$Persistent_Hotspot)/nrow(Da.mod)), 3)
      cat(paste0( "\n\n Dataset are from the ", Reg, " scenario ... ",
                  "\n Year                           = 201", 0 + year,
                  "\n Ratio of 1 in the training set = ",
                  round((sum(Da.mod$Persistent_Hotspot)/nrow(Da.mod)), 3),
                  "\n Ratio of 1 in the testing set  = ",
                  round(sum(Da.test$Persistent_Hotspot)/nrow(Da.test), 3)),
                  "\n Used.PHS.I                     =", Used.PHS.I,
                  "\n Predictors                     =", Com.Pred,
                  "\n Simulation                     =", r,
                  " \n\n")

      if(imbalance){
        hmult.majo <- 1e0
        hmult.mino <- 1e1
        p      <- 0.40
        if(k == 3){
          hmult.majo <- 1e-3
          hmult.mino <- 1e-3
          p      <- 0.50
        }

        if(k >= 4){
          p          <- 0.5
          hmult.majo <- 1e2
          hmult.mino <- 1e1
        }
        Da.mod.1 <- ROSE::ROSE(Persistent_Hotspot ~ .,
                               N          = 300,
                               hmult.majo = hmult.majo,
                               hmult.mino = hmult.mino,
                               data = Da.mod[, c(ind.y, Cov.Index)],
                               p    =  p,
                               seed = r)$data

        seq <- table(Da.mod.1$Persistent_Hotspot)
        print(seq)

        if((seq[[1]] == 300)){
          cat("...")
          Da.mod <- Da.mod[, c(ind.y, Cov.Index)]
        }else{
          Da.mod <- Da.mod.1
        }

       train.Data <-  Da.mod
      }else{
        train.Data    <- Da.mod[, c(ind.y, Cov.Index)]
      }
        test.Data     <- Da.test[, c(ind.y, Cov.Index)]

      ind.1 <- 0

      y.test <- test.Data$Persistent_Hotspot
      train.Data$Persistent_Hotspot <- as.factor(train.Data$Persistent_Hotspot)

      test.Data$Persistent_Hotspot     <- as.factor(test.Data$Persistent_Hotspot)
      if(h2o.run){
        train.h2o    <- as.h2o(train.Data)
        test.h2o     <- as.h2o(test.Data)

        nfolds <- 5
        if(!Variable.importance){
        model.gbm   <- h2o.gbm(x              = 2:(length(Cov.Index) + ind.1 + 1),
                               y              = 1,
                               nfolds         = nfolds,
                               distribution   = "bernoulli",
                               keep_cross_validation_predictions = TRUE,
                               training_frame = train.h2o)
        model.rf    <- h2o.randomForest(x              = 2:(length(Cov.Index) + ind.1 + 1),
                                        y              = 1,
                                        training_frame = train.h2o,
                                        # ntrees = 500,
                                        nfolds         = nfolds,
                                        keep_cross_validation_predictions = TRUE,
                                        seed           = r)
        model.tree  <- h2o.randomForest(x                = 2:(length(Cov.Index) + ind.1 + 1),
                                        y                = 1,
                                        training_frame   = train.h2o,
                                        validation_frame = train.h2o,
                                        ntrees           = 1,
                                        sample_rate      = 1,
                                        nfolds           = nfolds,
                                        keep_cross_validation_predictions = TRUE,
                                        seed             = r)
        model.logit <- h2o.glm(x              = 2:(length(Cov.Index) + ind.1 + 1),
                               y              = 1,
                               family         = "binomial",
                               nfolds         = nfolds,
                               keep_cross_validation_predictions = TRUE,
                               training_frame = train.h2o)
        model.lasso <- h2o.glm(x              = 2:(length(Cov.Index) + ind.1 + 1),
                               y              = 1,
                               family         = "binomial",
                               nfolds         = nfolds,
                               alpha          = 1,
                               keep_cross_validation_predictions = TRUE,
                               training_frame = train.h2o)
        model.lgt   <- h2o.glm(x              = 2:(length(Cov.Index) + ind.1 + 1),
                               y              = 1,
                               family         = "binomial",
                               nfolds         = nfolds,
                               lambda         = 0,
                               keep_cross_validation_predictions = TRUE,
                               training_frame = train.h2o)


        model.svm <- e1071::svm(Persistent_Hotspot ~ .,
                                data   = train.Data,#degree = 5,
                                kernel = "linear",
                                cross  = 10,
                                scale  = T)

        model.ensemble <- h2o.stackedEnsemble(x                     = 2:(length(Cov.Index) + ind.1 + 1),
                                              y                     = 1,
                                              # metalearner_algorithm = "drf",
                                              training_frame        = train.h2o,
                                              base_models           = list(model.gbm, model.rf, model.lgt))

        model.DL   <- h2o.deeplearning( x              = 2:(length(Cov.Index) + ind.1 + 1),
                                        y              = 1,
                                        nfolds         = nfolds,
                                        huber_alpha    = 0.1,
                                        rho            = 1e-1,
                                        elastic_averaging   = T,
                                        activation          = "TanhWithDropout",
                                        loss                = "CrossEntropy",
                                        keep_cross_validation_predictions = TRUE,
                                        training_frame = train.h2o,
                                        seed           = r)

        h2o.test.ped.gbm   <- h2o.predict(model.gbm,   test.h2o)
        h2o.test.ped.rf    <- h2o.predict(model.rf,    test.h2o)
        h2o.test.ped.tree  <- h2o.predict(model.tree,  test.h2o)
        h2o.test.ped.logit <- h2o.predict(model.logit, test.h2o)
        h2o.test.ped.lasso <- h2o.predict(model.lasso, test.h2o)
        h2o.test.ped.lgt   <- h2o.predict(model.lgt,   test.h2o)

        h2o.test.ped.svm   <- as.numeric(as.character(predict(model.svm, test.Data)))
        h2o.test.ped.ens   <- as.numeric(as.character(predict(model.ensemble, test.h2o)))
        h2o.test.ped.DL    <- h2o.predict(model.DL,   test.h2o)


        GBM.accuracy[r]   <- sum(as.numeric(as.vector(h2o.test.ped.gbm[, 1]))   == y.test)/length(y.test)
        RF.accuracy[r]    <- sum(as.numeric(as.vector(h2o.test.ped.rf[, 1]))    == y.test)/length(y.test)
        tree.accuracy[r]  <- sum(as.numeric(as.vector(h2o.test.ped.tree[, 1]))  == y.test)/length(y.test)
        Logit.accuracy[r] <- sum(as.numeric(as.vector(h2o.test.ped.logit[, 1])) == y.test)/length(y.test)
        lasso.accuracy[r] <- sum(as.numeric(as.vector(h2o.test.ped.lasso[, 1])) == y.test)/length(y.test)
        lgt.accuracy[r]   <- sum(as.numeric(as.vector(h2o.test.ped.lgt[, 1]))   == y.test)/length(y.test)
        svm.accuracy[r]   <- sum(as.numeric(as.vector(h2o.test.ped.svm))   == y.test)/length(y.test)
        CNN.accuracy[r]   <- sum(as.numeric(as.vector(h2o.test.ped.DL))   == y.test)/length(y.test)

        ENS.accuracy[r]<- sum(as.numeric(as.vector(h2o.test.ped.ens))   == y.test)/length(y.test)
        }
      }

      if(Variable.importance){
        aml <- h2o.automl(x              = 2:(length(Cov.Index) + ind.1 + 1),
                          y              = 1,
                          training_frame = train.h2o,
                          distribution   = "bernoulli",
                          max_models = 10,
                          max_runtime_secs = 20,
                          include_algos = c("GBM"))
        varimp_heatmap <- h2o.varimp_heatmap(aml, num_of_features = 50)
        rownames(varimp_heatmap[["data"]]) <- NULL
        varimp <- as.data.frame(varimp_heatmap[["data"]]) %>%
          aggregate(value ~ feature, mean)

        setDT(varimp)
        colnames(varimp) <- c("feature", "value")

     if(r == 1){
        beta.Rank[["gbm"]] <- varimp
        beta.Rank[["gbm"]]$iter <- r
      }else{
        temp <- varimp
        temp$iter <- r
        beta.Rank[["gbm"]] <- rbind(beta.Rank[["gbm"]], temp)
       }
      }

      if(LogitGPs.run){
        kernel         <- "laplacedot"
        model.LogitGPs <- kernlab::gausspr(as.formula(formula.LogitGPs),
                                           data   = train.Data,
                                           kernel = kernel,
                                           # tol    = 1e-3,
                                           # cross  = 10,
                                           type   = "classification")

        cat("...\n")
        n1 <- ceiling(nrow(test.Data)/2)
        n2 <- nrow(test.Data)
        test.Hotspot <- c(as.numeric(as.character(predict(model.LogitGPs,
                                                          newdata = test.Data[1:n1, ]))),
                          as.numeric(as.character(predict(model.LogitGPs,
                                                          newdata = test.Data[(n1 + 1):n2, ]))))



        LogitGPs.accuracy[r] <- sum(test.Hotspot == y.test)/length(y.test)
        #-------------------------------------------------------------------

      }
      if(sparseSVM.run){
        y         <- as.numeric(as.character((train.Data$Persistent_Hotspot)))
        X         <- as.matrix(as.data.frame(train.Data[, -1]))
        sparseSVM.mod <- sparseSVM(X, y,
                                   alpha = 1,
                                   gamma = 1,
                                   nlambda = 100)

        sparseSVM.pred.fit <- predict(sparseSVM.mod,
                                      X,
                                      lambda = sparseSVM.mod$lambda)
        train.err <- NULL
        for(l in 1:length(sparseSVM.mod$lambda)){
          train.err[l] <- sum(sparseSVM.pred.fit[, l]==y)/length(y)
        }
        if(r == 1){
          beta <- sparseSVM.mod$weights[-1, which.max(train.err)]
          beta.Rank[["sparseSVM"]] <- data.frame(feature = names(sparseSVM.mod$weights[-1, which.max(train.err)]),
                                                 value = abs(as.vector(beta)))
          beta.Rank[["sparseSVM"]]$iter <- r
        }else{
          beta <- sparseSVM.mod$weights[-1, which.max(train.err)]
          temp <- data.frame(feature = names(sparseSVM.mod$weights[-1, which.max(train.err)]),
                             value = abs(as.vector(beta)))
          temp$iter <- r
          beta.Rank[["sparseSVM"]] <-  rbind(beta.Rank[["sparseSVM"]], temp)
        }

        sparseSVM.pred <- predict(sparseSVM.mod, as.matrix(as.data.frame(test.Data[, -1])),
                                  lambda = c(sparseSVM.mod$lambda[which.max(train.err)]))[, 1]

        sparseSVM.accuracy[r] <- sum(sparseSVM.pred==y.test)/length(y.test)
      }
      if(dynaTree.run){
        model.dynaTrees <- dynaTrees(X     = train.Data[, -1],
                                     y     = as.numeric((train.Data$Persistent_Hotspot)),
                                     XX    = test.Data[, -1],
                                     N     = 2000,
                                     sub = nrow(train.Data),
                                     # R     = 30,
                                     model = "class",
                                     verb  = 0,
                                     pverb = 0)
        test.Hotspot <- apply(model.dynaTrees$p, 1, which.max) - 1
        dynaTrees.accuracy[r] <- sum(test.Hotspot == y.test)/length(y.test)
      }
      if(reglogit.run){
        y         <- as.numeric(as.character((train.Data$Persistent_Hotspot)))
        X         <- as.matrix(as.data.frame(train.Data[, -1]))
        fit.logit <- glm(y~X, family = binomial(link="logit"))

        if(k %in% c(1, 3)){nu <- 1E0}
        if(k == 2){nu <- 3E0}
        if(k >= 4){nu <- 1e3}


        model.reglogit <- tryCatch({reglogit::reglogit(T         = 2000,
                                                       y         = as.numeric(as.character((train.Data$Persistent_Hotspot))),
                                                       X         = as.matrix(as.data.frame(train.Data[, -1])),
                                                       nu        = nu,
                                                       bstart    = fit.logit$coef,
                                                       nup       = NULL,
                                                       # powerprior = F,
                                                       # kmax  = 1E3,
                                                       # kappa  = 10,
                                                       normalize = T)},
                                   error = function(e)
                                     print("reglogit.ERROR"))

        if(length(model.reglogit) > 1){
          test.Hotspot <- predict(model.reglogit, XX = as.matrix(as.data.frame(test.Data[, -1])))$c
          (reglogit.accuracy[r] <- sum(test.Hotspot == y.test)/length(y.test))


          true.ind.0 <- which(y.test == 0)
          true.ind.1 <- which(y.test == 1)

          test.ind.0 <- which(test.Hotspot == 0)
          test.ind.1 <- which(test.Hotspot == 1)
        }
      }
      if(Probit.run){
        y <- 1*(as.numeric(as.character((train.Data$Persistent_Hotspot))) > 0.5)
        model.BART <- BART::pbart(x.train = as.matrix(as.data.frame(train.Data[, -1])),
                                  y.train =  y, printevery = 1e4,
                                  sparse = TRUE, augment = TRUE,
                                  x.test = as.matrix(as.data.frame(test.Data[, -1])))

        pro <- apply(model.BART$prob.train, 2, median)

        thresh <- seq(1e-5, 1, length = 50)
        fit.accuracy.svc <- vector()
        y.train <- as.numeric(as.character((train.Data$Persistent_Hotspot)))
        for(i in 1:length(thresh)){
          train.hot <- ifelse(pro >= thresh[i], 1, 0)

          fit.accuracy.svc[i]  <- sum(train.hot == y.train)/length(y.train)
        }
        thre <- thresh[which.max(fit.accuracy.svc)]

        test.Hotspot <- ifelse(apply(model.BART$prob.test, 2, median) >= thre, 1, 0)

        probit.accuracy[r] <- sum(test.Hotspot == y.test)/length(y.test)

      }


      {
        cat("...................................................\n\n")
        cat(paste0(" Mean of GBM         = ", round(mean(GBM.accuracy,          na.rm = T), 3), " ",
                   "\n Mean of RF          = ", round(mean(RF.accuracy,         na.rm = T), 3), " ",
                   "\n Mean of Tree        = ", round(mean(tree.accuracy,       na.rm = T), 3), " ",
                   "\n Mean of Logit       = ", round(mean(Logit.accuracy,      na.rm = T), 3), " ",
                   "\n Mean of LASSO       = ", round(mean(lasso.accuracy,      na.rm = T), 3), " ",
                   "\n Mean of LGT         = ", round(mean(lgt.accuracy,        na.rm = T), 3), " ",
                   "\n Mean of svm         = ", round(mean(svm.accuracy,        na.rm = T), 3), " ",
                   "\n Mean of Ensemble    = ", round(mean(ENS.accuracy,        na.rm = T), 3), " ",
                   "\n Mean of LogitGPs    = ", round(mean(LogitGPs.accuracy,   na.rm = T), 3), " ",
                   "\n Mean of sparseSVM   = ", round(mean(sparseSVM.accuracy,  na.rm = T), 3), " ",
                   "\n Mean of dynaTrees   = ", round(mean(dynaTrees.accuracy,  na.rm = T), 3), " ",
                   "\n Mean of reglogit    = ", round(mean(reglogit.accuracy,   na.rm = T), 3), " ",
                   "\n Mean of probit      = ", round(mean(probit.accuracy,     na.rm = T), 3), " ",
                   "\n Mean of CNN         = ", round(mean(CNN.accuracy,        na.rm = T), 3), " "
                   )
        )
        cat("\n\n")
        print(Temp.0)
        print(Results)
        cat("\n\n")
      }

    }

    # if(k <= 5){
    Temp.1 <- data.table( Predictors     = Com.Pred, Region = Reg,
                          GBM          = round(mean(GBM.accuracy,        na.rm = T), 3),
                          RF           = round(mean(RF.accuracy,         na.rm = T), 3),
                          Tree         = round(mean(tree.accuracy,       na.rm = T), 3),
                          Logit        = round(mean(Logit.accuracy,      na.rm = T), 3),
                          LASSO        = round(mean(lasso.accuracy,      na.rm = T), 3),
                          LGT          = round(mean(lgt.accuracy,        na.rm = T), 3),
                          SVM          = round(mean(svm.accuracy,        na.rm = T), 3),
                          Ensemble     = round(mean(ENS.accuracy,        na.rm = T), 3),
                          LogitGPs     = round(mean(LogitGPs.accuracy,   na.rm = T), 3),
                          sparseSVM    = round(mean(sparseSVM.accuracy,  na.rm = T), 3),
                          dynaTrees    = round(mean(dynaTrees.accuracy,  na.rm = T), 3),
                          reglogit     = round(mean(reglogit.accuracy,   na.rm = T), 3),
                          probit       = round(mean(probit.accuracy,     na.rm = T), 3),
                          CNN          = round(mean(CNN.accuracy,        na.rm = T), 3)
    )
    Temp.0 <- rbind(Temp.0, Temp.1)


  }
  Results <- rbind(Results, Temp.0)
  setorderv(Results, c("Region", "Predictors"))
}
print(Results)

