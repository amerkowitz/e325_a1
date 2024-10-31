library(caret)
library(randomForest)
library(terra)

oct <- rast("/cloud/project/activity08/Oct_12.tif")
terra::plot(oct)
plotRGB(oct, r=3, g=2, b=1, scale=0.7, stretch="lin")

drStack <- rast(c("/cloud/project/activity08/May_19.tif",
                  "/cloud/project/activity08/June_10.tif",
                  "/cloud/project/activity08/June_18.tif",
                  "/cloud/project/activity08/Oct_12.tif"))

terra::plot(drStack)                 
plotRGB(drStack, r=3, g=2, b=1, scale=0.7, stretch="lin") 
lc <- vect("/cloud/project/activity08/land_pts.shp")
terra::plot(lc,"LCID", add=TRUE, legend=FALSE,
            col=hcl.colors(3, palette="Harmonic"))
head(values(lc))
drStackNN <- ifel(is.na(drStack),
                  -1, drStack)

trainPts <- subset(lc, lc$train== "train", drop=FALSE)

train <- extract(drStackNN, trainPts)
trainTable <- values(trainPts)
trainDF <- na.omit(cbind(y=as.factor(trainTable$LCID), 
                         train))

# set up cross fold validation
tc <- trainControl(method = "repeatedcv",
                   number=10,
                   repeats=10)
nbands <- 20
#
rf.grid <- expand.grid(mtry=1:round(sqrt(nbands)))

set.seed(43)
rf_model <- caret::train(x=trainDF[,3:22],
                         y=as.factor(trainDF[,1]),
                         method="rf",
                         metric ="Accuracy",
                         trainControl = tc,
                         tuneGrid=rf.grid)
rf_model

rf_prediction <- terra::predict(drStackNN, rf_model)
terra::plot(rf_prediction, col=hcl.colors(3,palette="Harmonic"))

rf_prediction_mask <- mask(rf_prediction,
                           drStack[[1]], maskvalue=NaN)
terra::plot(rf_prediction_mask, col=hcl.colors(3,palette="Harmonic"))

validPts <- subset(lc, lc$train == "valid", drop=FALSE)
valid_Table <- values(validPts)
valid_rf <- extract(rf_prediction_mask, validPts)
validDF_rf <- data.frame(y=valid_Table$LCID, 
                         rf=valid_rf$class)

rf_errorM <- confusionMatrix(as.factor(validDF_rf$rf),
                             as.factor(validDF_rf$y))
rf_errorM
colnames(rf_errorM$table) <- c("field", "tree", "path")
rownames(rf_errorM$table) <- c("field", "tree", "path")
rf_errorM$table
rf_errorM


rf_prediction_mask
count_rf <- freq(rf_prediction_mask)
count_rf$area_lc <- count_rf$count * 0.4*0.4

#neural network
nnet.grid <- expand.grid(size = seq(from = 1, to = 10, by = 1), # number of neurons units in the hidden layer 
                         decay = seq(from = 0.001, to = 0.01, by = 0.001)) # regularization parameter to avoid over-fitting
# train nnet
set.seed(18)
nnet_model <- caret::train(x = trainDF[,c(3:22)], y = as.factor(trainDF[,1]),
                           method = "nnet", metric="Accuracy", 
                           trainControl = tc, tuneGrid = nnet.grid,
                           trace=FALSE)
nnet_model
# predictions
nn_prediction <- terra::predict(drStackNN, nnet_model)

nn_prediction_mask <- mask(nn_prediction,#raster to mask
                           drStack[[1]], # raster or vector with information about mask extent
                           maskvalues=NaN # value in mask that indicates an area/cell should be excluded
)
# make map
terra::plot(nn_prediction_mask, col= hcl.colors(3, palette = "Harmonic"))

# make a confusion matrix

validPts <- subset(lc, lc$train == "valid", drop=FALSE)
valid_Table <- values(validPts)
valid_nn <- extract(nn_prediction_mask, validPts)
validDF_nn <- data.frame(y=valid_Table$LCID, 
                         nn=valid_nn$class)

nn_errorM <- confusionMatrix(as.factor(validDF_nn$nn),
                             as.factor(validDF_nn$y))

nn_errorM
colnames(nn_errorM$table) <- c("field", "tree", "path")
rownames(nn_errorM$table) <- c("field", "tree", "path")
nn_errorM$table
nn_errorM
nn_errorM$table

nn_prediction_mask
count_nn <- freq(nn_prediction_mask)
count_nn$area_lc <- count_nn$count * 0.4*0.4

#cell count neural net
freq(nn_prediction)

#cell count random forest
freq(rf_prediction)

# field RF area calculation
0.4*0.4*71019

# field NN area calculation
0.4*0.4*71047

#question 2

ndvi_1 <- (drStack[[5]]-drStack[[3]])/(drStack[[5]]+drStack[[3]])
ndvi_2 <- (drStack[[10]]-drStack[[8]])/(drStack[[10]]+drStack[[8]])
ndvi_3 <- (drStack[[15]]-drStack[[13]])/(drStack[[15]]+drStack[[13]])
ndvi_4 <- (drStack[[20]]-drStack[[18]])/(drStack[[20]]+drStack[[18]])


par(mfrow=c(1,2))
terra::plot(ndvi_1)
terra::plot(ndvi_2)
terra::plot(ndvi_3)
terra::plot(ndvi_4)


#question 3
nnet.grid2 <- expand.grid(size = seq(from = 4, to = 13, by = 1), # number of neurons units in the hidden layer 
                         decay = seq(from = 0.001, to = 0.01, by = 0.001)) # regularization parameter to avoid over-fitting
# train nnet
set.seed(18)
nnet_model2 <- caret::train(x = trainDF[,c(3:22)], y = as.factor(trainDF[,1]),
                           method = "nnet", metric="Accuracy", 
                           trainControl = tc, tuneGrid = nnet.grid2,
                           trace=FALSE)
nnet_model2

nn_prediction2 <- terra::predict(drStackNN, nnet_model2)

nn_prediction_mask2 <- mask(nn_prediction2,#raster to mask
                           drStack[[1]], # raster or vector with information about mask extent
                           maskvalues=NaN # value in mask that indicates an area/cell should be excluded
)
# make map
terra::plot(nn_prediction_mask2, col= hcl.colors(3, palette = "Harmonic"))


nn_prediction_mask2
count_nn2 <- freq(nn_prediction_mask2)
count_nn2$area_lc <- count_nn2$count * 0.4*0.4
