## 평가 : 5. 컴퓨터 비전을 위한 딥러닝 컨볼루션 엠니스트 511. 5-2 

install.packages("dplyr")
library(dplyr)
install.packages("keras")
install.packages("tensorflow")

library(tensorflow)
install_tensorflow()

library(keras)
install_keras()


### 신경망 시작하기

mnist
## 데이터 준비
mnist <- dataset_mnist()
train_images <- mnist$train$x
train_labels <- mnist$train$y
test_images <- mnist$test$x
test_labels <- mnist$test$y

str(train_images)

## 네트워크 아키텍처 구축
network <- keras_model_sequential() %>%
  layer_dense(units = 512, activation = "relu", input_shape = c(28 * 28)) %>%
  layer_dense(units = 10, activation = "softmax")

## 컴파일
network %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

## 이미지 데이터 준비
train_images <- array_reshape(train_images, c(60000, 28 * 28))
train_images <- train_images / 255

test_images <- array_reshape(test_images, c(10000, 28 * 28))
test_images <- test_images / 255

## 라벨 준비
train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)

## 모델 피팅
network %>% fit(train_images, train_labels, epochs = 5, batch_size = 128)

## 테스트 데이터 적합
metrics <- network %>% evaluate(test_images, test_labels)
metrics


##### Convnet

## 소규모 convnet 인스턴스화
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = c(28, 28, 1)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu")
model

## convnet 위에 분류자 추가
model <- model %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax")

## MNIST 이미지에 대한 convnet 학습
mnist <- dataset_mnist()
c(c(train_images, train_labels), c(test_images, test_labels)) %<-% mnist
train_images <- array_reshape(train_images, c(60000, 28, 28, 1))
train_images <- train_images / 255
test_images <- array_reshape(test_images, c(10000, 28, 28, 1))
test_images <- test_images / 255
train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)
model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)
model %>% fit(
  train_images, train_labels,
  epochs = 1, batch_size=10
)

## 모델 평가
results <- model %>% evaluate(test_images, test_labels)
results

## 모델 풀링
model_no_max_pool <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = c(28, 28, 1)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu")
model_no_max_pool

## cats and dog 데이터 준비
original_dataset_dir <- "c:/rwork/dl_data/original_data/train"
base_dir <- "c:/rwork/dl_data/cats_and_dogs_small"
dir.create(base_dir)
train_dir <- file.path(base_dir, "train")
dir.create(train_dir)
validation_dir <- file.path(base_dir, "validation")
dir.create(validation_dir)
test_dir <- file.path(base_dir, "test")
dir.create(test_dir)
train_cats_dir <- file.path(train_dir, "cats")
dir.create(train_cats_dir)
train_dogs_dir <- file.path(train_dir, "dogs")
dir.create(train_dogs_dir)
validation_cats_dir <- file.path(validation_dir, "cats")
dir.create(validation_cats_dir)
validation_dogs_dir <- file.path(validation_dir, "dogs")
dir.create(validation_dogs_dir)
test_cats_dir <- file.path(test_dir, "cats")
dir.create(test_cats_dir)
test_dogs_dir <- file.path(test_dir, "dogs")
dir.create(test_dogs_dir)
fnames <- paste0("cat.", 1:1000, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(train_cats_dir))
fnames <- paste0("cat.", 1001:1500, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(validation_cats_dir))
fnames <- paste0("cat.", 1501:2000, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(test_cats_dir))
fnames <- paste0("dog.", 1:1000, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(train_dogs_dir))
fnames <- paste0("dog.", 1001:1500, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(validation_dogs_dir))
fnames <- paste0("dog.", 1501:2000, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(test_dogs_dir))

## 데이터필드 확인
cat("total training cat images:", length(list.files(train_cats_dir)), "\n")

cat("total training dog images:", length(list.files(train_dogs_dir)), "\n")

cat("total validation cat images:",
    length(list.files(validation_cats_dir)), "\n")

cat("total validation dog images:",
    length(list.files(validation_dogs_dir)), "\n")

cat("total test cat images:", length(list.files(test_cats_dir)), "\n")

cat("total test dog images:", length(list.files(test_dogs_dir)), "\n")

## 분류를 위한 인스턴스화
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = c(150, 150, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

summary(model)


## 학습을 위한 모델 구성
model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(lr = 1e-4),
  metrics = c("acc")
)

## 데이터 전처리
train_datagen <- image_data_generator(rescale = 1/255)             
validation_datagen <- image_data_generator(rescale = 1/255)        

train_generator <- flow_images_from_directory(
  train_dir,                                                    
  train_datagen,                                                
  target_size = c(150, 150),                                    
  batch_size = 20,                                              
  class_mode = "binary"
)

validation_generator <- flow_images_from_directory(
  validation_dir,
  validation_datagen,
  target_size = c(150, 150),
  batch_size = 20,
  class_mode = "binary"
)

batch <- generator_next(train_generator)
str(batch)

## 배치 새성기를 사용한 모델 피팅
history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 10,
  epochs = 2,
  validation_data = validation_generator,
  validation_steps = 10
)

model %>% save_model_hdf5("cats_and_dogs_small_1.h5")

plot(history)

## 데이터 증가 구성 설정
datagen <- image_data_generator(
  rescale = 1/255,
  rotation_range = 40,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  shear_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE,
  fill_mode = "nearest"
)

## 무작위 데이터 증가를 통한 고양이 사진 생성
fnames <- list.files(train_cats_dir, full.names = TRUE)
img_path <- fnames[[3]]                                         

img <- image_load(img_path, target_size = c(150, 150))            
img_array <- image_to_array(img)                                      
img_array <- array_reshape(img_array, c(1, 150, 150, 3))              
augmentation_generator <- flow_images_from_data(img_array,
                                                generator = datagen, batch_size = 1)

op <- par(mfrow = c(2, 2), pty = "s", mar = c(1, 0, 1, 0))      
for (i in 1:4) {                                                
  batch <- generator_next(augmentation_generator)               
  plot(as.raster(batch[1,,,]))                                  
}                                                               
par(op)                                                           
## 드롭 아웃을 포함하는 convnet 정의
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = c(150, 150, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")
model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(lr = 1e-4),
  metrics = c("acc")
)

## convnet 훈련
datagen <- image_data_generator(
  rescale = 1/255,
  rotation_range = 40,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  shear_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE
)
test_datagen <- image_data_generator(rescale = 1/255)
train_generator <- flow_images_from_directory(train_dir, datagen, target_size = c(150, 150), batch_size = 32, class_mode = "binary")
validation_generator <- flow_images_from_directory(
  validation_dir,
  test_datagen,
  target_size = c(150, 150),
  batch_size = 32,
  class_mode = "binary"
)
history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 30,
  epochs = 30,
  validation_data = validation_generator,
  validation_steps = 10
)

model %>% save_model_hdf5("cats_and_dogs_small_2.h5")

plot(history)


