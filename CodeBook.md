
## Please find below the variable used in the tidy dataset (please refer to tidydata.txt).

### Dimensions : 

* `Subject` - list of number between 1 and 30, corresponding to the subject
* `Activity` - The activity corresponding to subject's movements. There are 6 differents activities : 

* `WALKING`
* `WALKING_UPSTAIRS`
* `WALKING_DOWNSTAIRS` 
* `SITTING` 
* `STANDING` 
* `LAYING` 

### Measurements

* tBodyAcc-mean()-X1
* tBodyAcc-mean()-Z3
* tBodyAcc-std()-Y5
* tBodyAcc-mad()-X7
* tBodyAcc-mad()-Z9
* tBodyAcc-max()-Y11
* tBodyAcc-min()-X13
* tBodyAcc-min()-Z15
* tBodyAcc-energy()-X17
* tBodyAcc-energy()-Z19
* tBodyAcc-iqr()-Y21
* tBodyAcc-entropy()-X23
* tBodyAcc-entropy()-Z25
* tBodyAcc-arCoeff()-X,227
* tBodyAcc-arCoeff()-X,429
* tBodyAcc-arCoeff()-Y,231
* tBodyAcc-arCoeff()-Y,433
* tBodyAcc-arCoeff()-Z,235
* tBodyAcc-arCoeff()-Z,437
* tBodyAcc-correlation()-X,Z39
* tGravityAcc-mean()-X41
* tGravityAcc-mean()-Z43
* tGravityAcc-std()-Y45
* tGravityAcc-mad()-X47
* tGravityAcc-mad()-Z49
* tGravityAcc-max()-Y51
* tGravityAcc-min()-X53
* tGravityAcc-min()-Z55
* tGravityAcc-energy()-X57
* tGravityAcc-energy()-Z59
* tGravityAcc-iqr()-Y61
* tGravityAcc-entropy()-X63
* tGravityAcc-entropy()-Z65
* tGravityAcc-arCoeff()-X,267
* tGravityAcc-arCoeff()-X,469
* tGravityAcc-arCoeff()-Y,271
* tGravityAcc-arCoeff()-Y,473
* tGravityAcc-arCoeff()-Z,275
* tGravityAcc-arCoeff()-Z,477
* tGravityAcc-correlation()-X,Z79
* tBodyAccJerk-mean()-X81
* tBodyAccJerk-mean()-Z83
* tBodyAccJerk-std()-Y85
* tBodyAccJerk-mad()-X87
* tBodyAccJerk-mad()-Z89
* tBodyAccJerk-max()-Y91
* tBodyAccJerk-min()-X93
* tBodyAccJerk-min()-Z95
* tBodyAccJerk-energy()-X97
* tBodyAccJerk-energy()-Z99
* tBodyAccJerk-iqr()-Y101
* tBodyAccJerk-entropy()-X103
* tBodyAccJerk-entropy()-Z105
* tBodyAccJerk-arCoeff()-X,2107
* tBodyAccJerk-arCoeff()-X,4109
* tBodyAccJerk-arCoeff()-Y,2111
* tBodyAccJerk-arCoeff()-Y,4113
* tBodyAccJerk-arCoeff()-Z,2115
* tBodyAccJerk-arCoeff()-Z,4117
* tBodyAccJerk-correlation()-X,Z119
* tBodyGyro-mean()-X121
* tBodyGyro-mean()-Z123
* tBodyGyro-std()-Y125
* tBodyGyro-mad()-X127
* tBodyGyro-mad()-Z129
* tBodyGyro-max()-Y131
* tBodyGyro-min()-X133
* tBodyGyro-min()-Z135
* tBodyGyro-energy()-X137
* tBodyGyro-energy()-Z139
* tBodyGyro-iqr()-Y141
* tBodyGyro-entropy()-X143
* tBodyGyro-entropy()-Z145
* tBodyGyro-arCoeff()-X,2147
* tBodyGyro-arCoeff()-X,4149
* tBodyGyro-arCoeff()-Y,2151
* tBodyGyro-arCoeff()-Y,4153
* tBodyGyro-arCoeff()-Z,2155
* tBodyGyro-arCoeff()-Z,4157
* tBodyGyro-correlation()-X,Z159
* tBodyGyroJerk-mean()-X161
* tBodyGyroJerk-mean()-Z163
* tBodyGyroJerk-std()-Y165
* tBodyGyroJerk-mad()-X167
* tBodyGyroJerk-mad()-Z169
* tBodyGyroJerk-max()-Y171
* tBodyGyroJerk-min()-X173
* tBodyGyroJerk-min()-Z175
* tBodyGyroJerk-energy()-X177
* tBodyGyroJerk-energy()-Z179
* tBodyGyroJerk-iqr()-Y181
* tBodyGyroJerk-entropy()-X183
* tBodyGyroJerk-entropy()-Z185
* tBodyGyroJerk-arCoeff()-X,2187
* tBodyGyroJerk-arCoeff()-X,4189
* tBodyGyroJerk-arCoeff()-Y,2191
* tBodyGyroJerk-arCoeff()-Y,4193
* tBodyGyroJerk-arCoeff()-Z,2195
* tBodyGyroJerk-arCoeff()-Z,4197
* tBodyGyroJerk-correlation()-X,Z199
* tBodyAccMag-mean()201
* tBodyAccMag-mad()203
* tBodyAccMag-min()205
* tBodyAccMag-energy()207
* tBodyAccMag-entropy()209
* tBodyAccMag-arCoeff()2211
* tBodyAccMag-arCoeff()4213
* tGravityAccMag-std()215
* tGravityAccMag-max()217
* tGravityAccMag-sma()219
* tGravityAccMag-iqr()221
* tGravityAccMag-arCoeff()1223
* tGravityAccMag-arCoeff()3225
* tBodyAccJerkMag-mean()227
* tBodyAccJerkMag-mad()229
* tBodyAccJerkMag-min()231
* tBodyAccJerkMag-energy()233
* tBodyAccJerkMag-entropy()235
* tBodyAccJerkMag-arCoeff()2237
* tBodyAccJerkMag-arCoeff()4239
* tBodyGyroMag-std()241
* tBodyGyroMag-max()243
* tBodyGyroMag-sma()245
* tBodyGyroMag-iqr()247
* tBodyGyroMag-arCoeff()1249
* tBodyGyroMag-arCoeff()3251
* tBodyGyroJerkMag-mean()253
* tBodyGyroJerkMag-mad()255
* tBodyGyroJerkMag-min()257
* tBodyGyroJerkMag-energy()259
* tBodyGyroJerkMag-entropy()261
* tBodyGyroJerkMag-arCoeff()2263
* tBodyGyroJerkMag-arCoeff()4265
* fBodyAcc-mean()-Y267
* fBodyAcc-std()-X269
* fBodyAcc-std()-Z271
* fBodyAcc-mad()-Y273
* fBodyAcc-max()-X275
* fBodyAcc-max()-Z277
* fBodyAcc-min()-Y279
* fBodyAcc-sma()281
* fBodyAcc-energy()-Y283
* fBodyAcc-iqr()-X285
* fBodyAcc-iqr()-Z287
* fBodyAcc-entropy()-Y289
* fBodyAcc-maxInds-X291
* fBodyAcc-maxInds-Z293
* fBodyAcc-meanFreq()-Y295
* fBodyAcc-skewness()-X297
* fBodyAcc-skewness()-Y299
* fBodyAcc-skewness()-Z301
* fBodyAcc-bandsEnergy()-1,8303
* fBodyAcc-bandsEnergy()-17,24305
* fBodyAcc-bandsEnergy()-33,40307
* fBodyAcc-bandsEnergy()-49,56309
* fBodyAcc-bandsEnergy()-1,16311
* fBodyAcc-bandsEnergy()-33,48313
* fBodyAcc-bandsEnergy()-1,24315
* fBodyAcc-bandsEnergy()-1,8317
* fBodyAcc-bandsEnergy()-17,24319
* fBodyAcc-bandsEnergy()-33,40321
* fBodyAcc-bandsEnergy()-49,56323
* fBodyAcc-bandsEnergy()-1,16325
* fBodyAcc-bandsEnergy()-33,48327
* fBodyAcc-bandsEnergy()-1,24329
* fBodyAcc-bandsEnergy()-1,8331
* fBodyAcc-bandsEnergy()-17,24333
* fBodyAcc-bandsEnergy()-33,40335
* fBodyAcc-bandsEnergy()-49,56337
* fBodyAcc-bandsEnergy()-1,16339
* fBodyAcc-bandsEnergy()-33,48341
* fBodyAcc-bandsEnergy()-1,24343
* fBodyAccJerk-mean()-X345
* fBodyAccJerk-mean()-Z347
* fBodyAccJerk-std()-Y349
* fBodyAccJerk-mad()-X351
* fBodyAccJerk-mad()-Z353
* fBodyAccJerk-max()-Y355
* fBodyAccJerk-min()-X357
* fBodyAccJerk-min()-Z359
* fBodyAccJerk-energy()-X361
* fBodyAccJerk-energy()-Z363
* fBodyAccJerk-iqr()-Y365
* fBodyAccJerk-entropy()-X367
* fBodyAccJerk-entropy()-Z369
* fBodyAccJerk-maxInds-Y371
* fBodyAccJerk-meanFreq()-X373
* fBodyAccJerk-meanFreq()-Z375
* fBodyAccJerk-kurtosis()-X377
* fBodyAccJerk-kurtosis()-Y379
* fBodyAccJerk-kurtosis()-Z381
* fBodyAccJerk-bandsEnergy()-9,16383
* fBodyAccJerk-bandsEnergy()-25,32385
* fBodyAccJerk-bandsEnergy()-41,48387
* fBodyAccJerk-bandsEnergy()-57,64389
* fBodyAccJerk-bandsEnergy()-17,32391
* fBodyAccJerk-bandsEnergy()-49,64393
* fBodyAccJerk-bandsEnergy()-25,48395
* fBodyAccJerk-bandsEnergy()-9,16397
* fBodyAccJerk-bandsEnergy()-25,32399
* fBodyAccJerk-bandsEnergy()-41,48401
* fBodyAccJerk-bandsEnergy()-57,64403
* fBodyAccJerk-bandsEnergy()-17,32405
* fBodyAccJerk-bandsEnergy()-49,64407
* fBodyAccJerk-bandsEnergy()-25,48409
* fBodyAccJerk-bandsEnergy()-9,16411
* fBodyAccJerk-bandsEnergy()-25,32413
* fBodyAccJerk-bandsEnergy()-41,48415
* fBodyAccJerk-bandsEnergy()-57,64417
* fBodyAccJerk-bandsEnergy()-17,32419
* fBodyAccJerk-bandsEnergy()-49,64421
* fBodyAccJerk-bandsEnergy()-25,48423
* fBodyGyro-mean()-Y425
* fBodyGyro-std()-X427
* fBodyGyro-std()-Z429
* fBodyGyro-mad()-Y431
* fBodyGyro-max()-X433
* fBodyGyro-max()-Z435
* fBodyGyro-min()-Y437
* fBodyGyro-sma()439
* fBodyGyro-energy()-Y441
* fBodyGyro-iqr()-X443
* fBodyGyro-iqr()-Z445
* fBodyGyro-entropy()-Y447
* fBodyGyro-maxInds-X449
* fBodyGyro-maxInds-Z451
* fBodyGyro-meanFreq()-Y453
* fBodyGyro-skewness()-X455
* fBodyGyro-skewness()-Y457
* fBodyGyro-skewness()-Z459
* fBodyGyro-bandsEnergy()-1,8461
* fBodyGyro-bandsEnergy()-17,24463
* fBodyGyro-bandsEnergy()-33,40465
* fBodyGyro-bandsEnergy()-49,56467
* fBodyGyro-bandsEnergy()-1,16469
* fBodyGyro-bandsEnergy()-33,48471
* fBodyGyro-bandsEnergy()-1,24473
* fBodyGyro-bandsEnergy()-1,8475
* fBodyGyro-bandsEnergy()-17,24477
* fBodyGyro-bandsEnergy()-33,40479
* fBodyGyro-bandsEnergy()-49,56481
* fBodyGyro-bandsEnergy()-1,16483
* fBodyGyro-bandsEnergy()-33,48485
* fBodyGyro-bandsEnergy()-1,24487
* fBodyGyro-bandsEnergy()-1,8489
* fBodyGyro-bandsEnergy()-17,24491
* fBodyGyro-bandsEnergy()-33,40493
* fBodyGyro-bandsEnergy()-49,56495
* fBodyGyro-bandsEnergy()-1,16497
* fBodyGyro-bandsEnergy()-33,48499
* fBodyGyro-bandsEnergy()-1,24501
* fBodyAccMag-mean()503
* fBodyAccMag-mad()505
* fBodyAccMag-min()507
* fBodyAccMag-energy()509
* fBodyAccMag-entropy()511
* fBodyAccMag-meanFreq()513
* fBodyAccMag-kurtosis()515
* fBodyBodyAccJerkMag-std()517
* fBodyBodyAccJerkMag-max()519
* fBodyBodyAccJerkMag-sma()521
* fBodyBodyAccJerkMag-iqr()523
* fBodyBodyAccJerkMag-maxInds525
* fBodyBodyAccJerkMag-skewness()527
* fBodyBodyGyroMag-mean()529
* fBodyBodyGyroMag-mad()531
* fBodyBodyGyroMag-min()533
* fBodyBodyGyroMag-energy()535
* fBodyBodyGyroMag-entropy()537
* fBodyBodyGyroMag-meanFreq()539
* fBodyBodyGyroMag-kurtosis()541
* fBodyBodyGyroJerkMag-std()543
* fBodyBodyGyroJerkMag-max()545
* fBodyBodyGyroJerkMag-sma()547
* fBodyBodyGyroJerkMag-iqr()549
* fBodyBodyGyroJerkMag-maxInds551
* fBodyBodyGyroJerkMag-skewness()553
* angle(tBodyAccMean,gravity)555
* angle(tBodyGyroMean,gravityMean)557
* angle(X,gravityMean)559
* angle(Z,gravityMean)561
* tBodyAcc-mean()-Y2
* tBodyAcc-std()-X4
* tBodyAcc-std()-Z6
* tBodyAcc-mad()-Y8
* tBodyAcc-max()-X10
* tBodyAcc-max()-Z12
* tBodyAcc-min()-Y14
* tBodyAcc-sma()16
* tBodyAcc-energy()-Y18
* tBodyAcc-iqr()-X20
* tBodyAcc-iqr()-Z22
* tBodyAcc-entropy()-Y24
* tBodyAcc-arCoeff()-X,126
* tBodyAcc-arCoeff()-X,328
* tBodyAcc-arCoeff()-Y,130
* tBodyAcc-arCoeff()-Y,332
* tBodyAcc-arCoeff()-Z,134
* tBodyAcc-arCoeff()-Z,336
* tBodyAcc-correlation()-X,Y38
* tBodyAcc-correlation()-Y,Z40
* tGravityAcc-mean()-Y42
* tGravityAcc-std()-X44
* tGravityAcc-std()-Z46
* tGravityAcc-mad()-Y48
* tGravityAcc-max()-X50
* tGravityAcc-max()-Z52
* tGravityAcc-min()-Y54
* tGravityAcc-sma()56
* tGravityAcc-energy()-Y58
* tGravityAcc-iqr()-X60
* tGravityAcc-iqr()-Z62
* tGravityAcc-entropy()-Y64
* tGravityAcc-arCoeff()-X,166
* tGravityAcc-arCoeff()-X,368
* tGravityAcc-arCoeff()-Y,170
* tGravityAcc-arCoeff()-Y,372
* tGravityAcc-arCoeff()-Z,174
* tGravityAcc-arCoeff()-Z,376
* tGravityAcc-correlation()-X,Y78
* tGravityAcc-correlation()-Y,Z80
* tBodyAccJerk-mean()-Y82
* tBodyAccJerk-std()-X84
* tBodyAccJerk-std()-Z86
* tBodyAccJerk-mad()-Y88
* tBodyAccJerk-max()-X90
* tBodyAccJerk-max()-Z92
* tBodyAccJerk-min()-Y94
* tBodyAccJerk-sma()96
* tBodyAccJerk-energy()-Y98
* tBodyAccJerk-iqr()-X100
* tBodyAccJerk-iqr()-Z102
* tBodyAccJerk-entropy()-Y104
* tBodyAccJerk-arCoeff()-X,1106
* tBodyAccJerk-arCoeff()-X,3108
* tBodyAccJerk-arCoeff()-Y,1110
* tBodyAccJerk-arCoeff()-Y,3112
* tBodyAccJerk-arCoeff()-Z,1114
* tBodyAccJerk-arCoeff()-Z,3116
* tBodyAccJerk-correlation()-X,Y118
* tBodyAccJerk-correlation()-Y,Z120
* tBodyGyro-mean()-Y122
* tBodyGyro-std()-X124
* tBodyGyro-std()-Z126
* tBodyGyro-mad()-Y128
* tBodyGyro-max()-X130
* tBodyGyro-max()-Z132
* tBodyGyro-min()-Y134
* tBodyGyro-sma()136
* tBodyGyro-energy()-Y138
* tBodyGyro-iqr()-X140
* tBodyGyro-iqr()-Z142
* tBodyGyro-entropy()-Y144
* tBodyGyro-arCoeff()-X,1146
* tBodyGyro-arCoeff()-X,3148
* tBodyGyro-arCoeff()-Y,1150
* tBodyGyro-arCoeff()-Y,3152
* tBodyGyro-arCoeff()-Z,1154
* tBodyGyro-arCoeff()-Z,3156
* tBodyGyro-correlation()-X,Y158
* tBodyGyro-correlation()-Y,Z160
* tBodyGyroJerk-mean()-Y162
* tBodyGyroJerk-std()-X164
* tBodyGyroJerk-std()-Z166
* tBodyGyroJerk-mad()-Y168
* tBodyGyroJerk-max()-X170
* tBodyGyroJerk-max()-Z172
* tBodyGyroJerk-min()-Y174
* tBodyGyroJerk-sma()176
* tBodyGyroJerk-energy()-Y178
* tBodyGyroJerk-iqr()-X180
* tBodyGyroJerk-iqr()-Z182
* tBodyGyroJerk-entropy()-Y184
* tBodyGyroJerk-arCoeff()-X,1186
* tBodyGyroJerk-arCoeff()-X,3188
* tBodyGyroJerk-arCoeff()-Y,1190
* tBodyGyroJerk-arCoeff()-Y,3192
* tBodyGyroJerk-arCoeff()-Z,1194
* tBodyGyroJerk-arCoeff()-Z,3196
* tBodyGyroJerk-correlation()-X,Y198
* tBodyGyroJerk-correlation()-Y,Z200
* tBodyAccMag-std()202
* tBodyAccMag-max()204
* tBodyAccMag-sma()206
* tBodyAccMag-iqr()208
* tBodyAccMag-arCoeff()1210
* tBodyAccMag-arCoeff()3212
* tGravityAccMag-mean()214
* tGravityAccMag-mad()216
* tGravityAccMag-min()218
* tGravityAccMag-energy()220
* tGravityAccMag-entropy()222
* tGravityAccMag-arCoeff()2224
* tGravityAccMag-arCoeff()4226
* tBodyAccJerkMag-std()228
* tBodyAccJerkMag-max()230
* tBodyAccJerkMag-sma()232
* tBodyAccJerkMag-iqr()234
* tBodyAccJerkMag-arCoeff()1236
* tBodyAccJerkMag-arCoeff()3238
* tBodyGyroMag-mean()240
* tBodyGyroMag-mad()242
* tBodyGyroMag-min()244
* tBodyGyroMag-energy()246
* tBodyGyroMag-entropy()248
* tBodyGyroMag-arCoeff()2250
* tBodyGyroMag-arCoeff()4252
* tBodyGyroJerkMag-std()254
* tBodyGyroJerkMag-max()256
* tBodyGyroJerkMag-sma()258
* tBodyGyroJerkMag-iqr()260
* tBodyGyroJerkMag-arCoeff()1262
* tBodyGyroJerkMag-arCoeff()3264
* fBodyAcc-mean()-X266
* fBodyAcc-mean()-Z268
* fBodyAcc-std()-Y270
* fBodyAcc-mad()-X272
* fBodyAcc-mad()-Z274
* fBodyAcc-max()-Y276
* fBodyAcc-min()-X278
* fBodyAcc-min()-Z280
* fBodyAcc-energy()-X282
* fBodyAcc-energy()-Z284
* fBodyAcc-iqr()-Y286
* fBodyAcc-entropy()-X288
* fBodyAcc-entropy()-Z290
* fBodyAcc-maxInds-Y292
* fBodyAcc-meanFreq()-X294
* fBodyAcc-meanFreq()-Z296
* fBodyAcc-kurtosis()-X298
* fBodyAcc-kurtosis()-Y300
* fBodyAcc-kurtosis()-Z302
* fBodyAcc-bandsEnergy()-9,16304
* fBodyAcc-bandsEnergy()-25,32306
* fBodyAcc-bandsEnergy()-41,48308
* fBodyAcc-bandsEnergy()-57,64310
* fBodyAcc-bandsEnergy()-17,32312
* fBodyAcc-bandsEnergy()-49,64314
* fBodyAcc-bandsEnergy()-25,48316
* fBodyAcc-bandsEnergy()-9,16318
* fBodyAcc-bandsEnergy()-25,32320
* fBodyAcc-bandsEnergy()-41,48322
* fBodyAcc-bandsEnergy()-57,64324
* fBodyAcc-bandsEnergy()-17,32326
* fBodyAcc-bandsEnergy()-49,64328
* fBodyAcc-bandsEnergy()-25,48330
* fBodyAcc-bandsEnergy()-9,16332
* fBodyAcc-bandsEnergy()-25,32334
* fBodyAcc-bandsEnergy()-41,48336
* fBodyAcc-bandsEnergy()-57,64338
* fBodyAcc-bandsEnergy()-17,32340
* fBodyAcc-bandsEnergy()-49,64342
* fBodyAcc-bandsEnergy()-25,48344
* fBodyAccJerk-mean()-Y346
* fBodyAccJerk-std()-X348
* fBodyAccJerk-std()-Z350
* fBodyAccJerk-mad()-Y352
* fBodyAccJerk-max()-X354
* fBodyAccJerk-max()-Z356
* fBodyAccJerk-min()-Y358
* fBodyAccJerk-sma()360
* fBodyAccJerk-energy()-Y362
* fBodyAccJerk-iqr()-X364
* fBodyAccJerk-iqr()-Z366
* fBodyAccJerk-entropy()-Y368
* fBodyAccJerk-maxInds-X370
* fBodyAccJerk-maxInds-Z372
* fBodyAccJerk-meanFreq()-Y374
* fBodyAccJerk-skewness()-X376
* fBodyAccJerk-skewness()-Y378
* fBodyAccJerk-skewness()-Z380
* fBodyAccJerk-bandsEnergy()-1,8382
* fBodyAccJerk-bandsEnergy()-17,24384
* fBodyAccJerk-bandsEnergy()-33,40386
* fBodyAccJerk-bandsEnergy()-49,56388
* fBodyAccJerk-bandsEnergy()-1,16390
* fBodyAccJerk-bandsEnergy()-33,48392
* fBodyAccJerk-bandsEnergy()-1,24394
* fBodyAccJerk-bandsEnergy()-1,8396
* fBodyAccJerk-bandsEnergy()-17,24398
* fBodyAccJerk-bandsEnergy()-33,40400
* fBodyAccJerk-bandsEnergy()-49,56402
* fBodyAccJerk-bandsEnergy()-1,16404
* fBodyAccJerk-bandsEnergy()-33,48406
* fBodyAccJerk-bandsEnergy()-1,24408
* fBodyAccJerk-bandsEnergy()-1,8410
* fBodyAccJerk-bandsEnergy()-17,24412
* fBodyAccJerk-bandsEnergy()-33,40414
* fBodyAccJerk-bandsEnergy()-49,56416
* fBodyAccJerk-bandsEnergy()-1,16418
* fBodyAccJerk-bandsEnergy()-33,48420
* fBodyAccJerk-bandsEnergy()-1,24422
* fBodyGyro-mean()-X424
* fBodyGyro-mean()-Z426
* fBodyGyro-std()-Y428
* fBodyGyro-mad()-X430
* fBodyGyro-mad()-Z432
* fBodyGyro-max()-Y434
* fBodyGyro-min()-X436
* fBodyGyro-min()-Z438
* fBodyGyro-energy()-X440
* fBodyGyro-energy()-Z442
* fBodyGyro-iqr()-Y444
* fBodyGyro-entropy()-X446
* fBodyGyro-entropy()-Z448
* fBodyGyro-maxInds-Y450
* fBodyGyro-meanFreq()-X452
* fBodyGyro-meanFreq()-Z454
* fBodyGyro-kurtosis()-X456
* fBodyGyro-kurtosis()-Y458
* fBodyGyro-kurtosis()-Z460
* fBodyGyro-bandsEnergy()-9,16462
* fBodyGyro-bandsEnergy()-25,32464
* fBodyGyro-bandsEnergy()-41,48466
* fBodyGyro-bandsEnergy()-57,64468
* fBodyGyro-bandsEnergy()-17,32470
* fBodyGyro-bandsEnergy()-49,64472
* fBodyGyro-bandsEnergy()-25,48474
* fBodyGyro-bandsEnergy()-9,16476
* fBodyGyro-bandsEnergy()-25,32478
* fBodyGyro-bandsEnergy()-41,48480
* fBodyGyro-bandsEnergy()-57,64482
* fBodyGyro-bandsEnergy()-17,32484
* fBodyGyro-bandsEnergy()-49,64486
* fBodyGyro-bandsEnergy()-25,48488
* fBodyGyro-bandsEnergy()-9,16490
* fBodyGyro-bandsEnergy()-25,32492
* fBodyGyro-bandsEnergy()-41,48494
* fBodyGyro-bandsEnergy()-57,64496
* fBodyGyro-bandsEnergy()-17,32498
* fBodyGyro-bandsEnergy()-49,64500
* fBodyGyro-bandsEnergy()-25,48502
* fBodyAccMag-std()504
* fBodyAccMag-max()506
* fBodyAccMag-sma()508
* fBodyAccMag-iqr()510
* fBodyAccMag-maxInds512
* fBodyAccMag-skewness()514
* fBodyBodyAccJerkMag-mean()516
* fBodyBodyAccJerkMag-mad()518
* fBodyBodyAccJerkMag-min()520
* fBodyBodyAccJerkMag-energy()522
* fBodyBodyAccJerkMag-entropy()524
* fBodyBodyAccJerkMag-meanFreq()526
* fBodyBodyAccJerkMag-kurtosis()528
* fBodyBodyGyroMag-std()530
* fBodyBodyGyroMag-max()532
* fBodyBodyGyroMag-sma()534
* fBodyBodyGyroMag-iqr()536
* fBodyBodyGyroMag-maxInds538
* fBodyBodyGyroMag-skewness()540
* fBodyBodyGyroJerkMag-mean()542
* fBodyBodyGyroJerkMag-mad()544
* fBodyBodyGyroJerkMag-min()546
* fBodyBodyGyroJerkMag-energy()548
* fBodyBodyGyroJerkMag-entropy()550
* fBodyBodyGyroJerkMag-meanFreq()552
* fBodyBodyGyroJerkMag-kurtosis()554
* angle(tBodyAccJerkMean),gravityMean)556
* angle(tBodyGyroJerkMean,gravityMean)558
* angle(Y,gravityMean)560
