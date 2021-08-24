# Texter Forecasting

## About this project 📽️
This project is prepared for the MSc Health Data Analytics Research Project. This study predicts hourly text volume data on a digital mental health service using calendar and external predictors.

Full methods and description of data collection are included in the dissertation.

The project was performed on Imperial's HPC and used R version 3.6.0 and Python 3.8.8.

## Project team 🧑‍🤝‍🧑
This was an individual project by, CID: 01906946

## Key Files 📂

The following data files are used in this analysis:
  | File name           			| Description                           	|
  |---------------------			|---------------------------------------	|
  | 000_timestamps_original.csv 		| The original conversation timestamps dataset 	| 
  | openweather_2010-_2021_snarestone_uk.csv 	| All historical weather data from 2010 	| 
  | covid_cases_data_2021-Jun-26.csv 		| UK daily COVID cases data 			| 
  | covid_deaths_data_2021-Jun-26.csv 		| UK daily COVID deaths data 			| 
  | 030 full data.rds 				| A prepared full dataset used in analyses 	| 
 These are not included in this repo and must be located before running the code.
  
 ## Key scripts 📜

Analysis included the use of both R scripts and Python Jupyter scripts.

The following scripts were for data preparation:
| script                            | Description                                                                                                                                                                                                                                     |
|-------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 003 Twitter API Import.R | Import Twitter Data |
| 013 Time Series Predictors from Scratch.R | Prepare Time Series Predictors |
| 014 Twitter+Text Predictors from Scratch.R | Prepare Twitter and Text Predictors  |
| 016 Twitter+Text Predictors Hourly.R | Prepare Twitter and Text Predictors |
| 022 First Full Data Creation.R | Bring all Data Together |
| 030 Observational Analyses.R | Final Tweaks to Dataset before Analysis |
These scripts produce the "030 full data.rds" file above.

The following scripts were used for observational analysis:
| script                            | Description                                                                                                                                                                                                                                     |
|-------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 030 Observational Analyses.R | Observational Analyses |

The following scripts were used for Long term forecasting:
| script                            | Description                                                                                                                                                                                                                                     |
|-------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 01 Train-Test | Construction of Train-Test sets for Long and Short term forecasting (incl both ex-post and ex-ante) |
| 07 Long LM train_predict.R | Linear Model Trained and Predictions Generated |
| 07a Long Naive train_predict.R | Naive Model and Predictions Generated |
| 08_Long_ARIMA_train_predict.R | SARIMAX Model Trained and Predictions Generated |
| 08a_Long_ARIMA_development.R | SARIMAX Model Development Plots Generated |
| 09_Long_NNAR_train_predict.R | AR NN Model Trained and Predictions Generated |
| 10 Long RNN train predict.ipynb | RNN Model Trained and Predictions Generated |
| 11 Long SVR train predict-Copy1.ipynb | SVR Model Trained and Predictions Generated |
| 11a Long SVR CrossVal Curves.R | SVR Model Development Plots Generated |
| 12 Long Preds Output Plot.R | Results and Plots Produced for All Predictor Sets |

The following scripts were used for Short term forecasting:
| script                            | Description                                                                                                                                                                                                                                     |
|-------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 01 Train-Test | Construction of Train-Test sets for Long and Short term forecasting (incl both ex-post and ex-ante) |
| 02_Short_ARIMA_train_predict.R | SARIMAX Model Trained and Predictions Generated |
| 02a Short Naive train_predict.R | Naive Model and Predictions Generated, Output Metrics and Plots Generated for SARIMAX and Naive Predictions  |
| 04_Short_NNAR_train_predict.R | AR NN Trained and Predictions Generated  |
| 05_Short NNAR Output Plot.R | Output Metrics and Plots Generated for AR NN |
| 06 Short RNN train predict.ipynb | RNN Trained and Predictions Generated  |
| 06 Short SVR train predict.ipynb | SVR Trained and Predictions Generated  |
| 06 Short SVR&RNN Output Plot.R | Output Metrics and Plots Generated for SVR & RNN |
| 13_Short_Local_SVR_data.R | Preparation of Windowed Data for Local SVR |
| 13 Short Local-SVR train predict.ipynb | Local-SVR Trained and Predictions Generated  |
| 13 Short LocalSVR Output Plot.R | Output Metrics and Plots Generated for Local-SVR |

## Getting started ✈️

### HPC folder structure 🗄️

The folder structure of the project directory should look like this.
```bash
.
├── data            # data
└── scripts         # data preparation and observational analysis scripts
└── results_scripts # scripts to produce forecasting results
└── results_data    # data produced by results scripts
└── results_output  # output plots from results scripts
```

### Prerequisites 💻

Required pacaged can be installed using `CondaPackages.yml` provided.

### Installation 🖱️

Clone the repo.
   ```sh
   git clone https://github.com/guarigla/HDA.git
   cd HDA
   ```

### Preparation and running the pipeline 🏃‍♀️️

1. Install R libraries listed above 👆 in your Conda on the HPC. 
    * To navigate to Conda R, run the following code:
   ```sh
   module load anaconda3/personal
   R
   ``` 
   
2. Create "data" folder under the directory.
3. Locate the "Key Files" data and save in the data folder.
6. Run the necessary commands within the scripts above to reproduce the analysis, communicating with the author if required.

&nbsp;
