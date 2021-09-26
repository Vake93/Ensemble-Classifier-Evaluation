# **Ensemble Classifier Evaluation - Heart Disease UCI Data Set**

## **Introduction**
Ensembles can give you a boost in accuracy on your dataset. You can create ensembles of machine learning algorithms in R. There are three main techniques (Boosting, Bagging and Stacking) that you can create an ensemble of machine learning algorithms in R. 
<br>

The three most popular methods for combining the predictions from different models are:
- Bagging: Building multiple models (typically of the same type) from different subsamples of the training dataset.
- Boosting: Building multiple models (typically of the same type) each of which learns to fix the prediction errors of a prior model in the chain.
- Stacking. Building multiple models (typically of differing types) and supervisor model that learns how to best combine the predictions of the primary models.

You can combine the predictions of multiple caret models using the `caretEnsemble` package. Given a list of caret models, the `caretStac()` function can be used to specify a higher-order model to learn how to best combine the predictions of sub-models together.

This is focused on Bagging and Stacking and on how you can continue to ratchet up the accuracy of the models on your own datasets.

### Bagging Algorithms
The base type bagging machine learning algorithms that will be examined in this assignment are:
- Bagged CART
- Random Forest

### Stacking Algorithms
The base type stacking machine learning algorithms that will be examined in this assignment are
- Classification and Regression Trees (CART)
- K-Nearest Neighbors (KNN)
- Naïve Bayes (NB)

## **Data Set Selection and Visualisation**

**Dataset used:** [UCI Heart disease](https://archive.ics.uci.edu/ml/datasets/heart+Disease)

## Introduction to the dataset
This dataset gives 13 variables along with a target condition of having or not having heart disease.

<img src="https://github.com/Vake93/IIT.DataMiningAndMachineLearning.CW2/raw/5512673a7caca80b96c9d5a904f23755e477ee4c/Images/dataset_props.png" style="height:50px;"/>

### The column definitions are as follows
- age: Age of the patient in years.
- sex: 1 = male and 0 = female
- cp: Chest pain type
    - 1 -> Typical Angina
    - 2 -> Atypical Angina
    - 3 -> Non-anginal Pain
    - 4 -> Asymptomatic
    > #### Note
    > Angina, also known as angina pectoris, is chest pain or pressure, usually due to not enough blood flow to the heart muscle. (Source: [Wikipidia](https://en.wikipedia.org/wiki/Angina))
- trestbps: Resting blood pressure in mm Hg on admission to the hospital
- chol: Serum cholesterol in mg/dl
- fbs: Fasting blood sugar level greater than 120 mg/dl
    - 0 -> False
    - 1 -> True
- restecg: Resting electrocardiographic results
    - 0 -> Normal
    - 1 -> Having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV)
    - 2 -> Showing probable or definite left ventricular hypertrophy by Estes’ criteria.

> #### Note
> <img src="https://github.com/Vake93/IIT.DataMiningAndMachineLearning.CW2/raw/master/Images/ecg.png" style="width:350px;"/> <br>
> In electrocardiography, the ST segment connects the QRS complex and the T wave and has a duration of 5ms to 150ms. <br> Interpretation 
> - The normal ST segment has a slight upward concavity.
> - Flat, down sloping, or depressed ST segments may indicate coronary ischemia.
> (Source: [Wikipidia](https://en.wikipedia.org/wiki/ST_segment))

- thalach: Maximum heart rate achieved in beats per minute
- exang: Exercise induced angina
    - 0 -> No
    - 1 -> Yes
- oldpeak: ST depression induced by exercise relative to rest
- slope: The slope of the peak exercise ST segment
    - 1 -> Upsloping
    - 2 -> Flat
    - 3 -> Down sloping
- ca: Number of major vessels [0-3] colored by fluoroscopy
- thal: Thalium stress test result
    - 1 -> Fixed defect
    - 2 -> Normal
    - 3 -> Reversable defect
- num: Diagnosis of heart disease (angiographic disease status)
    - 0 -> Less than 50% diameter narrowing
    - 1 -> Greater than 50% diameter narrowing

```R
str(heart)
```
![](https://github.com/Vake93/IIT.DataMiningAndMachineLearning.CW2/raw/master/Images/str_heart.png)

```R
head(heart)
```
![](https://github.com/Vake93/IIT.DataMiningAndMachineLearning.CW2/raw/master/Images/head_heart.png)

```R
summary(heart)
```
![](https://github.com/Vake93/IIT.DataMiningAndMachineLearning.CW2/raw/master/Images/summary_heart.png)

## Age Analysis

![](https://github.com/Vake93/IIT.DataMiningAndMachineLearning.CW2/raw/master/Images/age_analysis.png)
- Minimum age is 29 and maximum age is 77, average age is 54.37. Majority of the population is between age group 55 and 60 years.
- There is negative correlation between age and target. This implies that when get older probability of heart attack is decreasing.
- By observing the curve, we can see that from age 30 to 60 probability of heart attack is decreasing and from 60 again probability is increasing. After 70 chance of heart attack is more.
- Using Chi squared test we get a probability value of 0.13. There for we can conclude that target is independent of the age.

