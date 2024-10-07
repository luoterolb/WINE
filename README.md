# 🍷 Wine Quality Analysis and Prediction

## 🔍 Project Overview

This project focuses on analyzing and predicting the quality of red wines using a dataset of physicochemical properties. The goal is to identify relationships between wine attributes (such as acidity, alcohol content, and pH) and their quality, and to build predictive models to classify wine quality.

## 🚀 Key Concepts and Features

- **Exploratory Data Analysis**: We dive into the dataset with boxplots, scatter plots, and histograms to explore the relationships between wine attributes and quality.
- **Data Preprocessing**: Quality ratings are grouped into three categories: `low`, `medium`, and `high` to make prediction tasks more manageable and to handle class imbalances.
- **Predictive Modeling**: We test several machine learning algorithms such as K-Nearest Neighbors (KNN), Random Forest (RF), Support Vector Machines (SVM), and Decision Trees (CART) to classify wine quality.
- **Hyperparameter Tuning**: Random Forest was selected as the best-performing model and further optimized through hyperparameter tuning.

## 📊 Main Findings

- No single attribute directly correlates with wine quality. However, higher alcohol content, lower volatile acidity, and a moderate amount of sulfates tend to be indicators of better-quality wine.
- The dataset has class imbalance issues, with many wines rated between 5 and 6 and very few at the extremes (3, 4, 7, 8).
- Random Forest outperformed other models in both `accuracy` and `kappa`, making it the most reliable classifier for this dataset.

## 🛠️ Technology Stack

- **R Packages**: `ggplot2`, `dplyr`, `randomForest`, `caret`, `PerformanceAnalytics`, `GGally`, `plotly`
- **Machine Learning Methods**: KNN, SVM, Random Forest, Decision Trees (CART)
- **Visualization**: Extensive use of plots and charts to understand data relationships and evaluate model performance.

## 📈 Model Performance

- **Random Forest**: Best performing model with the highest accuracy and kappa scores after tuning hyperparameters.
- **Challenges**: Due to class imbalance, models struggle with predicting wines at the extreme quality levels (3 and 8).
  
## 🖥️ How to Use

1. Clone this repository.
2. Install the necessary R packages using `install.packages()`.
3. Load the dataset `winequality-red.csv`.
4. Run the script to explore the data, build predictive models, and evaluate their performance.

## 🌟 Conclusion

This project illustrates the complexities of predicting wine quality. While no single variable clearly defines quality, the use of machine learning algorithms—particularly Random Forest—proves effective in classifying wine quality into broad categories. Future improvements could involve oversampling underrepresented classes or exploring alternative feature engineering techniques.
