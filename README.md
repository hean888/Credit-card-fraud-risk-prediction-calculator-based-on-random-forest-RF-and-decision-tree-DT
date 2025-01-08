# Credit Card Fraud Risk Prediction Calculator

## Overview
**web address: https://hean7982.shinyapps.io/credit-fraud-prediction/**

This project aims to develop a **Fraud Risk Prediction Calculator** for credit card transactions. The tool utilizes machine learning models, specifically **Random Forest (RF)** and **Decision Tree (DT)**, to predict and classify fraud risks based on various customer data. The solution is deployed as a web application using **R Shiny**, allowing real-time fraud risk predictions to assist credit card business decisions.

## Tools and Technologies

- **R**: Programming language used for data analysis and model development.
- **Random Forest (RF)**: A machine learning algorithm used to predict the probability of fraud.
- **Decision Tree (DT)**: A classification algorithm to categorize customers and identify high-risk fraud groups.
- **Shiny**: An R framework used to deploy the fraud risk prediction model as a web application.
  
## Data

The dataset consists of **20,000 customer records**, with the following features:

- Customer credit histories
- Application information
- Delinquency history
- Spending history

This data is used to train and evaluate the fraud detection models.

## Work and Methodology

1. **Random Forest Model (RF)**:
   - Built a Random Forest model to predict the probability of fraud for a given customer.
   - Evaluated the variable importance to understand which factors contribute most to the fraud risk.

2. **Decision Tree Model (DT)**:
   - Used a Decision Tree model to classify customers into different fraud risk categories.
   - Identified high-risk customers who are more likely to engage in fraudulent activities.

3. **Visualization and Deployment**:
   - Developed an interactive **Fraud Risk Prediction Calculator** to visualize the fraud prediction results.
   - Deployed the calculator as a **web application** using **R Shiny**, allowing users to input customer data and receive real-time fraud risk predictions.

## Results

The visual fraud risk prediction calculator provides a real-time function that helps **banks** and financial institutions make informed decisions on credit card applications. The tool allows decision-makers to assess the fraud risk of a customer based on their application and historical data, improving business efficiency and reducing fraud-related losses.

## Future Work

- **Model Optimization**: Improve the performance and accuracy of the machine learning models by tuning hyperparameters and exploring other algorithms.
- **Real-Time Data Integration**: Implement the integration of real-time transaction data for on-the-fly fraud prediction.
- **Model Interpretability**: Enhance the interpretability of the models to provide clearer explanations for business users.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.


