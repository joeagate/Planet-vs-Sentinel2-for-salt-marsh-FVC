# %% ---------------------------------------------------------------------------
# Import libraries

print('Importing libraries')
# External libraries
import os
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.pipeline import Pipeline
from xgboost import XGBRegressor
from skopt import BayesSearchCV
from skopt.space import Real, Integer
import matplotlib.pyplot as plt
from sklearn import metrics
import joblib
from sklearn.inspection import permutation_importance
from sklearn.preprocessing import OneHotEncoder
from pathlib import Path
from sklearn.base import clone
from skopt.plots import plot_convergence
import time

# %% ---------------------------------------------------------------------------
# Build the training pipeline

print('Building training pipeline')
# Set the random seed
rand_seed = 123

# Set the estimators parameters for the pipeline
estimators = [
    ('reg', XGBRegressor(random_state=rand_seed,
                        objective='reg:squarederror'))
]

# Build the pipeline
pipe = Pipeline(steps=estimators)

# Define the search space for tuning
search_space = {
    'reg__max_depth': Integer(5, 15),  # Maximum depth of trees; higher values allow more complex models, lower is more conservative.
    'reg__learning_rate': Real(0.01, 0.3, prior='log-uniform'),  # Step size for updating weights; lower values make training slower but more conservative.
    'reg__subsample': Real(0.5, 1.0),  # Fraction of data used to grow each tree; lower values reduce overfitting but may underutilize data.
    'reg__min_child_weight': Integer(2, 15),  # Minimum sum of instance weight needed in a child node; higher values restrict tree growth, making the model simpler and more conservative.
    'reg__colsample_bytree': Real(0.3, 1.0),  # Fraction of features used per tree; lower values promote simpler models and reduce overfitting.
    'reg__colsample_bylevel': Real(0.3, 1.0),  # Fraction of features used per tree level; similar effect as `colsample_bytree`.
    'reg__colsample_bynode': Real(0.3, 1.0),  # Fraction of features used per tree node; lower values increase regularization.
    'reg__reg_alpha': Real(0.0, 5.0),  # L1 regularization on weights; higher values encourage sparsity and are more conservative.
    'reg__reg_lambda': Real(0.0, 100.0),  # L2 regularization on weights; higher values penalize large weights, making the model more conservative.
    'reg__gamma': Real(0.0, 5.0)  # Minimum loss reduction required to split a node; higher values restrict splitting, leading to simpler models.
}


# Create the operation using Bayesian searching cross-validation
reg = BayesSearchCV(pipe, search_space, 
                        cv=5, # 5-fold cross-validation
                        n_iter=100, # Number of iterations
                        n_jobs=6, # Set the number of parellel jobs to run. This is my number of cores -2. Change this to fit your system
                        scoring='neg_mean_squared_error',
                   random_state=rand_seed)

# %% ---------------------------------------------------------------------------
# Create a function to evaluate model accuracy

print('Defining a function to evaluate the accuracy of the regression models')
def evaluate_regression_model(observed, predicted):
    
    mae = metrics.mean_absolute_error(observed, predicted)
    # mse = metrics.mean_squared_error(observed, predicted)
    rmse = metrics.root_mean_squared_error(observed, predicted)
    r2 = metrics.r2_score(observed, predicted)
    
    return{
        'MAE': mae,
        # 'mse': mse,
        'RMSE': rmse,
        'R2': r2
    }

# %% ---------------------------------------------------------------------------
# Loop through the datasets and train XGBoost models

# Create a dictionary to convert between the file naming and the column naming
colnames_convert = {
    'unveg': 'Unvegetated',
    'low': 'Low',
    'mid': 'Mid',
    'high': 'High'
}

print('\n\nStarting the training loop')
# Loop through the samples directory
for root, subdirs, files in os.walk('./data/interim/samples/'):
    # raise Exception('You dont want to do that')
    # Filter to directories with files in
    if len(files) > 0:
        
        # Loop through files
        for file in files:
            # print(file)

            # Remove file extension
            name = file.replace('.csv', '')

            # Split by underscore
            parts = name.split('_')

            # Extract information from the filename
            satellite = parts[0]
            cover = parts[2]
            sample = parts[3]
            bands = parts[4]

            # Only proceed for the CommonBands
            if (bands == 'CommonBands'):
                print(satellite, cover, sample, bands)

                # Import the traing data
                df = pd.read_csv(Path(Path(root)/Path(file)))

                # Select the x and y data
                if satellite == 'Planet':
                    X = df.loc[:, "blue":"DTM"].copy()
                elif satellite == 'Sentinel':
                    X = df.loc[:, "B2":"DTM"].copy()
                y = df[colnames_convert[cover]].copy()

                # Loop through the one hot encoding
                for onehot in ('notonehot', 'onehot'):
                    print(onehot)
                    # Apply one hot encoding
                    # ======================
                    if onehot == 'onehot':
                        # raise Exception('Steady on partner')
                        # Select the site and established for one-hot encoding columns
                        oneHot_columns = ['Site', 'Est']

                        # Create the encoder
                        encoder = OneHotEncoder(sparse_output=False)

                        # Apply one-hot encoding to the categorical columns
                        one_hot_encoded = encoder.fit_transform(df[oneHot_columns])

                        #Create a DataFrame with the one-hot encoded columns
                        one_hot_df = pd.DataFrame(one_hot_encoded, columns=encoder.get_feature_names_out(oneHot_columns))

                        # Join to the X variable
                        X = pd.concat([X, one_hot_df], axis = 1)

                    # Train the model
                    # ===============
                    # Perform the test/train split
                    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=rand_seed)

                    # Create a clone of reg for training
                    reg_clone = clone(reg)

                    # Training
                    start = time.time()
                    reg_clone.fit(X_train, y_train)
                    end = time.time()

                    print('Model training time was', round((end-start)/60,4), 'minutes')
                    
                    # Save the trained model
                    joblib.dump(reg_clone, f'./models/{onehot}/{satellite}_{cover}_{sample}_{bands}_{onehot}_trained_model.pkl')

                    # Save the hyperparameter tuning results
                    # ======================================
                    cv_res = pd.DataFrame(reg_clone.cv_results_)
                    cv_res['satellite'] = satellite
                    cv_res['bands'] = bands
                    cv_res['sample'] = sample
                    cv_res['cover'] = cover
                    cv_res['onehot'] = onehot
                    cv_res.to_csv(f'./outputs/bayesian_parameter_tuning/{satellite}/{onehot}/{satellite}_{cover}_{sample}_{bands}_{onehot}_Bayesian_cv_results.csv')

                    # Save a convergence plot to drive
                    # ================================
                    ax = plot_convergence(reg_clone.optimizer_results_[0])
                    fig = ax.get_figure()
                    fig.savefig(f'./outputs/convergence_plots/{satellite}_{cover}_{sample}_{bands}_{onehot}_convergence_plot.png')
                    plt.close(fig)

                    # Training accuracies
                    # ===================
                    predicted_train = reg_clone.predict(X_train)
                    # Normalise the values out of scale
                    predicted_train[predicted_train > 1] = 1
                    predicted_train[predicted_train < 0] = 0
                    # Evaluate the accuracy
                    reg_accs_train = evaluate_regression_model(observed=y_train, predicted=predicted_train)
                    # Convert to data frame
                    reg_accs_train_df = pd.DataFrame.from_dict(reg_accs_train, orient='index').transpose()
                    # Add metadata
                    reg_accs_train_df['satellite'] = satellite
                    reg_accs_train_df['cover'] = cover
                    reg_accs_train_df['sample'] = sample
                    reg_accs_train_df['bands'] = bands
                    reg_accs_train_df['onehot'] = onehot
                    reg_accs_train_df['dataset'] = 'train'
                    # Save to drive
                    reg_accs_train_df.to_csv(f'./outputs/accuracies/{onehot}/{satellite}_{cover}_{sample}_{bands}_{onehot}_training_accuracies.csv')

                    # Testing accuracies
                    # ==================
                    predicted_test = reg_clone.predict(X_test)
                    # Normalise the values out of scale
                    predicted_test[predicted_test > 1] = 1
                    predicted_test[predicted_test < 0] = 0
                    # Evaluate the accuracy
                    reg_accs_test = evaluate_regression_model(observed=y_test, predicted=predicted_test)
                    # Convert to data frame
                    reg_accs_test_df = pd.DataFrame.from_dict(reg_accs_test, orient='index').transpose()
                    # Add metadata
                    reg_accs_test_df['satellite'] = satellite
                    reg_accs_test_df['cover'] = cover
                    reg_accs_test_df['sample'] = sample
                    reg_accs_test_df['bands'] = bands
                    reg_accs_test_df['onehot'] = onehot
                    reg_accs_test_df['dataset'] = 'test'
                    # Save to drive
                    reg_accs_test_df.to_csv(f'./outputs/accuracies/{onehot}/{satellite}_{cover}_{sample}_{bands}_{onehot}_testing_accuracies.csv')

                    # Save the observed vs predicted data
                    # ===================================
                    pd.DataFrame({
                        'cover': cover,
                        'satellite': satellite,
                        'bands': bands,
                        'sample': sample,
                        'onehot': onehot,
                        'observed': y_test,
                        'predicted': predicted_test
                    }).to_csv(f'./outputs/observed-predicted/{onehot}/{satellite}_{cover}_{sample}_{bands}_{onehot}_observed_predicted_test.csv')

                    # Calculate the permutation importance for the train data
                    # =======================================================
                    perm_importance_train = permutation_importance(reg_clone, X_train, y_train, n_repeats=30, random_state=rand_seed)
                    perm_importance_train_df = pd.DataFrame({
                        'feature': X_train.columns,
                        'mean_perm_importance': perm_importance_train.importances_mean,
                        'std_dev_perm_importance': perm_importance_train.importances_std
                    })
                    perm_importance_train_df['satellite'] = satellite
                    perm_importance_train_df['cover'] = cover
                    perm_importance_train_df['bands'] = bands
                    perm_importance_train_df['dataset'] = 'train'
                    perm_importance_train_df['sample'] = sample
                    perm_importance_train_df.to_csv(f'./outputs/feature_importance/{onehot}/{satellite}_{cover}_{sample}_{bands}_{onehot}_feature_importance_train.csv')

                    # Calculate the permutation importance for the test data
                    # ======================================================
                    perm_importance_test = permutation_importance(reg_clone, X_test, y_test, n_repeats=30, random_state=rand_seed)
                    perm_importance_test_df = pd.DataFrame({
                        'feature': X_test.columns,
                        'mean_perm_importance': perm_importance_test.importances_mean,
                        'std_dev_perm_importance': perm_importance_test.importances_std
                    })
                    perm_importance_test_df['satellite'] = satellite
                    perm_importance_test_df['cover'] = cover
                    perm_importance_test_df['bands'] = bands
                    perm_importance_test_df['dataset'] = 'test'
                    perm_importance_test_df['sample'] = sample
                    perm_importance_test_df.to_csv(f'./outputs/feature_importance/{onehot}/{satellite}_{cover}_{sample}_{bands}_{onehot}_feature_importance_test.csv')

                    print('\n')
