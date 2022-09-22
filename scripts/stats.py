"""
Code includes the bootstrap method written with Aleksei. 
Also includes a series of functions to get some other stats using PyCM, which are not available in sklearn. 
"""

from pycm import ConfusionMatrix
from sklearn.metrics import recall_score, cohen_kappa_score, balanced_accuracy_score, matthews_corrcoef, accuracy_score,\
    precision_score, confusion_matrix
import os
from tqdm import tqdm
import pandas as pd
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import itertools
from sklearn.metrics import confusion_matrix
import pycm


def bootstrap_test(metric, y, preds, null_hypothesis, n_bootstrap, seed=12345, stratified=True, alpha=95):
    """
    Parameters
    ----------
    metric : fucntion
        Metric to compute, e.g. AUC for ROC curve or AP for PR curve
    y : numpy.array
        Ground truth
    preds : numpy.array
        Predictions
    null_hypothesis :
        Value for the metric if predictions are random or some other kind of null hypothesis for testing the model
        performance against
    n_bootstrap:
        Number of bootstrap samples to draw
    seed : int
        Random seed
    stratified : bool
        Whether to do a stratified bootstrapping
    alpha : float
        Confidence intervals width
    """

    np.random.seed(seed)
    metric_vals = []
    classes = np.unique(y)
    inds = []
    for cls in classes:
        inds.append(np.where(y == cls)[0])

    for _ in tqdm(range(n_bootstrap), total=n_bootstrap, desc='Bootstrap:'):
        if stratified:
            ind_bs = []
            for ind_cur in inds:
                ind_bs.append(np.random.choice(ind_cur, ind_cur.shape[0]))
            ind = np.hstack(ind_bs)
        else:
            ind = np.random.choice(y.shape[0], y.shape[0])

        bootstrap_statistic = metric(y[ind], preds[ind])
        if bootstrap_statistic is not None:
            metric_vals.append(bootstrap_statistic)

    metric_vals = np.array(metric_vals)
    p_value = np.mean(metric_vals <= null_hypothesis)   # I am still not confident on this 
                                                        # (e.g. this looks like one sided test, should it not be two-tailed? 

    metric_val = metric(y, preds)
    ci_l = np.percentile(metric_vals, (100 - alpha) // 2)
    ci_h = np.percentile(metric_vals, alpha + (100 - alpha) // 2)

    return p_value, metric_val, ci_l, ci_h


def ccc(y_true, y_pred):
    """
    Reference numpy implementation of Lin's Concordance correlation coefficient
    See https://gitlab.com/-/snippets/1730605
    """

    # covariance between y_true and y_pred
    s_xy = np.cov([y_true, y_pred])[0, 1]
    
    # means
    x_m = np.mean(y_true)
    y_m = np.mean(y_pred)
    
    # variances
    s_x_sq = np.var(y_true)
    s_y_sq = np.var(y_pred)

    # concordance correlation coefficient
    ccc = (2.0 * s_xy) / (s_x_sq + s_y_sq + (x_m - y_m) ** 2)

    return ccc


def gwetsac(y_true, y_pred):
    cm = ConfusionMatrix(y_true, y_pred)
    ac = cm.AC1

    return ac


def no_info_rate(y_true, y_pred):
    cm = ConfusionMatrix(y_true, y_pred)
    nir = cm.NIR

    return nir


# macro and micro averages for multiclass
# More info here: https://datascience.stackexchange.com/questions/15989/micro-average-vs-macro-average-performance-in-a-multiclass-classification-settin
def specificity_micro(y_true, y_pred):  # TNR, Inverse recall
    cm = ConfusionMatrix(y_true, y_pred)
    tnr_micro = cm.TNR_Micro

    return tnr_micro


def sensitivity_micro(y_true, y_pred):  # Recall, TPR
    cm = ConfusionMatrix(y_true, y_pred)
    tpr_micro = cm.TPR_Micro

    return tpr_micro


def specificity_macro(y_true, y_pred):  # TNR, Inverse recall
    cm = ConfusionMatrix(y_true, y_pred)
    tnr_macro = cm.TNR_Macro

    return tnr_macro


def sensitivity_macro(y_true, y_pred):  # Recall, TPR
    cm = ConfusionMatrix(y_true, y_pred)
    tpr_macro = cm.TPR_Macro

    return tpr_macro


def specificity(y_true, y_pred):  # TNR, Inverse recall
    cm = ConfusionMatrix(y_true, y_pred)
    tnr = cm.TNR[1]     # pycm returns result for both classes, as it doesn't know the class label of the "positive" cases.
                        # This is why I have to index 1, as that represents the positive case, so its the TNR given 1 as positives.

    return tnr


def sensitivity(y_true, y_pred):  # Recall, TPR
    cm = ConfusionMatrix(y_true, y_pred)
    tpr = cm.TPR[1]     # pycm returns result for both classes, as it doesn't know the class label of the "positive" cases.
                        # This is why I have to index 1, as that represents the positive case.

    return tpr




#### MCs ####
# If MCs without subgroups, then we are comparing gt_mc_r + gt_mc_c to UpperMarrow + LowerMarrow
# If MCs size subgroup, then we are comparing gt_mc_r_size_subgroup + gt_mc_c_size_subgroup to UpperMarrow + LowerMarrow
# If MCs size and lbp then we need the lbp subset and we are comparing gt_mc_r_size_subgroup + gt_mc_c_size_subgroup to UpperMarrow + LowerMarrow in that dataframe
# NB: MC size subgroup just means that MC of width 1 have been counted as absent in the ground truth (it is not a smaller total number of samples)

def pfirrmann_stats(df, group, level, metric, nir):
    
    np_y = df['gt_pf'].astype('int32').to_numpy()
    np_pred = df['Pfirrmann'].astype('int32').to_numpy()

    # Here, the NIR is calculated from the confusion matrix and then used as the H0 for the metric bootstrap.
    # Needs to be included below and in the model output as necessary
    cm = ConfusionMatrix(np_y, np_pred)
    cm.print_matrix()
    
    if nir == 'auto':
        print('nir calculated from confusion matrix')
        print(cm.NIR)
        nir = cm.NIR

    pvalue, metric_val, ci_l, ci_h = bootstrap_test(metric=metric,
                                                    y=np_y,
                                                    preds=np_pred,
                                                    null_hypothesis=nir,  # nir or 0.5
                                                    n_bootstrap=10000,
                                                    seed=12345,
                                                    stratified=True,
                                                    alpha=95)

    results = [[group, level, metric.__name__, metric_val, ci_l, ci_h, pvalue, nir]]
    results_df = pd.DataFrame(results, columns = ['group', 'level', 'metric', 'values', 'values_ci_l', 'values_ci_u', 'p_values', 'no_information_rate'])
    return results_df

def mc_stats(df, group, level, metric, nir): 
    df = df.dropna(subset=['gt_mc_r'])
    df = df.dropna(subset=['UpperMarrow'])
    if group == 'all':
        np_y_r = df['gt_mc_r'].astype('int32').to_numpy()
        np_pred_r = df["UpperMarrow"].astype('int32').to_numpy()
        np_y_c = df['gt_mc_c'].astype('int32').to_numpy()
        np_pred_c = df["LowerMarrow"].astype('int32').to_numpy()
    elif group == 'lbp':
        df_lbp = df[df['lbp'] == 1].copy()
        np_y_r = df_lbp['gt_mc_r'].astype('int32').to_numpy()
        np_pred_r = df_lbp["UpperMarrow"].astype('int32').to_numpy()
        np_y_c = df_lbp['gt_mc_c'].astype('int32').to_numpy()
        np_pred_c = df_lbp["LowerMarrow"].astype('int32').to_numpy()
    elif group == 'size':
        np_y_r = df['gt_mc_r_size_subgroup'].astype('int32').to_numpy()
        np_pred_r = df["UpperMarrow"].astype('int32').to_numpy()
        np_y_c = df['gt_mc_c_size_subgroup'].astype('int32').to_numpy()
        np_pred_c = df["LowerMarrow"].astype('int32').to_numpy()
    elif group == 'lbp_size':
        df_lbp = df[df['lbp'] == 1].copy()
        np_y_r = df_lbp['gt_mc_r_size_subgroup'].astype('int32').to_numpy()
        np_pred_r = df_lbp["UpperMarrow"].astype('int32').to_numpy()
        np_y_c = df_lbp['gt_mc_c_size_subgroup'].astype('int32').to_numpy()
        np_pred_c = df_lbp["LowerMarrow"].astype('int32').to_numpy()

    np_y = np.concatenate([np_y_r, np_y_c])
    np_pred = np.concatenate([np_pred_r, np_pred_c])

    # Here, the NIR is calculated from the confusion matrix and then used as the H0 for the metric bootstrap.
    # Needs to be included below and in the model output as necessary
    cm = ConfusionMatrix(np_y, np_pred)
    cm.print_matrix()
    
    if nir == 'auto':
        print('nir calculated from confusion matrix')
        print(cm.NIR)
        nir = cm.NIR

    pvalue, metric_val, ci_l, ci_h = bootstrap_test(metric=metric,
                                                    y=np_y,
                                                    preds=np_pred,
                                                    null_hypothesis=nir,  # nir or 0.5
                                                    n_bootstrap=10000,
                                                    seed=12345,
                                                    stratified=True,
                                                    alpha=95)

    results = [[group, level, metric.__name__, metric_val, ci_l, ci_h, pvalue, nir]]
    results_df = pd.DataFrame(results, columns = ['group', 'level', 'metric', 'values', 'values_ci_l', 'values_ci_u', 'p_values', 'no_information_rate'])
    return results_df


file = "./data/df.csv"
data = pd.read_csv(file)

#### Pfirrmann grades ####

# metrics = [accuracy_score, balanced_accuracy_score, sensitivity_micro, specificity_micro, cohen_kappa_score, matthews_corrcoef, ccc, gwetsac]
# nirs = ['auto', 0.25, 0.5, 0.5, 0.4, 0.4, 0.4, 0.4]
# levels = ['all', "L1-L2", "L2-L3", "L3-L4", "L4-L5", "L5-S1"]
# groups = ['all', 'lbp', 'size', 'lbp_size']

# collated_results = []

# for i in range(len(metrics)):
#     metric = metrics[i]
#     nir = nirs[i]

#     #### All participants ####
#     group = 'all'
#     for level in levels:
#         if level == 'all':
#             df = data.copy()
#             results = pfirrmann_stats(df, group, level, metric, nir)
#             collated_results.append(results)
#         else:
#             df = data[data['Level'] == level].copy()
#             results = pfirrmann_stats(df, group, level, metric, nir)
#             collated_results.append(results)

#     #### LBP subgroup ####
#     group = 'lbp'
#     lbp = data[data['lbp'] == 1].copy()
#     for level in levels:
#         if level == 'all':
#             df = data.copy()
#             results = pfirrmann_stats(df, group, level, metric, nir)
#             collated_results.append(results)
#         else:
#             df = lbp[lbp['Level'] == level].copy()
#             results = pfirrmann_stats(df, group, level, metric, nir)
#             collated_results.append(results)

# results = pd.concat(collated_results)
# results.to_csv('pfirrmann_grading_results.csv')

#### Modic changes ####

metrics = [accuracy_score, balanced_accuracy_score, sensitivity, specificity, cohen_kappa_score, matthews_corrcoef, gwetsac]
nirs = ['auto', 0.5, 0.5, 0.5, 0.4, 0.4, 0.4]
levels = ['all', "L1-L2", "L2-L3", "L3-L4", "L4-L5", "L5-S1"]
groups = ['all', 'lbp', 'size', 'lbp_size']

collated_results = []

for i in range(len(metrics)):
    metric = metrics[i]
    
    nir = nirs[i]

    for j in groups:
        

        for level in levels:
            print(metric.__name__)
            print(j)
            print(level)
            if level == 'all':
                df = data.copy()
                results = mc_stats(df, group=j, level=level, metric=metric, nir=nir)
                collated_results.append(results)
            else:
                df = data[data['Level'] == level].copy()
                results = mc_stats(df, group=j, level=level, metric=metric, nir=nir)
                collated_results.append(results)

results = pd.concat(collated_results)
results.to_csv('mc_grading_results.csv')