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

    return metric_vals, p_value, metric_val, ci_l, ci_h


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

# file = "./data/df.csv"
file = "./data/previously_untested_subset.csv"
data = pd.read_csv(file)

# values = []
# # nir = []      # nir = the no information rate
# values_ci_l = []
# values_ci_u = []
# pvalues = []
# group = []

data = data.dropna(subset=['gt_mc_r'])
data = data.dropna(subset=['UpperMarrow'])

np_y_r = data['gt_mc_r'].astype('int32').to_numpy()
np_pred_r = data["UpperMarrow"].astype('int32').to_numpy()
np_y_c = data['gt_mc_c'].astype('int32').to_numpy()
np_pred_c = data["LowerMarrow"].astype('int32').to_numpy()

np_y = np.concatenate([np_y_r, np_y_c])
np_pred = np.concatenate([np_pred_r, np_pred_c])

# Here, the NIR is calculated from the confusion matrix and then used as the H0 for the metric bootstrap.
# Needs to be included below and in the model output as necessary
cm = ConfusionMatrix(np_y, np_pred)
cm.print_matrix()
print(cm.NIR)
# nir = cm.NIR

metric = balanced_accuracy_score

metric_vals, pvalue, metric_val, ci_l, ci_h = bootstrap_test(metric=metric,
                                                                y=np_y, preds=np_pred,
                                                                null_hypothesis=0.5,  # nir or 0.5
                                                                n_bootstrap=10000,
                                                                seed=12345,
                                                                stratified=True,
                                                                alpha=95)

# group.append(i)
# values.append(metric_val)
# values_ci_l.append(ci_l)
# values_ci_u.append(ci_h)
# pvalues.append(pvalue)
# print('nir')
# print(nir)
print(str(metric))
print(metric_val)
print('ci_l')
print(ci_l)
print('ci_h')
print(ci_h)
print('pvalue')
print(pvalue)

# results = pd.DataFrame(list(zip(group, values, values_ci_l, values_ci_u, pvalues)),  # ,nirs
#                         columns=['group', 'values', 'values_ci_l', 'values_ci_u', 'p_values']) 