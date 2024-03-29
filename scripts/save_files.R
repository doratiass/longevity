save(ctl_grid,df_split,df_test,
     df_train,df_train_cv,final_lasso,final_lasso_fit,
     final_xgb,final_xgb_fit,lambda_grid,lasso_best_auc,
     lasso_rec,lasso_res,lasso_spec,
     lasso_wf,ml_df,thresh_corr,thresh_other,
     xgb_best_auc,xgb_grid,xgb_prep,
     xgb_rec,xgb_res,xgb_spec,xgb_wf, file = "raw_data/models.RData")

save(deps, 
     shap_exp_lasso, shap_exp_log, shap_imp_bar_xgb,
     shap_imp_bee_xgb, shap_imp_lasso, shap_imp_log,
     shap_lasso, shap_log, shap_xgb,
     file = "raw_data/plots_shap.RData")

save(shap_xgb, file = "/Users/doratias/Documents/stat_projects/private_data/longevity/shap.RData")

