save(cal_plot,ctl_grid,df_split,df_test,
     df_train,df_train_cv,final_lasso,final_lasso_fit,
     final_xgb,final_xgb_fit,lambda_grid,lasso_best_auc,
     lasso_prep,lasso_rec,lasso_res,lasso_spec,
     lasso_wf,ml_df,thresh_corr,thresh_other,
     xgb_bake,xgb_best_auc,xgb_grid,xgb_prep,
     xgb_rec,xgb_res,xgb_spec,xgb_wf, file = "raw_data/models.RData")

save(cal_fig_lasso, cal_fig_log, cal_fig_xgb, cal_plot,
     dep_plot, dep_plot_list, deps, 
     shap_exp_lasso, shap_exp_log, shap_imp_bar_xgb,
     shap_imp_bee_xgb, shap_imp_lasso, shap_imp_log,
     shap_imp_xgb, shap_lasso, shap_log, shap_xgb,
     file = "raw_data/plots_shap.RData")
