setwd("/workpath/")
library(dplyr)
library(TwoSampleMR)
library(MendelianRandomization)

exposure_with_confounder_dat_list=""
outcome_dat_list=""

twosample_mvmr_function=function(exposure_with_confounder_dat_list=NULL,exposure_name=NULL,
                                 outcome_dat_list=NULL,outcome_name=NULL){
  if(is.null(exposure_name)){
    exposure_name=names(exposure_with_confounder_dat_list)
  }
  if(is.null(outcome_name)){
    outcome_name=names(outcome_dat_list)
  }
  mvmr_res_list<-list()
  k=1
  for (i in 1:length(exposure_with_confounder_dat_list)) {
    dat_exp_mv=exposure_with_confounder_dat_list[[i]]
    for (j in 1:length(outcome_dat_list)) {
      dat_out<-outcome_dat_list[[j]]
      if(!is.null(dim(dat_out))){
        mvdat <- mv_harmonise_data(
          exposure_dat = dat_exp_mv, 
          outcome_dat = dat_out,
          harmonise_strictness=2
        )
        #main_res
        mvmr_input<-mr_mvinput(bx=mvdat$exposure_beta ,bxse = mvdat$exposure_se,
                               by=mvdat$outcome_beta,byse=mvdat$outcome_se)
        
        res1<-mr_mvivw(mvmr_input,model = "default",correl = FALSE,distribution = "normal",alpha = 0.05)
        res2<-mr_mvegger(mvmr_input,orientate = 1,correl = FALSE,distribution = "normal",alpha = 0.05)
        res3<-mr_mvmedian(mvmr_input,distribution = "normal",alpha = 0.05,iterations = 10000,seed = 314159265)
        res4<-mr_mvlasso (mvmr_input,orientate = 1,distribution = "normal",alpha = 0.05,lambda = numeric(0))
        
        mvmr_list_inter=list(res_mvivw=res1,
                             res_mvegger=res2,
                             res_mvmedian=res3,
                             res_mvlasso=res4)
        
        mvmr_res_list[[k]]=mvmr_list_inter
        names(mvmr_res_list)[k]=paste0(exposure_name[i],"_",outcome_name[j])
        k=k+1
        
      }
    }
  }
  return(mvmr_res_list)
}

res_all_mv=twosample_mvmr_function(exposure_with_confounder_dat_list=exposure_with_confounder_dat_list,
                                   exposure_name=names(exposure_with_confounder_dat_list),
                                   outcome_dat_list=outcome_dat_list,
                                   outcome_name=names(outcome_dat_list))
