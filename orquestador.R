# Based on the parameters stored in the configuration file (settings.json), it executes
# the requested function
# - Creation: It launchs the creation scripts to generate the master tables to train the new models
# - Scoring: It launchs the scoring scripts either the new leads or to check the performance
# - Training: It launchs the training scripts to create new models (either tcredito or crediservice)
# It gives back an error if the parameter if not "Creation", "Scoring" or "Training"
switch(
  job,
  "Creation" = {
      create_monthly_tables(month_to_create
                            #original_path,
                            #staging_path,
                            #feature_path,
                            #master_path)
      )
  },
  "Scoring" = {
    score_mensual(model_alias_scoring,
                  date_to_score,
                  performance_calculation)
  },
  "Training" = {
    create_model(models_path,
                 model_alias,
                 master_path,
                 train_months,
                 dev_months,
                 test_months)
  },
  stop("It only accepts Creation|Scoring|Training ")
)