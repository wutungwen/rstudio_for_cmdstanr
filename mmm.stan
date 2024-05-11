data {
  int media_type;
  int time_type;
  int<upper=time_type> max_lag;
  int<upper=time_type> train_period;
  
  matrix[media_type, time_type] media;
  array[time_type] real outcome;
  
  real outcome_min;
  real outcome_max;
}
parameters {
  array[media_type] real<lower=0,upper=1> alpha;
  array[media_type] real<lower=0,upper=1> theta_normalized;
  array[media_type] real<lower=0> K;
  
  real<lower=0> sigma;
  real intercept;
  vector[media_type] beta;
}
transformed parameters {
  matrix[media_type, max_lag] adweight;
  matrix[media_type, time_type] adstock;
  matrix[media_type, time_type] adstock_hilled;
  for (m in 1:media_type){
    for (l in 1:max_lag){
      adweight[m, l] = alpha[m]^((l - theta_normalized[m] * max_lag)^2);
    }
    
    for (t in 1:time_type){
      if (t < max_lag){
        adstock[m, t] = (adweight[m,1:t] * media[m, 1:t]')/sum(adweight[m,1:t]);
        adstock_hilled[m, t] = 1/(1 + K[m]/adstock[m, t]);
      }
      else {
        adstock[m, t] = (adweight[m, :] * media[m, (t - max_lag + 1):t]')/sum(adweight[m, :]);
        adstock_hilled[m, t] = 1/(1 + K[m]/adstock[m, t]);
      }
    }
  }
}
model {
  alpha ~ beta(5, 5);
  theta_normalized ~ beta(5, 5);
  K ~ gamma(5, 5);
  
  sigma ~ gamma(10, 10);
  intercept ~ normal(0, 1);
  beta ~ normal(0, 1);
  
  for (t in 1:train_period){
    outcome[t] ~ normal(intercept + beta '* adstock_hilled[:, t], sigma);
  }
}
generated quantities {
  vector[time_type] predicted_outcome;
  matrix[media_type, time_type] ad_contribution;
  
  for (t in 1:time_type){
    for (m in 1:media_type){
      ad_contribution[m, t] = beta[m] * adstock_hilled[m, t] * (outcome_max - outcome_min) + outcome_min;
    }
    predicted_outcome[t] = normal_rng(intercept + beta '* adstock_hilled[:, t], sigma) * (outcome_max - outcome_min) + outcome_min;
  }
}



