data {
  int media_type;
  int time_type;
  int P;
  int<upper=time_type> max_lag;
  int<upper=time_type> train_period;
  
  matrix[media_type, time_type] media;
  vector[time_type] normalized_time;
  array[time_type] real outcome;
}
parameters {
  real<lower=0> d_alpha;                                       // ディリクレ過程の全体のパラメータ
  vector<lower=0, upper=1>[P - 1] breaks;  // ディリクレ過程のstick-breaking representationのためのパラメータ
  vector[P] trend_middle_point;
  vector<lower=0>[P] trend_spread;
  vector[P] trend;
  vector<lower=0>[media_type] adweight_sigma;
  array[media_type] vector[max_lag] adweight_unnormalized;
  
  real<lower=0> sigma;
  real intercept;
  vector<lower=0>[media_type] beta_sigma;
  vector[media_type] beta;
}
transformed parameters {
  simplex[P] z; //
  matrix[media_type, time_type] adstock;
  for (m in 1:media_type){
    for (t in 1:time_type){
      if (t < max_lag){
        adstock[m, t] = softmax(adweight_unnormalized[m,1:t]) '* media[m, 1:t]';
      }
      else {
        adstock[m, t] = softmax(adweight_unnormalized[m, :]) '* media[m, (t - max_lag + 1):t]';
      }
    }
  }
  {
    // stick-breaking representationの変換開始
    // https://discourse.mc-stan.org/t/better-way-of-modelling-stick-breaking-process/2530/2 を参考
    z[1] = breaks[1];
    real sum = z[1];
    for (p in 2:(P - 1)) {
      z[p] = (1 - sum) * breaks[p];
      sum += z[p];
    }
    z[P] = 1 - sum;
  }
}
model {
  d_alpha ~ gamma(0.1, 0.1);
  breaks ~ beta(1, d_alpha);
  
  trend_middle_point ~ normal(0, 1);
  trend_spread ~ gamma(0.1, 0.1);
  trend ~ normal(0, 1);
  
  adweight_sigma ~ gamma(0.1, 0.1);
  
  
  for (m in 1:media_type){
    adweight_unnormalized[m, 1] ~ normal(0, 1);
    for (l in 2:max_lag)
    adweight_unnormalized[m, l] ~ normal(adweight_unnormalized[m, l - 1], adweight_sigma[m] ^ 2);
  }
  
  sigma ~ gamma(0.1, 0.1);
  intercept ~ normal(0, 10);
  beta_sigma ~ gamma(0.1, 0.1);
  
  beta ~ normal(0, beta_sigma .^ 2);
  
  for (t in 1:train_period){
    vector[P] case_vector;
    for (p in 1:P){
      case_vector[p] = normal_lpdf(normalized_time[t] | trend_middle_point[p], trend_spread[p] ^ 2) + 
                       normal_lpdf(outcome[t] | intercept + normalized_time[t] * trend[p] + beta '* adstock[:, t], sigma ^ 2);
    }
    target += log_sum_exp(case_vector);
  }
}
generated quantities {
  array[time_type] vector[P] estimated_state;
  
  for (t in 1:time_type){
    estimated_state[t] = rep_vector(0.0, P);
  }
  
  vector[time_type] predicted_trend;
  vector[time_type] predicted_outcome;
  matrix[media_type, time_type] ad_contribution;
  
  for (t in 1:time_type){
    vector[P] case_vector;
    for (p in 1:P){
      case_vector[p] = normal_lpdf(normalized_time[t] | trend_middle_point[p], trend_spread[p] ^ 2);
    }
    int state = categorical_rng(softmax(case_vector));
    estimated_state[t, state] = 1.0; 
    predicted_trend[t] = normalized_time[t] * trend[state];
    for (m in 1:media_type){
      ad_contribution[m, t] = beta[m] * adstock[m, t];
    }
    predicted_outcome[t] = normal_rng(intercept + predicted_trend[t] + beta '* adstock[:, t], sigma ^ 2);
  }
}



