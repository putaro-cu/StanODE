functions { // モデル式の宣言
  vector beekman(real t, vector x, array[] real par) {
    vector[1] dxdt;
    
    real a = par[1]/10000;
    real b = par[2]/100000;
    real s = par[3];
    real n_total = par[4];
    
    dxdt[1] = (a + b * x[1]) * (n_total - x[1]) - (s * x[1]) / (s + x[1]);
    
    return dxdt;
  }
}

data {
  int<lower=0> Series; // 時系列のデータ数
  int<lower=0> N; // 1時系列のデータ数
  array[Series, N] real<lower=0> ts; // 時間
  array[Series, N] real<lower=0> x; // ts=tにおける個体数
  array[Series, N] real<lower=0> n_total; // 総個体数
}

parameters {
  real<lower=0> a;
  real<lower=0> b;
  real<lower=0> s;
  array[Series] real<lower=0> sigma;
  array[Series] vector<lower=0>[1] x0; 
}

transformed parameters {
  array[Series, 4] real par;
  for (i in 1:Series) {
    par[i, 1] = a;
    par[i, 2] = b;
    par[i, 3] = s;
    par[i, 4] = n_total[i, 1];
  }
}

model {

  // priors
  a ~ normal(0, 100);
  b ~ normal(0, 100);
  s ~ normal(0, 100);
  
  for (i in 1:Series) {
    sigma[i] ~ cauchy(0, 2.5);
    x0[i, 1] ~ uniform(0, n_total[i, 1]);
  }
  
  // 各時系列ごとにODEを解く
  for (i in 1:Series) {
    array[N] vector[1] mu = ode_rk45(beekman, x0[i,], 0, ts[i, 1:N], par[i,]); 
    for (j in 1:N) {
      x[i,j] ~ normal(mu[j], sigma[i]);
    }
  }
}

generated quantities {
  array[Series, N] vector[1] mu_pred;
  for (i in 1:Series){
    mu_pred[i,] = ode_rk45(beekman, x0[i,], 0, ts[1, 1:N], par[i,]);
  }
}