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
  int<lower=0> N; // 1時系列のデータ数
  array[N] real<lower=0> ts; // 時間
  array[N] real<lower=0> x; // ts=tにおける個体数
  int<lower=0> n_total; // 総個体数
}

parameters {
  real<lower=0> a;
  real<lower=0> b;
  real<lower=0> s;
  real<lower=0> sigma;
  vector<lower=0>[1] x0; 
}

transformed parameters {
  array[4] real par;
  par[1] = a;
  par[2] = b;
  par[3] = s;
  par[4] = n_total;
}

model {

  // priors
  a ~ normal(0, 100);
  b ~ normal(0, 100);
  s ~ normal(0, 100);
  sigma ~ cauchy(0, 2.5);
  x0[1] ~ normal(100, 10);
  
  array[N] vector[1] mu = ode_rk45(beekman, x0, 0, ts, par); 
  
  for (i in 1:N) {
    x[i] ~ normal(mu[i], sigma);
  }
}

generated quantities {
  array[N] vector[1] mu_pred = ode_rk45(beekman, x0, 0, ts, par);
}
