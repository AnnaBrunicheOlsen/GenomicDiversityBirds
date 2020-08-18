data {
  int<lower=0> nspecies;
  int<lower=0> nobs;
  int<lower=0> species[nobs];
  vector[nobs] logNe;
  vector[nobs] area;
  //vector[nobs] threat;
  vector[nobs] NT;
  vector[nobs] VU;
  vector[nobs] EN;
  vector[nobs] CR;
  vector[nobs] fruit;
  vector[nobs] omni;
  vector[nobs] plant;
  vector[nobs] invert;
  vector[nobs] migrates;
  vector[nobs] lat;
  vector[nobs] mass;
}

parameters {
  real<lower=0> eps;
  vector[nspecies] alpha;
  real grand_mean;
  real<lower=0> grand_sd;
  vector[nspecies] b_area;
  real<lower=0> ar_sd;
  real ar_mean;
  //real b_threat;
  real b_nt;
  real b_vu;
  real b_en;
  real b_cr;
  real b_fruit;
  real b_plant;
  real b_invert;
  real b_omni;
  real b_mig;
  real b_lat;
  real b_mass;

}

transformed parameters {
  
  

  vector[nobs] lp;
  for (i in 1:nobs){
    lp[i] = alpha[species[i]] + b_area[species[i]]*area[i] +
      b_fruit*fruit[i] + b_plant*plant[i] + b_omni*omni[i] + b_invert*invert[i] +
      b_mig*migrates[i] + b_lat*lat[i] + b_mass*mass[i] + //+ b_threat*threat[i];
      b_nt*NT[i] + b_vu*VU[i] + b_en*EN[i] + b_cr*CR[i];
  }
    



}

model {
  
  alpha ~ normal(grand_mean, grand_sd);
  b_area ~ normal(ar_mean, ar_sd);

  logNe ~ normal(lp, eps);

}

generated quantities {
  

  vector[nobs] yrep;
  real fruit_plant;
  real fruit_invert;
  real fruit_omni;
  real plant_invert;
  real plant_omni;
  real invert_omni;
  real nt_vu;
  real nt_en;
  real nt_cr;
  real vu_en;
  real vu_cr;
  real en_cr;
  
  for (i in 1:nobs){
    yrep[i] = normal_rng(lp[i], eps);
  }

  fruit_plant = b_fruit - b_plant;
  fruit_invert = b_fruit - b_invert;
  fruit_omni = b_fruit - b_omni;
  plant_invert = b_plant - b_invert;
  plant_omni = b_plant - b_omni;
  invert_omni = b_invert - b_omni;

  nt_vu = b_nt - b_vu;
  nt_en = b_nt - b_en;
  nt_cr = b_nt - b_cr;
  vu_en = b_vu - b_en;
  vu_cr = b_vu - b_cr;
  en_cr = b_en - b_cr;

}
