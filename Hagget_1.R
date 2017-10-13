## PROGRAM NAME: ps4huggett.m
##clear, clc

#% PARAMETERS
beta = .9932; #%discount factor 
sigma = 1.5; #% coefficient of risk aversion
b = 0.5; #% replacement ratio (unemployment benefits)
y_s = c(1, b);# % endowment in employment states
pi = rbind(c(.97, .03), c(.5, .5)); #% transition matrix


#% ASSET VECTOR
a_lo = -2; #%lower bound of grid points
a_hi = 5; #%upper bound of grid points
num_a = 10;

a = seq(from = a_lo, to = a_hi, length.out = num_a); #% asset (row) vector

#% INITIAL GUESS FOR q
q_min = 0.98;
q_max = 1.2;


#% ITERATE OVER ASSET PRICES
aggsav = 1 ;
while (abs(aggsav) >= 0.1){ 

q_guess <- (q_min + q_max)/2
  
#% CURRENT RETURN (UTILITY) FUNCTION
cons = do.call(rbind,lapply(a, function (x) x-(q_guess * a)))
              cons = lapply(y_s,function (x) x+cons) 
              ret = lapply(cons, function (x) (x ^ (1-sigma)) / (1 - sigma)); #% current period utility
              
              #Fixing
              ret[[1]][cons[[1]]<0]<- -Inf
              ret[[2]][cons[[2]]<0]<--Inf
              
              #% INITIAL VALUE FUNCTION GUESS
              v_guess = matrix(rep(0, 2*num_a), 2 ,num_a);
              
              #% VALUE FUNCTION ITERATION
              dis = 1;
              while (dis >.000001){
              #% CONSTRUCT RETURN + EXPECTED CONTINUATION VALUE
              
                value_mat_e = ret[[1]] + beta * ( matrix(rep(pi[1]*v_guess[1,],num_a), num_a , num_a , byrow = TRUE) +   matrix(rep(pi[2]*v_guess[2,],num_a), num_a , num_a , byrow = TRUE)   );
                
                value_mat_u =  ret[[2]] + beta * ( matrix(rep(pi[3]*v_guess[1,],num_a), num_a , num_a , byrow = TRUE) +  matrix(rep(pi[4] * v_guess[2,],num_a), num_a , num_a , byrow = TRUE)   );
                
                
                
                #% find the optimal k' for every k:
                vfn_e = apply(value_mat_e, 1, max);
                
                policy_e<-apply(value_mat_e, 1, which.max)
                
                #vfn_h[vfn_h==-Inf]=0  #necessary adjustment. Infinity is not good to be used in R
                
                #vfn = vfn;
                
                
                vfn_u = apply(value_mat_u, 1, max);
                
                policy_u<-apply(value_mat_u, 1, which.max)
                
                
                #vfn_l[vfn_l==-Inf]=0 
                
                #% what is the distance between current guess and value function
                diffrence_1 = abs(vfn_e - v_guess[1,]);  #One is sufficient
                
                diffrence_2 = abs(vfn_u - v_guess[2,])
                
                #diffrence[is.na(diffrence)]<-0  #We fix for the infinity bug here
                
                dis<-max(c(diffrence_1,diffrence_2))
                #% if distance is larger than tolerance, update current guess and
                #% continue, otherwise exit the loop
                v_guess[1,] <- vfn_e;
                v_guess[2,] <- vfn_u;
                
              }
                
                
                
                
              #% CHOOSE HIGHEST VALUE (ASSOCIATED WITH a CHOICE)
                
                #}

#% KEEP DECSISION RULE

pol_fn<-list()       
                     
pol_fn[[1]] = a[policy_e]

pol_fn[[2]] = a[policy_u]

#% SET UP INITITAL DISTRIBUTION

mu<-matrix(1/(2*num_a),2,num_a)

#mu<-matrix(0,2,num_a)

#mu[1,5]<-0

pol_fn<-rbind(pol_fn[[1]],pol_fn[[2]])

pol_indx<-rbind(policy_e,policy_u)
#% ITERATE OVER DISTRIBUTIONS


muv<-as.vector(mu)

mass<-muv[muv>0]

emp_ind<-apply(mu,1, function (x) sum(x>0))  #How many in each row
emp_ind<-unlist(lapply(emp_ind, function (x) rep(which(emp_ind==x)[1],x)))  #Here is a cheap fix


a_ind<-apply(mu,2, function (x) x>0)

#a_ind<-do.call(rbind,apply(a_ind,1,function (x) which(x)))

a_ind<-apply(a_ind,1,function (x) which(x))  #It is crucual there are no zeroes here

a_ind<-as.vector(t(a_ind))


#mass is a vector of all the actual values greater than zero stacked by column

#emp_ind is an index corresponding to every row index emp and unemp stacked by column

#a_ind is the column index for each non-zero element also as a vector by column

dis=1;
while (dis>0.0000001 ){
#% ITERATE OVER DISTRIBUTIONS
MuNew = matrix(0,2,num_a);


for (ii in 1:length(emp_ind)){
  
apr_ind <- pol_indx[emp_ind[ii],a_ind[ii]]; #% which a prime does the policy fn prescribe?

MuNew[, apr_ind] <- MuNew[, apr_ind] + (pi[emp_ind[ii], ]*mass[ii]);



}

dis = max(abs(mu-MuNew));
mu=MuNew




}


#Markets Clearing

aggsav<-mu[1,]%*%pol_fn[1,]+mu[2,]%*%pol_fn[2,]


if (aggsav>0){
q_min<-q_guess}

else{q_max<-q_guess}

print(aggsav)


}