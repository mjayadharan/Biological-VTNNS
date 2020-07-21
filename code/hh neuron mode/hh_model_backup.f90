program hh_model
 ! use functions
  use integrating_module
  use plotting_module
  implicit none
  real::voltage,tau_m,m_infty,n_infty,tau_n,h_infty,tau_h, I_k,n_initial_k,n_final_k,step_s,a_n,b_n,final_pp,initial_pp
  real::a_m,b_m,a_h,b_h,I_na,I_l,m_initial,m_final,h_initial,h_final,I_total
  real , parameter:: V_k=-77.0,V_Na=50.0,V_l=-54.402,G_Na=120.0,G_K=36.0,G_l=0.3,capac=1.0
  integer:: i,j,k

 !first going to model the potassium current with a fixed activation potential, assuming an initial value for gating variable n
  voltage=50.0
  I_l = G_l*(voltage-V_l)
  a_n= alpha_n(voltage)
  b_n= beta_n(voltage)
  a_m= alpha_m(voltage)
  b_m= beta_m(voltage)
  a_h= alpha_h(voltage)
  b_h= beta_h(voltage)
  n_initial_k= 0.0
  m_initial=0.0
  h_initial=.99
  initial_pp=.03
  final_pp=5.0
  step_s=(final_pp-initial_pp)/50
  open(11,file="potassium_current.dat")
  do i=1,50
     final_pp=initial_pp+step_s

     call euler_four(potassium_open,0.001,initial_pp,final_pp,n_initial_k,n_final_k,a_n,b_n)
     !write(*,*) initial_pp, final_pp
     i_k=G_K*(n_final_k**4)*(voltage-V_k) !potassium current
     n_initial_k=n_final_k

     
     call euler_four(sodium_open,0.001,initial_pp,final_pp,m_initial,m_final,a_m,b_m)
     call euler_four(sodium_close,0.001,initial_pp,final_pp,h_initial,h_final,a_h,b_h)
     i_na= G_Na*(m_final**3)*(h_final)*(voltage-V_na) !sodium current
     m_initial=m_final
     h_initial=h_final
     write(*,*) i_k, i_na
     i_total= -1*(i_na+i_k+i_l)    !total current
     write(11,*) initial_pp, i_total

     initial_pp= initial_pp+step_s
end do
close(11)







contains
  function alpha_m(vol)
    real:: alpha_m, vol
    alpha_m=(0.1*(vol+40.0))/(1-exp(-0.1*(vol+40.0)))
  end function alpha_m
  function alpha_n(vol)
    real:: alpha_n, vol
    alpha_n=(0.01*(vol+55.0))/(1-exp(-0.1*(vol+55.0)))
  end function alpha_n
  function beta_h(vol)
    real:: beta_h, vol
    beta_h=1.0/(1+exp(-0.1*(vol+35.0)))
  end function beta_h
  function alpha_h(vol)
    real:: alpha_h, vol
    alpha_h=0.07*exp(-0.05*(Vol+65.0))
  end function alpha_h
  function beta_m(vol)
    real:: beta_m, vol
    beta_m=4.0*exp(-0.0556*(Vol+65.0))
  end function beta_m
  function beta_n(vol)
    real:: beta_n, vol
    beta_n=0.125*exp(-0.0125*(Vol+65.0))
  end function beta_n

  function potassium_open(x,y,a_n,b_n) !differential function for gate variable
    real:: potassium_open,x,y,a_n,b_n,volt
    potassium_open= a_n*(1-y) - b_n*y

  end function potassium_open

  function sodium_open(x,y,a_m,b_m) !differential function for gate variable
    real:: sodium_open,x,y,a_m,b_m,volt
    sodium_open= a_m*(1-y) - b_m*y

  end function sodium_open
  
  function sodium_close(x,y,a_h,b_h) !differential function for gate variable
    real:: sodium_close,x,y,a_h,b_h,volt
    sodium_close= a_h*(1-y) - b_h*y

  end function sodium_close
  

end program hh_model
