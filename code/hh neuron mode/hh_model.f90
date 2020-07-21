program hh_model
 ! use functions
 ! use integrating_module
  !use plotting_module
  implicit none
  real::voltage,tau_m,m_infty,n_infty,tau_n,h_infty,tau_h, I_k,n_initial_k,n_final_k,step_s,a_n,b_n,final_pp,initial_pp,I_m
  real::a_m,b_m,a_h,b_h,I_na,I_l,m_initial,m_final,h_initial,h_final,I_total,voltage_final,dV
  real , parameter:: V_k=-72.14,V_Na=55.17,V_l=-49.42,G_Na=1.2,G_K=0.36,G_l=0.003,c_m=0.01
  integer:: i,j,k,mn,mm

 !membrane current is to be simulated with change in the gating variables
  voltage=-67.5
  I_m=1.0
  n_initial_k= alpha_n(voltage)/(alpha_n(voltage)+beta_n(voltage))
  m_initial= alpha_m(voltage)/(alpha_m(voltage)+beta_m(voltage))
  h_initial= alpha_h(voltage)/(alpha_h(voltage)+beta_h(voltage))
  n_final_k=0.0
  m_final=0.0
  h_final=0.0
  initial_pp=.03
  final_pp=50.0
  step_s=(final_pp-initial_pp)/1200
  open(11,file="current=0.5.dat")
  call execute_command_line('mkdir "animation1"')
  mn=0
  do i=1,1200
     mm= mod(i,12)
     if(i==0 .or. mm==0 ) then
        write(11,*) initial_pp,voltage
   open(12,file='gnucommand')
     write(12,*) 'set terminal png'
     write(12,*) 'set grid'
     if (mn<10) then
        write(12,112) 'set output "animation1/',0,0,mn,'.png"'
112     format(A23,i1,i1,i1,A5)
     else if(mn<100 .and. mn>=10) then
        write(12,113) 'set output "animation1/',0,mn,'.png"'
113     format(A23,i1,i2,A5)
     else
        write(12,114) 'set output "animation1/',mn,'.png"'
114     format(A23,i3,A5)
     end if

     write(12,*) 'set yrange [-80:80]'
     write(12,*) 'set xrange [0.0:50]'
     write(12,*) 'plot  "current=0.5.dat" w lp'
     !120  format(A5,F10.10,A1,F10.10,A21)
   
     write(12,*) 'replot'
     write(12,*) 'set output'
     write(12,*) 'exit'
     close(12)
     call execute_command_line('gnuplot "gnucommand"')
     mn=mn+1
  end if
        
     I_l = G_l*(voltage-V_l)
     final_pp=initial_pp+step_s
     a_n= alpha_n(voltage)
     b_n= beta_n(voltage)
     a_m= alpha_m(voltage)
     b_m= beta_m(voltage)
     a_h= alpha_h(voltage)
     b_h= beta_h(voltage)
     
     n_final_k = n_initial_k+ step_s*(potassium_open(initial_pp,n_initial_k,a_n,b_n))
     m_final = m_initial+ step_s*(sodium_open(initial_pp,m_initial,a_m,b_m))
     h_final = h_initial+ step_s*(sodium_close(initial_pp,h_initial,a_h,b_h))
     i_na= G_Na*(m_initial**3)*(h_initial)*(voltage-V_na) !sodium current
     i_k=G_K*(n_initial_k**4)*(voltage-V_k) !potassium current

     voltage_final=voltage + step_s*(voltage_function(I_m,I_k,I_na,I_l))
     !
     !
     m_initial=m_final
     h_initial=h_final
     n_initial_k=n_final_k
     voltage=voltage_final
     initial_pp=final_pp
end do
close(11)


call execute_command_line('ffmpeg -framerate 25 -i animation1/%03d.png -c:v libx264 -r 30 -pix_fmt yuv420p s_videos/out.mp4')




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

  function voltage_function(I_m,I_k,I_na,I_l) ! gives the function corresponding to dV/dt
    real:: I_m,I_k,I_na,I_l,voltage_function
    voltage_function= (I_m-(I_k+I_na+I_l))/c_m
  end function voltage_function

end program hh_model
