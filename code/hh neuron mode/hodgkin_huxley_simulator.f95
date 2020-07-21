program hodgkin_huxley_simulator
  ! this  program once compiled and executed ask for the input constant current and then simulate the voltage according to hodgkin huxley mode.

  !should be run in a linux based system with a terminal and ffmpeg and gnuplot should be installed to get the animation. administrative previlages are assumed.

  !the function written for gate variables can be modified and can be used as such also finding the limiting values of respective gating variables

  !Euler's method is used for solving the equations, can be easily updated to RK4 for reducing the error

  !a folder named animation1 will be created in the parent folder where plotted images will be saved by the name hodgkin_huxley.mp4

  !output video will be created with name 
  implicit none
  real::voltage,tau_m,m_infty,n_infty,tau_n,h_infty,tau_h, I_k,n_initial_k,n_final_k,step_s,a_n,b_n,final_pp,initial_pp,I_m
  real::a_m,b_m,a_h,b_h,I_na,I_l,m_initial,m_final,h_initial,h_final,I_total,voltage_final,dV,x_begin,x_close
  real , parameter:: V_k=-12.14,V_Na=115.0,V_l=10.6,G_Na=120,G_K=36,G_l=0.03,c_m=1.0
  integer:: i,j,k,mn,mm

  write(*,*) "please write the applied current in the real format:"
  read(*,*) I_m !reading the value of external current while running the program
  voltage=0.0
  n_initial_k= alpha_n(voltage)/(alpha_n(voltage)+beta_n(voltage))
  m_initial= alpha_m(voltage)/(alpha_m(voltage)+beta_m(voltage))
  h_initial= alpha_h(voltage)/(alpha_h(voltage)+beta_h(voltage))
  n_final_k=0.0
  m_final=0.0
  h_final=0.0
  initial_pp=.03
  final_pp=50.0
  step_s=(final_pp-initial_pp)/1200
  open(11,file="hodgkin huxley with constant current")
  call execute_command_line('mkdir "animation1"')
  mn=0
  x_begin=0.0
  x_close=50.0
  open(211,file="gating_variable.dat")
  do i=1,1200
     mm= mod(i,12)
     if(i==1 .or. mm==0 ) then
        write(11,*) initial_pp,voltage
        open(12,file='gnucommand')
 
     write(12,*) 'set terminal png'
     write(12,*) 'set grid'
     write(12,*) 'set xlabel "time in ms"'
     write(12,*) 'set ylabel "voltage in mV"'
     write(12,*) "set title 'Hodgkin huxley model with constant applied current'"
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

     write(12,*) 'set yrange [-20:115]'
     write(12,*) 'set xrange [',x_begin,':',x_close,']'
     write(12,*) 'plot  "hodgkin huxley with constant current" w lp'
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
     write(211,*) initial_pp,m_initial,h_initial,n_initial_k
     
     m_initial=m_final
     h_initial=h_final
     n_initial_k=n_final_k
     voltage=voltage_final
     initial_pp=final_pp
end do
close(11)

close(211)
call execute_command_line('ffmpeg -framerate 25 -i animation1/%03d.png -c:v libx264 -r 30 -pix_fmt yuv420p hodgkin_huxley.mp4')


call execute_command_line('rm -f  "gnucommand"')
call execute_command_line("rm -f 'hodgkin huxley with constant current'")

contains
  function alpha_m(vol)
    real:: alpha_m, vol
    alpha_m=(2.5-0.1*vol)/(exp(2.5-0.1*vol)-1)
  end function alpha_m
  function alpha_n(vol)
    real:: alpha_n, vol
    alpha_n=(0.1-0.01*vol)/(exp(1.0-0.1*vol)-1)
  end function alpha_n
  function beta_h(vol)
    real:: beta_h, vol
    beta_h=1.0/(1.0+exp(3-0.1*vol))
  end function beta_h
  function alpha_h(vol)
    real:: alpha_h, vol
    alpha_h=0.07*exp(-Vol/20.0)
  end function alpha_h
  function beta_m(vol)
    real:: beta_m, vol
    beta_m=4.0*exp(-vol/18)
  end function beta_m
  function beta_n(vol)
    real:: beta_n, vol
    beta_n=0.125*exp(Vol/80)
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

end program hodgkin_huxley_simulator
