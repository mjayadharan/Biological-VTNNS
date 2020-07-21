program hh_model
  use functions
  use integrating_module
  use plotting_module
  implicit none
  real::voltage,tau_m,m_infty,n_infty,tau_n,h_infty,tau_h
  integer:: i,j,k
  open(1,file='datafile.dat')
  do i=-121,80,4
     tau_m= 1/(alpha_m(i+0.0)+beta_m(i+0.0))
     m_infty=alpha_m(i+0.0)*tau_m
     write(1,*) i,m_infty
  end do
  close(1)
 open(12,file='gnucommand')
     write(12,*) 'set terminal png'
  write(12,*) 'set output "m_infty.png"'
  write(12,*) 'plot "m_infty.dat" with lines'
  write(12,*) 'replot'
  write(12,*) 'set output'
  write(12,*) 'exit'
  close(12)     
  call execute_command_line('gnuplot "gnucommand"')


    open(2,file='n_infty.dat')
  do i=-121,80,4
     tau_n= 1/(alpha_n(i+0.0)+beta_n(i+0.0))
     n_infty=alpha_n(i+0.0)*tau_n
     write(2,*) i,n_infty

  end do
  close(2)
  
 open(12,file='gnucommand')
     write(12,*) 'set terminal png'
  write(12,*) 'set output "n_infty.png"'
  write(12,*) 'plot "n_infty.dat" with lines'
  write(12,*) 'replot'
  write(12,*) 'set output'
  write(12,*) 'exit'
  close(12)     
  call execute_command_line('gnuplot "gnucommand"')

    open(3,file='datafile2.dat')
  do i=-121,80,4
     tau_h= 1/(alpha_h(i+0.0)+beta_h(i+0.0))
     h_infty=alpha_h(i+0.0)*tau_h
     write(3,*) i,h_infty

  end do
  close(4)
open(4,file='datafile3.dat')
  do i=-121,80,4
     tau_n= 1/(alpha_n(i+0.0)+beta_n(i+0.0))
     n_infty=alpha_n(i+0.0)*tau_n
     write(4,*) i,n_infty

  end do
  close(4)
  
 open(12,file='gnucommand')
     write(12,*) 'set terminal png'
  write(12,*) 'set output "h_infty.png"'
  write(12,*) 'plot "h_infty.dat" with lines'
  write(12,*) 'replot'
  write(12,*) 'set output'
  write(12,*) 'exit'
  close(12)     
  call execute_command_line('gnuplot "gnucommand"')
  call graph_video(-121.0,79.0)


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
 

end program hh_model
