program lifmodel
  use plotting_module
  !coupling of two neurons will be simulated and graphed using rk4 and gnuplot resp.
  !a normalised form as given in collin thesis is being employed so that the V_reset is zero and V_threshold=1.0 and some other normalisations are made ,
  !please refer the report or collin thesis.
  implicit none
  real , parameter :: g_c=0.2
  real :: v_1,v_2,beta ,delta,v_threshold,v_reset,I_app,I_total,k1,k2,k3,k4,l1,l2,l3,l4
  real:: time_initial,time_range,step_size,dummy1,dummy2,error1
  integer::n1,n2,n3

  beta = 0.2
  v_1=0.59
  v_2=0.0
  I_total = 3.0
  step_size = 0.001
  error1=0.001
  time_initial=15.0
  time_range = 45.0
  open(1,file="datapoints.dat")
  do n2=0,14
     write(1,*) n2,v_1,v_2,0.0
  end do
  n1 = int((time_range-time_initial)/step_size)
  do n2= 1,n1
     if(abs(v_1-1.0) <error1) Then
        v_1=3.0
     else if(abs(v_1-3.0) <0.2) then
           v_1=0.0
     end if

     if(abs(v_2-1.0) <error1) then
        v_2=3.0
     else if(abs(v_2-3.0) <0.2) then
        v_2=0.0
     end if
     
     write(1,*) time_initial, v_1,v_2,I_total
     k1 =  neuron1_voltage(v_1,v_2,I_total)
     l1=  neuron2_voltage(v_1,v_2,I_total)
     k2 =  neuron1_voltage(v_1+0.5*step_size*k1,v_2+0.5*step_size*l1,I_total)
     l2 =  neuron2_voltage(v_1+0.5*step_size*k1,v_2+0.5*step_size*l1,I_total)
     k3 =  neuron1_voltage(v_1+0.5*step_size*k2,v_2+0.5*step_size*l2,I_total)
     l3 =  neuron2_voltage(v_1+0.5*step_size*k2,v_2+0.5*step_size*l2,I_total)
     k4 =  neuron1_voltage(v_1+step_size*k3,v_2+step_size*l3,I_total)
     l4 =  neuron2_voltage(v_1+step_size*k3,v_2+step_size*l3,I_total)
     v_1 = v_1 + (step_size/6)*(k1+ 2.0*k2 + 2*k3 + k4)
     v_2 = v_2 + (step_size/6)*(l1+ 2.0*l2 + 2*l3 + l4)
     time_initial= time_initial + step_size
  end do
close(1)
  call graph_video(0.0,40.0)

  
contains
  function neuron1_voltage(v_1,v_2,I)
    real:: neuron1_voltage,v_1,v_2,I
    neuron1_voltage = -1.0*v_1 + I + g_c*(v_2 - v_1)
  end function neuron1_voltage

  function neuron2_voltage(v_1,v_2,I)
    real:: neuron2_voltage,v_1,v_2,I
    neuron2_voltage = -1.0*v_2 + I + g_c*(v_1 - v_2)
  end function neuron2_voltage

end program lifmodel
  
