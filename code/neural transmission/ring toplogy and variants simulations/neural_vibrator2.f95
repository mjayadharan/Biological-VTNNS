program neural_vibrator
  use numerical
  implicit none
  !slight modification from neural_network simulator, here neuron1 acts as an oscillator(origin) to which
  !other neurons are connected. oscillator frequency can be specified
  !also neuron1 is exempted from any kind of parameter update
  integer ,parameter:: num=10
  real(8) , parameter :: V_th=20.0 , V_reset=14.0
  real(8),parameter:: time_bound= 0.5
  
  real(8) , parameter :: tau=20.0, tau_s=5.0/tau
  real(8),parameter:: E_plus = 74.0, E_minus= -6.0
  real(8),parameter:: weight_plus=1.0, weight_minus=-1.0
  real(8),parameter:: adjusting_constant=0.0001,oscillator_period=0.01,time_lag=0.001
  !num is the number of neurons in neural network
  real(8):: neural_network(num,4),weight_matrix(num,num),firing_table(num)
  real(8):: random1,random2,dummy_zero,testing,testing_argument(4),h,dummy_time
  real(8):: next_fire_time,printing_array(num+1),vibrating,adjuster,update_time_table(num),update_time

  integer:: l,d
    neural_network=0.0
    weight_matrix=0.0
!    weight_matrix(1,2)=weight_in
!    weight_matrix(2,3)=
!    weight_matrix(3,4)=1.0
!    weight_matrix(4,1)=1.0
   do l =1,num-1
      weight_matrix(l,l+1)=weight_minus
           weight_matrix(1,l+1)=weight_plus
 !     weight_matrix(l+1,l)= weight_minus
    end do
!    weight_matrix(num,2)=weight_plus
    weight_matrix(2,num)=weight_minus
  do l =1,num
    call init_random_seed()
    !assigning a value between -75 and -53 V(0) for all neurons
    call    random_number(random1)
    neural_network(l,1)= 15.0- 9.0*random1
 end do
 neural_network(1,1)= 15.0
    do l=1,num
       call init_random_seed()
       call  random_number(random1)
       call random_number(random2)
       random1=0.015*random1
       random2=random2*0.015
       random2=0.0
       neural_network(l,2)= random1+random2
       neural_network(l,3)=(random1*E_plus+random2*E_minus)/(random1+random2)
    end do
    neural_network(1,2)=1.2
    neural_network(1,3)=74.0
   ! do l=2,num
  !  neural_network(l,3)=74.0
 !end do
    open (1,file="firing_table.dat")
    open (2,file="neurons_firing.dat")
    open (3,file="shitfile.dat")
    call firing_time_updater(neural_network,firing_table,dummy_zero,diff_central)
    !      write(*,*) "testing is",testing
    open(4,file="initialfiring.dat")
    open(5,file="update_table.dat")
    !testing=0.0
    testing_argument(1)=15.0
    testing_argument(2)=1.2
    testing_argument(3)=74.0
    testing_argument(4)= 0.15488245509844328
    write(4,*) voltage_function(testing_argument)
 !   do l=1,100
 !      testing_argument(4)= testing
       !random2=diff_central(voltage_function,testing_argument,4,h)
 !      random1= voltage_function(testing_argument)
 !      write(3,*) testing,random1
 !      !write(*,*) testing,random2
 !      testing= testing + 0.01
 !   end do
 !   close(3)
    firing_table(1)=oscillator_period
    update_time=10000.0
    next_fire_time=minval(firing_table)
    update_time=minval(update_time_table)
    do while(min(next_fire_time,update_time)<time_bound)
       if(update_time<next_fire_time) then
          write(3,*) "update  time passed"
          do d=1,num
             if(update_time_table(d)==update_time) then
                call incoming_updater(neural_network,d,update_time,weight_matrix)
                update_time_table(d)=100000.0
             end if
          end do
          call firing_time_updater(neural_network,firing_table,update_time,diff_central)
          write(4,*) firing_table
       else
          write(3,*) "firing time passed"
          do l =1,num
             if(firing_table(l)==next_fire_time) then
                ! dummy_time=next_fire_time
                ! next_fire_time=next_fire_time+adjuster
                ! firing_table(l)=next_fire_time
                write(3,*) "passed with time",next_fire_time
                
                if(l==1) then
                   firing_table(1)=firing_table(1) + oscillator_period
                   if(update_time_table(l)>next_fire_time+time_lag) then 
                      update_time_table(l)=next_fire_time+time_lag
                      write(5,*) update_time_table
                   end if
                   write(1,*) 20*next_fire_time,l,0.5
                else
                   write(1,*) 20*next_fire_time,l,0.5
                   if(update_time_table(l)>next_fire_time+time_lag) then 
                      update_time_table(l)=next_fire_time+time_lag
                      write(5,*) update_time_table
                   end if
                   call  outgoing_updater(neural_network,l,next_fire_time)
                end if
                
             end if
          end do
          call firing_time_updater(neural_network,firing_table,next_fire_time,diff_central)
          write(4,*) firing_table
       end if
       update_time=minval(update_time_table)
       next_fire_time= minval(firing_table)
    end do
    

    close(1)

    close(2)

    call execute_command_line('gnuplot "gnucommand"')
    
  contains
  !------------------------------  ----------    ----------
  !gives the value of g afer giving the initial g value and time
  function g_function(g_0,time)
    implicit none
    real(8):: g_0,time,g_function
    g_function = g_0* exp(-time/tau_s)
  end function g_function

!------------    ---------------------------    ----------------
  !gives the exact solution for voltage given the time and initial conditions
  function  voltage_function(arg_array)
    !arg_array = (V_0,g_0,E_s,time)
    implicit none
    !gamma_1,gamma_2 refers to different version of gamma integral in equation
    !dummy_1 and dummy_2 refers to dummy var for the subroutine incog
    real(8) ::arg_array(4), voltage_function,g_t,gamma_1,gamma_2,dummy_1,dummy_2,a,b,c
    g_t = g_function(arg_array(2),arg_array(4))
    a = 1-tau_s
    b= tau_s*g_t
    c= tau_s*arg_array(2)
    call incog(a,b,gamma_1,dummy_1,dummy_2)
    call incog(a,c,gamma_2,dummy_1,dummy_2)
    gamma_1 = gamma_1*exp(b)*(b**(-a))
    gamma_2 = gamma_2*exp(c)*(c**(-a))
    voltage_function = (-tau_s*arg_array(3)*g_t*gamma_1) + exp(-arg_array(4)+ tau_s&
         *(g_t-arg_array(2)))*(arg_array(1)+tau_s*arg_array(3)*arg_array(2)*gamma_2)

  end function voltage_function
  !----------- ------------------- ----------------- -----------------
  !special voltage function in which one of the gamma integral is given as an argument
  function  voltage_function_special(arg_array)
   ! arg_array=(V_0,g_0,E_s,gamma,time)
    implicit none
    !gamma_1,gamma_2 refers to different version of gamma integral in equation
    !dummy_1 and dummy_2 refers to dummy var for the subroutine incog
    real(8) ::arg_array(5), voltage_function_special,V_0,g_0,E_s,time,g_t
    real(8) :: gamma,gamma_1,gamma_2,dummy_1,dummy_2,a,b,c
    g_t = g_function(arg_array(2),arg_array(5))
    a = 1-tau_s
    b= tau_s*g_t
  !  c= tau_s*g_0
    call incog(a,b,gamma_1,dummy_1,dummy_2)
   ! call incog(a,c,gamma_2,dummy_1,dummy_2)
    gamma_1 = gamma_1*exp(b)*(b**(-a))
    gamma_2=arg_array(4)
    !gamma_2 = gamma_2*exp(c)*(c**(-a))
    voltage_function_special = (-tau_s*arg_array(3)*g_t*gamma_1) + exp(-arg_array(5)+ tau_s&
         *(g_t-arg_array(2)))*(arg_array(1)+tau_s*arg_array(3)*arg_array(2)*gamma_2)

  end function voltage_function_special

  !------- --------------- ------------ ---------------- -------
  !subroutine to update the state of a neuron after it is fired.
  subroutine outgoing_updater(Neural_network,i,fire_time)
    implicit none
    !i refers to the index of the neuron which is going to be fired
    integer :: i
    real(8) ::    Neural_network(num,4), fire_time
    neural_network(i,1) = V_reset
    neural_network(i,2)=neural_network(i,2) *&
         exp((neural_network(i,4)-fire_time)/tau_s)
    neural_network(i,4)= fire_time
  end subroutine outgoing_updater
  !------------------- ----------------- --------------------------


 !subroutine to update other neurons after one neuron fires.
  subroutine incoming_updater(neural_network,i,fire_time,weight_matrix)
    implicit none
  integer:: j,i
  real(8) ::    Neural_network(num,4), fire_time,w_dummy,alpha,beta,weight_matrix(num,num)
  real(8):: arg_array(4)
  alpha= (E_plus - E_minus)/2.0
  beta=  (E_plus + E_minus)/2.0
  do j = 1,num
     if (j .ne. i) then
        arg_array(1:3)=neural_network(j,1:3)
        arg_array(4)=fire_time-neural_network(j,4)
        neural_network(j,1) =voltage_function(arg_array)
        neural_network(j,2)=neural_network(j,2) *&
             exp((neural_network(j,4)-fire_time)/tau_s)
        w_dummy = weight_matrix(i,j)
        neural_network(j,3) = (neural_network(j,2)*neural_network(j,3) + alpha*w_dummy + beta*abs(w_dummy))/&
             (neural_network(j,2)+abs(w_dummy))
        neural_network(j,2)= neural_network(j,2) + abs(w_dummy)
        neural_network(j,4)= fire_time

     end if
  end do
end subroutine incoming_updater
!-------------- -------------- -------------- ----------------


!to update the spike firing time table
subroutine firing_time_updater(neural_network,firing_table,firing_time,diff_function)
  implicit none
  real(8),external:: diff_function
  !real(8),external::voltage_function
  !real(8) , external :: voltage_function_special
  real(8):: neural_network(num,4),firing_table(num),firing_time
  real(8)::dummy_voltage,dummy_time,guess, error,h,dummy_1,dummy_2,diff,arg_array(5),g_star
  real(8):: dummy_a,dummy_b,dummy_c,dummy_gamma_1,dummy_gamma_2,dummer_1,dummer_2,arg_array_2(4)
  real(8):: ar_arr(4),random_dummy
  integer::k,q

  do k=2,num
     firing_table(k)=1000000.0
   !  write(*,*) "neuron is",k
     if(neural_network(k,3)>V_th) then
   !     write(*,*) "reached first if"
        g_star= V_th/(neural_network(k,3)-v_th)
  !      write(*,*) "g_star is", g_star,neural_network(k,2)
        if(neural_network(k,2)>g_star) then
 !           write(*,*) "reached second if"
           dummy_a = 1-tau_s
           dummy_b= tau_s*g_star
           dummy_c= tau_s*neural_network(k,2)
           call incog(dummy_a,dummy_b,dummy_gamma_1,dummer_1,dummer_2)
           call incog(dummy_a,dummy_c,dummy_gamma_2,dummer_1,dummer_2)
    
           dummy_gamma_1 = dummy_gamma_1*exp(dummy_b)*(dummy_b**(-dummy_a))
           dummy_gamma_2 =dummy_gamma_2*exp(dummy_c)*(dummy_c**(-dummy_a))
           dummy_voltage = (-tau_s*neural_network(k,3)*g_star*dummy_gamma_1) &
                +((g_star/neural_network(k,2))**tau_s)* exp( tau_s&
                *(g_star-neural_network(k,2)))*(neural_network(k,1)&
                +tau_s*neural_network(k,3)*neural_network(k,2)*dummy_gamma_2)

    !                  write(*,*) dummy_voltage
           if(dummy_voltage>V_th) then
     !         write(*,*) "reached third if"
              
              guess = 0.0
              arg_array_2(1:3) = neural_network(k,1:3)
              arg_array_2(4)=guess
              error=voltage_function(arg_array_2)-V_th
              q=0
              do while(abs(error)>0.001)
                 if (q==20) then
                    write(*,*) "oop!! looped a lot"
                    stop
                 end if
                 q= q+1
          !       write(*,*) "guess is ", guess
           !      write(*,*) "looping"
                 arg_array(1:3)=neural_network(k,1:3)
                 arg_array(4)=dummy_gamma_2
                 arg_array(5)=guess
                 h=0.0000001
                 diff= diff_function(voltage_function_special,arg_array,5,h)
       !          write(*,*) "diff is ", diff
        !         write(*,*) "error is ",error
                 random_dummy= error/diff
                 if(random_dummy>0) then
                    if(neural_network(k,1)>V_th) then 
                    open(30,file="error_finder.dat")
                    write(30,*) k, neural_network(k,:)
                   
          !        write(*,*) "curve error"
                    guess= adjusting_constant
                    exit
                 else
                    guess=100000.0
                    exit
                 end if
              end if
                 guess = guess- error/diff
                 arg_array(5)= guess
                 error= voltage_function_special(arg_array)-V_th
              end do
              firing_table(k)= neural_network(k,4) + guess
           end if
        end if
     end if
  end do
end subroutine firing_time_updater
!---------- --------------- ------------------------ ---------------- --------
     



  
end program neural_vibrator
