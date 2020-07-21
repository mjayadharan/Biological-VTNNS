program tester
  !use functions
  !use integrating_module
  implicit none
  real::h,initial_p,final_p,initial_v,final_va
  integer::i
  ! call euler(squared,.00025,0.0,1.0,0.0,final_va)
  !  call write_xy_data("file1.dat")
  open(10,file='datapoints.dat')
  do i=1,100
     write(10,11) i,i**2
11   format(i3,i5)
  end do
  close(10)
  do i=1,100
     open(12,file='gnucommand')
     write(12,*) 'set terminal png'
  write(12,112) 'set output "',i,'.ps"'
112 format(A12,i3,A4)
  write(12,*) 'plot[1:i] "file1.dat" with lines'
  write(12,*) 'replot'
  write(12,*) 'set output'
  write(12,*) 'exit'
  close(12)
  call execute_command_line('gnuplot "gnucommand"')

end do
!contains
 ! function fun(a)
  !  real:: fun,a
   ! fun=3*a
 ! end function fun
 ! subroutine test(funct,x)
  !  real::x
   ! interface
    !   function funct(s)
     !    real:: funct,s
      ! end function funct
    !end interface
  ! y= funct(x)
  !write(*,*) final_va
!end subroutine test
end program tester
