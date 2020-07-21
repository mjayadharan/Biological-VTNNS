program tester
!use numerical
  implicit none
  real(8) :: A,X(2),GN,GP
  integer::i,j,gm(1)
  a=1.0
  x(1)=2.0
  x(2)=1.0
do i=1,10
write(*,*) i
if(i==5) then
stop
end if
end	do 
write(*,*) "didnt work"
contains
  function funct(x)
    ! integer::n
    real(8) ::funct,x,y
    funct= x**2+x**3

  end function funct
  
  function   funct2(x)
    real(8):: funct2,x(2)
    funct2 =  x(1)**2*x(2)**3
  end function funct2

  end program tester
        
