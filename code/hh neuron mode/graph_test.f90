program graph_tester
  use fortran_gnuplot
  implicit none

  integer::i
real(8) , dimension(100,2):: curve1,curve2
do i=0,100
   curve1(i,1)=i
   curve1(i,2)=i**2
   curve2(i,1)=i
   curve2(i,2)=i**2
end do

call f2gp(i,i,curve1,curve2,1,'x','y','..','..',1)

end program graph_tester
     
