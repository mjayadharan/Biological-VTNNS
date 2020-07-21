program graph_test
  use fortran_gnuplot
  implicit none
  integer::i
  real(8)::x
  real(8), dimension(100,2):: curve1,curve2
  do i=0,100
     curve1(i,1)=i+0.0
     curve1(i,2)=i**2+0.0
     curve2(i,1)=i+0.0
     curve2(i,2)=i**3+0.0
  end do
  call f2gp(i,i,curve1,curve2,1,'x','y','one','two')

end program graph_test
