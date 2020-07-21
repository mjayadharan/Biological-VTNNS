program shit
  use plotting_module
implicit none
integer::i,j,k,l
real(8):: r,s,t,u

open(1,file="datafile.dat" )
do i=1,120
   write(1,*) i,i**2
end do
close(1)
r=1.0
s=100.0
call graph_video(1.0,100.0)

end program shit
