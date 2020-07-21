module fortran_gnuplot
implicit none
contains
subroutine f2gp(n1,n2,xydata1,xydata2,plot_type,xlabel,ylabel,title1,title2,dummy1)
integer :: n1,n2                                     ! number of data points
real(8) :: xydata1(:,:)                             ! first data array
real(8) :: xydata2(:,:)                             ! second data array
integer :: plot_type,dummy1                                 ! 1 for linear plot, 2 for log plot, 3 for log-log plot
character(len=*) :: xlabel,ylabel,title1,title2      ! plot axis labels and title
!---------------
integer :: i
integer :: ret
!---------------
 
! write data on two separate files
OPEN(10,ACCESS='SEQUENTIAL',FILE='xydata1.dat')
do i=1,n1
   write(10,*) xydata1(i,1),xydata1(i,2)
enddo
CLOSE(10,STATUS='KEEP')
 
OPEN(10,ACCESS='SEQUENTIAL',FILE='xydata2.dat')
do i=1,n2
   write(10,*) xydata2(i,1),xydata2(i,2)
enddo
CLOSE(10,STATUS='KEEP')
 
! create gnuplot command file
OPEN(10,ACCESS='SEQUENTIAL',FILE='gp.txt')
write(10,*) 'set terminal postscript'
write(10,112) 'set output "',22,'.jpg"'
  112 format(A12,i2,A4)
write(10,*) 'set xlabel '//'"'//TRIM(xlabel)//'"'
write(10,*) 'set ylabel '//'"'//TRIM(ylabel)//'"'
if (plot_type==2) write(10,*) 'set log y'
if (plot_type==3) then
   write(10,*) 'set log x'
   write(10,*) 'set log y'
endif
 
if(n1>0.AND.n2>0) then
    write(10,*) 'plot "xydata1.dat" using 1:2 with lines title "'//TRIM(title1)//&
                &'", "xydata2.dat" using 1:2 with lines title "'//TRIM(title2)//'"'
endif
 
if(n1>0.AND.n2==0) write(10,*) 'plot "xydata1.dat" using 1:2 with lines title "'//TRIM(title1)//'"'
 
if(n2>0.AND.n1==0) write(10,*) 'plot "xydata2.dat" using 1:2 with lines title "'//TRIM(title2)//'"'
 
CLOSE(10,STATUS='KEEP')
 
! plot curve with gnuplot and cleanup files
ret=SYSTEM('gnuplot gp.txt')
ret=SYSTEM('rm gp.txt')
ret=SYSTEM('rm xydata1.dat')
ret=SYSTEM('rm xydata2.dat')
 
end subroutine
 
end module
