module plotting_module
  implicit none


contains

  subroutine  graph_video(x_initial,x_final)
    !will generate a video of the graph into the folder name
    !file name should be datafile.dat
  real:: x_initial,x_final,dummy1,step_size
  integer:: i,j,k,l,frame_rate
  step_size= (x_final - x_initial)/100
  dummy1=x_initial+step_size
  call execute_command_line('mkdir "animation"')
  !write(*,*) step_size
  do i=1,100
     open(12,file='gnucommand')
     write(12,*) 'set terminal png'
     if (i<10) then
        write(12,112) 'set output "animation/',0,0,i,'.png"'
112     format(A22,i1,i1,i1,A5)
     else if(i<100 .and. i>=10) then
        write(12,113) 'set output "animation/',0,i,'.png"'
113     format(A22,i1,i2,A5)
     else
        write(12,114) 'set output "animation/',i,'.png"'
114     format(A22,i3,A5)
     end if
     write(12,*) 'plot[',x_initial,':',dummy1,'] "datafile.dat" w lp'
!120  format(A5,F10.10,A1,F10.10,A21)
     write(12,*) 'replot'
     write(12,*) 'set output'
     write(12,*) 'exit'
     close(12)
     write(*,*) dummy1
     call execute_command_line('gnuplot "gnucommand"')
     dummy1=dummy1+step_size
  end do
  call execute_command_line('ffmpeg -framerate 10 -i animation/%03d.png -c:v libx264 -r 30 -pix_fmt yuv420p out.mp4')
  call execute_command_line('rm -f "gnucommand"')
end subroutine graph_video

end module plotting_module
     
  
