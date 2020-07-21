module plotting_module
  implicit none


contains

  subroutine  graph_video(x_initial,x_final)
    !will generate a video of the graph into the folder name
    !file name should be datafile.dat
  real:: x_initial,x_final,dummy1,step_size,x_left,x_right
  integer:: i,j,k,l,frame_rate
  step_size= (x_final - x_initial)/100
  x_left = x_initial
  x_right=10.0
  call execute_command_line('mkdir "animation1"')
  !write(*,*) step_size
  do i=1,100
     open(12,file='gnucommand')
     write(12,*) 'set terminal png'
     write(12,*) 'set grid'
     if (i<10) then
        write(12,112) 'set output "animation1/',0,0,i,'.png"'
112     format(A23,i1,i1,i1,A5)
     else if(i<100 .and. i>=10) then
        write(12,113) 'set output "animation1/',0,i,'.png"'
113     format(A23,i1,i2,A5)
     else
        write(12,114) 'set output "animation1/',i,'.png"'
114     format(A23,i3,A5)
     end if
     write(12,*) 'set ytics 1'
     write(12,*) 'set ylabel "voltage"'
     write(12,*) 'set xlabel "time"'
     write(12,*) 'set xrange [',x_left,':',x_right,']'
     write(12,*) 'set yrange [-0.5:4.0]'
     write(12,*) 'plot  "datapoints.dat" using 1:2 w l title "v_1" , "" using 1:3 w l title "v_2" &
          , "" using 1:4 w l title "I_applied"'
     !120  format(A5,F10.10,A1,F10.10,A21)
   
     write(12,*) 'replot'
     write(12,*) 'set output'
     write(12,*) 'exit'
     close(12)
    ! write(*,*) dummy1
     call execute_command_line('gnuplot "gnucommand"')
     x_left = x_left + step_size
     x_right = x_right + step_size
  end do
  call execute_command_line('ffmpeg -framerate 4 -i animation1/%03d.png -c:v libx264 -r 30 -pix_fmt yuv420p out.mp4')
 ! call execute_command_line('rm -f "gnucommand"')
end subroutine graph_video

end module plotting_module
     
  
