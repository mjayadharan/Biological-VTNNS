module integrating_module
 ! use functions
  implicit none


contains
  subroutine euler(fun,h,initial_p,final_p,initial_v,final_v)
    real::h,initial_p,final_p,initial_v,final_v,initial_point
    integer::i,j,k,l,n
    interface
       function fun(x,y)
         real:: fun,x,y
       end function fun

    end interface
    n=int(-(initial_p - final_p)/h)
    !  write(*,*)
    initial_point=initial_p
    final_v=initial_v
    do i=1,n
!       write(*,*) fun(initial_p,final_v)
       final_v=final_v+ h* fun(initial_point,final_v)
       initial_point=initial_point+h
!       write(*,*) initial_point

    end do
  end subroutine euler
  !
  !
  !
  !
  subroutine euler_four(fun,h,initial_p,final_p,initial_v,final_v,a_n,b_n)
    !specially made for hh neruon model, function take four variables
    real::h,initial_p,final_p,initial_v,final_v,initial_point,a_n,b_n
    integer::i,j,k,l,n
    interface
       function fun(x,y,a_n,b_n)
         real:: fun,x,y,a_n,b_n
       end function fun

    end interface
    n=int(-(initial_p - final_p)/h)
    !  write(*,*)w
    initial_point=initial_p
    final_v=initial_v
    do i=1,n
!       write(*,*) fun(initial_p,final_v)
       final_v=final_v+ h* fun(initial_point,final_v,a_n,b_n)
       initial_point=initial_point+h
       !write(*,*) initial_point,final_v

    end do
  end subroutine euler_four
  !
  !
  !
    subroutine euler_six(fun,h,initial_p,final_p,initial_v,final_v,I_m,n1,m,h1)
    !specially made for hh neruon model, function take four variables
    real::h,initial_p,final_p,initial_v,final_v,initial_point,I_m,n1,m,h1
    integer::i,j,k,l,n
    interface
       function fun(x,y,I_m,n1,m,h1)
         real:: fun,x,y,I_m,n1,m,h1
       end function fun

    end interface
    n=int(-(initial_p - final_p)/h)
    !  write(*,*)w
    initial_point=initial_p
    final_v=initial_v
    do i=1,n
!       write(*,*) fun(initial_p,final_v)
       final_v=final_v+ h* fun(initial_point,final_v,I_m,n1,m,h1)
       initial_point=initial_point+h
       !write(*,*) initial_point,final_v

    end do
  end subroutine euler_six
  

end module integrating_module
