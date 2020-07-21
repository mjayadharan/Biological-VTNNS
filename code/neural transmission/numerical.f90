module numerical
  !has some most commonly used numerical methods for differentiation and root finidng
  implicit none
  integer , parameter:: dp= kind(1.d0)



contains
  !for doing central numerical diff
  function diff_central(func,ar_of_arg,no,h)
    !func-funciton ,ar_of_arg- array of argument with last value being the variable wrt diff
    ! no- no.of arguments , h-nbd radius
    real(8):: diff_central,h
    integer :: no
    real(8) :: ar_of_arg(no),dummy_array0(no), dummy_array1(no),dummy_array2(no)
    real(8), external:: func
    dummy_array0 = 0.0
    dummy_array0(no)= h
    dummy_array1 = ar_of_arg + dummy_array0
    dummy_array2 = ar_of_arg - dummy_array0
    diff_central =0.5*(func(dummy_array1)- func(dummy_array2))/h
    !.d0*func(dummy_array2)-8.d0*func(dummy_array3)+ func(dummy_array4))
         
         
    

  end function diff_central


end module numerical
