module test
    use, intrinsic :: iso_c_binding
contains
    subroutine t_times2(v_in, v_out) bind(c, name='t_times2')
        integer, intent(in) :: v_in
        integer, intent(out) :: v_out
        !
        v_out=v_in*2
    end subroutine t_times2
    !
    subroutine t_square(v_in, v_out) bind(c, name='t_square')
        integer(c_int), intent(in) :: v_in
        integer(c_int), intent(out) :: v_out
        !
        v_out=v_in**2
    end subroutine t_square

    subroutine j_update(j_in) bind(c,name='j_update')
            real(c_double), intent(inout) :: j_in

            j_in = 0.5
    end subroutine j_update

end module test
