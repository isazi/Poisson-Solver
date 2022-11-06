module gauss_seidel

  use omp_lib
  use precisn, only: wp
  use config, only: nr, nc, max_iter, debug, dx
  use color, only: color_group, init_color_groups
  use grid, only: init_grid, get_xy_pos, func

  implicit none

contains

  subroutine gs_method(r, c, u_grid, u)
    implicit none
    integer, intent(in)        :: r, c
    real(kind=wp), intent(in)  :: u_grid(:,:)
    real(kind=wp), intent(out) :: u

    real(kind=wp) :: x, y
    real(kind=wp) :: b

    call get_xy_pos(r, c, x, y)
    b = func(x, y)
    u = -(1._wp/4._wp) * (b * dx * dx - u_grid(r-1, c) - u_grid(r+1, c) - u_grid(r, c-1) - u_grid(r, c+1))

  end subroutine gs_method

  subroutine solve(u_grid, max_diff, n_iter)

    real(kind=wp), intent(inout) :: u_grid(nr, nc)
    real(kind=wp), intent(out)   :: max_diff       ! max difference between matrix elements
    integer, intent(out)         :: n_iter         ! final number of iterations

    ! local vars
    type(color_group) :: red, blu
    real(kind=wp)     :: u_grid_old(nr, nc), u_next
    integer           :: r, c
    integer           :: i

    call init_grid(u_grid)
    call init_color_groups(red, blu)

    do n_iter = 1, max_iter

      u_grid_old = u_grid

      !$acc parallel loop
      do i = 1, red%num
        r = red%rows(i)
        c = red%cols(i)
        call gs_method(r, c, u_grid, u_next)
        u_grid(r, c) = u_next
      end do
      !$acc end parallel loop

      !$acc parallel loop
      do i = 1, blu%num
        r = blu%rows(i)
        c = blu%cols(i)
        call gs_method(r, c, u_grid, u_next)
        u_grid(r, c) = u_next
      end do
      !$acc end parallel loop

      max_diff = maxval(abs(u_grid - u_grid_old))
      if (max_diff < 1e-11) exit

    end do

  end subroutine solve

end module gauss_seidel
