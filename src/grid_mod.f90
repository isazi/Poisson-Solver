module grid

  use precisn, only: wp
  use config, only: nr, nc, alpha, x_0, y_0, dx, dy
  implicit none

  private
  public :: init_grid, get_xy_pos, func

contains
  subroutine get_xy_pos(r, c, x, y)
    implicit none
    integer, intent(in) :: r, c
    real(kind=wp), intent(out)  :: x, y

    x = x_0 + (c - 1) * dx
    y = y_0 + (r - 1) * dy

  end subroutine get_xy_pos

  subroutine init_grid(u_grid)
    implicit none
    real(kind=wp), intent(inout)  :: u_grid(:,:)

    u_grid = 0._wp

    u_grid(1, :) = alpha
    u_grid(:, 1) = alpha
    u_grid(nr, :) = alpha
    u_grid(:, nc) = alpha

  end subroutine init_grid

  function func(x, y) result(f)
    implicit none
    real(kind=wp), intent(in) :: x, y
    real(kind=wp) :: f

    ! currently f is constant with no dependence on x and y
    f = 0._wp*(x*y) + 2._wp

  end function func

end module grid
