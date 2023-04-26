! Distributed under MIT License
!
! Copyright (c) 2022 Tom Meltzer
!
! Please see LICENSE file for more information

module grid

  use precisn, only: wp
  use config, only: nr, nc, alpha, x_0, y_0, dx, dy
  implicit none

  private
  public :: init_grid, get_xy_pos, func

contains
  subroutine get_xy_pos(r, c, x, y)
    !$acc routine seq
    ! Get the x and y position in real-space for a given row and column (r, c)
    ! inputs:
    ! - r : int : row index
    ! - c : int : column index
    ! outputs:
    ! - x : int : x position
    ! - y : int : y position
    implicit none
    integer, intent(in) :: r, c
    real(kind=wp), intent(out)  :: x, y

    x = x_0 + (c - 1) * dx
    y = y_0 + (r - 1) * dy

  end subroutine get_xy_pos

  subroutine init_grid(u_grid)
    ! u_grid (the solution) is initially unknown and we want to find it iteratively using the Gauss-seidel method or directly using
    ! LAPACK solvers
    ! This routine sets the boundary condition and initializes the interior elements to zero.
    ! outputs:
    ! - u_grid : real(kind=wp)(:,:) : solution grid
    implicit none
    real(kind=wp), intent(out)  :: u_grid(:,:)

    u_grid = 0._wp

    u_grid(1, :) = alpha
    u_grid(:, 1) = alpha
    u_grid(nr, :) = alpha
    u_grid(:, nc) = alpha

  end subroutine init_grid

  function func(x, y) result(f)
    !$acc routine seq
    ! This function is the RHS of Laplacian D_xy U(x,y) = f(x,y)
    ! inputs:
    ! - x : real(kind=wp) : x position in real-space
    ! - y : real(kind=wp) : y position in real-space
    ! returns:
    ! - f : real(kind=wp) : value of function func at position (x,y)
    implicit none
    real(kind=wp), intent(in) :: x, y
    real(kind=wp) :: f

    ! currently f is constant with no dependence on x and y
    f = 0._wp*(x*y) + 2._wp

  end function func

end module grid
