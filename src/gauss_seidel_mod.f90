! Distributed under MIT License
!
! Copyright (c) 2022 Tom Meltzer
!
! Please see LICENSE file for more information

module gauss_seidel

  use omp_lib
  use precisn, only: wp
  use config, only: nr, nc, max_iter, debug, dx, dy
  use color, only: color_group, init_color_groups
  use grid, only: init_grid, get_xy_pos, func

  implicit none

contains

  subroutine gs_method(r, c, u_grid, u)
    ! Gauss-Seidel method to solve for u at given row and column (r, c)
    ! inputs:
    ! - r      : int                : row index
    ! - c      : int                : column index
    ! - u_grid : real(kind=wp)(:,:) : current u_grid
    ! outputs:
    ! - u      : real(kind=wp)      : solution at location (r, c)
    implicit none
    integer, intent(in)        :: r, c
    real(kind=wp), intent(in)  :: u_grid(:,:)
    real(kind=wp), intent(out) :: u

    real(kind=wp) :: x, y
    real(kind=wp) :: b

    call get_xy_pos(r, c, x, y)
    b = func(x, y)
    u = -(1._wp/4._wp) * (b * dx * dy - u_grid(r-1, c) - u_grid(r+1, c) - u_grid(r, c-1) - u_grid(r, c+1))

  end subroutine gs_method

  subroutine solve(u_grid, max_diff, n_iter)
    ! Parallelized solver calling iterative gs_method for each grid point. This method will converge when max_diff satisfies the
    ! exit criteria or when the number of iterations exceeds max_iter.
    ! outputs:
    ! - u_grid   : real(kind=wp)(:,:) : converged u_grid
    ! - max_diff : real(kind=wp)      : maximum difference in u_grid from previous iteration
    ! - n_iter   : int                : number of iterations performed

    real(kind=wp), intent(out) :: u_grid(nr, nc)
    real(kind=wp), intent(out)   :: max_diff       ! max difference between matrix elements
    integer, intent(out)         :: n_iter         ! final number of iterations

    type(color_group) :: red, blu
    real(kind=wp)     :: u_grid_old(nr, nc), u_next
    integer           :: r, c
    integer           :: i, j

    call init_grid(u_grid)
    call init_color_groups(red, blu)

    !$acc data copyin(red, red%rows, red%cols, &
    !$acc             blu, blu%rows, blu%cols, &
    !$acc             u_grid) &
    !$acc      create(u_next, u_grid_old)

    do n_iter = 1, max_iter

      !$omp parallel do private(j) collapse(2)
      !$acc parallel loop gang collapse(2)
      do j = 1, nc
        do i = 1, nr
          u_grid_old(i, j) = u_grid(i, j)
        end do
      end do
      !$omp end parallel do
      !$acc end parallel loop

      !$omp parallel do private(i, r, c, u_next)
      !$acc parallel loop private(r, c, u_next)
      do i = 1, red%num
        r = red%rows(i)
        c = red%cols(i)
        call gs_method(r, c, u_grid, u_next)
        u_grid(r, c) = u_next
      end do
      !$omp end parallel do
      !$acc end parallel loop

      !$omp parallel do private(i, r, c, u_next)
      !$acc parallel loop private(r, c, u_next)
      do i = 1, blu%num
        r = blu%rows(i)
        c = blu%cols(i)
        call gs_method(r, c, u_grid, u_next)
        u_grid(r, c) = u_next
      end do
      !$omp end parallel do
      !$acc end parallel loop

      max_diff = 0._wp
      !$omp parallel do private(j) collapse(2) reduction(max:max_diff)
      !$acc parallel loop collapse(2) reduction(max:max_diff)
      do j = 1, nc
        do i = 1, nr
          max_diff = max(max_diff, abs(u_grid_old(i, j) - u_grid(i, j)))
        end do
      end do
      !$omp end parallel do
      !$acc end parallel loop

      if (max_diff < 1e-11) exit

    end do
    !$acc update self(u_grid)
    !$acc end data

  end subroutine solve

end module gauss_seidel
