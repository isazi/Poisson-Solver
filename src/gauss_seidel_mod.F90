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
    !$omp declare target

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
    integer, allocatable :: red_rows(:), red_cols(:)
    integer, allocatable :: blu_rows(:), blu_cols(:)
    logical :: is_GPU

    is_GPU = .false.
#ifdef USEGPU
    is_GPU = .true.
#endif


    call init_grid(u_grid)
    call init_color_groups(red, blu)

    allocate(red_rows(red%num), red_cols(red%num))
    allocate(blu_rows(blu%num), blu_cols(blu%num))

    red_rows = red%rows
    red_cols = red%cols
    blu_rows = blu%rows
    blu_cols = blu%cols

    if (is_GPU) then
      print *, 'Running on GPU...'
    else
      print *, 'Running on CPU...'
    end if

    ! openMP 4.5 doesn't appear to support copying derived types
    !$omp target data if(is_GPU) map(to:red_rows, red_cols, blu_rows, blu_cols, u_grid_old) &
    !$omp             map(tofrom:u_grid)

    ! I have changed the openacc version to use the plain arrays instead of
    ! derived types to make the two version more similar
    !$acc data copyin(red_rows, red_cols, blu_rows, blu_cols, u_grid) &
    !$acc      create(u_grid_old)

    do n_iter = 1, max_iter

      !$omp target teams if(is_GPU)
      !$omp distribute parallel do simd collapse(2)
      !$acc parallel loop gang collapse(2)
      do j = 1, nc
        do i = 1, nr
          u_grid_old(i, j) = u_grid(i, j)
        end do
      end do
      !$omp end target teams
      !$acc end parallel loop

      !$omp target teams if(is_GPU)
      !$omp distribute parallel do simd private(r, c, u_next)
      !$acc parallel loop private(r, c, u_next)
      do i = 1, red%num
        r = red_rows(i)
        c = red_cols(i)
        call gs_method(r, c, u_grid, u_next)
        u_grid(r, c) = u_next
      end do
      !$omp end target teams
      !$acc end parallel loop

      !$omp target teams if(is_GPU)
      !$omp distribute parallel do simd private(r, c, u_next)
      !$acc parallel loop private(r, c, u_next)
      do i = 1, blu%num
        r = blu_rows(i)
        c = blu_cols(i)
        call gs_method(r, c, u_grid, u_next)
        u_grid(r, c) = u_next
      end do
      !$omp end target teams
      !$acc end parallel loop

      max_diff = 0._wp
#ifdef USEGPU
      !$omp target teams if(is_GPU)
      !$omp distribute parallel do simd collapse(2) reduction(max:max_diff)
#else
      !$omp parallel do simd collapse(2) reduction(max:max_diff)
#endif
      !$acc parallel loop collapse(2) reduction(max:max_diff)
      do j = 1, nc
        do i = 1, nr
          max_diff = max(max_diff, abs(u_grid_old(i, j) - u_grid(i, j)))
        end do
      end do
#ifdef USEGPU
      !$omp end target teams
#endif
      !$acc end parallel loop

      if (max_diff < 1e-11) exit

    end do
    !$acc update self(u_grid)
    !$acc end data
    !$omp end target data

  end subroutine solve

end module gauss_seidel
