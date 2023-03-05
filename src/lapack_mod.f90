module lapack

  ! This module uses LAPACK routine dgesv to solve A(N, N) x(N) = b(N)
  use omp_lib
  use precisn, only: wp
  use config, only: nr, nc, max_iter, debug, dx, dy
  use grid, only: init_grid, get_xy_pos, init_grid, func

  implicit none

  external :: dgesv
  integer :: N

contains

  subroutine init_b(u_grid, b)
    ! Initialize the vector b used in dgesv routine
    ! inputs:
    ! - u_grid : real(kind=wp)(:,:) : 2D spatial grid
    ! outputs:
    ! - b      : real(kind=wp)(:)   : solution vector
    implicit none
    real(kind=wp), intent(in) :: u_grid(nr, nc)
    real(kind=wp), intent(out), allocatable :: b(:)

    real(kind=wp) :: x, y
    integer :: i, r, c

    allocate(b(N))

    b = 0._wp

    associate( width => nr-2 )
      do i = 1, N
        r = (i-1) / width + 2
        c = mod(i-1, width) + 2
        call get_xy_pos(r, c, x, y)
        b(i) = func(x, y) * dx * dy - u_grid(r-1, c) - u_grid(r+1, c) - u_grid(r, c-1) - u_grid(r, c+1)
      end do
    end associate

  end subroutine init_b

  subroutine init_A(A)
    ! Initialize the maxtrix A used in dgesv routine
    ! outputs:
    ! - A : real(kind=wp)(:,:) : coefficient matrix
    implicit none
    real(kind=wp), intent(out), allocatable :: A(:,:)
    character(len=20) :: fmt_str

    integer :: i, r, c

    allocate(A(N,N))
    ! elements of A matrix are either 0.0, 1.0 or -4.0
    ! diagonal elements are always equal to -4.0
    ! off-diagonal elements can be 1.0 or 0.0 depending on stencil used
    ! currently only a 5-point stencil is setup so there will be a maximum of 4 non-zero off-diagonals in any row/column
    A = 0._wp
    associate( width => nr-2 )
      do i = 1, N
        r = (i-1) / width + 1
        c = mod(i-1, width) + 1
        A(i, i) = -4._wp ! diagonal elements are always -4.0 for a 5-point stencil
        if (c > 1) then
          A(i, (r-1)*width + c-1) = 1._wp
        end if
        if (c < width) then
          A(i, (r-1)*width + c+1) = 1._wp
        end if
        if (r > 1) then
          A(i, (r-2)*width + c) = 1._wp
        end if
        if (r < width) then
          A(i, (r)*width + c) = 1._wp
        end if
      end do
    end associate

    if (debug) then
      fmt_str = ''
      write (fmt_str, '(a,i0,a)') '(', N ,'(f5.2, 1x))'
      print *, 'A(N, N) = '
      print fmt_str, A
    end if

  end subroutine init_A

  subroutine solve(u_grid)
    ! solve A(N, N) x(N) = b(N) using LAPACK routine dgesv
    ! outputs:
    ! - u_grid : real(kind=wp)(:,:) : solution grid
    implicit none
    real(kind=wp), intent(out) :: u_grid(nr, nc)

    real(kind=wp), allocatable:: A(:,:), b(:)
    integer, allocatable :: ipiv(:)
    integer :: info

    N = (nc-2) * (nr-2)    ! matrix rank size

    call init_grid(u_grid) ! initialize spatial grid
    call init_A(A)         ! initialize coefficient matrix
    call init_b(u_grid, b) ! initialize solution vector

    allocate(ipiv(N))

    print '(a, i0)', ' solving A(N, N) x(N) = b(N) for N = ', N
    call dgesv(N, 1, A, N, ipiv, b, N, info)
    if (info /= 0) then
      print '(a, i0)', 'info = ', info
      print *, 'Error in dgesv LAPACK'
      stop 1
    end if

    u_grid(2:nr-1, 2:nc-1) = reshape(b, (/nr-2, nc-2/))

  end subroutine solve

end module lapack
