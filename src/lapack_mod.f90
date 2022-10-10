module lapack

  use omp_lib
  use precisn, only: wp
  use config, only: nr, nc, max_iter, debug, dx
  use grid, only: init_grid, get_xy_pos, init_grid, func

  implicit none

  external :: dgesv
  integer :: N

contains

  subroutine init_b(b, u_grid)
    implicit none
    real(kind=wp), intent(inout), allocatable :: b(:)
    real(kind=wp), intent(in) :: u_grid(nr, nc)

    real(kind=wp) :: x, y
    integer :: i, r, c

    allocate(b(N))

    b = 0._wp

    associate( width => nr-2 )
      do i = 1, N
        r = (i-1) / width + 2
        c = mod(i-1, width) + 2
        call get_xy_pos(r, c, x, y)
        b(i) = func(x, y) * dx * dx - u_grid(r-1, c) - u_grid(r+1, c) - u_grid(r, c-1) - u_grid(r, c+1)
      end do
    end associate

  end subroutine init_b

  subroutine init_A(A)
    implicit none
    real(kind=wp), intent(inout), allocatable :: A(:,:)

    integer :: i, r, c

    allocate(A(N,N))
    A = 0._wp
    associate( width => nr-2 )
      do i = 1, N
        r = (i-1) / width + 1
        c = mod(i-1, width) + 1
        A(i, i) = -4._wp
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
  end subroutine init_A

  subroutine solve(u_grid)

    real(kind=wp), intent(inout) :: u_grid(nr, nc)

    ! local vars
    real(kind=wp), allocatable:: A(:,:), b(:)
    integer, allocatable :: ipiv(:)
    integer :: info

    N = (nc-2) * (nr-2)

    call init_grid(u_grid)
    call init_A(A)
    call init_b(b, u_grid)

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
