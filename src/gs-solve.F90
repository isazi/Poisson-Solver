program gauss_seidel

  use omp_lib
  use precisn, only: wp
  use config, only: nr, nc, max_iter, debug
  use color, only: color_group, init_color_groups
  use grid, only: init_grid, get_xy_pos

  implicit none

  external :: dgesv, dsysv

  ! subroutine dgesv(n,nrhs,a,lda,ipiv,b,ldb,info)
  !   integer       , intent(in)                           :: n, nrhs, lda, ldb
  !   real(kind=wp) , intent(inout) , dimension(lda, n)    :: a
  !   integer       , intent(out)   , dimension(n)         :: ipiv
  !   real(kind=wp) , intent(inout) , dimension(ldb, nrhs) :: b
  !   integer       , intent(out)                          :: info
  ! end subroutine dgesv

  real(kind=wp) :: u_grid(nr, nc)
  character(len=20) :: u_fmt, a_fmt
  real(kind=wp) :: u_next, diff
  integer :: i, r, c
  integer :: n_iter
#ifdef USEGS
  type(color_group) :: red, blu
  real(kind=wp) :: u_grid_old(nr, nc)
#else
  real(kind=wp), allocatable:: A(:,:), x(:), b(:), A_copy(:,:)
  real(kind=wp) :: xp, yp
  integer :: j, N, info, ind
  integer, allocatable :: ipiv(:)
#endif

  call init_grid(u_grid)

  write(u_fmt, '(a, i0, a)') '(', nc, 'f6.2)'
  write(a_fmt, '(a, i0, a)') '(', (nc-2)*(nr-2), 'f6.2)'

#ifdef USEGS

  print *, 'using Gauss-Seidel'

  call init_color_groups(red, blu)

  do n_iter = 1, max_iter

    u_grid_old = u_grid

    !$omp parallel do private(i, r, c, u_next)
    do i = 1, red%num
      r = red%rows(i)
      c = red%cols(i)
      call gs_method(r, c, u_grid, u_next)
      u_grid(r, c) = u_next
    end do
    !$omp end parallel do

    !$omp parallel do private(i, r, c, u_next)
    do i = 1, blu%num
      r = blu%rows(i)
      c = blu%cols(i)
      call gs_method(r, c, u_grid, u_next)
      u_grid(r, c) = u_next
    end do
    !$omp end parallel do

    diff = maxval(abs(u_grid - u_grid_old))

    if (diff < 1e-6) exit

  end do

  print *, 'converged with diff = ', diff
  print *, 'number of iterations = ', n_iter

#else

  print *, 'using LAPACK'

  N = (nc-2) * (nr-2)

  allocate(A(N,N), x(N), b(N), ipiv(N), A_copy(N,N))

  A = 0._wp
  x = 0._wp
  b = 0._wp

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

  ! print *, '----------------------'
  ! print a_fmt, A

  A_copy = A

  do i = 1, nr-2
    do j = 1, nc-2
      r = i + 1
      c = j + 1
      call get_xy_pos(r+1, c+1, xp, yp)
      b((i-1)*(nr-2)+j) = func(xp, yp) - u_grid(r-1, c) - u_grid(r+1, c) - u_grid(r, c-1) - u_grid(r, c+1)
    end do
  end do

  call dgesv(N, 1, A_copy, N, ipiv, b, N, info)
  if (info /= 0) then
    print '(a, i0)', 'info = ', info
    print *, 'Error in dgesv LAPACK'
    stop 1
  end if

  u_grid(2:nr-1, 2:nc-1) = reshape(b, (/nr-2, nc-2/))

#endif

  print *, '----------------------'
  print u_fmt, u_grid

contains

  subroutine gs_method(r, c, u_grid, u)
    implicit none
    integer, intent(in) :: r, c
    real(kind=wp), intent(in)  :: u_grid(:,:)
    real(kind=wp), intent(out) :: u

    real(kind=wp) :: x, y
    real(kind=wp) :: b

    call get_xy_pos(r, c, x, y)
    b = func(x, y)
    u = -(1._wp/4._wp) * (b - u_grid(r-1, c) - u_grid(r+1, c) - u_grid(r, c-1) - u_grid(r, c+1))

  end subroutine gs_method

  function func(x, y) result(f)
    implicit none
    real(kind=wp), intent(in) :: x, y
    real(kind=wp) :: f

    f = x*y*0._wp + 1._wp

  end function func

end program gauss_seidel
