program gauss_seidel

  use omp_lib
  use precisn, only: wp
  use config, only: nr, nc, max_iter

  implicit none

  type color_coord
    integer, allocatable :: rows(:)
    integer, allocatable :: cols(:)
    integer, allocatable :: id(:)
  end type color_coord

  real(kind=wp) :: u_grid(nr, nc), u_grid_old(nr, nc)
  real(kind=wp) :: u_next
  character(len=20) :: u_fmt
  integer :: length
  integer :: i, j, r, c
  integer :: n_iter
  integer :: rcount, bcount
  type(color_coord) :: red, blu

  length = ((nc-2)*(nr-2)+1)/2 ! integer division
  print *, 'length = '
  print *,  length
  allocate(red%rows(length), red%cols(length))
  allocate(blu%rows(length), blu%cols(length))


  call init_grid(u_grid)
  write(u_fmt, '(a, i0, a)') '(', nc, 'f6.2)'

  !!$omp parallel
  !print *, 'hello from process: ', omp_get_thread_num()
  !!$omp end parallel

  rcount = 0
  bcount = 0

  do j = 2, nc-1
    do i = 2, nr-1
      if (is_red(i, j)) then
        red%rows(rcount+1) = i
        red%cols(rcount+1) = j
        rcount = rcount + 1
      else
        blu%rows(bcount+1) = i
        blu%cols(bcount+1) = j
        bcount = bcount + 1
      end if
    end do
  end do

  print *, 'red = '
  print '(20(i0, 1x))',  red%rows
  print '(20(i0, 1x))',  red%cols

  print *, 'blu = '
  print '(20(i0, 1x))',  blu%rows
  print '(20(i0, 1x))',  blu%cols

  print *, 'initial grid'
  print u_fmt, u_grid

  do n_iter = 1, max_iter

    u_grid_old = u_grid

    do i = 1, rcount
      r = red%rows(i)
      c = red%cols(i)
      call gs_method(r, c, u_grid, u_next)
      u_grid(r, c) = u_next
    end do

    do i = 1, bcount
      r = blu%rows(i)
      c = blu%cols(i)
      call gs_method(r, c, u_grid, u_next)
      u_grid(r, c) = u_next
    end do

    print *, 'max_diff = ', maxval(abs(u_grid - u_grid_old))

    if (maxval(abs(u_grid - u_grid_old)) < 1e-6) then
      print u_fmt, u_grid
      print *, 'number of iterations = ', n_iter
      exit
    end if

  end do


contains

  function is_red(r, c) result(ans)
    implicit none
    integer, intent(in) :: r, c
    logical :: ans

    ans = mod(r+c, 2) == 0
  end function is_red

  subroutine gs_method(r, c, u_grid, u)
    implicit none
    integer, intent(in) :: r, c
    real(kind=wp), intent(in)  :: u_grid(:,:)
    real(kind=wp), intent(out) :: u

    real(kind=wp) :: x, y
    real(kind=wp) :: b

    call get_xy_pos(r, c, x, y)
    b = func(x, y)
    u = (1._wp/4._wp) * (b - u_grid(r-1, c) - u_grid(r+1, c) - u_grid(r, c-1) - u_grid(r, c+1))

  end subroutine gs_method

  function func(x, y) result(f)
    implicit none
    real(kind=wp), intent(in) :: x, y
    real(kind=wp) :: f

    f = x*y*0._wp + 1._wp

  end function func

  subroutine get_xy_pos(r, c, x, y)
    use config, only: x_0, y_0, dx, dy
    implicit none
    integer, intent(in) :: r, c
    real(kind=wp), intent(out)  :: x, y

    x = x_0 + (c - 1) * dx
    y = y_0 + (r - 1) * dy

  end subroutine get_xy_pos

  subroutine init_grid(u_grid)
    use config, only: nr, nc, alpha
    implicit none
    real(kind=wp), intent(inout)  :: u_grid(:,:)

    u_grid = 0._wp

    u_grid(1, :) = alpha
    u_grid(:, 1) = alpha
    u_grid(nr, :) = alpha
    u_grid(:, nc) = alpha

  end subroutine init_grid

end program gauss_seidel
