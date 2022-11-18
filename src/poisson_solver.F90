program poisson_solver

  use omp_lib
  use precisn, only: wp
  use config, only: nr, nc, max_iter, debug, dx
  use color, only: color_group, init_color_groups
  use grid, only: init_grid, get_xy_pos, func
#ifdef USEGS
  use gauss_seidel, only: solve
#else
  use lapack, only: solve
#endif

  implicit none

  real(kind=wp), allocatable :: u_grid(:, :)
  real(kind=wp) :: t1, t2
  character(len=20) :: u_fmt, a_fmt, u_wrt
  character(len=12), dimension(:), allocatable :: args
  integer       :: num_args, i
#ifdef USEGS
  real(kind=wp) :: max_diff
  integer       :: n_iter
#endif

  t1 = omp_get_wtime()

  num_args = command_argument_count()
  if (num_args == 0) then
    print *, 'Error: you need to specify nr and n_iter'
    stop 100
  end if

  allocate(args(num_args))
  do i = 1, num_args
    call get_command_argument(i, args(i))
  end do

  read(args(1), *) nr
  nc = nr
  read(args(2), *) max_iter

  print *, 'max_iter = ',  max_iter

  allocate(u_grid(nr, nc))

  write(u_fmt, '(a, i0, a)') '(', nc, 'f6.2)'
  write(u_wrt, '(a, i0, a)') '(', nc, 'e14.6)'
  write(a_fmt, '(a, i0, a)') '(', (nc-2)*(nr-2), 'f6.2)'

  print '(a,i0,a,i0,a)', 'Problem size u_grid dims = [', nr, ',', nc, ']'

#ifdef USEGS

  print *, 'using Gauss-Seidel'
  call solve(u_grid, max_diff, n_iter)

  print *, 'converged with max_diff = ', max_diff
  print *, 'number of iterations = ', n_iter

#else

  print *, 'using LAPACK'
  call solve(u_grid)

#endif

  t2 = omp_get_wtime()
  print '(a, f10.4)', ' time (s) = ', t2 - t1
  print *, 'finished'
  write(10, u_wrt) u_grid
  ! write(*, u_wrt) u_grid

end program poisson_solver
