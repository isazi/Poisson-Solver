! Distributed under MIT License
!
! Copyright (c) 2022 Tom Meltzer
!
! Please see LICENSE file for more information

module config

  use precisn, only: wp
  implicit none

  private
  public :: nr, nc   ! number of rows and columns in 2D grid
  public :: dx, dy   ! spacing between columns and rows respectively
  public :: x_0, y_0 ! spatial grid runs from x_0 to x_N where x_N = x_0 + nc * dx
  public :: alpha    ! boundary value
  public :: max_iter ! max number of iterations for Gauss-Seidel routine
  public :: debug    ! print debug info if set to .true.

  integer :: nr, nc
  integer :: max_iter
  real(kind=wp), parameter :: x_0 = 1._wp, dx=0.1_wp
  real(kind=wp), parameter :: y_0 = x_0, dy=dx
  real(kind=wp), parameter :: alpha = 1.0_wp

  logical, parameter :: debug = .false.

end module config
