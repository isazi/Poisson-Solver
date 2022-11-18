module config

  use precisn, only: wp
  implicit none

  private
  public :: nr, nc
  public :: x_0, y_0, dx, dy
  public :: alpha
  public :: max_iter
  public :: debug

  integer :: nr, nc
  integer :: max_iter
  real(kind=wp), parameter :: x_0 = 1._wp, dx=0.1_wp
  real(kind=wp), parameter :: y_0 = x_0, dy=dx
  real(kind=wp), parameter :: alpha = 1.0_wp

  logical, parameter :: debug = .false.

end module config
