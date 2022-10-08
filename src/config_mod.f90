module config

  use precisn, only: wp
  implicit none

  private
  public :: nr, nc
  public :: x_0, y_0, dx, dy
  public :: alpha
  public :: max_iter

  integer, parameter :: nr = 6, nc = 6
  integer, parameter :: max_iter = 100
  real(kind=wp), parameter :: x_0 = 1._wp, dx=1._wp
  real(kind=wp), parameter :: y_0 = 1._wp, dy=1._wp
  real(kind=wp), parameter :: alpha = 6.0_wp

end module config


