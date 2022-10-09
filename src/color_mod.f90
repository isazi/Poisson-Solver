module color

  use precisn, only: wp
  use config, only: nr, nc, debug

  implicit none

  type color_group
    integer, allocatable :: rows(:)
    integer, allocatable :: cols(:)
    integer :: num
  end type color_group

  private
  public :: init_color_groups, color_group

contains

  function is_red(r, c) result(ans)
    implicit none
    integer, intent(in) :: r, c
    logical :: ans

    ans = mod(r+c, 2) == 0
  end function is_red

  subroutine init_color_groups(red, blu)
    use config, only: nr, nc
    implicit none
    type(color_group), intent(out) :: red, blu
    integer :: i, j, length

    length = ((nc-2)*(nr-2)+1)/2 ! integer division
    allocate(red%rows(length), red%cols(length))
    allocate(blu%rows(length), blu%cols(length))

    red%num = 0
    blu%num = 0

    do j = 2, nc-1
      do i = 2, nr-1
        if (is_red(i, j)) then
          red%rows(red%num+1) = i
          red%cols(red%num+1) = j
          red%num = red%num + 1
        else
          blu%rows(blu%num+1) = i
          blu%cols(blu%num+1) = j
          blu%num = blu%num + 1
        end if
      end do
    end do

    if (debug) then
      print *, 'red = '
      print '(20(i0, 1x))',  red%rows
      print '(20(i0, 1x))',  red%cols

      print *, 'blu = '
      print '(20(i0, 1x))',  blu%rows
      print '(20(i0, 1x))',  blu%cols
    end if

  end subroutine init_color_groups

end module color
