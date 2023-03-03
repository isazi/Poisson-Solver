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
    ! Check if a given combination of row and column corresponds to a red or black element.
    ! inputs:
    ! - r : int : row index
    ! - c : int : column index
    ! returns:
    ! - ans : logical : .true. if element is red
    implicit none
    integer, intent(in) :: r, c
    logical :: ans

    ans = (mod(r+c, 2) == 0)
  end function is_red

  subroutine init_color_groups(red, black)
    ! For a 2D spatial grid of dimension (nr, nc) setup the color_groups for each color (red/black). This routine essentially
    ! converts the 2D spatial grid into two sets of 1D arrays containing column and row indices.
    ! outputs:
    ! - red   : color_group : row and column information for red elements
    ! - black : color_group : row and column information for black elements
    use config, only: nr, nc
    implicit none
    type(color_group), intent(out) :: red, black
    integer :: i, j, length

    ! convert 2D spatial grid to 1D arrays containing row and column indices (color_groups)
    length = ((nc-2)*(nr-2)+1)/2 ! integer division
    allocate(red%rows(length), red%cols(length))
    allocate(black%rows(length), black%cols(length))

    red%num = 0
    black%num = 0

    ! initialize color_group indices
    do j = 2, nc-1
      do i = 2, nr-1
        if (is_red(i, j)) then
          red%rows(red%num+1) = i
          red%cols(red%num+1) = j
          red%num = red%num + 1
        else
          black%rows(black%num+1) = i
          black%cols(black%num+1) = j
          black%num = black%num + 1
        end if
      end do
    end do

    if (debug) then
      print *, 'red = '
      print '(20(i0, 1x))',  red%rows
      print '(20(i0, 1x))',  red%cols

      print *, 'black = '
      print '(20(i0, 1x))',  black%rows
      print '(20(i0, 1x))',  black%cols
    end if

  end subroutine init_color_groups

end module color
