module precisn
    use, intrinsic :: iso_fortran_env
    implicit none
    integer, parameter :: sp = selected_real_kind(6, 37)
    integer, parameter :: dp = selected_real_kind(15, 307)
    integer, parameter :: qp = selected_real_kind(33, 4931)
    integer, parameter :: wp = dp
end module precisn

