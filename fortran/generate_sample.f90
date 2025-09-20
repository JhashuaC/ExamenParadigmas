! generate_sample.f90
program generate_sample
  implicit none
  integer :: M, K, i, j, ios
  character(len=128) :: outfile
  real :: r
  integer :: unit_out
  call random_seed()

  M = 1000
  K = 5
  outfile = 'input.csv'

  open(newunit=unit_out, file=outfile, status='replace', action='write', iostat=ios)
  if (ios /= 0) then
     write(*,*) 'Cannot open output file.'
     stop
  end if

  ! header
  do j = 1, K
     if (j < K) then
        write(unit_out, '(A)', advance='no') 'col'//trim(adjustl(itoa(j)))//','
     else
        write(unit_out, '(A)') 'col'//trim(adjustl(itoa(j)))
     end if
  end do

  do i = 1, M
     do j = 1, K
        call random_number(r)
        ! generate some distributions: column 1 normal-ish, 2 uniform, 3 with outliers
        select case (j)
        case (1)
           r = 50.0 + 10.0*(r - 0.5)  ! approx normal-ish
        case (2)
           r = 10.0 + 90.0*r
        case (3)
           r = 5.0 + 2.0*r
           if (mod(i,200) == 0) r = r * 20.0  ! occasional outlier
        case (4)
           r = 100.0 * r
        case (5)
           r = 20.0 + 5.0*(r - 0.5)
        end select

        if (j < K) then
           write(unit_out,'(F0.6,A)', advance='no') r, ','
        else
           write(unit_out,'(F0.6)') r
        end if
     end do
  end do

  close(unit_out)
  write(*,*) 'Sample input.csv generated with M=', M, ' K=', K

contains
  function itoa(i) result(s)
    implicit none
    integer, intent(in) :: i
    character(len=32) :: s
    write(s,'(I0)') i
  end function itoa

end program generate_sample
