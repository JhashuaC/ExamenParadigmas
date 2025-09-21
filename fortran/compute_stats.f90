! compute_stats.f90
! Lee input.csv (cabecera + M filas x K columnas), calcula mean, median, stddev (muestral) y outliers (|z| >= 3)
program compute_stats
  implicit none

  character(len=256) :: infile = 'input.csv'
  character(len=256) :: outfile = 'stats.csv'
  character(len=256) :: elog = 'errors.log'
  integer :: ios, M, K
  real, allocatable :: data(:,:)
  real, allocatable :: means(:), medians(:), stddevs(:)
  integer, allocatable :: outliers(:)

  if (command_argument_count() >= 1) call get_command_argument(1, infile)
  if (command_argument_count() >= 2) call get_command_argument(2, outfile)

  ! create (or replace) error log
  open(newunit=ios, file=elog, status='replace', action='write', iostat=ios)
  if (ios == 0) then
     write(ios,'(A)') 'Log start'
     write(ios,'(A)') '-------------'
     close(ios)
  else
     write(*,*) 'Warning: could not create ', trim(elog)
  end if

  call read_csv_two_pass(infile, data, M, K, elog)

  if (.not. allocated(data)) then
     write(*,*) 'No data loaded. Check ', trim(elog)
     stop 1
  end if

  allocate(means(K), medians(K), stddevs(K), outliers(K))
  call compute_stats_array(data, M, K, means, medians, stddevs, outliers)
  call write_stats_csv(outfile, K, means, medians, stddevs, outliers)

  ! cleanup
  if (allocated(data)) deallocate(data)
  if (allocated(means)) deallocate(means)
  if (allocated(medians)) deallocate(medians)
  if (allocated(stddevs)) deallocate(stddevs)
  if (allocated(outliers)) deallocate(outliers)

  write(*,*) 'Done. Output -> ', trim(outfile), '   (errors -> ', trim(elog), ')'

contains

  subroutine read_csv_two_pass(filename, data, M, K, elog)
    implicit none
    character(len=*), intent(in) :: filename
    real, allocatable, intent(out) :: data(:,:)
    integer, intent(out) :: M, K
    character(len=*), intent(in) :: elog

    integer :: unit_in, ios, iostat2, rowcount, i, j
    character(len=1000) :: line, header
    integer :: lenline, start, comma_pos
    character(len=256) :: token
    real :: val
    integer :: logu
    real, parameter :: missing_sentinel = huge(1.0)

    ! defaults
    M = 0; K = 0
    if (allocated(data)) deallocate(data)
    allocate(data(0,0))

    ! open file
    open(newunit=unit_in, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
       open(newunit=logu, file=elog, status='old', action='write', position='append', iostat=iostat2)
       if (iostat2 == 0) then
          write(logu,'(A)') 'ERROR: Could not open input file: '//trim(filename)
          close(logu)
       end if
       return
    end if

    ! 1st pass: read header, count rows
    rowcount = 0
    read(unit_in,'(A)', iostat=ios) header
    if (ios /= 0) then
       open(newunit=logu, file=elog, status='old', action='write', position='append', iostat=iostat2)
       if (iostat2 == 0) then
          write(logu,'(A)') 'ERROR: Empty or missing header in input file.'
          close(logu)
       end if
       close(unit_in)
       return
    end if

    if (len_trim(header) == 0) then
       open(newunit=logu, file=elog, status='old', action='write', position='append', iostat=iostat2)
       if (iostat2 == 0) then
          write(logu,'(A)') 'ERROR: Header line empty.'
          close(logu)
       end if
       close(unit_in)
       return
    end if

    K = 1
    do i = 1, len_trim(header)
       if (header(i:i) == ',') K = K + 1
    end do

    do
       read(unit_in,'(A)', iostat=ios) line
       if (ios /= 0) exit
       ! ignore completely blank lines
       if (len_trim(line) > 0) rowcount = rowcount + 1
    end do
    close(unit_in)

    if (rowcount == 0) then
       open(newunit=logu, file=elog, status='old', action='write', position='append', iostat=iostat2)
       if (iostat2 == 0) then
          write(logu,'(A)') 'ERROR: No data rows found in input file.'
          close(logu)
       end if
       return
    end if

    M = rowcount
    allocate(data(M, K))
    data = missing_sentinel

    ! 2nd pass: parse rows safely
    open(newunit=unit_in, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
       open(newunit=logu, file=elog, status='old', action='write', position='append', iostat=iostat2)
       if (iostat2 == 0) then
          write(logu,'(A)') 'ERROR: Could not reopen file for parsing.'
          close(logu)
       end if
       return
    end if

    ! skip header
    read(unit_in,'(A)', iostat=ios) line
    if (ios /= 0) then
       close(unit_in)
       return
    end if


    rowcount = 0
    do
       read(unit_in,'(A)', iostat=ios) line
       if (ios /= 0) exit
       if (len_trim(line) == 0) cycle
       rowcount = rowcount + 1
       lenline = len_trim(line)
       start = 1

       do j = 1, K
          if (start > lenline) then
             token = ''
          else
             comma_pos = index(line(start:lenline), ',')
             if (comma_pos == 0) then
                token = adjustl(line(start:lenline))
                start = lenline + 1
             else
                if (comma_pos == 1) then
                   token = ''
                   start = start + 1
                else
                   token = adjustl(line(start:start+comma_pos-2))
                   start = start + comma_pos
                end if
             end if
          end if

          call read_val_safe(trim(token), val, filename, rowcount, j, elog, missing_sentinel)
          if (rowcount >= 1 .and. rowcount <= M .and. j >= 1 .and. j <= K) then
             data(rowcount, j) = val
          else
             ! shouldn't happen, but log if indices wrong
             open(newunit=logu, file=elog, status='old', action='write', position='append', iostat=iostat2)
             if (iostat2 == 0) then
                write(logu,'(A)') 'ERROR: index out of range while filling data at row '//trim(adjustl(itoa(rowcount)))//' col '//trim(adjustl(itoa(j)))
                close(logu)
             end if
          end if
       end do
    end do

    close(unit_in)
  end subroutine read_csv_two_pass

  subroutine read_val_safe(str, val, filename, row, col, elog, missing_sentinel)
    implicit none
    character(len=*), intent(in) :: str
    real, intent(out) :: val
    character(len=*), intent(in) :: filename, elog
    integer, intent(in) :: row, col
    real, intent(in) :: missing_sentinel
    integer :: r, logu

    if (len_trim(str) == 0) then
       val = missing_sentinel
       open(newunit=logu, file=elog, status='old', action='write', position='append', iostat=r)
       if (r == 0) then
          write(logu,'(A)') 'WARNING: Empty value at row '//trim(adjustl(itoa(row)))//', col '//trim(adjustl(itoa(col)))
          close(logu)
       end if
       return
    end if

    read(str,*,iostat=r) val
    if (r /= 0) then
       val = missing_sentinel
       open(newunit=logu, file=elog, status='old', action='write', position='append', iostat=r)
       if (r == 0) then
          write(logu,'(A)') 'ERROR: Non-numeric at row '//trim(adjustl(itoa(row)))//', col '//trim(adjustl(itoa(col)))//' token="'//trim(str)//'"'
          close(logu)
       end if
    end if
  end subroutine read_val_safe

  subroutine compute_stats_array(data, M, K, means, medians, stddevs, outliers)
    implicit none
    real, intent(in) :: data(:,:)
    integer, intent(in) :: M, K
    real, intent(out) :: means(:), medians(:), stddevs(:)
    integer, intent(out) :: outliers(:)

    integer :: j, i, n
    real, allocatable :: col(:), tmp(:)
    real :: s, mean, sd
    real, parameter :: missing_sentinel = huge(1.0)

    do j = 1, K
       ! gather valid values
       n = 0
       allocate(col(M))
       do i = 1, M
          if (data(i,j) /= missing_sentinel) then
             n = n + 1
             col(n) = data(i,j)
          end if
       end do

       if (n == 0) then
          means(j) = 0.0
          medians(j) = 0.0
          stddevs(j) = 0.0
          outliers(j) = 0
          deallocate(col)
          cycle
       end if

       ! mean
       s = 0.0
       do i = 1, n
          s = s + col(i)
       end do
       mean = s / real(n)
       means(j) = mean

       ! sample stddev
       if (n > 1) then
          sd = 0.0
          do i = 1, n
             sd = sd + (col(i) - mean)**2
          end do
          sd = sqrt(sd / real(n - 1))
       else
          sd = 0.0
       end if
       stddevs(j) = sd

       ! median: sort tmp(1:n)
       allocate(tmp(n))
       tmp(1:n) = col(1:n)
       call quicksort(tmp, 1, n)
       if (mod(n,2) == 1) then
          medians(j) = tmp((n+1)/2)
       else
          medians(j) = 0.5 * ( tmp(n/2) + tmp(n/2 + 1) )
       end if
       deallocate(tmp)

       ! outliers
       outliers(j) = 0
       if (sd > 0.0) then
          do i = 1, n
             if (abs((col(i) - mean) / sd) >= 3.0) outliers(j) = outliers(j) + 1
          end do
       end if

       deallocate(col)
    end do
  end subroutine compute_stats_array

  subroutine write_stats_csv(outfile, K, means, medians, stddevs, outliers)
    implicit none
    character(len=*), intent(in) :: outfile
    integer, intent(in) :: K
    real, intent(in) :: means(:), medians(:), stddevs(:)
    integer, intent(in) :: outliers(:)
    integer :: unit_out, i, ios

    open(newunit=unit_out, file=outfile, status='replace', action='write', iostat=ios)
    if (ios /= 0) then
       write(*,*) 'ERROR: Could not open output file: ', trim(outfile)
       return
    end if

    write(unit_out, '(A)') 'variable,mean,median,stddev,outliers'
    do i = 1, K
       write(unit_out, '(A)') 'col'//trim(adjustl(itoa(i)))//','// &
            trim(adjustl(to_string(means(i))))//','//trim(adjustl(to_string(medians(i))))//','// &
            trim(adjustl(to_string(stddevs(i))))//','//trim(adjustl(itoa(outliers(i))))
    end do

    close(unit_out)
  end subroutine write_stats_csv

  ! Basic quicksort (in-place)
  recursive subroutine quicksort(a, left, right)
    implicit none
    real, intent(inout) :: a(:)
    integer, intent(in) :: left, right
    integer :: i, j
    real :: pivot, tmp
    if (left >= right) return
    pivot = a((left + right) / 2)
    i = left
    j = right
    do
       do while (a(i) < pivot)
          i = i + 1
       end do
       do while (a(j) > pivot)
          j = j - 1
       end do
       if (i <= j) then
          tmp = a(i); a(i) = a(j); a(j) = tmp
          i = i + 1
          j = j - 1
       end if
       if (i > j) exit
    end do
    if (left < j) call quicksort(a, left, j)
    if (i < right) call quicksort(a, i, right)
  end subroutine quicksort

  ! helpers ----------------------------------------------------------------
  function itoa(i) result(s)
    implicit none
    integer, intent(in) :: i
    character(len=32) :: s
    write(s,'(I0)') i
  end function itoa

  function to_string(x) result(s)
    implicit none
    real, intent(in) :: x
    character(len=32) :: s
    write(s,'(F10.6)') x
  end function to_string

end program compute_stats
