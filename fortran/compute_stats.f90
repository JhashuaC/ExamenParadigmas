! compute_stats.f90
! Programa que lee input.csv, calcula mean, median, stddev (muestra) y outliers (|z| >= 3)
! Genera stats.csv y errors.log
program compute_stats
  implicit none

  ! variables
  character(len=256) :: infile, outfile, elog
  integer :: M, K, ios
  real, allocatable :: data(:,:)
  real, allocatable :: means(:), medians(:), stddevs(:)
  integer, allocatable :: outliers(:)

  ! default filenames
  infile = 'input.csv'
  outfile = 'stats.csv'
  elog = 'errors.log'

  ! remove previous errors.log if exists (attempt)
  open(unit=99, file=elog, status='replace', action='write', iostat=ios)
  if (ios /= 0) then
     write(*,*) 'Warning: Could not create errors.log (iostat=', ios, ')'
  end if
  close(99)

  ! Read CSV and get data in array (missing or non-numeric lines are skipped per value)
  call read_csv(infile, data, M, K, elog)

  if (.not. allocated(data)) then
     write(*,*) 'No data loaded. Check errors.log.'
     stop 1
  end if

  ! allocate result arrays
  allocate(means(K), medians(K), stddevs(K), outliers(K))
  means = 0.0
  medians = 0.0
  stddevs = 0.0
  outliers = 0

  ! compute stats
  call compute_stats_array(data, M, K, means, medians, stddevs, outliers)

  ! write stats.csv
  call write_stats_csv(outfile, K, means, medians, stddevs, outliers)

  ! deallocate
  deallocate(data)
  deallocate(means, medians, stddevs, outliers)

  write(*,*) 'Done. Output written to ', trim(outfile)
  write(*,*) 'Errors logged to ', trim(elog)
contains

  subroutine read_csv(filename, data, M, K, elog)
    implicit none
    character(len=*), intent(in) :: filename
    real, allocatable, intent(out) :: data(:,:)
    integer, intent(out) :: M, K
    character(len=*), intent(in) :: elog

    integer :: unit_in, ios, i, j, stat
    character(len=1000) :: line
    integer, parameter :: maxcols = 1000
    character(len=:), allocatable :: header
    integer :: ncols
    real, allocatable :: coltemp(:)
    real, allocatable :: tmpdata(:,:)
    integer :: maxrows
    character(len=1000) :: token
    integer :: pos, start, comma_pos
    logical :: firstline
    integer :: rowcount
    character(len=256) :: errmsg
    integer :: rstat
    real :: val
    integer :: istrlen

    ! initialization
    M = 0; K = 0
    allocate(data(0,0)) ; ! will be replaced
    maxrows = 1000000    ! límite razonable (puedes cambiar si necesitas más)
    rowcount = 0
    firstline = .true.

    open(newunit=unit_in, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
       open(unit=99, file=elog, status='old', action='write', position='append')
       write(99, '(A)') 'ERROR: Could not open input file: '//trim(filename)
       close(99)
       return
    end if

    ! allocate a temporary large array (grow if needed)
    ! We'll read line by line, detect K from header (first line)
    allocate(tmpdata(maxrows,1))
    do
       read(unit_in,'(A)', iostat=ios) line
       if (ios /= 0) exit
       if (firstline) then
          ! determine number of columns from header line by counting commas
          ncols = 1
          do pos = 1, len_trim(line)
             if (line(pos:pos) == ',') ncols = ncols + 1
          end do
          K = ncols
          ! reallocate tmpdata to have K columns
          deallocate(tmpdata)
          allocate(tmpdata(maxrows,K))
          tmpdata = 0.0
          firstline = .false.
          cycle
       end if

       ! parse numeric values in the line, splitting by comma
       start = 1
       do j = 1, K
          ! find next comma or end
          comma_pos = index(line(start:), ',')
          if (comma_pos == 0) then
             token = adjustl(line(start:))
          else
             token = adjustl(line(start:start+comma_pos-2))
          end if

          ! trim spaces
          token = trim(token)
          if (len_trim(token) == 0) then
             ! missing value -> log and set NaN (we will treat as missing by skipping later)
             open(unit=99, file=elog, status='old', action='write', position='append', iostat=rstat)
             if (rstat == 0) then
                write(99,'(A)') 'WARNING: Empty value at row '//trim(adjustl(itoa(rowcount+1)))//', col '//trim(adjustl(itoa(j)))
                close(99)
             end if
             val = -huge(1.0)  ! sentinel for missing
          else
             ! try to read real
             read(token, *, iostat=rstat) val
             if (rstat /= 0) then
                ! non-numeric
                open(unit=99, file=elog, status='old', action='write', position='append', iostat=rstat)
                if (rstat == 0) then
                   write(99,'(A)') 'ERROR: Non-numeric at row '//trim(adjustl(itoa(rowcount+1)))//', col '//trim(adjustl(itoa(j)))//' token="'//trim(token)//'"'
                   close(99)
                end if
                val = -huge(1.0)
             end if
          end if

          tmpdata(rowcount+1, j) = val

          if (comma_pos == 0) exit
          start = start + comma_pos
       end do
       rowcount = rowcount + 1
       if (rowcount >= maxrows) exit
    end do

    close(unit_in)

    if (rowcount == 0) then
       open(unit=99, file=elog, status='old', action='write', position='append', iostat=rstat)
       if (rstat == 0) then
          write(99, '(A)') 'ERROR: No data rows found in input file.'
          close(99)
       end if
       ! leave data unallocated to signal error
       deallocate(tmpdata)
       return
    end if

    ! allocate exact sized data and copy (replacing sentinel -huge with NaN marker -huge)
    M = rowcount
    allocate(data(M, K))
    do i = 1, M
       do j = 1, K
          data(i,j) = tmpdata(i,j)
       end do
    end do

    deallocate(tmpdata)
  end subroutine read_csv

  subroutine compute_stats_array(data, M, K, means, medians, stddevs, outliers)
    implicit none
    real, intent(in) :: data(:,:)
    integer, intent(in) :: M, K
    real, intent(out) :: means(:), medians(:), stddevs(:)
    integer, intent(out) :: outliers(:)

    integer :: j, i, count_valid
    real, allocatable :: col(:), colcopy(:)
    real :: s, mean, sd, v
    real, parameter :: missing_sentinel = -huge(1.0)
    integer :: idx, n

    allocate(col(M))
    allocate(colcopy(M))

    do j = 1, K
       ! collect valid values for column j
       n = 0
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
          cycle
       end if

       ! compute mean
       s = 0.0
       do i = 1, n
          s = s + col(i)
       end do
       mean = s / real(n)
       means(j) = mean

       ! compute stddev (sample: divide by n-1 if n>1; else 0)
       if (n > 1) then
          sd = 0.0
          do i = 1, n
             sd = sd + (col(i) - mean)**2
          end do
          sd = sqrt(sd / real(n-1))
       else
          sd = 0.0
       end if
       stddevs(j) = sd

       ! median: sort copy of col(1:n)
       colcopy(1:n) = col(1:n)
       call quicksort(colcopy, 1, n)
       if (mod(n,2) == 1) then
          medians(j) = colcopy((n+1)/2)
       else
          medians(j) = 0.5*(colcopy(n/2) + colcopy(n/2+1))
       end if

       ! outliers: |(x-mean)/sd| >= 3 ; if sd == 0 -> zero outliers
       outliers(j) = 0
       if (sd > 0.0) then
          do i = 1, n
             if (abs((col(i)-mean)/sd) >= 3.0) outliers(j) = outliers(j) + 1
          end do
       end if

    end do

    deallocate(col, colcopy)
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
       write(*,*) 'ERROR: Could not open output file: ', outfile
       return
    end if

    write(unit_out, '(A)') 'variable,mean,median,stddev,outliers'
    do i = 1, K
       write(unit_out, '(A,I0,A,F8.6,A,F8.6,A,F8.6,A,I0)') 'col', i, ',', means(i), ',', medians(i), ',', stddevs(i), ',', outliers(i)
    end do

    close(unit_out)
  end subroutine write_stats_csv

  ! QuickSort for real array (ascending). Uses indices inclusive left,right
  recursive subroutine quicksort(a, left, right)
    implicit none
    real, intent(inout) :: a(:)
    integer, intent(in) :: left, right
    integer :: i, j
    real :: pivot, tmp
    if (left >= right) return
    pivot = a((left+right)/2)
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

  ! simple integer->string (Fortran no intrinsic itoa)
  function itoa(i) result(s)
    implicit none
    integer, intent(in) :: i
    character(len=32) :: s
    write(s,'(I0)') i
  end function itoa

end program compute_stats
