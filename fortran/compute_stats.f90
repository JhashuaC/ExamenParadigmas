program compute_stats
  implicit none

  character(len=256) :: archivo_entrada = 'input.csv'
  character(len=256) :: archivo_salida  = 'stats.csv'
  character(len=256) :: archivo_log     = 'errors.log'

  integer :: filas, cols, ios

  real, allocatable :: tabla(:,:)
  real, allocatable :: medias(:), medianas(:), desvios(:)
  integer, allocatable :: num_outliers(:)

  ! si se pasan argumentos por consola (opcional)
  if (command_argument_count() >= 1) call get_command_argument(1, archivo_entrada)
  if (command_argument_count() >= 2) call get_command_argument(2, archivo_salida)

  ! reinicia el log
  open(newunit=ios, file=archivo_log, status='replace', action='write')
  write(ios,'(A)') 'Inicio del log de errores'
  write(ios,'(A)') '---------------------------------'
  close(ios)

  call leer_csv(archivo_entrada, tabla, filas, cols, archivo_log)

  if (.not. allocated(tabla)) then
     write(*,*) 'No se pudieron cargar datos, revise el log por favooooor.'
     stop 1
  end if

  ! reservar memoria para resultados
  allocate(medias(cols), medianas(cols), desvios(cols), num_outliers(cols))

  call calcular_estadisticas(tabla, filas, cols, medias, medianas, desvios, num_outliers)
  call escribir_resultados(archivo_salida, cols, medias, medianas, desvios, num_outliers)

  ! liberar memoria
  if (allocated(tabla)) deallocate(tabla)
  if (allocated(medias)) deallocate(medias)
  if (allocated(medianas)) deallocate(medianas)
  if (allocated(desvios)) deallocate(desvios)
  if (allocated(num_outliers)) deallocate(num_outliers)

  write(*,*) 'Listo! Resultados en ', trim(archivo_salida), ' y log en ', trim(archivo_log)

contains

  ! ---------------------------------------------------------
  ! Subrutina para leer archivo CSV en dos pasadas
  ! ---------------------------------------------------------
  subroutine leer_csv(nombre, datos, M, K, logname)
    implicit none
    character(len=*), intent(in) :: nombre, logname
    real, allocatable, intent(out) :: datos(:,:)
    integer, intent(out) :: M, K

    integer :: u, ios, filas, i, j, logu
    character(len=1000) :: linea, header
    integer :: lenlinea, start, coma
    character(len=64) :: token
    real :: valor
    real, parameter :: sentinel = huge(1.0)

    ! iniciar variables
    M = 0; K = 0
    if (allocated(datos)) deallocate(datos)

    ! abrir archivo
    open(newunit=u, file=nombre, status='old', action='read', iostat=ios)
    if (ios /= 0) then
       open(newunit=logu, file=logname, position='append')
       write(logu,'(A)') 'ERROR: no se pudo abrir archivo '//trim(nombre)
       close(logu)
       return
    end if

    ! leer cabecera para saber columnas
    read(u,'(A)', iostat=ios) header
    if (ios /= 0) then
       close(u)
       return
    end if
    K = 1
    do i = 1, len_trim(header)
       if (header(i:i) == ',') K = K + 1
    end do

    ! contar filas
    filas = 0
    do
       read(u,'(A)', iostat=ios) linea
       if (ios /= 0) exit
       if (len_trim(linea) > 0) filas = filas + 1
    end do
    close(u)

    M = filas
    allocate(datos(M,K))
    datos = sentinel

    ! segunda pasada: llenar datos
    open(newunit=u, file=nombre, status='old', action='read')
    read(u,'(A)') header   ! saltar cabecera

    filas = 0
    do
       read(u,'(A)', iostat=ios) linea
       if (ios /= 0) exit
       if (len_trim(linea) == 0) cycle
       filas = filas + 1
       lenlinea = len_trim(linea)
       start = 1

       do j = 1, K
          if (start > lenlinea) then
             token = ''
          else
             coma = index(linea(start:lenlinea), ',')
             if (coma == 0) then
                token = adjustl(linea(start:lenlinea))
                start = lenlinea + 1
             else
                token = adjustl(linea(start:start+coma-2))
                start = start + coma
             end if
          end if

          call leer_valor(trim(token), valor, logname, filas, j, sentinel)
          datos(filas,j) = valor
       end do
    end do
    close(u)
  end subroutine leer_csv

  ! ---------------------------------------------------------
  ! Leer un valor de texto, convertir a número y manejar errores
  ! ---------------------------------------------------------
  subroutine leer_valor(str, val, logname, fila, col, sentinel)
    implicit none
    character(len=*), intent(in) :: str, logname
    real, intent(out) :: val
    integer, intent(in) :: fila, col
    real, intent(in) :: sentinel
    integer :: ios, logu

    if (len_trim(str) == 0) then
       val = sentinel
       open(newunit=logu, file=logname, position='append')
       write(logu,'(A,I0,A,I0)') 'WARNING: valor vacío en fila ', fila, ', col ', col
       close(logu)
       return
    end if

    read(str,*,iostat=ios) val
    if (ios /= 0) then
       val = sentinel
       open(newunit=logu, file=logname, position='append')
       write(logu,'(A,I0,A,I0,A,A)') 'ERROR: valor no numérico en fila ', fila, ', col ', col, ' -> ', trim(str)
       close(logu)
    end if
  end subroutine leer_valor

  ! ---------------------------------------------------------
  ! Calcular estadísticas por columna
  ! ---------------------------------------------------------
  subroutine calcular_estadisticas(datos, M, K, medias, medianas, desvios, outliers)
    implicit none
    real, intent(in) :: datos(:,:)
    integer, intent(in) :: M, K
    real, intent(out) :: medias(:), medianas(:), desvios(:)
    integer, intent(out) :: outliers(:)

    integer :: i, j, n
    real, allocatable :: col(:), tmp(:)
    real :: suma, prom, sd
    real, parameter :: sentinel = huge(1.0)

    do j = 1, K
       ! recolectar valores válidos
       n = 0
       allocate(col(M))
       do i = 1, M
          if (datos(i,j) /= sentinel) then
             n = n + 1
             col(n) = datos(i,j)
          end if
       end do

       if (n == 0) then
          medias(j) = 0.0; medianas(j) = 0.0; desvios(j) = 0.0; outliers(j) = 0
          deallocate(col)
          cycle
       end if

       ! media
       suma = sum(col(1:n))
       prom = suma / real(n)
       medias(j) = prom

       ! desviación estándar muestral
       if (n > 1) then
          sd = sqrt(sum((col(1:n)-prom)**2) / real(n-1))
       else
          sd = 0.0
       end if
       desvios(j) = sd

       ! mediana
       allocate(tmp(n))
       tmp = col(1:n)
       call quicksort(tmp,1,n)
       if (mod(n,2) == 1) then
          medianas(j) = tmp((n+1)/2)
       else
          medianas(j) = 0.5*(tmp(n/2)+tmp(n/2+1))
       end if
       deallocate(tmp)

       ! outliers
       outliers(j) = 0
       if (sd > 0.0) then
          do i = 1, n
             if (abs((col(i)-prom)/sd) >= 3.0) outliers(j) = outliers(j) + 1
          end do
       end if

       deallocate(col)
    end do
  end subroutine calcular_estadisticas

  ! ---------------------------------------------------------
  ! Guardar resultados en archivo CSV
  ! ---------------------------------------------------------
  subroutine escribir_resultados(nombre, K, medias, medianas, desvios, outliers)
    implicit none
    character(len=*), intent(in) :: nombre
    integer, intent(in) :: K
    real, intent(in) :: medias(:), medianas(:), desvios(:)
    integer, intent(in) :: outliers(:)

    integer :: u, j
    open(newunit=u, file=nombre, status='replace', action='write')
    write(u,'(A)') 'variable,media,mediana,desvio,outliers'
    do j = 1, K
       write(u,'(A,I0,A,F10.4,A,F10.4,A,F10.4,A,I0)') &
            'col', j, ',', medias(j), ',', medianas(j), ',', desvios(j), ',', outliers(j)
    end do
    close(u)
  end subroutine escribir_resultados

  ! ---------------------------------------------------------
  ! Quicksort simple para ordenar
  ! ---------------------------------------------------------
  recursive subroutine quicksort(a, left, right)
    implicit none
    real, intent(inout) :: a(:)
    integer, intent(in) :: left, right
    integer :: i, j
    real :: pivote, tmp

    if (left >= right) return
    pivote = a((left+right)/2)
    i = left; j = right
    do
       do while (a(i) < pivote); i = i+1; end do
       do while (a(j) > pivote); j = j-1; end do
       if (i <= j) then
          tmp = a(i); a(i) = a(j); a(j) = tmp
          i = i+1; j = j-1
       end if
       if (i > j) exit
    end do
    if (left < j) call quicksort(a,left,j)
    if (i < right) call quicksort(a,i,right)
  end subroutine quicksort

end program compute_stats
