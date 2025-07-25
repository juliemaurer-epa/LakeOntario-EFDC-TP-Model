! This program reads dxdy.dat and prints out dy values for selected
! locations.
PROGRAM Read_DxDy

IMPLICIT NONE

INTEGER(kind=4), PARAMETER :: im = 256
INTEGER(kind=4), PARAMETER :: jm = 133
INTEGER(kind=4) :: i, j, k
REAL, DIMENSION(im, jm) :: dx, dy
CHARACTER(LEN=80) :: filename, output_file

filename = '/work/GLFBREEZ/Lake_Ontario/CGEM/CGEM/data/Lake_Ontario/dxdy.dat'
output_file = '/work/GLFBREEZ/Lake_Ontario/Areas/dx_dy.csv'

open(19, file = filename, status = 'old')

read(19,*) !dx
do j = 1, jm
   read(19,*) dx(:,j)
!   write(6,*) j,dx(:,j)
enddo

read(19,*) !dy
do j = 1, jm
   read(19,*) dy(:,j)
!   write(6,*) j,dy(:,j)
enddo

! write(6,*), dy(150,52), dy(150,54), dy(171,14)
close(19)

open(20, file = output_file, status = 'new', action = 'write')

write(20,*) 'i, j, dx, dy'

do i = 1, im
   do j = 1, jm
      write(20,*) i, ',', j, ',', dx(i,j), ',', dy(i,j)
   enddo
enddo

close(20)

END PROGRAM
