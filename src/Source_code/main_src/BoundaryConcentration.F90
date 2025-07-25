      MODULE BoundaryConcentration 

      USE netcdf_utils

      IMPLICIT NONE

      real,allocatable,save :: BC1(:) !BCvar1
      real,allocatable,save :: BC2(:) !BCvar2
      real,allocatable,save :: BC3(:) !BCvar3
      real,allocatable,save :: BC4(:) !BCvar4
      real,allocatable,save :: BC5(:) !BCvar5
      real,allocatable,save :: BC6(:) !BCvar6
      real,allocatable,save :: BC7(:) !BCvar7
      real,allocatable,save :: BC8(:) !BCvar8
      real,allocatable,save :: BC9(:) !BCvar9


      real,allocatable,save :: BC1A(:) !BCvar1
      real,allocatable,save :: BC2A(:) !BCvar2
      real,allocatable,save :: BC3A(:) !BCvar3
      real,allocatable,save :: BC4A(:) !BCvar4
      real,allocatable,save :: BC5A(:) !BCvar5
      real,allocatable,save :: BC6A(:) !BCvar6
      real,allocatable,save :: BC7A(:) !BCvar7
      real,allocatable,save :: BC8A(:) !BCvar8
      real,allocatable,save :: BC9A(:) !BCvar9
      real,allocatable,save :: BC1B(:) !BCvar1
      real,allocatable,save :: BC2B(:) !BCvar2
      real,allocatable,save :: BC3B(:) !BCvar3
      real,allocatable,save :: BC4B(:) !BCvar4
      real,allocatable,save :: BC5B(:) !BCvar5
      real,allocatable,save :: BC6B(:) !BCvar6
      real,allocatable,save :: BC7B(:) !BCvar7
      real,allocatable,save :: BC8B(:) !BCvar8
      real,allocatable,save :: BC9B(:) !BCvar9
      
      integer, allocatable, save :: bcIJ(:,:)  ! Grid cell indices of boundary concentration locations

      type(netCDF_file) :: boundaryconcentration_info(9)  !indices refer to order declared below in enum, 
      character(len=200) :: netcdf_boundaryconcentration_fileNames(9)  !holds filenames of hydro netcdf data files
      integer, dimension(9) :: startBcIndex ! holds the last time index from netcdf file used for each boundaryconcentration variable
                                            ! used as starting point for next lookup

      integer, parameter :: eBC1 = 1    !BCvar1
      integer, parameter :: eBC2 = 2    !BCvar2
      integer, parameter :: eBC3 = 3    !BCvar3
      integer, parameter :: eBC4 = 4    !BCvar4
      integer, parameter :: eBC5 = 5    !BCvar5
      integer, parameter :: eBC6 = 6    !BCvar6
      integer, parameter :: eBC7 = 7    !BCvar7
      integer, parameter :: eBC8 = 8    !BCvar8
      integer, parameter :: eBC9 = 9    !BCvar9

      integer, save :: fBCv, lBCv  ! looping index of FirstBoundaryConcentrationVar and LastBoundaryConcentrationVar

      integer(kind=8), save :: bc_tc1, bc_tc2 

      contains

      Subroutine Allocate_BoundaryConcentrations()

      USE Model_dim
      USE Fill_Value

      IMPLICIT NONE

      ALLOCATE(BC1(nBC))
      ALLOCATE(BC2(nBC))
      ALLOCATE(BC3(nBC))
      ALLOCATE(BC4(nBC))
      ALLOCATE(BC5(nBC))
      ALLOCATE(BC6(nBC))
      ALLOCATE(BC7(nBC))
      ALLOCATE(BC8(nBC))
      ALLOCATE(BC9(nBC))

      ALLOCATE(bcIJ(nBC,2))

      !Fill values for netCDF
      BC1 = fill(0)  
      BC2 = fill(0)
      BC3 = fill(0)
      BC4 = fill(0)
      BC5 = fill(0)
      BC6 = fill(0)
      BC7 = fill(0)
      BC8 = fill(0)
      BC9 = fill(0)

      return

      End Subroutine Allocate_BoundaryConcentrations 


      Subroutine Init_BoundaryConcentration_NetCDF()
      
      USE Model_dim

      IMPLICIT NONE

      integer :: i
      character(len=200) :: textfile

      !Set filenames for netCDF
      if (Which_gridio .eq. 1) then 
         write(netcdf_boundaryconcentration_fileNames(eBC1), '(A, A)') trim(DATADIR), '/INPUT/TN_BoundaryConcentrations.nc'
         write(netcdf_boundaryconcentration_fileNames(eBC2), '(A, A)') trim(DATADIR), '/INPUT/NO3_BoundaryConcentrations.nc'
         write(netcdf_boundaryconcentration_fileNames(eBC3), '(A, A)') trim(DATADIR), '/INPUT/NH4_BoundaryConcentrations.nc'
         write(netcdf_boundaryconcentration_fileNames(eBC4), '(A, A)') trim(DATADIR), '/INPUT/DON_BoundaryConcentrations.nc'
         write(netcdf_boundaryconcentration_fileNames(eBC5), '(A, A)') trim(DATADIR), '/INPUT/TP_BoundaryConcentrations.nc'
         write(netcdf_boundaryconcentration_fileNames(eBC6), '(A, A)') trim(DATADIR), '/INPUT/DIP_BoundaryConcentrations.nc'
         write(netcdf_boundaryconcentration_fileNames(eBC7), '(A, A)') trim(DATADIR), '/INPUT/DOP_BoundaryConcentrations.nc'
         write(netcdf_boundaryconcentration_fileNames(eBC8), '(A, A)') trim(DATADIR), '/INPUT/BOD_BoundaryConcentrations.nc'
         write(netcdf_boundaryconcentration_fileNames(eBC9), '(A, A)') trim(DATADIR), '/INPUT/DO_BoundaryConcentrations.nc'
      else if (Which_gridio .eq. 2) then
!         write(netcdf_fileNames(eSal), '(A, A)') trim(DATADIR), '/INPUT/S.nc'
!         write(netcdf_fileNames(eTemp), '(A, A)') trim(DATADIR), '/INPUT/T.nc'
!         write(netcdf_fileNames(eUx), '(A, A)') trim(DATADIR), '/INPUT/U.nc'
!         write(netcdf_fileNames(eVx), '(A, A)') trim(DATADIR), '/INPUT/V.nc'
!         write(netcdf_fileNames(eWx), '(A, A)') trim(DATADIR), '/INPUT/W.nc'
!         write(netcdf_fileNames(eKh), '(A, A)') trim(DATADIR), '/INPUT/KH.nc'
!         write(netcdf_fileNames(eE), '(A, A)') trim(DATADIR), '/INPUT/E.nc'
      else if (Which_gridio .eq. 3) then
!         write(netcdf_fileNames(eSal), '(A, A)') trim(DATADIR), 'NA'  !No salinity input
!         write(netcdf_fileNames(eTemp), '(A, A)') trim(DATADIR), '/INPUT/T.nc'
!         write(netcdf_fileNames(eUx), '(A, A)') trim(DATADIR), '/INPUT/U.nc'
!         write(netcdf_fileNames(eVx), '(A, A)') trim(DATADIR), '/INPUT/V.nc'
!         write(netcdf_fileNames(eWx), '(A, A)') trim(DATADIR), '/INPUT/W.nc'
!         write(netcdf_fileNames(eKh), '(A, A)') trim(DATADIR), '/INPUT/Kh.nc'
!         write(netcdf_fileNames(eE), '(A, A)') trim(DATADIR), '/INPUT/E.nc'
!         write(netcdf_fileNames(eWind), '(A, A)') trim(DATADIR), '/INPUT/Wind.nc'
!         write(netcdf_fileNames(eRad), '(A, A)') trim(DATADIR), '/INPUT/Rad.nc'
      endif


      if (Which_gridio .eq. 1 .OR. Which_gridio .eq. 2) then  !EFDC and NCOM do not use Wind or Rad from NetCDF
         fBCv = 1;
         lBCv = 9;
      else if (Which_gridio .eq. 3) then  !POM does not use Salinity
!         fHv = 2;
!         lHv = 9
      endif
      do i = fBCv, lBCv
        call open_netcdf(netcdf_boundaryconcentration_fileNames(i), 0, boundaryconcentration_info(i)%ncid)
        boundaryconcentration_info(i)%fileName = netcdf_boundaryconcentration_fileNames(i)
        call init_info(boundaryconcentration_info(i))
#ifdef DEBUG
call report_info(boundaryconcentration_info(i))
#endif
      enddo

      startBcIndex = 1

      write(textfile,'(A, A)') trim(DATADIR),'/BCindices.dat'
      open(19, file=textfile, status='old')
      read(19,*)    ! I and J indices of boundary concentration locations.
      do i = 1, nBC
         read(19,*) bcIJ(i,1:2)
      enddo
      close(19)

      bc_tc1=0
      bc_tc2=0
      
      
      End Subroutine Init_BoundaryConcentration_NetCDF


      Subroutine Close_BoundaryConcentration_NetCDF()

      IMPLICIT NONE
      integer :: i

      do i = fBCv, lBCv
        call close_netcdf(boundaryconcentration_info(i)%ncid)
      enddo

      End Subroutine Close_BoundaryConcentration_NetCDF


      End Module BoundaryConcentration 
