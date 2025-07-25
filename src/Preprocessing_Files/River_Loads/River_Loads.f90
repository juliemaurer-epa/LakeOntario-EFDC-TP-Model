PROGRAM RIVER_LOADS

USE NETCDF

IMPLICIT NONE

CHARACTER(LEN=35) :: TP_FILE   = 'TP_RiverLoads_ZeroGR_2018.nc'

INTEGER(kind=4) :: I, J, K
INTEGER(kind=4), PARAMETER :: NRIVERS = 9
INTEGER(kind=4), PARAMETER :: NDAYS = 365
INTEGER(kind=4), DIMENSION(NRIVERS) :: NR_ARR
INTEGER(kind=4) :: REC_NUM = 0
INTEGER(kind=4) :: STATUS, ISTATUS
INTEGER(kind=4) :: IDAY, IMONTH, IYEAR
INTEGER(kind=4) :: NCID_TP, NR_ID, TM_ID, NRVar_ID, TimeVar_ID, TP_ID

CHARACTER(LEN=4) :: YEAR = '2018'
CHARACTER(LEN=50), DIMENSION(NRIVERS) :: RIVER_FILE

! REAL :: TIME_INIT = 1356998400    ! 01/01/2013 midnight
REAL :: TIME_INIT =  1514764800   ! 01/01/2018 midnight
REAL :: TIME_SECONDS = 0
REAL :: TP_CONC, LOAD, FLOW
REAL, DIMENSION(NRIVERS, NDAYS) :: TP
REAL, DIMENSION(NRIVERS, NDAYS) :: TP1
REAL, DIMENSION(NRIVERS) :: TP_Daily_Load
REAL :: DOLAN_LOAD 

DO I = 1, NRIVERS
   NR_ARR(I) = I
ENDDO

! RIVER_FILE(1) = YEAR // '/' // 'Niagara_River_IntNOTL_2018.txt'
! RIVER_FILE(1) = YEAR // '/' // 'Niagara_River_USGS_2018.txt'
! RIVER_FILE(1) = YEAR // '/' // 'Niagara_River_2018.txt'
RIVER_FILE(2) = YEAR // '/' // 'EighteenMile_River_2018.txt'
RIVER_FILE(3) = YEAR // '/' // 'OakOrchard_River_2018.txt'
RIVER_FILE(4) = YEAR // '/' // 'Genessee_River_2018.txt'
RIVER_FILE(5) = YEAR // '/' // 'Oswego_River_2018.txt'
RIVER_FILE(6) = YEAR // '/' // 'Black_River_2018.txt'
RIVER_FILE(7) = YEAR // '/' // 'Twelve_Mile_Creek_River_2018.txt'
RIVER_FILE(8) = YEAR // '/' // 'Trent_River_2018.txt'
RIVER_FILE(9) = YEAR // '/' // 'Humber_River_2018.txt'

DOLAN_LOAD = 1743.50 / 365.0  ! Metric tons

DO J = 1, NDAYS
!   TP(1,J) = 0.0
   TP(1,J) = DOLAN_LOAD * 1.0E+03 / 8.64E+04 
ENDDO

DO I = 2, NRIVERS
   PRINT*, RIVER_FILE(I)
   OPEN(UNIT=10, FILE = RIVER_FILE(I), ACTION = 'READ')
   DO J = 1, NDAYS
      READ(10,*) IMONTH, IDAY, IYEAR, FLOW, TP_CONC, LOAD
!      TP(I,J) = LOAD * 1.0E+03 / 8.64E+04
!      PRINT*, IMONTH, IDAY, IYEAR, FLOW, TP_CONC, LOAD
!      IF (I == 1) THEN  ! USGS and Integrated NOTL loads are in kg
!          TP(I,J) = LOAD / 8.64E+04
      IF (I == 4) THEN
          TP(I,J) = 0.0 * LOAD * 1.0E+03 / 8.64E+04
      ELSE IF (I == 7) THEN
          TP(I,J) = 0.0 * LOAD * 1.0E+03 / 8.64E+04
      ELSE
          TP(I,J) = LOAD * 1.0E+03 / 8.64E+04
      ENDIF
   ENDDO
   CLOSE(UNIT=10)
ENDDO


! TP(:,91:120) = TP(:,121:150)

! TP1 = TP
! TP1(:,91:181) = TP(:,182:272)
! TP1(:,182:273) = TP(:,91:182)
! TP1(:,152:273) = TP(:,91:212)
! TP = TP1


!-----------------------------------------------------------------------------
!   Create NetCDF files
!-----------------------------------------------------------------------------
STATUS = NF90_CREATE(PATH = TP_FILE, CMODE = NF90_CLOBBER, NCID = NCID_TP)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!-----------------------------------------------------------------------------
!   Define dimensions
!-----------------------------------------------------------------------------
STATUS = NF90_DEF_DIM(NCID_TP, "Number_Rivers", NRIVERS, NR_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_DIM(NCID_TP, "Time", NF90_UNLIMITED, TM_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!-----------------------------------------------------------------------------
!   Define variables
!-----------------------------------------------------------------------------
STATUS = NF90_DEF_VAR(NCID_TP, "Number_Rivers", NF90_INT, (/ NR_ID /), NRVar_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID_TP, "Time", NF90_DOUBLE, (/ TM_ID /), TimeVar_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID_TP, "TP", NF90_FLOAT, (/ NR_ID, TM_ID /), TP_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
!   Variable Attributes
!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID_TP, TimeVar_ID, "long_name", "Simulation time")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID_TP, TimeVar_ID, "units", "Seconds since 01-01-1970.")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID_TP, NRVar_ID, "long_name", "Number of rivers")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID_TP, NRVar_ID, "units", "None")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID_TP, TP_ID, "long_name", "TP river loads")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID_TP, TP_ID, "units", "kg/s")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
!   End definitions
!------------------------------------------------------------------------------
STATUS = NF90_ENDDEF(NCID_TP)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
!   Assign values
!------------------------------------------------------------------------------
STATUS = NF90_PUT_VAR(NCID_TP, NRVar_ID, NR_ARR)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

REC_NUM = 0
TIME_SECONDS = TIME_INIT - 86400

DO J = 1, NDAYS

   REC_NUM = REC_NUM + 1
   TIME_SECONDS = TIME_SECONDS + 86400

   DO I = 1, NRIVERS
      TP_Daily_Load(I) = TP(I,J)
   ENDDO

   STATUS = NF90_PUT_VAR(NCID_TP, TimeVar_ID, TIME_SECONDS, START = (/ REC_NUM /))
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

   STATUS = NF90_PUT_VAR(NCID_TP, TP_ID, TP_Daily_Load, START = (/ 1, REC_NUM /), COUNT = (/ NRIVERS, 1 /))
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

ENDDO

STATUS = NF90_CLOSE(NCID_TP)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

END PROGRAM
