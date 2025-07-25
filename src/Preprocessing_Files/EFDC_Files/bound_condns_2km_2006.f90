PROGRAM BOUND_CONDITIONS
!------------------------------------------------------------------------------
!-
!-  $Id: $
!-  $Locker: $
!-
!-  Description :  This program creates a NetCDF interface for the 
!-                 Gulf of Mexico boundary conditions.
!-
!-  Calls:  HANDLE_ERR    
!-
!-  Created: 12/17/10  W. Melendez
!-
!- Revised:  7/1/2011  T. Feist - added descriptive title and year info
!-           7/26/2011  T. Feist - corrected typos
!-           11/15/2013  T. Feist - updated for 6km expanded grid
!-           02/11/2014  W. Melendez: Updated mapping file's pathname.
!-           06/10/2014  W. Melendez: Updated code for 2km grid.
!-           05/31/2016  W. Melendez: Added internal N and P boundary
!-                                    concentrations.
!- 
!------------------------------------------------------------------------------

USE NETCDF

IMPLICIT NONE

INTEGER, PARAMETER :: NUM_RECORDS = 4         ! Number of updates
INTEGER, PARAMETER :: NUM_BOUNDARIES = 10660  ! Number of boundaries
INTEGER, PARAMETER :: NQF = 1641156           ! Number of flow faces, u+v+w, no shoreline
INTEGER, PARAMETER :: NSB = 27944             ! Number of surficial grid cells
INTEGER, PARAMETER :: NL = 20                 ! Number of sigma layers

REAL, PARAMETER :: ANCP = 0.1509           ! N:C ratio
REAL, PARAMETER :: APCP = 0.00943          ! P:C ratio


CHARACTER(LEN=1) :: FFDIR
CHARACTER(LEN=3) :: CDIR
CHARACTER(LEN=1), DIMENSION(1:NUM_BOUNDARIES) :: FLOW_FACE_DIR
CHARACTER(LEN=1), DIMENSION(1:NUM_BOUNDARIES) :: FF_ORIENTATION

CHARACTER(LEN=24) :: CTIME
CHARACTER(LEN=24) :: TIMESTAMP
CHARACTER(LEN=20) :: USER_NAME    = 'Wilson Melendez'
CHARACTER(LEN=20) :: PROGRAM_NAME = 'bound_condns_2km_2006.f90'
CHARACTER(LEN=120) :: HISTORY_STRING
CHARACTER(LEN=132) :: CMESS
CHARACTER(LEN=26) :: CNAME = 'bound_conditions'

CHARACTER(LEN=30) :: INPUT_DOC    = 'DOC_BC_2006.txt'
CHARACTER(LEN=30) :: INPUT_DON    = 'DON_BC_2006.txt'
CHARACTER(LEN=30) :: INPUT_DOP    = 'DOP_BC_2006.txt'
CHARACTER(LEN=30) :: INPUT_LOC    = 'LOC_BC_2006.txt'
CHARACTER(LEN=30) :: INPUT_LON    = 'LON_BC_2006.txt'
CHARACTER(LEN=30) :: INPUT_LOP    = 'LOP_BC_2006.txt'
CHARACTER(LEN=30) :: INPUT_NH4    = 'NH4N_BC_2006.txt'
CHARACTER(LEN=30) :: INPUT_NOX    = 'NOX_BC_2006.txt'
CHARACTER(LEN=30) :: INPUT_PHYTO1 = 'PHYTO1_BC_2006.txt'
CHARACTER(LEN=30) :: INPUT_PHYTO2 = 'PHYTO2_BC_2006.txt'
CHARACTER(LEN=30) :: INPUT_ROC    = 'ROC_BC_2006.txt'
CHARACTER(LEN=30) :: INPUT_RON    = 'RON_BC_2006.txt'
CHARACTER(LEN=30) :: INPUT_ROP    = 'ROP_BC_2006.txt'
CHARACTER(LEN=30) :: INPUT_SA     = 'SA_BC_2006.txt'
CHARACTER(LEN=30) :: INPUT_SRP    = 'SRP_BC_2006.txt'
CHARACTER(LEN=30) :: INPUT_SU     = 'SU_BC_2006.txt'
CHARACTER(LEN=30) :: INPUT_ZOO    = 'ZOO_BC_2006.txt'
CHARACTER(LEN=30) :: INPUT_DO2    = 'DO_BC_2006.txt'
CHARACTER(LEN=30) :: INPUT_SAL    = 'SAL_BC_2006.txt'
CHARACTER(LEN=30) :: INPUT_TRA    = 'TRACER_BC_2006.txt'

CHARACTER(LEN=30) :: OUTPUT_FILE    = 'Bound_Condtns_2km_2006.nc'

CHARACTER(LEN=60) :: FILE_DESC = 'Base boundary conditions for 2006 2km grid'

CHARACTER(LEN=80) :: INPUT_MAP = '/work/GLFBREEZ/wmelende/Mapping_Files/map_2km.dat'

INTEGER :: STIME
INTEGER :: TIME
INTEGER :: IYR, IMON, IDAY
INTEGER :: JDN
INTEGER :: NCID, STATUS, ISTATUS

INTEGER :: NUMOB_DIM_ID

INTEGER :: REC_DIM_ID

INTEGER :: ICELL
INTEGER :: ISTAT
INTEGER :: IREC, I, J
INTEGER :: F, SB
INTEGER :: NUM_OPEN_BOUNDARIES

INTEGER :: DOC_ID
INTEGER :: DIA_ID
INTEGER :: DIAN_ID
INTEGER :: DIAP_ID
INTEGER :: GRE_ID
INTEGER :: GREN_ID
INTEGER :: GREP_ID
INTEGER :: ZOO_ID
INTEGER :: LOC_ID
INTEGER :: ROC_ID
INTEGER :: SRP_ID
INTEGER :: DOP_ID
INTEGER :: LOP_ID
INTEGER :: ROP_ID
INTEGER :: NH4_ID
INTEGER :: NO3_ID
INTEGER :: DON_ID
INTEGER :: LON_ID
INTEGER :: RON_ID
INTEGER :: SA_ID
INTEGER :: SU_ID
INTEGER :: SAL_ID
INTEGER :: DO2_ID
INTEGER :: TRACER_ID

INTEGER :: TIME_BC_ID
INTEGER :: FF_ID

INTEGER, DIMENSION(:), ALLOCATABLE :: FLOW_FACES
INTEGER, DIMENSION(1:NUM_BOUNDARIES) :: BCELLS
INTEGER, DIMENSION(1:NUM_BOUNDARIES) :: FF_INDEX
INTEGER, DIMENSION(1:NUM_BOUNDARIES) :: INDEX_BC
INTEGER, DIMENSION(1:NUM_BOUNDARIES) :: FF_CELL
INTEGER, DIMENSION(1:NQF) :: QD
INTEGER, DIMENSION(0:NQF) :: ILB, IB, JB, JRB
INTEGER, DIMENSION(1:NQF) :: LAYER
INTEGER, DIMENSION(1:NSB) :: NVF
INTEGER, DIMENSION(0:NL,1:NSB) :: VFN

REAL :: CONC

REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_DOC
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_DIA
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_GRE
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_ZOO
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_LOC
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_ROC
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_SRP
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_DOP
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_LOP
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_ROP
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_NH4
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_NO3
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_DON
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_LON
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_RON
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_SA
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_SU
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_SAL
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_DO2
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_TRACER

REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_DIAN  
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_DIAP
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_GREN
REAL, DIMENSION(1:NUM_BOUNDARIES,1:NUM_RECORDS) :: CONC_GREP

REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_DOC
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_DIA
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_GRE
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_ZOO
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_LOC
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_ROC
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_SRP
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_DOP
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_LOP
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_ROP
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_NH4
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_NO3
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_DON
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_LON
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_RON
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_SA
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_SU
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_SAL
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_DO2
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_TRACER

REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_DIAN
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_DIAP
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_GREN
REAL, DIMENSION(:,:), ALLOCATABLE :: CONC_OPENB_GREP

DOUBLE PRECISION, DIMENSION(1:NUM_RECORDS) :: TIME_BC

DOUBLE PRECISION :: DJDN             ! Julian day number

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!    DOC
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_DOC, STATUS='OLD', ACTION='READ')

I = 0
IREC = 0
DO
   READ(10,FMT=*,IOSTAT=ISTAT) ICELL, FFDIR, CDIR, IMON, IDAY, IYR, CONC
   IF (ISTAT < 0) EXIT

   I = I + 1   
   IF (I <= NUM_BOUNDARIES) THEN 
       BCELLS(I) = ICELL
       FLOW_FACE_DIR(I) = FFDIR
   ENDIF

   IF (I == (1 + IREC * NUM_BOUNDARIES)) THEN 
       IREC = IREC + 1
       J = 0
       CALL YMD_TO_JULIAN(IYR,IMON,IDAY,JDN)
       TIME_BC(IREC) = JDN - 5.0D-01
   ENDIF
   J = J + 1
   CONC_DOC(J,IREC) = CONC
ENDDO

CLOSE(UNIT=10)

!------------------------------------------------------------------------------
!   DON
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_DON, STATUS='OLD', ACTION='READ')

I = 0
IREC = 0
DO
   READ(10,FMT=*,IOSTAT=ISTAT) ICELL, FFDIR, CDIR, IMON, IDAY, IYR, CONC 
   IF (ISTAT < 0) EXIT
   I = I + 1  
   IF (I == (1 + IREC * NUM_BOUNDARIES)) THEN 
       IREC = IREC + 1
       J = 0
   ENDIF
   J = J + 1
   CONC_DON(J,IREC) = CONC
ENDDO

CLOSE(UNIT=10)

!------------------------------------------------------------------------------
!    DOP
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_DOP, STATUS='OLD', ACTION='READ')

I = 0
IREC = 0
DO
   READ(10,FMT=*,IOSTAT=ISTAT) ICELL, FFDIR, CDIR, IMON, IDAY, IYR, CONC 
   IF (ISTAT < 0) EXIT
   I = I + 1  
   IF (I == (1 + IREC * NUM_BOUNDARIES)) THEN 
       IREC = IREC + 1
       J = 0
   ENDIF
   J = J + 1
   CONC_DOP(J,IREC) = CONC
ENDDO

CLOSE(UNIT=10)

!------------------------------------------------------------------------------
!  LOC
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_LOC, STATUS='OLD', ACTION='READ')

I = 0
IREC = 0
DO
   READ(10,FMT=*,IOSTAT=ISTAT) ICELL, FFDIR, CDIR, IMON, IDAY, IYR, CONC 
   IF (ISTAT < 0) EXIT
   I = I + 1  
   IF (I == (1 + IREC * NUM_BOUNDARIES)) THEN 
       IREC = IREC + 1
       J = 0
   ENDIF
   J = J + 1
   CONC_LOC(J,IREC) = CONC
ENDDO

CLOSE(UNIT=10)

!------------------------------------------------------------------------------
!  LON
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_LON, STATUS='OLD', ACTION='READ')

I = 0
IREC = 0
DO
   READ(10,FMT=*,IOSTAT=ISTAT) ICELL, FFDIR, CDIR, IMON, IDAY, IYR, CONC 
   IF (ISTAT < 0) EXIT
   I = I + 1  
   IF (I == (1 + IREC * NUM_BOUNDARIES)) THEN 
       IREC = IREC + 1
       J = 0
   ENDIF
   J = J + 1
   CONC_LON(J,IREC) = CONC
ENDDO

CLOSE(UNIT=10)

!------------------------------------------------------------------------------
!  LOP
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_LOP, STATUS='OLD', ACTION='READ')

I = 0
IREC = 0
DO
   READ(10,FMT=*,IOSTAT=ISTAT) ICELL, FFDIR, CDIR, IMON, IDAY, IYR, CONC 
   IF (ISTAT < 0) EXIT
   I = I + 1  
   IF (I == (1 + IREC * NUM_BOUNDARIES)) THEN 
       IREC = IREC + 1
       J = 0
   ENDIF
   J = J + 1
   CONC_LOP(J,IREC) = CONC
ENDDO

CLOSE(UNIT=10)

!------------------------------------------------------------------------------
!  NH4
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_NH4, STATUS='OLD', ACTION='READ')

I = 0
IREC = 0
DO
   READ(10,FMT=*,IOSTAT=ISTAT) ICELL, FFDIR, CDIR, IMON, IDAY, IYR, CONC 
   IF (ISTAT < 0) EXIT
   I = I + 1  
   IF (I == (1 + IREC * NUM_BOUNDARIES)) THEN 
       IREC = IREC + 1
       J = 0
   ENDIF
   J = J + 1
   CONC_NH4(J,IREC) = CONC
ENDDO

CLOSE(UNIT=10)

!------------------------------------------------------------------------------
!   NO3
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_NOX, STATUS='OLD', ACTION='READ')

I = 0
IREC = 0
DO
   READ(10,FMT=*,IOSTAT=ISTAT) ICELL, FFDIR, CDIR, IMON, IDAY, IYR, CONC 
   IF (ISTAT < 0) EXIT
   I = I + 1  
   IF (I == (1 + IREC * NUM_BOUNDARIES)) THEN 
       IREC = IREC + 1
       J = 0
   ENDIF
   J = J + 1
   CONC_NO3(J,IREC) = CONC
ENDDO

CLOSE(UNIT=10)

!------------------------------------------------------------------------------
!   DIA
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_PHYTO1, STATUS='OLD', ACTION='READ')

I = 0
IREC = 0
DO
   READ(10,FMT=*,IOSTAT=ISTAT) ICELL, FFDIR, CDIR, IMON, IDAY, IYR, CONC 
   IF (ISTAT < 0) EXIT
   I = I + 1  
   IF (I == (1 + IREC * NUM_BOUNDARIES)) THEN 
       IREC = IREC + 1
       J = 0
   ENDIF
   J = J + 1
   CONC_DIA(J,IREC) = CONC
   CONC_DIAN(J,IREC) = CONC * ANCP
   CONC_DIAP(J,IREC) = CONC * APCP
ENDDO

CLOSE(UNIT=10)

!------------------------------------------------------------------------------
!   GRE
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_PHYTO2, STATUS='OLD', ACTION='READ')

I = 0
IREC = 0
DO
   READ(10,FMT=*,IOSTAT=ISTAT) ICELL, FFDIR, CDIR, IMON, IDAY, IYR, CONC 
   IF (ISTAT < 0) EXIT
   I = I + 1  
   IF (I == (1 + IREC * NUM_BOUNDARIES)) THEN 
       IREC = IREC + 1
       J = 0
   ENDIF
   J = J + 1
   CONC_GRE(J,IREC) = CONC
   CONC_GREN(J,IREC) = CONC * ANCP
   CONC_GREP(J,IREC) = CONC * APCP
ENDDO

CLOSE(UNIT=10)

!------------------------------------------------------------------------------
!  ROC
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_ROC, STATUS='OLD', ACTION='READ')

I = 0
IREC = 0
DO
   READ(10,FMT=*,IOSTAT=ISTAT) ICELL, FFDIR, CDIR, IMON, IDAY, IYR, CONC 
   IF (ISTAT < 0) EXIT
   I = I + 1  
   IF (I == (1 + IREC * NUM_BOUNDARIES)) THEN 
       IREC = IREC + 1
       J = 0
   ENDIF
   J = J + 1
   CONC_ROC(J,IREC) = CONC
ENDDO

CLOSE(UNIT=10)

!------------------------------------------------------------------------------
!   RON
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_RON, STATUS='OLD', ACTION='READ')

I = 0
IREC = 0
DO
   READ(10,FMT=*,IOSTAT=ISTAT) ICELL, FFDIR, CDIR, IMON, IDAY, IYR, CONC 
   IF (ISTAT < 0) EXIT
   I = I + 1  
   IF (I == (1 + IREC * NUM_BOUNDARIES)) THEN 
       IREC = IREC + 1
       J = 0
   ENDIF
   J = J + 1
   CONC_RON(J,IREC) = CONC
ENDDO

CLOSE(UNIT=10)

!------------------------------------------------------------------------------
!   ROP
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_ROP, STATUS='OLD', ACTION='READ')

I = 0
IREC = 0
DO
   READ(10,FMT=*,IOSTAT=ISTAT) ICELL, FFDIR, CDIR, IMON, IDAY, IYR, CONC 
   IF (ISTAT < 0) EXIT
   I = I + 1  
   IF (I == (1 + IREC * NUM_BOUNDARIES)) THEN 
       IREC = IREC + 1
       J = 0
   ENDIF
   J = J + 1
   CONC_ROP(J,IREC) = CONC
ENDDO

CLOSE(UNIT=10)

!------------------------------------------------------------------------------
!   SA
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_SA, STATUS='OLD', ACTION='READ')

I = 0
IREC = 0
DO
   READ(10,FMT=*,IOSTAT=ISTAT) ICELL, FFDIR, CDIR, IMON, IDAY, IYR, CONC 
   IF (ISTAT < 0) EXIT
   I = I + 1  
   IF (I == (1 + IREC * NUM_BOUNDARIES)) THEN 
       IREC = IREC + 1
       J = 0
   ENDIF
   J = J + 1
   CONC_SA(J,IREC) = CONC
ENDDO

CLOSE(UNIT=10)

!------------------------------------------------------------------------------
!  SRP
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_SRP, STATUS='OLD', ACTION='READ')

I = 0
IREC = 0
DO
   READ(10,FMT=*,IOSTAT=ISTAT) ICELL, FFDIR, CDIR, IMON, IDAY, IYR, CONC 
   IF (ISTAT < 0) EXIT
   I = I + 1  
   IF (I == (1 + IREC * NUM_BOUNDARIES)) THEN 
       IREC = IREC + 1
       J = 0
   ENDIF
   J = J + 1
   CONC_SRP(J,IREC) = CONC
ENDDO

CLOSE(UNIT=10)

!------------------------------------------------------------------------------
!   SU
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_SU, STATUS='OLD', ACTION='READ')

I = 0
IREC = 0
DO
   READ(10,FMT=*,IOSTAT=ISTAT) ICELL, FFDIR, CDIR, IMON, IDAY, IYR, CONC 
   IF (ISTAT < 0) EXIT
   I = I + 1  
   IF (I == (1 + IREC * NUM_BOUNDARIES)) THEN 
       IREC = IREC + 1
       J = 0
   ENDIF
   J = J + 1
   CONC_SU(J,IREC) = CONC
ENDDO

CLOSE(UNIT=10)

!------------------------------------------------------------------------------
!   ZOOPLANKTON
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_ZOO, STATUS='OLD', ACTION='READ')

I = 0
IREC = 0
DO
   READ(10,FMT=*,IOSTAT=ISTAT) ICELL, FFDIR, CDIR, IMON, IDAY, IYR, CONC 
   IF (ISTAT < 0) EXIT
   I = I + 1  
   IF (I == (1 + IREC * NUM_BOUNDARIES)) THEN 
       IREC = IREC + 1
       J = 0
   ENDIF
   J = J + 1
   CONC_ZOO(J,IREC) = CONC
ENDDO

CLOSE(UNIT=10)

!------------------------------------------------------------------------------
!   Dissolved Oxygen
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_DO2, STATUS='OLD', ACTION='READ')

I = 0
IREC = 0
DO
   READ(10,FMT=*,IOSTAT=ISTAT) ICELL, FFDIR, CDIR, IMON, IDAY, IYR, CONC 
   IF (ISTAT < 0) EXIT
   I = I + 1  
   IF (I == (1 + IREC * NUM_BOUNDARIES)) THEN 
       IREC = IREC + 1
       J = 0
   ENDIF
   J = J + 1
   CONC_DO2(J,IREC) = CONC
ENDDO

CLOSE(UNIT=10)

!------------------------------------------------------------------------------
!   Salinity
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_SAL, STATUS='OLD', ACTION='READ')

I = 0
IREC = 0
DO
   READ(10,FMT=*,IOSTAT=ISTAT) ICELL, FFDIR, CDIR, IMON, IDAY, IYR, CONC 
   IF (ISTAT < 0) EXIT
   I = I + 1  
   IF (I == (1 + IREC * NUM_BOUNDARIES)) THEN 
       IREC = IREC + 1
       J = 0
   ENDIF
   J = J + 1
   CONC_SAL(J,IREC) = CONC
ENDDO

CLOSE(UNIT=10)

!------------------------------------------------------------------------------
!   Tracer
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_TRA, STATUS='OLD', ACTION='READ')

I = 0
IREC = 0
DO
   READ(10,FMT=*,IOSTAT=ISTAT) ICELL, FFDIR, CDIR, IMON, IDAY, IYR, CONC 
   IF (ISTAT < 0) EXIT
   I = I + 1  
   IF (I == (1 + IREC * NUM_BOUNDARIES)) THEN 
       IREC = IREC + 1
       J = 0
   ENDIF
   J = J + 1
   CONC_TRACER(J,IREC) = CONC
ENDDO

CLOSE(UNIT=10)


!------------------------------------------------------------------------------
!   Open map file
!------------------------------------------------------------------------------
OPEN(UNIT=10, FILE=INPUT_MAP, STATUS='OLD', ACTION='READ')

!------------------------------------------------------------------------------
!  Read in mapping information
!------------------------------------------------------------------------------
ILB(0) = 0 ; IB(0) = 0 ; JB(0) = 0 ; JRB(0) = 0

READ (10,1110) (QD(F), ILB(F), IB(F), JB(F), JRB(F), LAYER(F), F = 1, NQF)

READ (10,1027) (NVF(SB), SB = 1, NSB)

READ (10,1100)


DO SB = 1, NSB

   READ (10,1130) (VFN(F,SB), F = 1, NVF(SB))

ENDDO


1110 FORMAT(:/(8X,6I8))
1027 FORMAT(//(11X,8I8))
1100 FORMAT(/)
1120 FORMAT(/(8X,I8,I10))
1130 FORMAT(8X,10I8)

CLOSE(UNIT=10)


!------------------------------------------------------------------------------
!   Associate boundary cell numbers to flow face numbers
!------------------------------------------------------------------------------
FF_INDEX = 0; FF_CELL = 0; INDEX_BC = 0

FF_ORIENTATION = ' '

J = 0

DO F = 1, NQF
   IF (QD(F) == 1) THEN
       IF (ILB(F) == 0 .AND. IB(F) == 0) THEN
           J = J + 1
           FF_INDEX(J) = F
           FF_ORIENTATION(J) = 'w'
           FF_CELL(J) = JB(F)
       ENDIF
       IF (JB(F) == 0 .AND. JRB(F) == 0) THEN
           J = J + 1
           FF_INDEX(J) = F
           FF_ORIENTATION(J) = 'e'
           FF_CELL(J) = IB(F)
       ENDIF
   ELSEIF (QD(F) == 2) THEN
       IF (ILB(F) == 0 .AND. IB(F) == 0) THEN
           J = J + 1
           FF_INDEX(J) = F
           FF_ORIENTATION(J) = 's'
           FF_CELL(J) = JB(F)
       ENDIF
       IF (JB(F) == 0 .AND. JRB(F) == 0) THEN
           J = J + 1
           FF_INDEX(J) = F
           FF_ORIENTATION(J) = 'n'
           FF_CELL(J) = IB(F)
       ENDIF
   ELSE
       IF (QD(F) == 3) EXIT
   ENDIF
ENDDO

!------------------------------------------------------------------------------
!   Store the number of open boundaries.
!------------------------------------------------------------------------------
NUM_OPEN_BOUNDARIES = J


!------------------------------------------------------------------------------
!   Allocate array that stores the flow faces of the open boundaries. 
!------------------------------------------------------------------------------
ALLOCATE(FLOW_FACES(1:NUM_OPEN_BOUNDARIES),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    PRINT*, 'Cannot allocate memory for FLOW_FACES array.'
    STOP
ENDIF

DO J = 1, NUM_OPEN_BOUNDARIES

   DO I = 1, NUM_BOUNDARIES

      IF ( (BCELLS(I) == FF_CELL(J)) .AND.  &
          (FLOW_FACE_DIR(I) == FF_ORIENTATION(J)) ) THEN

          INDEX_BC(J) = I
          FLOW_FACES(J) = FF_INDEX(J)

          EXIT
      ENDIF

   ENDDO

ENDDO


!------------------------------------------------------------------------------
!   Allocate open-boundary concentration arrays
!------------------------------------------------------------------------------
ALLOCATE(CONC_OPENB_DOC(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_DOC.'
    CALL REPORT(001,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_DON(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_DON.'
    CALL REPORT(002,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_DOP(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_DOP.'
    CALL REPORT(003,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_LOC(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_LOC.'
    CALL REPORT(004,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_LON(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_LON.'
    CALL REPORT(005,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_LOP(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_LOP.'
    CALL REPORT(006,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_NH4(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_NH4.'
    CALL REPORT(007,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_NO3(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_NO3.'
    CALL REPORT(008,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_DIA(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_DIA.'
    CALL REPORT(009,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_DIAN(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_DIAN.'
    CALL REPORT(009,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_DIAP(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_DIAP.'
    CALL REPORT(009,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_GRE(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_GRE.'
    CALL REPORT(010,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_GREN(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_GREN.'
    CALL REPORT(010,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_GREP(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_GREP.'
    CALL REPORT(010,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_ROC(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_ROC.'
    CALL REPORT(011,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_RON(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_RON.'
    CALL REPORT(012,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_ROP(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_ROP.'
    CALL REPORT(013,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_SA(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_SA.'
    CALL REPORT(014,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_SRP(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_SRP.'
    CALL REPORT(015,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_SU(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_SU.'
    CALL REPORT(016,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_ZOO(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_ZOO.'
    CALL REPORT(017,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_SAL(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_SAL.'
    CALL REPORT(018,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_DO2(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_DO2.'
    CALL REPORT(019,'A',CNAME,CMESS)
ENDIF

ALLOCATE(CONC_OPENB_TRACER(1:NUM_OPEN_BOUNDARIES,1:NUM_RECORDS),STAT=ISTATUS)
IF (ISTATUS /= 0) THEN
    CMESS = 'Cannot allocate memory for CONC_OPENB_TRACER.'
    CALL REPORT(020,'A',CNAME,CMESS)
ENDIF

DO IREC = 1, NUM_RECORDS
   DO J = 1, NUM_OPEN_BOUNDARIES
      CONC_OPENB_DOC(J,IREC)    = CONC_DOC(INDEX_BC(J),IREC)
      CONC_OPENB_DON(J,IREC)    = CONC_DON(INDEX_BC(J),IREC)
      CONC_OPENB_DOP(J,IREC)    = CONC_DOP(INDEX_BC(J),IREC)
      CONC_OPENB_LOC(J,IREC)    = CONC_LOC(INDEX_BC(J),IREC)
      CONC_OPENB_LON(J,IREC)    = CONC_LON(INDEX_BC(J),IREC)
      CONC_OPENB_LOP(J,IREC)    = CONC_LOP(INDEX_BC(J),IREC)
      CONC_OPENB_NH4(J,IREC)    = CONC_NH4(INDEX_BC(J),IREC)
      CONC_OPENB_NO3(J,IREC)    = CONC_NO3(INDEX_BC(J),IREC)
      CONC_OPENB_DIA(J,IREC)    = CONC_DIA(INDEX_BC(J),IREC)
      CONC_OPENB_DIAN(J,IREC)   = CONC_DIAN(INDEX_BC(J),IREC)
      CONC_OPENB_DIAP(J,IREC)   = CONC_DIAP(INDEX_BC(J),IREC)
      CONC_OPENB_GRE(J,IREC)    = CONC_GRE(INDEX_BC(J),IREC)
      CONC_OPENB_GREN(J,IREC)   = CONC_GREN(INDEX_BC(J),IREC)
      CONC_OPENB_GREP(J,IREC)   = CONC_GREP(INDEX_BC(J),IREC)
      CONC_OPENB_ROC(J,IREC)    = CONC_ROC(INDEX_BC(J),IREC)
      CONC_OPENB_RON(J,IREC)    = CONC_RON(INDEX_BC(J),IREC)
      CONC_OPENB_ROP(J,IREC)    = CONC_ROP(INDEX_BC(J),IREC)
      CONC_OPENB_SA(J,IREC)     = CONC_SA(INDEX_BC(J),IREC)
      CONC_OPENB_SRP(J,IREC)    = CONC_SRP(INDEX_BC(J),IREC)
      CONC_OPENB_SU(J,IREC)     = CONC_SU(INDEX_BC(J),IREC)
      CONC_OPENB_ZOO(J,IREC)    = CONC_ZOO(INDEX_BC(J),IREC)
      CONC_OPENB_DO2(J,IREC)    = CONC_DO2(INDEX_BC(J),IREC)
      CONC_OPENB_SAL(J,IREC)    = CONC_SAL(INDEX_BC(J),IREC)
      CONC_OPENB_TRACER(J,IREC) = CONC_TRACER(INDEX_BC(J),IREC)
   ENDDO
ENDDO


!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!   Create NetCDF files
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
STATUS = NF90_CREATE(PATH = OUTPUT_FILE, &
                     CMODE = NF90_CLOBBER, NCID = NCID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)


!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!   Define dimensions
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
STATUS = NF90_DEF_DIM(NCID, "NUM_OPEN_BOUNDARIES", &
         NUM_OPEN_BOUNDARIES, NUMOB_DIM_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_DIM(NCID,"RECORD_NUMBER",NF90_UNLIMITED,REC_DIM_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!   Define variables
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
STATUS = NF90_DEF_VAR(NCID,"TIME_BC",NF90_DOUBLE,  &
         (/ REC_DIM_ID /), TIME_BC_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"FLOW_FACES",NF90_INT,  &
         (/ NUMOB_DIM_ID /), FF_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"DOC",NF90_FLOAT,  &
         (/ NUMOB_DIM_ID, REC_DIM_ID /), DOC_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"DIA",NF90_FLOAT, & 
         (/ NUMOB_DIM_ID, REC_DIM_ID /), DIA_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"GRE",NF90_FLOAT,  &
        (/ NUMOB_DIM_ID, REC_DIM_ID /), GRE_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"ZOO",NF90_FLOAT,  &
        (/ NUMOB_DIM_ID, REC_DIM_ID /), ZOO_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"LOC",NF90_FLOAT,  &
        (/ NUMOB_DIM_ID, REC_DIM_ID /), LOC_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"ROC",NF90_FLOAT,  &
         (/ NUMOB_DIM_ID, REC_DIM_ID /), ROC_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"SRP",NF90_FLOAT,  &
         (/ NUMOB_DIM_ID, REC_DIM_ID /), SRP_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"DOP",NF90_FLOAT,  &
         (/ NUMOB_DIM_ID, REC_DIM_ID /), DOP_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"LOP",NF90_FLOAT,  &
         (/ NUMOB_DIM_ID, REC_DIM_ID /), LOP_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"ROP",NF90_FLOAT,  &
         (/ NUMOB_DIM_ID, REC_DIM_ID /), ROP_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"NH4",NF90_FLOAT,  &
         (/ NUMOB_DIM_ID, REC_DIM_ID /), NH4_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"NO3",NF90_FLOAT,  &
         (/ NUMOB_DIM_ID, REC_DIM_ID /), NO3_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"DON",NF90_FLOAT,  &
         (/ NUMOB_DIM_ID, REC_DIM_ID /), DON_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"LON",NF90_FLOAT,  &
         (/ NUMOB_DIM_ID, REC_DIM_ID /), LON_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"RON",NF90_FLOAT,  &
         (/ NUMOB_DIM_ID, REC_DIM_ID /), RON_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"SA",NF90_FLOAT,  &
         (/ NUMOB_DIM_ID, REC_DIM_ID /), SA_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"SU",NF90_FLOAT,  &
         (/ NUMOB_DIM_ID, REC_DIM_ID /), SU_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"DIAN",NF90_FLOAT,  &
         (/ NUMOB_DIM_ID, REC_DIM_ID /), DIAN_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"DIAP",NF90_FLOAT,  &
         (/ NUMOB_DIM_ID, REC_DIM_ID /), DIAP_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"GREN",NF90_FLOAT,  &
         (/ NUMOB_DIM_ID, REC_DIM_ID /), GREN_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"GREP",NF90_FLOAT,  &
         (/ NUMOB_DIM_ID, REC_DIM_ID /), GREP_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"SAL",NF90_FLOAT,  &
         (/ NUMOB_DIM_ID, REC_DIM_ID /), SAL_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"DO2",NF90_FLOAT,  &
         (/ NUMOB_DIM_ID, REC_DIM_ID /), DO2_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID,"Tracer",NF90_FLOAT,  &
         (/ NUMOB_DIM_ID, REC_DIM_ID /), TRACER_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!   Add Attributes
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!------------------------------------------------------------------------------
!   Global Attributes
!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,NF90_GLOBAL,"TITLE","BOUNDARY CONDITIONS")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,NF90_GLOBAL,"TITLE2",FILE_DESC)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STIME = TIME()
TIMESTAMP = CTIME(STIME)

HISTORY_STRING = 'This NetCDF file was created by ' // TRIM(USER_NAME) &
                 // ' using the program ' // TRIM(PROGRAM_NAME) // ' on ' &
                 // TRIM(TIMESTAMP)
STATUS = NF90_PUT_ATT(NCID,NF90_GLOBAL,"HISTORY",TRIM(HISTORY_STRING))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
!   Variable Attributes
!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,TIME_BC_ID,"LONG_NAME", &
         "Update time for boundary concentrations.")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,TIME_BC_ID,"UNITS","Julian days")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,TIME_BC_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,TIME_BC_ID,"VALID_MAX",REAL(1.0E+07))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,FF_ID,"LONG_NAME","Open Boundary Flow Faces")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,FF_ID,"UNITS","None")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,DOC_ID,"LONG_NAME","Dissolved Organic Carbon")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DOC_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DOC_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DOC_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,DIA_ID,"LONG_NAME","Diatoms")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DIA_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DIA_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DIA_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,DIAN_ID,"LONG_NAME","Internal N in Diatoms")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DIAN_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DIAN_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DIAN_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,DIAP_ID,"LONG_NAME","Internal P in Diatoms")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DIAP_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DIAP_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DIAP_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,GRE_ID,"LONG_NAME","Greens")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,GRE_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,GRE_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,GRE_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,GREN_ID,"LONG_NAME","Internal N in Greens")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,GREN_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,GREN_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,GREN_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,GREP_ID,"LONG_NAME","Internal P in Greens")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,GREP_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,GREP_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,GREP_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,ZOO_ID,"LONG_NAME","Herbivorous Zooplankton")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,ZOO_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,ZOO_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,ZOO_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,LOC_ID,"LONG_NAME", &
         "Labile Detrital Organic Carbon")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,LOC_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,LOC_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,LOC_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,ROC_ID,"LONG_NAME", &
         "Refractory Detrital Organic Carbon")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,ROC_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,ROC_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,ROC_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,SRP_ID,"LONG_NAME", "Soluble Reactive Phosphorus")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,SRP_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,SRP_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,SRP_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,DOP_ID,"LONG_NAME","Dissolved Organic Phosphorus")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DOP_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DOP_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DOP_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,LOP_ID,"LONG_NAME", &
         "Labile Detrital Organic Phosphorus")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,LOP_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,LOP_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,LOP_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,ROP_ID,"LONG_NAME", &
         "Refractory Detrital Organic Phosphorus")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,ROP_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,ROP_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,ROP_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,NH4_ID,"LONG_NAME","Ammonia")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,NH4_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,NH4_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,NH4_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,NO3_ID,"LONG_NAME","Nitrate")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,NO3_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,NO3_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,NO3_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,DON_ID,"LONG_NAME","Dissolved Organic Nitrogen")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DON_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DON_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DON_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,LON_ID,"LONG_NAME", &
         "Labile Detrital Organic Nitrogen")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,LON_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,LON_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,LON_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,RON_ID,"LONG_NAME", &
         "Refractory Detrital Organic Nitrogen")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,RON_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,RON_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,RON_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,SA_ID,"LONG_NAME","Available Silica")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,SA_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,SA_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,SA_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,SU_ID,"LONG_NAME","Unavailable Silica")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,SU_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,SU_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,SU_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,SAL_ID,"LONG_NAME","Salinity")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,SAL_ID,"UNITS",&
         "Practical Salinity Units (PSU)")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,SAL_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,SAL_ID,"VALID_MAX",REAL(50))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,DO2_ID,"LONG_NAME","Dissolved Oxygen")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DO2_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DO2_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,DO2_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID,TRACER_ID,"LONG_NAME","Tracer")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,TRACER_ID,"UNITS","kg/m^3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,TRACER_ID,"VALID_MIN",REAL(0.0))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID,TRACER_ID,"VALID_MAX",REAL(1.0E06))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)


!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!   End definitions
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
STATUS = NF90_ENDDEF(NCID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)


!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!   Write Data Values
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
STATUS = NF90_PUT_VAR(NCID,TIME_BC_ID,TIME_BC)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,FF_ID,FLOW_FACES)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,DOC_ID,CONC_OPENB_DOC)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,DIA_ID,CONC_OPENB_DIA)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,DIAN_ID,CONC_OPENB_DIAN)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,DIAP_ID,CONC_OPENB_DIAP)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,GRE_ID,CONC_OPENB_GRE)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,GREN_ID,CONC_OPENB_GREN)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,GREP_ID,CONC_OPENB_GREP)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,ZOO_ID,CONC_OPENB_ZOO)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,LOC_ID,CONC_OPENB_LOC)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,ROC_ID,CONC_OPENB_ROC)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,SRP_ID,CONC_OPENB_SRP)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,DOP_ID,CONC_OPENB_DOP)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,LOP_ID,CONC_OPENB_LOP)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,ROP_ID,CONC_OPENB_ROP)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,NH4_ID,CONC_OPENB_NH4)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,NO3_ID,CONC_OPENB_NO3)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,DON_ID,CONC_OPENB_DON)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,LON_ID,CONC_OPENB_LON)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,RON_ID,CONC_OPENB_RON)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,SA_ID,CONC_OPENB_SA)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,SU_ID,CONC_OPENB_SU)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,SAL_ID,CONC_OPENB_SAL)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,DO2_ID,CONC_OPENB_DO2)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID,TRACER_ID,CONC_OPENB_TRACER)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!   Close NetCDF file
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
STATUS = NF90_CLOSE(NCID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)



END PROGRAM BOUND_CONDITIONS
