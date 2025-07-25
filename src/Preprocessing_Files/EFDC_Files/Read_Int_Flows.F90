!------------------------------------------------------------
Program Read_Int_Flows
!------------------------------------------------------------
! This program reads hydrodynamic information
! from a binary file.

IMPLICIT NONE

INTEGER(kind=4) :: NQSIJ, NQCTL, NQWR, NQINFLM
INTEGER(kind=4) :: I
INTEGER(kind=4) :: Reason
INTEGER(kind=4), PARAMETER :: KC = 10   ! Maximum number of layers
INTEGER(kind=4), ALLOCATABLE, DIMENSION(:) :: IQS, JQS, IQCTLU
INTEGER(kind=4), ALLOCATABLE, DIMENSION(:) :: JQCTLU, IQCTLD, JQCTLD
INTEGER(kind=4), ALLOCATABLE, DIMENSION(:) :: IQWRU, JQWRU, KQWRU
INTEGER(kind=4), ALLOCATABLE, DIMENSION(:) :: IQWRD, JQWRD, KQWRD

REAL,ALLOCATABLE, DIMENSION(:,:) :: QINTFL
! REAL,ALLOCATABLE, DIMENSION(:,:) :: QINFRAC
! REAL :: TMID

! All variables/arrays referenced below have the same meaning as used in EFDC:
! NQSIJ = # of volumetric sources/sinks
! NQCTL = # of internal flow structures
! NQWR = # of withdrawal/return cell pairs
! IQS / JQS = array of “I” / “J” indices for volumetric source/sink locations
! IQCTLU / JQCTLU = array of “I” / “J” indices for “upstream” cell 
! associated with individual internal flow structures
! 
! IQCTLD / JQCTLD = array of “I” / “J” indices for “downstream” cell 
! associated with individual internal flow structures
! 
! IQWRU / JQWRU / KQWRU = array of “I” / “J” / “K” indices for “withdrawal” cells
! IQWRD / JQWRD / KQWRD = array of “I” / “J” / “K” indices for “return” cells
! NQINFLM = sum of all sources/sinks (NQSIJ+NQCTL+NQWR)
! QINTFL = array of flow rates associated with individual sources/sinks (1:NQINFLM) 
! and for individual vertical layers.

OPEN(UNIT = 60, FILE = 'EFDC_InternalFlows.txt', STATUS = 'UNKNOWN')

OPEN(UNIT = 61, FILE = 'EFDC_InternalFlows_Niagara_River.txt', STATUS = 'UNKNOWN')
OPEN(UNIT = 62, FILE = 'EFDC_InternalFlows_EighteenMile_River.txt', STATUS = 'UNKNOWN')
OPEN(UNIT = 63, FILE = 'EFDC_InternalFlows_OakOrchard_River.txt', STATUS = 'UNKNOWN')
OPEN(UNIT = 64, FILE = 'EFDC_InternalFlows_Genesee_River.txt', STATUS = 'UNKNOWN')
OPEN(UNIT = 65, FILE = 'EFDC_InternalFlows_Oswego_River.txt', STATUS = 'UNKNOWN')
OPEN(UNIT = 66, FILE = 'EFDC_InternalFlows_Outflow1.txt', STATUS = 'UNKNOWN')
OPEN(UNIT = 67, FILE = 'EFDC_InternalFlows_Outflow2.txt', STATUS = 'UNKNOWN')


OPEN(UNIT = 50, FILE = 'EFDC_intflw.bin', FORM = 'UNFORMATTED')
READ(50) NQSIJ, NQCTL, NQWR        
WRITE(60,*) NQSIJ, NQCTL, NQWR

IF(NQSIJ > 0)THEN
   ALLOCATE(IQS(NQSIJ),JQS(NQSIJ))
   READ(50) IQS,JQS
ENDIF

WRITE(60,*)
WRITE(60,*) 'IQS = ', IQS
WRITE(60,*) 'JQS = ', JQS      
   
IF(NQCTL > 0)THEN
   ALLOCATE(IQCTLU(NQCTL),JQCTLU(NQCTL))
   ALLOCATE(IQCTLD(NQCTL),JQCTLD(NQCTL))            
   READ(50) IQCTLU,JQCTLU,IQCTLD,JQCTLD       

   WRITE(60,*)     
   WRITE(60,*) IQCTLU
   WRITE(60,*)
   WRITE(60,*) JQCTLU
   WRITE(60,*)
   WRITE(60,*) IQCTLD
   WRITE(60,*)
   WRITE(60,*) JQCTLD
   WRITE(60,*)           
ENDIF

IF(NQWR > 0)THEN
   ALLOCATE(IQWRU(NQWR),JQWRU(NQWR),KQWRU(NQWR))
   ALLOCATE(IQWRD(NQWR),JQWRD(NQWR),KQWRD(NQWR))
   READ(50) IQWRU,JQWRU,KQWRU,IQWRD,JQWRD,KQWRD

   WRITE(60,*) IQWRU
   WRITE(60,*)
   WRITE(60,*) JQWRU
   WRITE(60,*)
   WRITE(60,*) KQWRU
   WRITE(60,*)
   WRITE(60,*) IQWRD
   WRITE(60,*)
   WRITE(60,*) JQWRD
   WRITE(60,*)
   WRITE(60,*) KQWRD
ENDIF

NQINFLM = NQSIJ + NQCTL + NQWR

WRITE(60,*)
WRITE(60,*) 'NQINFLM = ', NQINFLM
WRITE(60,*)

! QINTFL array will be read concurrent with transport arrays:
ALLOCATE(QINTFL(NQINFLM,KC+1))      

DO
  ! Read the time mid-point of each linkage interval (representing simulation day
  ! relative to day zero used in the EFDC simulation:
  ! READ(50,IOSTAT = Reason) TMID

  IF(NQINFLM > 0)THEN
     READ(50, IOSTAT = Reason) QINTFL 
     IF (Reason > 0) THEN
         PRINT*, 'Check input. Something went wrong: stopping execution of program.'
         EXIT
     ELSE IF (Reason < 0) THEN
         PRINT*, 'End of file has been reached: ending execution of program.'
         EXIT
     ENDIF
  END IF

  DO I = 1, NQINFLM
     WRITE(60,*) I, QINTFL(I,:)
  ENDDO
  WRITE(60,*) 

  WRITE(61,10) QINTFL(1,1:10)
  WRITE(62,10) QINTFL(2,1:10)
  WRITE(63,10) QINTFL(3,1:10)
  WRITE(64,10) QINTFL(4,1:10)
  WRITE(65,10) QINTFL(5,1:10)
  WRITE(66,10) QINTFL(6,1:10)
  WRITE(67,10) QINTFL(7,1:10)

ENDDO

10 FORMAT(10F15.7)

CLOSE(50)
CLOSE(60)

CLOSE(61) 
CLOSE(62)
CLOSE(63)
CLOSE(64)
CLOSE(65)
CLOSE(66)
CLOSE(67)

END
