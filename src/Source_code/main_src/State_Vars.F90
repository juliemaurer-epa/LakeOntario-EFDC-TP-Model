      MODULE State_Vars 

       IMPLICIT NONE

      real,allocatable,save :: f(:,:,:,:) !state variable array

#ifdef _MPI
!include 'mpif.h'
#endif

      contains

      Subroutine Set_Vars(Which_code,init_filename,myid,numprocs)

      IMPLICIT NONE

      integer, intent(in) :: myid,numprocs
      character(6), intent(in) :: Which_code 
      character(120), intent(in) :: init_filename

      call Vars_allocate()

      call Set_Initial_Conditions(Which_code,init_filename,myid,numprocs) 

#ifdef DEBUG
 write(6,*) "--subroutine set_vars"
 write(6,*) "allocate and set f array"
 write(6,*) 
#endif

      return

      End Subroutine Set_Vars

      Subroutine Vars_allocate()

      USE Model_dim
      USE Fill_Value

      IMPLICIT NONE

      integer ierr

      ALLOCATE(f(0:(myim+1),jm,km,nf),stat=ierr)
      if(ierr.ne.0) call error("f",ierr)

      f=fill(1)

      return

      End Subroutine Vars_allocate

      Subroutine Set_Initial_Conditions(Which_code,init_filename,myid,numprocs)

      IMPLICIT NONE

      integer, intent(in) :: myid,numprocs
      character(6), intent(in) :: Which_code
      character(120), intent(in) :: init_filename

      if(Which_code.eq."CGEM") then !CGEM

       call Set_Initial_Conditions_CGEM(init_filename,myid,numprocs)

      else if(Which_code.eq."GOMDOM") then !GOMDOM

       call Set_Initial_Conditions_GD(init_filename,myid,numprocs)

      else

       write(6,*) "Model ",Which_code," not found, Exiting."
       stop
    
      endif

      return

      End Subroutine Set_Initial_Conditions 


      End Module State_Vars 
