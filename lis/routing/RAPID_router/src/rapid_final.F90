!*******************************************************************************
!Subroutine - rapid_final 
!*******************************************************************************
#include "LIS_misc.h"
#ifdef PETSc

subroutine rapid_final

!Purpose:
!This subroutine allows to finalize RAPID for both regular runs and 
!optimization runs, by performing slightly different tasks depending on what 
!option is chosen.  
!Finalization Initialization tasks specific to Option 1
!     -Output final instantaneous flow
!     -Output babsmax, QoutRabsmin and QoutRabsmax
!Finalization Initialization tasks specific to Option 2
!     -N/A
!Finalization tasks common to all RAPID options:
!     -Prints some information about the types of objects used during simulation
!     -Destroy all PETSc and TAO objects 
!Author: 
!Cedric H. David, 2012-2020.


!*******************************************************************************
!Fortran includes, modules, and implicity
!*******************************************************************************
#include <petsc/finclude/petscksp.h>
use petscksp
use rapid_var, only :                                                          &
                   IS_riv_bas,JS_riv_bas,                                      &
                   IS_opt_routing,IS_opt_run,                                  &
                   BS_opt_Qfinal,BS_opt_influence,                             &
                   Qfinal_file,babsmax_file,QoutRabsmin_file,QoutRabsmax_file, &
                   ksp,vecscat,ZV_babsmax,ZV_SeqZero,ierr,                     &
                   ZV_pointer,rank,ZV_k,temp_char,                             &
                   ZV_QoutRabsmin,ZV_QoutRabsmax,                              &
                   temp_char2,ZM_A,pc,                                         &
                   IS_ksp_iter_max
implicit none

!*******************************************************************************
!Finalization procedure for OPTION 1
!*******************************************************************************
if (IS_opt_run==1) then

!-------------------------------------------------------------------------------
!Output final instantaneous Q (ZV_QoutR)
!-------------------------------------------------------------------------------
if (BS_opt_Qfinal) then
call rapid_create_Qfinal_file(Qfinal_file)
call rapid_open_Qfinal_file(Qfinal_file)
call rapid_write_Qfinal_file
call rapid_close_Qfinal_file
end if

!-------------------------------------------------------------------------------
!Output maximum absolute values of vector b (right-hand side of linear system)
!-------------------------------------------------------------------------------
if (BS_opt_influence) then
call VecScatterBegin(vecscat,ZV_babsmax,ZV_SeqZero,                            &
                     INSERT_VALUES,SCATTER_FORWARD,ierr)
call VecScatterEnd(vecscat,ZV_babsmax,ZV_SeqZero,                              &
                        INSERT_VALUES,SCATTER_FORWARD,ierr)
call VecGetArrayF90(ZV_SeqZero,ZV_pointer,ierr)
if (rank==0) then 
     open(42,file=babsmax_file)
     do JS_riv_bas=1,IS_riv_bas
          write(42,*) ZV_pointer(JS_riv_bas)
     end do
     close(42)
end if
call VecRestoreArrayF90(ZV_SeqZero,ZV_pointer,ierr)
end if

!-------------------------------------------------------------------------------
!Output minimum absolute values of instantaneous flow 
!-------------------------------------------------------------------------------
if (BS_opt_influence) then
call VecScatterBegin(vecscat,ZV_QoutRabsmin,ZV_SeqZero,                        &
                     INSERT_VALUES,SCATTER_FORWARD,ierr)
call VecScatterEnd(vecscat,ZV_QoutRabsmin,ZV_SeqZero,                          &
                        INSERT_VALUES,SCATTER_FORWARD,ierr)
call VecGetArrayF90(ZV_SeqZero,ZV_pointer,ierr)
if (rank==0) then 
     open(43,file=QoutRabsmin_file)
     do JS_riv_bas=1,IS_riv_bas
          write(43,*) ZV_pointer(JS_riv_bas)
     end do
     close(43)
end if
call VecRestoreArrayF90(ZV_SeqZero,ZV_pointer,ierr)
end if

!-------------------------------------------------------------------------------
!Output maximum absolute values of instantaneous flow 
!-------------------------------------------------------------------------------
if (BS_opt_influence) then
call VecScatterBegin(vecscat,ZV_QoutRabsmax,ZV_SeqZero,                        &
                     INSERT_VALUES,SCATTER_FORWARD,ierr)
call VecScatterEnd(vecscat,ZV_QoutRabsmax,ZV_SeqZero,                          &
                        INSERT_VALUES,SCATTER_FORWARD,ierr)
call VecGetArrayF90(ZV_SeqZero,ZV_pointer,ierr)
if (rank==0) then 
     open(44,file=QoutRabsmax_file)
     do JS_riv_bas=1,IS_riv_bas
          write(44,*) ZV_pointer(JS_riv_bas)
     end do
     close(44)
end if
call VecRestoreArrayF90(ZV_SeqZero,ZV_pointer,ierr)
end if

!-------------------------------------------------------------------------------
!End of initialization procedure for OPTION 1
!-------------------------------------------------------------------------------
end if


!*******************************************************************************
!Some information about types of objects used within RAPID run
!*******************************************************************************
!call PetscPrintf(PETSC_COMM_WORLD,'--------------------------'//char(10),ierr)
!call VecGetType(ZV_k,temp_char,ierr)
!call PetscPrintf(PETSC_COMM_WORLD,'type of vector: '//temp_char//char(10),ierr)
!call MatGetType(ZM_A,temp_char,ierr)
!call PetscPrintf(PETSC_COMM_WORLD,'type of matrix: '//temp_char//char(10),ierr)
!if (IS_opt_routing==1 .or. IS_opt_routing==3) then 
!     call KSPGetType(ksp,temp_char,ierr)
!else
!     temp_char='No KSP'
!end if
!call PetscPrintf(PETSC_COMM_WORLD,'type of KSP   : '//temp_char//char(10),ierr)
!if (IS_opt_routing==1 .or. IS_opt_routing==3) then 
!     call KSPGetPC(ksp,pc,ierr)
!     call PCGetType(pc,temp_char,ierr)
!else
!     temp_char='No PC'
!end if
!call PetscPrintf(PETSC_COMM_WORLD,'type of PC    : '//temp_char//char(10),ierr)
!write(temp_char ,'(i10)') rank
!write(temp_char2,'(i10)') IS_ksp_iter_max
!call PetscSynchronizedPrintf(PETSC_COMM_WORLD,'Rank     :'//temp_char //', '// &
!                                              'Max KSP  :'//temp_char2//       &
!                                               char(10),ierr)
!call PetscSynchronizedFlush(PETSC_COMM_WORLD,PETSC_NULL_INTEGER,ierr)
!call PetscPrintf(PETSC_COMM_WORLD,'--------------------------'//char(10),ierr)
!call PetscPrintf(PETSC_COMM_WORLD,char(10)//char(10)//char(10)//char(10),ierr)

!*******************************************************************************
!Destroy all objects
!*******************************************************************************
call rapid_destro_obj
!destroy PETSc and TAO objects (Mat,Vec,taoapp...), finalizes the libraries


!*******************************************************************************
!End subroutine 
!*******************************************************************************
end subroutine rapid_final

#else

! Dummy version
subroutine rapid_final
  use LIS_logmod, only: LIS_logunit, LIS_endrun
  implicit none
  write(LIS_logunit,*)'[ERR] RAPID called w/o PETSc support!'
  write(LIS_logunit,*)'[ERR] Recompile with PETSc and try again!'
  call LIS_endrun()
end subroutine rapid_final

#endif

