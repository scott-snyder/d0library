      SUBROUTINE GEOUCR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets up the Central Cryostat Geometry for GEANT
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  22-NOV-1988   Elliott A. Treadwell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER IVOLU,IT,IZ
C
      INCLUDE 'D0$INC:CRYVLN.INC'
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:SRCPR.INC'
C----------------------------------------------------------------------
C Names of Cryostat Volumes in SRCP
C
      CHARACTER*32 CENNM(NUCRVL,2) !CC SRCP names.
C
      DATA CENNM/
C
C ****  POSITIVE Z
C
     +           'CRY_CEN_WARM_TUBE+Z',  !(1) to indicate array
     +           'CRY_CEN_WARM_TOP+Z' ,
     +           'CRY_CEN_WARM_BOTTOM+Z',
     +           'CRY_CEN_COLD_TUBE+Z',
     +           'CRY_CEN_COLD_BOTTOM+Z',
     +           'CRY_CEN_COLD_TOP+Z',
C
C ****  NEGATIVE Z
C
     +           'CRY_CEN_WARM_TUBE-Z',  !(1) to indicate array
     +           'CRY_CEN_WARM_TOP-Z' ,
     +           'CRY_CEN_WARM_BOTTOM-Z',
     +           'CRY_CEN_COLD_TUBE-Z',
     +           'CRY_CEN_COLD_BOTTOM-Z',
     +           'CRY_CEN_COLD_TOP-Z'/
C
C----------------------------------------------------------------------
C  Central Cryostat
C
C
        DO 20 IZ = 1,2  !Do both  +/- Z
          DO 30 IT = 1,NUCRVL
            CALL VOLPOS(CENNM(IT,IZ))
            UCRYVL(IT,IZ)= ISRCPR(1)
   30     CONTINUE
   20   CONTINUE
C
  999 RETURN
      END
