      LOGICAL FUNCTION DLIMIT_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-DEC-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SIG_EFF.INC'
      INCLUDE 'D0$INC:TOP_CROSS.INC'
      INTEGER IER
C----------------------------------------------------------------------
      DLIMIT_INI = .FALSE.
C      CALL INZBRA
C      CALL INPAWC
C      CALL INZSTP
C
      CALL INRCP('DLIMIT_RCP',IER)       ! read in RCP file
      IF(IER.NE.0) GOTO 999              ! failed
C
      CALL EZGET_i('MPOL',MPOL,IER)
      CALL EZGET('TM_EFF',TM_EFF,IER)
C
      CALL EZGET('EE_SBR',EE_SBR,IER)
      CALL EZGET('EMU_SBR',EMU_SBR,IER)
      CALL EZGET('EJETS_SBR',EJETS_SBR,IER)
      CALL EZGET('EJETS_TAG_SBR',EJETS_TAG_SBR,IER)
      CALL EZGET('MUMU_SBR',MUMU_SBR,IER)
      CALL EZGET('MUJETS_SBR',MUJETS_SBR,IER)
      CALL EZGET('MUJETS_TAG_SBR',MUJETS_TAG_SBR,IER)
C
      CALL EZGET('EE_ERR',EE_ERR,IER)
      CALL EZGET('EMU_ERR',EMU_ERR,IER)
      CALL EZGET('EJETS_ERR',EJETS_ERR,IER)
      CALL EZGET('EJETS_TAG_ERR',EJETS_TAG_ERR,IER)
      CALL EZGET('MUMU_ERR',MUMU_ERR,IER)
      CALL EZGET('MUJETS_ERR',MUJETS_ERR,IER)
      CALL EZGET('MUJETS_TAG_ERR',MUJETS_TAG_ERR,IER)
C
      CALL EZGET_i('NEVENTS_DISC',NEVENTS_DISC,IER)
C
      CALL EZGET_i('M20',M20,IER)
      CALL EZGET_i('M18',M18,IER)
C
      DLIMIT_INI = .TRUE.
  999 RETURN
      END
