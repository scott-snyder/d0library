      SUBROUTINE CGTTMGETA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : build a table which contains whether the trigger
C-   towers at a given eta contain a massless gap or ICD
C-
C-   Inputs  : none
C-   Outputs : TTMGETA
C-   Controls:
C-
C-   Created   9-NOV-1990   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$INC:PHTT.INC'
      INCLUDE 'D0$INC:TTMGETA.INC'      ! whether TT has MG or ICD in it
      INCLUDE 'D0$INC:TTEDGE.INC'       ! 1st location of TT in PTCAEP2
      INTEGER TTETA                     ! loop index in trigger towers
C----------------------------------------------------------------------
      DO TTETA = -NETAL11,NETAL11       ! array says whether MG in this trigger
        TTMGETA(TTETA) = .FALSE.        ! tower
      ENDDO
C
C...This limits here are shaky, but work; want them to be 4,7: with MG OR ICD
      DO TTETA = PHTTETA(MNETAMG),PHTTETA(MXETAMG)
        TTMGETA(TTETA) = .TRUE.
        TTMGETA(-TTETA) = .TRUE.
      ENDDO
      TTMGETA(NETAL11) = .TRUE.         ! MG/ICD are in with end towers
      TTMGETA(-NETAL11) = .TRUE.
C----------------------------------------------------------------------
  999 RETURN
      END
