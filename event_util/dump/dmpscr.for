      SUBROUTINE DMPSCR(NAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     use EVE to examine file NAME
C-     This is a VAX only subroutine, becomes dummy in other machines
C-
C-   Input:
C-   NAME= character string giving file name
C-
C-   Created   3-AUG-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME
C----------------------------------------------------------------------
C
C&IF VAXVMS
      INTEGER TPU$TPU
      EXTERNAL TPU$TPU
      INTEGER TPU_STATUS

C
C          call EVE to examine NAME
      TPU_STATUS= TPU$TPU('TPU/READ_ONLY '//NAME)
      CALL LIBREP                      ! refresh screen
C&ENDIF
C
  999 RETURN
      END
