      SUBROUTINE JIQTXT(NDEV, CXSIZ, CYSIZ, ACTXSZ, ACTYSZ)
C----------------------------------------------------------------------
C-
CD   Purpose and Methods : To return the actual size of the hardware character
CD                         that would be selected by the specified device,
CD                         corresponding to the requested world coordinate
CD                         character size.
C-
C-   Inputs  : NDEV, CXSIZ, CYSIZ
C-   Outputs : ACTXSZ, ACTYSZ
C-   Controls: None
C-
C-   Created   2-MAY-1989   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
C
C    Common blocks:
CB      GENRL-R, GRFPAR-R
C
C    Calls:
CC      ERROR.
C
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      IMPLICIT NONE
      INTEGER  NDEV
      REAL     CXSIZ, CYSIZ, ACTXSZ, ACTYSZ
C
C    Then local declarations of variables (non-common variables).
C
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GENRL.INC/LIST'
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
C
C    Then executable code follows
C
      IF(CXSIZ .LE. 0.0 .OR. CYSIZ .LE. 0.0) THEN
        CALL ERROR(' CXSIZ AND/OR CYSIZ ARE LESS THAN OR EQUAL TO
     &    ZERO')
        GOTO 999
      ENDIF
C
      IF(NDEV .EQ. DSPDEV .AND. DINIT) THEN
        ACTXSZ = CXSIZ
        ACTYSZ = CYSIZ
      ELSE
        CALL ERROR(' DEVICE IS NOT INITIALIZED' )
        GOTO 999
      ENDIF
C
  999 RETURN
      END
