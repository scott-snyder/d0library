      SUBROUTINE BKMSEG( ITRAK,LAMSEG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book MSEG bank, hunging from MUON bank
C-                         return bank address
C-
C-   Inputs  : ITRAK : Track number of MUON bank
C-   Outputs : LAMSEG:address of MSEG bank
C-   Controls: none
C-
C-   Created   7-OCT-1992   Atsushi Taketani
C-   Updated   6-JUL-1994   Daria Zieminska  correct the format 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$LINKS:IZMSEG.LINK'
C
      INTEGER  ITRAK, NMOD, LAMSEG
C
      INTEGER  GZMUON, LMSEG, LUP, LZLAST, LUP2
      INTEGER  IXIO
C
      LOGICAL  FIRST
      DATA     FIRST/.TRUE./
C----------------------------------------------------------------------
      LAMSEG = 0                              ! assume failure
C
      LUP = GZMUON(ITRAK)                     ! get MUON address
      IF (LUP.EQ.0) GOTO 999                  ! no MUON bank
C
      IF ( FIRST ) THEN
        CALL MZFORM('MSEG','4I -F' , IXIO )
        FIRST = .FALSE.
      END IF
C
      LUP2 = LQ(LUP-IZMSEG)
      IF ( LUP2.EQ.0 ) THEN
        CALL MZBOOK( IXMAIN, LMSEG, LUP, -IZMSEG, 'MSEG',
     1               3, 0, 24, IXIO, 0 )
      ELSE
        LUP = LZLAST( IXMAIN,LUP2)             ! get last linear chain
        CALL MZBOOK( IXMAIN, LMSEG, LUP, 0,       'MSEG',
     1               3, 0, 24, IXIO, 0 )
      END IF
C
      LAMSEG = LMSEG
C
  999 RETURN
      END
