      SUBROUTINE BKVCAL(LVCAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book VCAL bank 
C-
C-   Inputs  : 
C-   Outputs : LVCAL
C-   Controls: 
C-
C-   Created  12-FEB-1994   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVCAL.LINK'
C I/O:
      INTEGER LVCAL
C Locals:
      LOGICAL FIRST
      INTEGER NL,NS,ND,NHEAD,NIO,BANK_VERSION
C Externals:
      INTEGER ISETVN,GZSVTX
C Data:
      DATA FIRST/.TRUE./
      DATA NL,NS,ND,NHEAD/0,0,710,70/
      DATA BANK_VERSION/0/
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL MZFORM('VCAL','7I 11F -B',NIO)
      ENDIF
      LSVTX = GZSVTX()
      IF (LSVTX .LE. 0)  CALL ERRMSG('VTXH bank does not exist',
     &    'BKVCAL','No supporting bank exists','F')
      CALL MZBOOK(IDVSTP,LVCAL,LSVTX,-IZVCAL_0,'VCAL',NL,NS,ND,NIO,0)
      IC(LVCAL  ) = ISETVN(IC(LVCAL),BANK_VERSION)
      IC(LVCAL+1) = BANK_VERSION
      IC(LVCAL+2) = NHEAD
      IC(LVCAL+3) = 1
  999 RETURN
      END
