      SUBROUTINE BKCAWX(LCASH,NCELLS,LCAWX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank CAWX
C-
C-   Inputs  : LCASH = Link of parent bank.
C-                     
C-   Outputs : Link of Booked CAWX Bank
C-   Controls: None
C-
C-   Updated   9-JUL-1995   Ian Adam  MODIFY BKCASH TO DO CAWX BANK 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZCAWX.LINK/LIST'
C
      INTEGER LCAWX,LCASH
      INTEGER IXIO
      INTEGER NCELLS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      LCAWX = 0
      IF(FIRST)THEN
        FIRST = .FALSE.
        CALL MZFORM('CAWX','2I/1I1F',IXIO)        ! Describe Bank format
      ENDIF
C
      LCAWX = LQ(LCASH-IZCAWX)
      IF (LCAWX.GT.0) CALL MZDROP(IXCOM,LCAWX,' ')

      IF ( LCASH.EQ.0 ) THEN
         CALL ERRMSG('CAPHEL','BKCAWX',
     &      'CANNOT BOOK CAWX BANK WITHOUT CASH LINK','W')
         GOTO 999
      ENDIF
C
      CALL MZBOOK
     &  (IXMAIN,LCAWX,LCASH,-IZCAWX,'CAWX',0,0,2*NCELLS+2,IXIO,0)
C
      IQ(LCAWX+1) = 1
      IQ(LCAWX+2) = NCELLS
C
  999 RETURN
      END
