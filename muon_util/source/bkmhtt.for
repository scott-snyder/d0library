      SUBROUTINE BKMHTT(LSUP,NDAT,LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book bank 'MHTT'.
C-
C-   Inputs  : 
C-      LSUP    I   address of support bank (MUOT).
C-
C-      NDAT    I   number of data words
C-
C-   Outputs : 
C-      LADDR   I   address of bank, MHTT.
C-
C-   Controls: 
C-
C-   Created  10-SEP-1990   HEDIN ----- needs both LSUP and NDAT
C-   to be non-zero for the bank to be booked
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMHTT.LINK'
C  -- variable in arguments...
      INTEGER LSUP,NDAT,LADDR,ND,MMBK(5) 
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
         CALL UCTOH('MHTT',MMBK(1),4,4)   ! IDH (hollerith bank name)
         MMBK(2)=0                        ! NL (total number of links)
         MMBK(3)=0                        ! NS (number of struct. links)
         ND=5
         MMBK(4)=ND                       ! ND (number of data words)
         CALL MZFORM('MHTT','/1B 4I',MMBK(5))  ! NIO (bank format)
         FIRST=.FALSE.
      ENDIF
      IF(NDAT.EQ.0) THEN
         MMBK(4)=ND
      ELSE
         MMBK(4)=NDAT
      ENDIF
      IF(LSUP.NE.0.AND.NDAT.NE.0) 
     A  CALL MZLIFT(IXMAIN,LADDR,LSUP,-IZMHTT,MMBK,0)
      RETURN
      END
