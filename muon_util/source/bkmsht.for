      SUBROUTINE BKMSHT(LSUP,NDAT,LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book bank 'MHTT'.
C-
C-   Inputs  : 
C-      LSUP    I   address of support bank (MUOT).
C-
C-      NDAT    I   DUMMY
C-
C-   Outputs : 
C-      LADDR   I   address of bank, MSHT.
C-
C-   Controls: 
C-
C-   Created  15-FEB-1994 Atsushi Taketani
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMSHT.LINK'
C  -- variable in arguments...
      INTEGER LSUP,NDAT,LADDR,ND,MMBK(5) 
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
         CALL UCTOH('MSHT',MMBK(1),4,4)   ! IDH (hollerith bank name)
         MMBK(2)=0                        ! NL (total number of links)
         MMBK(3)=0                        ! NS (number of struct. links)
         MMBK(4)=4                        ! ND (number of data words)
         CALL MZFORM('MSHT','-I',MMBK(5))  ! NIO (bank format)
         FIRST=.FALSE.
      ENDIF
      IF(LSUP.NE.0) 
     A  CALL MZLIFT(IXMAIN,LADDR,LSUP,-IZMSHT,MMBK,0)
      RETURN
      END
