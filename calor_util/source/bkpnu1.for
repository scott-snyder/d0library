      SUBROUTINE BKPNU1(NVERT,LPNU1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-       NVERT = number of additional primary vertices (number or repetitions)
C-   Outputs : 
C-     LPNU1 = pointer to PNU1 bank created 
C-
C-   Created 14-SEP-1995   Dhiman Chakraborty   
C-       version no. = 1
C        NR_PNU1 = 11
C-
C-   Updated   1-OCT-1995   Dhiman Chakraborty  
C-                          If BKPNU1 is called, then BKPNUT(2) will be called
C-                          there instead of in C2PMET because BKPNU1 is 
C-                          called first and needs PNUT2.  
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$LINKS:IZPNU1.LINK'
      INCLUDE 'D0$INC:PNU1.INC'
      INTEGER NVERT,IOPNU1,ND
      INTEGER LPNU1
      INTEGER GZPNUT
      EXTERNAL GZPNUT
      LOGICAL FIRST
      SAVE FIRST,IOPNU1 
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        CALL MZFORM('PNU1','3I/11F',IOPNU1)
      ENDIF
C
C*** PNU1 bank will hang from PNUT2 :
C
      LPNUT2 = GZPNUT(2)
      IF(LPNUT2.LE.0) then
        CALL BKPNUT(2)  ! Book the PNUT #2 bank
        LPNUT2 = GZPNUT(2)
      ENDIF  
      IF(LPNUT2.LE.0)  THEN
        CALL ERRMSG('BKPNU1','CAL_FIX',
     &          'PNUT2 not found, bailing','W')
        GOTO 999
      ENDIF
C
C    Now book a PNU1 bank
C
      ND = NVERT*NR_PNU1 + NOFF_PNU1
      CALL MZBOOK(IXMAIN,LPNU1,LPNUT2,-IZPNU1,
     &                    'PNU1',NL_PNU1,NS_PNU1,ND,IOPNU1,0)
  999 RETURN
      END
