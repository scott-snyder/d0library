      SUBROUTINE BKDLYR ( ILAYD )
C======================================================================
C
C   Purpose and Methods :  Books the bank "DLYR" for a specified
C-                         CDC detector layer. The CDCH bank has been booked
C-                         earlier.
C
C-  Inputs : ILAYD is the CDC layer.
C-
C-   Created   4-JAN-1987  T. Trippe for VTX
C-   Updated  20-APR-1987  G. Rahal   adapted to CDC
C-   Updated   4-FEB-1988   Olivier Callot   
C-   Updated  13-JUL-1989   Qizhong Li-Demarteau    put in version # 
C-   Updated  28-AUG-1991   Qizhong Li-Demarteau    add a word for # of 
C-                                                  hits on Sense Wire
C
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:CDCLNK.INC/LIST'
      INCLUDE 'D0$LINKS:IZDLYR.LINK/LIST'
      INTEGER ILAYD, MPDLYR(5), ISETVN
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
      DATA MPDLYR / 0, 32, 32, 2, 2 /
C
C======================================================================
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL UCTOH( 'DLYR', MPDLYR(1), 4, 4 )
      ELSE
      ENDIF
C
C ****  Book DLYR bank for the specified layer.
C
      CALL MZLIFT(IXMAIN, LDLYR(ILAYD), LCDCH, -IZDLYR-ILAYD, MPDLYR, 0)
C
C ****  Set numeric bank ID
C
      IQ( LDLYR (ILAYD) - 5) = ILAYD
      IQ(LDLYR(ILAYD)) = ISETVN(IQ(LDLYR(ILAYD)),0)
  999 CONTINUE
      RETURN
      END
