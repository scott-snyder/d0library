        SUBROUTINE ERROR(EMSG)
C
C    Purpose:
CD   This module is used for reporting an error message. It also
CD   terminates the emulator and program.  The parameter is:
CD      EMSG --> is the character string of the error message.
CD   
C
C    A. Virgo, R. Heckelsberg
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 26-Oct-1988
CH   History:
CH      26-OCT-88  ATV  Define local variables
CH      21-Jun-88  ATV  Changed logical unit number to 6.
CH      10-Jun-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB        SEGINF-R
C
C    Calls:
CC        JRCLOS, JCLOSE, JEND
C
C    Next is the declaration of parameters passed to the subroutine/function.
      CHARACTER*(*) EMSG 
C
C    Then local declarations of variables (non-common variables).
C
      INTEGER NSEG, LUNERR
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST' 
C
C    Then executable code follows
C
C    First determine if a segment is open and type of segment then
C    close it.
C
      NSEG = SEGNUM
      IF (NSEG .GT. 0) THEN
         CALL JRCLOS
      ELSEIF (NSEG .EQ. 0) THEN
         CALL JCLOSE
      ENDIF
C
C    Terminate graphics output.
C
      CALL JEND
C
C    Echo the error message
C
      LUNERR = 99
      OPEN(UNIT=LUNERR,FILE='TT:',STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE (LUNERR,*)' **** '//EMSG//' ****'
      CLOSE(UNIT=10)
C
C    Terminate with system abort message.
C
      CALL EXIT(44)
C
C    Dummy return
C
      RETURN
      END
