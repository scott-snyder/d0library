      SUBROUTINE ECRWRT(ILUN,IREC,IDAT,ILEN,ISWAP,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write the data in IDAT(1:ILEN) to the
C-    IREC-th record of the direct access file on unit ILUN.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-NOV-1993   John D Hobbs
C-   Modified 16-JAN-1995   John D Hobbs - Byteswap on output if 
C-     being done on input (Allows updating on different system than
C-     creation)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ILUN,IREC,ILEN,IDAT(1:ILEN),ISWAP,IERR
      IF( ISWAP.NE.0 ) CALL VXINVB(IDAT,ILEN)
      WRITE(ILUN,REC=IREC,ERR=998) IDAT
 999  CONTINUE
      IERR=0
      RETURN
 998  CONTINUE
      RETURN
      END
