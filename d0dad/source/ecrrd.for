      SUBROUTINE ECRRD(ILUN,IREC,IDAT,ILEN,ISWAP,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read the first ILEN words of data in physical
C-    record IREC on unit ILUN into IDAT(1:ILEN).
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-NOV-1993   John D Hobbs
C-   Modified 19-DEC-1994   JDH - Set error return code.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ILUN,IREC,ILEN,IDAT(1:ILEN),IERR,ISWAP
      READ(ILUN,REC=IREC,ERR=998) IDAT
      IF( ISWAP.NE.0 ) CALL VXINVB(IDAT,ILEN)
 999  CONTINUE
      IERR=0
      RETURN
 998  CONTINUE
      IERR = -1
      RETURN
      END
