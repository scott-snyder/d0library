      SUBROUTINE ECPUSH(ILUN,IMIN,IMAX,IDELT,IDAT,NWPREC,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make space in a catalog by moving records
C-      IMIN to IMAX to positions IMIN+IDELT to IMAX+IDELT.
C-      Simple tests indicate that internal buffering increases the 
C-      throughput 5x over a read/write alogorithm without 
C-      internal buffering.  There are NWPREC longwords per physical
C-      record
C-
C-  *** THIS IS TIME CONSUMING AND SHOULD BE AVOIDED IF POSSIBLE ***
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-NOV-1993   John D Hobbs
C-   Revised  18-NOV-1993   John D Hobbs
C-     use internal buffer
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ILUN,IMIN,IMAX,IDELT,NWPREC,IDAT(*),IERR
      INTEGER I,J
C----------------------------------------------------------------------
C
      J=1
      DO I=IMAX,IMIN,-1
        CALL ECRRD(ILUN,I,IDAT(J),NWPREC,0,IERR)
        IF( IERR.NE.0 ) GOTO 998
        J=J+NWPREC
      ENDDO
C
      J=1
      DO I=IMAX,IMIN,-1
        CALL ECRWRT(ILUN,I+IDELT,IDAT(J),NWPREC,0,IERR)
        IF( IERR.NE.0 ) GOTO 998
        J=J+NWPREC
      ENDDO
C
 999  CONTINUE
      IERR=0
      RETURN
C
 998  CONTINUE
      IERR = -1
      RETURN
      END
