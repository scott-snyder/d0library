      SUBROUTINE FCRUB(ILUN,IFILE,IREPL,IERR)
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : Mark file IFILE in the file catalog on unit
C-      ILUN as invalid.
C-      
C-   INPUTS  : 
C-   OUTPUTS : 
C-   CONTROLS: 
C-
C-   CREATED  22-DEC-1993   John D Hobbs
C-   MODIFIED 11-JAN-1996   John D Hobbs - Allow DELETE tag '0DELE0'
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      CHARACTER*200 CTEMP
      INTEGER ILUN,IFILE,IREPL,IERR,LENOCC,DATE,TIME
      EXTERNAL LENOCC
C
      CALL FCGET(ILUN,IFILE,CFNTMP,CGNTMP,CTPTMP,CFCTMP,IERR)
      IF( IERR.NE.0 ) GOTO 901
      IF( IREPL.GE.0 ) THEN
        WRITE(CTEMP,1001) FCREPL,IREPL,CFNTMP(1:LENOCC(CFNTMP))
 1001   FORMAT(A,I6.6,A)
      ELSE
        CALL DATIME(DATE,TIME)
        WRITE(CTEMP,1001) FCDELF,DATE,CFNTMP(1:LENOCC(CFNTMP))
      ENDIF
      CFNTMP=CTEMP
      CALL FCPUT(ILUN,CFNTMP,CGNTMP,CTPTMP,CFCTMP,IFILE,IERR)
      IF( IERR.NE.0 ) GOTO 902
C
 999  CONTINUE
      RETURN
C
 901  CONTINUE
      IERR = -1
      RETURN
C
 902  CONTINUE
      IERR = -2
      RETURN
      END
