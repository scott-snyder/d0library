      SUBROUTINE FCUNRUB(ILUN,IFILE,IERR)
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : Mark file IFILE in the file catalog on unit
C-      ILUN as valid.
C-      
C-   INPUTS  : 
C-   OUTPUTS : 
C-   CONTROLS: 
C-
C-   CREATED  10-OCT-1996   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      CHARACTER*200 CTEMP
      INTEGER ILUN,IFILE,IREPL,IERR,DATE,TIME,I
      LOGICAL LREPL,LDEL
C- External functions
      INTEGER LENOCC,ICLOC,ICFNBL
C----------------------------------------------------------------------

C- Get the record of interest from the already opened file catalog

      CALL FCGET(ILUN,IFILE,CFNTMP,CGNTMP,CTPTMP,CFCTMP,IERR)
      IF( IERR.NE.0 ) GOTO 901

C- Remove all occurances of either the delete or replace tags from the
C- start of the file name.

      CTEMP = CFNTMP
      I=ICFNBL(CTEMP)
      LREPL=ICLOC(FCREPL,LENOCC(FCREPL),CTEMP,I,LENOCC(CTEMP)).NE.0
      LDEL=ICLOC(FCDELF,LENOCC(FCDELF),CTEMP,I,LENOCC(CTEMP)).NE.0
      DO WHILE( LREPL .OR. LDEL )
        CTEMP = CTEMP(LENOCC(FCREPL)+7:LENOCC(CTEMP))
        I=ICFNBL(CTEMP)
        LREPL=ICLOC(FCREPL,LENOCC(FCREPL),CTEMP,I,LENOCC(CTEMP)).NE.0
        LDEL=ICLOC(FCDELF,LENOCC(FCDELF),CTEMP,I,LENOCC(CTEMP)).NE.0
      ENDDO
      CFNTMP=CTEMP

C- Rewrite the record

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
