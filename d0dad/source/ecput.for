      SUBROUTINE ECPUT(ILUN,IR,IE,IMSK,IFID,IZRN,IZBO,IFLAG,IERR)
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : Overall control for writing event records
c-      to an event catalog.
C-
C-   INPUTS  : 
C-   OUTPUTS : 
C-   CONTROLS: 
C-
C-   CREATED   8-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER ILUN,IR,IE,IMSK(*),IFID,IZRN,IZBO,IFLAG,IERR
      INTEGER IDAT(JSIZE)
C----------------------------------------------------------------------
C
C  Make sure the unit/bank correspondance is correct
C
      CALL ECLSET(ILUN,IERR)
      IF( IERR.NE.0 ) THEN
         IERR = -4
         GOTO 999
      ENDIF
C
C  Check for use of internal storage for sorting prior to insertion...
C
      IF( IFLAG.EQ.0 ) THEN
         WRITE(*,*) ' ECPUT: ILLEGAL FLAG VALUE=',IFLAG
CJDH         CALL ECWRT(ILUN,IR,IE,IMSK,IFID,IZRN,IZBO,IERR)
      ELSE
         IDAT(1)=IE
         IDAT(2)=IMSK(1)
         IDAT(3)=IMSK(2)
         IDAT(4)=IFID
         IDAT(5)=IZRN
         IDAT(6)=IZBO
         CALL ECINT(IR,IDAT,6,IERR)
         IF( IERR.NE.0 ) THEN
            WRITE(*,9001) IERR,IR,IE
 9001       FORMAT(' ECPUT: Error ',I4,' from ECINT for R/E',2I8)
            IERR = -1
            GOTO 999
         ENDIF
      ENDIF
C
  999 CONTINUE
      RETURN
      END
