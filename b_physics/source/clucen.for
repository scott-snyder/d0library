      SUBROUTINE CLUCEN(LPELC,POINT,EPOINT,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get electromagnetic cluster center coordinates 
C-   and errors
C-
C-   Inputs  : LPELC PELC bank location
C-   Outputs : POINT(3), EPOINT(3) cluster center position and errors
C-   Controls: 
C-
C-   Created  12-JUL-1993   Andrzej Zieminski, Daria Zieminska
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER LPELC,LCALC,IER,IERR,I,NCVAR,IT,STATUS
      REAL ECAL,EVAR(50)
      REAL POINT(3),EPOINT(3),ERCAL1,ERCAL2,ERCAL3
      LOGICAL FIRST,OK
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL EZPICK('ELFIT_RCP')
        FIRST=.FALSE.
        CALL EZGET('ERCAL1',ERCAL1,IER)
        CALL EZGET('ERCAL2',ERCAL2,IER)
        CALL EZGET('ERCAL3',ERCAL3,IER)
        CALL EZRSET
      END IF
C
      LCALC=LQ(LPELC-2)
      IF(LCALC.LE.0) THEN
        IERR=20
        GOTO 999
      ENDIF
      IERR=0
C
      ECAL=Q(LPELC+6)
      DO I=1,3
!        POINT(I)=Q(LCALC+13+I)
        EPOINT(I)=ERCAL1*(ERCAL2/ECAL)**ERCAL3
      ENDDO
C
C  Get POINT from CLEANEM
C
      CALL CLEANEM(LPELC,IT,OK,STATUS)
      CALL CLEANEM_CQUANS(NCVAR,EVAR)
      CALL UCOPY(EVAR(6),POINT(1),3)
C
  999 RETURN
      END
