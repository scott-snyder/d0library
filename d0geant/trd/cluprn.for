      SUBROUTINE CLUPRN(A,B)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : PRINT THE TRD CLUSTERS
C-
C-   Inputs  : A= 'AFTER " OR 'BEFORE' ,B= NAME OF THE CALLING ROUTINE
C-   Outputs : 
C-   Controls: 
C-
C-   Created                A. ZYLBERSTEJN
C-
C----------------------------------------------------------------------
C  
      IMPLICIT NONE
      INCLUDE 'D0$INC:CLUSM.INC/LIST'
      INCLUDE 'D0$INC:CLUSTR.INC'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      CHARACTER*6 A,B
      INTEGER I
C
      WRITE(LOUT,*)'CLUSTERS NUMBER',NSMEAR,A,' ',B
      IF(NSMEAR.LE.0)RETURN
      WRITE(LOUT,*)' ENERGIES'
      WRITE(LOUT,1000)(ECLES(I),I=1,NSMEAR)
      WRITE(LOUT,*)' X POSITIONS'
      WRITE(LOUT,1002)(XCLES(I),I=1,NSMEAR)
      IF (B.EQ.'CLUDEL') GO TO 900
      WRITE(LOUT,*)' IESCAPE'
      WRITE(LOUT,1006)(IESCAP(I),I=1,NSMEAR)
      IF(TIMEC(1).NE.0.)THEN
        WRITE(LOUT,*)' DRIFT TIMES'
        WRITE(LOUT,1004)(TIMEC(I),I=1,NSMEAR)
      END IF
      IF(YCLES(1).NE.0.)THEN
        WRITE(LOUT,*)' Y POSITIONS'
        WRITE(LOUT,1002)(YCLES(I),I=1,NSMEAR)
      END IF
      IF(ZCLES(1).NE.0)THEN
        WRITE(LOUT,*)' Z POSITIONS'
        WRITE(LOUT,1002)(ZCLES(I),I=1,NSMEAR)
      END IF
      IF(IWIRE(1).NE.0)THEN
        WRITE(LOUT,*)' HIT WIRES'
        WRITE(LOUT,1006)(IWIRE(I),I=1,NSMEAR)
C      IF(ZCLES(1).NE.0)THEN
        WRITE(LOUT,*)' ISTRIP'
        WRITE(LOUT,1006)(ISTRIP(I),I=1,NSMEAR)
C      END IF
C      IF(ZCLES(1).NE.0)THEN
        WRITE(LOUT,*)' DSTRIP'
        WRITE(LOUT,1002)(DSTRIP(I),I=1,NSMEAR)
C      END IF
      END IF
 1000 FORMAT(10F7.2)
 1002 FORMAT(10F7.3)
 1004 FORMAT(10F7.0)
 1006 FORMAT(10I7)
  900 RETURN
      END
