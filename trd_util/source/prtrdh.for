      SUBROUTINE PRTRDH (LUNOUT)
C ----------------------------------------------------------------------
C -
C -   PURPOSE AND METHODS : PRINT OF TRD ZEBRA BANK TRDH
C -
C -   INPUTS  :LUNOUT= Output logical number
C -   OUTPUTS :
C -
C-   Created  11-JAN-1988   A. ZYLBERSTEJN
C-   Updated  15-JUN-1989   A. Zylberstejn   
C-   Updated  30-AUG-1989   A. Zylberstejn  Introduce logical unit for print
C-                                          as argument of the routine 
C-
C-
C ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,LL,LUNOUT,ND,IW,NZBANK
C      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
c      INCLUDE 'D0$INC:TRDLNK.INC/LIST'
      INTEGER   GZTRDH,       LTRDH,NWHIT(6)
C
C      ------------------------------------------------------------------
      LTRDH=GZTRDH()
      IF (LTRDH.LE.0)THEN
        CALL INTMSG(' Problem_trd in PRTRDH: TRDH bank not booked')
        RETURN
      END IF
      WRITE(LUNOUT,1010)
      WRITE(LUNOUT,1004)
      CALL VZERO(NWHIT,6)
      DO 10 I=1,3
        LL=LQ(LTRDH-I)
  6     CONTINUE
        IF(LL.LE.0)GO TO 10
        ND=IQ(LL-1)
        IW=IQ(LL+ND)
        IF(IW.GT.1000)THEN
          NWHIT(I+3)=NWHIT(I+3)+1
        ELSE
          NWHIT(I)=NWHIT(I)+1
      END IF
        LL=LQ(LL)
        GO TO 6
  10  CONTINUE
      WRITE(LUNOUT,990)
 990  FORMAT(20X,' LAYER 1  LAYER 2  LAYER 3  LAYER 4  LAYER 5',
     &   ' LAYER 6')
      WRITE(LUNOUT,1000)(NWHIT(I),I=1,6)
      DO 60 I=1,3
        LL=LQ(LTRDH-I)
        IF(LL.GT.0)CALL PRTLYR(LUNOUT,I)
   60 CONTINUE
  999 RETURN
 1000 FORMAT(' NUMBER OF HITS ',6I8)
 1004 FORMAT(' PRINT OF BANK TRDH:GEANT TRD IDEALIZED HITS',/,
     +       ' -------------------------------------------')
 1010 FORMAT(' -----------------------------------------------------')
      END
