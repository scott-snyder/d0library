      SUBROUTINE PRGTRH (LUNOUT)
C ----------------------------------------------------------------------
C -
C -   PURPOSE AND METHODS : PRINT OF TRD ZEBRA BANK GTRH
C -
C -   INPUTS  :
C -   OUTPUTS :
C -
C-   Created  11-JAN-1988   A. ZYLBERSTEJN
C-   Updated  30-AUG-1989   A. Zylberstejn  Introduce logical unit for print
C-                                          as argument of the routine 
C-   Updated  19-JAN-1990   A. Zylberstejn  : Modify incorrect reference to
C-                                           LGTLY
C-   Updated  24-AUG-1990   A. Zylberstejn  :Correct call to NZBANK 
C-                                            (IXCOM  instead of IXMAIN) 
C-
C-
C ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,LUNOUT,NZBANK
C
C      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
C      INCLUDE 'D0$INC:GTRHLN.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER LGTRH,GZGTRH,LGTLY
C
      LGTRH = GZGTRH()
      IF(LGTRH.LE.0)THEN
        WRITE(LUNOUT,*)' PROBLEM_TRD IN PRGTRH: BANK LGTRH NOT BOOKED'
        GO TO 999
      END IF
      WRITE(LUNOUT,1010)
      WRITE(LUNOUT,1004)
      WRITE(LUNOUT,*)'                  I LAYER 1 I LAYER 2 I LAYER 3 I'
      WRITE(LUNOUT,*)'------------------I---------I---------I---------I'
        WRITE(LUNOUT,1000)NZBANK(IXCOM,LQ(LGTRH-1)),
     +                  NZBANK(IXCOM,LQ(LGTRH-2)),
     +                  NZBANK(IXCOM,LQ(LGTRH-3))
      DO 60 I=1,3
        LGTLY=LQ(LGTRH-I)
        IF(LGTLY.LE.0)THEN
          CALL INTMSG(' PROBLEM_TRD IN PRGTRH: BANK LGTLY NOT BOOKED')
          GO TO 999
        END IF
        CALL PRGTLY(LUNOUT,I)
   60 CONTINUE
  999 RETURN
 1000 FORMAT(' NUMBER OF HITS ','   I',3(I7,'  I'))
 1004 FORMAT('          BANK GTRH:GEANT TRD USER HITS')
 1010 FORMAT(' -----------------------------------------------------')
        END
