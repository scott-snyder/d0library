      SUBROUTINE PRPVES(PRUNIT,LOC,NPVES,CFL,IFL)
C------------------------------------------------------------------
C-
C-  Print out PVES (V0) bank
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LOC= bank address
C-  NPVES = bank number
C-  CFL   = flag to control printout
C-          'ALL' for all banks
C-          'ONE' for one bank only
C-  IFL     not used
C-
C-   Created  11-JUL-1990   Daria Zieminska
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZPVES.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER PRUNIT,LOC,NPVES,NVEE,IFL
      CHARACTER CFL*(*)
      INTEGER LPVES,LPARH
      INTEGER K1,K2
      INTEGER NS,LVERT,NVERT,TRAK1,TRAK2,K
      INTEGER GZPVES,GZPARH
      LPVES=LOC
      IF(CFL.EQ.'ONE') THEN
        IF(LPVES.EQ.0) THEN
          IF(NPVES.EQ.0) THEN
            WRITE(PRUNIT,110) LPVES,NPVES
            GOTO 1000
          END IF
          LPVES=GZPVES(NPVES)
        ENDIF
      ENDIF
C
      IF(CFL.EQ.'ALL') THEN
        LPARH=GZPARH()
        IF(LPARH.NE.0) THEN
          LPVES=LQ(LPARH-IZPVES)
        ELSE
          LPVES=0
        ENDIF
      ENDIF
      IF(LPVES.EQ.0) THEN
        WRITE (PRUNIT,100)
        GO TO 1000
      ELSE
        WRITE(PRUNIT,101) IQ(LPVES+1)
      ENDIF
C
    1 IF(LPVES.GT.0) THEN
        NS=IQ(LPVES-2)
        NVERT=IQ(LQ(LPVES-NS-1)-5)
        TRAK1=IQ(LQ(LPVES-NS-2)-5)
        TRAK2=IQ(LQ(LPVES-NS-3)-5)
        NVEE=IQ(LPVES-5)
        WRITE(PRUNIT,102) NVEE,NVERT,TRAK1,TRAK2,(IQ(LPVES+K),K=2,5),
     &  Q(LPVES+6),Q(LPVES+7)
        WRITE(PRUNIT,103) (Q(LPVES+K),K=13,24)
        WRITE(PRUNIT,104) (IQ(LPVES+K),K=11,12),Q(LPVES+26),Q(LPVES+27),
     1  Q(LPVES+25)
C
        IF(CFL.NE.'ONE') THEN
          LPVES=LQ(LPVES)               ! pointer to next bank
          GOTO 1
        ENDIF
        IF(CFL.EQ.'ALL') THEN
          LPARH=LQ(LPARH)               ! check for additional headers
          IF(LPARH.GT.0) THEN
            LPVES=LQ(LPARH-IZPVES)
            GOTO 1
          ENDIF
        ENDIF
      ENDIF
  100 FORMAT(10X/' PRPVES:  No PVES bank.')
  101 FORMAT(/,'   PVES bank    Version #',I3)
  102 FORMAT(/,'   PVES    VERT   TRAK1  TRAK2  Status',
     1'  NDF    NM     MX     ECAL1     ECAL2',
     2/8I7,2F10.2)
  103 FORMAT(/,'   Ks hypothesis:',//,
     1'      P1      THETA1     PHI1       P2      THETA2 ',
     2'    PHI2 ',
     3'           P3             THETA3             PHI3',/,
     4 6F10.4,F10.2,'+-',F5.2,F10.4,'+-',F7.4,F10.4,'+-',F7.4)
  104 FORMAT(/,'   IEND   NSTEP    CHISQ      PROB     LENGTH',/,
     12I7,3F10.2)
  110 FORMAT('   PRPVES: wrong bank address',2I9)
 1000 RETURN
      END
