      SUBROUTINE PRSMNT(UNIT,LSMNT,NSMNT,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print contents of SMNT bank
C-
C-   Inputs  : UNIT  - Unit number for printout
C-             LSMNT - Bank address
C-             NSMNT - Module number ( if 0, print all mods )
C-             CFL   - Print OLD/NEW/CURRENT/ALL bank 
C-             IFL   - How  much to print
C-   Outputs : 
C-   Controls: 
C-
C-   Created  9-MAY-1991   O.Eroshin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER UNIT,LSMNT,NSMNT,IFL
      CHARACTER CFL*(*)
      INTEGER I,II,LL
C
      WRITE(UNIT,100)  
  100 FORMAT('0    SAMUS MINIMUM TIMES BANK '/)
      WRITE(UNIT,101) (IC(LSMNT+I),I=1,10)
101   FORMAT(' TYPE     ',I6,' STATUS   ',I6,' QUALITY       ',I6/
     A         ' LOW RUN  ',I6,' HIGH RUN ',I6,' RUN GENERATED ',I6/
     A         ' DATE GENERATED ',I10,' TIME GENERATED ',I10/
     A         ' SECTION NAME   ',A2,' NO. CHANNELS ',I4)
      WRITE(UNIT,102)
102   FORMAT(
     &  '  MINIMUM TIME/#0  SIGMA/#0    MINIMUM TIME/#1  SIGMA/#1'/)
      LL = LSMNT+14
      DO I=1,IC(LSMNT+10)
        WRITE(UNIT,103) (C(LL+II),II=1,4)
  103   FORMAT(4F10.2)
        LL = LL+4
      END DO
C
      RETURN
      END
