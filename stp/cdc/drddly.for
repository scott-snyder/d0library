      SUBROUTINE DRDDLY
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : read Delay line velocities and T0's from
C-                         Guido's file into STP banks.
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  18-MAR-1991   Qizhong Li-Demarteau
C-   Modified 15-SEP-1992   Domenico Pizzuto  Fill DCBD with DL nonlinearity
C-                                            correction coeeficients.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LUN, IERR
      INTEGER HMOD, NDL(8), SEC, I, LAY, IDL, JDL, NDLY, NWDCBD 
      INTEGER PLDTMD, GZDTMD, IP, NBDELY, NWDELY, PLDCBD
      PARAMETER (NBDELY = 4)
      PARAMETER (NWDELY = 4)
      REAL    DLVELS(8), DLVELH(8), TZEROS(8), TZEROH(8), C2(8), C3(8)
      REAL    VELDLS(8,0:31), VELDLH(8,0:31)
      REAL    T0DLSW(8,0:31), T0DLHV(8,0:31)
      REAL    CC2(8,0:31)   , CC3(8,0:31)            

      LOGICAL OK
      CHARACTER*11 FILNAM
      DATA  FILNAM/'DELAY_GUIDO'/
C----------------------------------------------------------------------
C
C  open the delay line file
C
      CALL GTUNIT(1,LUN,IERR)
      CALL D0OPEN(LUN,FILNAM,'IF',OK)
      IF (.NOT. OK) THEN
          CALL ERRMSG('CDWSTP','DRDDLY',
     &    'Unable to open input file DELAY_GUIDO.DAT','W')
          GOTO 998
        ENDIF
C
      READ (LUN,1000)
 1000 FORMAT (10X)
C
      DO 110 SEC = 0, 31
        DO 101 I = 1, 8
  100     READ (LUN,1001,END=200) 
     &  HMOD,NDL(I),DLVELS(I),DLVELH(I),TZEROS(I),TZEROH(I),C2(I),C3(I)
 1001     FORMAT (1X,2I3,2X,2F8.5,2F8.1,2E11.3)
  101   CONTINUE
        IF (HMOD .EQ. SEC) THEN
          CALL VZERO(VELDLS(1,SEC),8)
          CALL VZERO(VELDLH(1,SEC),8)
          DO 210 NDLY = 1, 8
            VELDLS(NDLY,SEC) = DLVELS(NDLY) 
            VELDLH(NDLY,SEC) = DLVELH(NDLY) 
            T0DLSW(NDLY,SEC) = TZEROS(NDLY)
            T0DLHV(NDLY,SEC) = TZEROH(NDLY)
            CC2(NDLY,SEC)    = C2(NDLY)
            CC3(NDLY,SEC)    = C3(NDLY)
  210     CONTINUE
        ELSE
          WRITE(71,*) '  NO DL INFORMATION FOR SECTOR:',SEC
        ENDIF
  110 CONTINUE
  200 CONTINUE
C
C- Fill DTMD with delay line velocities and T0s.
C- Fill DCBD with delay line nonlinearity function correction coefficients.
      DO 300 LAY = 0, 3
        PLDTMD = GZDTMD(LAY)
        PLDCBD = LC (PLDTMD-1)
        NWDCBD = IC (PLDCBD+4)
         DO 400 SEC = 0, 31
          DO 500 JDL = 1, 2
            IP = PLDTMD + (SEC*NBDELY + (JDL-1)*2) * NWDELY + 4
            IDL = LAY*2 + JDL
            C(IP+1) = T0DLHV(IDL,SEC)
            C(IP+2) = VELDLH(IDL,SEC)
            IP = IP + NWDELY
            C(IP+1) = T0DLSW(IDL,SEC)
            C(IP+2) = VELDLS(IDL,SEC)
C-Insert nonlinearity correction coefficients in DCBD
            IP = PLDCBD+(SEC*2+(JDL-1))*NWDCBD+5
            C (IP+1) = CC2(IDL,SEC)
            C (IP+2) = CC3(IDL,SEC)
  500     CONTINUE
  400   CONTINUE
  300 CONTINUE
C
  999 RETURN
C
  998 WRITE(6, 2000)
 2000 FORMAT(5X,' error opening the DL T0 and Velocity file ')
      GOTO 999
      END
