      SUBROUTINE GTMSTC(NMOD,NCEL,TZER,TSLP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Supply ADC to Time calibration Constants
C-
C-   Inputs  : NMOD     Module ID
C-             NCEL     Cell Number
C-   Outputs : TZER(2)  T0 Constants
C-             TSLP(2)  Slope of ADC vs Time
C-   Controls:
C-
C-   Created   4-NOV-1993   B.S.Acharya
C-   Updated   7-FEB-1994   A.T. for preliminary calib const.
C-   Updated  31-OCT-1994   Tao Hu  :  add octant 4 and 7 .
C-   Updated  17-NOV-1994   DW  support compressed format
C-   Updated   2-FEB-1995   RM  add reasonable default T0 values and error
C-                          message if no MSTC bank found
C-   Updatae   3-MAR-1995   RM  remove call to MU_SCINT_MOD
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C-Arguements
      INTEGER NMOD,NCEL
      REAL    TZER(2),TSLP(2)
C-Includes--
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'     ! D0 ZEBRA bank.
C- Local variables
      INTEGER LMSTC,GZMSTC,GZMSTC_R
      INTEGER NPMT,I, K,ICTYPE
      LOGICAL ERR_OUT / .FALSE. /
C- Preliminary measurement for modules 200-247
      REAL    SLP(50)
      REAL    ZER(50)
C
      DATA SLP / 6.8,6.9,6.6,6.7,6.8,0,0,6.8,0,0,
     1           6.9,6.6,6.8,6.6,6.8,0,0,6.8,0,0,
     2           6.9,6.9,7.0,6.8,6.8,0,0,6.8,0,0,
     3           6.9,6.9,6.7,6.7,6.8,0,0,6.8,0,0,
     4           6.7,6.5,6.6,6.6,6.8,0,0,6.8,0,0 /
      DATA ZER / 693., 712., 681., 710., 1004., 0,0, 800., 0,0,
     1           985., 941., 964., 968., 950., 0,0, 800., 0,0,
     2           963., 968., 975., 955., 918., 0,0, 800., 0,0,
     3           976., 984., 976., 976., 961., 0,0, 800., 0,0,
     4           699., 683., 735., 706., 966., 0,0, 800., 0,0 /
C----------------------------------------------------------------------
C
      IF ( IQ(LHEAD+1).GT.1000 ) THEN      ! Monte Carlo
        DO I=1,2
          TZER(I)=1000.0
          TSLP(I)=0.333
        END DO
        GOTO 999
      END IF
C
C-- Set TZER and TSLP to default values
C
      DO I=1,2
        TZER(I)=780.0
        TSLP(I)=0.15
      ENDDO
C
      IF ( NMOD.GE.200 .AND. NMOD.LE.247 ) THEN
        K = NMOD - 199
        IF (ZER(K).NE.0.0) THEN
          DO I=1,2
            TZER(I) = ZER(K)
            TSLP(I) = 1.0/SLP(K)
          END DO
        END IF
      END IF
C
C                 --- Convert Cell No (3,7..63) to PMT number (0:15)
C
      NPMT=NCEL/4
C
C           --  Fetch Constants from the Current Set for two consecutive PMT's
C
      LMSTC=GZMSTC(NMOD)
      IF(LMSTC.NE.0)THEN
        ICTYPE=IC(LMSTC+1)
        IF(MOD(ICTYPE,100).GE.10) THEN
C           compress (L2) format
          TZER(1)=C(LMSTC+17+NPMT*2)
          TSLP(1)=C(LMSTC+18+NPMT*2)
          IF(NPMT.LT.15)THEN
            TZER(2)=C(LMSTC+17+NPMT*2+2)
            TSLP(2)=C(LMSTC+18+NPMT*2+2)
          ENDIF
        ELSE
          TZER(1)=C(LMSTC+17+NPMT*4)
          TSLP(1)=C(LMSTC+18+NPMT*4)
          IF(NPMT.LT.15)THEN
            TZER(2)=C(LMSTC+17+NPMT*4+4)
            TSLP(2)=C(LMSTC+18+NPMT*4+4)
          ENDIF
        ENDIF
C
C           --  If the Constants are 0 Set the Constants to Average Values
C
        DO I=1,2
          IF(TZER(I).EQ.0)TZER(I)=C(LMSTC+13)
          IF(TSLP(I).EQ.0)TSLP(I)=C(LMSTC+15)
        ENDDO
      ELSE
        IF (.NOT. ERR_OUT) THEN
          CALL ERRMSG('No MSTC bank - use defaults','GTMSTC',' ','I')
          ERR_OUT = .TRUE.
        ENDIF
C
C           --  IF no Current Set of Constants fetch from Reference Set
C
        LMSTC=GZMSTC_R(NMOD)
        IF(LMSTC.NE.0)THEN
          TZER(1)=C(LMSTC+17+NPMT*4)
          TSLP(1)=C(LMSTC+18+NPMT*4)
          IF(NPMT.LT.15)THEN
            TZER(2)=C(LMSTC+17+NPMT*4+4)
            TSLP(2)=C(LMSTC+18+NPMT*4+4)
          ENDIF
C
C           --  If the Constants are 0 Set the Constants to Average Values
C
          DO I=1,2
            IF(TZER(I).EQ.0)TZER(I)=C(LMSTC+13)
            IF(TSLP(I).EQ.0)TSLP(I)=C(LMSTC+15)
          ENDDO
        ENDIF
      ENDIF
C
  999 CONTINUE
C
      RETURN
      END
