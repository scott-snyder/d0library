      SUBROUTINE CALZED(IETA,LAYER,CENZED,DZED,CENRAD,DRAD,TILT,ARGSOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns nominal position and width of a cal module
C-   in the EC.
C-
C-   Inputs  : IETA    Eta index
C-             LAYER   Layer index
C-   Outputs : CENZED  Central z value
C-             DZED    Full width in z
C-             CENRAD  Central r value of module
C-             DRAD    Full extent of r of module
C-             TILT    Inclination of plane normal to z
C-   Controls: ARGSOK  Non zero if layer and eta values not in EC
C-
C-   Created  28-DEC-1988   Michael W. Peters
C-   Revised  03-AUG-1989   Stephen Kahn  --  fill numbers from ZEBRA banks
C-            17-NOV-1989   Stephen Kahn  -- ICDs added
C-            28-DEC-1989   Stephen Kahn  -- use CEXIST common
C-            10-DEC-1990   Nobu Oshima -- Put CALL EZRSET!!!
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:SCPARR.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZCEDP.LINK'
      INCLUDE 'D0$LINKS:IZCETA.LINK'
      INCLUDE 'D0$LINKS:IZCLYR.LINK'
      INCLUDE 'D0$PARAMS:CLYR.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C
      LOGICAL FIRST, CEXIST
      INTEGER LAYER,IETA,ARGSOK
      REAL CENZED,DZED,Z1,Z2,ZS,CENRAD, DRAD, TILT, TILTOH, R, DR
      INTEGER NSBLAY, JSBLAY, JBYT, JLAYER
      INTEGER IER, IDIV, NDIV, INDX, I1, I2, I3, NSC
      INTEGER IETAA, IEC
      INTEGER MNETEM( 1: 7),MNETIH(11:15),MNETMH(11:15),MNETOH(15:17)
      INTEGER MXETEM( 1: 7),MXETIH(11:15),MXETMH(11:15),MXETOH(15:17)
      INTEGER ICD, ICDMIN, ICDMAX, MNOHMG, MXOHMG, MNEHMG, MXEHMG
      REAL R1, R2, REGRSS, ZICD(9:14), DZICD(9:14), TILTICD(9:14)
      REAL RICD1(9:14), RICD2(9:14)
      REAL Z1OH(15:17),Z2OH(15:17)
      REAL ROH, DROH, RICH, DRICH, RIFH, DRIFH, REM(MXLYEM)
      REAL  DREM(MXLYEM)
      REAL RMFH, DRMFH, RMCH, DRMCH
      REAL RCCMG, DRCCMG, REHMG, DREHMG, ROHMG, DROHMG, ZOHMG, DZOHMG
      REAL TLTOHMG
      CHARACTER EMVOL*11, NEC*1, ICDVOL*11, NICD*2
      CHARACTER*4 CHAR4,CHAS4
      INTEGER ICHAR4,ICHAS4
      EQUIVALENCE (ICHAR4,CHAR4)
      EQUIVALENCE (ICHAS4,CHAS4)
      DATA     CHAR4 /'TUBS'/
      DATA     CHAS4 /'CONS'/
C
      REGRSS(ZS, Z1, Z2, R1, R2) = R1 + (R2-R1)*(ZS-Z1)/(Z2-Z1)
C
      DATA MNETEM/15,15,14,14,14,14,14/,MNETIH/17,18,19,20,21/
      DATA MXETEM/35,35,35,26,26,26,35/,MXETIH/37,37,37,37,37/
      DATA MNETMH/11,12,13,13,14/,MNETOH/ 8, 9,11/
      DATA MXETMH/16,17,17,18,20/,MXETOH/12,13,14/
      DATA MNOHMG, MXOHMG/ 8, 10/, MNEHMG, MXEHMG / 11, 13 /
      DATA EMVOL /'EC_EEM+EM+0'/, ICDVOL /'ICD_ETA_00+' /
      DATA ICDMIN, ICDMAX / 9,14/
      DATA FIRST /.TRUE./
C
C---
C
      IF(FIRST) THEN            ! initialize constants from SRCP
        CALL EZPICK('SRCP_ECAL')  ! select calor end cap
        CALL EZGET('OH_DIVISIONS+Z', RVAL, IER)
        IF( IER .NE. 0) THEN    ! SRCP bank not found
          ARGSOK = 8
          GO TO 999
        END IF
        NDIV = IVAL(1)          ! number of OH groups
        INDX = 3
C
        DO 20  IDIV = 1, NDIV
          IF( MOD(IDIV, 4) .EQ. 1) Z1OH(IDIV/4+15) = RVAL(INDX+2)
          IF( MOD(IDIV, 4) .EQ. 0) Z2OH(IDIV/4+14) = RVAL(INDX+3)
          I1 = IVAL(INDX+4)
          I2 = IVAL(INDX+I1+5)
          I3 = IVAL(INDX+I1+I2+6)
          INDX = INDX + I1 + I2 + I3 + 6
   20   CONTINUE
C
        CALL EZGET('EC_OCH+1',RVAL,IER)
        IF(IER.NE.0) THEN       ! SRCP bank not found
          ARGSOK = 9
          GO TO 999
        END IF
        TILTOH = RVAL(13)*RADIAN       ! tilt of plane
C
        CALL EZGET('OH_BIG_MVOLUME+Z',RVAL,IER)
        IF(IER.NE.0) THEN       ! SRCP bank not found
          ARGSOK = 10
          GO TO 999
        END IF
        ROH = (RVAL(20)+RVAL(21))/2.
        DROH = RVAL(21)-RVAL(20)
C
        CALL EZGET('EC_EIC+C+01',RVAL,IER)
        IF(IER.NE.0) THEN       ! SRCP bank not found
          ARGSOK = 11
          GO TO 999
        END IF
        RICH = (RVAL(12)+RVAL(13))/2.
        DRICH = RVAL(13)-RVAL(12)
C
        CALL EZGET('EC_EIF+F+01',RVAL,IER)
        IF(IER.NE.0) THEN       ! SRCP bank not found
          ARGSOK = 11
          GO TO 999
        END IF
        RIFH = (RVAL(12)+RVAL(13))/2.
        DRIFH = RVAL(13)-RVAL(12)
C
        CALL EZGET('MH_MOTHER_VOLUME+Z',RVAL,IER)
        IF(IER.NE.0) THEN       ! SRCP bank not found
          ARGSOK = 11
          GO TO 999
        END IF
        RMCH = (RVAL(17)+RVAL(18))/2.
        DRMCH = RVAL(18)-RVAL(17)
        RMFH = (RVAL(17)+RVAL(18))/2.
        DRMFH = RVAL(18)-RVAL(17)
C
        DO 25 IEC = MNLYEM, MXLYEM
          IF(IEC .LE. 2) THEN
            WRITE(NEC,'(I1)') IEC
          ELSE IF (IEC .GE. LYEM3A .AND. IEC .LE. LYEM3D) THEN
            NEC = '3'
          ELSE IF (IEC .EQ. MXLYEM) THEN
            NEC = '4'
          END IF
          EMVOL(11:11) = NEC
          CALL EZGET(EMVOL,RVAL,IER)
          IF(IER .NE. 0) THEN !     SRCP bank not found
            ARGSOK = 12
            GO TO 999
          END IF
          REM(IEC) = (RVAL(12)+RVAL(13))/2.
          DREM(IEC)=  RVAL(13)-RVAL(12)
   25   CONTINUE
C
        DO 28 ICD = ICDMIN, ICDMAX
          WRITE (NICD, '(I2.2)') ICD
          ICDVOL(9:10) = NICD
          CALL EZGET(ICDVOL,RVAL,IER)
          IF( IER .NE. 0) THEN
            ARGSOK = 13
            GO TO 999
          END IF
          RICD1(ICD) = 0.5*(RVAL(13)+RVAL(14))
          RICD2(ICD) = 0.5*(RVAL(15)+RVAL(16))
          DZICD(ICD) = 2.*RVAL(12)
          ZICD(ICD) = RVAL(10)
   28   CONTINUE

        TILTICD(9) = ATAN((2.*(ZICD(11)-ZICD(9)))/
     +     (RICD1(9)+RICD2(9)-RICD1(11)-RICD2(11)))
        TILTICD(12) = ATAN((2.*(ZICD(14)-ZICD(11)))/
     +    (RICD1(12)+RICD2(12)-RICD1(14)-RICD2(14)))
        DZICD(9) = 0.33333*(DZICD(9)+DZICD(10)+DZICD(11))
        DZICD(12) = 0.33333*(DZICD(12)+DZICD(13)+DZICD(14))
        Z1 = REGRSS(RICD1(11),0.5*(RICD1(9)+RICD2(9)), 0.5*(RICD1(11)+
     +   RICD2(11)),ZICD(9),ZICD(11))
        Z2 = REGRSS(RICD1(14),0.5*(RICD1(12)+RICD2(12)),0.5*(RICD1(14)+
     +   RICD2(14)),ZICD(12),ZICD(14))
        R1 = MIN(RICD1(9),RICD2(9),RICD1(11),RICD2(11))
        R2 = MAX(RICD1(9),RICD2(9),RICD1(11),RICD2(11))
        RICD1(9) = R1
        RICD2(9) = R2
        R1 = MIN(RICD1(12),RICD2(12),RICD1(14),RICD2(14))
        R2 = MAX(RICD1(12),RICD2(12),RICD1(14),RICD2(14))
        RICD1(12) = R1
        RICD2(12) = R2
C
        DO 29 ICD = ICDMIN, ICDMIN+2
          TILTICD(ICD) = TILTICD(9)
          ZICD(ICD) = Z1
          DZICD(ICD) = DZICD(9)
          RICD1(ICD) = RICD1(9)
          RICD2(ICD) = RICD2(9)
          TILTICD(ICD+3) = TILTICD(12)
          ZICD(ICD+3) = Z2
          DZICD(ICD+3) = DZICD(12)
          RICD1(ICD+3) = RICD1(12)
          RICD2(ICD+3) = RICD2(12)
   29   CONTINUE
C
        CALL EZGET('EC_MHG+1',RVAL,IER)
        IF(IER.NE.0) THEN       ! SRCP bank not found
          ARGSOK = 11
          GO TO 999
        END IF
        REHMG = SQRT(RVAL(8)**2 + RVAL(9)**2)
        DREHMG = 2.*RVAL(15)
C
        CALL EZGET('EC_OHG+1',RVAL,IER)
        IF(IER.NE.0) THEN       ! SRCP bank not found
          ARGSOK = 11
          GO TO 999
        END IF
        ROHMG = SQRT(RVAL(8)**2 + RVAL(9)**2)
        TLTOHMG = RVAL(13)*RADIAN
        DROHMG = 2.0*RVAL(12)*COS(TLTOHMG)
        ZOHMG = RVAL(10) + 0.5*DROHMG*TAN(TLTOHMG)
        DZOHMG = 2.*RVAL(15)
C
        CALL EZPICK('SRCP_UCAL')          ! select UCAL SRCP
        CALL EZGET('CC_SOUTH_MASSLESS_GAP_VOLUME',RVAL,IER)
        IF(IER.NE.0) THEN
          ARGSOK = 14
          GO TO 999
        END IF
        RCCMG = 0.5*(RVAL(12)+RVAL(13))
        DRCCMG = RVAL(13)-RVAL(12)
C
      FIRST =.FALSE.
      CALL EZRSET
      CALL EZRSET
      ENDIF
C---
      IETAA=IABS(IETA)
      ARGSOK = 1
      IF(.NOT.CEXIST(IETA,1,LAYER)) GO TO 999
      IF( IETAA .LT. 14 .AND. LAYER .LE.MXLYEM) GO TO 999
      IF( IETAA .LT. 11 .AND. LAYER .LE.MXLYFH .AND. LAYER .GE. MNLYFH)
     &   GO TO 999
      IF( IETAA .LT. 7 .AND. LAYER .LE. MXLYCH) GO TO 999
      IF(IETA.EQ. 14.AND.(LAYER.EQ.3.OR.LAYER.EQ.4)) GO TO 999
      IF(IETA.EQ.-14.AND.(LAYER.EQ.5.OR.LAYER.EQ.6)) GO TO 999
C
      IF(IETA .LT. 0 .AND. LAYER .GE. LYEM3A .AND. LAYER .LE. LYEM3D
     & .AND. IETAA .LE. 26) THEN
        JLAYER = MOD(LAYER-1,4) + 3 ! symmetry for negative FLOOR 3
      ELSE
        JLAYER = LAYER            ! need not worry about LY3 symmetry flip
      END IF
C
      LQCEDP = LC(LCGEH- IZCEDP)    ! pointer to tower dispatching bank
C
      LQCETA = LC(LQCEDP - IZCETA - IETAA + 1)    ! pointer to constant
C                                        ! eta bank
      LQCLYR = LC(LQCETA - IZCLYR - JLAYER + 1)   ! pointer to first
C                                        ! appropriate layer bank
      IF( LQCEDP .EQ. 0 .OR. LQCETA .EQ. 0 .OR. LQCLYR .EQ. 0)GO TO 999
C
      IF( (LAYER.GE.MNLYEM .AND. LAYER.LE.MXLYEM .AND.           ! EM
     &  IETAA.GE.MNETEM(LAYER).AND.IETAA.LE.MXETEM(LAYER)) .OR.
     +  (LAYER .GE. MNLYFH .AND. LAYER .LE. MNLYCH .AND.         ! IFH
     +  IETAA .GE. MNETIH(LAYER) .AND. IETAA .LE. MXETIH(LAYER)) .OR.
     +  (LAYER .GE. MNLYFH .AND. LAYER .LE. MNLYCH .AND.         ! MFH
     +  IETAA .GE. MNETMH(LAYER) .AND. IETAA .LE. MXETMH(LAYER))) THEN
C
        NSBLAY = JBYT(C(LQCLYR), JBNSBC, NBNSBC)
        IF(NSBLAY .LE. 1) THEN                ! single sub-cell
C ...  PATCH TO PROTECT AGAINST EMPTY BANK (TEMPORARY)   ...
          IF( IC(LQCLYR+ICELID) .EQ. 0) THEN
            LQCLYR = LC(LQCLYR)
          END IF
C ...   ....     ....     ....    ....    ....    ....    ....
          CENZED = C(LQCLYR+ICZ)              ! mid point of sub-cell
          IF( IC(LQCLYR+ICSHAP) .EQ. ICHAR4 ) THEN
            DZED = 2.*C(LQCLYR+ICPAR3)        ! cell extent in Z
          ELSE IF (IC(LQCLYR+ICSHAP) .EQ. ICHAS4) THEN
            DZED = 2.*C(LQCLYR+ICPAR1)        ! cell extent in Z
          ELSE                                ! other shapes should not
            DZED = -999.                      ! be here
            ARGSOK = 5
          END IF
        ELSE                                  ! more than one sub-cell
C
C     LOOP ON SUB-CELLS TO FIND FIRST AND LAST
C
   30     IF(LQCLYR .EQ. 0 ) GO TO 80
          JSBLAY = JBYT( C(LQCLYR), JBSBCL, NBSBCL)
          IF( JSBLAY .EQ. 0) GO TO 60
          IF( JSBLAY .EQ. 1) THEN             ! first sub cell
            ZS = 0
            IF( IC(LQCLYR+ICSHAP) .EQ. ICHAR4) THEN
              Z1 = C(LQCLYR+ICZ) - C(LQCLYR+ICPAR3)  ! low edge of Z
            ELSE IF( IC(LQCLYR+ICSHAP) .EQ. ICHAS4) THEN
              Z1 = C(LQCLYR+ICZ) - C(LQCLYR+ICPAR1)  ! low edge of Z
            ELSE
              Z1 = -999.
              ARGSOK = 6
            END IF
          END IF
          IF( IC(LQCLYR+ICSHAP) .EQ. ICHAR4) THEN
            Z2 = C(LQCLYR+ICZ) + C(LQCLYR+ICPAR3)  ! low edge of Z
          ELSE IF( IC(LQCLYR+ICSHAP) .EQ. ICHAS4) THEN
            Z2 = C(LQCLYR+ICZ) + C(LQCLYR+ICPAR1)  ! low edge of Z
          ELSE
            Z2 = -999.
            ARGSOK = 6
          END IF
C
          Z2 = MAX(Z2,ZS)
          ZS = Z2
   60     CONTINUE
          LQCLYR = LC(LQCLYR)
          GO TO 30
   80     CONTINUE
          CENZED=(Z1+Z2)/2.
          DZED=Z2-Z1
        END IF
C
        IF(LAYER.GE. MNLYEM .AND. LAYER .LE. MXLYEM .AND. IETAA .GE.
     +     MNETEM(LAYER) .AND. IETAA .LE. MXETEM(LAYER)) THEN  ! EM
          CENRAD = REM(LAYER)
          DRAD = DREM(LAYER)
        ELSE IF (LAYER .GE. MNLYFH .AND. LAYER .LE. MXLYFH .AND.
     +     IETAA.GE. MNETIH(LAYER)  .AND. IETAA .LE. MXETIH(LAYER) )
     +     THEN
          CENRAD = RIFH
          DRAD = DRIFH
        ELSE IF (LAYER .GE. MNLYFH .AND. LAYER .LE. MXLYFH .AND.
     +     IETAA.GE.MNETMH(LAYER) .AND. IETAA .LE. MXETMH(LAYER)) THEN
          CENRAD = RMFH
          DRAD = DRMFH
        ELSE IF (LAYER .EQ. MNLYCH .AND. IETAA.GE. MNETIH(LAYER)
     +     .AND. IETAA .LE. MXETIH(LAYER) ) THEN
          CENRAD = RIFH
          DRAD = DRIFH
        ELSE IF (LAYER .EQ. MNLYCH .AND. IETAA.GE.MNETMH(LAYER)
     +    .AND. IETAA .LE. MXETMH(LAYER)) THEN
          CENRAD = RMFH
          DRAD = DRMFH
        END IF
        TILT = 0.
C
      ELSE IF(LAYER.GE.15.AND.LAYER.LE.17.AND.
     &  IETAA.GE.MNETOH(LAYER).AND.IETAA.LE.MXETOH(LAYER)) THEN  ! OH
        CENZED=(Z1OH(LAYER)+Z2OH(LAYER))/2.
        DZED=Z2OH(LAYER)-Z1OH(LAYER)
        CENRAD=ROH
        DRAD = DROH
        TILT = TILTOH
C
      ELSE IF (LAYER .EQ. LYICD .AND. IETAA .GE. ICDMIN .AND. IETAA .LE.
     &  ICDMAX) THEN                        ! ICD
        TILT = TILTICD(IETAA)
        CENRAD = 0.5*(RICD1(IETAA) + RICD2(IETAA))
        DRAD = ABS(RICD2(IETAA)- RICD1(IETAA))
        DZED = DZICD(IETAA)
        CENZED = ZICD(IETAA)
C
      ELSE IF (LAYER .EQ. MNLYMG) THEN      ! CC MG
        TILT = 0.
        CENRAD = RCCMG
        DRAD = DRCCMG
        CENZED = C(LQCLYR+ICZ)
        DZED = 2.*C(LQCLYR+ICPAR3)
C
      ELSE IF (LAYER .EQ. MXLYMG .AND. IETAA .GE. MNEHMG .AND. IETAA
     &  .LE. MXEHMG) THEN                   ! MH MG
        TILT = 0.
        CENRAD = REHMG
        DRAD = DREHMG
        CENZED = C(LQCLYR+ICZ)
        DZED = 2.*C(LQCLYR+ICPAR3)
C
      ELSE IF (LAYER .EQ. MXLYMG .AND. IETAA .GE. MNOHMG .AND. IETAA
     &  .LE. MXOHMG) THEN                   ! OH MG
        TILT = TLTOHMG
        CENRAD = ROHMG
        DRAD = DROHMG
        CENZED = ZOHMG
        DZED = DZOHMG
C
      ELSE
        ARGSOK=2
        GO TO 999
      ENDIF
      IF(IETA.LT.0) THEN
        CENZED=-CENZED
        TILT = -TILT
      ENDIF
      ARGSOK=0
C----------------------------------------------------------------------
  999 RETURN
      END
