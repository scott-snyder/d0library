      SUBROUTINE FDGETD(HALF, QUAD, SECTOR, NDLHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find all delay line  hits on one sector,
C-               and fill banks FTSC with corresponding information.
C-
C-   Inputs  : HALF, QUAD, SECTOR = FDC Half,Quadrant, and Sector
C-   Outputs : NDELHIT = Number of hits with associated DL measurements.
C-              Bank FTSC is updated with delay line information
C-
C-   Created   4-NOV-1988   Jeffrey Bantly  modified from CDC equivalent
C-   Updated  20-MAR-1990   Jeffrey Bantly  general cleanup
C-   Updated  23-JUL-1990   Jeffrey Bantly  fix single end solutions
C-   Updated   5-NOV-1990   Jeffrey Bantly  alter delay length readin and
C-                                          demand positive times for match
C-   Updated  20-DEC-1990   Susan K. Blessing   Do not need positive times
C-    due to errors in finding timing pulse times.
C-   Updated   1-MAY-1991   Robert E. Avery  Clean up, get tzero's from STP
C-                                      and fix single end solutions again.
C-   Updated  26-JUN-1991   Robert E. Avery  Use lengths from STP bank.
C-   Updated  26-JUN-1991   Robert E. Avery  remove lvldbg parameter
C-   Updated  24-OCT-1991   Robert E. Avery  Put in correct error via RCP.
C-   Updated   4-NOV-1991   Robert E. Avery  VAX intrinsic functions fix
C-   Updated  27-JAN-1992   Susan K. Blessing   Only use delay line hits
C-    once and use the best combinations.  Change lots of variable names
C-    to try to make things clearer.  Allow single DL hits which are
C-    slightly ambiguous if SINGLE_DL is TRUE.
C-   Updated  12-MAY-1992   Susan K. Blessing  Remove FDEVNT.INC and 
C-    FDRESF.PARAMS. 
C-   Updated   1-MAR-1993   Susan K. Blessing  Remove PRODUC.  
C-   Updated  21-OCT-1993   Robert E. Avery  Count number of associated 
C-      DL hits. 
C-      Only retain 20 micron precision (for consistancy with FHIT bank):
C-   Updated  17-DEC-1993   Robert E. Avery  Don't accept unphysical 
C-                              values for z-position (simple cut). 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$PARAMS:FDCDEL.PARAMS'
C
      INTEGER HALF,QUAD,SECTOR
      INTEGER NDLHIT
C
      INTEGER LKFTDA, LKFTSC, NFTSC, NFTDA
      INTEGER I, J, K, IADCM, IADCP, IADC0
      INTEGER LHIT0, NHIT0, HIT0, LSEC0, LSEC
      INTEGER LHITM1, NHITM, HITM, LHITM, LHITM0, HITM0
      INTEGER LHITP1, NHITP, HITP, LHITP, LHITP0, HITP0
      INTEGER LUNDBG
      INTEGER ISET, IER, ICALL, ITMP1, ITMP2
      INTEGER JMINU(MX_HIT_WIRE), JPLUS(MX_HIT_WIRE)
      INTEGER NBMINU, NBPLUS
      INTEGER GZFTDA, GZFTSC
      INTEGER USUNIT
      INTEGER A,NCOMB,COMB,LIST(1000)
      INTEGER HITMSAVE(MX_HIT_WIRE),HITPSAVE(MX_HIT_WIRE)
      INTEGER AHITM(1000),AHITP(1000),AHIT0(1000)
      INTEGER AJMINU(1000),AJPLUS(1000)
C
      REAL ADIFF(1000),AZMINU(1000),AZPLUS(1000)
      REAL ZMINU(MX_HIT_WIRE), ZPLUS(MX_HIT_WIRE)
      REAL ZMAX, ZM, ZP, DIR
      REAL TM, TP, TS
      REAL VELMIN, VELPLU, TZSENS, TZPLUS, TZMINU
      REAL ZCOR, ERRZ, FDLTOL
      REAL ETZSEN,ETZPLU,ETZMIN
      REAL ERR_DL_TWO, ERR_DL_ONE
C
      LOGICAL DBG_FDGETD
      LOGICAL USED_M(0:MX_HIT_WIRE),USED_P(0:MX_HIT_WIRE)
      LOGICAL USED_0(MX_HIT_WIRE)
      LOGICAL OK
      LOGICAL SINGLE_DL
C
      REAL    ZCOR_CUT
      PARAMETER( ZCOR_CUT = 500. )
C
      INTEGER DUM
      PARAMETER (DUM=MX_HIT_WIRE+1)
C
      SAVE ICALL,DBG_FDGETD,FDLTOL
      SAVE USED_M,USED_P,USED_0,NCOMB
      SAVE ERR_DL_TWO,ERR_DL_ONE
      SAVE SINGLE_DL
C
      DATA ICALL / 0 /
      DATA ERR_DL_TWO / 0.5 /
      DATA ERR_DL_ONE / 2.0 /
      DATA NCOMB/0/
      DATA USED_M/DUM*.FALSE./
      DATA USED_P/DUM*.FALSE./
      DATA USED_0/MX_HIT_WIRE*.FALSE./
      DATA SINGLE_DL/.FALSE./
C
C----------------------------------------------------------------------
C
      IF (ICALL .EQ. 0) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET_l('DBG_FDGETD',DBG_FDGETD,IER)
        CALL EZGET('FDLTOL',FDLTOL,IER)
        CALL EZGET('ERR_DL_TWO',ERR_DL_TWO,IER)
        CALL EZGET('ERR_DL_ONE',ERR_DL_ONE,IER)
        CALL EZGET_l('SINGLE_DL',SINGLE_DL,IER)
        CALL EZRSET
        LUNDBG = USUNIT()
        ICALL = 1
      END IF
C
C ****  Get pointers on banks : FTSC and FTDA
C
      LKFTSC = GZFTSC(HALF, QUAD, SECTOR)
      NFTSC = IQ(LKFTSC+2)
      LKFTDA = GZFTDA(HALF, QUAD, SECTOR)
      NFTDA = IQ(LKFTDA+2)
C
      NDLHIT = 0
      IADC0 = 0                       ! sense wire 0 next to DL
      IADCM = 8                       ! Channel 8, -DL
      IADCP = IADCM+1                 ! Channel 9, +DL
C
C ****  LHITx1 are the pointers to the first hit data for each wire
C ****  NHITx  are the number of such hits for each wire
C ****  TZ and VEL are the time_to_position parameters
C
      LHITM1 = LKFTDA + IQ(LKFTDA + NFTDA+4 + IADCM) - 1
      LHITP1 = LKFTDA + IQ(LKFTDA + NFTDA+4 + IADCP) - 1
      LHIT0 = LKFTDA + IQ(LKFTDA + NFTDA+4 + IADC0) - 1
      LSEC0 = LKFTSC + IQ(LKFTSC + NFTSC+4 + IADC0) - 1
      NHITM = IQ(LKFTDA + 4 + IADCM)
      NHITP = IQ(LKFTDA + 4 + IADCP)
      NHIT0 = IQ(LKFTSC + 4 + IADC0)
C
      CALL FGTLTM(HALF,0,QUAD,SECTOR,0,ETZSEN,TZSENS,VELMIN,VELPLU)
      CALL FGTLTM(HALF,0,QUAD,SECTOR,8,ETZPLU,TZPLUS,VELPLU,ZMAX)
      CALL FGTLTM(HALF,0,QUAD,SECTOR,9,ETZMIN,TZMINU,VELMIN,ZMAX)
      IF (VELMIN.GT.0) VELMIN = VELMIN * (-1.)
C
      IF (DBG_FDGETD) THEN
        IF (LUNDBG.GT.0) THEN
          WRITE(LUNDBG,1000) IQ(LHEAD+9)
 1000     FORMAT(' ** FDGETD ** EVENT ',I6)
          WRITE(LUNDBG,1002) HALF, QUAD, SECTOR, NHIT0, NHITM, NHITP
 1002     FORMAT(' FDGETD - HALF ',I1,' QUAD ',I1,' SCTR ',I2,
     &      '  # of hits on SW, -Z, +Z = ',3I3)
          WRITE(LUNDBG,1001) LHITM1,LHITP1,LSEC0,
     &      IQ(LKFTDA+NFTDA+4+IADCM), IQ(LKFTDA+NFTDA+4+IADCP),
     &      IQ(LKFTSC+NFTSC+4+IADC0)
 1001     FORMAT(' FDGETD - LHITM1,LHITP1,LSEC0,IQ(M,P,0) = ',3I8,3I5)
        END IF
      END IF
C
C ****  Loop over sense wire 0 hits (delay measure requires a sense wire hit).
C
      DO 20 HIT0 = 1, NHIT0
        TS = Q(LHIT0 + OFTIME) - TZSENS     ! Sense wire drift time
C
C ****  Loop over all MINUS side hits.
C
        NBMINU = 0
        LHITM = LHITM1
        DO HITM = 1, NHITM
          TM = Q(LHITM + OFTIME) - TZMINU - TS       ! Delay time
C                                       ! - SW0 drift time
          ZM = TM * VELMIN           ! Delay line hit distance
          IF (ABS(ZM) .LE. ZMAX) THEN ! Less than DL length
            NBMINU = NBMINU + 1
            HITMSAVE(NBMINU) = HITM
            ZMINU(NBMINU) = ZM        ! store MINUS DL distance
            JMINU(NBMINU) = LHITM - LKFTDA      ! store DL hit location
          END IF
          LHITM = LHITM + IQ(LKFTDA + 3)        ! increment to next hit
        END DO
C
C ****  Loop over all PLUS side hits
C
        NBPLUS = 0
        LHITP = LHITP1
        DO HITP = 1, NHITP
          TP = Q(LHITP + OFTIME) - TZPLUS - TS       ! Delay time -
C                                       ! SW0 drift time
          ZP = TP * VELPLU            ! Delay line hit distance
          IF (ABS(ZP) .LE. ZMAX) THEN ! Less than DL length
            NBPLUS = NBPLUS + 1
            HITPSAVE(NBPLUS) = HITP
            ZPLUS(NBPLUS) = ZP        ! store PLUS DL distance
            JPLUS(NBPLUS) = LHITP - LKFTDA      ! store DL hit location
          END IF
          LHITP = LHITP + IQ(LKFTDA+3)    ! increment to next hit
        END DO
C
C ****  List of candidates are built. Debug if needed
C
        IF (DBG_FDGETD) THEN
          IF (LUNDBG.GT.0) THEN
            WRITE(LUNDBG, 1090) HIT0, TS
 1090       FORMAT(' FDGETD - hit number',I4,' time = ',F10.2)
            WRITE(LUNDBG, 1100) NBMINU, (ZMINU(HITM), HITM = 1,NBMINU)
 1100       FORMAT(5X,'Minus side',I3,' values :',10F8.2)
            WRITE(LUNDBG, 1110) NBPLUS, (ZPLUS(HITP), HITP = 1,NBPLUS)
 1110       FORMAT(5X,'Plus  side',I3,' values :',10F8.2)
          END IF
        END IF
C
C ****  Select the best pair of candidates (if any)
C
        IF (NBMINU.GT.0 .AND. NBPLUS.GT.0) THEN
C
C ****  At least one MINUS and one PLUS. Search best, and cut (criterion
C ****  is the compatibility of the Z from both sides
C
          DO HITM = 1, NBMINU
            DO HITP = 1, NBPLUS
              IF (ABS((ZPLUS(HITP)-ZMINU(HITM))-ZMAX) .LT. FDLTOL) THEN
                IF (NCOMB.LT.1000) THEN
                  NCOMB = NCOMB + 1
                  LIST(NCOMB) = NCOMB
                  ADIFF(NCOMB) = ABS((ZPLUS(HITP)-ZMINU(HITM))-ZMAX)
                  AHITM(NCOMB) = HITMSAVE(HITM)
                  AHITP(NCOMB) = HITPSAVE(HITP)
                  AHIT0(NCOMB) = HIT0
                  AZMINU(NCOMB) = ZMINU(HITM)
                  AZPLUS(NCOMB) = ZPLUS(HITP)
                  AJMINU(NCOMB) = JMINU(HITM)
                  AJPLUS(NCOMB) = JPLUS(HITP)
                END IF
              END IF
            END DO
          END DO
C
        END IF
C
C Save all single hits
        IF (SINGLE_DL .OR.
     &      (NBMINU.EQ.0.AND.NBPLUS.EQ.1) .OR.
     &      (NBPLUS.EQ.0.AND.NBMINU.EQ.1) ) THEN
C
          DO HITM = 1, NBMINU
            IF (NCOMB.LT.1000) THEN
              NCOMB = NCOMB + 1
              LIST(NCOMB) = NCOMB
              ADIFF(NCOMB) = 99999.
              AHITM(NCOMB) = HITMSAVE(HITM)
              AHITP(NCOMB) = 0
              AHIT0(NCOMB) = HIT0
              AZMINU(NCOMB) = ZMINU(HITM)
              AZPLUS(NCOMB) = 0
              AJMINU(NCOMB) = JMINU(HITM)
              AJPLUS(NCOMB) = 0
            END IF
          END DO
C
          DO HITP = 1, NBPLUS
            IF (NCOMB.LT.1000) THEN
              NCOMB = NCOMB + 1
              LIST(NCOMB) = NCOMB
              ADIFF(NCOMB) = 99999.
              AHITM(NCOMB) = 0
              AHITP(NCOMB) = HITPSAVE(HITP)
              AHIT0(NCOMB) = HIT0
              AZMINU(NCOMB) = 0
              AZPLUS(NCOMB) = ZPLUS(HITP)
              AJMINU(NCOMB) = 0
              AJPLUS(NCOMB) = JPLUS(HITP)
            END IF
          END DO
        END IF
C
        LHIT0 = LHIT0 + IQ(LKFTDA+3)
   20 CONTINUE                            ! end of loop over SW0 hits
C
      IF (NCOMB.EQ.0) THEN
        IF (DBG_FDGETD) THEN
          IF (LUNDBG.GT.0) THEN
            WRITE(LUNDBG,*) ' No delay line hit matches.'
          END IF
        END IF
C
      ELSE
        CALL SRTFLT(ADIFF,NCOMB,LIST)
C
        DO A = 1, NCOMB
C
          COMB = LIST(A)
          HITM = AHITM(COMB)
          HITP = AHITP(COMB)
          HIT0 = AHIT0(COMB)
C
          IF (.NOT.USED_M(HITM).AND.
     &        .NOT.USED_P(HITP).AND.
     &        .NOT.USED_0(HIT0)) THEN
C
            LHITM = 0
            LHITM0 = 0
            LHITP = 0
            LHITP0 = 0
C
            OK = .TRUE.
C
C Take care of single dl hits
            IF (SINGLE_DL.AND.(HITM.EQ.0.OR.HITP.EQ.0)) THEN
              CALL FSINGLE_DL(COMB,NCOMB,USED_0,USED_M,USED_P,
     &            AHIT0,AHITM,AHITP,OK)
            END IF
C
            IF (OK) THEN
C
              IF (HITM.GT.0) THEN
                LHITM = AJMINU(COMB)
                LHITM0 = LKFTDA + LHITM
                ZCOR = AZMINU(COMB) + ZMAX/2.
                ERRZ = ERR_DL_ONE
                USED_M(HITM) = .TRUE.
              END IF
              IF (HITP.GT.0) THEN
                LHITP = AJPLUS(COMB)
                LHITP0 = LKFTDA + LHITP
                ZCOR = AZPLUS(COMB) - ZMAX/2.
                ERRZ = ERR_DL_ONE
                USED_P(HITP) = .TRUE.
              END IF
C
              IF (HITM.GT.0.AND.HITP.GT.0) THEN
                ZCOR = (AZMINU(COMB) + AZPLUS(COMB))/2.
                ERRZ = ERR_DL_TWO
              END IF
C
              USED_0(HIT0) = .TRUE.
C
              IF (DBG_FDGETD) THEN
                IF (LUNDBG.GT.0) THEN
                  WRITE(LUNDBG,1150) HIT0,HITM,HITP
 1150             FORMAT(' HIT0, HITM, HITP = ',3I5)
                  WRITE(LUNDBG,1151) AZMINU(COMB),AZPLUS(COMB),ZCOR
 1151             FORMAT(' Best diff uses (-,+) = ',2F10.3,
     &                ' for zcoor = ',F10.3)
                END IF
              END IF
C
C ****  Reject if ZCOR is unphysical:
C
              IF ( ABS(ZCOR).GT.ZCOR_CUT ) GOTO 100
C
C ****  This is a good Z hit. Update info in FTSC for this hit
C
              DIR = 1.
              IF (HALF.EQ.0 .AND. QUAD.GE.4) DIR = -1.
              IF (HALF.EQ.1 .AND. QUAD.LE.3) DIR = -1.
C

              LSEC = LSEC0 + (HIT0-1)*IQ(LKFTSC+3)
              IQ(LSEC + OFPTZ) = LHITM
              IQ(LSEC + OFPTZ+1) = LHITP
              Q(LSEC + OFZCOR) = (ZCOR * DIR) / 10.  ! Conv to cm.
              Q(LSEC + OFERZ) = ERRZ                 ! Already in cm.
C
C  Only retain 20 micron precision (for consistancy with FHIT bank):
C
              Q(LSEC + OFZCOR) = FLOAT(NINT(Q(LSEC+OFZCOR)*500))/500.
              NDLHIT = NDLHIT + 1 
C
              IF (DBG_FDGETD) THEN
                IF (LUNDBG.GT.0) THEN
                  WRITE(LUNDBG,1200) LHITM,LHITP,
     &                Q(LSEC+OFZCOR), Q(LSEC+OFERZ)
 1200             FORMAT(' FDGETD - LHITM, LHITP = ',2I10/
     &                5X,'Z coordinate is ',F10.3,' +/- ',F10.3)
                END IF
              END IF
C
C ****  Adjust STATUS word of FTSC, combination of flags
C
              ISET = 0
              IF (LHITM .NE. 0) THEN
                ITMP1 = IQ(LHITM0+OFSTDA)
                ITMP2 = IQ(LSEC+OFSTSC)
                CALL MVBITS(ITMP1,0,8,ITMP2,16)
                IQ(LHITM0+OFSTDA) = ITMP1
                IQ(LSEC+OFSTSC) = ITMP2
                ISET = 1
              END IF
              IF (LHITP .NE. 0) THEN
                CALL MVBITS(IQ(LHITP0+OFSTDA),0,8,IQ(LSEC+OFSTSC),24)
                ISET = ISET + 2
              END IF
              CALL MVBITS(ISET,0,8,IQ(LSEC+OFSTSC),0)
C
            END IF
          END IF
  100     CONTINUE
        END DO
      END IF
C
C------------------------------------------------------------------------
C
  999 CONTINUE
C
      NCOMB = 0
      DO HITM = 1, NHITM
        USED_M(HITM) = .FALSE.
      END DO
      DO HITP = 1, NHITP
        USED_P(HITP) = .FALSE.
      END DO
      DO HIT0 = 1, NHIT0
        USED_0(HIT0) = .FALSE.
      END DO
C
      RETURN
      END
