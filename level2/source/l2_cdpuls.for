      SUBROUTINE L2_CDPULS(NPULSE,HITLST,MAXPUL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : search hits for a FADC channel. This routine
C-                         is special designed to be used in Level 2
C-
C-   Inputs  : LAYER, SECTOR, WIRE: FADC's logic address in layer, sector and
C-                                  wire number
C-             MAXPUL     = Maximum number of pulse allowed
C-   Outputs : NPULSE     = # of hits found 
C-             HITLST(LPULSE,*)= content of each hit as described in the doc.
C-
C-   Created  28-JUL-1992   Qizhong Li-Demarteau  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:CDLOCA.INC'
C
      INTEGER ADDR, MAXPUL, ERR
C*DC*      INTEGER MAXCNT                    ! Maxcnt not needed? C*DC*
      INTEGER NATUR,NPULSE,ISUML,J, NCALL, RUNNO
      INTEGER I,ILAST,IFIRST,ISUMF,S,INIT, IPEV
      INTEGER EXPDAT(2*LFADC), IPREAD, IFBIN, LMAX , HEIGHT(0:255)
      INTEGER IOFS, LABEL, IFLAG, IAREA, IPED, JDPDL, L2_GZDPDH
      INTEGER PULTH1(2), PULTH2(2), PULTH3(2), PULMAX(2), CDSURV
      INTEGER TABLE(0:255)
      REAL    PULWEI(2)
      REAL  HITLST(LPULSE,*)
      REAL  B(LFADC),COUNT, FLABEL, FLAG
      REAL  NBPBIN,SUM,SUMX, COEFF(50,2), COEF2(50,2)
      REAL  THR1, THR2, THR3, ERRB, FPED
C*DC*      REAL  FFRQCY, BILIRT, BILIPT              ! not needed? C*DC*
      INTEGER IER
      LOGICAL EZERROR
      LOGICAL NOIPED
C*DC*      LOGICAL BILFLG,SBTRCT, TBLFLG             ! not needed? C*DC*
C*DC*      LOGICAL FIRST
      CHARACTER*8 ZFORMA
C
      COMMON/CDPULS_PARMS/ COEFF, COEF2, PULTH1, PULTH2, PULTH3,
     &  PULMAX, PULWEI, TABLE
C
      EQUIVALENCE (LABEL, FLABEL), (FLAG,IFLAG)
C*DC*      SAVE FIRST
C*DC*      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C*DC*      IF (FIRST) THEN
C*DC*        FIRST = .FALSE.
C*DC*        FFRQCY = 106.0
C*DC*        BILFLG = .FALSE.
C*DC*        BILIRT = 8.6
C*DC*        BILIPT = 192.0
C*DC*        MAXCNT = 255
C*DC*        SBTRCT = .FALSE.
C*DC*        TBLFLG = .TRUE.
C*DC*        IF (BILFLG) CALL ZBICVT(BILIPT,BILIRT,MAXCNT)
C*DC*        IF (TBLFLG) MAXCNT = TABLE(MAXCNT)
C*DC*        DO 7 J = 1, 2
C*DC*          COEFF(1,J) = 1.
C*DC*          COEF2(1,J) = 1.*COEFF(1,J)
C*DC*          DO 4 I = 2, 50
C*DC*            COEFF(I,J) = COEFF(I-1,J) * PULWEI(J)
C*DC*            COEF2(I,J) = COEFF(I,J) * FLOAT(I)
C*DC*    4     CONTINUE
C*DC*    7   CONTINUE
C*DC*      ENDIF
C*DC*
      NPULSE=0
      NOIPED = .FALSE.
C
C ****  get FADC datas after unpacking
C
      LABEL  = ( LAYER * 32 + SECTOR ) * 16 + WIRE
C                                                ! CDUNPK swaps Layers 2&4 for
C     CALL CDUNPK( LAYER, SECTOR, WIRE, EXPDAT ) ! affected sector in COSMIC
C                                                ! Commisioning runs and
C                                                ! checks for MC which should
C                                                ! be obsoleted by RECDDN
C*DC Replace call to CDUNPK with the following:
C      ADDR = 8192 + LABEL                        ! Test C*DC*
      ADDR = 8192 + LAYER * 512 + SECTOR * 16 + WIRE 
      CALL L2_ZDEXPD(2, ADDR, EXPDAT)
C     CALL L2_ZDEXPD(DCDTYP=0:all banks;2:CDD2only, addressLABEL, DATAarray)
C*DC
C
      IF (EXPDAT(1) .EQ. 0 ) GOTO 999
C
C ****  Decides if Delay line : if WIRE is NBSENS or more
C
      IF ( WIRE .GE. NBSENS ) THEN
        NATUR = 2
      ELSE
        NATUR = 1
      ENDIF
      JDPDL = LC(LDPDH - (LAYER+1))
      JDPDL = JDPDL + (SECTOR*IC(JDPDL+4)+WIRE) * IC(JDPDL+3) + 4
      FPED  = C(JDPDL+1)              ! Pedestal
      IPED = NINT(FPED)
      THR1 = FLOAT(PULTH1(NATUR))
      THR2 = FLOAT(PULTH2(NATUR))
      THR3 = FLOAT(PULTH3(NATUR))
      IPREAD = 1
C
  100 IF (EXPDAT(IPREAD) .EQ. 0) GOTO 999
      IFBIN = EXPDAT(IPREAD+1)
      LMAX  = EXPDAT(IPREAD  )
      IPEV  = IPREAD + 1
      IPREAD = IPREAD + LMAX + 2
C      IF (FFRQCY .NE. 0) THEN
C        NBPBIN = 1000. / FFRQCY
C      ELSE
        NBPBIN = 1000. / 106.
C      ENDIF
C
C ****  compute first difference
C
      B(1) = 0.
C*DC*      IF (BILFLG .OR. TBLFLG) THEN                    !C*DC*
        NOIPED = .TRUE.
        EXPDAT(IPEV+1) = EXPDAT(IPEV+1) - IPED
        IF (EXPDAT(IPEV+1) .GT. 0) THEN
C*DC*          IF (TBLFLG) THEN                            !C*DC*
            EXPDAT(IPEV+1) = TABLE(EXPDAT(IPEV+1))
C*DC*          ELSE                                        !C*DC*
C*DC*            CALL ZBICVT(BILIPT,BILIRT,EXPDAT(IPEV+1)) !C*DC*
C*DC*          ENDIF                                       !C*DC*
        ENDIF
        DO 15 I = 2, LMAX
          EXPDAT(IPEV+I) = EXPDAT(IPEV+I) - IPED
        IF (EXPDAT(IPEV+I) .GT. 0) THEN
C*DC*          IF (TBLFLG) THEN                              !C*DC*
            EXPDAT(IPEV+I) = TABLE(EXPDAT(IPEV+I))
C*DC*          ELSE                                          !C*DC*
C*DC*            CALL ZBICVT(BILIPT,BILIRT,EXPDAT(IPEV+I))   !C*DC*
C*DC*          ENDIF                                         !C*DC*
        ENDIF
        B(I)=FLOAT( EXPDAT(IPEV+I) - EXPDAT(IPEV+I-1) )
   15   CONTINUE
C*DC*      ELSE                                                 !C*DC*
C*DC*        DO 5 I = 2, LMAX                                   !C*DC*
C*DC*          B(I)=FLOAT( EXPDAT(IPEV+I) - EXPDAT(IPEV+I-1) )  !C*DC*
C*DC*    5   CONTINUE                                           !C*DC*
C*DC*      ENDIF                                                !C*DC*
C
C ****  search for three successive bins above threshold
C ****  or two successive bins above threshold with sum above threshold
C
      IFIRST=0
      ILAST=1
      ISUMF=0
      ISUML=0
   10 COUNT=0
      I = ILAST + 1
   11 IF( I .GT. LMAX-1 ) GOTO 100
      IF( B(I) .GE. THR1 ) THEN
        IF( B(I-1) .GE. THR1 ) THEN
          IF ((NOIPED .AND. EXPDAT(IPEV+I) .GE. 0) .OR.
     &       ((.NOT. NOIPED) .AND. EXPDAT(IPEV+I) .GE. IPED)) THEN
            IF ( B(I+1).GE.THR1 .OR. B(I)+B(I-1).GE.THR2 ) THEN
              IFIRST = I - 2
              IF( B(IFIRST) .LE. 0 ) IFIRST = I - 1
              ISUMF = I - 2
              GOTO 30
            ELSE
              I = I + 3
            ENDIF
          ELSE
            I = I + 1
          ENDIF
        ELSE
          I = I + 1
        ENDIF
      ELSE
        I = I + 2
      ENDIF
      GO TO 11
C
C ****   found pulse - find where 1st difference returns to zero
C
   30 ILAST=IFIRST
      ISUML=ISUMF
      IOFS  = 1
      SUM=0.
      SUMX=0.
      DO 40 I = IFIRST, LMAX
        IF(B(I).LE.0.)THEN
          ILAST=I
          ISUML=I
          GO TO 50
        ENDIF
        SUM  = SUM  + B(I) * COEFF(IOFS,NATUR)
        SUMX = SUMX + B(I) * COEF2(IOFS,NATUR)
        IOFS = IOFS + 1
   40 CONTINUE
      ILAST=LMAX
C
C ****  require pulse height exceeding threshold
C
   50 IF ((EXPDAT(IPEV+ILAST-1) - EXPDAT(IPEV+IFIRST-1)) .LE.
     &  THR3)GO TO 10
C
C ****  calculate integral of pulse by truncating if a second pulse arrives
C ****  immediately, or if 3 consecutive differences are less than leading
C ****  edge threshold, or truncate if these conditions are not met after
C ****  mxtail number of bins after the pulse peak.
C
      IFLAG = 0.
      S=ISUML
   60 S=S+1
      COUNT=COUNT+1
      IF ((S+2).GE.LMAX.OR.(COUNT+3).GE.PULMAX(NATUR)) THEN
        ISUML=MIN(S+2, LMAX)
        GOTO 70
      END IF
      IF (B(S).GE.THR1 .AND. B(S+1).GE.THR1 .AND.
     +   (B(S+2).GE.THR1 .OR. B(S)+B(S+1).GE.THR2)) THEN
        IFLAG = IBSET(IFLAG,1)               ! set overlap flag
        ISUML=S-1
        GOTO 70
      END IF
      IF (-B(S).LE.THR1.AND.-B(S+1).LE.THR1 .AND.-B(S+2).LE.THR1) THEN
        ISUML=S
        IF (B(S+1).LE.0) THEN
          IF (B(S+2).LT.0) THEN
            ISUML = S+2
          ELSE
            ISUML= S+1
          ENDIF
        ENDIF
        GOTO 70
      END IF
      GOTO 60
C
C ****  record pulse parameters
C
  70  NPULSE=NPULSE+1
      HITLST(1,NPULSE) = FLABEL
      HITLST(2,NPULSE) = NBPBIN*(SUMX/SUM + IFIRST-1 + IFBIN - .5)  ! SHIFT BIN CENTER
      HITLST(4,NPULSE) = FLOAT((ILAST-IFIRST))*NBPBIN
C*DC*
C*DC*      IF (SBTRCT) THEN                                                    !C*DC*
C*DC*        HITLST(5,NPULSE) = EXPDAT(IPEV+ILAST-1) - EXPDAT(IPEV+IFIRST-1)   !C*DC*
C*DC*      ELSE                                                                !C*DC*
        HITLST(5,NPULSE) = EXPDAT(IPEV+ILAST-1)
C*DC*      ENDIF                                                               !C*DC*
C*DC*
      IF (WIRE .GE. NBSENS .AND. WIRE .LE. MXFADC) THEN
        HITLST(6,NPULSE) = 1.                 ! time error for delay line
      ELSE
        HITLST(6,NPULSE) = 4.                 ! drift time error
      ENDIF
      HITLST(8,NPULSE) = FLAG
C
      IF (NPULSE .LT. MAXPUL) GOTO 10
C
  999 CONTINUE
      RETURN
      END
