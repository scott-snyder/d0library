      FUNCTION FISA_MARK()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Mark FDC ISAJET track in FITR bank as being
C-      matched with reconstructed FDC track if it is the closest 
C-      matched track within momentum dependent cuts:
C-              R_DIFF < MIN( 4.0 cm / p_isa(GeV) , 4.0 cm)
C-              ANG_DIFF < MIN( 0.6 radian, / p_isa(GeV) , 0.6 radian )
C-      where R_DIFF is the distance between the FDC and ISAJET track in 
C-      the x-y plane at the face of the FDC, and ANG_DIFF is the opening
C-      angle between the FDC and ISAJET track. This match is probably 
C-      not reliable for momentum less than ~3 GeV (because of multiple 
C-      scattering.
C-
C-   Returned value  : .TRUE. (always)
C-   Inputs  : isajet and fdc banks.
C-   Outputs : Fills word 9 of FITR banks
C-
C-   Created  13-FEB-1992   Robert E. Avery
C-   Updated  27-FEB-1992   Robert E. Avery  Increase cut to account for 
C-      track measurement error (relevent for 1 layer tracks where error is
C-      from the delay line.) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL FISA_MARK
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER GZISAE,LISAE
      INTEGER GZFTRH,LFTRH
      INTEGER GZFITR,LFITR
      INTEGER IER
      INTEGER ISA_HALF
      INTEGER ISATRK,NISATRK
      INTEGER IFDC_HALF,IFDC_BEST
      INTEGER IFDCTRK ,NFDCTRK
      INTEGER IFDCDAT(26)
      INTEGER LADDER(0:2)
      INTEGER LBASE 
C
      REAL    ISADAT(9)
      REAL    X_ISA,Y_ISA,DX_ISA,DY_ISA
      REAL    FDCDAT(26),QHSEC(3,34)
      REAL    X_FDC,Y_FDC,DX_FDC,DY_FDC
      REAL    TAN_THETA
      REAL    PHI_FDC, PHI_ISA
      REAL    THE_FDC, THE_ISA
      REAL    P_ISA, Z_VTX, RADIUS
      REAL    R_DIFF_CONST, ANG_DIFF_CONST
      REAL    R_DIFF_CUT, ANG_DIFF_CUT
      REAL    R_DIFF_TRK_CUT, ANG_DIFF_TRK_CUT 
      REAL    R_DIFF, ANG_DIFF, X_DIFF
      REAL    R_ERR,ANG_ERR 
      REAL    R_DIFF_BEST 
      REAL    ERR_MULT
      REAL    PHI_ERROR, THETA_ERROR 
      REAL    Z0(0:1)
C
      LOGICAL FIRST
      EQUIVALENCE (IFDCDAT,FDCDAT)
C
      DATA FIRST /.TRUE./
      DATA R_DIFF_CONST   /4.0/
      DATA ANG_DIFF_CONST /0.6/
      DATA ERR_MULT /2.0/
C----------------------------------------------------------------------
      FISA_MARK = .FALSE.
      LISAE = GZISAE()
      IF ( LISAE .LE. 0 ) GOTO 999
C
      LFTRH = GZFTRH()
      IF ( LFTRH .LE. 0 ) GOTO 999
      NFDCTRK = IQ(LFTRH+2)
      IF ( NFDCTRK .LE. 0 ) GOTO 999
C
      LFITR = GZFITR()
      IF ( LFITR .LE. 0 ) GOTO 999
      NISATRK = IQ( LFITR + 1 )
      IF ( NISATRK .LE. 0 ) GOTO 999
C
      IF ( FIRST ) THEN
        Z0(0) = Q(LFTRH+3)
        Z0(1) = Q(LFTRH+4)
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('FISA_ANG_DIFF',ANG_DIFF_CONST,IER)
        CALL EZGET('FISA_R_DIFF',R_DIFF_CONST,IER)
        CALL EZGET('FISA_ERR_MULT',ERR_MULT,IER)
        CALL EZRSET
        IF ( IER.NE.0 ) THEN
          CALL EZPICK('FDC_RCP')
          CALL EZGET('FISA_ANG_DIFF',ANG_DIFF_CONST,IER)
          CALL EZGET('FISA_R_DIFF',R_DIFF_CONST,IER)
          CALL EZGET('FISA_ERR_MULT',ERR_MULT,IER)
          CALL EZRSET
        ENDIF
        FIRST = .FALSE.
      ENDIF
C
      DO ISATRK=  1, NISATRK
        CALL GTFITR(ISATRK, ISADAT)
        TAN_THETA = TAN(ISADAT(5))
        IF ( TAN_THETA .GT. 0 ) THEN
          ISA_HALF = 1
        ELSE
          ISA_HALF = 0
        ENDIF
        DX_ISA = TAN_THETA * COS(ISADAT(4))
        DY_ISA = TAN_THETA * SIN(ISADAT(4))
        Z_VTX = ISADAT(3)
        X_ISA = ISADAT(1) - (Z_VTX - Z0(ISA_HALF) ) * DX_ISA
        Y_ISA = ISADAT(2) - (Z_VTX - Z0(ISA_HALF) ) * DY_ISA
        P_ISA = ISADAT(6)
        P_ISA = MIN(P_ISA,24.999)
        PHI_ISA = ISADAT(4)
        THE_ISA = ISADAT(5)
        RADIUS = SQRT ( (X_ISA)**2 + (Y_ISA)**2 )
C
        IF ( (P_ISA.NE.0) .AND. (R_DIFF_CONST.GT.0) ) THEN
          R_DIFF_CUT = MIN( R_DIFF_CONST / P_ISA, R_DIFF_CONST )
          ANG_DIFF_CUT = MIN( ANG_DIFF_CONST / P_ISA, ANG_DIFF_CONST )
        ELSE
          R_DIFF_CUT = ABS(R_DIFF_CONST)
          ANG_DIFF_CUT = ABS(ANG_DIFF_CONST)
        ENDIF
C
        R_DIFF_BEST = R_DIFF_CUT
        IFDC_BEST = -1
C
        DO  IFDCTRK =  1, NFDCTRK
          CALL GTFDCT(IFDCTRK,FDCDAT,QHSEC,LADDER)
          IFDC_HALF = IAND(IFDCDAT(1),1)
          IF ( ISA_HALF.EQ.IFDC_HALF ) THEN
            X_FDC = FDCDAT(4)
            Y_FDC = FDCDAT(5)
            DX_FDC = FDCDAT(7)
            DY_FDC = FDCDAT(8)
            PHI_FDC = FDCDAT(6)
            THE_FDC = FDCDAT(22)
            R_DIFF = SQRT ( (X_FDC-X_ISA)**2 + (Y_FDC-Y_ISA)**2 )
            ANG_DIFF = SQRT ( (DX_FDC-DX_ISA)**2 + (DY_FDC-DY_ISA)**2 )
C
            ANG_ERR = SQRT( FDCDAT(23)**2 + FDCDAT(24)**2)
            ANG_DIFF_TRK_CUT =  
     &        SQRT(ANG_DIFF_CUT**2 + (ERR_MULT * ANG_ERR) **2)  
C
            PHI_ERROR = RADIUS * FDCDAT(23)
            THETA_ERROR = ABS( Z_VTX - Z0(ISA_HALF)) * FDCDAT(24)
            R_ERR = SQRT(THETA_ERROR**2 + PHI_ERROR**2)
            R_DIFF_TRK_CUT = 
     &         SQRT(R_DIFF_CUT**2 + (ERR_MULT * R_ERR)**2)  
C            
            IF ( ( R_DIFF.LT. R_DIFF_TRK_CUT )
     &        .AND. ( ANG_DIFF.LT.ANG_DIFF_TRK_CUT ) ) THEN
              IF ( (R_DIFF.LT.R_DIFF_BEST)
     &          .OR. (IFDC_BEST.EQ.-1) ) THEN
                IFDC_BEST   =  IFDCTRK
                R_DIFF_BEST =  R_DIFF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
C
        LBASE = LFITR + 2 + IQ(LFITR+2)*(ISATRK-1)
        IF ( IFDC_BEST.GT.0 ) THEN
          Q(LBASE+9) = IFDC_BEST
        ELSE
          Q(LBASE+9) = -999
        ENDIF
      ENDDO
C
      FISA_MARK = .TRUE.
  999 RETURN
      END
