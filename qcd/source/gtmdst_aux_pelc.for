      SUBROUTINE GTMDST_AUX_PELC( ITYPE, ICAND, ICLEAN_EM, NCASH,
     &  CONE_WORDS, PHI_TRK, THETA_TRK, XCOG_TRK, YCOG_TRK, ZCOG_TRK,
     &  CDCMIP, FDCMIP, VTXMIP, LIKE, EFFIC, TRDINFO, TRUN_MEAN,
     &  CAL_ETA, CHISQUARE, IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the extra PELC and PPHO words that
C-                         we have stored in the microdst
C-
C-   Inputs  :    ITYPE         [I]   1=PELC, 2=PPHO
C-                ICAND         [I]   Which PELC/PPHO candidate
C-   Outputs  :   ICLEAN_EM     [I]   Clean EM status word
C-                NCASH         [I]   # of CASH cells
C-                CONE_WORDS(3) [R]   ET in R=.4, Energy in R=.6, ET in R=.6
C-                PHI_TRK       [R]   Phi of track
C-                THETA_TRK     [R]   theta of track
C-                XCOG_TRK      [R]
C-                YCOG_TRK      [R]
C-                ZCOG_TRK      [R]
C-                CDCMIP        [R]
C-                FDCMIP        [R]
C-                VTXMIP        [R]
C-                LIKE          [R]   Likelihood based on anode energy
C-                EFFIC         [R]   Efficiency based on this likelihood
C-                TRDINFO       [L]   TRD info
C-                TRUNMEAN      [R]   TRD truncated mean
C-                CAL_ETA       [R]   Calorimeter eta of cluster
C-                CHISQUARE     [R]   Reduced chi-square
C-                IER           [I]   Error  0=ok, -1=MDST not found,
C-                                    -2 ICAND not found
C-   Outputs :
C-   Controls:
C-
C-   Created  16-JUL-1993   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER N_AUX_PELC_WORDS          ! Extra PELC/PPHO words for version 3
      INTEGER N_AUX_PPHO_WORDS, NWORDS
      PARAMETER( N_AUX_PELC_WORDS = 21 )
      PARAMETER( N_AUX_PPHO_WORDS = 8)
      INTEGER ICLEAN_EM, ITYPE, ICAND, NCASH
      LOGICAL TRDINFO
      REAL CHISQUARE, CAL_ETA, TRUN_MEAN, EFFIC, LIKE, VTXMIP
      REAL CDCMIP, FDCMIP, XCOG_TRK, YCOG_TRK, ZCOG_TRK, THETA_TRK
      REAL PHI_TRK, CONE_WORDS(3)
      INTEGER GZMDST, LMDST, LPELC, N, NSIZE, II, IER, IOFF, LPOINT, I
      BYTE IB(4)
      EQUIVALENCE ( II, IB(1) )
C----------------------------------------------------------------------
C
C: This only works for MDST
C
      LMDST = GZMDST()
      IF ( LMDST .LE. 0 ) THEN
        IER = -1
        GOTO 900
      ENDIF
C
C: Is this candidate legal?
C
      IF ( ITYPE .EQ. 1 ) THEN
        IOFF = 11               ! Electrons in MDST header
        NSIZE = N_AUX_PELC_WORDS
      ELSE
        IOFF = 17
        NSIZE = N_AUX_PPHO_WORDS
      ENDIF
      N   = IQ(LMDST+ IOFF + 1)
      IF ( N .LT. ICAND ) THEN
        IER = -2
        GOTO 900
      ENDIF
      LPELC = LMDST + IQ( LMDST+IOFF+2 ) + (ICAND-1)*IQ( LMDST+IOFF) - 1
C
C: Point to start of auxiliary block
C
      LPOINT = LPELC + IQ(LMDST+IOFF ) - NSIZE
C
C: Chisquare is the word right before
C
      CHISQUARE = Q( LPOINT )
C
C: Clean EM word
C
      DO I = 1, 4
        IB(I) = Q( LPOINT + I )
      ENDDO
      ICLEAN_EM = II
      LPOINT = LPOINT + 4
C
C: Others
C
      NCASH = Q( LPOINT + 1)
      DO I = 1, 3
        CONE_WORDS(I) = Q( LPOINT + 1 + I )
      ENDDO
C
C: That is all, except for electrons
C
      PHI_TRK = 0.
      THETA_TRK = 0.
      XCOG_TRK = 0.
      YCOG_TRK = 0.
      ZCOG_TRK = 0.
      CDCMIP = 0.
      FDCMIP = 0.
      VTXMIP = 0.
      LIKE   = 0.
      EFFIC  = 0.
      TRDINFO= 0.
      TRUN_MEAN = 0.
      CAL_ETA = 0.
      IF ( ITYPE .NE. 1 ) GOTO 999

      PHI_TRK = Q( LPOINT + 5 )
      THETA_TRK = Q( LPOINT + 6 )
      XCOG_TRK = Q( LPOINT + 7 )
      YCOG_TRK = Q( LPOINT + 8 )
      ZCOG_TRK = Q( LPOINT + 9 )
      CDCMIP = Q( LPOINT + 10 )
      FDCMIP = Q( LPOINT + 11 )
      VTXMIP = Q( LPOINT + 12 )
      LIKE   = Q( LPOINT + 13 )
      EFFIC  = Q( LPOINT + 14 )
      TRDINFO= Q( LPOINT + 15 )
      TRUN_MEAN = Q( LPOINT + 16 )
      CAL_ETA  = Q( LPOINT + 17 )

      IER = 0

  900 CONTINUE
  999 RETURN
      END
