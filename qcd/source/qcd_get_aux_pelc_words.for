      SUBROUTINE QCD_GET_AUX_PELC_WORDS( ITYPE, LCLUS, NMAX, WORDS,
     &  NWORDS )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get auxiliary PELC/PPHO words for inclusion
C-                         into the MDST
C-
C-   Inputs  : ITYPE      [I]       1 = PELC, 2 = PPHO
C-             LCLUS      [I]       Pointer to PELC or PPHO bank
C-             NMAX       [I]       Maximum size of WORDS array
C-   Outputs : WORDS      [R(*)]    Array or returned words
C-             NWORDS     [I]       Number of returned words
C-   Controls:
C-
C-   Created  13-JUL-1993   Richard V. Astur
C-   Updated  23-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'        ! Zebra common
      INTEGER ITYPE, LCLUS, NWORDS, NMAX
      REAL WORDS(NMAX)
c
      INTEGER I, LCACL, LCASH, LZTRK, LZFIT, LVTXT, LDTRK, LFDCT
      INTEGER LTRDT, NTRK_TRD, NTRK_LYR1, NTRK_LYR2, NTRK_LYR3
      BYTE IB(4)
      INTEGER II
      EQUIVALENCE( II, IB )
C----------------------------------------------------------------------
      NWORDS = 0
      CALL VZERO( WORDS, NMAX )
C
C ****  4 words  PELC / PPHO for each  electron/photon candidate
C
C
C ****  Clean EM words. Put one byte in each word
C                                               
      II = IQ( LCLUS + 30 )
      DO I = 1, 4
        WORDS(I + NWORDS ) = IB(I)
      ENDDO
      NWORDS = NWORDS + 4

C
C ****  4 words  from CACL bank for each  electron/photon candidate
C
      LCACL = LQ(LCLUS-2)             ! Link to associated CACL bank
      IF (LCACL.GT.0) THEN
        LCASH = LQ(LCACL-2)
        IF (LCASH .GT. 0) WORDS( NWORDS + 1) = IQ(LCASH+2) ! NCELLS
C      Plus the folowing 3 words from CACL             
C
C             +29    F       Et     inphysics isolation cone  (R=0.4)
C             +30    F       Energy in physics isolation cone (R=0.6)
C             +31    F       Et     inphysics isolation cone  (R=0.6)
C
        WORDS( NWORDS + 2 ) = Q( LCACL + 29 )
        WORDS( NWORDS + 3 ) = Q( LCACL + 30 )
        WORDS( NWORDS + 4 ) = Q( LCACL + 31 )
      ENDIF
      NWORDS = NWORDS + 4

C
C: Continue only for electrons
C
      IF ( ITYPE .NE. 1 ) GOTO 999
C
C ****  8 words from tracking banks for each electron
C
C
      LZTRK = LQ(LCLUS-3)             ! Link to associated ZTRAK bank
      IF (LZTRK .LE. 0) THEN          ! This should not happen
        CALL ERRMSG('NOZTRACK','QCD_GET_AUX_PELC_WORDS',
     &    'No Ztrack associated with electron ! ','W')
      ELSE
        LZFIT = LQ(LZTRK-1)             ! Link to global fit
        IF (LZFIT .LE. 0) THEN          ! This should not happen
          CALL ERRMSG('NOZFIT','CLEANEM',
     &      'No ZFIT info associated with electron ZTRK ! ','W')
        ELSE

          LVTXT = LQ(LZTRK-6)
          LDTRK = LQ(LZTRK-7)
          LFDCT = LQ(LZTRK-8)

          WORDS( NWORDS + 1)= Q(LZFIT+10)      ! phi trk
          WORDS( NWORDS + 2)= Q(LZFIT+13)      ! theta trk
          WORDS( NWORDS + 3)= Q(LZFIT+11)      ! xcog_trk
          WORDS( NWORDS + 4)= Q(LZFIT+12)      ! ycog_trk
          WORDS( NWORDS + 5)= Q(LZFIT+15)      ! zcog_trk
C
C           Ionization in chamber
C
          IF (LDTRK.NE.0)  WORDS( NWORDS + 6)= Q(LDTRK+20) ! CDCMIP
          IF (LFDCT.NE.0)  WORDS( NWORDS + 7)= Q(LFDCT+20) ! FDCMIP
          IF (LVTXT.NE.0)  WORDS( NWORDS + 8)= Q(LVTXT+20) ! VTXMIP

        ENDIF
      ENDIF
      NWORDS = NWORDS + 8

C
C ****  TRD information                   ! optional information
C
      LTRDT = LQ(LZTRK-9)           ! Link to associated TRD bank
      IF (LTRDT .GT. 0) THEN
        WORDS( NWORDS + 1) = Q(LTRDT+6)        ! Likelihood based on total
C                                      ! anode energy
        WORDS( NWORDS + 2) = Q(LTRDT+16)       ! Efficiency derived from
C                                              ! likelihood

        NTRK_TRD = IQ(LTRDT + 2)
        NTRK_LYR1 = MOD(NTRK_TRD,10) + 1
        NTRK_TRD = NTRK_TRD/10
        NTRK_LYR2 = MOD(NTRK_TRD,10) + 1
        NTRK_TRD = NTRK_TRD/10
        NTRK_LYR3 = MOD(NTRK_TRD,10) + 1
        if ((NTRK_LYR1.EQ.1).AND.(NTRK_LYR2.EQ.1).
     &      AND.(NTRK_LYR3.EQ.1)) then
          words(nwords+3) = 1
        else
          words(nwords+3) = 0
        endif
        WORDS( NWORDS + 4)  = Q(LTRDT+5)      ! TRD truncated mean
      ENDIF
C
C **** cal eta of electron/gamma
C
      WORDS( NWORDS + 5 ) = Q(LCLUS+19)       ! Cal eta
      NWORDS = NWORDS + 5

  999 CONTINUE
      IF ( NWORDS .GT. NMAX ) THEN
        CALL ERRMSG('Too many words','QCD_GET_AUX_PELC_WORDS',
     &    ' Number of words overflows array','F')
      ENDIF
      RETURN
      END
