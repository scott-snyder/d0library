      SUBROUTINE OBJECT_ALLJET(IDX, NMAX, ARRAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return quantities for object JET, using
C-   JETS banks only (JNEP banks are not used).
C-
C-   Inputs  : IDX      [I]   Object Number
C-             NMAX     [I]   Maximum number of quantities to return
C-   Outputs : ARRAY(*) [R]   Array of quantities
C-
C-              ARRAY(1..9)
C-                            PX, PY, PZ, E, ET, ETA, PHI, DET_ETA, QUALITY
C-                    I = 9
C-              ARRAY(I+1)    Sig**2(Ex)
C-              ARRAY(I+2)    Sig**2(Ey)
C-              ARRAY(I+3)    RMS Eta width
C-              ARRAY(I+4)    RMS Phi width
C-              ARRAY(I+5)    Fraction of EM Et
C-              ARRAY(I+6)    Flag for merging/splitting
C-              ARRAY(I+7)    Number of cells above threshold (1GeV def)
C-              ARRAY(I+8)    Fraction of ICD/MG Et(EtICD+EtMG/TOTAL_ET)
C-              ARRAY(I+9)    Fraction of CH Et (Et CH/TOTAL_ET)
C-              ARRAY(I+10)   Ratio of hottest to next-hottest cell
C-              ARRAY(I+11)   Number of CDC tracks in jet cone
C-              ARRAY(I+12)   Number of TOWERS comprising 90% of jet Et
C-              ARRAY(I+13)   ConeSize (-1.0 for Nearest Neighbor)
C-              ARRAY(I+14)   Phi Difference between MET and JET (PNUT(2))
C-              ARRAY(I+15)   Spare
C-              ARRAY(I+16)   Energy Correction Flag
C-              ARRAY(I+17)   Sig**2(Ez)
C-              ARRAY(I+18)   dExdEy
C-              ARRAY(I+19)   dExdEz
C-              ARRAY(I+20)   dEydEz
C-
C-   Controls:
C-
C-   Created   1-DEC-1992   Harrison B. Prosper
C-   Updated  14-DEC-1992   Harrison B. Prosper   apply JNEP
C-   Updated  30-DEC-1992   Serban D. Protopopescu  (calculate DET_ETA)
C-   Updated   3-FEB-1993   K. Wyatt Merritt  Make separate objects out of
C-                          jets with and without applying JNEP banks.  This
C-                          routine stores jets from JETS banks only;
C-                          OBJECT_JET stores jets using JNEP banks
C-                          when present.
C-   Updated  28-FEB-1993   Harrison B. Prosper  Jets without Jnep
C-   Updated  23-APR-1993   Harrison B. Prosper  Add conesize
C-   Updated  29-APR-1993   K. Wyatt Merritt  add angle between jet and MET
C-   Updated   4-MAY-1993   Marc Paterno  Assignment corrections
C-   Updated   6-MAY-1993   Marc Paterno  Prevent from reading off end of JETS
C-                                        banks
C-   Updated  18-MAY-1993   Stan M. Krzywdzinski, Harrison B. Prosper
C-                          Add error matrix
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDX, NMAX
      REAL    ARRAY(*)
      INTEGER NOBJS, NSIZE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:GM_PARAMS.DEF'
      INCLUDE 'D0$PARAMS:CAPH.DEF'
C----------------------------------------------------------------------
      INTEGER STATUS, I, J, K, L, II, JJ, NN
      INTEGER NUMBER, TOTAL, JET_TYPE
      INTEGER LJETS, LJETS1, LCAPH, GZJETS, NZBANK, IALGO
      INTEGER LVERT, GZVERT, LPNUT, GZPNUT, BANKLENGTH
      REAL    ZVTX, THETA
      REAL    MPHI, PHI_DIFFERENCE
C----------------------------------------------------------------------
      INTEGER MINNUM
      PARAMETER( MINNUM =  9 )
      INTEGER MAXBUF
      PARAMETER( MAXBUF = MINNUM + 20 )
C----------------------------------------------------------------------
      INTEGER IBUFFER(MAXBUF), JETNUM(MAXBUF)
      REAL    BUFFER(MAXBUF), ETMIN, ETJETS, ET(MAXBUF)
      EQUIVALENCE (IBUFFER(1), BUFFER(1))
C----------------------------------------------------------------------
      SAVE NUMBER, TOTAL, ETMIN, ET, JETNUM
C----------------------------------------------------------------------
C
C ****  OBJECT: JET
C
      II = MIN(IDX, NUMBER)
      NN = MIN(NMAX, MAXBUF)
      CALL VZERO(BUFFER, NN)
C
      LPNUT = GZPNUT(2)
      IF ( LPNUT .LE. 0 ) THEN         ! no PNUT
        MPHI = 0.0
      ELSE
        MPHI = Q(LPNUT + 10)
      ENDIF                        ! if lpnut .le. 0
C
C ****  Get address of JETS banks
C
      IF ( II .GT. 0 ) THEN
        JJ = IABS(JETNUM(II))      !GET JET LOCATION IN LINEAR CHAIN
        LJETS1 = GZJETS()
C
C ****  Find the length of the JETS bank (we use this to make sure we don't read
C ****  off the end of the bank)
C
        IF ( LJETS1 .GT. 0 ) THEN
          BANKLENGTH = IQ(LJETS1 - 1)
        ENDIF
C
        I = 0
        DO WHILE ( I .LT. TOTAL )
          LJETS = LJETS1
          I = I + 1
          IF ( I .EQ. JJ ) THEN
            I = TOTAL
          ENDIF
          LJETS1 = LQ(LJETS)
        ENDDO
C
C ****  FIXED PART
C
        CALL UCOPY(Q(LJETS+2), BUFFER(IPX), 6)
        BUFFER(IETA) = Q(LJETS+9)
        BUFFER(IPHI) = Q(LJETS+8)
        LVERT = GZVERT(1)
        ZVTX = 0.0
        IF (LVERT .GT. 0) ZVTX = Q(LVERT+5)
        THETA = Q(LJETS+7)
        CALL DET_ETA(ZVTX, THETA, BUFFER(IDETA))
C
C ****  OTHER
C
        CALL UCOPY(Q(LJETS+10), BUFFER(IX1), 5)
        BUFFER(IX6) = FLOAT(IQ(LJETS+15)) ! split/merge flag is an integer
C
C ****  Start of protection from reading off end of bank
C
        IF ( BANKLENGTH .GE. 16 ) THEN
          BUFFER(IX7) = FLOAT(IQ(LJETS+16)) ! number of cells is an integer
          IF ( BANKLENGTH .GE. 19) THEN

            CALL UCOPY(Q(LJETS+17), BUFFER(IX8), 3)
C
C ****  Add Cone Size
C
            IF ( BANKLENGTH .GE. 21 ) THEN
              BUFFER(IX11) = FLOAT(IQ(LJETS+20))
              BUFFER(IX12) = FLOAT(IQ(LJETS+21))
C
C ****  Add rest of covariance matrix
C
              IF ( BANKLENGTH .GE. 26 ) THEN
                BUFFER(IX16) = Q(LJETS+26)  ! Correction Status Flag
                CALL UCOPY(Q(LJETS+22), BUFFER(IX17), 4)
              ENDIF
            ENDIF                                ! if banklength .ge. 21
          ENDIF                                  ! if banklength .ge. 19
        ENDIF                                    ! if banklength .ge. 16
C
        LCAPH = LQ(LJETS+1)           !Origin link to CAPH
        JET_TYPE = IQ(LCAPH+K_ALGORITHM)
        IF ( JET_TYPE .EQ. A_CONE_JET ) THEN
          BUFFER(IX13) = Q(LCAPH+K_BASE+1)
        ELSE
          BUFFER(IX13) = -1.0
        ENDIF
C
        BUFFER(IX14) = PHI_DIFFERENCE(MPHI, Q(LJETS+8))
C
        CALL UCOPY(BUFFER, ARRAY, NN)
      ENDIF
      RETURN
C
C ****  Count number of jets
C
      ENTRY NOBJ_ALLJETS(NOBJS, NSIZE)
      LJETS = GZJETS()
      IF ( LJETS .GT. 0 ) THEN
        NUMBER = NZBANK(IXCOM, LJETS)
      ELSE
        NUMBER = 0
      ENDIF
      TOTAL = NUMBER  ! TOTAL NUMBER OF JETS
C
C ****  Get Et cut
C
      IF ( NUMBER .GT. 0 ) THEN
        LJETS = GZJETS()
        LCAPH = LQ(LJETS+1)
        IALGO = IQ(LCAPH+K_ALGORITHM)
C
        IF ( IALGO .EQ. A_CONE_JET ) THEN
          ETMIN = Q(LCAPH+7)
        ELSE
          ETMIN = Q(LCAPH+11)
        ENDIF
C
        JJ = 0
        DO I = 1, NUMBER
C
          ETJETS = Q(LJETS+6)
          IF ( JJ .LT. MAXBUF ) THEN
            JJ = JJ + 1
            ET(JJ) = 1.0/ETJETS
            JETNUM(JJ) =  I          !NOTE JETS LOCATION IN LINEAR CHAIN
          ENDIF
          LJETS = LQ(LJETS)
        ENDDO
        NUMBER = JJ
C
C ****  Sort according to 1/ET; That is, the largest ET first
C
        IF ( NUMBER .GT. 0 ) THEN
          CALL SRTFLT(ET, NUMBER, JETNUM)
          DO I =  1, NUMBER
            ET(I) = 1.0/ET(I)
          ENDDO
        ENDIF
      ENDIF
C
      NOBJS = NUMBER
      NSIZE = MAXBUF
C
  999 RETURN
      END
