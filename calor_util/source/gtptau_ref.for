      SUBROUTINE GTPTAU_REF(ITAU, NENTRIES, XDATA, IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Get the tracking related and other additional
C-                          information on the ITAUth tau particle found
C-                          from PTAU bank, except those directly from PTAU.
C-
C-   Inputs  : ITAU = sequence number of tau selected
C-
C-   Outputs :
C-             NENTRIES = number of filled entries in XDATA
C-             XDATA(1) = detector Eta of the ITAUth tau jet
C-             XDATA(2) = EM fraction of the ITAUth tau jet
C-             XDATA(3) = number of tracks
C-             XDATA(4) = Distance to tau jet in eta-phi for 1st track
C-             XDATA(5) = Detector eta for 1st track
C-             XDATA(6) = impact parameter in X-Y plane for 1st track
C-             XDATA(7) = distance to VERTEX_Z along Z axis for 1st track
C-
C-             XDATA(4) - XDATA(7) repeated for each additional track, up to a
C-                                 total of NMAXTRACKS associated tracks
C-
C-             IER:
C-                     =   0 OK
C-                      = -3 MDST data; GTPTAU_REF can't digest this
C-                      = -4 No PTAU bank
C-                      = -5 ITAUth PTAU not found
C-
C-   Created  17-MAY-1993 Hailin Li
C-   Modified 20-May-1993 Hailin Li   Drop transverse mass entry
C-   Updated  24-MAY-1993   Marc Paterno  Reworked from original template by
C-                                        Hailin Li.
C-                 Unused words now return 0.0, not -999.0
C-                 Altered so that no more than NMAXTRACKS tracks will be
C-                    returned.
C-                 Fixed bug in ETA calculation
C-                 Fixed error checking for ETA calculation
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  WORDS
      PARAMETER (WORDS = 20)
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER  ITAU, NENTRIES, IER
      REAL     XDATA(WORDS)
C----------------------------------------------------------------------
      INTEGER  LPTAU, LZTRK, LZFIT, GZPNUT, GZZFIT
      INTEGER  GZPTAU, LJETS, GZVERT, LVERT, NMAXTRACKS, NRETURNED
      EXTERNAL GZPTAU, GZVERT, GZPNUT, GZZFIT
      PARAMETER (NMAXTRACKS = 3)
      CHARACTER*4 PATH
      REAL     SMALL
      PARAMETER (SMALL = 1.0E-5)
      INTEGER  NTRK, I, NTRKREP
      PARAMETER (NTRKREP = 4)
      REAL     ZVTX, PHI, THETA, ETA, DETECTOR_ETA
      REAL     PHI_TAU, ETA_TAU
C---------------------------------------------------------------------
C
C ****  Initialization
C
      CALL VZERO (XDATA, WORDS, WORDS)
      NENTRIES = 0
C
C ****  Require RECO path -- others not handled
C
      CALL PATHGT (PATH)
      IF ( PATH .NE. 'RECO' ) THEN
        IER = -3
        RETURN
      ENDIF
C
C ****  Find the ITAU'th bank.
C
      IF ( ITAU .LE. 0 ) THEN
        IER = -5
        RETURN
      ENDIF

      LPTAU = GZPTAU()
      IF ( LPTAU .LE. 0 ) THEN
        IER = -4
        RETURN
      ENDIF

      I = 1

      DO WHILE ( I .LT. ITAU )
        LPTAU = LQ(LPTAU)
        IF ( LPTAU .LE. 0 ) THEN
          IER = -5
          RETURN
        ENDIF
        I = I + 1
      ENDDO
C
C ****  Now get the information we require from this bank.
C ****  Get detector eta
C
      THETA = Q(LPTAU+8)
      LVERT = GZVERT(1)                 ! primary vertex
      IF ( LVERT .GT. 0) THEN
        ZVTX = Q(LVERT+5)
        CALL DET_ETA(ZVTX, THETA, DETECTOR_ETA)
        XDATA(1) = DETECTOR_ETA
      ENDIF
C
C ****  Identify tau jet with a jet to get em fraction
C
      LJETS = LQ(LPTAU-2)
      IF (LJETS .GT. 0) THEN
        XDATA(2) = Q(LJETS+14)                      ! EM fraction
      ENDIF
C
C ****  ET_TAU = Q(LPTAU+7)
C
      PHI_TAU = Q(LPTAU+9)
      ETA_TAU = Q(LPTAU+10)
C
C ****  Number of tracks
C
      NTRK = IQ(LPTAU-3) - 2
      XDATA(3) = FLOAT(NTRK)
C
C **** Get tracking information
C
      NRETURNED = MIN (NTRK, NMAXTRACKS)            ! number of tracks returned
      NENTRIES = NRETURNED*NTRKREP + 3

      DO I = 1, NRETURNED
        LZTRK = LQ(LPTAU - 2 - I)

        IF (LZTRK .GT. 0) THEN
          LZFIT = GZZFIT(LZFIT)
          IF (LZFIT .GT. 0) THEN
            PHI = Q(LZFIT+10)        ! Phi of a track
            THETA = Q(LZFIT+13)      ! Theta of a track
            IF ( THETA .LE. 0.0 ) THEN
              ETA = 10.0
            ELSE IF ( THETA .GE. SNGL(PI)) THEN
              ETA = -10.0
            ELSE
              ETA = - LOG (TAN(THETA/2.0))
            ENDIF
            XDATA((I-1)*NTRKREP+4) =
     &        SQRT((PHI-PHI_TAU)**2 + (ETA-ETA_TAU)**2)
            IF ( LVERT .GT. 0) THEN
              CALL DET_ETA(ZVTX, THETA, DETECTOR_ETA)
              XDATA((I-1)*NTRKREP+5) = DETECTOR_ETA
            ENDIF
            XDATA((I-1)*NTRKREP+5) = Q(LZFIT+32)       ! Impact parameter in XY
            XDATA((I-1)*NTRKREP+6) = Q(LZFIT+33)       ! Distance to VERTEX_Z
          ENDIF
        ENDIF
      ENDDO                                          ! i = 1, nreturned

      RETURN
      END
