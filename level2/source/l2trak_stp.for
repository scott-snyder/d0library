      SUBROUTINE L2TRAK_STP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Move downloaded level 2 STP banks under the
C-                         level 2 header 
C-
C-   Created  01-NOV-1991   D Claes - From an editted copy of 
C-                          D0$CALOR_FILTER$SOURCE:CL2_SHUNT_STP.FOR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LAYER, L, L2CDC, LSL2H
      INTEGER LDMAT, LDWAL, LDTMP
      INTEGER GZSCDC, GZSL2H
      INTEGER GZDPDH, GZDGNH, GZDALH, GZDMAT, GZDWAL, GZDTMH
C----------------------------------------------------------------------
C
C...prepare for downloading: copy SCDC from under SCDC to under SL2H
C...drop all banks save DTMH (DTMW,DTMD - for drift and delay time constants) 
C...and DGEH (DRFT - for the CDC geometry - these const COULD be hardwired in)
C
C...assumes SCDC is available
C...Check that SL2H bank already exits, else book it
        LSL2H = GZSL2H()
        IF (LSL2H.LE.0) CALL BKSL2H(LSL2H)
        LSCDC = GZSCDC()
        IF (LSCDC.LE.0.) THEN
          CALL ERRMSG('CDC','L2TRAK_STP',
     &          'SCDC banks not found','F')
        ELSE
C          LDPDH = GZDPDH()               ! Keep pedestal banks for proper
C          CALL MZDROP(IXSTP,LDPDH,'L')   ! bilinear mapping of signals
          LDGNH = GZDGNH()
          CALL MZDROP(IXSTP,LDGNH,'L')
C          LDALH = GZDALH()               ! Alignment BANK - Don't drop
C          CALL MZDROP(IXSTP,LDALH,'L')   ! Carries ABSOLUTE positions
          LDMAT = GZDMAT()
          CALL MZDROP(IXSTP,LDMAT,'L')
          LDWAL = GZDWAL()
          CALL MZDROP(IXSTP,LDWAL,'L')
C
C Drop the 'correction' banks below those being kept
C
          LDTMH = GZDTMH()
          DO L = 1, 8
            LDTMP = LC( LC(LDTMH-L) - 1 )
            CALL MZDROP(IXSTP,LDTMP,'L')
          ENDDO
C
C Drop all alignment banks below DALH, except one wire in each layer, assuming
C that all other absolute positions, if needed, can be derived from them
C
          LDALH = GZDALH()
  500     CONTINUE
          DO LAYER = 1, 4
            DO L = 2, 32
              LDTMP = LC( LC(LDALH-LAYER) - L )
              CALL MZDROP(IXSTP,LDTMP,'L')
            ENDDO
          ENDDO
          LDALH = LC(LDALH)
          IF (LDALH.GT.0) GOTO 500
C
C         L2CDC = LC(LSL2H-IZ2SCDC)  ! see if already there
          L2CDC = LC(LSL2H-9)        ! see if already there
          IF (L2CDC.GT.0) CALL MZDROP(IXSTP,L2CDC,'L')    ! drop old data
C         CALL MZCOPY(IDVSTP,LSCDC,IDVSTP,LSL2H,-IZ2SCDC,'L')
          CALL MZCOPY(IDVSTP,LSCDC,IDVSTP,LSL2H,-9,'L')
        ENDIF
  999 RETURN
      END
