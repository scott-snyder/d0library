      SUBROUTINE VCTH_TO_VTTH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build VTTH banks from VTXT, VCTH and VCHT.
C-
C-   Inputs  : Requires VTXT, VCTH and VCHT (and VSEC)
C-   Outputs : Builds a VTTH bank for each VTXT. Only loads hit-pointers,
C-             and residuals, but not segment numbers (unavailable).
C-             CAUTION: If VSEC has not been built for one or more sectors,
C-             this routine will load the hit-pointers correctly (pointing 
C-             at non-existent VSECs), but sets the residuals in VTTH to 
C-             zero for the missing VSECs.
C-   Controls: 
C-
C-   Created  28-FEB-1994   Al Clark
C-   Updated  24-JUN-1994   Al Clark  Clean up for library insertion 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INTEGER LVTRH, GZVTRH, LVTXT, LVTTH
      INTEGER LVCHT, GZVCHT
      INTEGER LVCTH, GZVCTH
      INTEGER NH, IH, IHIT
      INTEGER IADD, LAY, SEC, WIRE, HITNUM, WIRLAY
      INTEGER VCHT_SEC(0:2)
      INTEGER LLVCTH, LPVCTH, NHTSWD, MAX_VTXT
      INTEGER VTXT_NUM, PAK_POS
      INTEGER L_R
      INTEGER ON_XY_MSK, ON_RZ_MSK
      INTEGER VERS,MAX_VERS
      INTEGER NHIT
      REAL    XHIT(24), YHIT(24), ZHIT(24), WT(24), WTZ(24)
      REAL    XG, YG, ZG, AL, ALZ, TGALZ, THETA, CHISQZ, SXY
      REAL    SINAL, COSAL
      LOGICAL ON_XY, ON_RZ
      PARAMETER (MAX_VERS=1)
C
C ****  VCHT packing parameters: SECTOR HEAD WORD
C
      INTEGER COUNT_OFFSET, COUNT_LENGTH, NHVSEC_OFFSET, NHVSEC_LENGTH
      INTEGER SECID_OFFSET
      PARAMETER ( COUNT_OFFSET = 0 )
      PARAMETER ( COUNT_LENGTH = 13 )
      PARAMETER ( NHVSEC_OFFSET = 13 )
      PARAMETER ( NHVSEC_LENGTH = 11 )
      PARAMETER ( SECID_OFFSET = 24 )
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./

C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL BKVTTH_2ARG_INI    ! Initialize BKVTTH for 2 arg calls
      ENDIF
C
      LVTRH = GZVTRH()
      IF ( LVTRH .LE. 0) GO TO 999    ! No tracks
      LVTXT = LQ(LVTRH-1)
      IF ( LVTXT .LE. 0) GO TO 999    ! No tracks

      LVCHT = GZVCHT()
      IF (LVCHT .LE. 0) GO TO 999     ! Need VCHT bank
      VERS = IQ(LVCHT+1)
      IF (VERS .LE. 0) GO TO 999   ! Only work with Vers 1 or higher

      LVCTH = GZVCTH()
      IF (LVCTH .LE. 0) GO TO 999   ! ALSO NEED VCTH

C**  Next, loop through VTXT, build VTTH banks

      LVTXT = LQ(LVTRH-1)
      DO WHILE ( LVTXT .GT. 0 )

C**   Check VTTH bank; if it exists, drop it and re-book.

        LVTTH = LQ(LVTXT-1)
        IF ( LVTTH .GT. 0 ) CALL MZDROP(IXCOM, LVTTH, ' ')
        CALL BKVTTH_2ARG( LVTXT, LVTTH )
        LVTXT = LQ(LVTXT)
      ENDDO   ! WHILE LVTXT .GT. 0

C**  Booking VTTH may have caused a garbage collection, so reset all the 
C**  Zebra pointers.

      LVTRH = GZVTRH()
      LVCHT = GZVCHT()
      LVCTH = GZVCTH()

      NHTSWD = IQ(LVCTH+4)            ! # of packed hits per wd in VCTH
      MAX_VTXT = IQ(LVCTH+5)          ! max # of vtxt allowed in VCTH
      LLVCTH = LVCTH + IQ(LVCTH+2)+1  ! RUNNING PNTR INTO VCTH; wd 1 of 1st blk

C**  Loop over VTXT again, set pointers, residuals

      LVTXT = LQ(LVTRH-1)
      DO WHILE ( LVTXT .GT. 0 )

        LVTTH = LQ(LVTXT-1)
        VTXT_NUM  = IQ(LVTXT-5)
        IF (VTXT_NUM .LE. MAX_VTXT ) THEN ! No data in VCTH if track-num too big

C**  UNPACK THE TRACK HEADER; get sector for each layer
          IF ( VTXT_NUM .NE. IBITS(IQ(LLVCTH), 0, 9)) THEN
            CALL ERRMSG('Track number mismatch',
     &            'VCTH_TO_VTTH',' ','W')
          ENDIF
          VCHT_SEC(0) = IBITS( IQ(LLVCTH),  9, 4)
          VCHT_SEC(1) = IBITS( IQ(LLVCTH), 13, 5)
          VCHT_SEC(2) = IBITS( IQ(LLVCTH), 18, 5)

          NH = IQ(LVTXT+2)      ! # of hits
          ON_XY_MSK = IQ(LVTXT+3)
          ON_RZ_MSK = IQ(LVTXT+4)

          IH = 0
          PAK_POS = 0
          LPVCTH = LLVCTH + 1             ! PTR to next wd to be unpacked
          DO WIRLAY = 0, 23
            ON_XY = BTEST( ON_XY_MSK, WIRLAY)
            IF (ON_XY) THEN
              IH = IH + 1
              WIRE = IBITS(WIRLAY, 0, 3)
              LAY =  IBITS(WIRLAY, 3, 2)
              SEC = VCHT_SEC(LAY)
              HITNUM = IBITS( IQ(LPVCTH), PAK_POS*8, 7)
              L_R =    IBITS( IQ(LPVCTH), PAK_POS*8+7, 1)
              IADD = L_R
              CALL MVBITS(WIRE, 0, 3, IADD, 1)
              CALL MVBITS(SEC,  0, 5, IADD, 4)
              CALL MVBITS(LAY,  0, 2, IADD, 9)
              IQ(LVTTH + 6 + (IH-1)*4) = IADD
              IQ(LVTTH + 7 + (IH-1)*4) = HITNUM
              PAK_POS = PAK_POS + 1
              IF ( PAK_POS .GE. NHTSWD) THEN
                PAK_POS = 0
                LPVCTH = LPVCTH + 1
              ENDIF
            ENDIF   ! ON_XY
          ENDDO     ! WIRLAY
          IF (IH .NE. NH) THEN
            CALL ERRMSG('HIT number mismatch',
     &            'VCTH_TO_VTTH','BAD UNPACK COUNT ','W')
          ENDIF
C**
C**  Here, need to get hit info from VCHT, calc XHIT, YHIT, ZHIT, WT and WTZ
C**
          CALL VTXTHT(LVTXT, NHIT, XHIT, YHIT, ZHIT, WT, WTZ)
C**
C**  Now calc residuals, loop over ih and store residuals
C**
          AL = Q(LVTXT + 6)
          XG = Q(LVTXT + 7)
          YG = Q(LVTXT + 8)
          SINAL = SIN(AL)
          COSAL = COS(AL)
          CHISQZ = Q(LVTXT + 13)
          IF ( CHISQZ .GT. 0. ) THEN
            THETA = Q(LVTXT + 9)
            ZG = Q(LVTXT + 11)
            ALZ = 1.570796 - THETA    ! Value for PI/2 same as in FTVTXT
            TGALZ = TAN(ALZ)
          ENDIF
          IH = 0
          DO WIRLAY = 0, 23
            ON_XY = BTEST( ON_XY_MSK, WIRLAY)
            IF (ON_XY) THEN
              IH = IH + 1
              IHIT = WIRLAY+1
              Q(LVTTH + 8 + (IH-1)*4) = 
     &          (YHIT(IHIT)-YG)*COSAL-(XHIT(IHIT)-XG)*SINAL
              ON_RZ = BTEST( ON_RZ_MSK, WIRLAY)
              IF ( ON_RZ .AND. ( CHISQZ .GT. 0.)) THEN
                SXY = (XHIT(IHIT)-XG)*COSAL+(YHIT(IHIT)-YG)*SINAL
                Q(LVTTH + 9 + (IH-1)*4) = ZHIT(IHIT)-ZG-SXY*TGALZ
              ENDIF ! ON_RZ
            ENDIF   ! ON_XY
          ENDDO     ! WIRLAY
          LLVCTH = LLVCTH + 2 + (NH-1)/NHTSWD
        ENDIF   ! Do we expect VCTH data?
        LVTXT = LQ(LVTXT)
      ENDDO   ! WHILE LVTXT .GT. 0
  999 RETURN
      END
