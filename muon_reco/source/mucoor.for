C===================================================================
      SUBROUTINE MUCOOR(IFLAG,ITRAK,GOOD_HITS,XHTRAK,YHTRAK,ZHTRAK,
     &  IWADS,IORT,QUAD,XCIN,YCIN,ZCIN,XCOT,YCOT,ZCOT,XA,YA,ZA,XTOC,
     &  YTOC,ZTOC,PTRIG,IFW1,IFW2)
C===================================================================
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Access hits on track from MUOH
C-
C-   Created   1-APR-1991   Don Franks & Akl
C-   Updated  27-MAY-1991   A.Klatchko
C-   Updated  18-JUN-1991   AKL USE MUOH FLAGS 0 & 8 ONLY
C-   Updated  21-JUN-1991   akl  add nominal magnet hit as output
C-   Updated   9-AUG-1991   A.Klatchko  add flag for bad hits
C-   DH 2/92 28 words/MUOH hit plus rotations
C
C  Description: Essentially identical to GTMHOT (adds vernir solutions).
C  ============ Gets information for track ITRACK.
C  Input:
C  ======
C  ITRAK -- Track number from MUOT bank.
C  IFLAG -- either use time (0) or vernir solutions (1) when no pad then
C           time (2)
C  Output:
C  =======
C  NHITS-- # hits on track
C  XHTRAK -- Drift direction coordinate (NOT the 'global' X coordinate)
C  YHTRAK -- Longitudinal coordinate (along the wire)
C  ZHTRAK -- Wire plane coordinate
C        ** These coordinates are kept for each raw hit along the track;
C        ** up to a max of parameter IC hits. Coordinate information comes
C        ** from bank MUOH through pointer from bank MHTT (Track hits).
C  QUAD -- Quadrant number,IORT - orientation
C  XSIN (in)- ZCOT (out) direction cosines of the LEVEL 2 software
C  PTRIG - LEVEL 2 momentum
C  IFW1,IFW2 - flags of LEVEL 2 from MUOH.
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER ITRAK,NHITS,IORT,QUAD,IFW1,IFW2,IFW3,IC
      PARAMETER (IC=30)
      INTEGER IWADS(IC),ISPARE,IFLAG,GOOD_HITS
      REAL XHTRAK(IC),YHTRAK(IC),ZHTRAK(IC)
      REAL XCIN,YCIN,ZCIN,XCOT,YCOT,ZCOT,PTRIG
      REAL ELCAL,ELFE,SPARE1,SPARE2
      INTEGER NOTRAX,LMUOH,IHIT,NHIT,IROT,GZMUOH
      INTEGER IHMUOH,NTYM,NRIF,NT,IPAD,IWIR,NSAMUS
      REAL XA,YA,ZA,XTOC,YTOC,ZTOC,CHIB,CHIL,DP
      REAL DRIF,WIRE
C
C  Executable code:
C  ================
C
      GOOD_HITS = 0
      CALL GTMTRH(NOTRAX)                     !  Header bank MTRH has
      IF (ITRAK.LE.NOTRAX) THEN               !  number of tracks NOTRAX
C
        CALL GTMUOT(ITRAK,NHITS,NSAMUS,QUAD,IFW1,IFW2,IFW3,ISPARE,XA,
     &    YA,ZA,XTOC,YTOC,ZTOC,XCIN,YCIN,ZCIN,XCOT,YCOT,ZCOT,CHIB,CHIL,
     &    PTRIG,DP,ELCAL,ELFE,SPARE1,SPARE2)
C
        LMUOH=GZMUOH(0)                       !  point to MUOH
        IF (NHITS.GT.IC) NHITS=IC             !  truncate to max number
C                                                  of raw hits on track.
C
        DO 40 IHIT=1,NHITS                            !!!! big loop --->
          CALL GTMHTT(ITRAK,IHIT,IWIR,IHMUOH,NRIF,NTYM,IPAD)
C                                               !  Track hit bank MHTT
C                                               !  IHIT identifies hit in
C                                               !  MUOH bank.
          NHIT=LMUOH+28*(IHMUOH-1)
          IF(IQ(NHIT+2) .EQ. 0 .OR. IQ(NHIT+2) .EQ. 8)THEN
            GOOD_HITS=GOOD_HITS+1
            XHTRAK(IHIT)=Q(NHIT+21)+Q(NHIT+26)            !  reads MUOH coords
            YHTRAK(IHIT)=Q(NHIT+22)+Q(NHIT+27)            !
            ZHTRAK(IHIT)=Q(NHIT+23)+Q(NHIT+28)            !  global coords
            IWADS(IHIT)=IWIR                      !  Wire address
            IORT=IQ(NHIT+5)                       !  Orientation
            IROT=IABS(IORT)
CC                                  **  bend view  **
            IF (NRIF.NE.0) THEN
C
              NT=IABS(NRIF)
              DRIF=NRIF/NT*Q(NHIT+14+NT)          !  Reads drift dist
            ELSE
              DRIF= -999999.0
            END IF
            IF (IFLAG.EQ.2)THEN     ! when no Vernier then time devision
CC                                  **  non - bend  **
              IF (IPAD .NE. 0) THEN    !  Reads pad solutions
                IF(IPAD.LT.0)WIRE=Q(NHIT+19)+(((IABS(IPAD))-1)*60.96)
                IF(IPAD.GT.0)WIRE=Q(NHIT+20)+(((IABS(IPAD))-1)*60.96)
              ELSE
                IF (NTYM.NE.0) THEN
C
                  WIRE=Q(NHIT+16+NT)       !  Reads dist along wire
                ELSE
                  WIRE= -999999.0
                END IF
              ENDIF
            ELSEIF(IFLAG.EQ.1)THEN    ! only Vernier soultions
              IF (IPAD .NE. 0) THEN    !  Reads pad solutions
                IF(IPAD.LT.0)WIRE=Q(NHIT+19)+(((IABS(IPAD))-1)*60.96)
                IF(IPAD.GT.0)WIRE=Q(NHIT+20)+(((IABS(IPAD))-1)*60.96)
              ELSE
                WIRE= -999999.0
              END IF
            ELSEIF(IFLAG.EQ.0)THEN    ! only time devisions (might want to
C                                    ! remove)
              IF (NTYM.NE.0) THEN
C
                WIRE=Q(NHIT+16+NT)       !  Reads dist along wire
              ELSE
                WIRE= -999999.0
              END IF
            ENDIF
C
            IF (IROT.EQ.1) THEN                   !  Rotate to non-global
              XHTRAK(IHIT)=Q(NHIT+23)+DRIF                !  coordinates according
              YHTRAK(IHIT)=Q(NHIT+22)+WIRE                !  to orientation.
              ZHTRAK(IHIT)=Q(NHIT+21)
            ELSE IF (IROT.EQ.2) THEN
              XHTRAK(IHIT)=Q(NHIT+23)+DRIF
              YHTRAK(IHIT)=Q(NHIT+21)+WIRE
              ZHTRAK(IHIT)=Q(NHIT+22)
            ELSE IF (IROT.EQ.3) THEN
              XHTRAK(IHIT)=Q(NHIT+21)+DRIF
              YHTRAK(IHIT)=Q(NHIT+22)+WIRE
              ZHTRAK(IHIT)=Q(NHIT+23)
            ELSE IF (IROT.EQ.4) THEN
              XHTRAK(IHIT)=Q(NHIT+22)+DRIF
              YHTRAK(IHIT)=Q(NHIT+21)+WIRE
              ZHTRAK(IHIT)=Q(NHIT+23)
            END IF
          ELSE
            XHTRAK(IHIT)=-999999.0
            YHTRAK(IHIT)=-999999.0
            ZHTRAK(IHIT)=-999999.0
          ENDIF
C
   40   CONTINUE                                      !!!  end loop ---->
      END IF
C
      RETURN
      END
