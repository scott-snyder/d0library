      SUBROUTINE SAMULT(NHITS,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : return number of hits in SAMUS stations
C-
C-   Inputs  : none
C-   Outputs : NHITS(6) number of hits in SAMUS stations
C-             OK= -1 fault: no hit information found
C-               =  1 number of hits from SAPH
C-               =  2 --- SAHT
C-               =  3 no SAPH found set hits to 0
C-   Controls: none
C-
C-   Created   1-FEB-1995   Andrei Mayorov
C-   Modified 13-MAR-1996   Andrew Brandt allow use with special DST's w/SAPH
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QCD_NTUP_INFO.INC/LIST'
      INTEGER N_PLANES, N_HEADER, N_STATION, N_SECTION, NWSAPH
      PARAMETER (N_HEADER=6, N_STATION=6, N_SECTION=6)
      PARAMETER (N_PLANES=3, NWSAPH=19)
      INTEGER NHITS(6),POINTER,IPL,OK,IMOD
      INTEGER LSAHS, LSSEC, LSSTG, LSAPH
      INTEGER MUDVER, GZSAHS, GZSSEC
      INTEGER GZSAPH,LMUHT,GZMUHT,LHIT
      EXTERNAL MUDVER, GZSAHS, GZSSEC, GZSAPH,GZMUHT,
     &  GZSAHT
      INTEGER IST, ISEC,  itube, nh,LTG
      INTEGER IHIT
      INTEGER pmdat(36)
      DATA PMDAT/400, 414, 420, 434, 440, 454,  ! X2
     *           401, 413, 421, 433, 441, 453,  ! U2
     *           402, 412, 422, 432, 442, 452,  ! Y2
     *           404, 410, 424, 430, 444, 450,  ! X1
     *           405, 417, 425, 437, 445, 457,  ! U1
     *           406, 416, 426, 436, 446, 456/  ! Y1
C
      LOGICAL FIRST,SAM
      INTEGER LENG,TRULEN,I
      CHARACTER*32 PNAME
      DATA FIRST/.TRUE./
      SAVE SAM
C----------------------------------------------------------------------
      IF(FIRST) THEN
        SAM = .FALSE.
        DO I = 1, NPACK
          PNAME = PACK(I)
          LENG = MIN(TRULEN(PNAME),32)
          IF(PNAME(1:LENG).EQ.'SAMRECO') SAM = .TRUE.
        END DO
        FIRST = .FALSE.
      END IF
C
      OK=-1
      CALL UZERO(NHITS,1,6)
C
C If SAMRECO is absent assume that we can only get hits from SAPH
C
C
      IF(.NOT.SAM) THEN
        LSAPH=GZSAPH()
        IF(LSAPH.GT.0) THEN
          OK=1
          LMUHT=GZMUHT(0)
          NH=IQ(LMUHT+5)
          DO IHIT=1,NH
            LHIT=LSAPH+(IHIT-1)*NWSAPH
            IMOD=IQ(LHIT+1)/256
            ITUBE=IQ(LHIT+1)-IMOD*256
            CALL SAFRPM(IMOD,IST,ISEC)
            NHITS(IST)=NHITS(IST)+1
          END DO
        ELSE
          OK=3
          CALL ERRMSG('SAMULT','SAMULT',
     &              ' NO SAPH BANK ','W')
        ENDIF
      ELSE
C
C SAMRECO case
C
        LSAHS=GZSAHS()
C
C First check SAHS bank
C
        IF (LSAHS.GT.0) THEN
          POINTER=0
          DO IST=1,6
            DO IPL=1,3
              POINTER=POINTER+1
              NHITS(IST)=NHITS(IST)+IQ(LSAHS+POINTER)
            END DO
          END DO
          OK=2
        ELSE
C
C If no SAHS bank and no SAPH use MUD bank
C
          LSAPH=GZSAPH()
          IF(LSAPH.LE.0) THEN
            OK = MUDVER(0)
            IF (OK.EQ.-1) GO TO 999
C
C         Create pointer banks, unpack raw data
C
            lmuht=gzmuht(0)
            IF(lmuht.NE.0) CALL MZDROP(IXMAIN,LMUht,' ')
            CALL BKMUHT(0,0,LMUHT)
            CALL MUDPAK(OK)
            IF (OK.LT.0) GO TO 999
            CALL muhits(36,pmdat,ok)
            IF (OK.LT.0) GO TO 999
            LSAPH=GZSAPH()
          END IF
C
C If no SAHS but there is a SAPH use it
C
          IF(LSAPH.GT.0) THEN
            LMUHT=GZMUHT(0)
            NH=IQ(LMUHT+5)
            DO IHIT=1,NH
              LHIT=LSAPH+(IHIT-1)*NWSAPH
              IMOD=IQ(LHIT+1)/256
              ITUBE=IQ(LHIT+1)-IMOD*256
              CALL SAFRPM(IMOD,IST,ISEC)
              NHITS(IST)=NHITS(IST)+1
            END DO
            OK=1
          ENDIF
        END IF
      END IF
  999 RETURN
      END
