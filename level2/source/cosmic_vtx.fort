C DEC/CMS REPLACEMENT HISTORY, Element TOOL1.FOR
C *1    19-SEP-1989 15:59:38 HOFTUN "Dummy TOOL included as example"
C DEC/CMS REPLACEMENT HISTORY, Element TOOL1.FOR
      SUBROUTINE COSMIC_VTX(PS_NUM,HARDWARE,RESULT_FLAG,FORCE_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Test TOOL # 1
C-
C-   Inputs  : PARAM_SET_NUMBER : Number in series of parameters to use.
C-             HARDWARE : Mask with bit set for Level-1 trigger which
C-                        started this filter.
C-   Outputs : RESULT_FLAG : Flag set to TRUE when TOOL wants event passed
C-             FORCE_FLAG :  Flag set to TRUE when TOOL wants event passed
C-                           without further filtering.
C-   Controls: None
C-
C-   Created  17-JAN-1989   Jan S. Hoftun
C-   Updated   3-FEB-1992   Ed Oltman  made crude vtx l2 filter -- lots of help
C-                              from Srini 
C-   Updated  09-APR-1992   Liang-ping Chen 
C_                          Updated the method of wire hit counting
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PS_NUM,HARDWARE
      LOGICAL RESULT_FLAG,FORCE_FLAG
      REAL    Y
      REAL    PEAKMAX
      PARAMETER MAX_CLUS = 5
      INTEGER HITLST(MAX_CLUS,3,0:7)
      INTEGER I,ERR,NL,LAY,NS(0:2),SEC,NW,WIR,NCLUS
      INTEGER NSEC(0:2)
      LOGICAL FIRST/.TRUE./
      LOGICAL STATUS
      INTEGER PK_THRESH(128),NW_THRESH(128),SEC_THRESH(128)
      INTEGER LAY_THRESH(128),WEND(128)
      DATA NSEC/15,31,31/
C----------------------------------------------------------------------
      RESULT_FLAG = .FALSE.
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('COSMIC_VTX',ERR)
        IF (ERR .NE. 0) THEN
          CALL ERRMSG(' COSMIC_VTX',' RCP FILE PROBLEM',
     &      ' RCP FILE NOT OPENED: NO EVENTS WILL PASS!!','W')
          GO TO 999
        ENDIF
        CALL EZGET('PK_THRESH',PK_THRESH,ERR)
        CALL EZGET('NW_THRESH',NW_THRESH,ERR)
        CALL EZGET('SEC_THRESH' ,SEC_THRESH ,ERR)
        CALL EZGET('LAY_THRESH' ,LAY_THRESH ,ERR)
        CALL EZGET('WEND'       ,WEND       ,ERR)
        CALL EZRSET
      ENDIF

C
C  Unpack Crate banks
C
      CALL VCRUNP(STATUS)               ! Unpack data
      IF (.NOT.STATUS) THEN
        GO TO 999
      ENDIF
C
C  Loop over Layers, sectors ,wires and determine whether the event
C  have enough  hited wires, sectors and layers.
C
      NL = 0
      DO LAY = 0,2
        NS(LAY) = 0
        DO SEC = 0,NSEC(LAY)
          NW = 0
          DO WIR = 0,7
            CALL VFSTRK(LAY,SEC,WIR,WEND(PS_NUM),NCLUS,HITLST)
            PEAKMAX=0.
            IF (NCLUS .GT. 0) THEN
              DO I = 1,MIN0(NCLUS,MAX_CLUS)
                IF (HITLST(I,3,WIR).GT.PEAKMAX) PEAKMAX=HITLST(I,3,WIR)
              ENDDO
              IF (PEAKMAX.GT.PK_THRESH(PS_NUM)) NW=NW+1
            ENDIF
            IF (NW .GE. NW_THRESH(PS_NUM)) THEN
              NS(LAY)=NS(LAY)+1
              GOTO 30
            ENDIF
          ENDDO                         ! WIRE LOOP
30        IF (NS(LAY) .GE. SEC_THRESH(PS_NUM)) THEN
            NL=NL+1
            GOTO 40
          ENDIF
        ENDDO                           ! SECTOR LOOP
40      IF (NL .GE. LAY_THRESH(PS_NUM)) THEN
          RESULT_FLAG = .TRUE.          
          GOTO 999
        ENDIF
      ENDDO                             !LAYER LOOP
 999  RETURN
      END
C
C 
      SUBROUTINE VCRUNP(STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack Crates and get pointers to Logical channels.
C-
C-   Inputs  : Minimum Phi supplied by Calorimeter
C-             Maximum Phi supplied by Calorimeter
C-   Outputs : none
C-   Controls: STATUS = Set false for bad data
C-
C-   Created  22-AUG-1990   Srini Rajagopalan
C-   Updated   2-FEB-1992   Ed Oltman   CHANGE 2'S TO 1'S AND 4'S TO 3'S ETC
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
C
      LOGICAL IMAP(0:9)
      INTEGER MAP(0:65000)
      COMMON /VTXMAP/ IMAP,MAP
C
      PARAMETER MAX_CLUS = 5
      INTEGER HITLST(MAX_CLUS,3,0:7)
      INTEGER ICRT,LENCRT
      INTEGER LCDD1,ADDR,CHNL,LENGTH
      INTEGER POINT,CRTID,MASK,START
      INTEGER PHIMIN,PHIMAX
      LOGICAL STATUS
      PARAMETER (MASK = 'FFFF'X)
      INTEGER TOTAL
C
C----------------------------------------------------------------------
C
      TOTAL = 0
      STATUS = .TRUE.
      DO 5 ICRT = 0,9
        IMAP(ICRT) = .FALSE.
    5 CONTINUE
C
      IF (LHEAD.EQ.0) THEN              ! Check if Top bank exists
        STATUS = .FALSE.
        GO TO 999
      ENDIF
      LCDD1 = LQ(LHEAD-IZCDD1)
      IF (LCDD1.LE.0) THEN              ! Check if CDD1 bank exists
        STATUS = .FALSE.
        GO TO 999
      ENDIF
C
      POINT = LCDD1 + IQ(LCDD1-1) - 16      ! Last data word
C
C  Loop over all sense wire crates.
C
      DO 10 ICRT = 0,9
        IF (POINT.LE.IQ(LCDD1-1)+4) GO TO 999 
        CRTID = IAND(IQ(POINT-2), MASK)
        IMAP((CRTID-3)/10) = .TRUE.     ! Crate data found
        LENCRT = IQ(POINT-3)
        START = POINT - LENCRT
        POINT = POINT - 4
        DO WHILE (POINT.GT.START+4)
          ADDR = POINT
          CHNL = IAND(ISHFT(IQ(POINT), -16), MASK)
C
C                                         ! Minimum expected LENGTH is 4
          LENGTH = IAND(IQ(POINT), MASK)  ! But crate problems may return 0
          IF (LENGTH.LT.4) GOTO 10        ! Skipping bad crates
C
          IF (MOD(CHNL,2) .EQ. 0 .AND. LENGTH .GT. 4) TOTAL = TOTAL + 1
          MAP(CHNL) = ADDR              ! Store pointer to channel address
          POINT = POINT - LENGTH/4
        ENDDO
        POINT = START
   10 CONTINUE
  999 RETURN
      END
      SUBROUTINE VFSTRK(LAYER,SECTOR,WIRE,WEND,NCLUS,HITLST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-AUG-1990   Srini Rajagopalan
C-   Updated   2-FEB-1992   Ed Oltman  make it VTX-esk 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      LOGICAL IMAP(0:9)
      INTEGER MAP(0:65000)
      COMMON /VTXMAP/ IMAP,MAP
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
C
      PARAMETER MAX_CLUS = 5
      INTEGER HITLST(MAX_CLUS,3,0:7)
      INTEGER LAYER,SECTOR,WIRE,WEND,LABEL
      INTEGER LCHA,CHNL,P,INDEX,MAX
      INTEGER POINT,END,HITLEN,HITADR
      INTEGER MASK8,MASK16,NCLUS,IER
C
      PARAMETER (MASK8 = 'FF'X)
      PARAMETER (MASK16 = 'FFFF'X)
C
C----------------------------------------------------------------------
      NCLUS = 0
      LABEL = 512*LAYER + 16*SECTOR + 2*WIRE + WEND
      POINT = MAP(LABEL)              ! Get pointer to channel data
      LCHA  = IAND(IQ(POINT),MASK16)
      IF (LCHA.LE.4) GO TO 999
      CHNL = IAND(ISHFT(IQ(POINT), -16), MASK16)
      IF (CHNL.NE.LABEL) GO TO 999
      END = POINT - LCHA/4 + 1
C
      DO WHILE (POINT.GT.END)
        POINT = POINT - 1
        HITLEN = IAND(IQ(POINT), MASK16)
        HITADR = IAND(ISHFT(IQ(POINT), -16), MASK16)
        IF (HITLEN/4 .GT. 1) THEN
          NCLUS = NCLUS + 1
          IF (NCLUS .LE. MAX_CLUS) THEN
            HITLST(NCLUS,1,WIRE) = HITLEN
            HITLST(NCLUS,2,WIRE) = HITADR
            MAX = 0
            P = POINT
            DO INDEX = 1,HITLEN/4-1
              P = P - 1
              MAX = MAX0(MAX,IAND(IQ(P),MASK8))
              MAX = MAX0(MAX,IAND(ISHFT(IQ(P), -8),MASK8))
              MAX = MAX0(MAX,IAND(ISHFT(IQ(P),-16),MASK8))
              MAX = MAX0(MAX,IAND(ISHFT(IQ(P),-24),MASK8))
            ENDDO
            HITLST(NCLUS,3,WIRE) = MAX
          ENDIF
          POINT = POINT - (HITLEN/4-1)
        ENDIF
      ENDDO
  999 RETURN
      END
