      SUBROUTINE L2_L0_VERTEX( FZPOS, FGOOD, SZPOS, SGOOD, MI_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack L0 vertex for use in L2
C-      override default constants with RCP by calling _PARAMETERS entry
C-
C-   Inputs  : VERTEX via ZEBCOM
C-   Outputs : FZPOS [F] = Z position of FAST Z vertex in cm.,
C-                  = nominal vertex if FGOOD=F
C-             FGOOD [L] = TRUE if good FAST Z vertex bit set.
C-             SZPOS [F] = Z position of Slow Z vertex in cm.,
C-                  = nominal if SGOOD=F
C-             SGOOD [L] = TRUE if good SLOW Z vertex bit set.
C-             MI_FLAG [I] = Multiple Interaction Flag 1-4 (see L0VT.ZEB)
C-                           0 = No slow Z info
C-   Controls: none
C-
C-   For details see D0 Note 967
C-   FAST_Z Vertex is a five bit two's complement integer
C-   The next significant bit is a good bit
C-   The last two bits in byte 525 are unused
C-
C-   SLOW_Z Vertex is a nine bit two's complement integer
C-
C-  Warning: decoding is vax-specific (for level 2 use)
C-
C-   Modified   7-JUL-1992   Jeffrey Bantly   from S.L.Linn routine
C-                                            L2_L0_VERTEX.FOR
C-   Updated  14-OCT-1992   James T. Linnemann  add L2_L0_PARAMETERS entry
C-   Updated  13-JAN-1993   Jeffrey Bantly   Mult Int Flag Bits changed
C-   Updated  24-MAR-1993   Jeffrey Bantly   Add ERRMSG calls
C-   Updated  16-MAR-1994   Jeffrey Bantly   Fix minbias bug.
C-   Updated  26-JUL-1994   Jeffrey Bantly   Fix |slowz|>100. bug and
C-                                              modify/add data checks
C-
C#######################################################################
C      ENTRY L2_L0_PARAMETERS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get vertex constants out of RCP file
C-        should be called at beginning of each run.
C-
C-   Inputs  : L2_L0.RCP
C-   Outputs : constants for this routine
C-   Controls: none
C-
C-   Created  14-OCT-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:L1C_EVENT_RAW.INC'
C
      REAL FZPOS, SZPOS
      LOGICAL FGOOD, SGOOD, SINTER
      INTEGER MI_FLAG
      INTEGER ZBIN, SZBIN, VERTEX, SVTX, SVERTEX, CBUNCH, IB, IER
      INTEGER GZTRGR, GZFIND_CRATE, LTRGR,LCRATE11,LCRATE01
      INTEGER FOFFSET, SOFFSET, SFOFFSET
      INTEGER FOFFSETA, SOFFSETA, SFOFFSETA
      INTEGER FOFFSETB, SOFFSETB, SFOFFSETB
      INTEGER HALF_BYTE, ONE_BYTE
      PARAMETER ( FOFFSETA=269, SOFFSETA=750, SFOFFSETA=352 )
      PARAMETER ( FOFFSETB=269, SOFFSETB=806, SFOFFSETB=352 )
      PARAMETER ( HALF_BYTE=15 , ONE_BYTE = 255 )
      INTEGER L0_SLOW_INTER,L0_SLOW_Z_GOOD,L0_SLOW_CENTER
      INTEGER L0_MI_FLAG_0,L0_MI_FLAG_1,L0_MI_FLAG_2,L0_MI_FLAG_3
      INTEGER L0_FAST_Z_GOOD,L0_CENTER_1,L0_FAST_Z_CENTER
      INTEGER ERRVAL, TOT_FLAG, HEAD_LEN, VERSION, NBLOCKS
      INTEGER L0_CBUNCH,BUNCH_WORD
      INTEGER MI_FLAG0,MI_FLAG1,MI_FLAG2,MI_FLAG3,SLOW_Z_GOOD
      INTEGER SLOW_Z_CENTER,INTERACTION
      INTEGER ANDOR1,ANDOR2
C
      REAL FAST_Z_MULTIPLIER, FAST_Z_OFFSET, FAST_Z_NOMINAL
      REAL SLOW_Z_MULTIPLIER, SLOW_Z_OFFSET, SLOW_Z_NOMINAL
C
      LOGICAL NEG_Z,BUNCH_BIT(32)
      LOGICAL OK, EZERROR, MCDATA
      LOGICAL FIRST
C
      CHARACTER*40 MSG1(12),MSG2(12)
C
      SAVE FAST_Z_MULTIPLIER, FAST_Z_OFFSET, FAST_Z_NOMINAL
      SAVE SLOW_Z_MULTIPLIER, SLOW_Z_OFFSET, SLOW_Z_NOMINAL
      SAVE MSG1,MSG2
      SAVE MCDATA,FIRST
C
      DATA FAST_Z_MULTIPLIER/6.25/,FAST_Z_OFFSET/0.00/
      DATA SLOW_Z_MULTIPLIER/0.75/,SLOW_Z_OFFSET/0.00/
      DATA FAST_Z_NOMINAL/-8.90/,SLOW_Z_NOMINAL/-8.90/
      DATA MSG1/'L0 Slow Z Good ANDOR bad','L0 MI Flag ANDOR bad',
     &         'L0 MI Flag ANDORs all bad',
     &         'L0 Slow Z Center ANDOR bad',
     &         'L0-L1 Bad Correct Bunch',
     &         'L0 Crate 01 Missing from data',
     &         'L0 Bad Crate Header Length','L0-L1 Correct Bunch bad',
     &         'L0 ANDORs do not match','L0-L1 Interaction bit bad',
     &         'L0 Interaction=0 bad bit',
     &         'L0 Bad ADC Data Length'/
      DATA MSG2/'L0 Slow Z Good ANDOR bit bad',
     &  'L0 MI Flag ANDOR bit bad',
     &  'L0 MI Flag ANDOR bits all bad',
     &  'L0 Slow Z Center ANDOR bit bad',
     &  'L0-L1 Bad Correct Bunch in L1 Crate',
     &  'L0 Crate 01 Missing from TRGR data',
     &  'L0 Bad Crate Header Length','L0-L1 Correct Bunch do not match',
     &  'L0 ANDOR1-ANDOR2 do not match',
     &  'L0-L1 Interaction bits do not match',
     &  'L0 bit bad when Interaction bit=0',
     &  'L0 Bad ADC Data Length'/
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C...determine data type and set offset parameters
      IF ( FIRST ) THEN
        FIRST=.FALSE.
C     Preset offset parameters
        FOFFSET = FOFFSETB
        SOFFSET = SOFFSETB
        SFOFFSET = SFOFFSETB
        LTRGR = GZTRGR()
        IF (LTRGR .LE. 0) GOTO 999
        LCRATE01 = GZFIND_CRATE('TRGR',LTRGR,1)
        IF (LCRATE01 .LE. 0) THEN
          ERRVAL=6
          CALL ERRMSG(MSG1(ERRVAL),'L2_L0_VERTEX',MSG2(ERRVAL),'W')
          GOTO 999
        ENDIF
        NBLOCKS = IQ(LCRATE01+2)
        NBLOCKS = IBITS(NBLOCKS,16,8)
        VERSION = IQ(LCRATE01+3)
        VERSION = IBITS(VERSION,0,16)
        IF ( VERSION.EQ.2 ) THEN
          IF ( NBLOCKS.EQ.25 ) SOFFSET = SOFFSETA
          IF ( NBLOCKS.EQ.29 ) SOFFSET = SOFFSETB
        ELSEIF ( VERSION.EQ.1 ) THEN
          SOFFSET = SOFFSETA
        ELSE
          ERRVAL=12
          CALL ERRMSG(MSG1(ERRVAL),'L2_L0_VERTEX',MSG2(ERRVAL),'W')
        ENDIF
      ENDIF
C...default values in case of missing data
      FGOOD = .FALSE.
      FZPOS = FAST_Z_NOMINAL
      SGOOD = .FALSE.
      SZPOS = SLOW_Z_NOMINAL
      MI_FLAG = 0
C
C...default values in case of MC data
      MCDATA = IQ(LHEAD+1) .GT. 1000
      IF ( MCDATA ) THEN
        CALL L1_EXTRACT_ISAJET()
        FGOOD = .TRUE.
        FZPOS = Z_VERTEX
        SGOOD = .TRUE.
        SZPOS = Z_VERTEX
        MI_FLAG = 1
        GOTO 999
      ENDIF
C
C  Fetch Zebra location in TRGR bank of Level 1 data crate, crate 11.
C
      LTRGR = GZTRGR()
      IF (LTRGR .LE. 0) GOTO 999
      LCRATE11 = GZFIND_CRATE( 'TRGR', LTRGR, 11 )
      IF (LCRATE11 .LE. 0) GOTO 999
C
C  Fetch FAST_Z vertex word from Crate 11
C
      VERTEX = IQ( LCRATE11 + FOFFSET )
      FGOOD = BTEST( VERTEX, 5 )
C
C  Fetch Level 1 ANDOR bits for L0 Slow Z and MI flag.
C
      SVERTEX = IQ( LCRATE11 + 195 )
      L0_FAST_Z_GOOD = IBITS(SVERTEX,0,1)
      L0_CENTER_1 = IBITS(SVERTEX,1,1)
      L0_FAST_Z_CENTER = IBITS(SVERTEX,6,1)
      L0_SLOW_INTER = IBITS(SVERTEX,7,1)
      L0_SLOW_Z_GOOD = IBITS(SVERTEX,16,1)
      L0_MI_FLAG_0 = IBITS(SVERTEX,17,1)
      L0_MI_FLAG_1 = IBITS(SVERTEX,18,1)
      L0_MI_FLAG_2 = IBITS(SVERTEX,19,1)
      L0_MI_FLAG_3 = IBITS(SVERTEX,20,1)
      L0_SLOW_CENTER = IBITS(SVERTEX,21,1)
C
C  Fetch both ANDOR terms related to Level 0 and make sure they match.
C
      ANDOR1 = IAND(IQ(LCRATE11 + 195),127)
      ANDOR2 = IAND(IQ(LCRATE11 + 211),127)
      IF ( ANDOR1.NE.ANDOR2 ) THEN
        ERRVAL=9
        CALL ERRMSG(MSG1(ERRVAL),'L2_L0_VERTEX',MSG2(ERRVAL),'W')
      ENDIF
C
C  Fetch the correct bunch (first one on wins) and send error msg if necessary
C
      CBUNCH = -1
      DO IB=5,0,-1
        BUNCH_BIT(IB+1) = BTEST(IQ(LCRATE11+195+2),IB)
        IF ( BUNCH_BIT(IB+1)) CBUNCH=IB+1
      ENDDO
      IF ( CBUNCH.LT.1 .OR. CBUNCH.GT.6 ) THEN
        ERRVAL=5
        CALL ERRMSG(MSG1(ERRVAL),'L2_L0_VERTEX',MSG2(ERRVAL),'W')
        GO TO 100
      ENDIF
C
C  Fetch Zebra location in TRGR bank of Level 0 data crate, crate 01.  Check
C  for errors demanding notification.
C
      LCRATE01 = GZFIND_CRATE('TRGR',LTRGR,1)
      IF (LCRATE01 .LE. 0) THEN
        ERRVAL=6
        CALL ERRMSG(MSG1(ERRVAL),'L2_L0_VERTEX',MSG2(ERRVAL),'W')
        GOTO 999
      ENDIF
      HEAD_LEN = IQ(LCRATE01)
      IF ( HEAD_LEN.NE.4 ) THEN
        ERRVAL=7
        CALL ERRMSG(MSG1(ERRVAL),'L2_L0_VERTEX',MSG2(ERRVAL),'W')
      ENDIF
C
C  Fetch the Level 0 Correct Bunch.
C
      BUNCH_WORD=IQ(LCRATE01-1+5)
      L0_CBUNCH=IBITS(BUNCH_WORD,0,8)+1
      IF(L0_CBUNCH.NE.CBUNCH) THEN
        ERRVAL=8
        CALL ERRMSG(MSG1(ERRVAL),'L2_L0_VERTEX',MSG2(ERRVAL),'W')
      ENDIF
C
C  Fetch the Level 0 Slow Z vertex position and Multiple Interaction Flag.
C
      SVTX = IQ(LCRATE01+SOFFSET+(L0_CBUNCH*6))
      SINTER = BTEST(SVTX,5)
      SGOOD = BTEST(SVTX,4)
      INTERACTION=0
      SLOW_Z_GOOD=0
      MI_FLAG0=0
      MI_FLAG1=0
      MI_FLAG2=0
      MI_FLAG3=0
      IF ( SINTER ) INTERACTION=1
      IF ( SGOOD ) SLOW_Z_GOOD=1
      IF ( BTEST(SVTX,0) ) MI_FLAG0 = 1
      IF ( BTEST(SVTX,1) ) MI_FLAG1 = 1
      IF ( BTEST(SVTX,2) ) MI_FLAG2 = 1
      IF ( BTEST(SVTX,3) ) MI_FLAG3 = 1
      IF ( SINTER ) THEN
        SVERTEX = IBITS(SVTX,16,16)
        NEG_Z = BTEST(SVERTEX,8)
        IF ( NEG_Z ) THEN     !undo twos-complement
          SVERTEX = SVERTEX - 1
          SVERTEX = NOT(SVERTEX)
          SZBIN =  - IAND( SVERTEX, ONE_BYTE)
        ELSE
          SZBIN = IAND(SVERTEX,ONE_BYTE)
        ENDIF
        SZPOS = SLOW_Z_MULTIPLIER*FLOAT(SZBIN) + SLOW_Z_OFFSET
C
C...worst bit on wins for multiples, best bit for singles
        IF ( BTEST(SVTX,2) ) MI_FLAG = 3
        IF ( BTEST(SVTX,3) ) MI_FLAG = 4
        IF ( BTEST(SVTX,1) ) MI_FLAG = 2
        IF ( BTEST(SVTX,0) ) MI_FLAG = 1
C
      ENDIF
      SLOW_Z_CENTER=0
      IF ( ABS(SZPOS).LE.10.5 ) SLOW_Z_CENTER=1
      IF ( INTERACTION.NE.L0_SLOW_INTER ) THEN
        ERRVAL=10
        CALL ERRMSG(MSG1(ERRVAL),'L2_L0_VERTEX',MSG2(ERRVAL),'W')
      ENDIF
      IF ( INTERACTION.NE.0 ) THEN
        IF ( SLOW_Z_GOOD.NE.L0_SLOW_Z_GOOD ) THEN
          ERRVAL=1
          CALL ERRMSG(MSG1(ERRVAL),'L2_L0_VERTEX',MSG2(ERRVAL),'W')
        ENDIF
        IF ( MI_FLAG0.NE.L0_MI_FLAG_0 ) THEN
          ERRVAL=2
          CALL ERRMSG(MSG1(ERRVAL),'L2_L0_VERTEX',MSG2(ERRVAL),'W')
        ENDIF
        IF ( MI_FLAG1.NE.L0_MI_FLAG_1 ) THEN
          ERRVAL=2
          CALL ERRMSG(MSG1(ERRVAL),'L2_L0_VERTEX',MSG2(ERRVAL),'W')
        ENDIF
        IF ( MI_FLAG2.NE.L0_MI_FLAG_2 ) THEN
          ERRVAL=2
          CALL ERRMSG(MSG1(ERRVAL),'L2_L0_VERTEX',MSG2(ERRVAL),'W')
        ENDIF
        IF ( MI_FLAG3.NE.L0_MI_FLAG_3 ) THEN
          ERRVAL=2
          CALL ERRMSG(MSG1(ERRVAL),'L2_L0_VERTEX',MSG2(ERRVAL),'W')
        ENDIF
        TOT_FLAG = L0_MI_FLAG_0+L0_MI_FLAG_1+L0_MI_FLAG_2+L0_MI_FLAG_3
        IF ( TOT_FLAG.GT.2 ) THEN
          ERRVAL=3
          CALL ERRMSG(MSG1(ERRVAL),'L2_L0_VERTEX',MSG2(ERRVAL),'W')
        ENDIF
        IF ( L0_SLOW_CENTER.NE.SLOW_Z_CENTER ) THEN
          ERRVAL=4
          CALL ERRMSG(MSG1(ERRVAL),'L2_L0_VERTEX',MSG2(ERRVAL),'W')
        ENDIF
      ELSE
        ERRVAL=0
        IF ( L0_SLOW_Z_GOOD.NE.0 ) ERRVAL=11
        IF ( L0_MI_FLAG_0.NE.0 ) ERRVAL=11
        IF ( L0_SLOW_CENTER.NE.0 ) ERRVAL=11
        IF ( ERRVAL.EQ.11 )
     &    CALL ERRMSG(MSG1(ERRVAL),'L2_L0_VERTEX',MSG2(ERRVAL),'W')
      ENDIF
      IF ( IQ(LCRATE01).NE.4 ) THEN
        ERRVAL=5
        CALL ERRMSG(MSG1(ERRVAL),'L2_L0_VERTEX',MSG2(ERRVAL),'W')
      ENDIF
C
C...now unpack Fast Z VERTEX word and turn into a vertex position
  100 CONTINUE
      IF ( FGOOD ) THEN
        NEG_Z = BTEST( VERTEX, 4)
        IF( NEG_Z )THEN             !undo two's complement
          VERTEX = VERTEX - 1
          VERTEX = NOT(VERTEX)
          ZBIN =  - IAND( VERTEX, HALF_BYTE)
        ELSE
          ZBIN =  IAND( VERTEX, HALF_BYTE )
        ENDIF
        FZPOS = FAST_Z_MULTIPLIER*FLOAT(ZBIN) + FAST_Z_OFFSET
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
C#######################################################################
      ENTRY L2_L0_PARAMETERS
C----------------------------------------------------------------------
      CALL EZPICK('L2_L0_RCP')
      OK = .NOT.EZERROR(IER)
      IF ( OK ) THEN
        CALL EZGET('FAST_Z_MULTIPLIER',FAST_Z_MULTIPLIER,IER)
        CALL EZGET('FAST_Z_OFFSET',FAST_Z_OFFSET,IER)
        CALL EZGET('FAST_Z_NOMINAL',FAST_Z_NOMINAL,IER)
        CALL EZGET('SLOW_Z_MULTIPLIER',SLOW_Z_MULTIPLIER,IER)
        CALL EZGET('SLOW_Z_OFFSET',SLOW_Z_OFFSET,IER)
        CALL EZGET('SLOW_Z_NOMINAL',SLOW_Z_NOMINAL,IER)
        CALL EZRSET
      ENDIF
      IF (.NOT.OK) THEN
        CALL ERRMSG('L2_L0_VERTEX_PARAMS','L2_L0_VERTEX',
     &      'Couldn''t find bank','W')
      ELSEIF (IER .NE. 0) THEN      ! Error reading RCP
        CALL ERRMSG('L2_L0_VERTEX_PARAMS','L2_L0_VERTEX',
     &          'Couldn''t find parameter: used default','W')
      ENDIF
      RETURN
      END
