      SUBROUTINE GM_OUT(BUF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods: This COMIS routine fills ntuple 2 using the
C-   events listed in vector LIST.
C-
C-   Inputs  : BUF  [R]   Buffer containing list of events
C-   Outputs :
C-   Controls:
C-
C-   Created  10-NOV-1992   Harrison B. Prosper
C-
C----------------------------------------------------------------------
C      IMPLICIT NONE
      REAL    BUF
      INTEGER IBUF
C----------------------------------------------------------------------
      VECTOR LIST
      VECTOR NLIST
      VECTOR VALUE
C----------------------------------------------------------------------
      INTEGER JRUN, JEVENT, II,JJ, NN, IDFIRST
      LOGICAL MORE_EVENTS
      REAL    WEIGHT
      REAL X(30)
C----------------------------------------------------------------------
C-----------------------------------------------------------------------
C Name:     GMPAW.INC
C Created:  1-Dec-1992    Harrison B. Prosper
C-----------------------------------------------------------------------
      INTEGER IDNEVT
      REAL    VIDN1,VIDN2,VIDN3
      REAL
     +RUN     ,EVENT   ,TRIGGER ,OBJECT  ,COUNT   ,NUMBER  ,
     +PX      ,PY      ,PZ      ,E       ,ET      ,ETA     ,
     +PHI     ,DETA    ,QUALITY ,X1      ,X2      ,X3      ,
     +X4      ,X5      ,X6      ,X7      ,X8      ,X9      ,
     +X10     ,X11     ,X12     ,X13     ,X14     ,X15
C-----------------------------------------------------------------------
      COMMON/PAWIDN/IDNEVT,VIDN1,VIDN2,VIDN3,
     +RUN     ,EVENT   ,TRIGGER ,OBJECT  ,COUNT   ,NUMBER  ,
     +PX      ,PY      ,PZ      ,E       ,ET      ,ETA     ,
     +PHI     ,DETA    ,QUALITY ,X1      ,X2      ,X3      ,
     +X4      ,X5      ,X6      ,X7      ,X8      ,X9      ,
     +X10     ,X11     ,X12     ,X13     ,X14     ,X15
C-----------------------------------------------------------------------
      INTEGER IRUN, IEVENT, IOBJECT, INUMBER, IQUALITY, ITRIGGER, ICOUNT
      INTEGER ID_VERT, ID_PHOT, ID_ELEC
      INTEGER ID_MUON, ID_TAU,  ID_JET, ID_NU, ID_END
      INTEGER IPX, IPY, IPZ, IE, IET, IETA, IPHI, IDETA, IQUAL,IBASE,NUM
C--------------------------------------------------------------------------
      EQUIVALENCE(QUALITY,IQUALITY)
      EQUIVALENCE(TRIGGER,ITRIGGER)
C--------------------------------------------------------------------------
      PARAMETER ( IPX  = 1 )
      PARAMETER ( IPY  = 2 )
      PARAMETER ( IPZ  = 3 )
      PARAMETER ( IE   = 4 )
      PARAMETER ( IET  = 5 )
      PARAMETER ( IETA = 6 )
      PARAMETER ( IPHI = 7 )
      PARAMETER ( IDETA= 8 )
      PARAMETER ( IQUAL= 9 )
      PARAMETER ( IBASE= 9 )
      PARAMETER ( NUM  =25 )
C--------------------------------------------------------------------------
      PARAMETER ( ID_END   =-1 )
      PARAMETER ( ID_VERT  = 0 )
      PARAMETER ( ID_PHOT  = 1 )
      PARAMETER ( ID_ELEC  = 2 )
      PARAMETER ( ID_MUON  = 3 )
      PARAMETER ( ID_TAU   = 4 )
      PARAMETER ( ID_JET   = 5 )
      PARAMETER ( ID_NU    = 6 )
C-----------------------------------------------------------------------
      CHARACTER*8 CHTAGS( 30)
      DATA CHTAGS /
     +'RUN     ','EVENT   ','TRIGGER ','OBJECT  ','COUNT   ','NUMBER  ',
     +'PX      ','PY      ','PZ      ','E       ','ET      ','ETA     ',
     +'PHI     ','DETA    ','QUALITY ','X1      ','X2      ','X3      ',
     +'X4      ','X5      ','X6      ','X7      ','X8      ','X9      ',
     +'X10     ','X11     ','X12     ','X13     ','X14     ','X15     '/
C-----------------------------------------------------------------------
C----------------------------------------------------------------------
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      WEIGHT = 0.0
C
      IF ( FIRST ) THEN
        FIRST   = .FALSE.
        IDFIRST = IDNEVT
C
C ****  Decide which buffer to use
C
        IBUF = IFIX(BUF)
        IF     ( IBUF .LE. 0 ) THEN
          JJ = 0
        ELSEIF ( IBUF .GT. 3 ) THEN
          JJ = 0
        ELSE
          JJ = IBUF
        ENDIF
        IF ( JJ .LE. 0 ) THEN
          JJ = 1
        ENDIF
C
        NEVENT= NLIST(JJ)
C
        PRINT*,' GM_OUT: Book ntuple'
C
        CALL HBOOKN(2,'Selected Events',30,'LUN2',30000,CHTAGS)
C
        PRINT*,' GM_OUT: Number of events to write ', NEVENT
C
      ENDIF
C
      IF ( IDNEVT .EQ. IDFIRST ) THEN
        PRINT*,' GM_OUT: Use events listed in buffer ',JJ
C
        II    = 1
        JRUN  = LIST(1,II,JJ)
        JEVENT= LIST(2,II,JJ)
        MORE_EVENTS = NEVENT .GT. 0
      ENDIF
C
      IF ( MORE_EVENTS ) THEN
C
C ****  Set weight to 1 if current event is in the vector LIST
C
        IRUN   = IFIX(RUN)
        IF ( IRUN .EQ. JRUN ) THEN
          IEVENT = IFIX(EVENT)
          IF ( IEVENT .EQ. JEVENT ) THEN
            WEIGHT = 1.0
          ENDIF
        ENDIF
C
C ****  Check if this is a selected event
C
        IF ( WEIGHT .GT. 0.5 ) THEN
C
          IOBJECT = IFIX(OBJECT)
C
          IF ( IOBJECT .EQ. ID_END ) THEN
C
C ****  Load content of VALUE into field number
C ****  then fill ntuple 2
C
            NUMBER = VALUE(II,JJ)
            CALL UCOPY(RUN,X(1),30)
            CALL HFN(2,X)
C
            MORE_EVENTS = II .LT. NEVENT
            IF ( MORE_EVENTS ) THEN
              II    = II + 1
              JRUN  = LIST(1,II,JJ)
              JEVENT= LIST(2,II,JJ)
            ELSE
C
C ****  WRITE OUT LAST BUFFER
C
              ICYCLE = 0
              CALL HCDIR('//LUN2',' ')
              CALL HROUT(2,ICYCLE,' ')
              PRINT*,' GM_OUT: Done! Events written ', II
              WEIGHT =-1.0
            ENDIF
C
          ELSE
            CALL UCOPY(RUN,X(1),30)
            CALL HFN(2,X)
          ENDIF
        ENDIF
      ENDIF
C
      GM_OUT = WEIGHT
      END
