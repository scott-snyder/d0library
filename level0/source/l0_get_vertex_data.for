       SUBROUTINE L0_GET_VERTEX_DATA(FASTZ_ID,VERTEX_BOARD,VERTEX_INFO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fetch the Level 0 Vertex data words from the
C-                         TRGR bank
C-
C-   Inputs  : none
C-   Outputs : FASTZ_ID
C-             VERTEX_BOARD
C-             VERTEX_INFO(board_id#1-2,bunch#1-6,vertex word#1-6,
C-                         detail info#1-8)
C-   Controls: none
C-
C-   Created   1-JUN-1992   Jeffrey Bantly
C-   Modified  20-AUG-1992  Haowei Xu   Incresed VERTEX_INFO array size
C-                                      to handle North and South boards
C-   Updated  26-JAN-1994   Jeffrey Bantly  make Run 1a and Run 1b compatible 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER NDATA
      INTEGER FIRST_BUNCH,LAST_BUNCH
      INTEGER BLOCK_WORD
      INTEGER BLOCK_TYPE
      INTEGER BLOCK_LENGTH
      INTEGER VBOARD
      INTEGER FASTZ_ID(2),VERTEX_BOARD(2)
      INTEGER DESCRIPTOR
      INTEGER VERTEX_INFO(2,6,6,8)
      INTEGER DATA_WORD(1000)
      INTEGER IBUNCH,IVERTEX
      INTEGER IDATA
      INTEGER VERTEX_WORD
      INTEGER IBITS
      INTEGER ERR
      INTEGER L0_MAX_NDATA
C
      CHARACTER*80 MESSG
C
      LOGICAL FIRST, MCDATA
      LOGICAL PRODUC, PRODFL, EZERROR
      EXTERNAL PRODUC, EZERROR
C
      SAVE FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        CALL EZPICK('LEVEL0_RCP')
        IF ( EZERROR(ERR) ) THEN
          CALL ERRMSG('LEVEL0-no-rcp','L0EXPD',
     &                                 'LEVEL0_RCP not found.','W')
        ELSE
          CALL EZGET('L0_MAX_NDATA', L0_MAX_NDATA, ERR)
          IF ( ERR.NE.0 ) L0_MAX_NDATA=2000
          CALL EZRSET
        ENDIF
        MCDATA = IQ(LHEAD+1) .GT. 1000
        PRODFL = PRODUC()
        FIRST=.FALSE.
      ENDIF
C
C  Fetch the Level 0 Crate of Data.
C
      CALL VZERO(VERTEX_INFO,576)
      VERTEX_WORD=0
      CALL VZERO(FASTZ_ID,2)
      CALL VZERO(VERTEX_BOARD,2)
C
      CALL L0EXPD(3,NDATA,DATA_WORD)
      IF (NDATA.LE.0 .OR. NDATA.GT.L0_MAX_NDATA) GOTO 997
C
C  Search for Vertex data blocks (type=2) only.
C
      IDATA = 1
   10 CONTINUE
      BLOCK_WORD = DATA_WORD(IDATA)
      BLOCK_TYPE   = IBITS(BLOCK_WORD,16,16)
      BLOCK_LENGTH = IBITS(BLOCK_WORD, 0,16)
      IF (BLOCK_TYPE.NE.3) GOTO 100
C
C  Decode Vertex descriptor word for data block.
C
      DESCRIPTOR = DATA_WORD(IDATA+1)
      VBOARD     = IBITS(DESCRIPTOR, 0,1)+1
      IF (VBOARD.LT.1.AND.VBOARD.GT.2) GOTO 995
C
      LAST_BUNCH   = IBITS(DESCRIPTOR,24,8)
      FIRST_BUNCH  = IBITS(DESCRIPTOR,16,8)
      IF ( FIRST_BUNCH.LT.1 ) GOTO 990
      IF ( LAST_BUNCH.GT.6 ) GOTO 990
      FASTZ_ID(VBOARD)     = IBITS(DESCRIPTOR, 1,1)
      VERTEX_BOARD(VBOARD) = IBITS(DESCRIPTOR, 0,1)+1    ! 1:North,2:South
C
C  Decode individual vertex data words in data block.
C
      IVERTEX = IDATA - 5
      DO 101 IBUNCH = FIRST_BUNCH, LAST_BUNCH
        IVERTEX = IVERTEX + 6
        IF (IVERTEX+6.GT.NDATA) THEN
          GOTO 998
        ENDIF
C
        VERTEX_WORD = DATA_WORD(IVERTEX+1)
        VERTEX_INFO(VBOARD,IBUNCH,1,1)=IBITS(VERTEX_WORD, 8,24)
        VERTEX_INFO(VBOARD,IBUNCH,1,2)=IBITS(VERTEX_WORD, 0, 8)
C
        VERTEX_WORD = DATA_WORD(IVERTEX+2)
        VERTEX_INFO(VBOARD,IBUNCH,2,1)=IBITS(VERTEX_WORD,24, 8)
        VERTEX_INFO(VBOARD,IBUNCH,2,2)=IBITS(VERTEX_WORD,16, 8)
        VERTEX_INFO(VBOARD,IBUNCH,2,3)=IBITS(VERTEX_WORD, 0,16)
C
        VERTEX_WORD = DATA_WORD(IVERTEX+3)
        VERTEX_INFO(VBOARD,IBUNCH,3,1)=IBITS(VERTEX_WORD, 8,24)
        VERTEX_INFO(VBOARD,IBUNCH,3,2)=IBITS(VERTEX_WORD, 0, 8)
C
        VERTEX_WORD = DATA_WORD(IVERTEX+4)
        VERTEX_INFO(VBOARD,IBUNCH,4,1)=IBITS(VERTEX_WORD,24, 8)
        VERTEX_INFO(VBOARD,IBUNCH,4,2)=IBITS(VERTEX_WORD,16, 8)
        VERTEX_INFO(VBOARD,IBUNCH,4,3)=IBITS(VERTEX_WORD, 0,16)
C
        VERTEX_WORD = DATA_WORD(IVERTEX+5)
        VERTEX_INFO(VBOARD,IBUNCH,5,1)=IBITS(VERTEX_WORD,24, 8)
        VERTEX_INFO(VBOARD,IBUNCH,5,2)=IBITS(VERTEX_WORD,16, 8)
        VERTEX_INFO(VBOARD,IBUNCH,5,3)=IBITS(VERTEX_WORD, 8, 8)
        VERTEX_INFO(VBOARD,IBUNCH,5,4)=IBITS(VERTEX_WORD, 1, 1)
        VERTEX_INFO(VBOARD,IBUNCH,5,5)=IBITS(VERTEX_WORD, 0, 1)
C
        VERTEX_WORD = DATA_WORD(IVERTEX+6)
        VERTEX_INFO(VBOARD,IBUNCH,6,1)=IBITS(VERTEX_WORD,16,16)
        VERTEX_INFO(VBOARD,IBUNCH,6,2)=IBITS(VERTEX_WORD, 8, 8)
        VERTEX_INFO(VBOARD,IBUNCH,6,3)=IBITS(VERTEX_WORD, 5, 1)
        VERTEX_INFO(VBOARD,IBUNCH,6,4)=IBITS(VERTEX_WORD, 4, 1)
        VERTEX_INFO(VBOARD,IBUNCH,6,5)=IBITS(VERTEX_WORD, 3, 1)
        VERTEX_INFO(VBOARD,IBUNCH,6,6)=IBITS(VERTEX_WORD, 2, 1)
        VERTEX_INFO(VBOARD,IBUNCH,6,7)=IBITS(VERTEX_WORD, 1, 1)
        VERTEX_INFO(VBOARD,IBUNCH,6,8)=IBITS(VERTEX_WORD, 0, 1)
C
  101 CONTINUE
C
C   Skip to next data block if any.
C
  100 CONTINUE
      IDATA = IDATA + BLOCK_LENGTH
      IF ( IDATA.LE.NDATA ) GOTO 10
C
C   Done.
C
      GOTO 999
C
C   Error handling.
C
  990 CONTINUE
      WRITE(MESSG,*) ' Vertex bunch outside normal range : ',
     &  FIRST_BUNCH,LAST_BUNCH
      IF (.NOT.PRODFL ) CALL ERRMSG('LEVEL0-bad-scaler-bunch',
     &  'L0_GET_VERTEX_DATA',MESSG,'W')
      GOTO 999
  995 CONTINUE
      WRITE(MESSG,*) ' Wrong board ID : ',VBOARD
      IF (.NOT.PRODFL ) CALL ERRMSG('LEVEL0-wrong-vtxbd',
     &  'L0_GET_VERTEX_DATA',MESSG,'W')
      GOTO 999
  997 CONTINUE
      WRITE(MESSG,*) 'Number data words exceeds maximum expected =',
     &  L0_MAX_NDATA,NDATA
      IF (.NOT.PRODFL ) CALL ERRMSG('LEVEL0-more-than-max-data',
     &  'L0_GET_VERTEX_DATA',MESSG,'W')
      GOTO 999
  998 CONTINUE
      WRITE(MESSG,*) 'Number data words exceeds maximum expected =',
     &  NDATA,IVERTEX+6
      IF (.NOT.PRODFL ) CALL ERRMSG('LEVEL0-more-than-vtx-data',
     &  'L0_GET_VERTEX_DATA',MESSG,'W')
      GOTO 999
C
C----------------------------------------------------------------------
  999 RETURN
      END
