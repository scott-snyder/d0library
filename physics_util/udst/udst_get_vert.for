      SUBROUTINE UDST_GET_VERT(LVERT,XDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill vertex info into XDATA
C-
C-   Inputs  : LVERT
C-   Outputs : XDATA
C-
C-   Created  23-DEC-1993   Ulrich Heintz
C-   Updated  10-OCT-1995   Ian Adam  Add VOBJ,WTVERT 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER KVERT,KVERT1,VERT_STATUS_WORD,ID_VERT,I,LVERT,TEMP
      PARAMETER( KVERT = 6 )
      REAL    XX(KVERT),XDATA(KVERT),OUT(2)
      EQUIVALENCE(XX,ZVERT)
      CHARACTER*8 VERT_TAGS(KVERT)
      DATA VERT_TAGS/'ZVERT','DZVERT','VSTAT_','VSTAT','VOBJ','WTVERT'/
      REAL ZVERT,DZVERT,VSTAT_,VSTAT,VOBJ,WTVERT
      COMMON/VERT_OBJECT/ ZVERT,DZVERT,VSTAT_,VSTAT,VOBJ,WTVERT
C----------------------------------------------------------------------
      ZVERT  = Q(LVERT+5)     ! z-position of vertex
      DZVERT = Q(LVERT+8)     ! error

      VERT_STATUS_WORD=IQ(LVERT+2)

      CALL SPLIT_BITMASK(VERT_STATUS_WORD,OUT)
      VSTAT_ = OUT(1)
      VSTAT  = OUT(2)
      
      TEMP = IQ(LVERT)
      CALL SPLIT_BITMASK(TEMP,OUT)
      VOBJ   = OUT(2)

      IF (IQ(LVERT+1).GE.2) THEN
        WTVERT = Q(LVERT+19)
      ELSE
        WTVERT = 0.0
      ENDIF

      DO I=1,KVERT
        XDATA(I)=XX(I)
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
C
      ENTRY UDST_VERT_TAGS(KVERT1,ID_VERT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book VERT group
C-
C----------------------------------------------------------------------
      KVERT1=KVERT
      ID_VERT=2
      CALL UDST_BOOK_GROUP(ID_VERT,'VERT',VERT_TAGS,KVERT)
C----------------------------------------------------------------------
      RETURN
      END
