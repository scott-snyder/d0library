      SUBROUTINE PACK_RECO_BITS(NTOT,ID_OBJ,ET,BITON)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      fill RECO bit words in HEAD bank
C-   Inputs  : 
C-       NTOT         = total number of objects
C-       ID_OBJ(NTOT) = object ids (see ESUM.PARAMS)
C-       ET (NTOT)    = Et of object (negative for mu-)
C-       BITON(NTOT)  = if true object has level 2 counterpart
C-
C-  ENTRY UNPACK_RECO_BITS(NOBJ,ID_OBJ,ET,BITON)
C-      reverse of PACK_RECO_BITS (gives NOBJ instead of NTOT)
C-      NOBJ(I)= number of objects of type I
C-               same types are given consecutively
C-
C-   Created  27-MAR-1992   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NOBJ(*),ID_OBJ(*),NTOT
      REAL    ET(*)
      LOGICAL BITON(*)
      BYTE BYTES(4)
      INTEGER PACK,BYTE_VALUE,BYTE_LOC(4)
      EQUIVALENCE (PACK,BYTES(1))
      INTEGER IWORDS(10),PREV_ID
      INTEGER N,I,K,BITSET,BITNUM,BYTL,BYTH,ID,IW
      REAL    SCALES(10)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C              first setup arrays from parameters
      IF(FIRST) THEN
        CALL INI_RECO_BITS(SCALES,IWORDS,BYTE_LOC)
        FIRST=.FALSE.
      ENDIF
C
C         calculate value and location
      PREV_ID=-1
      DO N=1,NTOT
        ID=ID_OBJ(N)
        IF(ID.NE.PREV_ID) THEN
          PREV_ID=ID
          K=0
        ENDIF
        K=K+1
        IF(ID.GT.0.AND.ID.LT.11.AND.K.LT.5) THEN
          IW=IWORDS(ID)
          BYTE_VALUE=ET(N)/SCALES(ID)
          IF(BYTE_VALUE.GT.127) BYTE_VALUE=127
          IF(BYTE_VALUE.LT.-127) BYTE_VALUE=-127
          IF ( IW.LT.10 ) THEN
            BYTL=K
            IF(BYTL.EQ.1) PACK=0
          ELSE
            BYTL=4                 !  treat ETSUM differently
            IW=9
          ENDIF
          BYTES(BYTE_LOC(BYTL))=BYTE_VALUE
          IF(BITON(N).AND.IW.LT.9) THEN
            BITNUM=(IW-1)*4+K-1
            BITSET=IBSET(BITSET,BITNUM)
          ENDIF
        ENDIF
        IQ(LHEAD+19+IW)=PACK
      ENDDO
      IQ(LHEAD+19)=BITSET
      GOTO 999
C
C
      ENTRY UNPACK_RECO_BITS(NOBJ,ID_OBJ,ET,BITON)
C              first setup arrays from parameters
      IF(FIRST) THEN
        CALL INI_RECO_BITS(SCALES,IWORDS,BYTE_LOC)
        FIRST=.FALSE.
      ENDIF
C
C         extract values
      N=0
      DO I=1,10
        NOBJ(I)=0
        IW=IWORDS(I)
        IF(IW.GT.9) IW=9
        PACK=IQ(LHEAD+19+IW)
        IF(PACK.NE.0) THEN
          BYTL=1
          BYTH=4
          IF(IWORDS(I).EQ.9) BYTH=3
          IF(IWORDS(I).GT.9) BYTL=4      ! ETSUM is in 4th byte
          DO K=BYTL,BYTH
            BYTE_VALUE=BYTES(BYTE_LOC(K))
            IF(BYTE_VALUE.NE.0) THEN
              N=N+1
              ID_OBJ(N)=I
              NOBJ(I)=NOBJ(I)+1
              ET(N)=BYTE_VALUE*SCALES(I)
            ENDIF
            BITNUM=(IW-1)*4+K-1
            IF(BITNUM.LT.32) BITON(N)=BTEST(BITSET,BITNUM)
          ENDDO
        ENDIF
      ENDDO
  999 RETURN
      END
