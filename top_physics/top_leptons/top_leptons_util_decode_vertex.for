      SUBROUTINE TOP_LEPTONS_UTIL_DECODE_VERTEX(NOVERT_P,NOVERT_S,
     1  NO_ZTRAK,NO_ZTRAK_VERTX_P,VERTX_P,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get Primary and Secondary Vertex Information
C-                          for current event. 
C-
C-   Inputs  : None
C-          
C-   Outputs : 
C-             NOVERT_P              No. of Primary Vertices
C-             NOVERT_S              No. of Secondary Vertices
C-             NO_ZRTRAK             Total Ztraks (CDC+FDC)
C-             NO_ZTRAK_VERTX_P(I)   Ztrak Multiplicity for vertx I
C-             VERTX_P(J,I)          x,y,z position of primary vertex I
C-             IER                   Error  flag = -1/1 for bad/OK
C-
C-   Controls: None
C-
C-   Created   9-OCT-1992   Stephen J. Wimpenny
C-   Modified 22_Mar-1993   Routine name changed for library compatibility
C-                          ( was Get_Vertx_Data).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      INTEGER IER,ITEMP,JTEMP,I,J,JBIT
      INTEGER LVERH,GZVERH,LVERT
      INTEGER LDTRH,GZDTRH,LFTRH,GZFTRH
      INTEGER NO_ZTRAK,NOVERT_P,NOVERT_S,NO_ZTRAK_VERTX_P(5)
      REAL TEMP,VERTX_P(3,5)
C
      IER=-1
      NO_ZTRAK=0
      NOVERT_P=0
      NOVERT_S=0
      CALL VZERO(VERTX_P,15)
      CALL VZERO(NO_ZTRAK_VERTX_P,5)
C
C *** Total Ztrak count
C
      LDTRH=GZDTRH()
      IF(LDTRH.NE.0) THEN
        NO_ZTRAK=NO_ZTRAK+IQ(LDTRH+2)
      ENDIF
      LFTRH=GZFTRH()
      IF(LFTRH.NE.0) THEN
        NO_ZTRAK=NO_ZTRAK+IQ(LFTRH+2)
      ENDIF
C
C *** Vertex Information
C
      ITEMP=0
      LVERH=GZVERH()
      IF(LVERH.NE.0) THEN
        NOVERT_P=IQ(LVERH+2)
        NOVERT_S=IQ(LVERH+3)
        IER=1
        LVERT=LQ(LVERH-1)
        DO WHILE (LVERT.GT.0)
          IF(JBIT(IQ(LVERT+2),31).EQ.1.OR.JBIT(IQ(LVERT+2),32).EQ.1)
     &      THEN
            ITEMP=ITEMP+1
            IF(ITEMP.LT.6) THEN
              VERTX_P(1,ITEMP)=Q(LVERT+3)
              VERTX_P(2,ITEMP)=Q(LVERT+4)
              VERTX_P(3,ITEMP)=Q(LVERT+5)
              JTEMP=0
              DO I=1,7
                J=I-1
                IF(JBIT(IQ(LVERT+2),I).EQ.1) JTEMP=JTEMP+2**J
              ENDDO
              TEMP=FLOAT(NO_ZTRAK*JTEMP)/100.
              NO_ZTRAK_VERTX_P(ITEMP)=TEMP
              ENDIF
            ENDIF
          LVERT=LQ(LVERT)
        ENDDO
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
