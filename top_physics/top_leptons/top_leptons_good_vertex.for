      LOGICAL FUNCTION TOP_LEPTONS_GOOD_VERTEX()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Tests Vertex Information for current event
C-                         against some use-defined cuts. If the event
C-                         passes this selection then the routine returns
C-                         as .TRUE..
C-
C-   Inputs  : 
C-              None
C-   Outputs : 
C-              GOOD_VERTEX = .TRUE./.FALSE. FOR OK/Reject Event
C-
C-   Controls: 
C-              None
C-
C-   Created   1-NOV-1992   Stephen J. Wimpenny
C-   Modified 22-Mar-1993   Routine name changed for library compatibility
C-                          (was Good_Vertex).
C-                          Routine name change Get_Vertex_Data
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL DO_VERTEX_CUTS,FIRST,IOK
      REAL MIN_VERTEX_Z,MAX_VERTEX_Z,VERTX_P(3,5)
      INTEGER IV,IER,MAX_NO_VERTEX_P
      INTEGER NO_ZTRAK,NOVERT_P,NOVERT_S,NO_ZTRAK_VERTX_P(5)
C
      DATA FIRST/.TRUE./
C
      IF(FIRST) THEN
C
C *** Read Vertex cut information from RCP file
C
        CALL EZPICK('TOP_LEPTONS_RCP')
        CALL EZGET('DO_VERTEX_CUTS',DO_VERTEX_CUTS,IER)
        IF(DO_VERTEX_CUTS) THEN
          IF(IER.EQ.0) CALL EZGET('VERTEX_MAX_NO_P',MAX_NO_VERTEX_P,IER)
          IF(IER.EQ.0) CALL EZGET('VERTEX_ZMIN',MIN_VERTEX_Z,IER)
          IF(IER.EQ.0) CALL EZGET('VERTEX_ZMAX',MAX_VERTEX_Z,IER)
        ENDIF
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error in TOP_LEPTONS_RCP',
     &    'TOP_LEPTONS_GOOD_VERTEX',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      TOP_LEPTONS_GOOD_VERTEX=.TRUE.
      IF(.NOT.DO_VERTEX_CUTS) GO TO 999
C
C *** Cuts wanted so look at what we have got
C
      CALL TOP_LEPTONS_UTIL_DECODE_VERTEX(NOVERT_P,NOVERT_S,NO_ZTRAK,
     1  NO_ZTRAK_VERTX_P,VERTX_P,IER)
C
C *** Cut on primary vertex multiplicity
C
      IF(NOVERT_P.GT.MAX_NO_VERTEX_P) THEN
        TOP_LEPTONS_GOOD_VERTEX=.FALSE.
        GO TO 999
      ENDIF
C
C *** Cut on Vertex z-position
C
      IF(MIN_VERTEX_Z.GT.MAX_VERTEX_Z) GO TO 999
      IOK=.FALSE.
      DO IV=1,NOVERT_P
        IF(VERTX_P(3,IV).LE.MAX_VERTEX_Z
     1    .AND.VERTX_P(3,IV).GE.MIN_VERTEX_Z) THEN
          IOK=.TRUE.
        ENDIF
      ENDDO
      IF(.NOT.IOK) THEN
        TOP_LEPTONS_GOOD_VERTEX=.FALSE.
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
