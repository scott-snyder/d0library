      LOGICAL FUNCTION VERTEX() 
C------------------------------------------------------------------
C 
C-   Purpose and Methods : Main routine for finding primary vertices.
C  
C  Qizhong Li, Daria Zieminska 5-April-1990
C-   Updated  19-SEP-1990   Qizhong Li-Demarteau  try FDC before VTX 
C-   Updated  31-JAN-1991   Qizhong Li-Demarteau  added a choice for
C-                                       more accurate vertex finding 
C-   Updated   2-MAR-1991   Daria Zieminska  modifications in preparation
C                           for finding precise vertex using VTX tracks 
C-   Updated  22-MAR-1991   Qizhong Li-Demarteau  added switches for
C-                                subdetectors and try FDC after others
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZERROR
C-   Updated  17-OCT-1991   Qizhong Li-Demarteau  added multi-vertex finding
C-   Updated  22-MAY-1992   Qizhong Li-Demarteau  added a choice to use
C-                                 VERTEX_FDC_TRK (FDC tracks) first
C-   Updated  18-SEP-1992   Alexandre Zinchenko call XYVERT to find X,Y 
C-                                              of vertex
C-   Updated  24-NOV-1993   Qizhong Li-Demarteau   added VERTEX_TYPE to
C-                                 allow choices to build VERT from
C-                                 data, ISAJET, norminal position or LV0
C-   Updated   1-FEB-1995   Qizhong Li-Demarteau  remove call to VERTEX_VTX
C-                                                (requested by VTX group)
C-   Updated  13-FEB-1995   Liang-ping Chen Comment out VERTEX_VTX(1),
C-                          to avoid RECO crash with STA with no CD data.
C-                          Remove call to VERTEX_XYRCP_RESTORE
C-                          Replace with XYRCP 
C-   Updated  12-APR-1995   Norman A. Graf  Put check on MCDATA before
C-                          calling XYRCP or XYVERT
C------------------------------------------------------------------
      IMPLICIT NONE  
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IER
      INTEGER VERTEX_TYPE
      LOGICAL VERTEX_CDC,VERTEX_FDC,VERNDR
      LOGICAL FIRST, METHOD, VERTEX_ACCURATE,FIRST_CDC, VERTEX_MULTI
      LOGICAL CDCON, FDCON
      LOGICAL MCDATA
C      LOGICAL VTXON,VERTEX_VTX
      LOGICAL EZERROR, FIND, VERTEX_FDC_TRK, FIRST_FDC, FDC_TRK
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      VERTEX = .FALSE.
      FIND = .FALSE.
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VERTEX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','VERTEX',
     &    'Unable to find bank VERTEX_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('VERTEX_TYPE',VERTEX_TYPE,IER)
        CALL EZGET('METHOD',METHOD,IER)
        CALL EZGET('FIRST_CDC',FIRST_CDC,IER)
        CALL EZGET('FIRST_FDC',FIRST_FDC,IER)
        CALL EZGET('FDC_TRK',FDC_TRK,IER)
        CALL EZRSET
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','VERTEX',
     &    'Unable to find bank ZTRAKS_RCP','W')
          GOTO 999
        ENDIF
C        CALL EZGET('VTXON',VTXON,IER)
        CALL EZGET('CDCON',CDCON,IER)
        CALL EZGET('FDCON',FDCON,IER)
        MCDATA = IQ(LHEAD + 1) .GT. 1000
        CALL EZRSET
      END IF
C
C ****  Read in XY information.  OVERWRITE RCP BANK!
C
      IF(.NOT.MCDATA) CALL XYRCP
C
C when VERTEX_TYPE = 0, build VERT from data, 
C otherwise build VERT by VERT_BUILD depending on VERTEX_TYPE
C
      IF (VERTEX_TYPE .NE. 0) THEN
        CALL VERT_BUILD
        GOTO 999
      ENDIF
C
      CALL DHDIR('VERTEX_RCP','HBOOK_DIRECTORY',IER,' ')
      IF (IER.NE.0) THEN
        CALL ERRMSG('VERTEX','VERTEX',
     &  ' ERROR SETTING HBOOK DIRECTORY ','W')
      END IF
C
      IF (METHOD .EQ. 2) THEN              ! precise vertex
        IF (FIRST_CDC) THEN
          IF (CDCON) THEN
            IF (VERTEX_ACCURATE()) GOTO 100  ! using CDC tracks
C          VERTEX=VERTEX_VTX(METHOD)       ! using VTX tracks
C          IF (VERTEX) GO TO 100
          ENDIF
        ELSE
          IF (FIRST_FDC .AND. FDCON) THEN
            FIND = VERTEX_FDC_TRK()   ! using FDC tracks
            IF (FIND) GOTO 100
          ENDIF
C          VERTEX=VERTEX_VTX(METHOD)
C          IF (VERTEX) GO TO 100
          IF (CDCON) FIND = VERTEX_ACCURATE() 
          IF (FIND) GO TO 100
        END IF
      ELSE  
        IF (FIRST_CDC .AND. CDCON) THEN
          IF (METHOD .GT. 2) THEN
            IF (VERTEX_MULTI()) GOTO 100  ! multi-vertices finding by CDC
            IF (FDC_TRK .AND. FDCON) THEN
              IF (VERTEX_FDC_TRK()) GOTO 100   ! using FDC tracks
            ENDIF
          ELSE
            IF (VERTEX_CDC()) GOTO 100    ! using CDC hits (fast vertex)
          ENDIF
        ELSE
          IF (FIRST_FDC .AND. FDCON) THEN
            FIND = VERTEX_FDC_TRK()   ! using FDC tracks
            IF (FIND) GOTO 100
            IF (CDCON) THEN
              IF (METHOD .GT. 2) THEN
                IF (VERTEX_MULTI()) GOTO 100  ! multi-vertices finding by CDC
              ELSE
                IF (VERTEX_CDC()) GOTO 100    ! using CDC hits (fast vertex)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C     IF (VTXON) VERTEX=VERTEX_VTX(1)        ! using VTX hits
      IF (.NOT. VERTEX .AND. FDCON .AND. (.NOT.FDC_TRK)) 
     &  VERTEX = VERTEX_FDC()                ! using FDC hits
C
 100  CONTINUE
      IF(.NOT.MCDATA) CALL XYVERT ! find X and Y of vertex
      CALL VERHIS
      VERTEX=.TRUE. 
  999 RETURN 
C
      ENTRY VERNDR
      VERNDR=.TRUE.
      RETURN
      END       
