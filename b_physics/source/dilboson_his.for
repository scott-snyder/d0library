      SUBROUTINE DILBOSON_HIS(NHIST,HISDAT,WEIGHT)
C----------------------------------------------------------------------
C-
C-  Booking and filling histograms for DILBOSON package
C-
C-  4-DEC-1991 Chris Murphy, Daria Zieminska
C-  
C-   Updated  24-MAR-1992   c.r.murphy - simplified booking and
C-   filling of histos and allow for reading of titles from rcp.      
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:VEEKIN.INC/LIST'
      LOGICAL FIRST
      INTEGER IER,ERR,MAXHIST,NUMHIST
      PARAMETER (MAXHIST=50)
      INTEGER SLEN,IHIST,NHIST
      REAL DILBOS_HIST(9,MAXHIST),HISDAT,WEIGHT
      CHARACTER*20 TITL
      DATA FIRST/.TRUE./
C
C Create/set HBOOK directory DILBOSON
C
C      CALL DHDIR('DILBOSON_RCP','HBOOK_DIRECTORY',IER,' ')
C      IF ( IER.NE.0 ) THEN
C        CALL ERRMSG('DILBOSON','DILBOSON',
C     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
C      ENDIF
C      IF (FIRST) THEN
C        CALL EZPICK('DILBOSON_RCP')
C        CALL EZGET('DILBOS_HIST(1)',DILBOS_HIST(1,1),ERR)
C        CALL EZGETA('DILBOS_HIST',0,0,0,NUMHIST,ERR)
C        NUMHIST=NUMHIST/9
C        CALL EZRSET
C        FIRST=.FALSE.
C        DO IHIST=1,NUMHIST
C          CALL DHTOC(40,DILBOS_HIST(5,IHIST),TITL)
C          IF (DILBOS_HIST(1,IHIST).EQ.1.) THEN
C            CALL HBOOK1(IHIST,TITL,NINT(DILBOS_HIST(2,IHIST)),
C     &        DILBOS_HIST(3,IHIST),DILBOS_HIST(4,IHIST),0.)
C          ENDIF
C        ENDDO
C      ENDIF
C
C
C      IF (DILBOS_HIST(1,NHIST).EQ.1.) THEN
C        CALL HFILL(NHIST,HISDAT,0.,WEIGHT)
C      ENDIF
C
C
 1000 RETURN
      END
