      FUNCTION MCDATA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Drop GEAN and/or ISAJET banks and/or RECO banks
C-
C-   ENTRY MCDDIA
C-      change option on which to drop
C-      default is to drop GEAN but not ISAJET
C-
C-   ENTRY MCDINI: tell about defaults at initialization
C-
C-   Created  20-JAN-1989   Serban D. Protopopescu
C-   Updated  10-Jan-1996  sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZGEAN.LINK'
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZFAKE.LINK'
      INCLUDE 'D0$LINKS:IZRECO.LINK'
      INTEGER LGHIT,GZGHIT
      LOGICAL MCDATA,MCDDIA,MCDINI
      LOGICAL GEAN,ISAE,FAKE,GHIT,RECO,YES
      CHARACTER*78 MSG
      CHARACTER*4 MCBANKS(5),PATH,SPATH
      DATA GEAN,GHIT,ISAE,FAKE,RECO/.TRUE.,3*.FALSE.,.TRUE./
      DATA MCBANKS/'GEAN',3*' ','RECO'/
C----------------------------------------------------------------------
C
      IF(GHIT) THEN
        LGHIT=GZGHIT()
        CALL MZDROP(IXCOM,LGHIT,' ')
      ENDIF
      IF(GEAN) CALL MZDROP(IXCOM,LQ(LHEAD-IZGEAN),' ')
      IF(ISAE) CALL MZDROP(IXCOM,LQ(LHEAD-IZISAE),' ')
      IF(FAKE) CALL MZDROP(IXCOM,LQ(LHEAD-IZFAKE),' ')
      IF(RECO) CALL MZDROP(IXCOM,LQ(LHEAD-IZRECO),' ')
      IF(GEAN.OR.GHIT) CALL MZGARB(IXMAIN,0)   ! avoid garbage collection later
      IF(RECO) CALL MKPATH
      MCDATA=.TRUE.
      RETURN
C
C
      ENTRY MCDDIA
C
      CALL PATHRS
      CALL PATHGT(SPATH)
      WRITE(MSG,111)SPATH
      CALL GETPAR(1,MSG(1:42),'C',PATH)
      IF(PATH.EQ.' ') PATH=SPATH
      CALL PATHDF(PATH)
      CALL PATHRS
      WRITE(MSG,113) PATH
      CALL INTMSG(MSG)
C
      WRITE(MSG,110) MCBANKS
      CALL OUTMSG(MSG)
      YES=.FALSE.
      CALL GETPAR(1,'Change which banks to drop? [N]:','L',YES)
C
      IF(YES) THEN
        MCBANKS(1)= ' '
        GEAN=.TRUE.
        CALL GETPAR(1,'Drop GEAN banks? [Y]:','L',GEAN)
        IF(GEAN) MCBANKS(1)='GEAN'
        IF(.NOT.GEAN) THEN
          MCBANKS(2)=' '
          GHIT=.TRUE.
          CALL GETPAR(1,'Drop GHIT banks? [Y]:','L',GHIT)
          IF(GHIT) MCBANKS(2)='GHIT'
        ENDIF
        MCBANKS(3)=' '
        ISAE=.FALSE.
        CALL GETPAR(1,'Drop ISAE banks? [N]:','L',ISAE)
        IF(ISAE) MCBANKS(3)='ISAE'
        MCBANKS(4)=' '
        FAKE=.FALSE.
        CALL GETPAR(1,'Drop FAKE banks? [N]:','L',FAKE)
        IF(FAKE) MCBANKS(4)='FAKE'
        MCBANKS(5)=' '
        RECO=.FALSE.
        CALL GETPAR(1,'Drop RECO banks? [N]:','L',RECO)
        IF(RECO) MCBANKS(4)='RECO'
      ENDIF
C
      MCDDIA=.TRUE.
C
      RETURN
C
C
      ENTRY MCDINI
C
      WRITE(MSG,110) MCBANKS
      CALL INTMSG(MSG)
      CALL PATHGT(PATH)
      WRITE(MSG,112) PATH
      CALL INTMSG(MSG)
      MCDINI=.TRUE.
C
  999 RETURN
C
  110 FORMAT(' Following banks will be dropped: ',54A6)
  111 FORMAT(' Chose PATH (RECO, FAKE or GEAN), [',A4,']:>')
  112 FORMAT(' Default PATH is ',A4,'. Use option User Dialog to change'
     &  ,' PATH or drops.')
  113 FORMAT(' Default PATH is now ',A4,'.')
      END
