      SUBROUTINE ECEMCR_DRV
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : test driver for ECEMCM
C-        what this does should be done in CAPHEL
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-SEP-1992   Anthony L. Spadafora
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'

      LOGICAL FIRST /.TRUE./
      LOGICAL ECEMCR_DEBUG

      INTEGER NVER,LPTR_IN,GZCACL,IER,GZPELC,GZPPHO
      INTEGER CORR_APPLIED
      INTEGER OUTUNIT /11/
      INTEGER ICLUS
      INTEGER NUMRUN,NUMEVT
C
      REAL    ETAD_MIN
      REAL    ZV(14),DZ(14)
      REAL    DE,ERR_DE
      REAL    Z_R,ETAD
      REAL    E_UNC,E_CORR

C----------------------------------------------------------------------
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('ECEMCR_RCP')
        CALL EZGET('ECEMCR_DEBUG',ECEMCR_DEBUG,IER)
        CALL EZRSET
      ENDIF
C
C----------------------------------------------------------------------
C...    first get vertex
C..
      CALL ZVERTE(NVER,ZV,DZ)                ! Vertex from tracking
      IF(NVER.EQ.0) THEN
        CALL ERRMSG('No Vertices','ECEM_DRV','z set to 0','W')
        ZV(1)=0.0
      ENDIF

      CALL EVNTID(NUMRUN,NUMEVT)
C----------------------------------------------------------------------
C..     CLUSTER LOOP

      LCACL = GZCACL()
      DO WHILE(LCACL .NE. 0)
        LPTR_IN = LCACL
        CALL ECEMCR(LPTR_IN,ZV(1),DZ(1),DE,ERR_DE,CORR_APPLIED,IER)
        IF(IER.NE.0.AND.IER.NE.-2) THEN
          CALL ERRMSG('CALORIMETER','ECEMCR_DRV',
     &      ' ECEMCR IER.NE.0 CACL BANK ','W')
        ENDIF
C        
C       don't save these results. i.e. don't overwrite CACL
C...    dump results
        IF(ECEMCR_DEBUG) THEN
          ICLUS = IQ(LCACL+2)
          WRITE(OUTUNIT,10) ICLUS,DE,ERR_DE,CORR_APPLIED
   10     FORMAT(' ECEMCR_DRV: ICLUS ',I5,' DE/ERR_DE/CORR_APPLIED',
     &      2F15.5,Z8)
          CALL PRCACL(OUTUNIT,ICLUS,0,'   ',1)
        ENDIF
C
  300   CONTINUE                          ! GO TO NEXT CLUSTER
        LCACL = LQ(LCACL)
      ENDDO
C----------------------------------------------------------------------
C
C..     ELECTRON PELC LOOP

      LPELC = GZPELC()
      DO WHILE(LPELC .NE. 0)
        LPTR_IN = LPELC
        CALL ECEMCR(LPTR_IN,ZV(1),DZ(1),DE,ERR_DE,CORR_APPLIED,IER)
        IF(IER.NE.0.AND.IER.NE.-2) THEN
          CALL ERRMSG('CALORIMETER','ECEMCR_DRV',
     &      ' ECEMCR IER.NE.0 PELC BANK ','W')
        ENDIF
C
C...    dump results
        IF(ECEMCR_DEBUG) THEN
          E_UNC= Q(LPTR_IN+6)
          WRITE(OUTUNIT,12) E_UNC,DE,ERR_DE,CORR_APPLIED
   12     FORMAT(' ECEMCR_DRV: PELC BANK E/DE/ERR_DE/CORR_APPLIED',
     &      3F15.5,Z8)
          CALL PRPELC(OUTUNIT,LPTR_IN,LPTR_IN-5,'ONE',1)
        ENDIF
C
C       save results by overwriting PELC
        IF(IER.EQ.0) THEN
          E_UNC = Q(LPTR_IN+6)
          E_CORR = E_UNC + DE
          Q(LPTR_IN+3) = Q(LPTR_IN+3)*E_CORR/E_UNC
          Q(LPTR_IN+4) = Q(LPTR_IN+4)*E_CORR/E_UNC
          Q(LPTR_IN+5) = Q(LPTR_IN+5)*E_CORR/E_UNC
          Q(LPTR_IN+6) = E_CORR
          Q(LPTR_IN+7) = Q(LPTR_IN+7)*E_CORR/E_UNC
          Q(LPTR_IN+29) = ERR_DE      !spare
          Q(LPTR_IN+31) = FLOAT(CORR_APPLIED)
        ENDIF
C
        IF(ECEMCR_DEBUG) 
     &    CALL PRPELC(OUTUNIT,LPTR_IN,LPTR_IN-5,'ONE',1)
C          
C
  400   CONTINUE                          ! GO TO NEXT PELC
        LPELC = LQ(LPELC)
      ENDDO
c
C----------------------------------------------------------------------
C
C..     PHOTON PPHO LOOP

      LPPHO = GZPPHO()
      DO WHILE(LPPHO .NE. 0)
        LPTR_IN = LPPHO
        CALL ECEMCR(LPTR_IN,ZV(1),DZ(1),DE,ERR_DE,CORR_APPLIED,IER)
        IF(IER.NE.0.AND.IER.NE.-2) THEN
          CALL ERRMSG('CALORIMETER','ECEMCR_DRV',
     &      ' ECEMCR IER.NE.0 PPHO BANK ','W')
        ENDIF
C
C...    dump results
        IF(ECEMCR_DEBUG) THEN
          E_UNC = Q(LPTR_IN+6)
          WRITE(OUTUNIT,14) E_UNC,DE,ERR_DE,CORR_APPLIED
   14     FORMAT(' ECEMCR_DRV: PPHO BANK E/DE/ERR_DE/CORR_APPLIED',
     &     3F15.5,Z8)
          CALL PRPPHO(OUTUNIT,LPTR_IN,LPTR_IN-5,'ONE',1)
        ENDIF
C
C       save results by overwriting PPHO
        IF(IER.EQ.0) THEN
          E_UNC = Q(LPTR_IN+6)
          E_CORR = E_UNC + DE
          Q(LPTR_IN+3) = Q(LPTR_IN+3)*E_CORR/E_UNC
          Q(LPTR_IN+4) = Q(LPTR_IN+4)*E_CORR/E_UNC
          Q(LPTR_IN+5) = Q(LPTR_IN+5)*E_CORR/E_UNC
          Q(LPTR_IN+6) = E_CORR
          Q(LPTR_IN+7) = Q(LPTR_IN+7)*E_CORR/E_UNC
          Q(LPTR_IN+29) = ERR_DE      !spare
          Q(LPTR_IN+31) = CORR_APPLIED
        ENDIF
C          
        IF(ECEMCR_DEBUG) 
     &      CALL PRPPHO(OUTUNIT,LPTR_IN,LPTR_IN-5,'ONE',1)
C
  500   CONTINUE                          ! GO TO NEXT PPHO
        LPPHO = LQ(LPPHO)
      ENDDO
c
C----------------------------------------------------------------------
  999 RETURN
      END
