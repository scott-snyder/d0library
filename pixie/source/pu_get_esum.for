      SUBROUTINE PU_GET_ESUM(ARRAY,IARRAY,NOBJS,VERTZ,PATHNAM,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get Information from ESUM(Event SUMmary) Bank 
C-                         to make its display.
C-
C-   Outputs : ARRAY  [F] - 
C-             IARRAY [I] - ID's
C-             NOBJS  [I] - Number of objects
C-             IOK    [I] - 0 for Okay, 1 for Bad or No ESUM.
C-   Controls:
C-
C-   Modified  5-FEB-1993   Nobuaki Oshima
C-                          Fix the Miss_Et display problem on Phi.
C-   Created   5-FEB-1992   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER NFOUND(ID_ALL:LAST_TYPE),IFLAG,IER
      INTEGER IARRAY(NPHIL,2*NETAL)
      INTEGER NOBJS(*)
      INTEGER NVTX,NJET,NMUO,NELE,NPHO,NTAU,NMIS,NSUM
      INTEGER IETA,IP,IE,I,IOK
      REAL    ET,ETA,THETA,PHI
      REAL    VERTZ(*)
      REAL    ARRAY(NPHIL,2*NETAL)
      CHARACTER*4 CLEVEL,PATHNAM
C-
C----------------------------------------------------------------------
C-
C--- Clear Array
C-
      IOK = 0
      CALL VZERO(ARRAY,NPHIL*2*NETAL)
      CALL VZERO(IARRAY,NPHIL*2*NETAL)
C-
C--- CHECK ESUM BANK IS AVAILABLE OR NOT...
C-
      CALL PUGETA('PHYDIS ESUM PATH',CLEVEL)
      PATHNAM = CLEVEL
      CALL GTESUM_COUNTS(CLEVEL,NFOUND,IER)
      IF (IER .NE. 0) THEN
        CALL ERRMSG('PIXIE','PU_GET_ESUM','Bank ESUM NOT FOUND','W')
        IOK = 1
        GO TO 999
      ENDIF
C-
C=== VERTEX
C-
      NVTX = 0
      DO I = 1,NFOUND(ID_VERTEX)
        CALL GTESUM(CLEVEL,ID_VERTEX,I,ET,ETA,THETA,PHI,IFLAG,IER)
        IF (IER .NE. 0) THEN
          GO TO 100
        ENDIF
        NVTX = NVTX + 1
        IF (NVTX .LE. 3) THEN
          VERTZ(NVTX) = PHI
        ENDIF
      ENDDO
C-
C=== JET
C-
  100 NJET = 0
      DO I = 1,NFOUND(ID_JET)
        CALL GTESUM(CLEVEL,ID_JET,I,ET,ETA,THETA,PHI,IFLAG,IER)
        IF (IER .NE. 0) THEN
          GO TO 200
        ENDIF
        NJET = NJET + 1
        IF (ETA .GT. 3.7) THEN
          ETA=3.7
        ELSEIF(ETA .LT. -3.7) THEN
          ETA=-3.7
        ENDIF
        IETA = 10.*ETA
        IP = (PHI/TWOPI)*64 + 1
        IF(IETA .LT. 0)THEN
          IE = IETA + NETAL + 1
        ELSE
          IE = IETA + NETAL
        ENDIF
        ARRAY(IP,IE)  = ET
        IF (IARRAY(IP,IE).GT.100 .AND. IARRAY(IP,IE).LE.10000) THEN
          IARRAY(IP,IE) = IARRAY(IP,IE) + ID_JET*10000
        ELSEIF (IARRAY(IP,IE).GT.0 .AND. IARRAY(IP,IE).LE.100) THEN
          IARRAY(IP,IE) = IARRAY(IP,IE) + ID_JET*100
        ELSE
          IARRAY(IP,IE) = ID_JET
        ENDIF
      ENDDO
C-
C=== MUON
C-
  200 NMUO = 0
      DO I = 1,NFOUND(ID_MUON)
        CALL GTESUM(CLEVEL,ID_MUON,I,ET,ETA,THETA,PHI,IFLAG,IER)
        IF (IER .NE. 0) THEN
          GO TO 300
        ENDIF
        NMUO = NMUO + 1
        IF (ETA .GT. 3.7) THEN
          ETA=3.7
        ELSEIF(ETA .LT. -3.7) THEN
          ETA=-3.7
        ENDIF
        IETA = 10.*ETA
        IP = (PHI/TWOPI)*64 + 1
        IF(IETA .LT. 0)THEN
          IE = IETA + NETAL + 1
        ELSE
          IE = IETA + NETAL
        ENDIF
        ARRAY(IP,IE)  = ET
        IF (IARRAY(IP,IE).GT.100 .AND. IARRAY(IP,IE).LE.10000) THEN
          IARRAY(IP,IE) = IARRAY(IP,IE) + ID_MUON*10000
        ELSEIF (IARRAY(IP,IE).GT.0 .AND. IARRAY(IP,IE).LE.100) THEN
          IARRAY(IP,IE) = IARRAY(IP,IE) + ID_MUON*100
        ELSE
          IARRAY(IP,IE) = ID_MUON
        ENDIF
      ENDDO
C-
C=== ELECTRON
C-
  300 NELE = 0
      DO I = 1,NFOUND(ID_ELECTRON)
        CALL GTESUM(CLEVEL,ID_ELECTRON,I,ET,ETA,THETA,PHI,IFLAG,IER)
        IF (IER .NE. 0) THEN
          GO TO 400
        ENDIF
        NELE = NELE + 1
        IF (ETA .GT. 3.7) THEN
          ETA=3.7
        ELSEIF(ETA .LT. -3.7) THEN
          ETA=-3.7
        ENDIF
        IETA = 10.*ETA
        IP = (PHI/TWOPI)*64 + 1
        IF(IETA .LT. 0)THEN
          IE = IETA + NETAL + 1
        ELSE
          IE = IETA + NETAL
        ENDIF
        ARRAY(IP,IE)  = ET
        IF (IARRAY(IP,IE).GT.100 .AND. IARRAY(IP,IE).LE.10000) THEN
          IARRAY(IP,IE) = IARRAY(IP,IE) + ID_ELECTRON*10000
        ELSEIF (IARRAY(IP,IE).GT.0 .AND. IARRAY(IP,IE).LE.100) THEN
          IARRAY(IP,IE) = IARRAY(IP,IE) + ID_ELECTRON*100
        ELSE
          IARRAY(IP,IE) = ID_ELECTRON
        ENDIF
      ENDDO
C-
C=== PHOTON
C-
  400 NPHO = 0
      DO I = 1,NFOUND(ID_PHOTON)
        CALL GTESUM(CLEVEL,ID_PHOTON,I,ET,ETA,THETA,PHI,IFLAG,IER)
        IF (IER .NE. 0) THEN
          GO TO 500
        ENDIF
        NPHO = NPHO + 1
        IF (ETA .GT. 3.7) THEN
          ETA=3.7
        ELSEIF(ETA .LT. -3.7) THEN
          ETA=-3.7
        ENDIF
        IETA = 10.*ETA
        IP = (PHI/TWOPI)*64 + 1
        IF(IETA .LT. 0)THEN
          IE = IETA + NETAL + 1
        ELSE
          IE = IETA + NETAL
        ENDIF
        ARRAY(IP,IE)  = ET
        IF (IARRAY(IP,IE).GT.100 .AND. IARRAY(IP,IE).LE.10000) THEN
          IARRAY(IP,IE) = IARRAY(IP,IE) + ID_PHOTON*10000
        ELSEIF (IARRAY(IP,IE).GT.0 .AND. IARRAY(IP,IE).LE.100) THEN
          IARRAY(IP,IE) = IARRAY(IP,IE) + ID_PHOTON*100
        ELSE
          IARRAY(IP,IE) = ID_PHOTON
        ENDIF
      ENDDO
C-
C=== TAU
C-
  500 NTAU = 0
      DO I = 1,NFOUND(ID_TAU)
        CALL GTESUM(CLEVEL,ID_TAU,I,ET,ETA,THETA,PHI,IFLAG,IER)
        IF (IER .NE. 0) THEN
          GO TO 600
        ENDIF
        NTAU = NTAU + 1
        IF (ETA .GT. 3.7) THEN
          ETA=3.7
        ELSEIF(ETA .LT. -3.7) THEN
          ETA=-3.7
        ENDIF
        IETA = 10.*ETA
        IP = (PHI/TWOPI)*64 + 1
        IF(IETA .LT. 0)THEN
          IE = IETA + NETAL + 1
        ELSE
          IE = IETA + NETAL
        ENDIF
        ARRAY(IP,IE)  = ET
        IF (IARRAY(IP,IE).GT.100 .AND. IARRAY(IP,IE).LE.10000) THEN
          IARRAY(IP,IE) = IARRAY(IP,IE) + ID_TAU*10000
        ELSEIF (IARRAY(IP,IE).GT.0 .AND. IARRAY(IP,IE).LE.100) THEN
          IARRAY(IP,IE) = IARRAY(IP,IE) + ID_TAU*100
        ELSE
          IARRAY(IP,IE) = ID_TAU
        ENDIF
      ENDDO
C-
C=== MISS_ET
C-
  600 NMIS = 0
      DO I = 1,NFOUND(ID_ETMISS)
        CALL GTESUM(CLEVEL,ID_ETMISS,I,ET,ETA,THETA,PHI,IFLAG,IER)
        IF (IER .NE. 0) THEN
          GO TO 700
        ENDIF
        NMIS = NMIS + 1
        IE = NETAL
        IP = (PHI/TWOPI)*64 + 1
        ARRAY(IP,IE)  = ET
        IF (IARRAY(IP,IE).GT.100 .AND. IARRAY(IP,IE).LE.10000) THEN
          IARRAY(IP,IE) = IARRAY(IP,IE) + ID_ETMISS*10000
        ELSEIF (IARRAY(IP,IE).GT.0 .AND. IARRAY(IP,IE).LE.100) THEN
          IARRAY(IP,IE) = IARRAY(IP,IE) + ID_ETMISS*100
        ELSE
          IARRAY(IP,IE) = ID_ETMISS
        ENDIF
      ENDDO
C-
C=== ETSUM(Just Skip)
C-
  700 NSUM = 0
C-
      NOBJS(1) = NJET
      NOBJS(2) = NMUO
      NOBJS(3) = NELE
      NOBJS(4) = NPHO
      NOBJS(5) = NTAU
      NOBJS(6) = NMIS
  999 RETURN
      END
