      SUBROUTINE MUON_GETBIN(ISAPART,RVERTEX,P4)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given Vertex and 4 vector of track,
C-                         DETERMINES impact ETA,PHI and the KEY.
C-                         To be used for MUONLIBRARY for Muon Chamber
C-                         Hits
C-
C-   Inputs  : ISAPART = PARTICLE ID (ISAJET)
C-             RVERTEX(3) = Vertex of track
C-             P4(4)     = 4 vector of track
C-   Outputs : KEY(*)    = Key for MUON HIT library, IN MULCON
C-   CREATED 10-MAY-1993   Jasbir Singh
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MULCON.INC'
      INCLUDE 'D0$INC:MULDAT.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER IER,ISAPART,GEAPART
      LOGICAL EM,HAD
      REAL    RVERTEX(*),P4(*)
      REAL    DIR_COSINE(3),PP,ZV,P_KE,PHI_KEY_CUT,ARC
      REAL ARGSOK
      INTEGER IBIN,I,ICC
      INTEGER KEY_ID,KEY_PHI
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      CALL UCOPY(P4,P4_PRIMARY,4)       ! STORE
      PP = SQRT(P4(1)*P4(1)+P4(2)*P4(2)+P4(3)*P4(3))
      DO I = 1 , 3
        DIR_COSINE(I) = P4(I)/PP
      ENDDO
      CALL MUON_CLINPH_FAST(RVERTEX,DIR_COSINE,ETAM_PRIMARY,
     &  PHIM_PRIMARY,IETAM_PRIMARY,IPHIM_PRIMARY,ICC,ARGSOK)
      IF(ARGSOK.NE.0)THEN
        GO TO 990
      ENDIF
C
C ****  determine keys
C
      KEY(1) = IPHIM_PRIMARY
      KEY(2) = IETAM_PRIMARY
      CALL ISAGEA(ISAPART,GEAPART)      ! CONVERT TO GEANT.
      IPART_PRIMARY = GEAPART           ! STORE GEANT ID
      CALL GEAN_KEYID(GEAPART,KEY_ID)   ! KEY_ID GIVEN GEAN_PARTID
C
      IF(P4(4).LT.MOMBIN(1).OR.KEY_ID.EQ.0)GOTO 990
C
      DO 100 IBIN=1,NMOM
        IF (MOMBIN(IBIN).LE.P4(4))THEN
          IF(MOMBIN(IBIN+1).GT.P4(4))THEN
            KEY(3)=IBIN
            GO TO 101
          ENDIF
        ENDIF
  100 CONTINUE
  101 KEY(4) = KEY_ID                   ! PARTICLE ID
  999 RETURN
C
  990 CALL UZERO(KEY,1,NKEY)               ! ZERO THEM
      RETURN
      END
