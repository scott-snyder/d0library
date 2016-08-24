      SUBROUTINE FLDLEP(SECTOR,FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate Phi, Eta and z vertex position of a 
C-                         track in CDC in a Level-2 node. Fill filter
C-                         result bank DLEP.
C-
C-   Inputs  : SECTOR = Sector numbers in all 4 layers where a segement
C-                      has been found.
C-   Outputs : FLAG   = Set true if the event is to be passed.
C-   Controls: none.
C-
C-   Created  22-APR-1991   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER SECTOR(0:3)
      INTEGER LYR02,LYR13
      INTEGER CND1A,CND1B,CND2A,CND2B,CND3A,CND3B,CND4A,CND4B
      INTEGER LYRALLA,LYRALLB,LYRALL
      INTEGER NTRACK,ISCT,KSCT,PHI
C
      INTEGER LDLEP,GZFRES
      LOGICAL CDC_REQ4,FLAG,BTEST
      REAL    THETA,ZVTX,PI
      DATA PI /3.141593/
      DATA CDC_REQ4 /.FALSE./
C
C----------------------------------------------------------------------
      LYR02 = iand(SECTOR(0),SECTOR(2))
      LYR13 = iand(SECTOR(1),SECTOR(3))
      IF (CDC_REQ4) THEN
        LYRALL = ior(iand(LYR02,LYR13), iand(ISHFTC(LYR02,-1,32),LYR13))
      ELSE
        CND1A =  iand(LYR02,SECTOR(1))
        CND1B =  iand(ISHFTC(LYR02,-1,32), SECTOR(1))

        CND2A =  iand(LYR02,SECTOR(3))
        CND2B =  iand(ISHFTC(LYR02,-1,32), SECTOR(3))

        CND3A =  iand(LYR13,SECTOR(0))
        CND3B =  iand(LYR13,ISHFTC(SECTOR(0),-1,32))

        CND4A =  iand(LYR13,SECTOR(2))
        CND4B =  iand(LYR13,ISHFTC(SECTOR(2),-1,32))
C
        LYRALLA = ior(ior(CND1A,CND2A),ior(CND3A,CND4A))
        LYRALLB = ior(ior(CND1B,CND2B),ior(CND3B,CND4B))
        LYRALL =  ior(LYRALLA, LYRALLB)
      ENDIF
C
C  Pass event if segement found in r-phi,  Calculate Eta,Phi and z vertex.
C  Book DLEP filter result bank and fill.
C
      NTRACK = 0
      CALL BKDLEP(LDLEP)              ! Book filter bank
      IF (LYRALL.NE.0) THEN
        FLAG = .TRUE.                   ! Pass event
C
        DO ISCT = 0,31
          IF (BTEST(LYRALLA,ISCT)) THEN
            NTRACK = NTRACK + 1
            PHI = 2*ISCT
            CALL DL2GT_ETA(ISCT,ISCT,ISCT,ISCT,THETA,ZVTX)
            Q(LDLEP+3*NTRACK-1) = ZVTX
            Q(LDLEP+3*NTRACK)   = (2.*PI*FLOAT(PHI)/64.) + PI/64.
            Q(LDLEP+3*NTRACK+1) = THETA
          ENDIF
          IF (BTEST(LYRALLB,ISCT)) THEN
            NTRACK = NTRACK + 1
            PHI = 2*ISCT+1
            IF (PHI.LT.0) PHI = 64 + PHI
            KSCT = ISCT + 1
            IF (KSCT.EQ.32) KSCT = 0
            CALL DL2GT_ETA(KSCT,ISCT,KSCT,ISCT,THETA,ZVTX)
            Q(LDLEP+3*NTRACK-1) = ZVTX
            Q(LDLEP+3*NTRACK)   = (2.*PI*FLOAT(PHI)/64.) + PI/64.
            Q(LDLEP+3*NTRACK+1) = THETA
          ENDIF
        ENDDO
      ENDIF
      IQ(LDLEP+1) = NTRACK              ! Fill number of tracks found
C
  999 RETURN
      END
