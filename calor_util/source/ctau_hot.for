      SUBROUTINE CTAU_HOT(LJETS,ET,JETA,JPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find Et for the hottest 4 towers in the jet
C-
C-   Inputs  : LJETS : address of JETS bank
C-   Outputs : ET: array of Et for the 4 hottest toweres
C-             JETA: array of IETA for the 4 hottest toweres
C-             JPHI: array of IPHI for the 4 hottest toweres
C-
C-   Created  12-NOV-1993   Qizhong Li-Demarteau
C-   Updated  12-DEC-1994   Qizhong Li-Demarteau  added 3rd and 4th Et
C-                                          and added ETA and PHI info
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCATE.INC'
      INCLUDE 'D0$LINKS:IZJPTS.LINK'
      INTEGER LJPTS, LJETS, LCAEP, GZCAEP, LCATE, GZCATE
      INTEGER I, NJPTS, NREPCAEP, NREPCATE, MAX_CELLS
      PARAMETER( MAX_CELLS = 2000 )
      INTEGER PTRCATE(MAX_CELLS), IMAP(MAX_CELLS), PTOWERS(MAX_CELLS)
      INTEGER POINTER, IADDR, NCELL, SAMETW, NTOWERS
      INTEGER IETA, IPHI, ILYR
      REAL    ETTOWER(500), ET(4)
      INTEGER JETA(4), JPHI(4), IETATOWER(500), IPHITOWER(500)
C----------------------------------------------------------------------
C
      DO I = 1, 4
        ET(I) = 0.0
        JETA(I) = 0
        JPHI(I) = 0
      ENDDO
C
      LJPTS = LQ(LJETS - IZJPTS)
      IF (LJPTS .LE. 0) GOTO 999   
      NJPTS = IQ(LJPTS+2)
      IF (NJPTS .GT. MAX_CELLS) THEN
        CALL ERRMSG ('CTAU_HOT', 'TOO MANY CELLS',' TRUNCATED', 'W')
        NJPTS = MAX_CELLS
      ENDIF
C
      LCAEP = GZCAEP()
      IF ( LCAEP .LE. 0 ) THEN
        CALL ERRMSG('CALORIMETER','CTAU_HOT',' LCAEP = 0','W')
        GOTO 999
      END IF
      NREPCAEP = IQ(LCAEP+2)
C
      DO 200 I = 1 , NJPTS
        POINTER = LCAEP + NREPCAEP*(IQ(LJPTS+2+I)-1)
        IADDR = IQ(POINTER+4)  
        CALL CAEP_INDICES(IADDR,IETA,IPHI,ILYR)
        PTRCATE(I) = PTCATE(IETA,IPHI,2)
  200 CONTINUE
C
      NCELL = NJPTS
      LCATE = GZCATE()
      NREPCATE = IQ(LCATE+2)
C
      CALL SRTINT(PTRCATE,NCELL,IMAP)
      SAMETW = 0
      NTOWERS = 0
      DO I = 1, NCELL
        IF (PTRCATE(I) .NE. SAMETW) THEN
          SAMETW = PTRCATE(I)
          NTOWERS = NTOWERS+1
          PTOWERS(NTOWERS) = PTRCATE(I)
          POINTER = LCATE + (NREPCATE*(PTOWERS(NTOWERS)-1)+4)
          ETTOWER(NTOWERS) = Q(POINTER+4)
          IETATOWER(NTOWERS) = IQ(POINTER+8)
          IPHITOWER(NTOWERS) = IQ(POINTER+9)
          IF (NTOWERS .GE. 4) GOTO 100
        ENDIF
      ENDDO
  100 DO I = 1, 4
        ET(I) = ETTOWER(I)
        JETA(I) = IETATOWER(I)
        JPHI(I) = IPHITOWER(I)
      ENDDO
C
  999 RETURN
      END
