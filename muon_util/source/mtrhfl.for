      SUBROUTINE MTRHFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill MTRH words 2-10 (word 1 is icnremented
C-                         by routine MUMUOT)
C-
C-   Inputs  : none
C-   Outputs : fills MTRH as follows:
C         2  I  STP version number for MC generation, copy from MUD1
C         3  I  STP version number for MC generation, copy from MUD1
C         4  I  STP version number for MURECO
C         5  I  STP version number for MURECO
C-        6  I  Truncation flags for SAMUS North
C-        7  I  Truncation flags for SAMUS Sorth
C-        8  I  L1.5 mgr states/mgr errors
C-        9  I  L1.5 high pt octants (run 1a and run 1b)
C-       10  I  L1.5 low pt octants (run 1b only)
C-       11  I  L1.0 WAMUS
C-       12  I  L1.0 SAMUS
C-       13  I  
C-       14  F  WAMUS Magnet Polarity
C-       15  F  SAMUS Magnet Polarity
C-   Controls: none
C-
C-   Created  11-MAY-1994   Darien R. Wood
C-   Modified 7/94  MF  Use MOTWRD for L15 words, 9/94 add words 11-15
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER I,NMOD,IMUD1,ITRNC,NMUHP,IMUHP
      INTEGER JPLN,NMUOH,IMUOH,NMSCT,IMSCT,ISP1,ISP2
      INTEGER ITRIG,ICENFL,JCCT,JFINE(4),JLAT(3)
      INTEGER IFLAG_TRUNC_N,IFLAG_TRUNC_S,IBIT_TRUNC,IVSN,LMTRH
      INTEGER IDUMMY,ISEC,ISTA,ILAY
      INTEGER MUDVER,GZMTRH,IMERR
      INTEGER IL1WA,IL1SA,ISTAT,IPTHI,IPTLO,IXTRA
      REAL PWAM,PSAM
      EXTERNAL MUDVER,GZMTRH
C
C----------------------------------------------------------------------
      LMTRH = GZMTRH(1)            ! find MTRH bank
      IF ( LMTRH.EQ.0 ) GOTO 999
C
C*** fill STP version words (2-5)
C
      CALL FLVSN_MTRH
C
C*** fill SAMUS truncation flags (6-7)
C
C initialize SAMUS truncation flag words
      IFLAG_TRUNC_N = 0
      IFLAG_TRUNC_S = 0
C location of truncation bit depends on MUD1 version
      IVSN = MUDVER(0)
      IF(MOD(IVSN,10).EQ.1) THEN
C 1A format
        IBIT_TRUNC = 1
      ELSEIF(MOD(IVSN,10).EQ.2) THEN
C 1B format
        IBIT_TRUNC = 5
      ELSE
C bad format
        GOTO 888
      ENDIF
C loop over SAMUS module
      DO I=400,460
        CALL GTMUHM(I,NMOD,IMUD1,ITRNC,NMUHP,IMUHP,
     &                  JPLN,NMUOH,IMUOH,NMSCT,IMSCT,ISP1,ISP2,
     &                  ITRIG,ICENFL,JCCT,JFINE,JLAT)
        IF(NMOD.GT.0) THEN
          IF(BTEST(ITRNC,IBIT_TRUNC)) THEN
            CALL MUMDAT(NMOD,IDUMMY,ISEC,ISTA)
            IF(ISEC.GE.1 .AND. ISEC.LE.6  .AND.
     &        ISTA.GE.1 .AND. ISTA.LE.6) THEN
C mark truncation by half-plane (0-17) in North and in South
              ILAY = MOD(ISTA-1,3)*6 + ISEC-1
              IF(ISTA.LE.3) THEN
                IFLAG_TRUNC_N = IBSET(IFLAG_TRUNC_N,ILAY)
              ELSE
                IFLAG_TRUNC_S = IBSET(IFLAG_TRUNC_S,ILAY)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      IQ(LMTRH+6) = IFLAG_TRUNC_N
      IQ(LMTRH+7) = IFLAG_TRUNC_S
  888 CONTINUE
C
C*** fill Level 1.5 words (8-12)
C
      CALL MOTWRD(IL1WA,IL1SA,ISTAT,IPTHI,IPTLO,IXTRA)
      IQ(LMTRH+8) = ISTAT
      IQ(LMTRH+9) = IPTHI
      IQ(LMTRH+10) = IPTLO
      IQ(LMTRH+11) = IL1WA
      IQ(LMTRH+12) = IL1SA
C
C*** fill magnet polarity
C
      CALL MU_MAG_POL(0,PWAM,PSAM,IMERR)
      Q(LMTRH+14) = PWAM
      Q(LMTRH+15) = PSAM
C
  999 RETURN
      END
