      SUBROUTINE MUHPFL(IGO,NMOD,NCEL,NLAT,NLOC)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Fill MUHP bank for one MUD1 hit
C-
C-   Inputs  :  IGO    - control flag
C-                       IGO=1   initialize/book bank
C-                          =2   fill even information
C-                          =3   fill odd information
C-                          =99  compress bank
C-              NMOD   - Module id
C-              NCEL   - Cell number
C-              NLAT   - Latch bits
C-              NLOC   - Location in MUD1 bank
C-
C-    Output :  D0 Zebra output bank.   (MUHP)
C-
C-    Created :  26-AUG-93  M. Fortner
C-
C-    Modified:  2/94 M. Fortner  added MUHM bank fill
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
C                              
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'     ! D0 ZEBRA bank.
C
      INTEGER IGO,NMOD,NCEL,NLAT,NLOC,NWD
C
C  -- local variables.         
      INTEGER LMUHP,JMUHP,IMUHP
      INTEGER I,MLOC(460),NDUM(4)
      INTEGER NMUHP,LMUHT
      INTEGER  GZMUHP
      EXTERNAL GZMUHP
      SAVE JMUHP,LMUHP
C
C                Initialize and book bank if needed
C
      IF (IGO.EQ.1) THEN
          LMUHP = GZMUHP(0)
          IF (LMUHP.EQ.0) THEN
              CALL BKMUHP(0,0,LMUHP)
          ENDIF
          DO I = 1,460
            MLOC(I) = 0
          ENDDO
          JMUHP = 0
C
C                First load of even information
C
      ELSE IF (IGO.EQ.2) THEN
          IMUHP = LMUHP + JMUHP*5
          JMUHP = JMUHP + 1
          IQ(IMUHP+1) = NCEL
          IQ(IMUHP+2) = NLAT
          IQ(IMUHP+3) = NLOC
          IQ(IMUHP+4) = NLOC
          IQ(IMUHP+5) = 0
          IF (MLOC(NMOD).EQ.0) THEN
              CALL MUHMFL(4,NMOD,0,JMUHP,NDUM)
              CALL MUOFFL(4,NMOD,0,JMUHP)
          ELSE
              IMUHP = LMUHP + (MLOC(NMOD)-1)*5
              IQ(IMUHP+5) = JMUHP
          ENDIF
          MLOC(NMOD) = JMUHP
C
C                Second load of odd information
C
      ELSE IF (IGO.EQ.3) THEN
          IMUHP = LMUHP + (MLOC(NMOD)-1)*5
          IQ(IMUHP+4) = NLOC
C
C                Compress bank
C
      ELSE IF (IGO.EQ.99) THEN
          LMUHP = GZMUHP(0)
          LMUHT = LQ(LMUHP+1)
          NMUHP = IQ(LMUHT+7)
          NWD = NMUHP*5 - IQ(LMUHP-1)
          CALL MZPUSH(IXCOM,LMUHP,0,NWD,'I')
C
C
      ENDIF
C
      RETURN
      END
