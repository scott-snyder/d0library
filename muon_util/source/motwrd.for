      SUBROUTINE MOTWRD(IL1WA,IL1SA,ISTAT,IPTHI,IPTLO,IXTRA)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Return packed muon trigger information
C-
C-    Input  :  
C-
C-    Output :  IL1WA  - CCT bits for WAMUS central, north & south
C-              IL1SA  - CCT bits for overlap and SAMUS
C-              ISTAT  - OTC status bits
C-              IPTHI  - OTC bits for high pt table
C-              IPTLO  - OTC bits for low pt table
C-
C-    Created :  8-JUL-94  M. Fortner
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IL1WA,IL1SA,ISTAT,IPTHI,IPTLO,IXTRA
      INTEGER ILWC,ILWN,ILWS,ILON,ILOS,ILSN,ILSS
      INTEGER JREG,IVER,IQUAD,IMGR,IDUM,ICRID
      INTEGER IOTC,IDNO,IFLG,IOST,NBNK,IBNK
      INTEGER IC1,IC2,IC3,IPT,ICCT,IX1,IX2,IX3
      INTEGER I,IPTR,ILO,IHI
      INTEGER MUOBIT(0:15,5)
      DATA MUOBIT/
     1      7, 0, 1, 2, 3, 4, 5, 6, 8*56,
     2     12,12, 8,11,15,15,13,13, 9,10,14,14,4*57,
     3     20,20,16,19,23,23,21,21,17,18,22,22,4*58,
     4     24,27,25,26,12*59,
     5     28,31,29,30,12*60/
C
      IL1WA = 0
      IL1SA = 0
      ISTAT = 0
      IPTHI = 0
      IPTLO = 0
      IXTRA = 0
C
C        Fetch level 1 information
C
      CALL MOTCCT(ILWC,ILWN,ILWS,ILON,ILOS,ILSN,ILSS)
      CALL MVBITS(ILWC,12,8,IL1WA, 0) ! CF octant bits
      DO IDUM=0,3
        CALL MVBITS(ILWN,IDUM+12,1,IL1WA,10+IDUM*2)   ! WN bits
        CALL MVBITS(ILWS,IDUM+12,1,IL1WA,10+IDUM*2+1) ! WS bits
        CALL MVBITS(ILON,IDUM+12,1,IL1SA,IDUM*2)      ! ON bits
        CALL MVBITS(ILOS,IDUM+12,1,IL1SA,IDUM*2+1)    ! OS bits
        CALL MVBITS(ILSN,IDUM+2, 1,IL1SA,10+IDUM*2)   ! SN bits
        CALL MVBITS(ILSS,IDUM+2, 1,IL1SA,10+IDUM*2+1) ! SS bits
      ENDDO
      CALL MVBITS(ILWC,20,1,IL1WA, 8) ! CF region bit
      CALL MVBITS(ILWN,20,1,IL1WA,18) ! WN region bit
      CALL MVBITS(ILWS,20,1,IL1WA,19) ! WS region bit
      CALL MVBITS(ILWC, 0,2,IL1WA,20) ! CF 2 bit counter bits
      CALL MVBITS(ILWN, 0,2,IL1WA,24) ! WN 2 bit counter bits
      CALL MVBITS(ILWS, 0,2,IL1WA,26) ! WS 2 bit counter bits
      CALL MVBITS(ILON,20,1,IL1SA, 8) ! ON region bit
      CALL MVBITS(ILOS,20,1,IL1SA, 9) ! OS region bit
      CALL MVBITS(ILSN,20,1,IL1SA,18) ! SN region bit
      CALL MVBITS(ILSS,20,1,IL1SA,19) ! SS region bit
      CALL MVBITS(ILON, 0,2,IL1SA,20) ! ON 2 bit counter bits
      CALL MVBITS(ILOS, 0,2,IL1SA,22) ! OS 2 bit counter bits
      CALL MVBITS(ILSN, 0,2,IL1SA,24) ! SN 2 bit counter bits
      CALL MVBITS(ILSS, 0,2,IL1SA,26) ! SS 2 bit counter bits
      CALL MVBITS(ILON,10,1,IL1SA,28) ! ON mult. veto bit
      CALL MVBITS(ILOS,10,1,IL1SA,29) ! OS mult. veto bit
      CALL MVBITS(ILSN, 8,1,IL1SA,30) ! SN mult. veto bit
      CALL MVBITS(ILSS, 8,1,IL1SA,31) ! SS mult. veto bit
C
C        Fetch level 1.5 information
C
      DO JREG = 1,5
        CALL GTMTRG(JREG,-1,IVER,IQUAD,IMGR,IDUM,ICRID)
        IPTR = (JREG-1)*2 + 15
        IF (BTEST(IMGR,3)) ISTAT=IBSET(ISTAT,IPTR)
        IF (BTEST(IMGR,0)) ISTAT=IBSET(ISTAT,IPTR+1)
        IF (IVER.GE.11028) THEN
          CALL GTMTRG(JREG,-2,IVER,IQUAD,IMGR,IDUM,ICRID)
          IPTR = (JREG-1)*3
          CALL MVBITS(IMGR,0,3,ISTAT,IPTR)
        ENDIF
C
        DO IOTC = 0,15
          CALL GTMTRG(JREG,IOTC,IDNO,IFLG,IOST,NBNK,IBNK)
          IF (NBNK.GT.0) THEN
            IPTR = MUOBIT(IOTC,JREG)
            IF (IPTR.GE.32) THEN
              ISTAT = IBSET(ISTAT,IPTR-32)
            ELSE
              ILO = 0
              IHI = 0
              DO IDUM = IBNK,IBNK+NBNK-1
                CALL GTMOTR(IDUM,IC1,IC2,IC3,IPT,ICCT,IX1,IX2,IX3)
                IF (IVER.LT.11028) ICCT=1
                IF (IPT.GT.0.AND.ICCT.GT.0) IHI=1
                IF (ICCT.GT.0) ILO=1
              ENDDO
              IF (IHI.GT.0) IPTHI=IBSET(IPTHI,IPTR)
              IF (ILO.GT.0) IPTLO=IBSET(IPTLO,IPTR)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
      RETURN
      END
