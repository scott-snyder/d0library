      SUBROUTINE MU_SAM_MAC(MODNO,MODID,MCRS,MFINE,NFINE)
C---------------------------------------------------------------
C  Fill one module with his in MAC SAMUS format
C  and create fine and coarce centroids
C  Assumes existence of SAHH and MUD1 banks
C  Created 11-91 M. Fortner
C  Change coarse centroids to OR of hits  5-18-92, K. Bazizi
C  Ad centroid trancation in MAC         10-10-92, K. Bazizi
C
C
C  MONOD is the Phil Martin number
C  MODID is the id for OTC triger on fine centroids
C  MCRS is the 16 bit coarce centroid output
C  MFINE is the list of fine centroid addresses
C  NFINE is the number of fine centroids
C
C---------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MODNO,MODID,MFINE(32),NFINE,MCRS(0:15)
      INTEGER NRAW, IMUD1,IWADD,IWEVEN,IWODD,NMOD,NWIR
      INTEGER I,J,JMUD1,IHITS
      INTEGER IC0,IC1,IH0,IH1,IH2,IH3
      INTEGER MODXY,MACHIT(128),MCENT(0:127),IDFINE
      INTEGER SAM_CENT_CUT,IER
      CHARACTER*72 STRING
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C<<
C
C-- get fine centroid cut for MAC
      IF (FIRST) THEN
        FIRST=.FALSE.
C-- Get names of files for OTC tables
        CALL EZPICK('MUSIM_RCP')
        CALL EZERR(IER)     ! Check if error
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' EZPICK ERR','MU_SAM_MAC',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZGET('SAM_MAC_CENT_CUT',SAM_CENT_CUT,IER)
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' SAM_MAC_CENT_CUT','MU_SAM_MAC',STRING,'F')
          GOTO 999
        ENDIF
      ENDIF
C               Initialize arrays
      DO I=1,128
        MACHIT(I)=0
      ENDDO
      DO I=1,32
        MFINE(I)=0
      ENDDO
      DO I=0,15
        MCRS(I)=0
      ENDDO
      MODXY=0
      IF ((MODNO/2)*2.EQ.MODNO) MODXY=1
C<<
C.. Get location of module in MUD1
      CALL GTSAHH(MODNO,IMUD1,NRAW)
C<<
      IF (NRAW.EQ.0) RETURN
C<<
C.. Loop over hits in this module
      DO I=1,NRAW
        JMUD1=IMUD1+(I-1)*3-1
        CALL GTMUDS(JMUD1,IWADD,IWEVEN,IWODD,NMOD,NWIR)
        NWIR=NWIR/2
        MACHIT(NWIR+2)=1
      ENDDO
C<<
C-- calculate coarse centroids
      IHITS=64
      IF (MODXY.EQ.0) IHITS=96
      DO I=1,IHITS
        J=(I-1)/4
        IF(MODXY.EQ.0) J=(I-1)/6
        IF(MACHIT(I+1).NE.0) MCRS(J)=1
      ENDDO
C<<
C-- calculate fine centroids
      DO I=1,IHITS
        J=I*2-1
        IC0=0
        IC1=0
        IH0=MACHIT(I)
        IH1=MACHIT(I+1)
        IH2=MACHIT(I+2)
        IH3=MACHIT(I+3)
        IF (IH0.EQ.0.AND.IH1.NE.0.AND.IH2.EQ.0) IC0=1
        IF (IH0.NE.0.AND.IH1.NE.0.AND.IH2.NE.0) IC0=1
        IF (IH0.EQ.0.AND.IH1.NE.0.AND.IH2.NE.0.AND.IH3.EQ.0) IC1=1
        IF (MODXY.EQ.0) THEN
          IF (IC0.NE.0.OR.IC1.NE.0) MCENT(I-1)=1
        ELSE
          IF (IC0.NE.0) MCENT(J-1)=1
          IF (IC1.NE.0) MCENT(J)=1
        ENDIF
      ENDDO
C<<
C--  fill fine centroid list
      IDFINE=ABS(MODID)*2048
      IHITS=128
      IF (MODXY.EQ.0) IHITS=96
      DO I=0,IHITS-1
        IF (MCENT(I).NE.0) THEN
          IF (NFINE.LT.SAM_CENT_CUT) THEN
            NFINE=NFINE+1
            MFINE(NFINE)=I+IDFINE
          ENDIF
        ENDIF
      ENDDO
999   CONTINUE
C------------------------------------------------------------------------
      RETURN
      END
