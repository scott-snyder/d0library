      SUBROUTINE MUCPLN(LMUOT,PLANES_ALL,PLANES_FIT,IAPLN,IFPLN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Loop over all hits on track and count planes
C-
C-   Inputs  :  LMUOT - Pointer to MUOT track
C-
C-   Outputs :  PLANES_ALL - Compressed list of planes/layer
C-                WA+10*WB+100*WC+1000*WD+10000*SA+100000*SB+1000000*SC+
C-                10000000*WA'+100000000*(WB'+WC') where the ' corresponds
C-                to mixed orientation tracks
C-              PLANES_FIT as above but for those planes used in
C-                         the drift fit
C-              IAPLN - Packed bits for all hit layers
C-              IFPLN - Packed bits for fit hit layers
C-
C-   Created :  D. Hedin      October 8, 1992
C-
C-   Updated   6-JAN-1993   Daria Zieminska  add SAMUS information
C-   Updated   13-APR-1993   Daria Zieminska  protect against #hits > 9
C-   DH 10/94 FOR pure SAMUS tracks, just keep total number of hits
C-   Updated   03-FEB-1995  I.Mandrichenko Use new format of STTH
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMHTT.LINK'
      INCLUDE 'D0$LINKS:IZSTTH.LINK'
C
      INTEGER LMUOT,NPTRACK,NSAMUS,IQUAD,PLANES_ALL,PLANES_FIT
      INTEGER I,NMOD,NPLN,NWIR,ILYR(13),JLYR(13),KLYR(13),LLYR(13)
      INTEGER IERR,IHIT,ITSIGN,LMHTT,IWADD,LAYER,MULAYR,MUQUAD,II
      INTEGER IAPLN,IFPLN,IBIT,LSTTH,NSA,NSB,NSC
      INTEGER NSABC,IDUMMY
      REAL    DUMMY(6)
C
      NPTRACK=IQ(LMUOT+1)
      NSAMUS=IQ(LMUOT+2)
      IQUAD=IQ(LMUOT+3)
      DO I=1,13
        ILYR(I)=0
        JLYR(I)=0
        KLYR(I)=0
        LLYR(I)=0
      ENDDO
      LMHTT=LQ(LMUOT-IZMHTT)
      DO IHIT=1,NPTRACK
        IWADD = IQ(LMHTT+1+5*(IHIT-1))
        ITSIGN = IQ(LMHTT+3+5*(IHIT-1))
        CALL MUADD(IWADD,NMOD,NPLN,NWIR,IERR)
        LAYER=MULAYR(NMOD)
        IF(LAYER.EQ.1) II=NPLN+1
        IF(LAYER.GE.2) II=(LAYER-1)*3+NPLN+2
        IF(MUQUAD(NMOD).EQ.IQUAD) THEN    ! IN SAME QUADRANT
          ILYR(II)=1                      ! SAME QUADRANT ALL HITS
          IF(ITSIGN.NE.0) JLYR(II)=1      ! SAME QUADRANT ALL HITS
        ELSE
          KLYR(II)=1                      ! OTHER QUADRANT ALL HITS
          IF(ITSIGN.NE.0) LLYR(II)=1      ! OTHER QUADRANT ALL HITS
        ENDIF
      ENDDO
      PLANES_ALL=(ILYR(1)+ILYR(2)+ILYR(3)+ILYR(4))+     ! A LAYER PLANES
     A           (ILYR(5)+ILYR(6)+ILYR(7))*10+           ! B LAYER
     A           (ILYR(8)+ILYR(9)+ILYR(10))*100+           ! B LAYER
     A           (ILYR(11)+ILYR(12)+ILYR(13))*1000+           ! C LAYER
     A           (KLYR(1)+KLYR(2)+KLYR(3)+KLYR(4))*10000000+  ! A LAYER ADJACENT
     A(KLYR(5)+KLYR(6)+KLYR(7)+KLYR(8)+KLYR(9)+KLYR(10)+KLYR(11)
     A +KLYR(12)+KLYR(13))*100000000                  ! BC LAYER ADJACENT
      PLANES_FIT=(JLYR(1)+JLYR(2)+JLYR(3)+JLYR(4))+     ! A LAYER PLANES
     A           (JLYR(5)+JLYR(6)+JLYR(7))*10+           ! B LAYER
     A           (JLYR(8)+JLYR(9)+JLYR(10))*100+           ! B LAYER
     A           (JLYR(11)+JLYR(12)+JLYR(13))*1000+           ! C LAYER
     A           (LLYR(1)+LLYR(2)+LLYR(3)+LLYR(4))*10000000+  ! A LAYER ADJACENT
     A(LLYR(5)+LLYR(6)+LLYR(7)+LLYR(8)+LLYR(9)+LLYR(10)+LLYR(11)
     A +LLYR(12)+LLYR(13))*100000000                  ! BC LAYER ADJACENT
C
C  Add SAMUS information
C
      LSTTH=LQ(LMUOT-IZSTTH)
      IF( LSTTH.GT.0 ) THEN
        NSABC = 0
        CALL GTSTTH(0, LSTTH, NSABC, IDUMMY, IDUMMY, IDUMMY,
     +        DUMMY, DUMMY(1))
        NSA = MOD(NSABC,10)
        NSB = MOD(NSABC/10,10)
        NSC = MOD(NSABC/100,10)
      ELSE
        NSA=0
        NSB=0
        NSC=0
        NSABC = 0
      END IF
C
      PLANES_ALL=PLANES_ALL+NSABC*10000
      IF (NPTRACK.EQ.0) THEN ! pure SAMUS: all hits used in fit
        PLANES_FIT=PLANES_FIT+NSABC*10000
      END IF
      IF (NPTRACK.GT.0.AND.NSAMUS.GT.0) THEN
        IF (NSA.EQ.6) THEN
          PLANES_FIT=PLANES_FIT+NSA*10000
        END IF
        IF (NSB+NSC.GT.3) THEN
          PLANES_FIT=PLANES_FIT+NSB*100000+NSC*1000000
        END IF
      END IF
C-
C- Pack planes hit into IAPLN and IFPLN words
      IAPLN=0
      IFPLN=0
      DO IBIT=0,3
        IF(ILYR(IBIT+1).NE.0) IAPLN=IBSET(IAPLN,IBIT)    ! A planes -all
        IF(KLYR(IBIT+1).NE.0) IAPLN=IBSET(IAPLN,IBIT+4)  ! A' planes-all
        IF(JLYR(IBIT+1).NE.0) IFPLN=IBSET(IFPLN,IBIT)    ! A planes -fit
        IF(LLYR(IBIT+1).NE.0) IFPLN=IBSET(IFPLN,IBIT+4)  ! A' planes-fit
      ENDDO
      DO IBIT=0,2
        IF(ILYR(IBIT+5).NE.0) IAPLN=IBSET(IAPLN,IBIT+8)  ! B planes -all
        IF(KLYR(IBIT+5).NE.0) IAPLN=IBSET(IAPLN,IBIT+11) ! B' planes-all
        IF(JLYR(IBIT+5).NE.0) IFPLN=IBSET(IFPLN,IBIT+8)  ! B planes -fit
        IF(LLYR(IBIT+5).NE.0) IFPLN=IBSET(IFPLN,IBIT+11) ! B' planes-fit
      ENDDO
      DO IBIT=0,2
        IF(ILYR(IBIT+8).NE.0) IAPLN=IBSET(IAPLN,IBIT+14) ! C planes -all
        IF(KLYR(IBIT+8).NE.0) IAPLN=IBSET(IAPLN,IBIT+17) ! C' planes-all
        IF(JLYR(IBIT+8).NE.0) IFPLN=IBSET(IFPLN,IBIT+14) ! C planes -fit
        IF(LLYR(IBIT+8).NE.0) IFPLN=IBSET(IFPLN,IBIT+17) ! C' planes-fit
      ENDDO
      DO IBIT=0,2
        IF(ILYR(IBIT+11).NE.0) IAPLN=IBSET(IAPLN,IBIT+20) ! D planes -all
        IF(KLYR(IBIT+11).NE.0) IAPLN=IBSET(IAPLN,IBIT+23) ! D' planes-all
        IF(JLYR(IBIT+11).NE.0) IFPLN=IBSET(IFPLN,IBIT+20) ! D planes -fit
        IF(LLYR(IBIT+11).NE.0) IFPLN=IBSET(IFPLN,IBIT+23) ! D' planes-fit
      ENDDO
C-
      RETURN
      END
