      SUBROUTINE MUBOOK_DTZERO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books Histograms for signed residual and
C-                         for fine adjustment of DT0, for every
C-                         module.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   6-DEC-1991   Eric James
C-   DH 4/92 ADD SOME HISTOGRAMS
C     DH 8/92 get rid of mobo stuff
C     PQ 8/92 add histograms of dt0,dtslope and overall resolutions
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*10 CMODU
      CHARACTER*26 DTTIT
      INTEGER MODNUM, MUNMOD3, NMODU, ID, ID2, I, DUM, K, KID
C
      CALL HBOOK1(10000,' DT 3-MISS OE SIG (cm)/MOD ',40,0.,100.,0.)
      CALL HBOOK1(10001,' OLD DT0-1 ',40,1400.,2200.,0.)
      CALL HBOOK1(10002,' OLD DT0-2 ',40,1400.,2200.,0.)
      CALL HBOOK1(10003,' OLD DTSLOPE-1 ',40,0.01,0.05,0.)
      CALL HBOOK1(10004,' OLD DTSLOPE-2 ',40,0.01,0.05,0.)
      CALL HBOOK1(10005,' DT0 ADJUSTMENTS ',40,-100.,100.,0.)
      CALL HBOOK1(10006,' DTSLOPE FACTORS ',40,0.8,1.2,0.)
c
      DTTIT(1:26) = '3-MISS overall resolution'
      CALL HBOOK1(11000,DTTIT,50,-150.,150.,0.)
      DTTIT(1:26) = 'DT0-PAD center ALL EVEN'
      CALL HBOOK1(12000,DTTIT,40,-100.,100.,0.)
      DTTIT(1:26) = 'DT0-PAD far end ALL EVEN'
      CALL HBOOK1(13000,DTTIT,40,-100.,100.,0.)
      DTTIT(1:26) = 'DT0-PAD center ALL ODD'
      CALL HBOOK1(14000,DTTIT,40,-100.,100.,0.)
      DTTIT(1:26) = 'DT0-PAD far end all ODD'
      CALL HBOOK1(15000,DTTIT,40,-100.,100.,0.)
      DTTIT(1:26) = 'DT-PAD overall resolution'
      CALL HBOOK1(16000,DTTIT,40,-100.,100.,0.)
      DTTIT(1:26) = 'DT0-PAD elec end ALL MODS'
      CALL HBOOK1(17000,DTTIT,40,-100.,100.,0.)
      DTTIT(1:26) = 'DT0-PAD center ALL MODS'
      CALL HBOOK1(18000,DTTIT,40,-100.,100.,0.)
      DTTIT(1:26) = 'DT0-PAD far end ALL MODS'
      CALL HBOOK1(19000,DTTIT,40,-100.,100.,0.)
C
      NMODU = MUNMOD3(0,DUM)                   ! number of modules
      DO  I = 1,NMODU                         ! loop over modules
        MODNUM = MUNMOD3(1,I)
        ID = MODNUM
        WRITE(CMODU,101) MODNUM
C
        DTTIT(1:26) = 'DT0s OFFSETS  '//CMODU
        CALL HBOOK1(10000+ID,DTTIT,50,-100.,100.,0.)
        DTTIT(1:26) = 'DT0 3-MISS(cm) '//CMODU
        CALL HBOOK1(11000+ID,DTTIT,50,-150.,150.,0.)
        DTTIT(1:26) = 'DT0-PAD center E'//CMODU
        CALL HBOOK1(12000+ID,DTTIT,40,-100.,100.,0.)
        DTTIT(1:26) = 'DT0-PAD high end E'//CMODU
        CALL HBOOK1(13000+ID,DTTIT,40,-100.,100.,0.)
        DTTIT(1:26) = 'DT0-PAD center O'//CMODU
        CALL HBOOK1(14000+ID,DTTIT,40,-100.,100.,0.)
        DTTIT(1:26) = 'DT0-PAD high end O'//CMODU
        CALL HBOOK1(15000+ID,DTTIT,40,-100.,100.,0.)
        DTTIT(1:26) = 'TIME DIVISION(CM) '//CMODU
        CALL HBOOK1(16000+ID,DTTIT,40,-400.,400.,0.)
        DTTIT(1:26) = 'DT0-PAD low end '//CMODU
        CALL HBOOK1(17000+ID,DTTIT,40,-100.,100.,0.)
        DTTIT(1:26) = 'DT0-PAD center '//CMODU
        CALL HBOOK1(18000+ID,DTTIT,40,-100.,100.,0.)
        DTTIT(1:26) = 'DT0-PAD high end '//CMODU
        CALL HBOOK1(19000+ID,DTTIT,40,-100.,100.,0.)
      ENDDO
C
  101 FORMAT(I5)
  102 FORMAT(I5,I2)
  999 RETURN
      END
