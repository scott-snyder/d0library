      SUBROUTINE DMPINI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Initialize DUMP facility
C-
C-   ENTRY DMPFLG(L,VALUE)
C-     set dump flag L to VALUE
C-
C-   Input:
C-   L    = number of flag
C-   VALUE= logical value for flag
C-
C-   Created  15-SEP-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,L
      LOGICAL VALUE
      INCLUDE 'D0$INC:DUMP.INC'
      CHARACTER*16 DFLAGS(NDFL)
C
      DATA DFLAGS/
     &  'DUMP_NONE_F','DUMP_PROCES','DUMP_USER','DUMP_ALL_F',
     &  'DUMP_NONE_H','DUMP_ALL_H','DUMP_TRGR','DUMP_MUD1',
     &  'DUMP_CDD1','DUMP_CDD2','DUMP_CDD3','DUMP_CDD4',
     &  'DUMP_CAD1','DUMP_CAD2','DUMP_SCREEN','DUMP_QPRINT',
     &  'DUMPF_REQ','DUMPH_REQ','DUMPU_REQ','DUMP_EVENT'/
C----------------------------------------------------------------------
C
C        reset all counters
      NDUMP=0
      NSKIP=0
      NFDONE=0
      NHDONE=0
C
      CALL FLGBK(DFLAGS,NDFL)
      DO 11 I=1,NDFL
   11   CALL FLGSET(DFLAGS(I),.FALSE.)
      CALL FLGSET(DFLAGS(2),.TRUE.)  ! set default to process dump
      CALL FLGSET(DFLAGS(5),.TRUE.)  ! set default to no raw dumps
C
  999 RETURN
C
C
      ENTRY DMPFLG(L,VALUE)
      CALL FLGSET(DFLAGS(L),VALUE)
      RETURN
      END
