      SUBROUTINE RZVIN1(VECT,NDIM,RNFILE,RKEY,ICYCLE,CHOPT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EMULATES RZVIN1
C-
C-   Inputs  : NDIM declared dimension of VECT
C-             RKEY  Keyword Vector of information to be read
C-             ICYCLE cycle number of Key to be read
C-                    > = highest number means highest number
C-                    =0 means read lowest cycle
C-             CHOPT  = Dummy
C-   Outputs : VECT Array contatining information
C-             RNFILE Actual length of array on the file
C-   Controls:
C-
C-   Created  10-APR-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    VECT(*)
      INTEGER NDIM,RNFILE,RKEY(*),ICYCLE
      CHARACTER*(*) CHOPT
      INCLUDE 'D0$INC:SHLCON.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER GET_CYCLE,HI_CYCLE
      INTEGER K,LREC,SREC
C
      INTEGER IOSTAT_READ
      CHARACTER*80 DIRECTORY,FILE
      INTEGER IOSTAT_OPEN
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      LOGICAL KEYED
      INTEGER IER
C
      INTEGER K1,K2,K3,ICYC
      INTEGER INDX_KEY
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('SHOWERLIBRARY_RCP')              ! select SHOWERLIB bank
        CALL EZGET('KEYED_ACCESS',KEYED,IER)
        CALL EZRSET
      ENDIF
C
      IQUEST(1) = 0
      HI_CYCLE = CYCLES(INDX_KEY(RKEY))
C
      GET_CYCLE = ICYCLE
C
      IF(ICYCLE.EQ.0)THEN
        GET_CYCLE = 1
      ELSEIF(ICYCLE.GE.HI_CYCLE)THEN
        GET_CYCLE = HI_CYCLE
      ENDIF
C
  123 CONTINUE
      IF ( KEYED ) THEN
        CALL PACK_REC(RKEY,GET_CYCLE,LREC)
        READ(KEY=LREC,UNIT=ISUNIT,ERR=997,IOSTAT=IOSTAT_READ)
     &  SREC,RNFILE,(VECT(K),K=1,RNFILE)
        IF(SREC.NE.LREC)THEN
          CALL ERRMSG('SHOWERLIBRARY','RZVIN1',
     &      'KEYS IN RETRIEVED FILE NOT THOSE ASKED FOR','W')
        ENDIF
      ENDIF
      IQUEST(6) = GET_CYCLE
      IQUEST(50) = HI_CYCLE
C
      RETURN
  998 CALL ERRMSG('SHOWERLIBRARY','RZVIN1',
     &  'ERROR OPENING FILE ','W')
      IQUEST(1) = 998
      GO TO 999
  997 CALL ERRMSG('SHOWERLIBRARY','RZVIN1',
     &  'ERROR IN READING RECORD ','W')
      IQUEST(1) = 997
  999 RETURN
      END
