      SUBROUTINE HSTRFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      This is a DUMMY in ZEBRA_UTIL 
C-      For production programs HSTRFL is build by PROGRAM_BUILDER
C-
C-   Created   3-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LHSTR
      INTEGER CREATION_TIME,OFTIM,PRODID,VERSION,PASS
      CHARACTER*40 PROGRAM_NAME
      CHARACTER*8 CREATION_SITE
      CHARACTER*8 PROCESSING_SITE
      CHARACTER*8 GTSITE
      PARAMETER( CREATION_TIME = 0 )
      PARAMETER( PRODID = 0 )
      PARAMETER( VERSION = 0 )
      PARAMETER( PASS = 0 )
      PARAMETER( PROGRAM_NAME = 'NOT_PRODUCTION' )
      PARAMETER( CREATION_SITE = 'UNDEFIND' )
      INTEGER OFFTIM
      EXTERNAL OFFTIM,GTSITE
C----------------------------------------------------------------------
C
      CALL BKHSTR(LHSTR)
      IQ(LHSTR+1)=1
      IQ(LHSTR+2)=PRODID
      IQ(LHSTR+3)=VERSION
      IQ(LHSTR+4)=PASS
      IQ(LHSTR+5)=CREATION_TIME
      IQ(LHSTR+6)=OFFTIM()
      CALL UCTOH(PROGRAM_NAME,Q(LHSTR+7),40,40)
      CALL UCTOH(CREATION_SITE,Q(LHSTR+17),8,8)
      PROCESSING_SITE = GTSITE()
      CALL UCTOH(PROCESSING_SITE,Q(LHSTR+19),8,8)
  999 RETURN
      END
