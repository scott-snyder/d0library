      PROGRAM TRWSTP
C-------------------------------------------------------------------------------
C-                                                             
C-    Initialize Zebra for online programs                     
C-    data is in first division of primary store               
C-                                                             
C-    /ZEBCOM/ is the common block for data Zebra banks        
C-    /ZEBSTP/ is the common block for constants Zebra banks   
C-     AZ   Nov.13, 1986                                       
C-     SDP  Oct.  , 1986                                       
C-     B MANSOULIE Nov 1987                                    
C-     Updated BM  11-Mar-1988 change to ZEBSTP VER 0.2        
C-     Updated     FEB-1992   Alain PLUQUET simplifications    
C-     Updated  30-JUN-1992   Alain PLUQUET use exchange mode  
C-------------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LUN,ILEN
      CHARACTER*12 XCHOP
      LOGICAL OK
      DATA LUN/9/
c-------------------------------------------------------------------------------
c   initalization in /ZEBSTP/ for constants (store 1)
c-------------------------------------------------------------------------------
      CALL MZEBRA (0)
      IXSTP=1
      CALL MZSTOR (IXSTP,'/ZEBSTP/','C',FENSTP,LSTPH,LSLV0,ZCONS,
     +   ZCONS(20000),ENDZC)
      IDVSTP=IXSTP+2    ! IDVSTP is the second division in the ZEBSTP store
      CALL TSTPBK
      CALL DZSTOR(' IXSTP',IXSTP)
      CALL TSTTYP
      CALL D0OPEN(LUN,'TRD_STPFILE','OG',OK)
      CALL XZRECL(ILEN,XCHOP)
      CALL FZFILE(LUN,ILEN,XCHOP)
      CALL FZOUT(LUN,IDVSTP,LSTPH,1,'D',0,0,0)
      CALL FZENDO (LUN,'T')
      END

