      SUBROUTINE PRVTXH(PRUNIT,KVTXH,NVTXH,CFL,IFL)
C------------------------------------------------------------------
C-
C-  Print out  VTXH (Vertex Chamber hits) bank
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  KVTXH = bank address 
C-  NVTXH = bank number  (not used)
C-  CFL   = 'ONE': print bank VTXH
C-          'ALL': same, but finds LVTXH
C-           All other values act the same as 'ONE'
C-          
C-  IFL   = 0: no printout
C-        >=1: prints number of hits in VTX
C-        >=3: full printout
C-
C-         D.Z. JAN.,1987                    
C-    10-OCT-1988  Tom Trippe - modified to add words to bank
C-    11-JAN-1989  Peter Grudberg - added two words for z-strips
C-    25-JUN-1989  Peter Grudberg - implement IFL and CFL
C-------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'
      INTEGER PRUNIT,KVTXH,NVTXH,IFL
      INTEGER NHITS,NHWIR,NHSTR,NWDPVH,NWIPSC,NWDPDA,NFAPSC    
      INTEGER NWVZLA, NWVZDA, GZVTXH
      CHARACTER CFL*(*)
C-------------------------------------------------------------------
      IF ( IFL .LE. 0 ) GO TO 999
      LVTXH=GZVTXH()
      IF (LVTXH.LE.0) THEN
        WRITE(PRUNIT,1011) LVTXH
        GO TO 999
      END IF
C
C **** Number of hits in VTX: IFL > 0
C
      NHITS=IQ(LVTXH+1)    ! number of hits  in Vertex Chamber
      NHWIR=IQ(LVTXH+2)    ! number of wire  hits in Vertex Chamber
      NHSTR=IQ(LVTXH+3)    ! number of strip hits in Vertex Chamber
      WRITE ( PRUNIT, 1000 ) NHITS, NHWIR, NHSTR
C
C **** Full printout: IFL > 2
C
      IF ( IFL .LE. 2 ) GO TO 999
      NWDPVH=IQ(LVTXH+4)   ! number of words per VSEC hit           
      NWIPSC=IQ(LVTXH+5)   ! number of wires per sector             
      NWDPDA=IQ(LVTXH+6)   ! number of words per hit in VWDA    
      NFAPSC=IQ(LVTXH+7)   ! number of FADC channels per sector 
      NWVZLA=IQ(LVTXH+8)   ! number of words per VZLA hit
      NWVZDA=IQ(LVTXH+9)   ! number of words per VZDA hit
      WRITE ( PRUNIT, 1010 ) NWDPVH, NWIPSC, NWDPDA, 
     &                       NFAPSC, NWVZLA, NWVZDA
C
 1000 FORMAT(/,' ********** Hit bank for Vertex Chamber VTXH',/,
     $ ' No. of hits in VTX:  Total/wire_hits/strip_hits =',3I6)
 1010 FORMAT(' No. of words/VSEC hit =',I3,' No. of wires/sector =',I2/
     $' No. of words/VWDA hit =',I3,'  No. of FADC chan./sector =',I3/
     $' No. of words/VZLA hit =',I3,'  No. of words/VZDA hit =',I3) 
 1011 FORMAT(/' No VTX hits, LVTXH =',I10)
  999 CONTINUE
      RETURN 
      END
