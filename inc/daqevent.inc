C	==================================
C	Global Event Buffer for DAQ System
C	==================================
C
C	Modulename:-
C	============
C	DAQ$CMS:DAQEVENT.INC
C
C	Description:-
C	=============
C	This Include File contains the Global Event Buffer Common 
C	Block Definition. The size of this Buffer is determined by the
C	DAQ_EVENT_BUFFER_SIZE Parameter defined in the DAQCONFIG.INC
C	Include File.
C
C	Creation Date :  8 Mar 1983
C	===========================
C
      Parameter EVENT_PAGE_SIZE = 128*((DAQ_EVENT_BUFFER_SIZE+127)/128)
      Common/DAQ_EVENT/DAQ_EVENT(0:EVENT_PAGE_SIZE-1)
      Integer*4        DAQ_EVENT
C
