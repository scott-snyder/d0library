C	============================================
C	Global Read-only Event Buffer for DAQ System
C	============================================
C
C	Modulename:-
C	============
C	DAQ$LIBRARY:EVENTBUF.INC
C
C	Description:-
C	=============
C	This Include File contains the Global Event Buffer 
C	Declaration used by Consumer Processes.
C
C	Creation Date :  1 Mar 1983
C	===========================
C
      Common/DAQ_EVENT/DAQ_EVENT
      Integer*4        DAQ_EVENT
      Dimension        DAQ_EVENT(0:1)
C
      Integer*2        DAQ_EVENT_I2
      Dimension        DAQ_EVENT_I2(0:1)
C
      Equivalence     (DAQ_EVENT_I2(0), DAQ_EVENT(0))
C
