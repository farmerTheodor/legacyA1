SUBROUTINE calcLOGjclark (diameterSmall,diameterLarge,totalLength,KERF,volume)
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
! THIS SUBROUTINE WAS WRITTEN BY J.E.BRICKELL OF THE U.S.FOREST SERVICE
! TO CALCULATE BOARD FOOT VOLUME OF SAWLOGS BY THE INTERNATIONAL RULE.
! VARIABLES IN THE CALLING SEQUENCE ARE:
! diameterSmall = LOG’S SCALING DIAMETER (INCHES)
! diameterLarge = DIB AT LOG’S LARGE END (INCHES) (0.0 IF 1/2 INCH TAPER)
! totalLength = TOTAL LOG LENGTH (FEET)
! KERF >0 IF KERF ASSUMPTION IS 1/4 INCH
! KERF <0, OR = 0, IF KERF ASSUMPTION IS 1/8 INCH
! volume = LOG VOLUME RETURNED TO THE CALLING PROGRAM
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
 
 volume=0.0

! IF TOTAL LOG LENGTH IS LESS THAN FOUR FEET NO BOARD FOOT VOLUME WILL BE
! COMPUTED.
 if(totalLength-4.0 < 0)then
 return
 end if

! IF THE LOG’S LARGE END DIAMETER IS FURNISHED TO JCLARK A TAPER RATE
! WILL BE COMPUTED. IF diameterLarge=0 THE STANDARD ASSUMPTION OF 1/2 INCH PER 4
! FEET OF LOG LENGTH WILL BE USED.
 if(diameterLarge > 0)then
    taperRate=4.0*(diameterLarge-diameterSmall)/totalLength
 else 
    taperRate=0.5
 end if 

! THE FOLLOWING LOOP (THROUGH STATEMENT 5) FINDS OUT HOW MANY FULL 4
! FOOT SEGMENTS THE LOG CONTAINS.
 do I=1,20
    IF(totalLength-FLOAT(4*I) < 0) exit
 end do
 numSegments=I-1
 segmentedLength=FLOAT(4*numSegments)

! THE FOLLOWING STATEMENT MOVES THE SCALING DIAMETER DOWN TO THE END OF
! THE 4 FOOT SEGMENTS AND INCREASES IT ACCORDING TO TAPER.
 diameter=diameterSmall+(taperRate/4.0)*(totalLength-segmentedLength)

! THE FOLLOWING LOOP (THROUGH STATEMENT 7) FINDS OUT HOW MANY FULL FEET
! OF LENGTH ARE IN THE SEGMENT LESS THAN 4 FEET LONG.
 do I=1,4
    extraLength=FLOAT(I)
    IF(segmentedLength-totalLength+extraLength > 0) exit
 end do

! THE NEXT THREE STATEMENTS CALCULATE LOG VOLUME IN THE 1, 2, OR 3 FOOT
! SEGMENT AT THE SMALL END OF THE LOG.
 extraLength=extraLength-1.0
 diameterInsideExtra=diameterSmall+(taperRate/4.0)*(totalLength-segmentedLength-extraLength)
 volumeAdditional=0.055*extraLength*diameterInsideExtra*diameterInsideExtra-0.1775*extraLength*diameterInsideExtra

! THE FOLLOWING LOOP (THROUGH 9) CALCULATES VOLUME IN THE PORTION OF
! THE LOG CONTAINING WHOLE 4 FOOT SEGMENTS.
 do I=1,numSegments
    diameterInside=diameter+taperRate*FLOAT(I-1)
    volume=volume+0.22*diameterInside*diameterInside-0.71*diameterInside
 end do
 volume=volume+volumeAdditional

! IF ‘KERF’ IS GREATER THAN ZERO, INTERNATIONAL 1/8 INCH VOLUME AS
! COMPUTED ABOVE WILL BE CONVERTED TO INTERNATIONAL 1/4 INCH VOLUME.
 if (KERF <=0)return
 volume=0.905*volume
 return
 end subroutine calcLOGjclark


real function calcLOGvolume(diameterSmall,diameterLarge,totalLength)
    implicit none
    real, intent(in) :: diameterSmall,diameterLarge,totalLength
    
    !the 'm' in the variable names stand for metric
    real :: A1=0.0, A2=0.0, mDiameterSmall=0.0, mDiameterLarge=0.0, mTotalLength=0.0, curDiameterLong=0.0
    real, parameter :: pi=3.14159

    !if a value was not given for diameter large then we estimate it here
    curDiameterLong=diameterLarge
    if (diameterLarge <= 0) then
       curDiameterLong = diameterSmall + (5*.5)
    endif
    
    !conversion to metric units
    !in inches
    mDiameterSmall = (diameterSmall / 39.37) / 2.0
    mDiameterLarge = (curDiameterLong / 39.37) / 2.0
    
    !in feet
    mTotalLength = totalLength / 3.2808
    
    !calculate area of log ends
    A1 = pi * mDiameterSmall ** 2
    A2 = pi * mDiameterLarge ** 2
    
    !perform smalians formula
    calcLOGvolume = ((A2 + A1) / 2.0) * mTotalLength
end function calcLOGvolume

subroutine getLogData(diameterSmall, diameterLarge, totalLength, KERF)
    implicit none
    real, intent(inout) :: diameterSmall, diameterLarge, totalLength
    integer, intent(inout) :: KERF
    real :: msrmnt
    write(*,*)'(to exit press ctrl+c)'
    write(*,*)'please input your values(unit of measurement)(input method)'
    write(*,*)'Diameter inside Bark Small(in)(decimal):'
    read(*,*)diameterSmall
    write(*,*)'Diameter inside Bark Large(in)(decimal):'
    read(*,*)diameterLarge
    write(*,*)'Total Length(ft)(decimal):'
    read(*,*)totalLength
    write(*,*)'KERF(in)(decimal):'
    read(*,*)msrmnt

    !determine if 1/4" or 1/8 kerf is used(1 for 1/4, 0 for 1/8)
    KERF = 1
    if(msrmnt < .25)KERF = 0
    
end subroutine getLogData

program Log
   real :: diameterSmall=20.0, diameterLarge=0.0, totalLength=20.0, volume=0
   integer :: KERF=1
   do
      !get input
      call getLogData(diameterSmall, diameterLarge, totalLength, KERF)

      !calculate output
      call calcLOGjclark(diameterSmall, diameterLarge, totalLength, KERF, volume)
      trueV = calcLOGvolume(diameterSmall, diameterLarge, totalLength)

      !show output
      write(*,*)'board feet:',volume, 'volume output(meters cubed):',trueV
   end do
end program Log