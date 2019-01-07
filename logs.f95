SUBROUTINE calcLOGjclark (DS,DL,TL,KERF,V)
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
! THIS SUBROUTINE WAS WRITTEN BY J.E.BRICKELL OF THE U.S.FOREST SERVICE
! TO CALCULATE BOARD FOOT VOLUME OF SAWLOGS BY THE INTERNATIONAL RULE.
! VARIABLES IN THE CALLING SEQUENCE ARE:
! DS = LOG’S SCALING DIAMETER (INCHES)
! DL = DIB AT LOG’S LARGE END (INCHES) (0.0 IF 1/2 INCH TAPER)
! TL = TOTAL LOG LENGTH (FEET)
! KERF >0 IF KERF ASSUMPTION IS 1/4 INCH
! KERF <0, OR = 0, IF KERF ASSUMPTION IS 1/8 INCH
! V = LOG VOLUME RETURNED TO THE CALLING PROGRAM
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
 V=0.0
! IF TOTAL LOG LENGTH IS LESS THAN FOUR FEET NO BOARD FOOT VOLUME WILL BE
! COMPUTED.
 IF(TL-4.0 < 0)then
 RETURN
 end if
! IF THE LOG’S LARGE END DIAMETER IS FURNISHED TO JCLARK A TAPER RATE
! WILL BE COMPUTED. IF DL=0 THE STANDARD ASSUMPTION OF 1/2 INCH PER 4
! FEET OF LOG LENGTH WILL BE USED.
 IF(DL > 0)then
    T=4.0*(DL-DS)/TL
 else 
    T=0.5
 end if 
! THE FOLLOWING LOOP (THROUGH STATEMENT 5) FINDS OUT HOW MANY FULL 4
! FOOT SEGMENTS THE LOG CONTAINS.
 do I=1,20
    IF(TL-FLOAT(4*I) < 0) exit
 end do
 L=I-1
 SL=FLOAT(4*L)
! THE FOLLOWING STATEMENT MOVES THE SCALING DIAMETER DOWN TO THE END OF
! THE 4 FOOT SEGMENTS AND INCREASES IT ACCORDING TO TAPER.
 D=DS+(T/4.0)*(TL-SL)
! THE FOLLOWING LOOP (THROUGH STATEMENT 7) FINDS OUT HOW MANY FULL FEET
! OF LENGTH ARE IN THE SEGMENT LESS THAN 4 FEET LONG.
 do I=1,4
    XI=FLOAT(I)
    IF(SL-TL+XI > 0) exit
 end do
! THE NEXT THREE STATEMENTS CALCULATE LOG VOLUME IN THE 1, 2, OR 3 FOOT
! SEGMENT AT THE SMALL END OF THE LOG.
 XL=XI-1.0
 DEX=DS+(T/4.0)*(TL-SL-XL)
 VADD=0.055*XL*DEX*DEX-0.1775*XL*DEX
! THE FOLLOWING LOOP (THROUGH 9) CALCULATES VOLUME IN THE PORTION OF
! THE LOG CONTAINING WHOLE 4 FOOT SEGMENTS.
 do I=1,L
    DC=D+T*FLOAT(I-1)
    V=V+0.22*DC*DC-0.71*DC
 end do
 V=V+VADD
! IF ‘KERF’ IS GREATER THAN ZERO, INTERNATIONAL 1/8 INCH VOLUME AS
! COMPUTED ABOVE WILL BE CONVERTED TO INTERNATIONAL 1/4 INCH VOLUME.
 IF (KERF <=0)RETURN
 V=0.905*V
 RETURN
 end subroutine calcLOGjclark


real function calcLOGvolume(DS,DL,TL)
    implicit none
    real, intent(in) :: DS,DL,TL
    real :: pi=3.14159, A1=0.0, A2=0.0, Mds=0.0, Mdl=0.0, Mtl=0.0, CurDL=0.0
    CurDL=DL
    if (DL <= 0) then
       CurDL = DS + (5*.5)
    endif
    !conversion to metric units
    !in
    Mds = (DS / 39.37) / 2.0
    Mdl = (CurDL / 39.37) / 2.0
    !ft
    Mtl = TL / 3.2808
    A1 = pi * Mds ** 2
    A2 = pi * Mdl ** 2
    calcLOGvolume = ((A2 + A1) / 2.0) * Mtl
end function calcLOGvolume

subroutine getLogData(DS, DL, TL, KERF)
    implicit none
    real, intent(inout) :: DS, DL, TL
    integer, intent(inout) :: KERF
    real :: msrmnt
    write(*,*)'to exit press ctrl+c'
    write(*,*)'please input your values(unit of measurement)(input method)'
    write(*,*)'Diameter inside Bark Small(in)(decimal):'
    read(*,*)DS
    write(*,*)'Diameter inside Bark Large(in)(decimal):'
    read(*,*)DL
    write(*,*)'Total Length(ft)(decimal):'
    read(*,*)TL
    write(*,*)'KERF(in)(decimal):'
    read(*,*)msrmnt
    KERF = 1
    if(msrmnt < .25)KERF = 0
    
end subroutine getLogData

program Log
   real :: DS=20.0, DL=0.0, TL=20.0, V=0
   integer :: KERF=1
   do
      call getLogData(DS, DL, TL, KERF)
      call calcLOGjclark(DS, DL, TL, KERF, V)
      !V is board feet
      trueV = calcLOGvolume(DS, DL, TL)
      write(*,*)'board feet:',V, 'volume output:',trueV
   end do
   !convert to volume in meters cubed

end program Log