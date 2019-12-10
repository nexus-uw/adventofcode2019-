program solution
    integer :: result1
    logical :: work1, work2

    result1 = 0

    ! do n = 111122,111122 ! -> true
    ! do n = 123444,123444 ! -> false
    ! do n = 112233,112233 ! -> true
    !  do n = 112222,112222 ! -> true
    !  do n = 122222,122222 ! -> false
    !  do n = 222223,222223 ! -> false
    do n = 153517,630395
      if (work2(n,99,.false.,0)) then
        result1 = result1 + 1
      end if
    end do
    print *, 'result: ',result1

end program solution
!part
recursive function work2(num, previous,seenPair, seenCount) result (result)

  integer, intent(in) :: num, previous, seenCount
  logical , intent(in) :: seenPair
  logical ::result, digitsDescreasing,seenPairStillValid
  integer::digit, newSeenCount

  seenPairStillValid = seenPair

  digit = mod(num,10)

  digitsDescreasing = (digit <= previous)

  if (digit == previous)then
    newSeenCount = seenCount + 1
  else
    if (seenCount == 2) then
      seenPairStillValid = .true.
    end if
    newSeenCount = 1
  end if

  if (num >= 10) then
    result = digitsDescreasing .and. work2(int(num / 10), digit, seenPairStillValid,newSeenCount )
  else
    result = digitsDescreasing .and. (seenPairStillValid .or.newSeenCount == 2)
  end if
end function work2

! part 1
recursive function work1(num, previous, seenPair) result (result)

  integer, intent(in) :: num, previous
  logical , intent(in) :: seenPair
  logical ::result, digitsDescreasing,seenPairStillValid
  integer::digit

  digit = mod(num,10)

  digitsDescreasing = (digit <= previous)
  seenPairStillValid = seenPair .or. digit == previous

  if (num >= 10) then
    result = digitsDescreasing .and. work1(int(num / 10), digit, seenPairStillValid )
  else
    result = digitsDescreasing .and. seenPairStillValid
  end if
end function work1