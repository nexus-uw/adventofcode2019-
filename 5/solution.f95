
program solution
   integer, dimension(678) :: input
   integer :: inputLength, de, a, b,c, desitionation, inputValue, outputValue, index, param, val1, val2
   inputLength = 678

   input = (/ 3,225,1,225,6,6,1100,1,238,225,104,0,1102,78,40,225,1102,52,43,224,1001,224,-2236,224,4,224,102,8,223,223,101,4,224,&
   224,1,224,223,223,1,191,61,224,1001,224,-131,224,4,224,102,8,223,223,101,4,224,224,1,223,224,223,1101,86,74,225,1102,14,76,225&
   ,1101,73,83,224,101,-156,224,224,4,224,102,8,223,223,101,6,224,224,1,224,223,223,1102,43,82,225,2,196,13,224,101,-6162,224,224&
   ,4,224,102,8,223,223,101,5,224,224,1,223,224,223,1001,161,51,224,101,-70,224,224,4,224,102,8,223,223,1001,224,1,224,1,224,223&
   ,223,102,52,187,224,1001,224,-832,224,4,224,102,8,223,223,101,1,224,224,1,224,223,223,1102,19,79,225,101,65,92,224,1001,224,&
   -147,224,4,224,1002,223,8,223,101,4,224,224,1,223,224,223,1102,16,90,225,1102,45,44,225,1102,92,79,225,1002,65,34,224,101,&
   -476,224,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,&
   247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,&
   1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314&
   ,0,0,106,0,0,1105,1,99999,107,226,226,224,1002,223,2,223,1005,224,329,1001,223,1,223,1007,226,226,224,102,2,223,223,1005,224,344&
   ,101,1,223,223,1008,226,226,224,102,2,223,223,1005,224,359,1001,223,1,223,8,226,677,224,102,2,223,223,1006,224,374,101,1,223,&
   223,1107,226,677,224,1002,223,2,223,1006,224,389,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,404,101,1,223,223,107,&
   677,677,224,102,2,223,223,1006,224,419,1001,223,1,223,7,677,226,224,102,2,223,223,1005,224,434,101,1,223,223,1007,677,677,224,&
   102,2,223,223,1005,224,449,1001,223,1,223,108,226,677,224,102,2,223,223,1005,224,464,1001,223,1,223,108,226,226,224,102,2,223,&
   223,1006,224,479,101,1,223,223,107,226,677,224,102,2,223,223,1006,224,494,1001,223,1,223,7,226,226,224,1002,223,2,223,1006,224&
   ,509,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,524,101,1,223,223,1107,677,226,224,102,2,223,223,1005,224,539,101,1&
   ,223,223,1008,677,226,224,102,2,223,223,1005,224,554,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,569,101,1,223,223,&
   1107,677,677,224,102,2,223,223,1006,224,584,1001,223,1,223,1108,226,226,224,1002,223,2,223,1006,224,599,101,1,223,223,7,226,677&
   ,224,102,2,223,223,1006,224,614,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,629,101,1,223,223,1007,677,226,224,102,2&
   ,223,223,1006,224,644,101,1,223,223,8,677,677,224,1002,223,2,223,1006,224,659,101,1,223,223,8,677,226,224,102,2,223,223,1005,&
   224,674,101,1,223,223,4,223,99,226 /)

    index = 1
    !inputValue = 1 !part 1
    inputValue = 5 !part 2
    loop: do while (index <= inputLength)

        param = input(index) ! in the form abcde
        de = mod(param, 100)
        c = mod(int(param / 100),10)
        b = mod(int(param / 1000),100)
        a = mod(int(param / 10000),1000)
        Print *, param,a,b,c,de
        if (c == 0)then
            val1 = input(input(index + 1)+1)
        else
            val1 = input(index + 1)
        end if

        if (b == 0)then
            val2 = input(input(index + 2)+1)
        else
            val2 = input(index + 2)
        end if

        if (a == 0)then
            desitionation = input(index + 3)+1
        else
            ! this should never happen
            desitionation = index + 3
        end if

        select case (de)
        case (1:2)
            ! add/multiply


            if (de == 1) then
                input(desitionation) = int(val1 + val2)
            else
                input(desitionation) = int(val1 * val2)
            end if
            index = index + 4
        case (3:4)
            ! input/output

            desitionation = input(index+1)+1
            if (de == 3) then
                input(desitionation) = inputValue
            else
                outputValue = input(desitionation)
            end if
            index = index + 2

        !part 2 stuff
        case (5)

            ! jump-if-true
            if(val1 /= 0) then
                index = val2 +1
            else
                index = index + 3
            end if
        case (6)


            ! jump-if-false
            if(val1 == 0) then
                index = val2 +1
            else
                index = index + 3
            end if
        case (7)


            ! less than
            if(val1 < val2) then
                input(desitionation) = 1
            else
                input(desitionation) = 0
            end if
            index = index + 4
        case (8)

            ! equals
            if(val1 == val2) then
                input(desitionation) = 1
            else
                input(desitionation) = 0
            end if
            index = index + 4
        case (99)
            ! end program
            exit loop
        case default
            Print *, 'unexpected op code',param,'at',index
            exit loop
        end select
    end do loop
    print *, outputValue
end program solution
