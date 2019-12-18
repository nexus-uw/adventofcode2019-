
program solution
    integer::work
    integer::workOutput,signalIndex
    integer, dimension(119) :: signals
    ! see index.js for calculation of this list
    signals = (/01234,01243,01324,01342,01423,01432,02134,02143,02314,02341,02413,02431,03124,&
03142,&
03214,&
03241,&
03412,&
03421,&
04123,&
04132,&
04213,&
04231,&
04312,&
04321,&
10234,&
10243,&
10324,&
10342,&
10423,&
10432,&
12034,&
12043,&
12304,&
12340,&
12403,&
12430,&
13024,&
13042,&
13204,&
13240,&
13402,&
13420,&
14023,&
14032,&
14203,&
14230,&
14302,&
14320,&
20134,&
20143,&
20314,&
20341,&
20413,&
20431,&
21034,&
21043,&
21304,&
21340,&
21403,&
21430,&
23014,&
23041,&
23104,&
23140,&
23401,&
23410,&
24013,&
24031,&
24103,&
24130,&
24301,&
24310,&
30124,&
30142,&
30214,&
30241,&
30412,&
30421,&
31024,&
31042,&
31204,&
31240,&
31402,&
31420,&
32014,&
32041,&
32104,&
32140,&
32401,&
32410,&
34012,&
34021,&
34102,&
34120,&
34201,&
34210,&
40123,&
40132,&
40213,&
40231,&
40312,&
40321,&
41023,&
41032,&
41203,&
41230,&
41302,&
41320,&
42013,&
42031,&
42103,&
42130,&
42301,&
42310,&
43012,&
43021,&
43102,&
43120,&
43201/)
    do signalIndex = 1, 119
        workOutput = 0
        do n = 0, 4
            workOutput = work(n,workOutput)
        end do
    end do
end program solution

function work(phaseSetting, intputValue) result (outputValue)
   integer :: phaseSetting, intputValue, readCount
   integer :: outputValue

   integer, dimension(503) :: input
   integer :: inputLength, de, a, b,c, desitionation, index, param, val1, val2
   inputLength = 503

   input = (/ 3,8,1001,8,10,8,105,1,0,0,21,34,51,68,89,98,179,260,341,422,99999,3,9,1001,9,&
   4,9,102,4,9,9,4,9,99,3,9,1002,9,5,9,1001,9,2,9,1002,9,2,9,4,9,99,3,9,1001,9,3,9,102,3,9,&
   9,101,4,9,9,4,9,99,3,9,102,2,9,9,101,2,9,9,1002,9,5,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,4,&
   9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,&
   9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,&
   9,1001,9,1,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,&
   9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,&
   1,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,&
   3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,&
   9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,&
   1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,&
   4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,&
   9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,&
   1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99 /)

    index = 1
    readCount = 0
    !inputValue = 1 !part 1
    loop: do while (index <= inputLength)

        param = input(index) ! in the form abcde
        de = mod(param, 100)
        c = mod(int(param / 100),10)
        b = mod(int(param / 1000),100)
        a = mod(int(param / 10000),1000)
        ! Print *, param,a,b,c,de
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
                if (readCount == 0) then
                    input(desitionation) = phaseSetting
                else if(readCount == 1)then
                    input(desitionation) = intputValue
                else
                    Print *, 'too many read attemps....'
                end if
                readCount = readCount + 1
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
    ! print *, outputValue
end function work
