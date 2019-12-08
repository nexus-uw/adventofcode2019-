
program solution
    integer :: result1,noun,verb
    call work(12,2,result1)
    print *, 'part 1 result: ',result1
    fidlar: do noun = 0, 100
        do verb = 0, 100
            call work(noun,verb,result1)

            if (result1 == 19690720) then
                print *, 'part 2 result:', (100*noun + verb)
                exit fidlar
            end if
        end do
    end do fidlar

end program solution

subroutine work(noun,verb,result)
    integer, dimension(165) :: input
    integer :: desitionation, noun, verb, result

    ! https://stackoverflow.com/questions/34194589/line-truncated-syntax-error-in-argument-list
    input = (/ 1,0,0,3,&
    1,1,2,3&
    ,1,3,4,3,1,5,0,3,2,1,10,19,1,9,19,23,1,13,23,27,1,5,&
    27,31,2,31,6,35,1,35,5,39,1,9,39,43,1,43,5,47,1,47,5,51,2,10,51,55,1,5,55,59,&
    1,59,5,63,2,63,9,67,1,67,5,71,2,9,71,75,1,75,5,79,1,10,79,83,1,83,10,87,1,10,&
    87,91,1,6,91,95,2,95,6,99,2,99,9,103,1,103,6,107,1,13,107,111,1,13,111,115,2,&
    115,9,119,1,119,6,123,2,9,123,127,1,127,5,131,1,131,5,135,1,135,5,139,2,10,&
    139,143,2,143,10,147,1,147,5,151,1,151,2,155,1,155,13,0,99,2,14,0,0 /)
    input(2) = noun
    input(3) = verb


    loop: do index=1,size(input) ,4


        command = input(index)
        a = input(input(index+1)+1)
        b = input(input(index+2)+1)
        desitionation = input(index+3)+1
        if (command == 99) then
            exit loop ! labelled loop breaks supper cool. eat poopies typescript
        else if (command == 1) then
            input(desitionation) = int(a + b)
        else if (command == 2) then
            input(desitionation) = int(a * b)
        else
            Print *, 'shit'
        end if
    end do loop
    result = input(1)
end subroutine work
