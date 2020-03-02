!  Assignment1
!  Pradipta Dasgupta, CSCI 305


    program Assignment1

    implicit none

       integer :: money, money1
       integer :: lunker, loonter, little, pooney 
       integer :: lunker1, loonter1, little1, pooney1  
       integer :: dt(8)
       
    
       call DATE_AND_TIME(values=dt)
       print*,"mo","-","dy","-","year",",","Hr",":","mi"
       print '(i2.2,"-",i2.2,"-",i4.4,",",i2.2,":",i2.2)',dt(3),dt(2),dt(1),dt(5),dt(6)
    
     write(*,*) 'How much money do you have?'
      read *, money 
      money1 = money 
      
      if((money >= 1) .and. (money <= 99)) then
      call split(money, lunker, loonter, little, pooney)
      call split2(money, lunker1, loonter1, little1, pooney1)
      
    else
        write(*,*) 'Error! Wrong input!' 
        
        end if 
          
          
     contains
    
            subroutine split(money, lunker, loonter, little, pooney)
            
             integer :: mod1 
             integer :: mod2
             integer :: mod3
             integer :: mod4
             
            
            integer, intent(in) :: money
            integer, intent(out) :: lunker, loonter, little, pooney 
           
            lunker = money/30
            
           
            mod1 = mod(money, 30)
            
            loonter = (mod1)/15 
            
            mod2 =  mod(mod1, 15)
            
            little = (mod2)/5
            
            mod3 = mod(mod2, 5)
            
            pooney = (mod3)/1
            
            if(lunker > 0) then
            print *, 'Lunker(30):', lunker 
            end if
            
            if(loonter > 0) then
            print *, 'Loonter(15):', loonter
            end if 
            
            if(little > 0) then 
            print *, 'Little(5):', little
            end if 
            
            if(pooney > 0) then
            print *, 'Pooney(1):', pooney 
            end if
             
            
            
            
            
            
            end subroutine split 
            
            subroutine split2(money1, lunker1, loonter1, little1, pooney1)
            
             integer :: mod1 
             integer :: mod2
             integer :: mod3
             integer :: mod4
             integer :: n 
            
            integer, intent(in) :: money1
            integer, intent(out) :: lunker1, loonter1, little1, pooney1 
           
            lunker1 = money1/30
            
           
            mod1 = mod(money1, 30)
            
            loonter1 = (mod1)/15 
            
            mod2 =  mod(mod1, 15)
            
            little1 = (mod2)/5
            
            mod3 = mod(mod2, 5)
            
            pooney1 = (mod3)/1
            
            if(lunker1 > 0) then 
            write(*, 1 , advance = 'no') "Lunker(30):",  lunker1
            1 format(a11, i2)                 
            end if 
            
            if(loonter1 > 0) then
                write(*, 4, advance = 'no') "Loonter(15):", loonter1
            4 format (a12, i2)                               
            end if 
            
            if(little1 > 0) then
                write(*, 1, advance = 'no') "Little(5):", little1
                2 format (a12, i2)
            end if
            
            if(pooney1 > 0) then
                write(*, 1, advance = 'no') "Pooney(1):", pooney1
                3 format (a12, i2)
                end if
            
           end subroutine split2 
            
            
            
            
       
    

    end program Assignment1

