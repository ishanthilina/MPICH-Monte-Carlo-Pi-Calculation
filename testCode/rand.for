	   subroutine genRandNum (num)
	   	   	implicit none

	   	   	REAL :: num

	   	   	num=Rand(0)
	   	   	
	   end subroutine genRandNum	   

	   program main
	   	implicit none
	   	
	   	REAL :: x

c 		Better to call SRand in each thread. Ex seed = some_constant+threadId	   	
	   	call SRand(12321)
	   	call genRandNum(x)

	   	print *,x

	   	call genRandNum(x)

	   	print *,x

	   end program main