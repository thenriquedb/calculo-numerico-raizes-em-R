metodo_secante <- function(a, b, Toler, MaxIter, Fx) 
{
	# Para o método funcionar corretamente é necessário que |Fx(b)| seja maior que |Fx(a)
    # Caso este requisito não seja atendido será realizado um SWAP entre as variaveis  
    if(abs(Fx(a)) < abs(Fx(b))) 
	{
		t <- a
		a <- b
		b <- t
	}

	iter <- 0
	x <- b # X inicial

	repeat {
		deltaX <- -((Fx(x)/(Fx(b) - Fx(a))) * (b - a))
		x <- x + deltaX

		a <- b
		b <- x
		iter <- iter + 1

		if(iter >= MaxIter) 
		{
			cat("Quantidade maxíma de interações atingida. \n")
			return(NA)
		} 

		if((abs(deltaX) <= Toler) && (abs(Fx(x) <= Toler)))
		{
			return(x)
		}

	}

}

ModeloBarris <- function(x) 
{
    return( (1/((0.08**2)*32)) * ( 527*(527-470) * log(1 + ((0.08*x)/(527-470))) - 527*0.08*x ) + 300)
}

cat("Raiz de x: ", metodo_secante(40,45, 0.00001, 100, ModeloBarris),'\n')


