
newton <- function(X0, Fx, Dx, erro, maxIter) 
{
    iter <- 1
    x <- X0

    repeat 
    {
        novoX <- x - (Fx(x) / Dx(Fx, x, 0.00001))
        dif <- abs(novoX - x)

        if ((dif <= erro) && (Fx(novoX) <= erro)) 
        {
            return(novoX)
        }

        iter <- iter + 1
        
        # Caso o número de iterações chegue ao limite o laço será interropido
        if (iter > maxIter)
        {
            return(NA)
        } 

        x <- novoX
    }

}

# Calcula a derivada de Fx utilizando o método das diferenças finitas
DerivadaNumerica <- function(Fx, x, h)
{   
    return((Fx(x + h) - Fx(x))/h)
}


ModeloBarris <- function(x) 
{
    return( (1/((0.08**2)*32)) * ( 527*(527-470) * log(1 + ((0.08*x)/(527-470))) - 527*0.08*x ) + 300)
}

cat('A raiz é ', newton(42.5, ModeloBarris, DerivadaNumerica, 0.00001, 100),'\n')

