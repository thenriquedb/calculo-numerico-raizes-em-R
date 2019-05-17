# Fx: Função
# a: Inicio do intervalo
# b: Final do intervalo
# erro: Margem de erro
bisseccao <- function(Fx, a, b, erro)
{
    # Existira raizes reais apenas quando a raizes dos intervalos forem negativas
    if(Fx(a) * Fx(b) > 0) 
    {
        cat("Não existe raiz no intervalo informado. \n")
        return(NA)
    } 
    
    # Ponto médio entre 'a' e 'b'
    novoX <- (a+b)/2

    repeat 
    {
        # Garantir que seja o menor intervalo possivel
        if(abs(Fx(novoX)) <= erro && abs(b-a) <= erro)
        {
            return(novoX)
        }

        # Excluir metade que não contém a raiz 
        if(Fx(a)*Fx(novoX) > 0)
        {
            a <- novoX
        } else {
            b <- novoX
        }

        novoX <- (a+b)/2
    }
}

ModeloBarris <- function(x) 
{
    return( (1/((0.08**2)*32)) * ( 527*(527-470) * log(1 + ((0.08*x)/(527-470))) - 527*0.08*x ) + 300)
}

cat('A raiz é ', bisseccao(ModeloBarris,45.0, 50.0,0.00001), '\n')